#' Wrapper to simplify INLA models
#'
#' Function to wrap INLA models with predictors
#'
#' @param data Data frame for INLA model
#' @param formula Specified formula (no random effects yet)
#' @param newdat New data for predictors as a list
#' @param family Family distribution for INLA, default is "gaussian"
#' @param compute_dic Compute DIC, logical
#' @param compute_waic Compute WAIC, logical
#' @param compute_cpo Compute CPO, logical
#' @param compute_marginals Compute marginals, logical
#' @param verbose Set INLA output verbosity, logical
#' @param ... Additional parameters passed to INLA
#' @export
#' @examples
#' # Set seed for reproducibility
#' set.seed(123)
#
# # Example usage with cement data
# data("cement")
#
# # Generate a grid of new values for prediction
# newdat <- generate_repeated_grid(cement, predictors = c("x1", "x2", "x3"), n = 3)
#
# # Run the prediction function
# result <- inla2(
#   formula = y ~ x1 + x2 + x3,
#   data = cement,
#   newdat = newdat
# )
#
# # Display the predictions
# head(result$inla_predictions)
#

# Generic function to fit an INLA model and make predictions on new data
inla2 <- function(formula, data, newdat, response_log = FALSE, predictor_log = NULL, seed = NULL) {

  # Set the seed for reproducibility, if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Parse the formula to detect log() transformations
  terms_info <- terms(formula)
  response_var <- all.vars(formula)[1]
  response_log <- grepl("log\\(", deparse(terms_info[[2]]))  # Check if log() is used on the response

  # Identify log-transformed predictors
  predictor_log <- sapply(attr(terms_info, "variables")[-1], function(term) {
    deparse(term) %in% grep("log\\(", deparse(term), value = TRUE)
  })
  predictor_log <- names(predictor_log)[predictor_log]  # Get only the predictors with log()

  # Transform response variable if it's on the log scale
  if (response_log) {
    data[[response_var]] <- log(data[[response_var]])
    newdat[[response_var]] <- NA  # Set response in newdat to NA
  } else {
    newdat[[response_var]] <- NA
  }

  # Transform log-transformed predictors in both data and newdat
  for (pred in predictor_log) {
    data[[pred]] <- log(data[[pred]])
    newdat[[pred]] <- log(newdat[[pred]])
  }

  # Remove response variable from data and newdat for the effects in the stack
  effects_data <- data[, setdiff(names(data), response_var), drop = FALSE]
  effects_newdat <- newdat[, setdiff(names(newdat), response_var), drop = FALSE]

  # Add an intercept column explicitly to both effects data frames
  effects_data$intercept <- 1
  effects_newdat$intercept <- 1

  # Create INLA stack for observed data with explicit intercept
  stack_observed <- INLA::inla.stack(
    data = list(y = data[[response_var]]),
    A = list(1),
    effects = list(effects_data),
    tag = "observed"
  )

  # Create INLA stack for prediction data with explicit intercept
  stack_prediction <- INLA::inla.stack(
    data = list(y = NA),
    A = list(1),
    effects = list(effects_newdat),
    tag = "prediction"
  )

  # Combine the stacks
  stack <- INLA::inla.stack(stack_observed, stack_prediction)

  # Fit the INLA model using the combined stack
  model <- INLA::inla(
    formula = stats::update(formula, . ~ . + intercept - 1),  # Explicit intercept, remove implicit intercept
    data = INLA::inla.stack.data(stack),
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE)
  )

  # Extract the index of predictions in the stack
  index_pred <- INLA::inla.stack.index(stack, "prediction")$data

  # Extract predictions for new data
  predictions <- model$summary.linear.predictor[index_pred, c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")]

  # If the response is on the log scale, exponentiate the predictions to transform them back
  if (response_log) {
    predictions <- predictions %>%
      dplyr::mutate(
        mean = exp(mean),
        sd = exp(sd),  # Note: This transformation may not be fully accurate for sd, but commonly used
        `0.025quant` = exp(`0.025quant`),
        `0.5quant` = exp(`0.5quant`),
        `0.975quant` = exp(`0.975quant`)
      )
  }

  # Combine predictions with newdat
  predicted_newdat <- dplyr::select(
    cbind(newdat, predictions),
    -y
  ) %>%
    dplyr::rename(lower = `0.025quant`, median = `0.5quant`, upper = `0.975quant`)

  # Return the model and predictions as a list
  return(list(inla_model = model, inla_predictions = predicted_newdat))
}
