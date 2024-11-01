

#' Wrapper to simplify INLA models
#'
#' Function to wrap inla models iwth predictors
#'
#'
#'
#' @param data df for inla model
#' @param formula specified formula (no ranef yet)
#' @param newdata for predictors as list
#' @param family default gaussian
#' @param compute_dic compute dic, logical
#' @param compute_waic compute waic, logical
#' @param verbose silence inla
#' @param ... passes functions
#' @export
#' # example code
# # Example usage:
# # Define predictors and formula
# newdat <- data.frame(age = seq(1, 200, by = 1), species = unique(mean_chrono_standardised$species))
# formula <- log(pred_cum_radius) ~ log(age) * species
# fm1_inla_pred <- run_inla_model(data = mean_chrono_standardised, formula = formula, newdat = newdat)


# Wrapper function for INLA model
inla <- function(data, formula, newdat, family = "gaussian", compute_dic = TRUE, compute_waic = TRUE, marginals=TRUE, verbose = TRUE) {

  # Extract response and predictor variables from the formula
  response <- all.vars(formula)[1]  # The response variable is the first element
  predictors <- all.vars(formula)[-1]  # Exclude the response variable to get predictors

  # Construct new data for predictions
  newdat_inla <- base::expand.grid(newdat[predictors]) |> base::as.data.frame()

  # Prepare the effects list dynamically based on predictors
  effects_fit <- list()
  effects_pred <- list()

  for (pred in predictors) {
    effects_fit[[pred]] <- data[[pred]]
    effects_pred[[pred]] <- newdat_inla[[pred]]
  }

  # Define the stack for fitting data using the response variable
  stack_fit <- INLA::inla.stack(
    data = list(response = base::log(data[[response]])),  # Log-transform the response
    A = list(1),
    effects = list(effects_fit),
    tag = "obs"
  )

  # Define the stack for predictions
  stack_pred <- INLA::inla.stack(
    data = list(response = NA),
    A = list(1),
    effects = list(effects_pred),
    tag = "pred"
  )

  # Combine fit and prediction stacks
  stack <- INLA::inla.stack(stack_fit, stack_pred)

  # Run the INLA model
  model <- INLA::inla(
    formula,
    data = INLA::inla.stack.data(stack),
    family = family,
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE),
    control.compute = list(
      dic = compute_dic,
      waic = compute_waic,
      cpo = TRUE,
      return.marginals.predictor = marginals
    ),
    verbose = verbose
  )

  output <- list(stack = stack, model = model)
  names(output) <- c("stack", "model")
  return(output)
}

