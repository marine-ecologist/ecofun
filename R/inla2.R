

#' Wrapper to simplify INLA models
#'
#' Function to wrap inla models iwth predictors
#'
#'
#'
#' @param data df for inla model
#' @param formula specified formula (no ranef yet)
#' @param newdat newdata for predictors as list
#' @param family default gaussian
#' @param compute_dic compute dic, logical
#' @param compute_waic compute waic, logical
#' @param compute_marginals compute waic, logical
#' @param verbose silence inla
#' @param ... passes functions
#' @export
#' @examples

# # Set seed for reproducibility
# set.seed(123)
#
# # Generate fake data
# n <- 100
# y <- runif(n, 1, 10)  # Predictor variable y, uniform distribution between 1 and 10
# z <- factor(sample(c("A", "B", "C"), n, replace = TRUE))  # Categorical predictor z with 3 levels
# x <- 3 + 0.5 * y + as.numeric(z) * 1.5 + rnorm(n, sd = 0.5)  # Response variable x with interaction effects
#
# # Combine into a data frame
# fake_data <- data.frame(x = x, y = y, z = z)
#
# # Create a new data frame for predictions
# newdat <- expand.grid(y = seq(1, 10, length.out = 20), z = levels(z))
#
# # Specify the formula
# formula <- x ~ y * z
#
# m1 <- inla2(formula, data=fake_data, newdat)


inla2 <- function(data, formula, newdat, family = "gaussian", compute_dic = TRUE, compute_waic = TRUE, compute_marginals = TRUE, verbose = TRUE, ...) {

  tictoc::tic()

  # Extract response and predictor variables from the formula
  response <- all.vars(formula)[1]  # Extract response variable
  predictors <- all.vars(formula)[-1]  # Extract predictors

  # Construct new data for predictions based on predictors
  newdat_inla <- base::expand.grid(newdat[predictors]) |> base::as.data.frame()

  # Prepare the effects list dynamically based on predictors
  effects_fit <- list()
  effects_pred <- list()

  for (pred in predictors) {
    effects_fit[[pred]] <- data[[pred]]
    effects_pred[[pred]] <- newdat_inla[[pred]]
  }

  # Define the stack for fitting data using the response variable without transformations
  stack_fit <- INLA::inla.stack(
    data = list(x = data[[response]]),  # Use the response variable directly
    A = list(1),
    effects = list(effects_fit),
    tag = "obs"
  )

  # Define the stack for predictions
  stack_pred <- INLA::inla.stack(
    data = list(x = NA),
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
    #control.inla = control_inla,
    control.compute = list(
      dic = compute_dic,
      waic = compute_waic,
      cpo = TRUE,
      return.marginals.predictor = compute_marginals
    )
  )

  # Inspect model output
  summary(model)

  # Prepare output
  output <- list(stack = stack, newdat = newdat_inla, model = model)

  # Print timing information
  (tictoc::toc())

  return(output)
}
