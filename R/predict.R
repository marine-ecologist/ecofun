

#' Wrapper to predict from INLA models
#'
#' Function to wrap inla models with predictors
#'
#'
#'
#' @param input input from ecofun::inla
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
# predict(m1)

predict <- function(input, ...) {

  stack = input$stack
  model = input$model
  new_data = input$newdat

  # Get the index of predictions based on the stack
  index_pred <- INLA::inla.stack.index(stack, tag = "pred")$data

  # Extract predictions and exponentiate to original scale
  predicted_df <- data.frame(
    predicted_mean = (model$summary.fitted.values$mean[index_pred]),
    predicted_lower = (model$summary.fitted.values$`0.025quant`[index_pred]),
    predicted_upper = (model$summary.fitted.values$`0.975quant`[index_pred])
  )

  # Combine with new data
  preds_inla <- cbind(new_data, predicted_df)

  return(preds_inla)
}
