

#' Wrapper to predict from INLA models
#'
#' Function to wrap inla models with predictors
#'
#'
#'
#' @param input input from ecofun::inla
#' @param ... passes functions
#' @export

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
