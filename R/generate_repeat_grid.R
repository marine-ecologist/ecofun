#' Wrapper to simplify INLA models
#'
#' Function to wrap INLA models with predictors
#'
#' @param data Data frame for INLA model
#' @param predictors Specified formula (no random effects yet)
#' @param n New data for predictors as a list
#' @param ... Additional parameters passed to INLA
#' @export


generate_repeated_grid <- function(data, predictors, n) {
  # Generate the grid for specified predictors
  pred_grid <- expand.grid(lapply(predictors, function(p) seq(min(data[[p]]), max(data[[p]]))))
  colnames(pred_grid) <- predictors

  # Repeat the grid n times and combine into a single data frame
  repeated_grid <- do.call(rbind, replicate(n, pred_grid, simplify = FALSE))

  return(repeated_grid)
}

