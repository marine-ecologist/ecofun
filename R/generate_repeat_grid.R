#' Wrapper to simplify INLA models
#'
#' Function to wrap INLA models with predictors
#'
#' @param data Data frame for INLA model
#' @param predictors Specified formula (no random effects yet)
#' @param n New data for predictors as a list
#' @param ... Additional parameters passed to INLA
#' @export


# Function to generate a repeated grid for INLA predictions
generate_repeated_grid <- function(data, predictors, n = 1) {

  # Initialize an empty list to store the sequences for each predictor
  predictor_values <- list()

  for (pred in predictors) {
    if (is.numeric(data[[pred]])) {
      # For numeric variables, generate a sequence from min to max
      predictor_values[[pred]] <- seq(min(data[[pred]], na.rm = TRUE),
                                      max(data[[pred]], na.rm = TRUE),
                                      length.out = n)
    } else if (is.factor(data[[pred]])) {
      # For factor variables, include all levels of the factor
      predictor_values[[pred]] <- levels(data[[pred]])
    } else {
      stop(paste("Predictor", pred, "is neither numeric nor factor, which is not supported."))
    }
  }

  # Create the grid using expand.grid and repeat it if needed
  grid <- expand.grid(predictor_values)
  repeated_grid <- grid[rep(seq_len(nrow(grid)), each = n), ]

  return(repeated_grid)
}
