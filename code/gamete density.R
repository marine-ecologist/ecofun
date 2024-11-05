
### basic


# Given parameters
gamete_density <- 10000               # eggs/m^3
interactive_distance <- 1.5 / 1000    # m (converted from mm to meters)
epsilon <- 1e-4                       # m^2/s^3 (turbulent dissipation rate)
lead_constant <- 1.9                  # lead parameter constant

# Calculate each component
R_power <- interactive_distance^(7/3)
epsilon_power <- epsilon^(1/3)

# Calculate encounter rate (E_turbulence)
E_turbulence <- lead_constant * gamete_density * pi * R_power * epsilon_power
E_turbulence  # encounters per second









# Define ranges for each parameter
gamete_density_values <- c(1e5, 1e6, 1e7)       # Gamete density (eggs/m^3)
interactive_distance_values <- seq(1, 5, 1)     # Interactive distance (mm)
epsilon_values <- c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4)  # Turbulent dissipation rate (m^2/s^3)
lead_constant <- 1.9                            # Fixed lead parameter constant

# Create an orthogonal design (full factorial design) for the parameters
results <- expand.grid(
  gamete_density = gamete_density_values,
  interactive_distance_mm = interactive_distance_values,
  epsilon = epsilon_values
) |> mutate(lead_constant = lead_constant)


# Define the formula as a function
calculate_encounter_rate <- function(gamete_density, interactive_distance_mm, epsilon, lead_constant) {
  interactive_distance_m <- interactive_distance_mm / 1000   # Convert interactive distance from mm to meters
  encounter_rate <- lead_constant * gamete_density * pi * interactive_distance_m^(7/3) * epsilon^(1/3)   # Calculate the encounter rate based on the given formula
  return(encounter_rate)
}

# Apply the function across each row in the design data frame to calculate encounter rates
results$E_turbulence <- mapply(
  calculate_encounter_rate,
  gamete_density = design$gamete_density,
  interactive_distance_mm = design$interactive_distance_mm,
  epsilon = design$epsilon,
  lead_constant = design$lead_constant
)

# Display the first few rows of the updated design data frame
head(results)

hist(results$E_turbulence)
