# Prompt the user for weight (in kg) and height (in meters)
weight_input <- readline(prompt = "Enter weight (in kg): ")
height_input <- readline(prompt = "Enter height (in meters): ")

# Convert inputs to numeric values
weight <- as.numeric(weight_input)
height <- as.numeric(height_input)

# Calculate BMI
bmi <- weight / (height^2)

# Check the BMI against the category ranges using logical operators
is_underweight <- bmi <= 18.4
is_normal      <- bmi >= 18.5 & bmi <= 24.9
is_overweight  <- bmi >= 25.0 & bmi <= 39.9
is_obese       <- bmi >= 40.0

# Print the expected logical outputs
cat("Underweight:", is_underweight, "\n")
cat("Normal:", is_normal, "\n")
cat("Overweight:", is_overweight, "\n")
cat("Obese:", is_obese, "\n")