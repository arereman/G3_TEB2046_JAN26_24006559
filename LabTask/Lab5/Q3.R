# Header
cat("Check whether an n digits number is Armstrong or not:\n")
cat("-----------------------------------------------------\n")

# Prompt for user input
input_str <- readline(prompt = "Input an integer: ")

# Convert the input string to a numeric value
num <- as.numeric(input_str)

# Proceed only if the input is a valid number
if (!is.na(num)) {
  
  # Find n (the number of digits)
  n <- nchar(input_str)
  
  # Split string into individual character digits, then convert to numeric
  digits <- as.numeric(strsplit(input_str, "")[[1]])
  
  # Calculate the sum of each digit raised to the power of 'n'
  sum_pow <- sum(digits^n)
  
  # Compare the result and print the final output
  if (sum_pow == num) {
    cat(num, "is an Armstrong number.\n")
  } else {
    cat(num, "is not an Armstrong number.\n")
  }
  
} else {
  cat("Invalid input. Please enter a valid integer.\n")
}