# prompt user for input and store it as a string
input_string <- readline(prompt = "Input year: ")

# convert string input into an integer
year <- as.integer(input_string)

# check if conversion worked
if (!is.na(year)) {
  
  # leap year logic
  if ((year %% 4 == 0) && ((year %% 100 != 0) || (year %% 400 == 0))) {
    cat("Output:", year, "is a leap year.\n")
  } else {
    cat("Output:", year, "is a not leap year.\n") 
  }
  
} else {
  cat("Error: Please enter a valid number.\n")
}