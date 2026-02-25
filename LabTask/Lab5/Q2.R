# prompt user for input and store it as a string
input_string <- readline(prompt = "Input an integer: ")

# convert string input into an integer
x <- as.integer(input_string)

# check if conversion worked
if (!is.na(x)) {
  
  # for loop to count up until inputted integer
  for(i in 1:x){
    # cube logic
    cat("Number is:", i, " and cube of ", i, " is: ", i^3, "\n")
  }
  
} else {
  cat("Error: Please enter a valid number.\n")
}
