# Prompt the user for their name and phone number
name_input <- readline(prompt = "Enter your name: ")
phone_input <- readline(prompt = "Enter your phone number (no spaces/dashes): ")

# Convert the name to all uppercase
name_upper <- toupper(name_input)

# Extract the first 3 digits and the last 4 digits using substring
first_3_digits <- substr(phone_input, 1, 3)
last_4_digits  <- substr(phone_input, nchar(phone_input) - 3, nchar(phone_input))

# Print the formatted output
cat("Hi,", name_upper, "\n")
cat("A verification code has been sent to", first_3_digits, "- xxxxx", last_4_digits, "\n")