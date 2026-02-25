# Prompt the user for the two strings
string1 <- readline(prompt = "Enter string 1: ")
string2 <- readline(prompt = "Enter string 2: ")

# Convert both strings to lowercase and compare them
is_similar <- tolower(string1) == tolower(string2)

# Print the final result 
cat("This program compare 2 strings. Both inputs are similar:", is_similar, "\n")