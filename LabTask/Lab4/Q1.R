# Create and Find Levels of Factor of the Vector
age <- c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60, 52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58)
factor_age <- factor(age)
table(factor_age)

# Divide the Levels of Factor into 5 Ranges
age <- as.numeric(as.character(age))
age_range <- cut(age, breaks = 5)
age_range_factor <- factor(age_range)
table(age_range_factor)