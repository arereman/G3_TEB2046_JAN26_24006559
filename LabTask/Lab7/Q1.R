# Import libraries used
library(readxl)
library(dplyr)
library(xlsx)

# Create working object for titanic dataset
titanic <- read.csv("titanic.csv")

cat("--- Checking for Missing Values ---\n")
# Find sum of NULL values
cat("Total NAs in dataset:", sum(is.na(titanic)), "\n")

# Print the NULL values by column to see where the missing data is
print(sapply(titanic, function(x) sum(is.na(x))))

cat("Original dimensions:", dim(titanic), "\n\n")

survivorData <- titanic %>%
  select(PassengerId, Survived, Pclass, Sex, Age, Fare, Embarked)

titanic_cleaned <- na.omit(survivorData)
cat("Cleaned dimensions: ", dim(titanic_cleaned), "\n\n")

females_only <- filter(titanic_cleaned, Sex == 'female')
high_fare_only <- filter(titanic_cleaned, Fare > 500)
under_50_only <- filter(titanic_cleaned, Age < 50)

# Chained filtering example
specific_group <- titanic_cleaned %>% 
  filter(Sex == 'female', Fare > 500, Age < 50)

# Sort by ascending order
titanic_sortbyfare <- arrange(titanic_cleaned, Fare)
# Sort by descending order
titanic_sortbyfaredesc <- arrange(titanic_cleaned, desc(Fare))

# View(survivorData)
# View(titanic_sortbyfaredesc)

# Export to CSV
write.csv(titanic_sortbyfare, "titanic_sortbyfare.csv", row.names = FALSE)
write.csv(titanic_sortbyfaredesc, "titanic_sortbyfaredesc.csv", row.names = FALSE)
write.csv(survivorData, "survivorData.csv", row.names = FALSE)