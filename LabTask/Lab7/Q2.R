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

cat("\n======================================================\n")
cat("      TITANIC REPORT     \n")
cat("======================================================\n\n")

# --- OVERALL SURVIVAL INFERENCE ---
total_passengers <- nrow(titanic)
overall_survivors <- sum(titanic$Survived == 1, na.rm = TRUE)
overall_rate <- (overall_survivors / total_passengers) * 100

cat("1. OVERALL SURVIVAL:\n")
cat(sprintf("Out of the %d passengers recorded in the dataset, %.1f%% survived the tragedy.\n\n", 
            total_passengers, overall_rate))

# --- GENDER INFERENCE ---
gender_stats <- titanic %>%
  group_by(Sex) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

female_rate <- gender_stats$Survival_Rate[gender_stats$Sex == "female"]
male_rate <- gender_stats$Survival_Rate[gender_stats$Sex == "male"]

cat("INFERENCE ON GENDER:\n")
cat(sprintf("Gender was a massive determining factor for survival. Females had a staggering %.1f%% survival rate, whereas only %.1f%% of males survived. This strongly suggests the 'women and children first' protocol was heavily enforced.\n\n", 
            female_rate, male_rate))

# --- PASSENGER CLASS INFERENCE ---
class_stats <- titanic %>%
  group_by(Pclass) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

class1_rate <- class_stats$Survival_Rate[class_stats$Pclass == 1]
class3_rate <- class_stats$Survival_Rate[class_stats$Pclass == 3]

cat("INFERENCE ON SOCIOECONOMIC STATUS (CLASS):\n")
cat(sprintf("First-class passengers had a distinct advantage, with a %.1f%% survival rate. In stark contrast, Third-class passengers had a significantly lower survival rate of %.1f%%, indicating a strong correlation between socioeconomic status and survival probability.\n\n", 
            class1_rate, class3_rate))

# --- EMBARKATION POINT INFERENCE ---
embark_stats <- titanic %>%
  filter(Embarked %in% c("C", "S", "Q")) %>%
  group_by(Embarked) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

cherbourg_rate <- embark_stats$Survival_Rate[embark_stats$Embarked == "C"]
southampton_rate <- embark_stats$Survival_Rate[embark_stats$Embarked == "S"]

cat("INFERENCE ON POINT OF EMBARKATION:\n")
cat(sprintf("Passengers who embarked from Cherbourg (C) had the highest survival rate at %.1f%%, compared to those from Southampton (S) who had a survival rate of %.1f%%.\n\n", 
            cherbourg_rate, southampton_rate))

# --- AGE AND FARE ---
actual_survivors <- titanic_cleaned %>% filter(Survived == 1)

avg_survivor_age <- mean(actual_survivors$Age)
avg_survivor_fare <- mean(actual_survivors$Fare)

cat("PROFILE OF A SURVIVOR (Based on cleaned continuous data):\n")
cat(sprintf("Looking exclusively at the survivors with recorded ages, the average age was %.1f years old, and they paid an average fare of $%.2f. This reinforces the finding that wealthier passengers had better odds of securing a lifeboat.\n\n", 
            avg_survivor_age, avg_survivor_fare))