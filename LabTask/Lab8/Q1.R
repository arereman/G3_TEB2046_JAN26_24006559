# --- Load Required Libraries ---
library(dplyr)

# Install plotrix if not available (needed for 3D Pie Chart)
if (!requireNamespace("plotrix", quietly = TRUE)) {
  install.packages("plotrix")
}
library(plotrix)


# ============================================================
# SECTION 1: LOAD & CLEAN DATA
# ============================================================

titanic <- read.csv("titanic.csv")

cat("--- Checking for Missing Values ---\n")
cat("Total NAs in dataset:", sum(is.na(titanic)), "\n")
print(sapply(titanic, function(x) sum(is.na(x))))
cat("Original dimensions:", dim(titanic), "\n\n")

survivorData <- titanic %>%
  select(PassengerId, Survived, Pclass, Sex, Age, Fare, Embarked)

titanic_cleaned <- na.omit(survivorData)
cat("Cleaned dimensions:", dim(titanic_cleaned), "\n\n")

# Filtered subsets
females_only     <- filter(titanic_cleaned, Sex == "female")
high_fare_only   <- filter(titanic_cleaned, Fare > 500)
under_50_only    <- filter(titanic_cleaned, Age < 50)
specific_group   <- titanic_cleaned %>% filter(Sex == "female", Fare > 500, Age < 50)

# Sorted exports
titanic_sortbyfare     <- arrange(titanic_cleaned, Fare)
titanic_sortbyfaredesc <- arrange(titanic_cleaned, desc(Fare))

write.csv(titanic_sortbyfare,     "titanic_sortbyfare.csv",     row.names = FALSE)
write.csv(titanic_sortbyfaredesc, "titanic_sortbyfaredesc.csv", row.names = FALSE)
write.csv(survivorData,           "survivorData.csv",           row.names = FALSE)


# ============================================================
# SECTION 2: COMPUTE STATISTICS FOR REPORT
# ============================================================

total_passengers  <- nrow(titanic)
overall_survivors <- sum(titanic$Survived == 1, na.rm = TRUE)
overall_rate      <- (overall_survivors / total_passengers) * 100

gender_stats <- titanic %>%
  group_by(Sex) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

female_rate <- gender_stats$Survival_Rate[gender_stats$Sex == "female"]
male_rate   <- gender_stats$Survival_Rate[gender_stats$Sex == "male"]

class_stats <- titanic %>%
  group_by(Pclass) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

class1_rate <- class_stats$Survival_Rate[class_stats$Pclass == 1]
class2_rate <- class_stats$Survival_Rate[class_stats$Pclass == 2]
class3_rate <- class_stats$Survival_Rate[class_stats$Pclass == 3]

embark_stats <- titanic %>%
  filter(Embarked %in% c("C", "S", "Q")) %>%
  group_by(Embarked) %>%
  summarise(Survival_Rate = mean(Survived, na.rm = TRUE) * 100)

cherbourg_rate  <- embark_stats$Survival_Rate[embark_stats$Embarked == "C"]
southampton_rate <- embark_stats$Survival_Rate[embark_stats$Embarked == "S"]
queenstown_rate  <- embark_stats$Survival_Rate[embark_stats$Embarked == "Q"]

actual_survivors   <- titanic_cleaned %>% filter(Survived == 1)
avg_survivor_age   <- mean(actual_survivors$Age)
avg_survivor_fare  <- mean(actual_survivors$Fare)


# ============================================================
# SECTION 3: VISUALIZATIONS  (Lab 8a techniques applied)
# ============================================================

# ----------------------------------------------------------
# PLOT 1 — PIE CHART (Lab 8a §2.2–2.4)
# Overall Survival Split
# ----------------------------------------------------------
png(file = "plot1_pie_overall_survival.png", width = 700, height = 500)

survival_counts <- c(overall_survivors, total_passengers - overall_survivors)
survival_labels_raw <- c("Survived", "Did Not Survive")
survival_pct   <- round(100 * survival_counts / sum(survival_counts), 1)
survival_labels <- paste0(survival_labels_raw, "\n", survival_pct, "%")

pie(survival_counts,
    labels = survival_labels,
    main   = "Overall Titanic Survival (Pie Chart)",
    col    = c("steelblue", "tomato"),
    cex    = 1.1)

legend("topright",
       legend = survival_labels_raw,
       fill   = c("steelblue", "tomato"),
       cex    = 0.9)

dev.off()
cat(">> Saved: plot1_pie_overall_survival.png\n")


# ----------------------------------------------------------
# PLOT 2 — BAR CHART (Lab 8a §2.12–2.13)
# Survival Rate by Gender
# ----------------------------------------------------------
png(file = "plot2_bar_gender_survival.png", width = 700, height = 500)

gender_rates <- c(female_rate, male_rate)
gender_names <- c("Female", "Male")

bp <- barplot(gender_rates,
              names.arg = gender_names,
              xlab      = "Gender",
              ylab      = "Survival Rate (%)",
              main      = "Survival Rate by Gender (Bar Chart)",
              col       = c("orchid", "dodgerblue"),
              border    = "black",
              ylim      = c(0, 100))

# Add value labels on bars
text(x   = bp,
     y   = gender_rates + 3,
     labels = paste0(round(gender_rates, 1), "%"),
     cex  = 1.1,
     font = 2)

abline(h = overall_rate, col = "darkred", lty = 2, lwd = 1.5)
text(x = bp[2] + 0.6, y = overall_rate + 3,
     labels = paste0("Overall: ", round(overall_rate, 1), "%"),
     col = "darkred", cex = 0.85)

dev.off()
cat(">> Saved: plot2_bar_gender_survival.png\n")


# ----------------------------------------------------------
# PLOT 3 — GROUPED BAR CHART (Lab 8a §2.14)
# Survival Count (Survived vs Died) by Passenger Class
# ----------------------------------------------------------
png(file = "plot3_grouped_bar_class_survival.png", width = 800, height = 550)

class_counts <- titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(Count = n(), .groups = "drop")

class_matrix <- matrix(
  c(
    class_counts$Count[class_counts$Pclass == 1 & class_counts$Survived == 0],
    class_counts$Count[class_counts$Pclass == 2 & class_counts$Survived == 0],
    class_counts$Count[class_counts$Pclass == 3 & class_counts$Survived == 0],
    class_counts$Count[class_counts$Pclass == 1 & class_counts$Survived == 1],
    class_counts$Count[class_counts$Pclass == 2 & class_counts$Survived == 1],
    class_counts$Count[class_counts$Pclass == 3 & class_counts$Survived == 1]
  ),
  nrow = 2, byrow = TRUE
)

bar_colors <- c("tomato", "steelblue")
class_labels <- c("1st Class", "2nd Class", "3rd Class")

barplot(class_matrix,
        beside     = TRUE,
        names.arg  = class_labels,
        xlab       = "Passenger Class",
        ylab       = "Number of Passengers",
        main       = "Survival Count by Passenger Class (Grouped Bar Chart)",
        col        = bar_colors,
        border     = "black",
        ylim       = c(0, max(class_matrix) * 1.2))

legend("topright",
       legend = c("Did Not Survive", "Survived"),
       fill   = bar_colors,
       cex    = 0.95)

dev.off()
cat(">> Saved: plot3_grouped_bar_class_survival.png\n")


# ----------------------------------------------------------
# PLOT 4 — BOX PLOT (Lab 8a §2.6–2.7)
# Fare distribution by Survival Status (within cleaned data)
# ----------------------------------------------------------
png(file = "plot4_boxplot_fare_survival.png", width = 750, height = 550)

# Cap outliers for readability (fares > 300 are extreme outliers)
titanic_capped <- titanic_cleaned
titanic_capped$Fare[titanic_capped$Fare > 300] <- 300

boxplot(Fare ~ Survived,
        data     = titanic_capped,
        names    = c("Did Not Survive", "Survived"),
        xlab     = "Survival Status",
        ylab     = "Fare Paid (USD, capped at 300)",
        main     = "Fare Distribution by Survival Status (Box Plot)",
        col      = c("tomato", "steelblue"),
        notch    = TRUE,
        varwidth = TRUE,
        border   = "black")

mtext("Note: Fares capped at $300 to reduce extreme outlier distortion",
      side = 1, line = 4, cex = 0.8, col = "gray40")

dev.off()
cat(">> Saved: plot4_boxplot_fare_survival.png\n")


# ----------------------------------------------------------
# PLOT 5 — HISTOGRAM (Lab 8a §2.8–2.9)
# Age distribution of Survivors vs Non-Survivors
# ----------------------------------------------------------
png(file = "plot5_histogram_age.png", width = 800, height = 550)

survivors_age     <- titanic_cleaned$Age[titanic_cleaned$Survived == 1]
non_survivors_age <- titanic_cleaned$Age[titanic_cleaned$Survived == 0]

hist(survivors_age,
     xlab    = "Age (Years)",
     ylab    = "Number of Passengers",
     main    = "Age Distribution: Survivors vs Non-Survivors (Histogram)",
     col     = rgb(0.2, 0.5, 0.8, 0.6),
     border  = "white",
     xlim    = c(0, 80),
     ylim    = c(0, 50),
     breaks  = 15)

hist(non_survivors_age,
     col    = rgb(0.9, 0.2, 0.2, 0.5),
     border = "white",
     breaks = 15,
     add    = TRUE)

legend("topright",
       legend = c("Survived", "Did Not Survive"),
       fill   = c(rgb(0.2, 0.5, 0.8, 0.6), rgb(0.9, 0.2, 0.2, 0.5)),
       cex    = 0.95)

abline(v = mean(survivors_age, na.rm = TRUE),
       col = "darkblue", lty = 2, lwd = 2)
abline(v = mean(non_survivors_age, na.rm = TRUE),
       col = "darkred",  lty = 2, lwd = 2)

dev.off()
cat(">> Saved: plot5_histogram_age.png\n")


# ----------------------------------------------------------
# PLOT 6 — SCATTERPLOT (Lab 8a §2.10)
# Age vs Fare coloured by Survival
# ----------------------------------------------------------
png(file = "plot6_scatter_age_fare.png", width = 800, height = 600)

point_colors <- ifelse(titanic_cleaned$Survived == 1, "steelblue", "tomato")
point_shapes <- ifelse(titanic_cleaned$Survived == 1, 16, 17)

fare_cap <- 300
plot_data <- titanic_cleaned[titanic_cleaned$Fare <= fare_cap, ]
pc <- ifelse(plot_data$Survived == 1, "steelblue", "tomato")
ps <- ifelse(plot_data$Survived == 1, 16, 17)

plot(x    = plot_data$Age,
     y    = plot_data$Fare,
     col  = pc,
     pch  = ps,
     xlab = "Age (Years)",
     ylab = "Fare Paid (USD, capped at $300)",
     xlim = c(0, 80),
     ylim = c(0, fare_cap),
     main = "Age vs Fare by Survival Status (Scatterplot)")

legend("topright",
       legend = c("Survived", "Did Not Survive"),
       col    = c("steelblue", "tomato"),
       pch    = c(16, 17),
       cex    = 0.95)

dev.off()
cat(">> Saved: plot6_scatter_age_fare.png\n")


# ----------------------------------------------------------
# PLOT 7 — LINE GRAPH (Lab 8a §2.15–2.16)
# Survival Rate by Passenger Class — line trend
# ----------------------------------------------------------
png(file = "plot7_line_class_survival_rate.png", width = 750, height = 500)

class_survival_rates <- class_stats$Survival_Rate[order(class_stats$Pclass)]

plot(x    = 1:3,
     y    = class_survival_rates,
     type = "o",
     col  = "steelblue",
     pch  = 16,
     lwd  = 2,
     xaxt = "n",
     xlab = "Passenger Class",
     ylab = "Survival Rate (%)",
     ylim = c(0, 80),
     main = "Survival Rate Trend by Passenger Class (Line Graph)")

axis(1, at = 1:3, labels = c("1st Class", "2nd Class", "3rd Class"))

points(x   = 1:3,
       y   = class_survival_rates,
       col = "steelblue",
       pch = 16,
       cex = 1.5)

text(x      = 1:3,
     y      = class_survival_rates + 3.5,
     labels = paste0(round(class_survival_rates, 1), "%"),
     cex    = 1.0,
     font   = 2,
     col    = "darkblue")

abline(h   = overall_rate,
       col = "darkred",
       lty = 2,
       lwd = 1.5)

text(x      = 3.05,
     y      = overall_rate + 3,
     labels = paste0("Overall avg: ", round(overall_rate, 1), "%"),
     col    = "darkred",
     cex    = 0.8,
     adj    = 1)

dev.off()
cat(">> Saved: plot7_line_class_survival_rate.png\n")


# ----------------------------------------------------------
# PLOT 8 — 3D PIE CHART (Lab 8a §2.5)
# Embarkation Point Share
# ----------------------------------------------------------
png(file = "plot8_3dpie_embarkation.png", width = 750, height = 550)

embark_counts <- titanic %>%
  filter(Embarked %in% c("C", "S", "Q")) %>%
  group_by(Embarked) %>%
  summarise(Count = n())

emb_values <- embark_counts$Count
emb_pct    <- round(100 * emb_values / sum(emb_values), 1)
emb_labels <- paste0(c("Cherbourg (C)", "Queenstown (Q)", "Southampton (S)"),
                     "\n", emb_pct, "%")

pie3D(emb_values,
      labels  = emb_labels,
      explode = 0.1,
      col     = c("gold", "lightgreen", "lightskyblue"),
      main    = "Passengers by Embarkation Point (3D Pie Chart)",
      labelcex = 0.85)

dev.off()
cat(">> Saved: plot8_3dpie_embarkation.png\n")


# ============================================================
# SECTION 4: PRINTED REPORT (same as original + references)
# ============================================================

cat("\n======================================================\n")
cat("             TITANIC REPORT (ENHANCED)               \n")
cat("======================================================\n\n")

cat("1. OVERALL SURVIVAL:\n")
cat(sprintf(
  "Out of the %d passengers recorded in the dataset, %.1f%% survived the tragedy.\n",
  total_passengers, overall_rate))
cat("   >> See: plot1_pie_overall_survival.png\n\n")

cat("2. INFERENCE ON GENDER:\n")
cat(sprintf(
  "Gender was a massive determining factor for survival. Females had a staggering %.1f%% survival rate,\nwhereas only %.1f%% of males survived. This strongly suggests the 'women and children first'\nprotocol was heavily enforced.\n",
  female_rate, male_rate))
cat("   >> See: plot2_bar_gender_survival.png\n\n")

cat("3. INFERENCE ON SOCIOECONOMIC STATUS (CLASS):\n")
cat(sprintf(
  "First-class passengers had a distinct advantage, with a %.1f%% survival rate.\nSecond-class passengers had a survival rate of %.1f%%.\nIn stark contrast, Third-class passengers had a significantly lower survival rate of %.1f%%,\nindicating a strong correlation between socioeconomic status and survival probability.\n",
  class1_rate, class2_rate, class3_rate))
cat("   >> See: plot3_grouped_bar_class_survival.png\n")
cat("   >> See: plot7_line_class_survival_rate.png\n\n")

cat("4. FARE AS AN INDICATOR OF SURVIVAL:\n")
cat(sprintf(
  "Survivors consistently paid higher fares than non-survivors, reinforcing the link between\nwealth and access to lifeboats. The box plot clearly shows a higher median fare among survivors.\n"))
cat("   >> See: plot4_boxplot_fare_survival.png\n\n")

cat("5. AGE DISTRIBUTION AMONG SURVIVORS:\n")
cat(sprintf(
  "Looking exclusively at survivors with recorded ages, the average age was %.1f years, compared\nto an average of %.1f years for non-survivors. Young children appear more frequently among\nsurvivors, consistent with the 'women and children first' protocol.\n",
  mean(survivors_age), mean(non_survivors_age)))
cat("   >> See: plot5_histogram_age.png\n\n")

cat("6. AGE vs FARE (SURVIVOR PROFILE):\n")
cat(sprintf(
  "The scatterplot reveals that survivors who paid higher fares tend to cluster across all ages,\nbut wealthier, middle-aged passengers show a notably higher survival density.\nThe average survivor paid a fare of $%.2f.\n",
  avg_survivor_fare))
cat("   >> See: plot6_scatter_age_fare.png\n\n")

cat("7. INFERENCE ON POINT OF EMBARKATION:\n")
cat(sprintf(
  "Passengers who embarked from Cherbourg (C) had the highest survival rate at %.1f%%,\ncompared to those from Southampton (S) at %.1f%% and Queenstown (Q) at %.1f%%.\nThis likely reflects the higher proportion of first-class passengers who boarded at Cherbourg.\n",
  cherbourg_rate, southampton_rate, queenstown_rate))
cat("   >> See: plot8_3dpie_embarkation.png\n\n")

cat("======================================================\n")
cat("  All 8 plot files saved to working directory.\n")
cat("======================================================\n")