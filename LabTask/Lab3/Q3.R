# create list of records
chemistry_scores <- list(
  Robert = 59, Hemsworth = 71, Scarlett = 83, Evans = 68,
  Pratt = 65, Larson = 57, Holland = 62, Paul = 92,
  Simu = 92, Renner = 59
)

# append new Physics scores by combining them into a Data Frame
student_data <- data.frame(
  Student_Name = names(chemistry_scores),
  Chemistry = unlist(chemistry_scores),
  Physics = c(89, 86, 65, 52, 60, 67, 40, 77, 90, 61),
  row.names = NULL
)

# count how many students failed (<= 49) in each subject
fails_chemistry <- sum(student_data$Chemistry <= 49)
fails_physics   <- sum(student_data$Physics <= 49)
fails_both      <- sum(student_data$Chemistry <= 49 & student_data$Physics <= 49)

# find highest scores for each subject
max_chem <- max(student_data$Chemistry)
max_phys <- max(student_data$Physics)

# extract names of students who got those highest scores
top_chem_student <- paste(student_data$Student_Name[student_data$Chemistry == max_chem], collapse = ", ")
top_phys_student <- paste(student_data$Student_Name[student_data$Physics == max_phys], collapse = ", ")

# calculate combined total to find best overall student across both subjects
student_data$Total_Score <- student_data$Chemistry + student_data$Physics
top_overall_student <- paste(student_data$Student_Name[student_data$Total_Score == max(student_data$Total_Score)], collapse = ", ")

# --- Print the Outputs ---
cat("--- Failure Counts ---\n")
cat("Students failing Chemistry: ", fails_chemistry, "\n")
cat("Students failing Physics:   ", fails_physics, "\n")
cat("Students failing BOTH:      ", fails_both, "\n\n")

cat("--- Highest Scores ---\n")
cat("Highest in Chemistry: ", top_chem_student, "(", max_chem, ")\n")
cat("Highest in Physics:   ", top_phys_student, "(", max_phys, ")\n")
cat("Best overall across both subjects: ", top_overall_student, "(Total:", max(student_data$Total_Score), ")\n")