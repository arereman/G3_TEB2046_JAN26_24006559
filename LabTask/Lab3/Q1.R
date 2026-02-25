# store input vector of 20 exam scores
scores <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54)

# check whether each student passed (>49). Returns TRUE or FALSE.
passed <- scores > 49

# categorize scores into grades based on the provided table
grades <- cut(scores, 
              breaks = c(-Inf, 49, 59, 69, 79, 89, 100), 
              labels = c("F", "E", "D", "C", "B", "A"),
              right = TRUE) # right = TRUE means ranges include the upper bound, e.g., (49, 59]

# create a data frame to show the individual records clearly
student_records <- data.frame(
  Score = scores,
  Grade = grades,
  Passed = passed
)

# count the number of students per grade
grade_counts <- table(grades)

# create summary table
summary_table <- data.frame(
  Score_Range = c("90-100", "80-89", "70-79", "60-69", "50-59", "<=49"),
  Grade = c("A", "B", "C", "D", "E", "F"),
  Number_of_students = as.numeric(c(grade_counts["A"], grade_counts["B"], grade_counts["C"], 
                                    grade_counts["D"], grade_counts["E"], grade_counts["F"]))
)

# print output
cat("--- Individual Student Records (Score, Grade, Pass Status) ---\n")
print(student_records)

cat("\n--- Number of Students per Grade ---\n")
print(summary_table)