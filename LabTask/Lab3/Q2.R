# store the student records in a list and give names to the elements
student_records <- list(
  Robert = 59,
  Hemsworth = 71,
  Scarlett = 83,
  Evans = 68,
  Pratt = 65,
  Larson = 57,
  Holland = 62,
  Paul = 92,
  Simu = 92,
  Renner = 59
)

# convert list to a numeric vector so we can easily perform math on it
scores <- unlist(student_records)

# find highest, lowest, and average exam scores
highest_score <- max(scores)
lowest_score  <- min(scores)
average_score <- mean(scores)

# find names of students with those specific scores
top_students    <- paste(names(scores)[scores == highest_score], collapse = ", ")
bottom_students <- paste(names(scores)[scores == lowest_score], collapse = ", ")

# print output
cat("Highest Score:", highest_score, "\n")
cat("Lowest Score:", lowest_score, "\n")
cat("Average Score:", average_score, "\n")
cat("Student with highest score:", top_students, "\n")
cat("Student with lowest score:", bottom_students, "\n")