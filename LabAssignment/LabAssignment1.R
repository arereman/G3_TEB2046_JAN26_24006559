library(tidyverse)
library(stringr)
library(lubridate)

# =====================================================================
# Deep Structural Repair (Pre-Dataframe text manipulation)
# =====================================================================

raw_text <- read_file("Unclean_Dataset.csv")

# Mend broken rows: Remove newlines that follow a pipe
cleaned_text <- str_replace_all(raw_text, "\\|\\s*\\n\\s*", "|")

# Standardize delimiters: Replace all pipes and slashes with commas
cleaned_text <- str_replace_all(cleaned_text, "[|/]", ",")

# Break glued records and strip trailing garbage.
cleaned_text <- str_replace_all(cleaned_text, "(\\$[0-9.]+|\"[^\"]+\")\\s*,+", "\\1\n")

# Read into dataframe as characters
df <- read_csv(cleaned_text, col_names = FALSE, show_col_types = FALSE, col_types = cols(.default = "c"))

# Enforce 8 columns safely. 
df <- df[, 1:min(8, ncol(df))]
if(ncol(df) < 8) {
  for(i in (ncol(df)+1):8) df[[paste0("V", i)]] <- NA
}

# Apply headers and remove the old header row
colnames(df) <- c("Student_ID", "First_Name", "Last_Name", "Age", "Gender", "Course", "Enrollment_Date", "Total_Payments")
df <- df[-1, ]


# Fix anomaly where Names were typed into the Student_ID column
is_name_in_id <- str_detect(df$Student_ID, "[A-Za-z]") & !is.na(df$Student_ID)
df$First_Name[is_name_in_id] <- df$Student_ID[is_name_in_id]
df$Student_ID[is_name_in_id] <- NA

df <- df %>%
  mutate(
    across(everything(), str_trim),
    
    # Safely preserve Enrollment Dates
    Enrollment_Date = na_if(Enrollment_Date, "NA"),
    Enrollment_Date = na_if(Enrollment_Date, ""),
    
    # --- Mend Data Type Outliers ---
    corrected_age = case_when(
      str_detect(Age, "^[A-Za-z]") & str_detect(Gender, "\\d+") ~ Gender,
      TRUE ~ Age
    ),
    corrected_gender = case_when(
      str_detect(Age, "^[A-Za-z]") & str_detect(Gender, "\\d+") ~ Age,
      TRUE ~ Gender
    ),
    Age = corrected_age,
    Gender = corrected_gender,
    Course = if_else(str_detect(Course, "^\\d+$"), NA_character_, Course),
    # -------------------------------------------------
    
    # --- Parse 2-word First Names ---
    extracted_last = word(First_Name, 2, -1),
    Last_Name = coalesce(extracted_last, Last_Name),
    First_Name = word(First_Name, 1),
    # -----------------------------------------------
    
    # Standardize Course typos
    Course = case_when(
      str_detect(Course, "(?i)Machine Learnin") ~ "Machine Learning",
      str_detect(Course, "(?i)Web Developmet|Web Develpment") ~ "Web Development",
      str_detect(Course, "(?i)Data Analysis|Data Analytics") ~ "Data Analytics", 
      TRUE ~ Course
    ),
    
    # Salvage ages hidden in Gender, clean Age column of '*', and cast to numeric
    Age = coalesce(str_extract(Age, "\\d+"), str_extract(Gender, "\\d+")),
    Age = as.numeric(Age),
    
    # Standardize Gender
    Gender = str_to_upper(str_extract(Gender, "^[A-Za-z]"))
  ) %>%
  select(-extracted_last, -corrected_age, -corrected_gender)

# --- Handle Duplicate Data Points ---

df <- df %>%
  distinct(Student_ID, First_Name, Last_Name, .keep_all = TRUE)

# --- Drop rows with more than 2 Null values ---

df <- df %>%
  filter(rowSums(is.na(.)) <= 2)

# --- Standardize Dates and Currency ---

df <- df %>%
  mutate(
    # 1. Standardize Dates
    # Remove stray '#' symbols
    Enrollment_Date = str_replace_all(Enrollment_Date, "#", ""),
    # Parse American (mdy), Standard (ymd), and Text formats (dby)
    parsed_date = parse_date_time(str_trim(Enrollment_Date), orders = c("mdy", "ymd", "dby")),
    # Format uniformly to dd-mm-yy
    Enrollment_Date = format(parsed_date, "%d-%m-%y"),
    
    # 2. Standardize Currency
    # Extract just the numeric components (ignoring commas/spaces)
    numeric_payment = str_extract(Total_Payments, "[0-9.,]+"),
    # Determine the correct currency symbol
    currency_symbol = case_when(
      str_detect(Total_Payments, "\\$") ~ "$",
      str_detect(Total_Payments, "\\?") ~ "£",
      !is.na(Total_Payments) ~ "¥", # Anything else becomes Chinese Yuan
      TRUE ~ NA_character_
    ),
    # Recombine the symbol with the clean number
    Total_Payments = if_else(is.na(numeric_payment), NA_character_, paste0(currency_symbol, numeric_payment))
  ) %>%
  select(-parsed_date, -numeric_payment, -currency_symbol)

# ID Distribution (Handling Collisions and Missing)

df$Student_ID <- suppressWarnings(as.numeric(df$Student_ID))

df$Student_ID[duplicated(df$Student_ID, incomparables = NA)] <- NA

existing_ids <- na.omit(unique(df$Student_ID))
if(length(existing_ids) > 0) {
  all_possible_ids <- seq(min(existing_ids), max(existing_ids) + sum(is.na(df$Student_ID)) + 100)
  available_ids <- setdiff(all_possible_ids, existing_ids)
  df$Student_ID[is.na(df$Student_ID)] <- available_ids[1:sum(is.na(df$Student_ID))]
}

# Save Result

write_csv(df, "Cleaned_Dataset.csv")
print("Cleaned_Dataset.csv generated.")