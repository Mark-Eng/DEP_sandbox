library(tidyverse)

# Set the working directory to the folder containing the files
setwd("C:/Users/ME/Desktop/R/DEP_sandbox/merge_sandbox/Medline Results")

# Create a subfolder for merged files
dir.create("Merged", showWarnings = FALSE)

# List all files in the folder
files <- list.files(pattern = "*.ris")

# Extract the numeric values from the filenames
file_numbers <- str_extract(files, "\\((\\d+)-(\\d+)\\)") %>%
  str_extract("\\d+") %>%
  as.numeric()

# Order files based on the lower bound of the range
files_sorted <- files[order(file_numbers)]

# Function to merge and write two files at a time
merge_and_write <- function(file1, file2) {
  # Read the contents of the two files
  content1 <- readLines(file1)
  content2 <- readLines(file2)
  
  # Combine the contents
  merged_content <- c(content1, content2)
  
  # Extract the range from the file names
  range1 <- str_extract(file1, "\\((\\d+)-(\\d+)\\)")
  range2 <- str_extract(file2, "\\((\\d+)-(\\d+)\\)")
  
  # Replace 'NA' with the upper bound in the second range
  range2[2] <- ifelse(is.na(range2[2]), range1[2], range2[2])
  
  # Create the output file name
  output_name <- gsub("\\((\\d+)-(\\d+)\\)", sprintf("(%s-%s)", range1[1], range2[2]), file1)
  
  # Write the merged content to the Merged folder
  writeLines(merged_content, file.path("Merged", output_name))
}

# Iterate through the sorted files and merge them two at a time
for (i in seq(1, length(files_sorted), by = 2)) {
  file1 <- files_sorted[i]
  file2 <- files_sorted[i + 1]
  
  # Perform the merge and write
  merge_and_write(file1, file2)
}
