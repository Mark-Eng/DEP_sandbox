library(openxlsx)
library(tidyverse)
library(janitor)

setwd("C:\\Users\\ME\\OneDrive - 3ie\\Mark's work folder\\FSN\\Living EGM\\Duplicate analysis\\2023-07")

file_pattern <- "dup_report\\d+\\.xlsx"
file_paths <- list.files(pattern = file_pattern, full.names = TRUE)

dfs <- lapply(file_paths, function(path) read.xlsx(path, fillMergedCells = TRUE))

fsn_dup <- bind_rows(dfs)

fsn_dup <- fsn_dup %>%
  clean_names() %>%
  filter(grepl("FSN", duplicates))

master_ids <- fsn_dup %>%
  select(master_id) %>%
  unique() %>%
  pull(master_id)

master_id_string <- paste(master_ids, collapse = ",")

# Splitting the master_id_string into chunks of 444 values
chunk_size <- 444
chunks <- strsplit(master_id_string, ",", fixed = TRUE)[[1]]  # Extracting the first element of the list
num_chunks <- ceiling(length(chunks) / chunk_size)

# Writing each chunk to separate text files
for (i in 1:num_chunks) {
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, length(chunks))
  
  current_chunk <- chunks[start:end]
  current_chunk_string <- paste(current_chunk, collapse = ",")
  
  file_name <- paste("master_ids", i, ".txt", sep = "")
  writeLines(current_chunk_string, file_name)
}








