library(tidyverse)

# ---------
# Look for log book files and wrangle the important information

files_in_wd <- list.files()

log_book_name <- files_in_wd[str_detect(files_in_wd, ".xlsx")]

log_book <- readxl::read_xlsx(log_book_name, sheet = 1, skip = 6)

#log_book <- read_csv("200204_D_MaxHohl_MtPy_Imgs_LogBook.csv", skip = 6)

log_book_samples <- log_book |>
  mutate(n = seq_len(n())) |>
  filter(SampleType == "Sample") |>
  mutate(Sample = str_remove(Sample, "\ -\ \\d\\d|\ -\ \\d"))

sample_parameters <- log_book_samples |>
  distinct(Sample, .keep_all = TRUE) |>
  select(Sample, BeamSize, ScanSpeed)

# Check if the file exists.
write_csv(sample_parameters, "Sample_Parameters.csv")

cf_files <- sample_parameters |>
  distinct(BeamSize, ScanSpeed)

# --------------
# Create directories for the laser lines and images (Data and Results)
#

samples <- pull(sample_parameters, Sample)

main_dir <- c("/home/max/Documents/Coding_Projects/R/Laser_Maps")
sub_dir_data <- paste0(main_dir, "/Data")
sub_dir_results <- paste0(main_dir, "/Results")

create_dir <- function(main_path, data_result, samples) {
  if (data_result == "data" | data_result == "Data") {
    sub_dir <- paste0(main_path, "/", "Data")
  } else if (data_result == "results" | data_result == "Results") {
    sub_dir <- paste0(main_path, "/", "Results")
  } else {
    stop()
  }

  if(!dir.exists(paste0(sub_dir))) {
    dir.create(paste0(sub_dir))
  } else {
    print(paste0(data_result, " directory already exists"))
  }

  for (sample in samples) {
    sample_dir <- paste0(sub_dir, "/", sample)
    ifelse(!dir.exists(sample_dir),
           dir.create(sample_dir),
           print(paste0(data_result, "/", sample, " already exists"))) # Remove later but good check
  }
}

create_dir(main_path = main_dir, data_result = "Results", samples=samples)

create_dir(main_path = main_dir, data_result = "Data", samples=samples)

# ------------
# Copy lines in the respective data folders

lines_dir <- files_in_wd[str_detect(files_in_wd, "lines")]

lines_all <- list.files(paste0(lines_dir, "/"))

lines_number <- lines_all[str_detect(lines_all, "\\d\\d\\d\\d.csv")]

for (sample in samples) {
  line_indices <- log_book_samples |>
    filter(Sample == sample) |>
    pull(n)

  file.copy(from = paste0(lines_dir, "/", lines_number[line_indices]),
          to = paste0(main_dir, "/Data/", sample),
          copy.mode = TRUE)
}

#line_indices <- log_book_samples |>
#  filter(Sample == sample) |>
#  pull(n)
#
#lines_number[line_indices]
#
#file.copy(from = paste0(lines_dir, "/", lines_number[line_indices]),
#          to = paste0(main_dir, "/Data/", sample),
#          copy.mode = TRUE)
