library(tidyverse)

sample_parameters <- read_csv("Sample_Parameters.csv") |>
  mutate(across(.cols = 2:3, round)) |>
  mutate(Parameters = paste0(BeamSize, "_", ScanSpeed))

cf_wrangler <- function(cf_file) {

  cf_df <- read_delim(cf_file, delim = ",", skip = 62)
  standards = c('STDGL3','GSD-1G','NIST610','NIST612','BCR-2G','PeruPy','Po724-T','STDGL2B2') #standards which can be present in LADR output to be excluded when calculating correction factors

  isotopes <- names(cf_df)[str_detect(names(cf_df), "\\d\\d\\d[:alpha:]|\\d\\d[:alpha:]|\\d[:alpha:]")]

  cf_na <- cf_df |>
    filter(!Sample %in% standards) |>
    mutate(across(.cols = isotopes, .fns = as.numeric)) |>
    mutate(across(.cols = isotopes, ~if_else(. <= 0, NA, .)))

  cf_avg <- left_join(
    cf_na |>
      group_by(Sample) |>
      summarize(across(.cols = isotopes, mean, na.rm = TRUE)) |>
      pivot_longer(names_to = "Isotopes", values_to = "Sum", cols = isotopes),
    cf_na |>
      mutate(across(.cols = isotopes, ~sum(!is.na(.)))) |>
      distinct(Sample, .keep_all = TRUE) |>
      select(Sample, isotopes) |>
      pivot_longer(names_to = "Isotopes", values_to = "NA_n", cols = isotopes)
  ) |>
    mutate(Cf_mean = Sum/NA_n)

  if (str_detect(cf_file, "ppm")) {
    return(cf_avg |> rename(Ppm_mean = Cf_mean))
  } else {
    return(cf_avg |> rename(Cps_mean = Cf_mean))
  }
}

# Get the right cf files
cf_files <- list.files(here::here(), "*.cf")

cf_endings <- sample_parameters |>
  distinct(Parameters) |> pull()

cf_endings <- cf_endings[4] # Just for testing purposes

# This is likely to be the main loop. Read the right cf file based on the ending, by
# using the cf_wrangler() function.

for (ending in cf_ending) {
print(ending)}

for (ending in cf_endings) {
  samples <- sample_parameters |>   # Make sure only the samples corresponding to the cf parameters are selected
    filter(Parameters == ending) |> pull(Sample)

  ppm_cf <- cf_files[str_detect(cf_files, paste0("ppm_", ending))]
  cps_cf <- cf_files[str_detect(cf_files, paste0("cps_", ending))]


 cf_cps_avg <- cf_wrangler(cf_file=cps_cf)
 cf_ppm_avg <- cf_wrangler(cf_file=ppm_cf)

 cf_conversion <- left_join(
   cf_ppm_avg |> select(Isotopes, Ppm_mean),
   cf_cps_avg |> select(Isotopes, Cps_mean),
   by = c("Isotopes")) |>
   mutate(Conversion_Factor = Ppm_mean/Cps_mean) |>
   select(Isotopes, Conversion_Factor) |>
   mutate(Isotope_numbers = str_extract(Isotopes, pattern = "\\d\\d\\d|\\d\\d|\\d")) |>
   mutate(Isotopes = str_remove(Isotopes, "\\d\\d\\d|\\d\\d|\\d")) |>
   mutate(Isotopes = paste0(Isotopes, Isotope_numbers)) |>
   select(-Isotope_numbers)

 # Look and load for lines for each sample ----------------------------------
 #
 isotopes <- cf_conversion |> pull(Isotopes)

 for (sample in samples) {
   lines_per_sample <- list.files(here::here("Data", sample)) # Cp data files again!

   beam_size <- sample_parameters |>
     filter(Sample == sample) |>
     pull(BeamSize)

   beam_size_y <- beam_size
   ppm_df <- tibble()
   cps_df <- tibble()

   for (line in lines_per_sample) {

     # Read each line for the sample
     line_df <- read_csv(here::here("Data", sample, line), skip = 3) |>
       mutate(`Time [Sec]` = as.numeric(`Time [Sec]`)) |>
       filter(!is.na(`Time [Sec]`)) |>
       filter(`Time [Sec]` >= 11) # make this a variable

     cf_conversion_rep <- cf_conversion |>
         pivot_wider(values_from = Conversion_Factor,
                     names_from = Isotopes) |>
         replicate(n = nrow(line_df), simplify = FALSE) |>
         bind_rows()

     # Convert cps for each line into ppm
     line_ppm <- bind_cols(line_df[1], line_df[names(cf_conversion_rep)]*cf_conversion_rep) |>
       rename("x" = 1) |>
       mutate(xy = beam_size) |>
       mutate(x = cumsum(xy)) |> # changes time column into spot size
       mutate("y" = beam_size_y) |> # increment y with each for loop
       select("x", "y", isotopes)

     line_cps <- line_df |>
       rename("x" = 1) |>
       mutate(xy = beam_size) |>
       mutate(x = cumsum(xy)) |>
       mutate("y" = beam_size_y) |>
       select("x", "y", isotopes)

     beam_size_y = beam_size_y + beam_size

     ppm_df <- bind_rows(ppm_df, line_ppm)
     cps_df <- bind_rows(cps_df, line_cps)
   }
   write_csv(ppm_df, file = paste0(here::here("Results"), "/", sample, "/", sample, "_ppm.csv"))
   write_csv(cps_df, file = paste0(here::here("Results"), "/", sample, "/", sample, "_cps.csv"))
 }
}


# Things still missing:
#   Subtract the gas blank from cps and ppm files
#   A way to handle 0 values and NA values
#   Plot the maps
