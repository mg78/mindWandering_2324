# mw_304_fb_preprocessing

# Data collected as part of 304 projects 2024-25, asking for feedback on the experiment.
# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla: questionnaire-x1jo

# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.arhq() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'mw_dat').
# 3) For subsequent data files, use function clean.feng_easy() again, but assign output to 'temp' and add rows to the original data frame.
# 4) Once dataframe with participants from all setups is created, score responses and calculate score
# 5) Save results in new data file

# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)
                       
# Functions
## check n participants
check.n <- function(data){
  obs <- length(data$`Event Index`)
  data_n <- data.frame(matrix(ncol = 1, nrow = obs))
  data_n$origID<- data$`Participant Private ID`
  data_n$origID <- as.factor(data_n$origID)
  nsub <-length(levels(data_n$origID)) ### count and print subjects
  print(paste0("subjects: ", nsub))
}

clean.fb <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question", "Response Type", "Key",
           "Response")
}

# 1) Read in data files -----------
fb_sona <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_questionnaire-x1jo.csv"))
fb_sl <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_questionnaire-x1jo.csv"))

# 2) Combine data files
nsub <- check.n(fb_sona) # check number of participants in data file
fb_dat <- clean.fb(fb_sona)

n <- check.n(fb_sl) # check number of participants in data file
temp <- clean.fb(fb_sl) # subsequent data files: pivot, select and rename relevant variables
fb_dat <- fb_dat %>%
  add_row(temp)

# 3) Once dataframe with participants from all setups is created, pivot separately for 2 sorts of questions
fb <- fb_dat %>%
  select("Participant Private ID", "Question", "Key", "Response") %>%
  filter(.data$Key == "value") %>%
  pivot_wider(names_from = Question, values_from = Response) %>%
  rename(origID = `Participant Private ID`) %>%
  mutate(origID = as.factor(origID)) %>%
  select(-Key)

# 5) Save results in new data file -----------
write_csv(fb, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "fb_20250402.csv"))

test <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "fb_20250402.csv"))
