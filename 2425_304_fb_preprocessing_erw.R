# mw_304_fb_preprocessing

# Data collected as part of 304 projects 2024-25, using a questionnaire to ask about use of calculator, 
# phone and general environment and experience.
# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla: questionnaire-23s2

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

# 1) Read in data files -----------
fb_sona_feb25 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_201657-v10_20250215", "data_exp_201657-v10_questionnaire-23s2.csv"))
fb_sl_feb25 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_209080-v4_20250215", "data_exp_209080-v4_questionnaire-23s2.csv"))

# 2) Combine data files
nsub <- check.n(fb_sona_feb25) # check number of participants in data file
fb_dat <- fb_sona_feb25

n <- check.n(fb_sl_feb25) # check number of participants in data file
temp <- fb_sl_feb25 # subsequent data files: pivot, select and rename relevant variables
fb_dat <- fb_dat %>%
  add_row(temp)

# 3) Once dataframe with participants from all setups is created, pivot separately for 2 sorts of questions
fb1 <- fb_dat %>%
  select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Question", "Key", "Response") %>%
  filter(!.data$Response == "BEGIN") %>%
  filter(!.data$Response == "END") %>%
  filter(.data$Key == "value") %>%
  pivot_wider(names_from = Question, values_from = Response) %>%
  rename(origID = `Participant Private ID`) %>%
  mutate(origID = as.factor(origID))

fb2 <- fb_dat%>%
  select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Question", "Key", "Response") %>%
  filter(!.data$Response == "BEGIN") %>%
  filter(!.data$Response == "END") %>%
  filter(!.data$Key == "value") %>%
  filter(!.data$Key == "quantised") %>%
  filter(!.data$Key == "other") %>%
  pivot_wider(names_from = Key, values_from = Response) %>%
  rename(origID = `Participant Private ID`) %>%
  mutate(origID = as.factor(origID))
  
# 4) Combine pivoted tables and tidy up variable names

fb <- fb1 %>%
  left_join(fb2) %>%
  select(-`Task Name`, -`Task Version`, -`Response Type`, -Key)

# 5) Save results in new data file -----------
write_csv(fb, here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "fb_20250215.csv"))

test <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "fb_20250215.csv"))
