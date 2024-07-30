# Read in, clean up and output into a single file the responses to the OCD questionnaire
# Gorilla: questionnaire-ephm
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.ocd() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'ocd_dat').
# 3) For subsequent data files, use function clean.ocd() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.ocd <- function(data){
  data %>%
    select ("Participant Private ID", "Task Version", "Question", "Key", "Response") %>%
    filter(Key == "quantised") %>%
    select(-`Task Version`, -Key) %>%
    rename(origID = `Participant Private ID`) %>%
    mutate(Response = as.numeric(Response))
}

# 1) Read in data files -----------
ocd_sona_jul24 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_180633-v7_20240730", "data_exp_180633-v7_questionnaire-ephm.csv"))
ocd_sl_jul24 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_179131-v14_20240730", "data_exp_179131-v14_questionnaire-ephm.csv"))

# 2) For first data file -----------
nsub <- check.n(ocd_sona_jul24) # check number of participants in data file
ocd_dat <- clean.ocd(ocd_sona_jul24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## ocd_sl_jul24
n <- check.n(ocd_sl_jul24) # check number of participants in data file
temp <- clean.ocd(ocd_sl_jul24) # subsequent data files: pivot, select and rename relevant variables
ocd_dat <- ocd_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Calculate score -----------
ocd_score <- ocd_dat %>%
  group_by(origID) %>%
  summarise(ocd_total = sum(Response, na.rm = TRUE))

# 5) Save results in new data file -----------
write_csv(ocd_score, here("mw2324_06data/mw2324_processed", "ocd_score_20240730.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "ocd_score_20240730.csv"))
