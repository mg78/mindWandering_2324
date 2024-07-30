# Read in, clean up and output into a single file the responses to demographics questions 
# Gorilla: questionnaire-2hcl
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.demo() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'demo_dys').
# 3) For subsequent data files, use function clean.demo() again, but assign output to 'temp' and add rows to the original data frame.
# 4) Save results in new data file

## Amended on 30th July 2024 to adapt to Molly's experiment.

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

clean.demo <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    pivot_wider(names_from = `Question Key`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           age = Age,
           gender = Gender,
           gender_text = `Gender-text`,
           ocd_diag = OCD_Diagnosis) %>%
    mutate(origID = as.factor(origID),
           age = as.numeric(age)) %>%
    select(origID, age, gender, gender_text, multilingual, ocd_diag)
}

# 1) Read in data files -----------
demo_sona_jul24 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_180633-v7_20240730", "data_exp_180633-v7_questionnaire-2hcl.csv"))
demo_sl_jul24 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_179131-v14_20240730", "data_exp_179131-v14_questionnaire-2hcl.csv"))

# 2) For first data file -----------
n <- check.n(demo_sona_jul24) # check number of participants in data file
demo_dat <- clean.demo(demo_sona_jul24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------
## demo_sl_jul24
n <- check.n(demo_sl_jul24) # check number of participants in data file
temp <- clean.demo(demo_sl_jul24) # subsequent data files: pivot, select and rename relevant variables

demo_dat <- demo_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Save results in new data file -----------
write_csv(demo_dat, here("mw2324_06data/mw2324_processed", "demo_dat_molly_20240730.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "demo_dat_molly_20240730.csv"))




