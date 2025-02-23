# Read in, clean up and output into a single file the responses to prize draw entry questions
# Only for Simple link version
# Gorilla: questionnaire-hykf
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.demo() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'demo_sona').
# 3) For subsequent data files, use function clean.demo() again, but assign output to 'temp' and add rows to the original data frame.
# 4) Save results in new data file

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
pd_sl_feb25 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_209080-v4_20250215", "data_exp_209080-v4_questionnaire-hykf.csv"))

# 2) For first data file --------
n <- check.n(pd_sl_feb25) # check number of participants in data file

pd <- pd_sl_feb25 %>%
  filter(`Response Type` == "response") %>%
  filter(Key != "quantised") %>%
  filter(Key != "value") %>%
  select("Participant Private ID", "Task Name", "Question", "Response Type", "Key", "Response") %>%
  pivot_wider(names_from = Key, values_from = Response) %>%
  drop_na(`Name-value`) %>%
  rename(name = `Name-value`,
         email = `Email address-value`) %>%
  select(name, email)

# 4) Save results in new data file -----------
write_csv(pd, here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "pd_20250220.csv"))

test <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "pd_20250220.csv"))






