# Read in, clean up and output into a single file the responses to the ARHQ
# Gorilla: questionnaire-gew1
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.asd() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'asd_dat').
# 3) For subsequent data files, use function clean.asd() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.asd <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    filter(grepl("-quantised", .data$`Question Key`)) %>%
    rename(origID = `Participant Private ID`)
}

# 1) Read in data files -----------
asd_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10-3_20240207", "data_exp_155541-v10_questionnaire-gew1.csv"))
asd_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240207", "data_exp_158033-v3_questionnaire-gew1.csv"))

# 2) For first data file -----------
nsub <- check.n(asd_sona_feb24) # check number of participants in data file
asd_dat <- clean.asd(asd_sona_feb24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## asd_sl_feb24
n <- check.n(asd_sl_feb24) # check number of participants in data file
temp <- clean.asd(asd_sl_feb24) %>% # subsequent data files: pivot, select and rename relevant variables
  mutate(Response = as.numeric(Response))
  
asd_dat <- asd_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Calculate score -----------
asd_score <- asd_dat %>%
  group_by(origID) %>%
  summarise(asd_total = sum(Response, na.rm = TRUE))

# 5) Save results in new data file -----------
write_csv(asd_score, here("mw2324_06data/mw2324_processed", "asd_score_20240209.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "asd_score_20240209.csv"))
