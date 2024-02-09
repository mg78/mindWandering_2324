# Read in, clean up and output into a single file the responses to the ASRS
# Gorilla: questionnaire-bb43
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.asrs() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'asrs_dat').
# 3) For subsequent data files, use function clean.asrs() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.asrs <- function(data){
  data %>%
    select ("Participant Private ID", "Task Version", "Question", "Key", "Response") %>%
    filter(Key == "quantised") %>%
    select(-`Task Version`, -Key) %>%
    rename(origID = `Participant Private ID`) %>%
    mutate(Response = as.numeric(Response))
}



# 1) Read in data files -----------
asrs_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10-3_20240207", "data_exp_155541-v10_questionnaire-bb43.csv"))
asrs_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240207", "data_exp_158033-v3_questionnaire-bb43.csv"))

# 2) For first data file -----------
nsub <- check.n(asrs_sona_feb24) # check number of participants in data file
asrs_dat <- clean.asrs(asrs_sona_feb24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## asrs_sl_feb24
n <- check.n(asrs_sl_feb24) # check number of participants in data file
temp <- clean.asrs(asrs_sl_feb24) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Calculate score -----------
asrs_score <- asrs_dat %>%
  group_by(origID) %>%
  summarise(asrs_total = sum(Response, na.rm = TRUE))

# 5) Save results in new data file -----------
write_csv(asrs_score, here("mw2324_06data/mw2324_processed", "asrs_score_20240209.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "asrs_score_20240209.csv"))

# OLD CODE-------------------
  rename(
    "Q1" = "When you have a task that requires a lot of thought, do you avoid or delay getting started?",
    "Q2" = "Do you have difficulty getting things in order when you have to do a task that requires organization?",
    "Q3" = "Do you have trouble wrapping up the fine details of a project, once the challenging parts have been done?",
    "Q4" = "Do you fidget or squirm with your hands or your feet when you have to sit down for a long time?",
    "Q5" = "Do you have problems remembering appointments or obligations?",
    "Q6" = "Do you feel overly active and compelled to do things, like you were driven by a motor?"
  ) %>%
  mutate(Q1 = as.numeric(Q1), #modify to numeric values
         Q2 = as.numeric(Q2),
         Q3 = as.numeric(Q3),
         Q4 = as.numeric(Q4),
         Q5 = as.numeric(Q5),
         Q6 = as.numeric(Q6))%>%
  group_by(`Schedule ID`) %>%
  mutate(ADHD_Score = sum(Q1, Q2, Q3, Q4, Q5, Q6)) #create a sum score
