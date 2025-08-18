# Read in, clean up and output into a single file the responses to the ASRS
# Gorilla: questionnaire-5zpv and questionnaire-6kdr
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
    filter(Key == "quantised" | Key == "score") %>%
    select(-`Task Version`, -Key) %>%
    rename(origID = `Participant Private ID`) %>%
    mutate(Response = as.numeric(Response)) %>%
    mutate(Response_rc = recode(Response,
                                "1" = "0",
                                "2" = "1",
                                "3" = "2",
                                "4" = "3",
                                "5" = "4")) %>%
    mutate(Response_rc = as.numeric(Response_rc))
}

# 1) Read in data files -----------
asrs_slv8_aug25b1 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_questionnaire-5zpv.csv"))
asrs_slv9_aug25b1 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v9_20250818", "data_exp_234742-v9_questionnaire-5zpv.csv"))
asrs_sona_aug25b1 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_233799-v28_20250818", "data_exp_233799-v28_questionnaire-5zpv.csv"))
asrs_slv8_aug25b2 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_questionnaire-6kdr.csv"))
asrs_slv9_aug25b2 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v9_20250818", "data_exp_234742-v9_questionnaire-6kdr.csv"))
asrs_sona_aug25b2 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_233799-v28_20250818", "data_exp_233799-v28_questionnaire-6kdr.csv"))

# 2) For first data file -----------
nsub <- check.n(asrs_slv8_aug25b1) # check number of participants in data file
asrs_dat <- clean.asrs(asrs_slv8_aug25b1) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## asrs_slv9_aug25b1
n <- check.n(asrs_slv9_aug25b1) # check number of participants in data file
temp <- clean.asrs(asrs_slv9_aug25b1) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

## asrs_sona_aug25b1
n <- check.n(asrs_sona_aug25b1) # check number of participants in data file
temp <- clean.asrs(asrs_sona_aug25b1) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

## asrs_slv8_aug25b2
n <- check.n(asrs_slv8_aug25b2) # check number of participants in data file
temp <- clean.asrs(asrs_slv8_aug25b2) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

## asrs_slv9_aug25b2
n <- check.n(asrs_slv9_aug25b2) # check number of participants in data file
temp <- clean.asrs(asrs_slv9_aug25b2) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

## asrs_sona_aug25b2
n <- check.n(asrs_sona_aug25b2) # check number of participants in data file
temp <- clean.asrs(asrs_sona_aug25b2) # subsequent data files: pivot, select and rename relevant variables
asrs_dat <- asrs_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Calculate score -----------
# Following Kessler et al. 2005, two scores are derived:
# 1) the total of responses across all six items (asrs_score), with a minimum of 0 and a maximum of 24 (6 x 4);
# 2) a dichotomous scoring where responses that scored ‘0’ indicated no risk and ‘1’ at risk (asrs_screen). For Items 1, 2 and 3 (Never, Rarely = 0,
# Sometimes, Often, Very Often =1) and items 4, 5 and 6 were the same besides ‘Sometimes’ = 0. Participants who totalled three or less were
# categorised as “Low Risk of ADHD” and four or more as “High risk of ADHD”. The higher the total score from 0-6, the more risk a
# participant had ADHD.

asrs_score <- asrs_dat %>%
  filter(Question != "ADHD_Risk") %>%
  group_by(origID) %>%
  summarise(asrs_total = sum(Response_rc, na.rm = TRUE))

asrs_screen <- asrs_dat %>%
  filter(Question == "ADHD_Risk") %>%
  rename(adhd_risk = Response) %>%
  select(-Question, -Response_rc)

asrs_all <- asrs_score %>%
  left_join(asrs_screen)

# 5) Save results in new data file -----------
write_csv(asrs_all, here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "asrs_all_20250818.csv"))

test <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "asrs_all_20250818.csv"))

#######
# Following Kessler et al. 2005, two scores are derived:
# 1) the total of responses across all six items (asrs_score), with a minimum of 0 and a maximum of 24 (6 x 4);
# 2) a dichotomous scoring where responses that scored ‘0’ indicated no risk and ‘1’ at risk (asrs_screen). For Items 1, 2 and 3 (Never, Rarely = 0,
# Sometimes, Often, Very Often =1) and items 4, 5 and 6 were the same besides ‘Sometimes’ = 0. Participants who totalled three or less were
# categorised as “Low Risk of ADHD” and four or more as “High risk of ADHD”. The higher the total score from 0-6, the more risk a
# participant had ADHD.
# asrs_total: Sum of responses (ranging from 0 for Never to 4 for Very Often) on all questions
# asrs_screen: Using dichotic scoring outlined by Kessler et al., 2025 to capture risk

