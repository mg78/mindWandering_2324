# Read in, clean up and output into a single file the responses to demographics questions 
# Gorilla: questionnaire-hpzd
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.demo() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'demo_dys').
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

clean.demo <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Key", "Response", "Object Name") %>%
    filter(!.data$Response == "BEGIN") %>%
    filter(!.data$Response == "END") %>%
    filter(.data$`Key` == "value") %>%
    pivot_wider(names_from = `Object Name`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           age = `Age`,
           gender = Gender) %>%
    mutate(gender = recode(gender,
                           "1"="man",
                           "2"="woman",
                           "3"="non-binary",
                           "4"="other")) %>%
    mutate(origID = as.factor(origID),
           age = as.numeric(age),
           gender = as.factor(gender)) %>%
    select(origID, age, gender)
}

# 1) Read in data files -----------
demo_sona_aug25 <- read_csv(here("mw2425_06data/halleWarren/data_exp_233799-v28_20250814", "data_exp_233799-v28_questionnaire-hpzd.csv"))
demo_sl_aug25 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_questionnaire-hpzd.csv"))
demo_sl2_aug25 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v9_20250814", "data_exp_234742-v9_questionnaire-hpzd.csv"))

# 2) For first data file -----------
n <- check.n(demo_sl_aug25) # check number of participants in data file
demo_dat <- clean.demo(demo_sl_aug25) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------
## demo_sl2_aug25
n <- check.n(demo_sl2_aug25) # check number of participants in data file
temp <- clean.demo(demo_sl2_aug25) # subsequent data files: pivot, select and rename relevant variables

demo_dat <- demo_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

## demo_sona_aug25
n <- check.n(demo_sona_aug25) # check number of participants in data file
temp <- clean.demo(demo_sona_aug25) # subsequent data files: pivot, select and rename relevant variables

demo_dat <- demo_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Save results in new data file -----------
write_csv(demo_dat, here("mw2425_06data/halleWarren/mw2425_hw_processed", "demo_dat_20250814.csv"))

test <- read_csv(here("mw2425_06data/halleWarren/mw2425_hw_processed", "demo_dat_20250814.csv"))



####################################
# Original piped code
demo_dys <- demo_sona_jan22 %>%
  select(`Participant Private ID`, `Task Name`, `Task Version`, `Question Key`, `Response`) %>%
  filter(!`Question Key` == "BEGIN QUESTIONNAIRE") %>%
  filter(!`Question Key` == "END QUESTIONNAIRE") %>%
  pivot_wider(names_from = `Question Key`, values_from = Response) %>%
  rename(origID = `Participant Private ID`,
         age = Age,
         gender = Gender,
         gender_text = `Gender-text`,
         bilingual_text = `bilingual-text`,
         handedness = categorical_hand) %>%
  mutate(origID = as.factor(origID)) %>%
  select(origID, age, gender, gender_text, bilingual, bilingual_text, handedness)



