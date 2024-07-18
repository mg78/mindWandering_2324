# Read in, clean up and output into a single file the responses to prize draw questions 
# Gorilla: questionnaire-3e69
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files

# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# 1) Read in data files -----------
pd_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240318", "data_exp_158033-v3_questionnaire-3e69.csv"))

# 2) 
pd <- pd_sl_feb24 %>%
  select(`Event Index`, `Participant Private ID`, `Task Name`, `Response`) %>%
  filter(`Event Index` == 3)

# 2b)
# Also need to get names for list to be returned to department admin

pd <- pd_sl_feb24 %>%
  select(`Event Index`, `Participant Private ID`, `Task Name`, `Response`) %>%
  filter(`Event Index` == 2 | `Event Index` == 3)

# 3) Save results in new data file -----------
write_csv(pd, here("mw2324_06data/mw2324_processed", "pd_dat_20240710.csv"))





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



