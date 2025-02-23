# mw_304_mw_preprocessing

# Data collected as part of 304 projects 2024-25, using a mental maths paradigm, but with easy and difficult version of
# all 4  operations (addition, subtraction, multiplication, division).
# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla: set 1 (eDeD) -> task-idt7 
#          set 2 (dEdE) -> task-drth

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

clean.mw <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version","Spreadsheet", "Display", "Trial Number",
           "Response Type", "Response", "Object Name", "Object Number", "Reaction Time",
           "Spreadsheet: display":"Spreadsheet: item_correct") %>%
    filter(.data$`Response Type` == "response")
}

# 1) Read in data files -----------
mw_sona_feb25_set1 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_201657-v10_20250215", "data_exp_201657-v10_task-idt7.csv"))
mw_sona_feb25_set2 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_201657-v10_20250215", "data_exp_201657-v10_task-drth.csv"))
mw_sl_feb25_set1 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_209080-v4_20250215", "data_exp_209080-v4_task-idt7.csv"))
mw_sl_feb25_set2 <- read_csv(here("mw2425_06data/erinRothwell-Wood/data_exp_209080-v4_20250215", "data_exp_209080-v4_task-drth.csv"))

# 2) For first data file -----------
nsub <- check.n(mw_sona_feb25_set1) # check number of participants in data file
mw_dat <- clean.mw(mw_sona_feb25_set1) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## mw_sona_feb25_set2
n <- check.n(mw_sona_feb25_set2) # check number of participants in data file
temp <- clean.mw(mw_sona_feb25_set2) # subsequent data files: pivot, select and rename relevant variables
mw_dat <- mw_dat %>%
  add_row(temp)

## mw_sl_feb25_set1
n <- check.n(mw_sl_feb25_set1) # check number of participants in data file
temp <- clean.mw(mw_sl_feb25_set1) # subsequent data files: pivot, select and rename relevant variables
mw_dat <- mw_dat %>%
  add_row(temp)

## mw_sl_feb25_set2
n <- check.n(mw_sl_feb25_set2) # check number of participants in data file
temp <- clean.mw(mw_sl_feb25_set2) # subsequent data files: pivot, select and rename relevant variables
mw_dat <- mw_dat %>%
  add_row(temp)

## Tidy up variable names
mw_dat <- mw_dat %>%
  rename(origID = `Participant Private ID`,
         display = Display,
         spreadsheet = Spreadsheet,
         set = `Spreadsheet: set`,
         trial = `Trial Number`,
         response_type = `Response Type`,
         response = Response,
         rt = `Reaction Time`,
         condition = `Spreadsheet: condition`,
         block = `Spreadsheet: block`,
         item_number = `Spreadsheet: item`,
         item = `Spreadsheet: item_text`,
         item_answer = `Spreadsheet: item_correct`,
         probe = `Spreadsheet: probe`) %>%
  select(-`Task Name`, -`Task Version`, -`Object Name`, -`Object Number`, -`Spreadsheet: display`,
         -`Spreadsheet: instruct_num`, -`Spreadsheet: instruct_text`, -`Spreadsheet: condition_num`,
         -`Spreadsheet: block_number_text`, -probe)

# 4) Once dataframe with participants from all setups is created, score responses and calculate score

## Mental maths performance
mm <- mw_dat %>%
  select(origID, display, spreadsheet, trial, response_type, response, rt, block, item_number, item, condition, set, item_answer) %>%
  filter(display == "block") %>%
  filter(block > 0) %>%
  mutate(item_correct = case_when(response == item_answer ~ 1, TRUE ~0)) %>%
  group_by(origID, condition, set) %>%
  summarise(mm_acc = mean(item_correct)*100,
            mm_rt = mean(rt))

## Mind wandering
mw <- mw_dat %>%
  select(origID, display, spreadsheet, trial, response, block, set) %>%
  filter(display == "probe") %>%
  mutate(condition = case_when(
    spreadsheet == "set1_eded_30" & block == "1" ~ "Easy",
    spreadsheet == "set1_eded_30" & block == "2" ~ "Difficult",
    spreadsheet == "set1_eded_30" & block == "3" ~ "Easy",
    spreadsheet == "set1_eded_30" & block == "4" ~ "Difficult",
    spreadsheet == "set2_dede_30" & block == "1" ~ "Difficult",
    spreadsheet == "set2_dede_30" & block == "2" ~ "Easy",
    spreadsheet == "set2_dede_30" & block == "3" ~ "Difficult",
    spreadsheet == "set2_dede_30" & block == "4" ~ "Easy")) %>%
  mutate(mw_response = dplyr::recode(response,
                          "On Task" = "0",
                          "Intentional" = "1",
                          "Unintentional" = "1")) %>%
  mutate(mw_response = as.numeric(mw_response)) %>%
  mutate(mw_intent = dplyr::recode(response,
                            "Unintentional" = "0",
                            "Intentional" = "1")) %>%
  mutate(mw_intent = as.numeric(mw_intent))

mw_score <- mw %>%
  group_by(origID, condition, set) %>%
  summarise(mw_freq = mean(mw_response)*100)

mw_intent_score <- mw %>%
  filter(response != "On Task") %>%
  group_by(origID, condition, set) %>%
  summarise(mw_intent_freq = mean(mw_intent)*100)

## Combine into one df

mw_scores <- mm %>%
  left_join(mw_score) %>%
  left_join(mw_intent_score)

# 5) Save results in new data file

write_csv(mw_scores, here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "mw_scores_20250223.csv"))

test <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "mw_scores_20250223.csv"))
