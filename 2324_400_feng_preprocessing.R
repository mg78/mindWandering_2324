# mw_400_feng_preprocessing

# Data collected as part of Molly's 400 project 2023-24, using Feng et al., paradigm, with both easy and difficult texts.
# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla: set 1 -> task-n8kh, set 2 -> task-353c

# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.feng() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'feng_dat').
# 3) For subsequent data files, use function clean.feng() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.feng <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Spreadsheet", "Display", "Trial Number",
           "Response Type", "Response", "Object Name", "Object Number", "Reaction Time",
           "Spreadsheet: passage":"Spreadsheet: correct_response") %>%
    filter(.data$`Response Type` == "response")
}

# 1) Read in data files -----------
feng_sona_jul24_set1 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_180633-v7_20240730", "data_exp_180633-v7_task-n8kh.csv"))
feng_sona_jul24_set2 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_180633-v7_20240730", "data_exp_180633-v7_task-353c.csv"))
feng_sl_jul24_set1 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_179131-v14_20240730", "data_exp_179131-v14_task-n8kh.csv"))
feng_sl_jul24_set2 <- read_csv(here("mw2324_06data/data_mollyPugh/data_exp_179131-v14_20240730", "data_exp_179131-v14_task-353c.csv"))

# 2) For first data file -----------
nsub <- check.n(feng_sona_jul24_set1) # check number of participants in data file
nsub <- check.n(feng_sona_jul24_set2) # check number of participants in data file
feng_set1_dat <- clean.feng(feng_sona_jul24_set1) # first data file: pivot, select and rename relevant variables
feng_set2_dat <- clean.feng(feng_sona_jul24_set2)
feng_dat <- feng_set1_dat %>%
  add_row(feng_set2_dat)

# 3) For subsequent data files -----------

## feng_sl_jul24
n <- check.n(feng_sl_jul24_set1) # check number of participants in data file
n <- check.n(feng_sl_jul24_set2) # check number of participants in data file
temp_set1 <- clean.feng(feng_sl_jul24_set1) # subsequent data files: pivot, select and rename relevant variables
temp_set2 <- clean.feng(feng_sl_jul24_set2)

temp <- temp_set1 %>% # add participants 'temp' to main data frame
  add_row(temp_set2)

feng_dat <- feng_dat %>%
  add_row(temp) %>%
  rename(origID = `Participant Private ID`,
         display = Display,
         spreadsheet = Spreadsheet,
         trial = `Trial Number`,
         response_type = `Response Type`,
         response = Response,
         rc_response = `Object Number`,
         rt = `Reaction Time`,
         passage = `Spreadsheet: passage`,
         line = `Spreadsheet: line`,
         line_text = `Spreadsheet: line_text`,
         probe = `Spreadsheet: probe`,
         condition = `Spreadsheet: condition`,
         set = `Spreadsheet: set`,
         question_text = `Spreadsheet: question_text`,
         question_correct = `Spreadsheet: correct_response`) %>%
  select(-`Task Name`, -`Task Version`, -`Spreadsheet: condition_num`, -`Object Name`) %>%
  filter(response_type == "response")



# 4) Once dataframe with participants from all setups is created, score responses and calculate score

## Reading comprehension
rc_dat <- feng_dat %>%
  select(origID, display, spreadsheet, passage, line, condition, question_text, question_correct, trial, rc_response, rt) %>%
  filter(display == "comprehension") %>%
  mutate(rc_response_rec = recode(rc_response,
                                  "2" = "a",
                                  "3" = "b",
                                  "4" = "c",
                                  "5" = "d",
                                  "6" = "e")) %>%
  mutate(rc_correct = case_when(question_correct == rc_response_rec ~ 1, TRUE ~0)) %>%
  mutate(condition = case_when(                                  # add info on condition
    spreadsheet == "set1" & passage == 1 ~ "Difficult",
    spreadsheet == "set1" & passage == 3 ~ "Difficult",
    spreadsheet == "set1" & passage == 5 ~ "Difficult",
    spreadsheet == "set1" & passage == 7 ~ "Difficult",
    spreadsheet == "set1" & passage == 2 ~ "Easy",
    spreadsheet == "set1" & passage == 4 ~ "Easy",
    spreadsheet == "set1" & passage == 6 ~ "Easy",
    spreadsheet == "set1" & passage == 8 ~ "Easy",
    spreadsheet == "set2" & passage == 2 ~ "Difficult",
    spreadsheet == "set2" & passage == 4 ~ "Difficult",
    spreadsheet == "set2" & passage == 6 ~ "Difficult",
    spreadsheet == "set2" & passage == 8 ~ "Difficult",
    spreadsheet == "set2" & passage == 1 ~ "Easy",
    spreadsheet == "set2" & passage == 3 ~ "Easy",
    spreadsheet == "set2" & passage == 5 ~ "Easy",
    spreadsheet == "set2" & passage == 7 ~ "Easy"
    ))

rc_score <- rc_dat %>%
  group_by(origID, condition) %>%
  summarise(rc_acc = mean(rc_correct)*100,
            rct = mean(rt))

## Mind wandering
mw_dat <- feng_dat %>%
  select(origID, display, spreadsheet, trial, response) %>%
  filter(display == "probe") %>%
  mutate(passage = case_when(                                  # add info on passage
    trial < 8 ~ "1",
    trial == 8 | trial == 9 ~ "2",
    trial == 10 | trial == 11 ~ "3",
    trial %in% c(12:15)  ~ "4",
    trial %in% c(16:18) ~ "5",
    trial %in% c(19:22) ~ "6",
    trial == 23 | trial == 24 ~ "7",
    trial %in% c(25:27) ~ "8"
  )) %>%
  mutate(condition = case_when(                                  # add info on condition
    spreadsheet == "set1" & passage == 1 ~ "Difficult",
    spreadsheet == "set1" & passage == 3 ~ "Difficult",
    spreadsheet == "set1" & passage == 5 ~ "Difficult",
    spreadsheet == "set1" & passage == 7 ~ "Difficult",
    spreadsheet == "set1" & passage == 2 ~ "Easy",
    spreadsheet == "set1" & passage == 4 ~ "Easy",
    spreadsheet == "set1" & passage == 6 ~ "Easy",
    spreadsheet == "set1" & passage == 8 ~ "Easy",
    spreadsheet == "set2" & passage == 2 ~ "Difficult",
    spreadsheet == "set2" & passage == 4 ~ "Difficult",
    spreadsheet == "set2" & passage == 6 ~ "Difficult",
    spreadsheet == "set2" & passage == 8 ~ "Difficult",
    spreadsheet == "set2" & passage == 1 ~ "Easy",
    spreadsheet == "set2" & passage == 3 ~ "Easy",
    spreadsheet == "set2" & passage == 5 ~ "Easy",
    spreadsheet == "set2" & passage == 7 ~ "Easy"
  )) %>%
  mutate(mw = case_when(response == "On Task" ~ 0, TRUE ~ 1)) %>%      # add variable to code whether mind wandering (1) or not (0)
  mutate(intentionality = case_when(response == "Unintentional" ~ 0,   # add variable whether mind wandering is unintentional (0) or intentional (1), or whether on task (9)
                                    response == "Intentional" ~ 1,
                                    TRUE ~ 9))

## Reading time
rt_dat <- feng_dat %>%
  select(origID, display, spreadsheet, trial, line, rt) %>%
  filter(display == "passages") %>%
  filter(line > 0)

rt_score <- rt_dat %>%
  group_by(origID) %>%
  summarise(rt_mean = mean(rt))

## Combine into one df


# 5) Save results in new data file

write_csv(mw, here("mw2324_06data/mw2324_processed", "mw_20240730.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "mw_20240730.csv"))
