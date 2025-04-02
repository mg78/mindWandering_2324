# mw_304_feng_preprocessing

# Data collected as part of 304 projects 2024-25, using Feng et al., paradigm, but with difficult texts only.
# Measures intentional as well as unintentional mind wandering and there is a physical and digital condition.
# Participants were randomly allocated to one of 4 tracks:
# 1) Digital A - task-d2yz - passages 1, 3, 5 and 7 digital, then passages 2, 4, 6 and 8 physical
# 2) Digital B - task-6rlz - passages 2, 4, 6 and 8 digital, then passages 1, 3, 5 and 7 physical
# 3) Physical A - task-j2gmn - passages 1, 3, 5 and 7 physical, then passages 2, 4, 6 and 8 digital
# 4) Physical B - task-acau - passages 2, 4, 6 and 8 physical, then passages 1, 3, 5 and 7 digital

# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.arhq() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'feng_dat').
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

clean.feng_diff <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Spreadsheet", "Display", "Trial Number",
           "Response Type", "Response", "Object Name", "Object Number", "Reaction Time",
           "Spreadsheet: passage":"Spreadsheet: correct_response")
}

# 1) Read in data files -----------
feng_sona_digitalA <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_task-d2yz.csv"))
feng_sona_digitalB <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_task-6rlz.csv"))
feng_sona_physicalA <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_task-j2gm.csv"))
feng_sona_physicalB <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_task-acau.csv"))

feng_sl_digitalA <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_task-d2yz.csv"))
feng_sl_digitalB <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_task-6rlz.csv"))
feng_sl_physicalA <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_task-j2gm.csv"))
feng_sl_physicalB <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_task-acau.csv"))


# 2) For first data file -----------
nsub <- check.n(feng_sona_digitalA) # check number of participants in data file
nsub <- check.n(feng_sona_digitalB) # check number of participants in data file
nsub <- check.n(feng_sona_physicalA) # check number of participants in data file
nsub <- check.n(feng_sona_physicalA) # check number of participants in data file

feng_digitalA_dat <- clean.feng_diff(feng_sona_digitalA) # first data file: pivot, select and rename relevant variables
feng_digitalB_dat <- clean.feng_diff(feng_sona_digitalB)
feng_sona_physicalA <- clean.feng_diff(feng_sona_physicalA)
feng_sona_physicalB <- clean.feng_diff(feng_sona_physicalB)

feng_dat <- feng_digitalA_dat %>%
  add_row(feng_digitalB_dat) %>%
  add_row(feng_sona_physicalA) %>%
  add_row(feng_sona_physicalB)

# 3) For subsequent data files -----------

## feng_sl
nsub <- check.n(feng_sl_digitalA) # check number of participants in data file
nsub <- check.n(feng_sl_digitalB) # check number of participants in data file
nsub <- check.n(feng_sl_physicalA) # check number of participants in data file
nsub <- check.n(feng_sl_physicalA) # check number of participants in data file

temp_digitalA_dat <- clean.feng_diff(feng_sl_digitalA) # first data file: pivot, select and rename relevant variables
temp_digitalB_dat <- clean.feng_diff(feng_sl_digitalB)
temp_sona_physicalA <- clean.feng_diff(feng_sl_physicalA)
temp_sona_physicalB <- clean.feng_diff(feng_sl_physicalB)

temp <- temp_digitalA_dat %>%
  add_row(temp_digitalB_dat) %>%
  add_row(temp_sona_physicalA) %>%
  add_row(temp_sona_physicalB)

feng_dat <- feng_dat %>%
  add_row(temp) %>%
  rename(origID = `Participant Private ID`,
         display = Display,
         spreadsheet = Spreadsheet,
         trial = `Trial Number`,
         response_type = `Response Type`,
         response = Response,
         rc_response = `Object Name`,
         rc_response_quant = `Object Number`,
         rt = `Reaction Time`,
         passage = `Spreadsheet: passage`,
         line = `Spreadsheet: line`,
         line_text = `Spreadsheet: line_text`,
         probe = `Spreadsheet: probe`,
         condition = `Spreadsheet: condition`,
         set = `Spreadsheet: set`,
         question_text = `Spreadsheet: question_text`,
         question_correct = `Spreadsheet: correct_response`) %>%
  select(-`Task Name`, -`Task Version`, -`Spreadsheet: condition_num`) %>%
  filter(response_type == "response") %>%
  mutate(condition = case_when(
    spreadsheet == "Digital A" & passage == "1" ~ "Digital",
    spreadsheet == "Digital A" & passage == "3" ~ "Digital",
    spreadsheet == "Digital A" & passage == "5" ~ "Digital",
    spreadsheet == "Digital A" & passage == "7" ~ "Digital",
    spreadsheet == "Digital A" & passage == "2" ~ "Physical",
    spreadsheet == "Digital A" & passage == "4" ~ "Physical",
    spreadsheet == "Digital A" & passage == "6" ~ "Physical",
    spreadsheet == "Digital A" & passage == "8" ~ "Physical",
    
    spreadsheet == "Digital B" & passage == "1" ~ "Physical",
    spreadsheet == "Digital B" & passage == "3" ~ "Physical",
    spreadsheet == "Digital B" & passage == "5" ~ "Physical",
    spreadsheet == "Digital B" & passage == "7" ~ "Physical",
    spreadsheet == "Digital B" & passage == "2" ~ "Digital",
    spreadsheet == "Digital B" & passage == "4" ~ "Digital",
    spreadsheet == "Digital B" & passage == "6" ~ "Digital",
    spreadsheet == "Digital B" & passage == "8" ~ "Digital",
    
    spreadsheet == "Physical A" & passage == "2" ~ "Digital",
    spreadsheet == "Physical A" & passage == "4" ~ "Digital",
    spreadsheet == "Physical A" & passage == "6" ~ "Digital",
    spreadsheet == "Physical A" & passage == "8" ~ "Digital",
    spreadsheet == "Physical A" & passage == "1" ~ "Physical",
    spreadsheet == "Physical A" & passage == "3" ~ "Physical",
    spreadsheet == "Physical A" & passage == "5" ~ "Physical",
    spreadsheet == "Physical A" & passage == "7" ~ "Physical",
    
    spreadsheet == "Physical B" & passage == "1" ~ "Digital",
    spreadsheet == "Physical B" & passage == "3" ~ "Digital",
    spreadsheet == "Physical B" & passage == "5" ~ "Digital",
    spreadsheet == "Physical B" & passage == "7" ~ "Digital",
    spreadsheet == "Physical B" & passage == "2" ~ "Physical",
    spreadsheet == "Physical B" & passage == "4" ~ "Physical",
    spreadsheet == "Physical B" & passage == "6" ~ "Physical",
    spreadsheet == "Physical B" & passage == "8" ~ "Physical",
  )) %>%
  mutate(order = case_when(
    spreadsheet == "Digital A" ~ "Digital first",
    spreadsheet == "Digital B" ~ "Digital first",
    spreadsheet == "Physical A" ~ "Physical first",
    spreadsheet == "Physical B" ~ "Physical first"
  ))



# 4) Once dataframe with participants from all setups is created, score responses and calculate score

## Reading comprehension
rc_dat <- feng_dat %>%
  select(origID, display, order, condition, passage, line, question_text, question_correct, trial, rc_response_quant, rt) %>%
  filter(display == "comprehension") %>%
  mutate(rc_response_rec = recode(rc_response_quant,
                                  "2" = "a",
                                  "3" = "b",
                                  "4" = "c",
                                  "5" = "d",
                                  "6" = "e")) %>%
  mutate(rc_correct = case_when(question_correct == rc_response_rec ~ 1, TRUE ~0))
  
rc_score <- rc_dat %>%
  group_by(origID, order, condition) %>%
  summarise(rc_acc = mean(rc_correct)*100,
            rct = mean(rt))

## Mind wandering
mw_dat <- feng_dat %>%
  select(origID, display, order, trial, response) %>%
  filter(display == "probe" | display == "probe_physical") %>%
  mutate(mw_response = dplyr::recode(response,
                                     "On Task" = "0",
                                     "Intentional" = "1",
                                     "Unintentional" = "1")) %>%
  mutate(mw_response = as.numeric(mw_response)) %>%
  mutate(mw_intent = dplyr::recode(response,
                                   "Unintentional" = "0",
                                   "Intentional" = "1")) %>%
  mutate(mw_intent = as.numeric(mw_intent)) %>%
  mutate(condition = case_when(
    display == "probe" ~ "Digital",
    display == "probe_physical" ~ "Physical"
  ))
  
mw_score <- mw_dat %>%
  group_by(origID, order, condition) %>%
  summarise(mw_freq = mean(mw_response)*100)
  
mw_intent_score <- mw_dat %>%
  filter(response != "On Task") %>%
  group_by(origID, order, condition) %>%
  summarise(mw_intent_freq = mean(mw_intent)*100)

## Combine into one df

mw <- rc_score %>%
  left_join(mw_score) %>%
  left_join(mw_intent_score)

# 5) Save results in new data file

write_csv(mw, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_20250402.csv"))

test <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_20250402.csv"))
