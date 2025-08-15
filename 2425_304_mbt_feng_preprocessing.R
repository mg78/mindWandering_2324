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
  add_row(temp)

feng_dat <- feng_dat %>%
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
    spreadsheet == "Physical B" & passage == "8" ~ "Physical"
  )) %>%
  mutate(order = case_when(
    spreadsheet == "Digital A" ~ "Digital first",
    spreadsheet == "Digital B" ~ "Digital first",
    spreadsheet == "Physical A" ~ "Physical first",
    spreadsheet == "Physical B" ~ "Physical first"
  )) %>%
  mutate(probeNumber = case_when(
    spreadsheet == "Digital A" & display == "probe" & trial == "1" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "1" ~ "1",
    spreadsheet == "Digital A" & display == "probe" & trial == "2" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "2" ~ "2",
    spreadsheet == "Digital A" & display == "probe" & trial == "3" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "3" ~ "3",
    spreadsheet == "Digital A" & display == "probe" & trial == "4" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "4" ~ "4",
    spreadsheet == "Digital A" & display == "probe" & trial == "5" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "5" ~ "5",
    spreadsheet == "Digital A" & display == "probe" & trial == "6" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "6" ~ "6",
    spreadsheet == "Digital A" & display == "probe" & trial == "7" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "7" ~ "7",
    spreadsheet == "Digital A" & display == "probe" & trial == "8" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "8" ~ "8",
    spreadsheet == "Digital A" & display == "probe" & trial == "9" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "9" ~ "9",
    spreadsheet == "Digital A" & display == "probe" & trial == "10" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "10" ~ "10",
    spreadsheet == "Digital A" & display == "probe" & trial == "11" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "11" ~ "11",
    spreadsheet == "Digital A" & display == "probe" & trial == "12" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "12" ~ "12",
    spreadsheet == "Digital A" & display == "probe" & trial == "13" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "13" ~ "13",
    spreadsheet == "Digital A" & display == "probe" & trial == "14" | spreadsheet == "Physical A" & display == "probe_physical" & trial == "14" ~ "14",
    
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "1" | spreadsheet == "Physical A" & display == "probe" & trial == "1" ~ "15",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "2" | spreadsheet == "Physical A" & display == "probe" & trial == "2" ~ "16",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "3" | spreadsheet == "Physical A" & display == "probe" & trial == "3" ~ "17",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "4" | spreadsheet == "Physical A" & display == "probe" & trial == "4" ~ "18",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "5" | spreadsheet == "Physical A" & display == "probe" & trial == "5" ~ "19",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "6" | spreadsheet == "Physical A" & display == "probe" & trial == "6" ~ "20",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "7" | spreadsheet == "Physical A" & display == "probe" & trial == "7" ~ "21",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "8" | spreadsheet == "Physical A" & display == "probe" & trial == "8" ~ "22",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "9" | spreadsheet == "Physical A" & display == "probe" & trial == "9" ~ "23",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "10" | spreadsheet == "Physical A" & display == "probe" & trial == "10" ~ "24",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "11" | spreadsheet == "Physical A" & display == "probe" & trial == "11" ~ "25",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "12" | spreadsheet == "Physical A" & display == "probe" & trial == "12" ~ "26",
    spreadsheet == "Digital A" & display == "probe_physical" & trial == "13" | spreadsheet == "Physical A" & display == "probe" & trial == "13" ~ "27",
    
    spreadsheet == "Digital B" & display == "probe" & trial == "1" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "1" ~ "1",
    spreadsheet == "Digital B" & display == "probe" & trial == "2" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "2" ~ "2",
    spreadsheet == "Digital B" & display == "probe" & trial == "3" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "3" ~ "3",
    spreadsheet == "Digital B" & display == "probe" & trial == "4" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "4" ~ "4",
    spreadsheet == "Digital B" & display == "probe" & trial == "5" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "5" ~ "5",
    spreadsheet == "Digital B" & display == "probe" & trial == "6" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "6" ~ "6",
    spreadsheet == "Digital B" & display == "probe" & trial == "7" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "7" ~ "7",
    spreadsheet == "Digital B" & display == "probe" & trial == "8" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "8" ~ "8",
    spreadsheet == "Digital B" & display == "probe" & trial == "9" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "9" ~ "9",
    spreadsheet == "Digital B" & display == "probe" & trial == "10" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "10" ~ "10",
    spreadsheet == "Digital B" & display == "probe" & trial == "11" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "11" ~ "11",
    spreadsheet == "Digital B" & display == "probe" & trial == "12" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "12" ~ "12",
    spreadsheet == "Digital B" & display == "probe" & trial == "13" | spreadsheet == "Physical B" & display == "probe_physical" & trial == "13" ~ "13",
    
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "1" | spreadsheet == "Physical B" & display == "probe" & trial == "1" ~ "14",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "2" | spreadsheet == "Physical B" & display == "probe" & trial == "2" ~ "15",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "3" | spreadsheet == "Physical B" & display == "probe" & trial == "3" ~ "16",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "4" | spreadsheet == "Physical B" & display == "probe" & trial == "4" ~ "17",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "5" | spreadsheet == "Physical B" & display == "probe" & trial == "5" ~ "18",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "6" | spreadsheet == "Physical B" & display == "probe" & trial == "6" ~ "19",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "7" | spreadsheet == "Physical B" & display == "probe" & trial == "7" ~ "20",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "8" | spreadsheet == "Physical B" & display == "probe" & trial == "8" ~ "21",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "9" | spreadsheet == "Physical B" & display == "probe" & trial == "9" ~ "22",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "10" | spreadsheet == "Physical B" & display == "probe" & trial == "10" ~ "23",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "11" | spreadsheet == "Physical B" & display == "probe" & trial == "11" ~ "24",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "12" | spreadsheet == "Physical B" & display == "probe" & trial == "12" ~ "25",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "13" | spreadsheet == "Physical B" & display == "probe" & trial == "13" ~ "26",
    spreadsheet == "Digital B" & display == "probe_physical" & trial == "14" | spreadsheet == "Physical B" & display == "probe" & trial == "14" ~ "27"
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

rc_score_megan <- rc_dat %>%                # Order and condition irrelevant for project Megan
  group_by(origID) %>%
  summarise(rc_acc = mean(rc_correct)*100,
            rct = mean(rt))

## Mind wandering

mw_dat <- feng_dat %>%
  select(origID, spreadsheet, display, order, trial, probeNumber, response) %>%
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
  )) %>%
  select(-display) %>%
  mutate(passage = case_when(
    spreadsheet == "Digital A" & probeNumber == 1 | spreadsheet ==  "Physical A" & probeNumber == 1 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 2 | spreadsheet ==  "Physical A" & probeNumber == 2 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 3 | spreadsheet ==  "Physical A" & probeNumber == 3 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 4 | spreadsheet ==  "Physical A" & probeNumber == 4 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 5 | spreadsheet ==  "Physical A" & probeNumber == 5 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 6 | spreadsheet ==  "Physical A" & probeNumber == 6 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 7 | spreadsheet ==  "Physical A" & probeNumber == 7 ~ 1,
    spreadsheet == "Digital A" & probeNumber == 8 | spreadsheet ==  "Physical A" & probeNumber == 8 ~ 3,
    spreadsheet == "Digital A" & probeNumber == 9 | spreadsheet ==  "Physical A" & probeNumber == 9 ~ 3,
    spreadsheet == "Digital A" & probeNumber == 10 | spreadsheet ==  "Physical A" & probeNumber == 10 ~ 5,
    spreadsheet == "Digital A" & probeNumber == 11 | spreadsheet ==  "Physical A" & probeNumber == 11 ~ 5,
    spreadsheet == "Digital A" & probeNumber == 12 | spreadsheet ==  "Physical A" & probeNumber == 12 ~ 5,
    spreadsheet == "Digital A" & probeNumber == 13 | spreadsheet ==  "Physical A" & probeNumber == 13 ~ 7,
    spreadsheet == "Digital A" & probeNumber == 14 | spreadsheet ==  "Physical A" & probeNumber == 14 ~ 7,
    spreadsheet == "Digital A" & probeNumber == 15 | spreadsheet ==  "Physical A" & probeNumber == 15 ~ 2,
    spreadsheet == "Digital A" & probeNumber == 16 | spreadsheet ==  "Physical A" & probeNumber == 16 ~ 2,
    spreadsheet == "Digital A" & probeNumber == 17 | spreadsheet ==  "Physical A" & probeNumber == 17 ~ 4,
    spreadsheet == "Digital A" & probeNumber == 18 | spreadsheet ==  "Physical A" & probeNumber == 18 ~ 4,
    spreadsheet == "Digital A" & probeNumber == 19 | spreadsheet ==  "Physical A" & probeNumber == 19 ~ 4,
    spreadsheet == "Digital A" & probeNumber == 20 | spreadsheet ==  "Physical A" & probeNumber == 20 ~ 4,
    spreadsheet == "Digital A" & probeNumber == 21 | spreadsheet ==  "Physical A" & probeNumber == 21 ~ 6,
    spreadsheet == "Digital A" & probeNumber == 22 | spreadsheet ==  "Physical A" & probeNumber == 22 ~ 6,
    spreadsheet == "Digital A" & probeNumber == 23 | spreadsheet ==  "Physical A" & probeNumber == 23 ~ 6,
    spreadsheet == "Digital A" & probeNumber == 24 | spreadsheet ==  "Physical A" & probeNumber == 24 ~ 6,
    spreadsheet == "Digital A" & probeNumber == 25 | spreadsheet ==  "Physical A" & probeNumber == 25 ~ 8,
    spreadsheet == "Digital A" & probeNumber == 26 | spreadsheet ==  "Physical A" & probeNumber == 26 ~ 8,
    spreadsheet == "Digital A" & probeNumber == 27 | spreadsheet ==  "Physical A" & probeNumber == 27 ~ 8,
    
    spreadsheet == "Digital B" & probeNumber == 1 | spreadsheet ==  "Physical B" & probeNumber == 1 ~ 2,
    spreadsheet == "Digital B" & probeNumber == 2 | spreadsheet ==  "Physical B" & probeNumber == 2 ~ 2,
    spreadsheet == "Digital B" & probeNumber == 3 | spreadsheet ==  "Physical B" & probeNumber == 3 ~ 4,
    spreadsheet == "Digital B" & probeNumber == 4 | spreadsheet ==  "Physical B" & probeNumber == 4 ~ 4,
    spreadsheet == "Digital B" & probeNumber == 5 | spreadsheet ==  "Physical B" & probeNumber == 5 ~ 4,
    spreadsheet == "Digital B" & probeNumber == 6 | spreadsheet ==  "Physical B" & probeNumber == 6 ~ 4,
    spreadsheet == "Digital B" & probeNumber == 7 | spreadsheet ==  "Physical B" & probeNumber == 7 ~ 6,
    spreadsheet == "Digital B" & probeNumber == 8 | spreadsheet ==  "Physical B" & probeNumber == 8 ~ 6,
    spreadsheet == "Digital B" & probeNumber == 9 | spreadsheet ==  "Physical B" & probeNumber == 9 ~ 6,
    spreadsheet == "Digital B" & probeNumber == 10 | spreadsheet ==  "Physical B" & probeNumber == 10 ~ 6,
    spreadsheet == "Digital B" & probeNumber == 11 | spreadsheet ==  "Physical B" & probeNumber == 11 ~ 8,
    spreadsheet == "Digital B" & probeNumber == 12 | spreadsheet ==  "Physical B" & probeNumber == 12 ~ 8,
    spreadsheet == "Digital B" & probeNumber == 13 | spreadsheet ==  "Physical B" & probeNumber == 13 ~ 8,
    spreadsheet == "Digital B" & probeNumber == 14 | spreadsheet ==  "Physical B" & probeNumber == 14 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 15 | spreadsheet ==  "Physical B" & probeNumber == 15 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 16 | spreadsheet ==  "Physical B" & probeNumber == 16 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 17 | spreadsheet ==  "Physical B" & probeNumber == 17 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 18 | spreadsheet ==  "Physical B" & probeNumber == 18 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 19 | spreadsheet ==  "Physical B" & probeNumber == 19 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 20 | spreadsheet ==  "Physical B" & probeNumber == 20 ~ 1,
    spreadsheet == "Digital B" & probeNumber == 21 | spreadsheet ==  "Physical B" & probeNumber == 21 ~ 3,
    spreadsheet == "Digital B" & probeNumber == 22 | spreadsheet ==  "Physical B" & probeNumber == 22 ~ 3,
    spreadsheet == "Digital B" & probeNumber == 23 | spreadsheet ==  "Physical B" & probeNumber == 23 ~ 5,
    spreadsheet == "Digital B" & probeNumber == 24 | spreadsheet ==  "Physical B" & probeNumber == 24 ~ 5,
    spreadsheet == "Digital B" & probeNumber == 25 | spreadsheet ==  "Physical B" & probeNumber == 25 ~ 5,
    spreadsheet == "Digital B" & probeNumber == 26 | spreadsheet ==  "Physical B" & probeNumber == 26 ~ 7,
    spreadsheet == "Digital B" & probeNumber == 27 | spreadsheet ==  "Physical B" & probeNumber == 27 ~ 7,
  )) %>%
  select (-spreadsheet)

mw_score <- mw_dat %>%
  group_by(origID, order, condition) %>%
  summarise(mw_freq = mean(mw_response)*100)

mw_intent_score <- mw_dat %>%
  filter(response != "On Task") %>%
  group_by(origID, order, condition) %>%
  summarise(mw_intent_freq = mean(mw_intent)*100)

mw_score_megan <- mw_dat %>%               # Order and condition irrelevant for project Megan
  group_by(origID) %>%
  summarise(mw_freq = mean(mw_response)*100)

mw_intent_score_megan <- mw_dat %>%       # Order and condition irrelevant for project Megan
  filter(response != "On Task") %>%
  group_by(origID) %>%
  summarise(mw_intent_freq = mean(mw_intent)*100)

## Combine into one df

mw <- rc_score %>%
  left_join(mw_score) %>%
  left_join(mw_intent_score)

mw_megan <- rc_score_megan %>%
  left_join(mw_score_megan) %>%
  left_join(mw_intent_score_megan)

# 5) Save results in new data file

write_csv(mw, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_20250402.csv"))
write_csv(mw_dat, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_dat_20250417.csv")) # For project Annabel Boylan
write_csv(mw_megan, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_megan_20250402.csv")) # For project Megan Tolentino

test <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_20250402.csv"))
test2 <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_dat_20250417.csv"))
test3 <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_megan_20250402.csv"))
