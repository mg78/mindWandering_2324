# mw_400_mw_preprocessing

# Data collected as part of 400 project 2425 (student: Halle Warren), using a 'sustained attention
# to response' (SART) task with two ways to measure mind wandering probe-based and self-report.
# Also distinguished between different types of mind-wandering (tune out, space out, zone out
# in probe-caught condition; tune out vs. space out in self-report condition) as defined in
# Jackson et al. 2012. Probe-caught and self-report administered as two separate tasks/
# conditions, counter-balanced across participants.

# Based on:
# Jackson et al. (2012). Psychology and Aging, 27(1), 106-119. 
# Welhaf et al. (2025). Memory and Cognition.

# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla:  branch 1 -> probe-caught first (task-jj2g), self-report second (task-ucpj)
#           branch 2 -> self-report first (task-kwp2), probe-caught second (task-qc6r)

# Data collected on sona and via simple link. For the simple link version of experiment, data were collected using two separate versions
# (v8 and v9).
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.mw() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'mw_dat').
# 3) For subsequent data files, use function clean.mw() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.mw_trials <- function(data){
  data %>%
    select("Participant Private ID", "Tree Node Key", "Task Name", "Task Version","Current Spreadsheet", "Trial Number", "Display", "Screen",
           "Response Type", "Response", "Tag", "Reaction Time", "Spreadsheet: digit", "Spreadsheet: correct", "Spreadsheet: size", "randomiser-mq2e") %>%
    filter(.data$Display == 'trial')
}

clean.mw_probes <- function(data){
  data %>%
    select("Participant Private ID", "Tree Node Key", "Task Name", "Task Version","Current Spreadsheet", "Trial Number", "Display", "Screen",
           "Response Type", "Response", "Tag", "Reaction Time", "Spreadsheet: digit", "Spreadsheet: correct", "Spreadsheet: size", "randomiser-mq2e") %>%
    filter(.data$Display == 'probe') %>%
    filter(.data$`Trial Number` > 2) # Discard first 2 thought probes as they occurred during practice blocks
}

clean.mw_selfreport <- function(data){
  data %>%
    select("Participant Private ID", "Tree Node Key", "Task Name", "Task Version","Current Spreadsheet", "Trial Number", "Display", "Screen",
           "Response Type", "Response", "Tag", "Reaction Time", "Spreadsheet: digit", "Spreadsheet: correct", "Spreadsheet: size", "randomiser-mq2e") %>%
    filter(.data$Display == 'trial')
}

# 1) Read in data files -----------
## Experiment advertised via Sona
mw_pc_sona_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_233799-v28_20250814", "data_exp_233799-v28_task-jj2g.csv"))
mw_sr_sona_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_233799-v28_20250814", "data_exp_233799-v28_task-ucpj.csv"))
mw_pc_sona_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_233799-v28_20250814", "data_exp_233799-v28_task-qc6r.csv"))
mw_sr_sona_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_233799-v28_20250814", "data_exp_233799-v28_task-kwp2.csv"))

## Experiment advertised via Simple link (version 8)
mw_pc_slv8_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_task-jj2g.csv"))
mw_sr_slv8_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_task-ucpj.csv"))
mw_pc_slv8_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_task-qc6r.csv"))
mw_sr_slv8_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_task-kwp2.csv"))

## Experiment advertised via Simple link (version 9)
mw_pc_slv9_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v9_20250814", "data_exp_234742-v9_task-jj2g.csv"))
mw_sr_slv9_aug25b1 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v9_20250814", "data_exp_234742-v9_task-ucpj.csv"))
mw_pc_slv9_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v9_20250814", "data_exp_234742-v9_task-qc6r.csv"))
mw_sr_slv9_aug25b2 <- read_csv(here("mw2425_06data/halleWarren/data_exp_234742-v9_20250814", "data_exp_234742-v9_task-kwp2.csv"))

# 2) Performance -----------
# 2a) For first data file 
## mw_pc_sona_aug25b1
nsub <- check.n(mw_pc_sona_aug25b1) # check number of participants in data file
mw_dat_trials <- clean.mw_trials(mw_pc_sona_aug25b1) # first data file: pivot, select and rename relevant variables

# 2b) For subsequent data files
## probe-caught
## mw_pc_sona_aug25b2
n <- check.n(mw_pc_sona_aug25b2) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_pc_sona_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_pc_slv8_aug25b1
n <- check.n(mw_pc_slv8_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_pc_slv8_aug25b1) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_pc_slv8_aug25b2
n <- check.n(mw_pc_slv8_aug25b2) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_pc_slv8_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_pc_slv9_aug25b1
n <- check.n(mw_pc_slv9_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_pc_slv9_aug25b1) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_pc_slv9_aug25b2
n <- check.n(mw_pc_slv9_aug25b2) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_pc_slv9_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw - self-report
## mw_sr_sona_aug25b1
n <- check.n(mw_sr_sona_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_sona_aug25b1) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_sr_sona_aug25b2
n <- check.n(mw_sr_sona_aug25b2) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_sona_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_sr_slv8_aug25b1
n <- check.n(mw_sr_slv8_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_slv8_aug25b1) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_sr_slv8_aug25b2
n <- check.n(mw_sr_slv8_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_slv8_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_sr_slv9_aug25b1
n <- check.n(mw_sr_slv9_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_slv9_aug25b1) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)

## mw_sr_slv9_aug25b2
n <- check.n(mw_sr_slv9_aug25b1) # check number of participants in data file
temp_trials <- clean.mw_trials(mw_sr_slv9_aug25b2) # subsequent data files: pivot, select and rename relevant variables
mw_dat_trials <- mw_dat_trials %>%
  add_row(temp_trials)


# 3) Mind wandering - probe-caught ------------
# 3a) For first data file 
## mw_pc_sona_aug25b1
nsub <- check.n(mw_pc_sona_aug25b1) # check number of participants in data file
mw_dat_probes <- clean.mw_probes(mw_pc_sona_aug25b1) # first data file: pivot, select and rename relevant variables

# 3b) For subsequent data files
## mw_pc_sona_aug25b2
n <- check.n(mw_pc_sona_aug25b2) # check number of participants in data file
temp_probes <- clean.mw_probes(mw_pc_sona_aug25b2)
mw_dat_probes <- mw_dat_probes %>%
  add_row(temp_probes)

## mw_pc_slv8_aug25b1
n <- check.n(mw_pc_slv8_aug25b1) # check number of participants in data file
temp_probes <- clean.mw_probes(mw_pc_slv8_aug25b1)
mw_dat_probes <- mw_dat_probes %>%
  add_row(temp_probes)

## mw_pc_slv8_aug25b2
n <- check.n(mw_pc_slv8_aug25b2) # check number of participants in data file
temp_probes <- clean.mw_probes(mw_pc_slv8_aug25b2)
mw_dat_probes <- mw_dat_probes %>%
  add_row(temp_probes)

## mw_pc_slv9_aug25b1
n <- check.n(mw_pc_slv9_aug25b1) # check number of participants in data file
temp_probes <- clean.mw_probes(mw_pc_slv9_aug25b1)
mw_dat_probes <- mw_dat_probes %>%
  add_row(temp_probes)

## mw_pc_slv9_aug25b2
n <- check.n(mw_pc_slv9_aug25b2) # check number of participants in data file
temp_probes <- clean.mw_probes(mw_pc_slv9_aug25b2)
mw_dat_probes <- mw_dat_probes %>%
  add_row(temp_probes)

# 4) Mind wandering - self-report ------------
# 4a) For first data file 
## mw_sr_sona_aug25b1
n <- check.n(mw_sr_sona_aug25b1) # check number of participants in data file
mw_dat_selfreport <- clean.mw_selfreport(mw_sr_sona_aug25b1)

# 4b) For subsequent data files
## mw_sr_sona_aug25b2
n <- check.n(mw_sr_sona_aug25b2) # check number of participants in data file
temp_selfreport <- clean.mw_selfreport(mw_sr_sona_aug25b2)
mw_dat_selfreport <- mw_dat_selfreport %>%
  add_row(temp_selfreport)

## mw_sr_slv8_aug25b1
n <- check.n(mw_sr_slv8_aug25b1) # check number of participants in data file
temp_selfreport <- clean.mw_selfreport(mw_sr_slv8_aug25b1)
mw_dat_selfreport <- mw_dat_selfreport %>%
  add_row(temp_selfreport)

## mw_sr_slv8_aug25b2
n <- check.n(mw_sr_slv8_aug25b2) # check number of participants in data file
temp_selfreport <- clean.mw_selfreport(mw_sr_slv8_aug25b2)
mw_dat_selfreport <- mw_dat_selfreport %>%
  add_row(temp_selfreport)

## mw_sr_slv9_aug25b1
n <- check.n(mw_sr_slv9_aug25b1) # check number of participants in data file
temp_selfreport <- clean.mw_selfreport(mw_sr_slv9_aug25b1)
mw_dat_selfreport <- mw_dat_selfreport %>%
  add_row(temp_selfreport)

## mw_sr_slv9_aug25b2
n <- check.n(mw_sr_slv9_aug25b2) # check number of participants in data file
temp_selfreport <- clean.mw_selfreport(mw_sr_slv9_aug25b2)
mw_dat_selfreport <- mw_dat_selfreport %>%
  add_row(temp_selfreport)

# 5) Some intermediate actions  ------------
## We now have 3 key data files, let's save each of them to avoid having to rerun every time:
### 1) One data file that contains performance data for all participants in all tasks (mw_dat_trials)
write_csv(mw_dat_trials, here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_trials.csv"))
### 2) One data file that contains mind wandering data for all participants in the probe-caught tasks (mw_dat_probes)
write_csv(mw_dat_probes, here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_probes.csv"))
### 3) One data file that contains mind wandering data for all participants in the self-report tasks (mw_dat_selfreport)
write_csv(mw_dat_selfreport, here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_selfreport.csv"))

mw_dat_trials <- read_csv(here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_trials.csv"))
mw_dat_probes <- read_csv(here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_probes.csv"))
mw_dat_selfreport <- read_csv(here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_dat_selfreport.csv"))

## Tidy up variable names
mw_dat_trials <- mw_dat_trials %>%
  rename(origID = `Participant Private ID`,
         task = `Tree Node Key`,
         trial = `Trial Number`,
         display = Display,
         screen = Screen,
         response_type = `Response Type`,
         response = Response,
         rt = `Reaction Time`, ### !! Need to add 200 ms to each rt
         digit = `Spreadsheet: digit`,
         answer = `Spreadsheet: correct`,
         order = `randomiser-mq2e`) %>%
  select(-`Task Name`, -`Task Version`, -`Current Spreadsheet`, -Tag,
        -`Spreadsheet: size`) %>%
  filter(response_type == 'response') %>%
  mutate(origID = as.factor(origID),
         task = as.factor(task),
         order = as.factor(order))

mw_dat_probes <- mw_dat_probes %>%
  rename(origID = `Participant Private ID`,
         task = `Tree Node Key`,
         trial = `Trial Number`,
         response_type = `Response Type`,
         response = Tag,
         order = `randomiser-mq2e`) %>%
  select(-`Task Name`, -`Task Version`, -`Current Spreadsheet`, -Display,
         -Screen, -Response, -`Reaction Time`,
         -`Spreadsheet: digit`, -`Spreadsheet: correct`, -`Spreadsheet: size`) %>%
  filter(response_type == 'response') %>%
  mutate(origID = as.factor(origID),
         task = as.factor(task),
         order = as.factor(order))

mw_dat_selfreport <- mw_dat_selfreport %>%
  rename(origID = `Participant Private ID`,
         task = `Tree Node Key`,
         screen = `Screen`,
         response = Tag,
         order = `randomiser-mq2e`) %>%
  select(-`Task Name`, -`Task Version`, -`Current Spreadsheet`, -`Trial Number`, -Display, -`Response Type`,
         -Response, -`Reaction Time`, -`Spreadsheet: digit`, -`Spreadsheet: correct`, -`Spreadsheet: size`) %>%
  filter(screen == 'self report') %>%
  mutate(origID = as.factor(origID),
         task = as.factor(task),
         order = as.factor(order))

# 6) Calculate scores  ------------

## Mind wandering - probe-caught
mw_pc <- mw_dat_probes %>%
  select(origID, response, order) %>%
  mutate(mw = dplyr::recode(response,
                          "on task" = "0",
                          "tune out" = "1",
                          "zone out" = "1",
                          "space out" = "1")) %>%
  mutate(mw = as.numeric(mw))

mw_pc2 <- mw_dat_probes %>%
  filter(response != "on task") %>%
  mutate(mw_aware = dplyr::recode(response,
                            "zone out" = "0",
                            "space out" = "0",
                            "tune out" = "1")) %>%
  mutate(mw_aware = as.numeric(mw_aware))

mw_pc_score <- mw_pc %>%
  group_by(origID, order) %>%
  summarise(mw_pc_count = sum(mw))

mw_pc_aware_score <- mw_pc2 %>%
  group_by(origID, order) %>%
  summarise(mw_pc_aware_count = sum(mw_aware))

## Mind wandering - self-report
mw_sr <- mw_dat_selfreport %>%
  select(origID, response, order) %>%
  mutate(mw = dplyr::recode(response,
                            "tune out" = "1",
                            "space out" = "1")) %>%
  mutate(mw = as.numeric(mw))

mw_sr_score <- mw_sr %>%
  group_by(origID, order) %>%
  summarise(mw_sr_count = sum(mw))

# ## Performance on SART
# performance <- mw_dat_trials %>%
#   select(origID, display, spreadsheet, trial, response_type, response, rt, block, item_number, item, condition, set, item_answer) %>%
#   filter(display == "block") %>%
#   filter(block > 0) %>%
#   mutate(item_correct = case_when(response == item_answer ~ 1, TRUE ~0)) %>%
#   group_by(origID, condition, set) %>%
#   summarise(mm_acc = mean(item_correct)*100,
#             mm_rt = mean(rt))

# 7) Combine into one df --------

mw_scores <- mw_pc_score %>%
  left_join(mw_pc_aware_score) %>%
  left_join(mw_sr_score)

mw_scores <- mw_scores %>%
  mutate(mw_sr_count = replace_na(mw_sr_count, 0))

# 5) Save results in new data file

write_csv(mw_scores, here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_scores_20250815.csv"))

test <- read_csv(here("mw2425_06data/halleWarren/mw2425_hw_processed", "mw_scores_20250815.csv"))
