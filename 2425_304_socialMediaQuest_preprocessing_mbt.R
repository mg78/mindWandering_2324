# mw_304_socialMediaQuest_preprocessing_mbt

# Data collected as part of 304 projects 2024-25
# Social Media Questionnaire
# Origin ?
# Read in, clean up and output into a single file the responses on the mind wandering task as implemented on Gorilla.
# Gorilla: questionnaire-5lv3

# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.smq() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'smq_dat').
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

clean.smq <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question", "Response Type", "Key",
           "Response")
}

# 1) Read in data files -----------
smq_sona <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_questionnaire-5lv3.csv"))
smq_sl <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_questionnaire-5lv3.csv"))

# 2) Combine data files
nsub <- check.n(smq_sona) # check number of participants in data file
smq_dat <- clean.smq(smq_sona)

n <- check.n(smq_sl) # check number of participants in data file
temp <- clean.smq(smq_sl) # subsequent data files: pivot, select and rename relevant variables
smq_dat <- smq_dat %>%
  add_row(temp)

# 3) Once dataframe with participants from all setups is created, pivot separately for 2 sorts of questions

q <- rep(rep(1:27, each = 2), 34)
k <- rep(c("value", "quantised"), 918)

smq <- smq_dat %>%
  select("Participant Private ID", "Response Type", "Question", "Key", "Response") %>%
  filter(`Response Type` == "response") %>%
  mutate(questionNumber = q,
         questionKey = k) %>%
  filter(questionKey == "quantised") %>%
  rename(origID = `Participant Private ID`,
         questionText = Key,
         response = Response) %>%
  select(origID, questionNumber, questionText, response) %>%
  mutate(origID = as.factor(origID),
         response = as.numeric(response))

smq_mean <- smq %>%
  group_by(origID) %>%
  summarise(smq_mean = mean(response))

smq_subscales <- smq %>%
  select(-questionText) %>%
  pivot_wider(names_from = questionNumber, values_from = response) %>%
  group_by(origID) %>%
  mutate(preoccupation = mean(`1`:`3`),
         tolerance = mean(`4`:`6`),
         withdrawal = mean(`7`:`9`),
         persistence = mean(`10`:`12`),
         escape = mean(`13`:`15`),
         problems = mean(`16`:`18`),
         deception = mean(`19`:`21`),
         displacement = mean(`22`:`24`),
         conflict = mean(`25`:`27`)) %>%
  select(origID, preoccupation:conflict)

smq_scores <- smq_mean %>%
  left_join(smq_subscales)

smq_reliability <- smq %>%
  select(-questionText) %>%
  pivot_wider(names_from = questionNumber, values_from = response)

# 5) Save results in new data file -----------
write_csv(smq_scores, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "smq_scores_20250404.csv"))
write_csv(smq_reliability, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "smq_reliability_20250408.csv"))

test <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "smq_scores_20250404.csv"))
