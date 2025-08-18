# Read in, clean up and output into a single file the responses to the diagnosis questionnaire
# Gorilla: questionnaire-wctd
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.diagnoses() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'diag_dat').
# 3) For subsequent data files, use function clean.diagnoses() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.diagnoses <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object ID", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(.data$Key == "value") %>%
    select(`Participant Private ID`, `Object ID`, Response) %>%
    pivot_wider(names_from = `Object ID`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           diag_gr = `object-19`,
           await = `object-27`,
           other_med = `object-23`) %>%
    mutate(origID = as.factor(origID),
           diag_gr = as.factor(diag_gr),
           await = as.factor(await),
           other_med = as.factor(other_med))
}

clean.diagnoses2_p1 <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object ID", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(.data$Key != "quantised") %>%
    filter(.data$`Object ID` == "object-19") %>%
    select(.data$`Participant Private ID`, Key, Response) %>%
    pivot_wider(names_from = Key, values_from = Response) %>%
    mutate(diag_gr = case_when(autism == "1" ~ "autism",
                             ADHD == "1" ~ "adhd",
                             multiple == "1" ~ "multiple",
                             control == "1" ~ "control",
                             `__other` == "1" ~ "other")) %>%
    rename(origID = `Participant Private ID`,
         other_text = other) %>%
    mutate(origID = as.factor(origID),
         diag_gr = as.factor(diag_gr),
         autism = as.numeric(autism),
         ADHD = as.numeric(ADHD),
         multiple = as.numeric(multiple),
         control = as.numeric(control),
         `__other` = as.numeric(`__other`)) %>%
    select(.data$origID, diag_gr, other_text)
}

clean.diagnoses3_p1 <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object ID", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(.data$Key != "quantised") %>%
    filter(.data$`Object ID` == "object-19") %>%
    select(.data$`Participant Private ID`, Key, Response) %>%
    pivot_wider(names_from = Key, values_from = Response) %>%
    mutate(diag_gr = case_when(autism == "1" ~ "autism",
                               ADHD == "1" ~ "adhd",
                               multiple == "1" ~ "multiple",
                               control == "1" ~ "control",
                               `__other` == "1" ~ "other")) %>%
    rename(origID = `Participant Private ID`) %>%
    mutate(origID = as.factor(origID),
           diag_gr = as.factor(diag_gr),
           autism = as.numeric(autism),
           ADHD = as.numeric(ADHD),
           multiple = as.numeric(multiple),
           control = as.numeric(control),
           `__other` = as.numeric(`__other`)) %>%
    select(.data$origID, diag_gr)
}

clean.diagnoses2_p2 <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object ID", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(.data$Key != "quantised") %>%
    filter(.data$`Object ID` == "object-23" |.data$`Object ID` == "object-27") %>%
    select(`Participant Private ID`, `Object ID`, Response) %>%
    pivot_wider(names_from = `Object ID`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           await = `object-27`,
           other_med = `object-23`) %>%
    mutate(origID = as.factor(origID),
           await = as.factor(await),
           other_med = as.factor(other_med))
}

# 1) Read in data files -----------
diag_sona_aug25 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_233799-v28_20250818", "data_exp_233799-v28_questionnaire-wctd.csv"))
diag_sl_aug25 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v8_20250801", "data_exp_234742-v8_questionnaire-wctd.csv"))
diag_sl2_aug25 <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/data_exp_234742-v9_20250818", "data_exp_234742-v9_questionnaire-wctd.csv"))

# 2) For first data file -----------
nsub <- check.n(diag_sl_aug25) # check number of participants in data file
diag_dat <- clean.diagnoses(diag_sl_aug25)
diag_dat <- diag_dat %>%
  add_column(other_text = NA)

# 3) For subsequent data files -----------

## diag_sl2_aug25
nsub <- check.n(diag_sl2_aug25) # check number of participants in data file
temp_p1 <- clean.diagnoses2_p1(diag_sl2_aug25) # subsequent data files: pivot, select and rename relevant variables
temp_p2 <- clean.diagnoses2_p2(diag_sl2_aug25)
temp <- temp_p1 %>%
  left_join(temp_p2)

diag_dat <- diag_dat %>%
  add_row(temp)

## diag_sona_aug25
nsub <- check.n(diag_sona_aug25) # check number of participants in data file
temp_p1 <- clean.diagnoses3_p1(diag_sona_aug25) # subsequent data files: pivot, select and rename relevant variables
temp_p2 <- clean.diagnoses2_p2(diag_sona_aug25)
temp <- temp_p1 %>%
  left_join(temp_p2)

diag_dat <- diag_dat %>%
  add_row(temp) %>%
  relocate(origID, diag_gr, other_text, await, other_med)

# 4) Save results in new data file
write_csv(diag_dat, here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "diag_dat_20250818.csv"))

test <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "diag_dat_20250818.csv"))

##

