# Read in, clean up and output into a single file the responses to the diagnosis questionnaire
# Gorilla: questionnaire-kgbm
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

clean.diagnoses_p1 <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object Name", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(!.data$Key == "quantised") %>%
    filter(.data$`Object Name` == "gr_diag") %>%
    select(`Participant Private ID`, `Object Name`, Response) %>%
    pivot_wider(names_from = `Object Name`, values_from = Response) %>%
    rename(origID = `Participant Private ID`) %>%
    rename(group = gr_diag) %>%
    mutate(origID = as.factor(origID))
}

clean.diagnoses_p2 <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Response Type", "Key", "Object Name", "Question", "Response") %>%
    filter(.data$`Response Type` == "response") %>%
    filter(!.data$Key == "quantised") %>%
    filter(.data$`Object Name` == "gr_diagMult_which") %>%
    select(`Participant Private ID`, Key, Response) %>%
    pivot_wider(names_from = Key, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           dyslexia = `Developmental dyslexia`,
           adhd = `Attention Deficit Hyperactivity Disorder (ADHD)`,
           asd = `Autism spectrum disorder`) %>%
    mutate(origID = as.factor(origID))
}

# 1) Read in data files -----------
diag_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10_20240318", "data_exp_155541-v10_questionnaire-kgbm.csv"))
diag_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240318", "data_exp_158033-v3_questionnaire-kgbm.csv"))

# 2) For first data file -----------
nsub <- check.n(diag_sona_feb24) # check number of participants in data file
diag_dat_p1 <- clean.diagnoses_p1(diag_sona_feb24)
diag_dat_p2 <- clean.diagnoses_p2(diag_sona_feb24)
diag_dat <- diag_dat_p1 %>%
  inner_join(diag_dat_p2)

# 3) For subsequent data files -----------

## diag_sl_feb24
n <- check.n(diag_sl_feb24) # check number of participants in data file
temp_p1 <- clean.diagnoses_p1(diag_sl_feb24) # subsequent data files: pivot, select and rename relevant variables
temp_p2 <- clean.diagnoses_p2(diag_sl_feb24)
temp <- temp_p1 %>%
  inner_join(temp_p2)
diag_dat <- diag_dat %>%
  add_row(temp)


# 4) Once dataframe with participants from all setups is created, add 1 or 0 to each diagnosis column (dyslexia, adhd, asd)
diag_dat <- diag_dat %>%
  mutate(control = case_when(group == "control" ~ 1, .default = 0),
         dyslexia_new = case_when(group == "dyslexia" | dyslexia == 1 ~ 1, .default = 0),
         adhd_new = case_when(group == "adhd" | adhd == 1 ~ 1, .default = 0),
         asd_new = case_when(group == "asd" | asd == 1 ~ 1, .default = 0)
         ) %>%
  select(origID, group, control:asd_new) %>%
  mutate(group2 = case_when(group == "control" ~ "control", .default = "neurodiverse")) %>%
  rename(dyslexia = dyslexia_new,
         adhd = adhd_new,
         asd = asd_new)

# 5) Save results in new data file
write_csv(diag_dat, here("mw2324_06data/mw2324_processed", "diag_dat_20240318.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240318.csv"))

