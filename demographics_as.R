# Read in, clean up and output into a single file the responses to demographics questions 
# Gorilla: questionnaire-7oy7
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.demo() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'demo_sona').
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
    select("Participant Private ID", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    pivot_wider(names_from = `Question Key`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           age = Age,
           gender = Gender,
           gender_text = `Gender-text`,
           englishStrongest = Bilingual,
           otherStrongest_text = `Bilingual-text`,
           handedness = categorical_hand,
           undergraduate = Education) %>%
    mutate(origID = as.factor(origID),
           age = as.numeric(age)) %>%
    select(origID, age, gender, gender_text, englishStrongest, otherStrongest_text, handedness, undergraduate)
}

# 1) Read in data files -----------
demo_sona_jan25 <- read_csv(here("mw2425_06data/ameliaSimmonds/data_exp_202242-v3_20250130", "data_exp_202242-v3_questionnaire-7oy7.csv"))
demo_sl_jan25 <- read_csv(here("mw2425_06data/ameliaSimmonds/data_exp_195194-v10_20250130", "data_exp_195194-v10_questionnaire-7oy7.csv"))

# 2) For first data file -----------
n <- check.n(demo_sona_jan25) # check number of participants in data file
demo_dat <- clean.demo(demo_sona_jan25) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------
## demo_sl_feb24
n <- check.n(demo_sl_jan25) # check number of participants in data file
temp <- clean.demo(demo_sl_jan25) # subsequent data files: pivot, select and rename relevant variables

demo_dat <- demo_dat %>% # add participants 'temp' to main data frame
  add_row(temp)


# 4) Save results in new data file -----------
write_csv(demo_dat, here("mw2425_06data/ameliaSimmonds/mw2425_as_processed", "demo_dat_20250130.csv"))

test <- read_csv(here("mw2425_06data/ameliaSimmonds/mw2425_as_processed", "demo_dat_20250130.csv"))



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



