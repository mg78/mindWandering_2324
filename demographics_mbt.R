# Read in, clean up and output into a single file the responses to demographics questions 
# Gorilla: sona - questionnaire-g99h
#          voucher - questionnaire-v6bd
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
    select("Participant Private ID", "randomiser-3edb", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    pivot_wider(names_from = `Question Key`, values_from = Response) %>%
    rename(origID = `Participant Private ID`,
           spreadsheet = `randomiser-3edb`,
           age = Age,
           english = English) %>%
    mutate(origID = as.factor(origID),
           age = as.numeric(age),
           gender = as.factor(gender),
           english = as.factor(english)) %>%
    mutate(order = case_when(
      spreadsheet == "Digital A" ~ "Digital first",
      spreadsheet == "Digital B" ~ "Digital first",
      spreadsheet == "Physical A" ~ "Physical first",
      spreadsheet == "Physical B" ~ "Physical first")) %>%
    select(origID, order, age, gender, english)
}

# 1) Read in data files -----------
demo_sona_mar25 <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_201675-v16", "data_exp_201675-v16_questionnaire-g99h.csv"))
demo_sl_mar25 <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/data_exp_210469-v6", "data_exp_210469-v6_questionnaire-v6bd.csv"))

# 2) For first data file -----------
n <- check.n(demo_sona_mar25) # check number of participants in data file
demo_dat <- clean.demo(demo_sona_mar25) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------
## demo_sl_feb25
n <- check.n(demo_sl_mar25) # check number of participants in data file
temp <- clean.demo(demo_sl_mar25) # subsequent data files: pivot, select and rename relevant variables

demo_dat <- demo_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Save results in new data file -----------
write_csv(demo_dat, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "demo_dat_20250401.csv"))

test <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "demo_dat_20250401.csv"))



