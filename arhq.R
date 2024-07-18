# Read in, clean up and output into a single file the responses to the ARHQ
# Gorilla: questionnaire-mf58
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.arhq() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'arhq_dat').
# 3) For subsequent data files, use function clean.arhq() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.arhq <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    pivot_wider(names_from = `Question Key`, values_from = Response) %>%
    select("Participant Private ID",
           "attitude_school-quantised",
           "difficulty_reading-quantised",
           "help_reading-quantised",
           "reverse-quantised",
           "diffiuclty_names-quantised",
           "compare_reading-quantised",
           "struggle_schoolwork-quantised",
           "struggle_EnglishClass-quantised",
           "attitude_reading-quantised",
           "reading_pleasure-quantised",
           "reading_speed-quantised",
           "reading_work-quantised",
           "learning_spelling-quantised",
           "compare_spelling-quantised",
           "remember_names-quantised",
           "remember_address-quantised",
           "remember_instructions-quantised",
           "currently_reverse-quantised",
           "books_read-quantised",
           "magazines_read-quantised",
           "daily_newspaper-quantised",
           "sunday_newspaper-quantised") %>%
    rename(origID = `Participant Private ID`) %>%
    mutate(origID = as.factor(origID),
           attitude_school = as.numeric(`attitude_school-quantised`),
           difficulty_reading = as.numeric(`difficulty_reading-quantised`),
           help_reading = as.numeric(`help_reading-quantised`),
           reverse = as.numeric(`reverse-quantised`),
           difficulty_names = as.numeric(`diffiuclty_names-quantised`),
           compare_reading = as.numeric(`compare_reading-quantised`),
           struggle_schoolwork = as.numeric(`struggle_schoolwork-quantised`),
           struggle_English = as.numeric(`struggle_EnglishClass-quantised`),
           attitude_reading = as.numeric(`attitude_reading-quantised`),
           reading_pleasure = as.numeric(`reading_pleasure-quantised`),
           reading_speed = as.numeric(`reading_speed-quantised`),
           reading_work = as.numeric(`reading_work-quantised`),
           learning_spelling = as.numeric(`learning_spelling-quantised`),
           compare_spelling = as.numeric(`compare_spelling-quantised`),
           remember_names = as.numeric(`remember_names-quantised`),
           remember_address = as.numeric(`remember_address-quantised`),
           remember_instructions = as.numeric(`remember_instructions-quantised`),
           currently_reverse = as.numeric(`currently_reverse-quantised`),
           books_read = as.numeric(`books_read-quantised`),
           magazines_read = as.numeric(`magazines_read-quantised`),
           daily_newspaper = as.numeric(`daily_newspaper-quantised`),
           sunday_newspaper = as.numeric(`sunday_newspaper-quantised`)) %>%
    select(origID,
           attitude_school, 
           difficulty_reading, 
           help_reading, 
           reverse, 
           difficulty_names,
           compare_reading,
           struggle_schoolwork,
           struggle_English,
           attitude_reading,
           reading_pleasure,
           reading_speed,
           reading_work,
           learning_spelling,
           compare_spelling,
           remember_names,
           remember_address,
           remember_instructions,
           currently_reverse,
           books_read,
           magazines_read,
           daily_newspaper,
           sunday_newspaper) %>%
    pivot_longer(attitude_school:sunday_newspaper, names_to = "question", values_to = "response")
}

# 1) Read in data files -----------
arhq_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10_20240318", "data_exp_155541-v10_questionnaire-mf58.csv"))
arhq_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240318", "data_exp_158033-v3_questionnaire-mf58.csv"))

# 2) For first data file -----------
nsub <- check.n(arhq_sona_feb24) # check number of participants in data file
arhq_dat <- clean.arhq(arhq_sona_feb24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## arhq_sl_feb24
n <- check.n(arhq_sl_feb24) # check number of participants in data file
temp <- clean.arhq(arhq_sl_feb24) # subsequent data files: pivot, select and rename relevant variables
arhq_dat <- arhq_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Once dataframe with participants from all setups is created, score responses and calculate score
# Lefly et al. suggested to score the ARHQ in the following manner: each question can have a score
# of 0 to 4, with lower scores being indicative of fewer problems. To derive an overall score, add up
# responses to all questions and divide by the total maximum score possible. With regard to the
# the total number of possible points, Lefly had 4 x 23 = 92, we have 22 questions x 4 = 88. 
# Lefly et al. report that a score of 0.4 or higher shows good sensitivity and specificity to identify
# a history of reading problems.

arhq_score <- arhq_dat %>%
  mutate(lefly = recode(response,
                        "1"="0",
                        "2"="1",
                        "3"="2",
                        "4"="3",
                        "5"="4"),
         lefly = as.numeric(lefly)) %>%
  group_by(origID) %>%
  summarise(arhq_total = sum(lefly, na.rm = TRUE),
            arhq = arhq_total/88)

# 5) Save results in new data file
write_csv(arhq_score, here("mw2324_06data/mw2324_processed", "arhq_score_20240318.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "arhq_score_20240318.csv"))
