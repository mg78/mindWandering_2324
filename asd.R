# Read in, clean up and output into a single file the responses to the ARHQ
# Gorilla: questionnaire-gew1
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.asd() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'asd_dat').
# 3) For subsequent data files, use function clean.asd() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.asd <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version", "Question Key", "Response") %>%
    filter(!.data$`Question Key` == "BEGIN QUESTIONNAIRE") %>%
    filter(!.data$`Question Key` == "END QUESTIONNAIRE") %>%
    filter(grepl("-quantised", .data$`Question Key`)) %>%
    rename(origID = `Participant Private ID`)
}

# 1) Read in data files -----------
asd_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10_20240318", "data_exp_155541-v10_questionnaire-gew1.csv"))
asd_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240318", "data_exp_158033-v3_questionnaire-gew1.csv"))

# 2) For first data file -----------
nsub <- check.n(asd_sona_feb24) # check number of participants in data file
asd_dat <- clean.asd(asd_sona_feb24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## asd_sl_feb24
n <- check.n(asd_sl_feb24) # check number of participants in data file
temp <- clean.asd(asd_sl_feb24) %>% # subsequent data files: pivot, select and rename relevant variables
  mutate(Response = as.numeric(Response))
  
asd_dat <- asd_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Calculate score -----------
asd_score <- asd_dat %>%
  group_by(origID) %>%
  summarise(asd_total = sum(Response, na.rm = TRUE))

asd_scores <- asd_dat %>%
  select(origID, `Question Key`, Response) %>%
  pivot_wider(names_from = `Question Key`, values_from = "Response") %>%
  rename(
    "Q1" = "question 1 -quantised",
    "Q2" = "question 2 -quantised",
    "Q3" = "question 3 -quantised",
    "Q4" = "question 4 -quantised",
    "Q5" = "question 5-quantised",
    "Q6" = "question 6 -quantised",
    "Q7" = "question 7-quantised", 
    "Q8" = "question 8 -quantised",
    "Q9" = "question 9 -quantised", 
    "Q10" = "question 10 -quantised",
    "Q11" = "question 11 -quantised",
    "Q12" = "question 12 -quantised", 
    "Q13" = "question 13 -quantised", 
    "Q14" = "question 14 -quantised", 
    "Q15" = "question 15 -quantised",
    "Q16" = "question 16-quantised", 
    "Q17" = "question 17-quantised", 
    "Q18" = "question 18-quantised", 
    "Q19" = "question 19 -quantised", 
    "Q20" = "question 20-quantised",
    "Q21" = "question 21-quantised", 
    "Q22" = "question 22-quantised", 
    "Q23" = "question 23 -quantised", 
    "Q24" = "question 24-quantised", 
    "Q25" = "question 25-quantised",
    "Q26" = "question 26 -quantised",
    "Q27" = "question 27 -quantised", 
    "Q28" = "question 28-quantised") %>%
    mutate(Q2_rc= recode(Q2,
                         "1"= "4",
                         "2" = "3",
                         "3" = "2",
                         "4" = "1"),
           Q4_rc= recode(Q4,
                         "1"= "4",
                         "2" = "3",
                         "3" = "2",
                         "4" = "1"),
           Q6_rc= recode(Q6,
                         "1"= "4",
                         "2" = "3",
                         "3" = "2",
                         "4" = "1"),
           Q7_rc= recode(Q7,
                         "1"= "4",
                         "2" = "3",
                         "3" = "2",
                         "4" = "1"),
           Q10_rc= recode(Q10,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q11_rc= recode(Q11,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q13_rc= recode(Q13,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q14_rc= recode(Q14,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q15_rc= recode(Q15,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q16_rc= recode(Q16,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q17_rc= recode(Q17,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q18_rc= recode(Q18,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q20_rc= recode(Q20,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1"),
           Q23_rc= recode(Q23,
                          "1"= "4",
                          "2" = "3",
                          "3" = "2",
                          "4" = "1")) %>%
  mutate(Q1 = as.numeric(Q1),
         Q2_rc = as.numeric(Q2_rc),
         Q3 = as.numeric(Q3),
         Q4_rc = as.numeric(Q4_rc),
         Q5 = as.numeric(Q5),
         Q6_rc = as.numeric(Q6_rc),
         Q7_rc = as.numeric(Q7_rc),
         Q8 = as.numeric(Q8),
         Q9 = as.numeric(Q9),
         Q10_rc = as.numeric(Q10_rc),
         Q11_rc = as.numeric(Q11_rc),
         Q12 = as.numeric(Q12),
         Q13_rc = as.numeric(Q13_rc),
         Q14_rc = as.numeric(Q14_rc),
         Q15_rc = as.numeric(Q15_rc),
         Q16_rc = as.numeric(Q16_rc),
         Q17_rc = as.numeric(Q17_rc),
         Q18_rc = as.numeric(Q18_rc),
         Q19 = as.numeric(Q19),
         Q20_rc = as.numeric(Q20_rc),
         Q21 = as.numeric(Q21),
         Q22 = as.numeric(Q22),
         Q23_rc = as.numeric(Q23_rc),
         Q24 = as.numeric(Q24),
         Q25 = as.numeric(Q25),
         Q26 = as.numeric(Q26),
         Q27 = as.numeric(Q27),
         Q28 = as.numeric(Q28)) %>%
  group_by(origID) %>%
  mutate(ASD_Score_total = sum(Q1, Q2_rc, Q3, Q4_rc, Q5, Q6_rc, Q7_rc, Q8, Q9, Q10_rc,Q11_rc, Q12, Q13_rc,Q14_rc,Q15_rc,Q16_rc,Q17_rc,Q18_rc, Q19, Q20_rc, Q21, Q22,Q23_rc, Q24, Q25,Q26, Q27, Q28),
         ASD_Score1 = sum(Q1, Q2_rc, Q3, Q4_rc, Q5, Q6_rc, Q7_rc, Q8),
         ASD_Score2 = sum(Q9, Q10_rc,Q11_rc),
         ASD_Score3 = sum(Q12, Q13_rc,Q14_rc,Q15_rc),
         ASD_Score4 = sum(Q16_rc,Q17_rc,Q18_rc, Q19, Q20_rc, Q21, Q22,Q23_rc),
         ASD_Score5 = sum(Q24, Q25,Q26, Q27, Q28)
  )

# 5) Save results in new data file -----------
write_csv(asd_scores, here("mw2324_06data/mw2324_processed", "asd_scores_20240318.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "asd_scores_20240318.csv"))
