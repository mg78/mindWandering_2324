 #############
 # Read in, clean up and output into a single file the responses to the ASC
 # Gorilla: questionnaire-1p99
 # Assumes separate .csv files for each experiment/version/project/wave of data collection
 # Optional: Can use function check.n() for checking n participants
 # Main: 
 # 1) Read in data files
 # 2) Use function clean.asc() to pivot, select and rename relevant variables. For the first data file, assign output to a 
 # new data frame (e.g., 'asc_dat').
 # 3) For subsequent data files, use function clean.asc() again, but assign output to 'temp' and add rows to the original data frame.
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
 
 clean.asc <- function(data){
   data %>%
     select("Participant Private ID", "Task Name", "Task Version", "Question", "Key", "Response") %>%
     filter(!.data$Response == "BEGIN") %>%
     filter(!.data$Response == "END") %>%
     filter(.data$Key == "quantised") %>%
     rename(origID = `Participant Private ID`) %>%
     select(-`Task Name`, -`Task Version`, -Key) %>%
     mutate(Response = as.numeric(Response))
 }
 
 # 1) Read in data files -----------
 asc_sona_jan25 <- read_csv(here("mw2425_06data/ameliaSimmonds/data_exp_202242-v3_20250130", "data_exp_202242-v3_questionnaire-1p99.csv"))
 asc_sl_jan25 <- read_csv(here("mw2425_06data/ameliaSimmonds/data_exp_195194-v10_20250130", "data_exp_195194-v10_questionnaire-1p99.csv"))
 
 # 2) For first data file -----------
 nsub <- check.n(asc_sona_jan25) # check number of participants in data file
 asc_dat <- clean.asc(asc_sona_jan25) # first data file: pivot, select and rename relevant variables
 
 # 3) For subsequent data files -----------
 
 ## asc_sl_jan25
 n <- check.n(asc_sl_jan25) # check number of participants in data file
 temp <- clean.asc(asc_sl_jan25) # subsequent data files: pivot, select and rename relevant variables
 
 asc_dat <- asc_dat %>% # add participants 'temp' to main data frame
   add_row(temp)
 
 # 4) Calculate score -----------
 
 # Questions
 # Question 1:
 # I. Some students feel that they are very good at their university work BUT 
 # II. Other students worry about whether they can do the work assigned to them
 
 # Question 2:
 # I. Some students feel like they are just as smart as others their age BUT 
 # II. Other students aren't so sure and wonder if they are as smart 
 
 # Question 3:
 # I. Some students are pretty slow in finishing university work BUT 
 # II. Other students can do their university work quickly  
 
 # Question 4:
 # I. Some students often forget what they learn BUT 
 # II. Other students can remember things easily
 
 # Question 5:
 # I. Some students do very well at their modules BUT 
 # II. Other students don't do very well at their modules 
 
 # Question 6:
 # I. Some students have trouble figuring out the answers to their university work BUT 
 # II. Other students almost always can figure out the answers 
 
 # Answer options:
 # 1 = I. is really true for me
 # 2 = I. is sort of true for me
 # 3 = II. is sort of true for me
 # 4 = II. is really true for me
 
 # Questions that require recoding are: 1, 2, and 5
 
 asc_scores <- asc_dat %>%
   pivot_wider(names_from = Question, values_from = Response)
 
 # column names for the questions need to be changed to something more manageble
 
 colnames(asc_scores)[2] = "q1"
 colnames(asc_scores)[3] = "q2"
 colnames(asc_scores)[4] = "q3"
 colnames(asc_scores)[5] = "q4"
 colnames(asc_scores)[6] = "q5"
 colnames(asc_scores)[7] = "q6"

 # Now we can recode questions 1, 2, and 5
 
 asc_scores_rc <- asc_scores %>%
   mutate(q1_rc = recode(q1,
                        "1" = "4",
                        "2" = "3",
                        "3" = "2",
                        "4" = "1"),
          q2_rc= recode(q2,
                        "1"= "4",
                        "2" = "3",
                        "3" = "2",
                        "4" = "1"),
          q5_rc= recode(q5,
                        "1"= "4",
                        "2" = "3",
                        "3" = "2",
                        "4" = "1")) %>%
   mutate(q1_rc = as.numeric(q1_rc),
          q2_rc = as.numeric(q2_rc),
          q5_rc = as.numeric(q5_rc))

 # Calculate asc score
 asc_score <- asc_scores_rc %>%
   mutate(asc_mean = (q1_rc + q2_rc + q3 + q4 + q5_rc + q6)/6) %>%
   select(origID, asc_mean)

 # 5) Save results in new data file -----------
 write_csv(asc_score, here("mw2425_06data/ameliaSimmonds/mw2425_as_processed", "asc_score_20250216.csv"))
 
 test <- read_csv(here("mw2425_06data/ameliaSimmonds/mw2425_as_processed", "asc_score_20250216.csv"))
 

 