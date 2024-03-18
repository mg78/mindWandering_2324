# Read in, clean up and output into a single file the responses to the ROAR
# Gorilla: task-simw
# Assumes separate .csv files for each experiment/version/project/wave of data collection
# Optional: Can use function check.n() for checking n participants
# Main: 
# 1) Read in data files
# 2) Use function clean.roar() to pivot, select and rename relevant variables. For the first data file, assign output to a 
# new data frame (e.g., 'roar_dat').
# 3) For subsequent data files, use function clean.roar() again, but assign output to 'temp' and add rows to the original data frame.
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

clean.roar <- function(data){
  data %>%
    select("Participant Private ID", "Task Name", "Task Version",
           "display", "Attempt",
           "Trial Number", "Spreadsheet Row", "Response", "Reaction Time", "Correct") %>%
    filter(.data$Attempt == 1) %>% # get rows with relevant info
    filter(!.data$display == "Practice") %>% # get rid off practice trials
    rename(origID = `Participant Private ID`,
           rt = `Reaction Time`) %>%
    mutate(origID = as.factor(origID),
           rt = as.double(rt))
}

# 1) Read in data files -----------
roar_sona_feb24 <- read_csv(here("mw2324_06data/data_exp_155541-v10-3_20240207", "data_exp_155541-v10_task-simw.csv"))
roar_sl_feb24 <- read_csv(here("mw2324_06data/data_exp_158033-v3_20240207", "data_exp_158033-v3_task-simw.csv"))

# 2) For first data file -----------
nsub <- check.n(roar_sona_feb24) # check number of participants in data file
roar_dat <- clean.roar(roar_sona_feb24) # first data file: pivot, select and rename relevant variables

# 3) For subsequent data files -----------

## roar_sl_feb24
n <- check.n(roar_sl_feb24) # check number of participants in data file
temp <- clean.roar(roar_sl_feb24) # subsequent data files: pivot, select and rename relevant variables
roar_dat <- roar_dat %>% # add participants 'temp' to main data frame
  add_row(temp)

# 4) Data cleaning and processing as outlined in Yeatman et al. : -------
# Once a dataframe with participants from all setups is created, response times and accuracy scores are
# 
# Data cleaning (following Yeatman et al., 2021; 'Data analysis (study 1)')

# 4.1) Log-transform Rts
# 4.2) Identify outliers by calculating median RTs for each participant and exclude participants whose median RT
# was more than 3 SDs below the sample mean. Check accuracy for those participants (likely to be at chance level).
# So far, none present in our data.
# 4.3) For analysis of RT data, responses shorter than 0.1 s or longer than 5 s were removed. Then, quartiles
# and the interquartile range (IQR) of the RT distribution were calculated per participant. Responses that were
# longer than 3 times the IQR from the 3rd quartile, or shorter than 3 times the IQR from the 1st quarter, were
# excluded.

# Note ad 4.3): Yeatman et al. (2021) excluded responses shorter than 0.2 s, which is standard. However, likely due to the
# way in which the task was programmed, there are a lot of responses below 0.2 s, and it seems unlikely
# that that is all due to task non-compliance. Therefore, decided to go with 0.1 s.

lowcut <- log(100) #define threshold for anticipations
highcut <- log(5000)

zcut <- 3 #define cut off for slow responses

roar_dat <- roar_dat %>%
  mutate(logRt = log(rt),
         anticipations = case_when(logRt < lowcut ~ 1, TRUE ~ 0),
         tooslow = case_when(logRt > highcut ~ 1, TRUE ~ 0))

# Have a look at sample and participant rts
sampleRts <- roar_dat %>%
  summarise(medLogRt = median(logRt, na.rm = TRUE),
            minLogRt = min(logRt, na.rm = TRUE),
            maxLogRt = max(logRt, na.rm = TRUE),
            meanLogRt = mean(logRt, na.rm = TRUE),
            medRt = median(rt, na.rm = TRUE),
            minRt = min(rt, na.rm = TRUE),
            maxRt = max(rt, na.rm = TRUE),
            meanRt = mean(rt, na.rm = TRUE))

participantsRts <- roar_dat %>%
  group_by(origID) %>%
  summarise(medLogRt = median(logRt, na.rm = TRUE),
            minLogRt = min(logRt, na.rm = TRUE),
            maxLogRt = max(logRt, na.rm = TRUE),
            meanLogRt = mean(logRt, na.rm = TRUE),
            medRt = median(rt, na.rm = TRUE),
            minRt = min(rt, na.rm = TRUE),
            maxRt = max(rt, na.rm = TRUE),
            meanRt = mean(rt, na.rm = TRUE))

# compute limits for each participant individually
limits <- roar_dat %>%
  filter(Correct == 1) %>%
  filter(anticipations == 0) %>%
  filter(tooslow == 0) %>%
  group_by(origID) %>%
  summarise(lowerQuartile = quantile(logRt, probs = 0.25, na.rm = TRUE),
            upperQuartile = quantile(logRt, probs = 0.75, na.rm = TRUE),
            quartileDiff = upperQuartile - lowerQuartile,
            lowerLimit = lowerQuartile - zcut*quartileDiff,
            upperLimit = upperQuartile + zcut*quartileDiff)

nsub <- length(limits$origID)
roar_dat$outlier<-0

for (i in 1:nsub) { # loop through subjects
  subname <- roar_dat$origID[i] # find subject 
  myrows <- which(roar_dat$origID==subname) # select rows for this subject
  #NB myrows is NOT a consecutive series, because done in blocks
  firstrow <-min(myrows)
  tmp <- data.frame(roar_dat[myrows,])
  
  # create outlier variable
  w1 <- which(roar_dat$logRt[myrows]> limits$upperLimit[i]) #rows with outlier, nb RELATIVE to myrows range
  roar_dat$outlier[myrows[w1]]<-1
}
#For RT we just remove slow outliers, so ignore lower_limit
roar_dat$logRt[roar_dat$outlier>0]<-NA #logRt variable now has outliers excluded as NA
roar_dat$logRt[roar_dat$anticipations>0]<-NA #logRt variable now has anticipations excluded as NA
roar_dat$logRt[roar_dat$tooslow>0]<-NA #logRt variable now has rts that were too slow excluded as NA

# summarise n anticipations and slow responses per participant
outliers <- roar_dat %>%
  select(origID, anticipations, outlier) %>% 
  group_by(origID) %>%
  summarise(total_tooFast = sum(anticipations),
            total_tooSlow = sum(outlier)) %>%
  mutate(perc_tooFast = (total_tooFast/84)*100,
         perc_tooSlow = (total_tooSlow/84)*100) %>%
  mutate(
    roar_exclude_rt = case_when(
      perc_tooFast > 10 ~ 2, # participants with anticipations (rt < log(100) ) on more than 10% of trials coded as 2
      perc_tooSlow > 10 ~ 1, # participants with excessively slow responses (Hoaglin-Iglewicz procedure) on more than 10% of trials coded as 1
      TRUE ~ 0)
  )

# Have a look at the histograms with/without outliers
all_rt <- ggplot(roar_dat, aes(rt)) +
  geom_histogram()
all_rt

all_logRt <- ggplot(roar_dat, aes(log(rt))) +
  geom_histogram()
all_logRt

out_logRt <- ggplot(roar_dat, aes(logRt)) +
  geom_histogram()
out_logRt

check_tooSlow <- roar_dat %>%
  filter(Correct == 1) %>%
  filter(rt > 7000)

# Calculating scores ---------
### now select relevant columns
roar_score <- roar_dat %>%
  select(origID, rt, logRt, Correct) %>%
  rename(correct = Correct) %>%
  group_by(origID) %>%
  filter(correct == 1) %>%
  summarise(roar_acc = sum(correct)/84,  # 4.4) Calculate proportion correct (most informative measure)
            roar_logRt = mean(logRt, na.rm = TRUE)) # 4.5) Calculate mean logRT

### add info outliers
roar_score <- roar_score %>%
  left_join(outliers)

# 5) Save results in new data file ---------
write_csv(roar_score, here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv"))

test <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv"))

##### Some preliminary analyses
diagnosis_dat <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240213.csv"))

roar_dat <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv")) %>%
  inner_join(diagnosis_dat) %>%
  filter(roar_exclude_rt == 0) 

summary(roar_dat)

ggplot(roar_dat, aes(x = roar_acc)) +
  geom_histogram() +
  labs(x = "Accuracy") +
  theme_bw() +
  xlim(c(0,1))

ggplot(roar_dat, aes(x = roar_logRt)) +
  geom_histogram() +
  labs(x = "Accuracy") +
  theme_bw()


ggplot(roar_dat, aes(x = roar_logRt, y = roar_acc, colour = group)) +
  geom_point() +
  labs(x = "Reaction time (log)", y = "Accuracy (proportion correct)") +
  theme_bw()

