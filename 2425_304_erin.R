# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender, language background) - to report
# Mind wandering during mental maths task (incl. performance on mental maths task)
# Feedback

# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "demo_dat_20250215.csv"))
mw_scores <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "mw_scores_20250223.csv"))
fb_dat <- read_csv(here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "fb_20250215.csv"))

# 2) Combine data ---------
erin_demo_fb <- demo_dat %>%
  left_join(fb_dat) 

# 3) Save results in new data file ---------
write_csv(erin_demo_fb, here("mw2425_06data/erinRothwell-Wood/mw_2425_erw_processed", "erin_demo_fb_20240318.csv"))

# NOTE: Other file needed: mw_scores_20250215.csv

##########
# Abbreviations
# erin_demo_fb_20250215.csv
# origID: participant number
# age: age in years
# gender: man, woman, prefer not to say, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question
# set: set1 (eded) or set2 (dede)
# Followed by responses to feedback questions

# In mw_scores_20250215.csv
# origID: participant number
# condition: easy or difficult
# mm_acc: percentage correct on mental math task (%)
# mm_rt: average reaction time on mental math task (in ms)
# mw_freq: percentage of probes on which participant reported to mind wander (intentional or unintentional)
# mw_intent_freq: of the probes on which participant reported to mind wander, percentage on which they did so intentionally