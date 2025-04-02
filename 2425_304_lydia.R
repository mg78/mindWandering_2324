# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender, language background) - to report
# Mind wandering during mental maths task (incl. performance on mental maths task)
# Feedback

# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "demo_dat_20250401.csv"))
mw_scores <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_20250402.csv"))
fb_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "fb_20250402.csv"))

# 2) Combine data ---------
lydia_demo_fb <- demo_dat %>%
  left_join(fb_dat) 

# 3) Save results in new data file ---------
write_csv(lydia_demo_fb, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "lydia_demo_fb_20250402.csv"))

# NOTE: Other file needed: mw_20250402.csv

##########
# Abbreviations
# lydia_demo_fb_20250402.csv
# origID: participant number
# age: age in years
# gender: man, woman, non-binary, prefer not to say, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question
# order: Digital first vs. Physical first
# english: english is first language (yes, no)
# Followed by responses to feedback question

# In mw_20250402.csv
# origID: participant number
# order: Digital first vs. Physical first
# condition: Digital vs Physical
# rc_acc: percentage correct on reading comprehension questions
# rct: time taken to complete reading comprehension questions (in ms)
# mw_freq: percentage of probes on which participant reported to mind wander (intentional or unintentional)
# mw_intent_freq: of the probes on which participant reported to mind wander, percentage on which they did so intentionally