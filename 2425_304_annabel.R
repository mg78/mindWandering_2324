# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender, language background) - to report
# Mind wandering data per probe (trial)
# Feedback

# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "demo_dat_20250401.csv"))
mw_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_dat_20250417.csv"))
fb_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "fb_20250402.csv"))

# 2) Combine data ---------
annabel_demo_fb <- demo_dat %>%
  left_join(fb_dat) 

# 3) Save results in new data file ---------
write_csv(annabel_demo_fb, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "annabel_demo_fb_20250402.csv"))

# NOTE: Other file needed: mw_dat_20250409.csv

mw_dat <- mw_dat %>%
  mutate(response = as.factor(response)) %>%
  mutate(response = fct_infreq(response))

levels(mw_dat$response)
##########
# Abbreviations
# annabel_demo_fb_20250402.csv
# origID: participant number
# age: age in years
# gender: man, woman, non-binary, prefer not to say, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question
# order: Digital first vs. Physical first
# english: english is first language (yes, no)
# Followed by responses to the feedback question

# In mw_dat_20250409.csv
# origID: participant number
# order: Digital first vs. Physical first
# condition: Digital vs Physical
# passage: passage number
# trial: probe within condition (1-14 or 1-13)
# probeNumber: probe number across experiment (1-27)
# response: raw response to mind wandering probe (on task or intentional or unintentional)
# mw_response: recoded from response to indicate whether the participant was on task (0) or mind wandered (intentional or unintentional) 
# mw_intent: recoded from response to indicate whether participant mind wandered unintentionally (0) or intentionally (1) on probes where they indicated that they mind wandered