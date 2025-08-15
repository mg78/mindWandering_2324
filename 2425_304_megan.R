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
mw <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "mw_megan_20250402.csv"))
fb_dat <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "fb_20250402.csv"))
smq <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "smq_scores_20250404.csv"))

smq_reliability <- read_csv(here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "smq_reliability_20250408.csv"))

# 2) Combine data ---------
megan_dat <- demo_dat %>%
  left_join(fb_dat) %>%
  left_join(mw) %>%
  select(-order) %>%
  left_join(smq)

# 3) Save results in new data file ---------
write_csv(megan_dat, here("mw2425_06data/mackin-boylan-tolentino/mw_2425_mbt_processed", "megan_dat_20250404.csv"))

# Also needed (for reliability analysis): smq_reliability_20250408.csv 
##########
# Abbreviations
# megan_dat_20250404.csv
# origID: participant number
# age: age in years
# gender: man, woman, non-binary, prefer not to say, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question
# english: english is first language (yes, no)
# Followed by response to the feedback question

# rc_acc: percentage correct on reading comprehension questions
# rct: time taken to complete reading comprehension questions (in ms)
# mw_freq: percentage of probes on which participant reported to mind wander (intentional or unintentional)
# mw_intent_freq: of the probes on which participant reported to mind wander, percentage on which they did so intentionally

# smq_mean: average score across all 27 items of the social media disorder questionnaire
# Followed by average score on subscales

# origID: participant number
# followed by responses on items 1 to 27 on the social media disorder questionnaire as separate variables
