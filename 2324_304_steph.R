# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender, language background) - to report
# Diagnosis (yes/no) - to report
# ASD questionnaire
# ROAR - control
# Model A: MW freq as a function of ASD symptomology (total) and ROAR + interaction?
# Model B: MW freq as a function of ASD categories and ROAR + interaction?

# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2324_06data/mw2324_processed", "demo_dat_20240209.csv"))
diag_dat <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240207.csv"))
asd_score <- read_csv(here("mw2324_06data/mw2324_processed", "asd_score_20240209.csv"))
roar_score <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv"))
feng <- read_csv(here("mw2324_06data/mw2324_processed", "mw_20240208.csv"))

# 2) Combine data ---------
steph_dat <- demo_dat %>%
  inner_join(diag_dat) %>%
  inner_join(asd_score) %>%
  inner_join(roar_score) %>%
  inner_join(feng) %>%
  select(-handedness, -footedness, -rt_mean, -rct, -rc_acc)

# 3) Save results in new data file ---------
write_csv(steph_dat, here("mw2324_06data/mw2324_processed", "steph_dat_20240209.csv"))


##########
# Abbreviations
# origID: participant number
# age: age in years
# gender: man, woman, prefer not to say, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question
# bilingual: language background ("Do you regard yourself as bilingual?"; No vs. Yes)
# bilingual_text: strongest language
# group: control vs. dyslexia vs. adhd vs. asd vs. multiple
# control: 0=no, 1=yes
# dyslexia: 0=no, 1=yes
# adhd: 0=no, 1=yes
# asd: 0=no, 1=yes
# asd_total: ASD questionnaire - sum of points on all questions
# roar_acc: proportion correct on ROAR
# roar_logRt: mean log rt on correct trials on ROAR
# total_tooFast: number of trials on which participant responded in less than 100 ms
# total_tooSlow: number of trails on which participant responded more slowly than 3 standard deviation above their own third quartile
# perc_tooFast: percentage of trials on which participant responded in less than 100 ms
# percentage of trails on which participant responded more slowly than 3 standard deviation above their own third quartile
# roar_exclude_rt: roar task - participants who responded too fast (< 100 ms; coded as 2) or too slow (based on 
# Hoaglin-Iglewicz procedure; coded as 1)
# mw_freq: percentage of probes on which participant reported to mind wander
