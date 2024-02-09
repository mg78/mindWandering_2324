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
