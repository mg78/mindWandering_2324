# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)


# Required variables
# Demographics (age, gender, language background) - to report
# Diagnosis (yes/no) - to report
# Model: MW frequency as a function of ADHD symptomology and word reading ability (ROAR; as a control)


# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2324_06data/mw2324_processed", "demo_dat_20240209.csv"))
diag_dat <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240207.csv"))
asrs_score <- read_csv(here("mw2324_06data/mw2324_processed", "asrs_score_20240209.csv"))
roar_score <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv"))
feng <- read_csv(here("mw2324_06data/mw2324_processed", "mw_20240208.csv"))

# 2) Combine data ---------
rachael_dat <- demo_dat %>%
  inner_join(diag_dat) %>%
  inner_join(asrs_score) %>%
  inner_join(roar_score) %>%
  inner_join(feng) %>%
  select(-handedness, -footedness, -rt_mean, -rct, -rc_acc)

# 3) Save results in new data file ---------
write_csv(rachael_dat, here("mw2324_06data/mw2324_processed", "rachael_dat_20240209.csv"))
