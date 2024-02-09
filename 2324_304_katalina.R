# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender, language background) - to report
# Diagnosis (yes/no) - to report
# Model A: RC as a function of group and MW
# Model B: RC as a function of word reading ability (ROAR) and reading history (ARHQ) and MW

# 1) Read in data files -----------
demo_dat <- read_csv(here("mw2324_06data/mw2324_processed", "demo_dat_20240209.csv"))
diag_dat <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240207.csv"))
arhq_score <- read_csv(here("mw2324_06data/mw2324_processed", "arhq_score_20240207.csv"))
roar_score <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240207.csv"))
feng <- read_csv(here("mw2324_06data/mw2324_processed", "mw_20240208.csv"))

# 2) Combine data ---------
katalina_dat <- demo_dat %>%
  inner_join(diag_dat) %>%
  inner_join(arhq_score) %>%
  inner_join(roar_score) %>%
  inner_join(feng) %>%
  select(-handedness, -footedness, -rt_mean, -rct)

# 3) Save results in new data file ---------
write_csv(katalina_dat, here("mw2324_06data/mw2324_processed", "katalina_dat_20240209.csv"))
