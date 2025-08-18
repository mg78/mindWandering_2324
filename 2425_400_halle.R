# Set-up ------------------------------------------------------------------
# Load relevant libraries
library(here)
library(tidyverse)

# Required variables
# Demographics (age, gender) - to report
# Diagnosis
# ASD
# ASRS
# Mind wandering data
# Feedback

# 1) Read in data files -----------
demo_dat <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "demo_dat_20250818.csv"))
diag_dat <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "diag_dat_20250818.csv"))
asd_dat <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "asd_scores_20250818.csv"))
asrs_dat <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "asrs_all_20250818.csv"))
mw_dat <- read_csv(here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "mw_scores_20250818.csv"))

fb_dat <- 
voucher <- 

# 2) Combine data ---------

asd_dat <- asd_dat %>% # Only keep derived scores (total and subscales)
  select(origID, ASD_Score_total:ASD_Score5)

halle_dat <- demo_dat %>%
  left_join(diag_dat) %>%
  left_join(asd_dat) %>%
  left_join(asrs_dat) %>%
  left_join(mw_dat)
  
# 3) Save results in new data file ---------
write_csv(halle_dat, here("work_project_data/mw/mw2425_06data/halleWarren/mw2425_hw_processed", "halle_dat_20250818.csv"))

##########
# Abbreviations
# halle_dat_20250814.csv
# origID: participant number

# Demographics
# age: age in years
# gender: man, woman, non-binary, other (see under gender_text)
# gender_text: self-reported gender if 'other' selected on previous question

# Diagnosis
# diag_gr: self-report group (control; autism; adhd; multiple; other); "Have you been diagnosed by a professional with any of the following?"
# other_text: self-report if 'other' selected on previous question
# await: whether participant is awaiting a diagnosis (yes, autism; yes, adhd; yes, both autism and adhd; no)
# other_med: whether participant currently uses medication that might affect their performance (yes, and within active time period; yes, but not
# within active time period; no)

# AQ-Short - autism traits
# Following Hoekstra et al. (2011) using the abridged version of the autism-specturem quotient (AQ-Short)
# Hoekstra et al. (2011) The Construction and Validation of an Abridged Version of the Autism-Spectrum Quotient (AQ-Short). Journal of 
# Autism and Developmental Disorders, 41 (5), 589-596.
# ASD_Score_total: Sum of scores on all questions, with a higher score indicating more autistic traits (minimum = 28, maximum = 112)
# ASD_Score1: Sum of scores on items on subscale 'social skills'
# ASD_Score2: Sum of scores on items on subscale 'routine'
# ASD_Score3: Sum of scores on items on subscale 'switching'
# ASD_Score4: Sum of scores on items on subscale 'imagination'
# ASD_Score5: Sum of scores on items on subscale 'numbers and patterns'

# ASRS - adhd risk
# Following Kessler et al. 2005, two scores are derived:
# 1) the total of responses across all six items (asrs_score), with a minimum of 0 and a maximum of 24 (6 x 4);
# 2) a dichotomous scoring where responses that scored ‘0’ indicated no risk and ‘1’ at risk (adhd_risk). For Items 1, 2 and 3 (Never, Rarely = 0,
# Sometimes, Often, Very Often =1) and items 4, 5 and 6 were the same besides ‘Sometimes’ = 0. Participants who totalled three or less were
# categorised as “Low Risk of ADHD” and four or more as “High risk of ADHD”. The higher the total score from 0-6, the more risk a
# participant had ADHD.
# asrs_total: Sum of responses (ranging from 0 for Never to 4 for Very Often) on all questions
# adhd_risk: Using dichotic scoring outlined by Kessler et al., 2025 to capture risk

# Mind wandering
# Following Jackson et al. 2012 and Welhaf et al. 2025.
# order: probe-caught first, self-report second (probe_selfreport) vs. self-report first, probe-caught second (selfrepor_probe)
# mw_pc_count: Number of probes on which participant reported any type of mind wandering (tune out, space out or zone out;
# in other words 'not on task' in the probe-caught condition). Minimum possible = 0, maximum possible = 12.
# mw_pc_aware_count: On probes where the participant reported to be mind wandering, the number of probes on which participant
# reported to be aware of mind wandering (tune out), rather than be unaware (zone out and space out). Minimum possible = 0,
# maximum possible = 12. If NA, then participant reported to be 'on task' on all probes.
# mw_sr_count: Number of times participant self-reported mind wandering (either tune out or space out). Minimum possible = 0, in 
# the case where the participant didn't self-report any instances of mind wandering.
