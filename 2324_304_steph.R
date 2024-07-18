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
demo_dat <- read_csv(here("mw2324_06data/mw2324_processed", "demo_dat_20240318.csv"))
diag_dat <- read_csv(here("mw2324_06data/mw2324_processed", "diag_dat_20240318.csv"))
asd_scores <- read_csv(here("mw2324_06data/mw2324_processed", "asd_scores_20240318.csv"))
roar_score <- read_csv(here("mw2324_06data/mw2324_processed", "roar_score_20240318.csv"))
feng <- read_csv(here("mw2324_06data/mw2324_processed", "mw_20240318.csv"))

# 2) Combine data ---------
steph_dat <- demo_dat %>%
  inner_join(diag_dat) %>%
  inner_join(asd_scores) %>%
  inner_join(roar_score) %>%
  inner_join(feng) %>%
  #select(-handedness, -footedness, -rt_mean, -rct, -rc_acc)
  select(-handedness, -footedness, -rt_mean, -rct)

# 3) Save results in new data file ---------
write_csv(steph_dat, here("mw2324_06data/mw2324_processed", "steph_dat_20240318.csv"))

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


############
# Preliminary analyses

steph_dat <- read_csv("mw2324_06data/mw2324_processed/steph_dat_20240216.csv")
steph_dat <- steph_dat %>%
  filter(roar_exclude_rt == 0) %>%
  mutate(roar_acc_sc = scale(roar_acc),
         roar_logRt_sc = scale(roar_logRt),
         asd_tot_sc = scale(ASD_Score_total),
         asd_switching_sc = scale(ASD_Score3),
         mw_freq_sc = scale(mw_freq))

ggplot(steph_dat, aes(x = roar_acc, y = rc_acc, colour = group)) +
  geom_point() +
  labs(x = "Word reading accuracy", y = "Reading comprehension accuracy") +
  theme_bw()

ggplot(steph_dat, aes(x = roar_logRt, y = rc_acc, colour = group)) +
  geom_point() +
  labs(x = "Word reading response time", y = "Reading comprehension accuracy") +
  theme_bw()

steph_dat_ac <- steph_dat %>%
  filter(group == "control" | group == "asd")


m1 <- lm(mw_freq ~ asd_tot_sc + roar_acc_sc, steph_dat_ac)

summary(m1)

m2 <- lm(mw_freq ~ asd_tot_sc + roar_logRt_sc, steph_dat_ac)

summary(m2)

m3 <- lm(rc_acc ~ mw_freq_sc + + asd_tot_sc + roar_acc_sc, steph_dat_ac)

summary (m3)

m4 <- lm(rc_acc ~ mw_freq_sc + + asd_tot_sc + roar_logRt_sc, steph_dat_ac)

summary (m4)

ggplot(steph_dat_ac, aes(x = ASD_Score1, y = mw_freq, colour = group)) +
  geom_point() +
  labs(x = "ASD Score 1", y = "Mind wandering frequency") +
  theme_bw()

ggplot(steph_dat_ac, aes(x = ASD_Score2, y = mw_freq, colour = group)) +
  geom_point() +
  labs(x = "ASD Score 2", y = "Mind wandering frequency") +
  theme_bw()

ggplot(steph_dat_ac, aes(x = ASD_Score3, y = mw_freq)) +
  geom_point() +
  geom_smooth (method = lm) +
  labs(x = "ASD Score 3", y = "Mind wandering frequency") +
  theme_bw()

ggplot(steph_dat_ac, aes(x = ASD_Score4, y = mw_freq, colour = group)) +
  geom_point() +
  labs(x = "ASD Score 4", y = "Mind wandering frequency") +
  theme_bw()

ggplot(steph_dat_ac, aes(x = ASD_Score5, y = mw_freq, colour = group)) +
  geom_point() +
  labs(x = "ASD Score 5", y = "Mind wandering frequency") +
  theme_bw()


m5 <- lm(mw_freq ~ asd_switching_sc + roar_acc_sc, steph_dat_ac)

summary (m5)

m6 <- lm(mw_freq ~ asd_switching_sc + roar_logRt_sc, steph_dat_ac)

summary (m6)

ggplot(steph_dat_ac, aes(x = asd_switching_sc, y = mw_freq)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(intercept = 34.4559099, slope = -8.0364309, colour = "red") +
  labs(x = "ASD Score 3", y = "Mind wandering frequency") +
  theme_bw()

m7 <- lm(rc_acc ~ asd_switching_sc + roar_acc_sc, steph_dat_ac)

summary(m7)

