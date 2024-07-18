
library(broom)
library(tidyverse)

#read in data 
diss_raw <- read_csv("katalina_dat_new.csv")
head(diss_raw)

#exluxion from roar 
diss_data <- diss_raw %>%
  filter(roar_exclude_rt == 0) %>%    # exclude people who performed unreliably on the ROAR
  filter(control == 1 | dyslexia == 1) 

diss_box_dyselxia <- diss_data %>%
  filter(dyslexia == 1)

diss_box_control <-diss_data %>%
  filter(control == 1)

#Descriptive statistics 
catagories <- c(diss_data$gender)
catagories_counts <- table(catagories)
print(catagories_counts)

catdig <- c(diss_data$group2)
catdig_count <-table(catdig)
print(catdig_count)

catlang <- c(diss_data$bilingual)
catlang_count <-table(catlang)
print(catlang_count)

range(diss_data$age)
mean(diss_data$age)
sd(diss_data$age)
summary(diss_data$age)

##Whole sample 
summary(diss_data$dyslexia)
summary(diss_data$roar_acc)
summary(diss_data$bilingual_text)
summary(diss_data$group2)
summary(diss_data$arhq)
summary(diss_data$mw_freq)
summary(diss_data$rc_acc)
summary(diss_box_dyselxia$roar_acc)


sd_roar <- sd(diss_data$roar_acc)
print(sd_roar)
sd_arhq <- sd(diss_data$arhq)
print(sd_arhq)
sd_mw <- sd(diss_data$mw_freq)
print(sd_mw)
sd_rc <- sd(diss_data$rc_acc)
print(sd_rc)


##Dyslexia_diagnosis group
mean(diss_box_dyselxia$rc_acc)
mean(diss_box_dyselxia$arhq)
mean(diss_box_dyselxia$mw_freq)
mean(diss_box_dyselxia$roar_acc)

sd(diss_box_dyselxia$rc_acc)
sd(diss_box_dyselxia$arhq)
sd(diss_box_dyselxia$mw_freq)
sd(diss_box_dyselxia$roar_acc)

#Control_diagnosiis
mean(diss_box_control$rc_acc)
mean(diss_box_control$arhq)
mean(diss_box_control$mw_freq)
mean(diss_box_control$roar_acc)

sd(diss_box_control$rc_acc)
sd(diss_box_control$arhq)
sd(diss_box_control$mw_freq)
sd(diss_box_control$roar_acc)



#Correlations 

##Correlation between ARHQ and ROAR
cor_results <- cor.test(diss_data$roar_acc,
                        diss_data$arhq,
                        method = "pearson",
                        alternative = "two.sided") %>%
  tidy()

cor_results

##Correlation between mind wandering and reading comprehension
cor_mw_rc <- cor.test(diss_data$mw_freq,
                      diss_data$rc_acc,
                      method = "pearson",
                      alternative = "two.sided") %>%
  tidy()

cor_mw_rc


#Multiple Regression 


d_z <- diss_data %>% 
  mutate(arhq_z = (arhq - mean(arhq)) / sd(arhq),
         roar_acc_z = (roar_acc - mean(roar_acc)) / sd(roar_acc),
         mw_z = (mw_freq - mean(mw_freq)) / sd(mw_freq),
         rc_acc_z = (rc_acc - mean(rc_acc)) / sd(rc_acc), 
         arhq_total_z = (arhq_total - mean(arhq_total)) / sd(arhq_total)
  )


##Model A MW as a function of reading ability (ROAR) + history of reading (ARHQ)       
         
mA_z <- lm(mw_freq ~ roar_acc_z + arhq_z, d_z)
summary(mA_z)

mA_total_z <- lm(mw_freq ~ roar_acc_z + arhq_total_z, d_z)
summary(mA_total_z)


##Model B RC as a function of word reading ability (ROAR) and reading history (ARHQ) and MW

mB_z <- lm(rc_acc ~ roar_acc_z + arhq_z + mw_z, d_z)
summary(mB_z)

mB_total_z <- lm(rc_acc ~ roar_acc_z + arhq_total_z + mw_z, d_z)
summary(mB_total_z)


##Model C
mC_z <- lm(rc_acc ~ roar_acc_z + group2 + mw_z, d_z)
summary(mC_z)




#Visualizing Data 


ggplot(diss_data, aes(x = roar_acc, y = arhq)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ROAR accuracy", y = "ARHQ total score",)


##model 1 MR: MW ~ ARHQ + ROAR

plot_mw_arhq <- ggplot(diss_data, aes(x = arhq, y = mw_freq)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ARHQ total score", y = "Mind wandering frequency")

(plot_mw_arhq)

plot_mw_roar <- ggplot(diss_data, aes(x = roar_acc, y = mw_freq)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ROAR accuracy", y = "Mind wandering frequency")

(plot_mw_roar)


(plot_mw_arhq + plot_mw_roar)


##Model 2 RC ~ ARHQ + ROAR + + MW

plot_RC_ARHQ <- ggplot(diss_data, aes(x = arhq, y = rc_acc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ARHQ total score", y = "Reading comprehension accuracy")

(plot_RC_ARHQ)

plot_RC_ARHQ_total <- ggplot(diss_data, aes(x = arhq_total, y = rc_acc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ARHQ total score", y = "Reading comprehension accuracy")

(plot_RC_ARHQ_total)




plot_RC_roar <- ggplot(diss_data, aes(x = roar_acc, y = rc_acc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "ROAR accuracy", y = "Reading comprehension accuracy")

(plot_RC_roar)


plot_RC_MW <- ggplot(diss_data, aes(x = mw_freq, y = rc_acc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "Mind wandering frequency", y = "Reading comprehension score")

(plot_RC_MW)



##Model 3 RC ~ Group 2 + ROAR + MW

plot_RC_Gro <- ggplot(diss_data, aes(x = group2, y = rc_acc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "Mind wandering frequency ", y = "Reading comprehsion score")


(plot_RC_Gro)

plt.figure(figsize)
sns.scatterplot


boxplot(values ~ group, data = data)



ggplot(diss_box_control, aes(x= control, y=rc_acc)) +
  geom_boxplot() +
  labs(y = "Reading comprehension")

ggplot(diss_box_dyselxia, aes(x= dyslexia, y=rc_acc)) + 
  geom_boxplot() +
  labs(y = "Reading comprehension")







