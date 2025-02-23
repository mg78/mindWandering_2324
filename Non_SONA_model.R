library(here)
library(tidyverse)


demo_NS <- read_csv("demo_dat_non.csv")
asc <- read_csv("asc_score_20250216.csv")
roar <- read_csv("roar_score_Non.csv")
feng <- read_csv("mw_NON.csv")

dat_NS <- demo_NS %>%
  inner_join(roar) %>%
  inner_join(feng) %>%
  inner_join(asc) %>%
  select(-roar_logRt, -englishStrongest, -undergraduate)


head(dat_NS)

summary(dat_NS) # look at means and ranges within each variable

#not working currently, not sure if it needs to 
chart.Correlation(dat_NS[, c(2:4)], histogram=TRUE, pch=19) # note this does not include the outcome varibale comlum so dont use RC
# 2:4 is the colums included 


mean(dat_NS$rc_acc) # reading comp because its our outcome 

summary(m0 <- lm(rc_acc ~ 1, dat_NS)) # putting just 1 = estimate the intercept.

summary(m1 <- lm(rc_acc ~ mw_freq, dat_NS)) 
# by default, lm() will model the "1" so it does not needed to be hard coded when other predictors are present

range(dat_NS$asc_mean) #new factor, e.g ASC or WR or Age 

dat_NS$c_asc_mean <- dat_NS$asc_mean - mean(dat_NS$asc_mean) # centring variables wrong i think oops 
dat_NS$c_mw_freq <- dat_NS$mw_freq - mean(dat_NS$mw_freq)
dat_NS$c_mw_freq <- dat_NS$mw_freq - mean(dat_NS$mw_freq)
dat_NS$c_roar_acc <- dat_NS$roar_acc - mean(dat_NS$roar_acc)
dat_NS$c_rt_mean <- dat_NS$rt_mean - mean(dat_NS$rt_mean)


summary(m2 <- lm(rc_acc ~ mw_freq + c_asc_mean, dat_NS)) #RC then MW then AGE??
summary(m3 <- lm(rc_acc ~ c_mw_freq + c_asc_mean, dat_NS))
summary(m4 <- lm(rc_acc ~ c_mw_freq + c_asc_mean + c_roar_acc, dat_NS))

(m4_summary <- summary(m4_i <- lm(rc_acc ~ c_mw_freq + 
                                    c_asc_mean + 
                                    c_roar_acc + 
                                    c_mw_freq:c_roar_acc, dat_NS)))


#no difference, was i meant to be centring variables / have i done the centring wrong 
summary(m5 <- lm(rc_acc ~ c_mw_freq + c_asc_mean + c_rt_mean, dat_NS))
summary(m6 <- lm(rc_acc ~ mw_freq + asc_mean + rt_mean, dat_NS))



#graph for rc, mw and gender
ggplot(dat_NS, aes(c_mw_freq, rc_acc)) +
  geom_point(aes(color = factor(gender)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(m5)[1], sum(coef(m5)[1:2])),
    slope = c(coef(m5)[4], sum(coef(m5)[4:5])),
    color = c("black", "grey")) +
  scale_color_manual(values = c("black", "grey")) +
  labs(x = "gender", y = "Reading acc") + 
  theme_bw()


#Future problems 
# Need to centre age using this as will be categorical when i have child data 
comp_data$c_MW <- d$MW - mean(d$MW) # mean age

head(dat_NS$c_age) # positive values = mom completed high school
