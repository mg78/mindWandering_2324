
#Library s 
library(here)
library(tidyverse)
library(rstatix) # allows group_by()
library(effectsize) # a neat little package that provides the effect size

library(Rmisc) # allows shorthand calculations of standard errors and confidence intervals
library(effects)
library(broom)
library(gridExtra)
library(PerformanceAnalytics)


#read in data 
diss_raw <- read_csv("katalina_dat_20240318.csv")
head(diss_raw)

#exluxion from roar 
diss_data <- diss_raw %>%
  filter(roar_exclude_rt == 0) %>%    # exclude people who performed unreliably on the ROAR
  filter(control == 1 | dyslexia == 1) # only include people who are either a control or have indicated they have dyslexia 

#Descriptives
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

#Correlation between reading ability (ROAR) and history of reading (ARHQ), both of which are indicators of dysleix traits 
(cor(diss_data$roar_acc, diss_data$arhq))


ggplot(diss_data, aes(x = roar_acc, y = arhq)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "reading ability")


cor_results <- cor.test(diss_data$roar_acc,
                        diss_data$arhq,
                        method = "pearson",
                        alternative = "two.sided") %>%
  tidy()

cor_results


#Model A MW as a function of reading ability (ROAR) + history of reading (ARHQ)

mA <- lm(mw_freq ~ roar_acc + arhq, diss_data)
summary(mA)



##Visulising countinous varibles 
p_arhq <- ggplot(diss_data, aes(x = arhq)) + geom_density(fill = "slateblue")
print(p_arhq)
p_arhq_hist <- ggplot(diss_data, aes(x =arhq)) + geom_histogram()
print(p_arhq_hist)

p_roar_acc <- ggplot(diss_data, aes(x = roar_acc)) + geom_density(fill = "slateblue")
p_roar_acc_hist <- ggplot(diss_data, aes(x = roar_acc)) + geom_histogram()
print(p_roar_acc_hist)
print(p_roar_acc)

p_mw <- ggplot(diss_data, aes(x = mw_freq)) + geom_density(fill = "slateblue")
print(p_mw)

#chart.Correlation(diss_data[,c(1,3,4)], histogram=TRUE, pch=19)
### -> "could not find function "chart.Correlation"

##Centering variables 
d_z <- diss_data %>% 
  mutate(arhq_z = (arhq - mean(arhq)) / sd(arhq),
         roar_acc_z = (roar_acc - mean(roar_acc)) / sd(roar_acc),
         mw_z = (mw_freq - mean(mw_freq)) / sd(mw_freq),
         rc_acc_z = (rc_acc - mean(rc_acc)) / sd(rc_acc)
  )

mA_z <- lm(mw_z ~ roar_acc_z + arhq_z, d_z)
summary(mA_z)

mA_z.2 <- lm(mw_freq ~ roar_acc_z + arhq_z, d_z)
summary(mA_z.2)
###do i mean center the outcome?



#Model B RC as a function of word reading ability (ROAR) and reading history (ARHQ) and MW

mB <- lm(rc_acc ~ roar_acc + arhq + mw_freq, diss_data)
summary(mB)

##after mean centering

mB <- lm(rc_acc ~ roar_acc + arhq + mw_freq, diss_data)
summary(mB)

mB_z <- lm(rc_acc_z ~ roar_acc_z + arhq_z + mw_z, d_z)
summary(mB_z)

mBi_z <- lm(rc_acc ~ arhq_z*mw_z, d_z)
summary(mBi_z)

mC_z <- lm(rc_acc_z ~ roar_acc_z + group2 + mw_z, d_z)
summary(mC_z)

ggplot(data=diss_data, aes(x = rc_acc)) +
  geom_histogram()

ggplot(data=diss_data, aes(y = rc_acc, x = roar_acc)) +
  geom_point() +
  geom_smooth (method = "lm")

ggplot(data=diss_data, aes(y = rc_acc, x = arhq)) +
  geom_point() +
  geom_smooth (method = "lm")

ggplot(data=diss_data, aes(y = rc_acc, x = mw_freq)) +
  geom_point() +
  geom_smooth (method = "lm")

#Visulaising data 

p_arhq <- ggplot(diss_data, aes(x = arhq)) + geom_density(fill = "slateblue")
print(p_arhq)
p_arhq_hist <- ggplot(diss_data, aes(x =arhq)) + geom_histogram()
print(p_arhq_hist)

p_roar_acc <- ggplot(diss_data, aes(x = roar_acc)) + geom_density(fill = "slateblue")
p_roar_acc_hist <- ggplot(diss_data, aes(x = roar_acc)) + geom_histogram()
print(p_roar_acc_hist)
print(p_roar_acc)

p_mw <- ggplot(diss_data, aes(x = mw_freq)) + geom_density(fill = "slateblue")
print(p_mw)

chart.Correlation(diss_data[,c(1,3,4)], histogram=TRUE, pch=19)
###does not work


##Box plot residuals 

boxplot(mA_z[['residuals']],
        main='Boxplot Residuals model A',
        ylap='residuals')


boxplot(mB_z[['residuals']],
        main='Boxplot Residuals model B',
        ylap='residuals')


###model A
plot(predictorEffect("roar_acc_z",mA_z))
plot(predictorEffect("arhq_z",mA_z))

plot(predictorEffect("roar_acc_z",mA_z, partial.residuals = TRUE))
plot(predictorEffect("arhq_z",mA_z, partial.residuals = TRUE))


###model b
plot(predictorEffect("mw_z",mB_z))
plot(predictorEffect("roar_acc_z",mB_z))
plot(predictorEffect("arhq_z",mB_z))

plot(predictorEffect("mw_z",mB_z, partial.residuals = TRUE))
plot(predictorEffect("roar_acc_z",mB_z, partial.residuals = TRUE))
plot(predictorEffect("arhq_z",mB_z, partial.residuals = TRUE))



mA_metrics <- augment(mA_z)
print(mA_metrics)
mB_metrics <- augment(mB_z)

par(mfrow = c(2,2))
plot(mA_z)
plot(mB_z)

##Interaction between MW and WRA



#Interaction Model A
ggplot(diss_data, aes(mw_freq, roar_acc)) +
  geom_point() +
  geom_abline(
    intercept = c(-10),
    slope = c(1),
    color = c("black")) +
  scale_color_manual(values = c("black")) +
  labs(x = "MW", y = "word reading ability") +
  theme_bw()

ggplot(diss_data, aes(mw_freq, roar_acc)) +
  geom_point(aes(color = factor(arhq)), show.legend = FALSE) +
  geom_abline(
    intercept = c(-10),
    slope = c(1,1),
    color = c("black", "grey")) +
  scale_color_manual(values = c("black", "grey")) +
  labs(x = "MW", y = "word reading ability") +
  theme_bw()
#######Not workiing
summary(diss_data)

chart.Correlation(diss_data[,c(2:4)], histogram = TRUE, phc=19)
#######Not workiing

### Intercept only modle - tells us the mean score 
summary(m0_A <- lm(mw_z ~ 1, d_z))
mean(d_z$mw_z)

m0_A <- lm(mw_z ~ 1, d_z)
summary(m0_A)


m1_A <- lm(mw_z ~ roar_acc_z, d_z)
summary(m1_A)

anova(m0_A, m1_A)
##### signifocant at <.01

m2_A <-lm(mw_z ~ roar_acc_z + arhq_z, d_z)
summary(m2_A)

anova(m1_A, m2_A)
####significant at <.05 so we reject the null hypothesis 



(mA_summary <- summary(m3_A <- lm(mw_z ~ roar_acc_z +
                                   arhq_z +
                                   roar_acc_z: arhq_z, d_z)))

anova(m2_A, mA_summary)
#####error 

# Interaction Model B 
m0_B <- lm(rc_acc ~ 1, d_z)
summary(m0_B)

m1_B <- lm(rc_acc ~ roar_acc_z, d_z)
summary(m1_B)

anova(m0_B, m1_B)
###significant at <.05


m2_B <- lm(rc_acc ~ roar_acc_z + arhq_z, d_z)
summary(m2_B)
#####not significant 

anova(m1_B, m2_B)
#### non significant so we fail to reject the null hypothesis
m3_B <- lm(rc_acc ~ roar_acc_z + arhq_z + mw_z, d_z)
summary(m3_B)

anova(m2_B, m3_B)
####non significant

(mB_summary <- summary(m4_B <- lm(rc_acc_z ~ roar_acc_z +
                                    arhq_z +
                                    mw_z +
                                    roar_acc_z: mw_z +
                                    arhq_z: mw_z, d_z)))

m2 <- lm(rc_acc ~ roar_acc_z + mw_z + roar_acc_z * mw_z, d_z)
summary(m2)

data$interaction_term <- data$x1 * data$x2


# Fit the multiple regression model with the interaction term
model <- lm(y ~ x1 + x2 + interaction_term, data = data)


# Print the summary of the regression model
summary(model)
anova(m1, m2)

diss_raw$interaction_term <- diss_raw$roar_acc * diss_raw$mw_freq

model_in_A  <- lm(rc_acc ~ roar_acc + mw_freq + interaction_term, data = diss_raw)

summary(model_in_A)

j













