library(tidyverse)
library(data.table)
setwd("/Users/simon/Documents//Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")
#Casegruppeanalyse: pregnancy rate (blev gravid/blev ikke gravid), og for dem der blev gravide live birth (ja/nej).
# Analysen skal være logistisk regression med OR (+ 95% konfidensinterval) 
# for hver af de metaboliske biomarkører – også både justeret (BMI og alder) og ujusteret

#Load data
d = readxl::read_excel("Data/AbHab2 19-11-2024.xlsx")
d = filter(d, patient_type %in% c(1))
table(d$group)
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))


#We only look at the cases here
d = filter(d, group == "Cases")

#Quick summary 
table(d$`Pregnancy rate`) #563/715 = 79% #0, did not achieve pregnancy | 1, achieved pregnancy
cat("Percentage: ", round( (table(d$`Pregnancy rate`)[2] / nrow(d)), 2), "% achieved pregnancy" )
table(d$Live_birth, useNA = "always") #Not sure what the numbers indicate #0 No | 1 Live birth | 2/3 never achieved pregnancy or excluded for other reasons 
table(d$outcome_pregnancy_hv) 


#We want to predict both pregnancy and, in case of pregnancy, live birth...  
#Lets summarize the data at a per sample level

Samples = d%>%
  dplyr::select(ID = record_id, patient_type, `Pregnancy rate`, Live_birth)%>%
  mutate(
    Got_pregnant = ifelse(`Pregnancy rate` == 1, T, F),
 #   Excluded = F,
  #  Excluded = ifelse(Live_birth %in% c(2,3), T, Excluded),
#    Live_birth = ifelse(Live_birth == 1, T, F), #We also count 3rd trimester births as live births
    Combined = "NA",
    Combined = ifelse(`Pregnancy rate` == 1 & Live_birth == 0, "Pregnant but no live birth", Combined),
    Combined = ifelse(`Pregnancy rate` == 1 & Live_birth == 1, "Pregnant and live birth", Combined),
    Combined = ifelse(`Pregnancy rate` == 0 & Live_birth == 2, "Not pregnant", Combined),
    Combined = ifelse(`Pregnancy rate` == 1 & Live_birth == 3, "Ongoing pregnant / moved away", Combined),
    Combined = ifelse(is.na(Live_birth), "Missing data", Combined)
  )

table(Samples$Combined) #Ask ML if missing data is actually cause they were never pregnant...

### Biomarkers
bio = d%>%
  dplyr::select(ID = record_id, age, BMI, smoking, hba1c, glucose = glucose_middel, total_kolesterol, ldl, hdl, triglycerider = triglycerid)


### Time to do some modelling:
### I can summarize pregnancny and live birth into a 4-level factor . Or, i can make two separate models. 
# A nested structure could be represented in Bayesian, but hard for me to implement and unfamiliar to the readers.
#So... Either two binom GLM or one multilevel, with no inherent relation between the levels. 
# I gues two seperate are best. Easier to conclude from.

#### Predicting getting pregnant  ######
#HBA1C
d = left_join(Samples, bio)

gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + hba1c",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
hba1c = data.frame(feature = "hba1c", 
                   pval = coef(summary(gg))[,4][5],
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[5]]),
                     'confint_97,5%' = exp(confint(gg)[[10]]))

#glucose
gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + glucose",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
glucose = data.frame(feature = "glucose", 
                     pval = coef(summary(gg))[,4][5],
                   odds_ratio_glm = odds_ratio,
                   'confint_2,5%' = exp(confint(gg)[[5]]),
                   'confint_97,5%' = exp(confint(gg)[[10]]))

#total_kolesterol
gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + total_kolesterol",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
total_kolesterol = data.frame(feature = "Total kolesterol", 
                              pval = coef(summary(gg))[,4][5],
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[5]]),
                     'confint_97,5%' = exp(confint(gg)[[10]]))


#ldl
gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + ldl",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
ldl = data.frame(feature = "ldl", 
                 pval = coef(summary(gg))[,4][5],
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[5]]),
                     'confint_97,5%' = exp(confint(gg)[[10]]))


#hdl
gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + hdl",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
hdl = data.frame(feature = "hdl", 
                 pval = coef(summary(gg))[,4][5],
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[5]]),
                     'confint_97,5%' = exp(confint(gg)[[10]]))

#triglycerider
gg = glm(
  data = d,
  formula = "Got_pregnant ~ age + BMI + smoking + triglycerider",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
triglycerider = data.frame(feature = "triglycerider",
                           pval = coef(summary(gg))[,4][5],
                 odds_ratio_glm = odds_ratio,
                 'confint_2,5%' = exp(confint(gg)[[5]]),
                 'confint_97,5%' = exp(confint(gg)[[10]]))


###Assemble output
out = bind_rows(hba1c, 
                #glucose, 
                total_kolesterol, ldl, hdl, triglycerider)
# out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "glucose", "Total kolesterol", "ldl", "hdl", "triglycerider")),
#                      labels = rev(c("Age", "BMI","HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL","Triglycerides")))

out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "Total kolesterol", "ldl", "hdl", "triglycerider")),
                     labels = rev(c("Age", "BMI","HbA1c", "Total Cholesterol", "LDL Cholesterol", "HDL Cholesterol","Triglycerides")))

out = out%>%
  mutate(
    pval2 = "NS",
    pval2 = ifelse(pval < 0.05, "p \u2264 0.05", pval2),
    pval2 = ifelse(pval < 0.01, "p \u2264 0.01", pval2),
    pval2 = ifelse(pval < 0.001, "p \u2264 0.001", pval2),
    Test = "Live birth, in case of pregnancy"
  )
live_birth = out

g1 = ggplot(out, aes(x = odds_ratio_glm, y = feature))+
  geom_vline(xintercept = 1, lty = 2, col = "grey80")+
  
  geom_point(shape = 18, size = 4)+
  geom_text(aes(label = pval2), nudge_y = 0.5, size = 3)+ #Fixing the pval labels
  
  #geom_text(aes(label = paste("p = ", scales::scientific(pval,3), sep ="")), nudge_y = 0.5, size = 3)+
  geom_errorbarh(aes(xmin = confint_2.5., xmax = confint_97.5.), height = 0.1)+
  ggpubr::theme_classic2()+
  xlab("Odds ratio with 95%-confidence interval")+
  ggtitle("Achieved pregnancy")+
  expand_limits(y = 6)

g1 

### Now the same for live birth ####
#HBA1C
d = left_join(Samples, bio)%>%
  dplyr::filter(Combined %in% c("Pregnant but no live birth", "Pregnant and live birth"))

table(d$Live_birth)


gg = glm(
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + hba1c",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
hba1c = data.frame(feature = "hba1c", 
                   pval = coef(summary(gg))[,4][5],
                   odds_ratio_glm = odds_ratio,
                   'confint_2,5%' = exp(confint(gg)[[5]]),
                   'confint_97,5%' = exp(confint(gg)[[10]]))

#glucose
gg = glm(
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + glucose",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
glucose = data.frame(feature = "glucose", 
                     pval = coef(summary(gg))[,4][5],
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[5]]),
                     'confint_97,5%' = exp(confint(gg)[[10]]))

#total_kolesterol
gg = glm( 
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + total_kolesterol",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
total_kolesterol = data.frame(feature = "Total kolesterol", 
                              pval = coef(summary(gg))[,4][5],
                              odds_ratio_glm = odds_ratio,
                              'confint_2,5%' = exp(confint(gg)[[5]]),
                              'confint_97,5%' = exp(confint(gg)[[10]]))


#ldl
gg = glm(
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + ldl",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
ldl = data.frame(feature = "ldl", 
                 pval = coef(summary(gg))[,4][5],
                 odds_ratio_glm = odds_ratio,
                 'confint_2,5%' = exp(confint(gg)[[5]]),
                 'confint_97,5%' = exp(confint(gg)[[10]]))


#hdl
gg = glm(
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + hdl",
  family = binomial(link = "logit") #Logistic
)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
hdl = data.frame(feature = "hdl", 
                 pval = coef(summary(gg))[,4][5],
                 odds_ratio_glm = odds_ratio,
                 'confint_2,5%' = exp(confint(gg)[[5]]),
                 'confint_97,5%' = exp(confint(gg)[[10]]))

#triglycerider
gg = glm(
  data = d,
  formula = "Live_birth ~ age + BMI + smoking + triglycerider",
  family = binomial(link = "logit") #Logistic
)

summary(gg)

odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
triglycerider = data.frame(feature = "triglycerider",
                           pval = coef(summary(gg))[,4][5],
                           odds_ratio_glm = odds_ratio,
                           'confint_2,5%' = exp(confint(gg)[[5]]),
                           'confint_97,5%' = exp(confint(gg)[[10]]))

###Assemble output
out = bind_rows(hba1c, 
                #glucose,
                total_kolesterol, ldl, hdl, triglycerider)
# out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "glucose", "Total kolesterol", "ldl", "hdl", "triglycerider")), 
#                      labels = rev(c("Age", "BMI","HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL" ,"Triglycerides")))

out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "Total kolesterol", "ldl", "hdl", "triglycerider")), 
                     labels = rev(c("Age", "BMI","HbA1c", "Total Cholesterol", "LDL Cholesterol", "HDL Cholesterol" ,"Triglycerides")))


out = out%>%
  mutate(
    pval2 = "NS",
    pval2 = ifelse(pval < 0.05, "p \u2264 0.05", pval2),
    pval2 = ifelse(pval < 0.01, "p \u2264 0.01", pval2),
    pval2 = ifelse(pval < 0.001, "p \u2264 0.001", pval2),
    Test = "Achieved Pregnancy"
  )
achieved_preg = out

g2 = ggplot(out, aes(x = odds_ratio_glm, y = feature))+
  geom_vline(xintercept = 1, lty = 2, col = "grey80")+
  
  geom_point(shape = 18, size = 4)+
  geom_text(aes(label = pval2), nudge_y = 0.5, size = 3)+ #Fixing the pval labels
  
#  geom_text(aes(label = paste("p = ", scales::scientific(pval,3), sep ="")), nudge_y = 0.5, size = 3)+
  geom_errorbarh(aes(xmin = confint_2.5., xmax = confint_97.5.), height = 0.1)+
  ggpubr::theme_classic2()+
  xlab("Odds ratio with 95%-confidence interval")+
  ggtitle("Live birth, in case of pregnancy")+
  expand_limits(y = 6)

g2

library(patchwork)
g = g1 / g2 & theme(
  plot.title = element_text(size = 9),
  axis.title.y = element_blank(),
  axis.title.x = element_text(size = 9),
  axis.text = element_text(size = 9)
)

out = bind_rows(achieved_preg, live_birth)

g
quartz(type = 'pdf', file = "Results/Figures/PRIMARY_Rate_of_pregnancy_and_live_birth.pdf", width = 5, height = 6)
g 
dev.off()
out
openxlsx::write.xlsx(out, "Results/Tables/PRIMARY_Achieved_preg_and_live_births.xlsx")
