# Odds ratio med 95 % konfidensintervaller: sammenligning mellem kontroller og cases med forhøjede/abnorme metaboliske biomarkører (ja/nej) for hver biomarkører
# HbA1c > 39.0
# Glucose > 6.4
# Total cholesterol >5.0 mmol/l
# LDL > 3.0 mmol/l
# HDL < 1.2
# Triglycerider > 2.0

#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
#source("Plot_themes.R") #load theme for plots

#Load data
d = fread("Data/matched_data_290124.tsv")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))

d = dplyr::select(d,
                  age, BMI, smoking,
                  record_id, patient_type, group, 'hba1c', 'glucose_middel', 'total_kolesterol',
                  'ldl', 'hdl', 'vldl', 'triglycerid')%>%
  mutate(
    'HbA1c>44' = ifelse(hba1c >= 44,"Pathological", "Normal"),
    'Glucose>6,4'= ifelse(glucose >= 6.4, "Pathological", "Normal"), 
    'Total cholesterol>5' = ifelse(total_kolesterol >= 5, "Pathological", "Normal"),
    'LDL>3' = ifelse(ldl >= 3, "Pathological", "Normal"), 
    'HDL<1,2' = ifelse(hdl < 1.2, "Pathological", "Normal"),  #Change to pathological 
    'Triglycerides>2' = ifelse(triglycerider >= 2, "Pathological", "Normal")
  )


d$hba1c = NULL


d$`Glucose>6,4` = factor(d$`Glucose>6,4`, levels = (c("Pathological", "Normal")))
d$`Total cholesterol>5` = factor(d$`Total cholesterol>5`, levels = (c("Pathological", "Normal")))
d$`LDL>3` = factor(d$`LDL>3`, levels = (c("Pathological", "Normal")))
d$`HDL<1,2` = factor(d$`HDL<1,2`, levels = (c("Pathological", "Normal")))
d$`Triglycerides>2` = factor(d$`Triglycerides>2`, levels = (c("Pathological", "Normal")))



# Odds ratio med 95 % konfidensintervaller: sammenligning mellem kontroller og cases med 
# forhøjede/abnorme metaboliske biomarkører (ja/nej) for hver biomarkører

# #HBA1C
# gg = glm('group ~ BMI + `HbA1c>39`', family = binomial, data = d)
# summary(gg)
# 
# ss = summary(gg)
# odds_ratio = exp(coef(gg))[3] #Exponential of the coefficient of being Pathological 
# odds_ratio
# 
# out_tmp = data.frame(feature = "HbA1c>39", 
#                      odds_ratio_glm = odds_ratio,
#                      'confint_2,5%' = exp(confint(gg)[[3]]),
#                      'confint_97,5%' = exp(confint(gg)[[6]]),
#                      p_val = ss$coefficients[12])
# 
# summary = d%>%
#   group_by(group)%>%
#   na.omit(`HbA1c>39`)%>%
#   mutate(n = n(),
#          n_Pathological = sum(`HbA1c>39` == "Pathological"),
#          rate = n_Pathological / n)%>%
#   distinct(group, n, n_Pathological, rate)%>%
#   pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))
# 
# out_tmp = bind_cols(out_tmp, summary)
# out = out_tmp

#Glucose>6,4
gg = glm('group ~ BMI + `Glucose>6,4`', family = binomial, data = d)
summary(gg)

ss = summary(gg)
odds_ratio = exp(coef(gg))[3] #Exponential of the coefficient of being Pathological 
odds_ratio

out_tmp = data.frame(feature = "Glucose>6,4", 
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[3]]),
                     'confint_97,5%' = exp(confint(gg)[[6]]),
                     p_val = ss$coefficients[12])

summary = d%>%
  group_by(group)%>%
  na.omit(`Glucose>6,4`)%>%
  mutate(n = n(),
         n_Pathological = sum(`Glucose>6,4` == "Pathological"),
         rate = n_Pathological / n)%>%
  distinct(group, n, n_Pathological, rate)%>%
  pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))

out_tmp = bind_cols(out_tmp, summary)

out = out_tmp
out
#Total cholesterol>5
gg = glm('group ~ BMI + age + `Total cholesterol>5`', family = binomial, data = d)
summary(gg)

ss = summary(gg)
odds_ratio = exp(coef(gg))[4] #Exponential of the coefficient of being Pathological 
odds_ratio

out_tmp = data.frame(feature = "Total cholesterol>5", 
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[4]]),
                     'confint_97,5%' = exp(confint(gg)[[8]]),
                     p_val = ss$coefficients[16])

summary = d%>%
  group_by(group)%>%
  na.omit(`Total cholesterol>5`)%>%
  mutate(n = n(),
         n_Pathological = sum(`Total cholesterol>5` == "Pathological"),
         rate = n_Pathological / n)%>%
  distinct(group, n, n_Pathological, rate)%>%
  pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))

out_tmp = bind_cols(out_tmp, summary)

out = bind_rows(out, out_tmp)

#LDL>3
gg = glm('group ~ BMI + `LDL>3`', family = binomial, data = d)
summary(gg)

ss = summary(gg)
odds_ratio = exp(coef(gg))[3] #Exponential of the coefficient of being Pathological 
odds_ratio

out_tmp = data.frame(feature = "LDL>3", 
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[3]]),
                     'confint_97,5%' = exp(confint(gg)[[6]]),
                     p_val = ss$coefficients[12])

summary = d%>%
  group_by(group)%>%
  na.omit(`LDL>3`)%>%
  mutate(n = n(),
         n_Pathological = sum(`LDL>3` == "Pathological"),
         rate = n_Pathological / n)%>%
  distinct(group, n, n_Pathological, rate)%>%
  pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))

out_tmp = bind_cols(out_tmp, summary)

out = bind_rows(out, out_tmp)

#HDL<1,2
gg = glm('group ~ BMI + `HDL<1,2`', family = binomial, data = d)
summary(gg)

ss = summary(gg)
odds_ratio = exp(coef(gg))[3] #Exponential of the coefficient of being Pathological 
odds_ratio

out_tmp = data.frame(feature = "HDL<1,2", 
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[3]]),
                     'confint_97,5%' = exp(confint(gg)[[6]]),
                     p_val = ss$coefficients[12])

summary = d%>%
  group_by(group)%>%
  na.omit(`HDL<1,2`)%>%
  mutate(n = n(),
         n_Pathological = sum(`HDL<1,2` == "Pathological"),
         rate = n_Pathological / n)%>%
  distinct(group, n, n_Pathological, rate)%>%
  pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))

out_tmp = bind_cols(out_tmp, summary)

out = bind_rows(out, out_tmp)

#Triglycerides>2
gg = glm('group ~ BMI + `Triglycerides>2`', family = binomial, data = d)
summary(gg)

ss = summary(gg)
odds_ratio = exp(coef(gg))[3] #Exponential of the coefficient of being Pathological 
odds_ratio

out_tmp = data.frame(feature = "Triglycerides>2", 
                     odds_ratio_glm = odds_ratio,
                     'confint_2,5%' = exp(confint(gg)[[3]]),
                     'confint_97,5%' = exp(confint(gg)[[6]]),
                     p_val = ss$coefficients[12])

summary = d%>%
  group_by(group)%>%
  na.omit(`Triglycerides>2`)%>%
  mutate(n = n(),
         n_Pathological = sum(`Triglycerides>2` == "Pathological"),
         rate = n_Pathological / n)%>%
  distinct(group, n, n_Pathological, rate)%>%
  pivot_wider(names_from = c(group), values_from = c(n_Pathological, n, rate))

out_tmp = bind_cols(out_tmp, summary)

out = bind_rows(out, out_tmp)
out

#Make graph
out2 = out%>%
  pivot_longer(cols = c(rate_Control, rate_Cases))

out2$feature = factor(out2$feature, levels = c('HbA1c>39', 'Glucose>6,4', 'Total cholesterol>5',
                                   'LDL>3', 'HDL<1,2', 'Triglycerides>2'))

out2$name = factor(out2$name, levels =  c("rate_Control", "rate_Cases"), labels =  c("Control", "Cases"))
out2 = out2%>%
  mutate(
    id = paste(feature, "\n", "Odds ratio: ", round(odds_ratio_glm, 2), "\n",
               "95%-CI: [", round(confint_2.5., 2), ":", round(confint_97.5., 2), "]",
               "\n", "p \u2264 ", scales::scientific(p_val, digits = 2),
               sep = ""),
    id = ifelse(odds_ratio_glm == 0, paste(feature, "\n", "Odds ratio: NA", "\n",
                                              "95%-CI: NA",
                                           "\n", "p \u2264 ", 
                                           sep = ""), 
                id),
  )

out2 = arrange(out2, feature)
out2$id = factor(out2$id, levels = unique(out2$id))

ggplot(out2, aes(x = name, y = value))+
  geom_histogram(stat = "identity", width = 0.3, fill = "dodgerblue4", color = "black", linewidth = .4)+
  facet_wrap(~id)+
  tt+
  ylab("Rate")+
  xlab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  ggtitle("All cases")

ggsave("../Results/Figures/Figure 4/Adjusted_Odds_ratio_biomarkers_above_treshhold.pdf",
       width = 3.5, height = 3, device = cairo_pdf)


#Make figure with confint
dev.off()

pd = distinct(out, feature, odds_ratio_glm, confint_2.5.,confint_97.5., p_val)
pd$feature = factor(pd$feature, levels = rev(distinct(pd, feature)$feature))
#pd2 = pd[-1,] #Discard HbA1C cause zero cases

quartz(type = 'pdf', file = "../Results/Figures/Figure 4/cases_ALL.pdf", width = 4, height = 3)

ggplot(pd, aes(x = odds_ratio_glm, y = feature))+
  geom_point()+
  geom_errorbarh(aes(xmin = confint_2.5., xmax = confint_97.5.), height = 0.2, size = 0.2)+
  tt+
  theme(
    plot.margin = margin(6,12,6,6)
  )+
  geom_vline(xintercept = 1, lty = 2, size = 0.4, col = "grey60")+
  geom_text(aes(label = 
                  paste("p \u2264 ", scales::scientific(p_val, digits = 2))), 
            hjust = 0.3,
            size = 3, nudge_y = 0.3)+
  ylab("")+
  xlab("Odds-ratio")+
  ggtitle(label = "All cases, adjusted")
dev.off()  

#Maker table
out3 = out2%>%
  mutate(
    id = paste(round(odds_ratio_glm, 2),
               ", 95%-CI: [", round(confint_2.5., 2), ":", round(confint_97.5., 2), "]",
               sep = ""),
    id = ifelse(odds_ratio_glm == 0, paste("NA",
                                           ", 95%-CI: NA", 
                                           sep = ""), 
                id),
  )%>%
  distinct(feature, "odds ratio" = id, "p-value" = paste(scales::scientific(p_val, digits = 2)) )
out3 = out3%>%
  mutate(
    "Patient group" = "All cases",
    "Adjusted for baseline characteristica" = "Yes"
  )

write.table(out3, "../Results/Tables/TMP/orAll_ad.tsv", sep = "\t", col.names = T, row.names = F)

