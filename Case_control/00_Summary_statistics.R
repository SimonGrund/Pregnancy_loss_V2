library(tidyverse)
library(data.table)
library(patchwork)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

d = readxl::read_excel("Data/AbHab 29-02-2024.xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
matched = fread("Data/matched_data.tsv")

d$patient_type = factor(d$patient_type, levels = c(1,2), 
                        labels = c(
                          "Primary \nRPL",
                          "Secondary \nRPL"))

d$smoking[is.na(d$smoking)] = 5
d$smoking = factor(d$smoking, levels = c(1,3,2,5), labels =   c("Current",
                                                                "Former",
                                                                "Never",
                                                                "Unknown"
))

matched$smoking[is.na(matched$smoking)] = 5
matched$smoking = factor(matched$smoking, levels = c(1,3,2,5), labels =   c("Current",
                                                                "Former",
                                                                "Never",
                                                                "Unknown"
))

cases = filter(d, group == "Cases")
controls = filter(d, group == "Control")

matched_cases = filter(matched, group == "Cases")
matched_controls = filter(matched, group == "Control")

#Prepare table
baseline = c(
  "Age at referral, median [IQR] (years)",
  "BMI, median [IQR] (range)",
  "Number of smokers. (Current / Former / Never / Unknown)"
)


baseline = 
  data.frame(
    Variable = baseline,
    Cases_before_matching = NA,
    Controls_before_matching = NA,
    Cases_after_matching = NA,
    Controls_after_matching = NA
  )

## Age
ageplot1 = ggplot(d, aes(x = age, fill = group))+
  # geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2, position = "stack")+
  tt+
  #  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of patients")+
  xlab("Age")+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  theme(
    legend.title = element_blank()
  )+
  ggtitle("Age Before Propensity Matching")
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
ageplot1

baseline$Cases_before_matching[1] = paste(
  round(median(cases$age, na.rm = T), 1), " [", round(quantile(cases$age, 0.25, na.rm = T),1), ":",round(quantile(cases$age, 0.75, na.rm = T),1), "] (n=", sum(!is.na(cases$age)), ")"
  , sep = "") 
baseline$Controls_before_matching[1]= paste(
  round(median(controls$age, na.rm = T), 1), " [", round(quantile(controls$age, 0.25, na.rm = T),1), ":",round(quantile(controls$age, 0.75, na.rm = T),1), "] (n=", sum(!is.na(controls$age)), ")"
  , sep = "") 

##### #### 

ageplot2 = ggplot(matched, aes(x = age, fill = group))+
  # geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2, position = "stack")+
  tt+
  #  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of patients")+
  xlab("Age")+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  theme(
    legend.title = element_blank()
  )+
  ggtitle("Age After Propensity Matching")
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
ageplot2
ageplot1 + ageplot2 + 
  plot_layout(guides = "collect")

baseline$Cases_after_matching[1] = paste(
  round(median(matched_cases$age, na.rm = T), 1), " [", round(quantile(matched_cases$age, 0.25, na.rm = T),1), ":",round(quantile(matched_cases$age, 0.75, na.rm = T),1), "] (n=", sum(!is.na(matched_cases$age)), ")"
  , sep = "") 
baseline$Controls_after_matching[1] = paste(
  round(median(matched_controls$age, na.rm = T), 1), " [", round(quantile(matched_controls$age, 0.25, na.rm = T),1), ":",round(quantile(matched_controls$age, 0.75, na.rm = T),1), "] (n=", sum(!is.na(matched_controls$age)), ")"
  , sep = "") 

# BMI
bmiplot1 = ggplot(d, aes(x = BMI, fill = group))+
  # geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2, position = "stack")+
  tt+
  #  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of patients")+
  xlab("Age")+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  theme(
    legend.title = element_blank()
  )+
  ggtitle("BMI Before Propensity Matching")
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
bmiplot1

baseline$Cases_before_matching[2] = paste(
  round(median(cases$BMI, na.rm = T), 1), " [", round(quantile(cases$BMI, 0.25, na.rm = T),1), ":",round(quantile(cases$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(cases$BMI)), ")"
  , sep = "") 
baseline$Controls_before_matching[2]= paste(
  round(median(controls$BMI, na.rm = T), 1), " [", round(quantile(controls$BMI, 0.25, na.rm = T),1), ":",round(quantile(controls$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(controls$BMI)), ")"
  , sep = "") 

bmiplot2 = ggplot(matched, aes(x = BMI, fill = group))+
  # geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2, position = "stack")+
  tt+
  #  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of patients")+
  xlab("Age")+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  theme(
    legend.title = element_blank()
  )+
  ggtitle("BMI After Propensity Matching")
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
bmiplot2
bmiplot1 + bmiplot2 + 
  plot_layout(guides = "collect")

baseline$Cases_after_matching[2] = paste(
  round(median(matched_cases$BMI, na.rm = T), 1), " [", round(quantile(matched_cases$BMI, 0.25, na.rm = T),1), ":",round(quantile(matched_cases$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(matched_cases$BMI)), ")"
  , sep = "") 
baseline$Controls_after_matching[2] = paste(
  round(median(matched_controls$BMI, na.rm = T), 1), " [", round(quantile(matched_controls$BMI, 0.25, na.rm = T),1), ":",round(quantile(matched_controls$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(matched_controls$BMI)), ")"
  , sep = "") 

#Smoking
d2 = d%>%
  count(smoking, group) %>%       
  group_by(group) %>%
  mutate(pct= prop.table(n)) 

#dev.off() #Closes other graph windows
smokeplot1 = ggplot(d2, aes(x = smoking, y = pct, fill = group))+
  # geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2, width = 0.6, fill = "dodgerblue3")+
  geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2)+
  tt+
  xlab("Smoking status")+
  ylab("Pct. of patients")+
  scale_y_continuous(labels = scales::percent)+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  ggtitle("Smoking Status Before Propensity Matching")

smokeplot1

baseline$Cases_before_matching[3] = paste(
  sum(cases$smoking == "Current", na.rm = T), " / ", 
  sum(cases$smoking == "Former", na.rm = T), " / ", 
  sum(cases$smoking == "Never", na.rm = T), " / ", 
  sum(cases$smoking == "Unknown", na.rm = T)
  , sep = "") 
baseline$Controls_before_matching[3] = paste(
  sum(controls$smoking == "Current", na.rm = T), " / ", 
  sum(controls$smoking == "Former", na.rm = T), " / ", 
  sum(controls$smoking == "Never", na.rm = T), " / ", 
  sum(controls$smoking == "Unknown", na.rm = T)
  , sep = "") 


matched2 = matched%>%
  count(smoking, group) %>%       
  group_by(group) %>%
  mutate(pct= prop.table(n)) 

#dev.off() #Closes other graph windows
smokeplot2 = ggplot(matched2, aes(x = smoking, y = pct, fill = group))+
  # geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2, width = 0.6, fill = "dodgerblue3")+
  geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2)+
  tt+
  xlab("Smoking status")+
  ylab("Pct. of patients")+
  scale_y_continuous(labels = scales::percent)+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))+
  ggtitle("Smoking Status After Propensity Matching")

smokeplot2

baseline$Cases_after_matching[3] = paste(
  sum(matched_cases$smoking == "Current", na.rm = T), " / ", 
  sum(matched_cases$smoking == "Former", na.rm = T), " / ", 
  sum(matched_cases$smoking == "Never", na.rm = T), " / ", 
  sum(matched_cases$smoking == "Unknown", na.rm = T)
  , sep = "") 
baseline$Controls_after_matching[3] = paste(
  sum(matched_controls$smoking == "Current", na.rm = T), " / ", 
  sum(matched_controls$smoking == "Former", na.rm = T), " / ", 
  sum(matched_controls$smoking == "Never", na.rm = T), " / ", 
  sum(matched_controls$smoking == "Unknown", na.rm = T)
  , sep = "") 


smokeplot1 + smokeplot2 + 
  plot_layout(guides = "collect")

#Export table
openxlsx::write.xlsx(baseline, "Results/Tables/Table3.xlsx")

### Collect all plots
ageplot1 + ageplot2 + bmiplot1 + bmiplot2 + smokeplot1 + smokeplot2 +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.justification = "left",
        legend.direction = "vertical") 

ggsave( filename = "Results/Figures/Case_control_characteristics.pdf", device = "pdf",
        width = 6.3, height = 6)
