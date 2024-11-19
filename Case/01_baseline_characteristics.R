library(tidyverse)
library(data.table)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

d = readxl::read_excel("Data/AbHab2 19-11-2024.xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
d = filter(d, group == "Cases")

# Pregnancy losses
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

##### A figure of the data size
table(d$group)
primary = filter(d, patient_type == "Primary \nRPL")
secondary = filter(d, patient_type == "Secondary \nRPL")

##### To fill out table 1, we first we make the empty table#####
baseline = c(
  "Types of prior losses",
  "Number of prior losses, median [IQR], (range)",
  "Age at referral, median [IQR] (years)",
  "BMI, median [IQR] (range)",
  "Number of smokers. (Current / Former / Never / Unknown)"
)


baseline = 
  data.frame(
    Variable = baseline,
    Complete_cohort = rep("", 5),
    Primary_RPL = rep("", 5),
    Secondary_RPL = rep("", 5),
    p_value = NA,
    Conclusion = NA
  )

#Now, fill it out
#  "Types of prior losses (No loss / Primary \nRPL / Secondary \nRPL)"
baseline$Complete_cohort[1] = paste(nrow(d), sep = "") 

baseline$Primary_RPL[1] = paste(
  sum(primary$patient_type == "Primary \nRPL", na.rm = T)
  , sep = "") 

baseline$Secondary_RPL[1] = paste(
  sum(d$patient_type == "Secondary \nRPL", na.rm = T)
  , sep = "") 

TypesOfLosses = ggplot(d, aes(x = patient_type, fill = patient_type))+
  geom_histogram(stat = "count", col = "black", size = 0.2, width = 0.6)+
  tt+
  xlab("")+
  ylab("No. of patients")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))

TypesOfLosses

#### #"Number of prior losses, median [IQR], (range)"# #####
baseline$Complete_cohort[2] = paste(
  median(d$N_losses_before_ref, na.rm = T), " [",round(quantile(d$N_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(d$N_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(d$N_losses_before_ref))
  , sep = "") 

baseline$Primary_RPL[2] = paste(
  median(primary$N_losses_before_ref, na.rm = T), " [",round(quantile(primary$N_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(primary$N_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(primary$N_losses_before_ref))
  , sep = "") 

baseline$Secondary_RPL[2] = paste(
  median(secondary$N_losses_before_ref, na.rm = T), " [",round(quantile(d$N_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(d$N_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(secondary$N_losses_before_ref))
  , sep = "") 

#dev.off()
priorlossplot = 
  ggplot(d, aes(x = N_losses_before_ref, fill = patient_type))+
  geom_histogram(size = 0.2, col = "black", 
                 binwidth = 1)+
  tt+
  xlab("No. of prior losses")+
  ylab("No. of patients")+
  #facet_grid(rows = vars(group), scales = "free_x")+
  scale_x_continuous(n.breaks = 20)+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
priorlossplot

#Test for equivalence
eqb = sd(primary$N_losses_before_ref, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = TOSTER::wilcox_TOST(
               formula = hba1c ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
res1a

p = max(res1a$TOST$p.value[2], res1a$TOST$p.value[3])
baseline$p_value[2] = p
baseline$Conclusion[2] = res1a$decision$combined


####### "Age at referral, median [IQR] (years)"] #####
baseline$Complete_cohort[3] = paste(
  round(median(d$age), 1), " [", round(quantile(d$age, 0.25),1), ":",round(quantile(d$age, 0.75),1), "] (n=", nrow(d), ")"
  , sep = "") 


baseline$Primary_RPL[3] = paste(
  round(median(primary$age), 1), " [", round(quantile(primary$age, 0.25),1), ":",round(quantile(primary$age, 0.75),1), "] (n=", nrow(primary), ")"
  , sep = "") 

baseline$Secondary_RPL[3] = paste(
  round(median(secondary$age), 1), " [", round(quantile(secondary$age, 0.25),1), ":",round(quantile(secondary$age, 0.75),1), "] (n=", nrow(secondary), ")"
  , sep = "") 

ageplot = ggplot(d, aes(x = age, fill = patient_type))+
 # geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2, position = "stack")+
  tt+
#  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of Patients")+
  xlab("Age")+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))+
  theme(
    legend.title = element_blank()
  )
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
ageplot

#Test for equivalence
eqb = sd(primary$age, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = TOSTER::t_TOST(
  formula = age ~ patient_type,
  data = d,
  eqb = c(-eqb, eqb),
  eqbound_type = "raw",
  alternative = "e")
res1a

p = max(res1a$TOST$p.value[2], res1a$TOST$p.value[3])
baseline$p_value[3] = p
baseline$Conclusion[3] = res1a$decision$combined


######  "BMI, median [IQR] (range)" ##### 
baseline$Complete_cohort[4] = paste(
  round(median(d$BMI, na.rm = T), 1), " [", round(quantile(d$BMI, 0.25, na.rm = T),1), ":",round(quantile(d$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(d$BMI)), ")"
  , sep = "") 

baseline$Primary_RPL[4] = paste(
  round(median(primary$BMI, na.rm = T), 1), " [", round(quantile(primary$BMI, 0.25, na.rm = T),1), ":",round(quantile(primary$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(primary$BMI)), ")"
  , sep = "") 
baseline$Secondary_RPL[4] = paste(
  round(median(secondary$BMI, na.rm = T), 1), " [", round(quantile(secondary$BMI, 0.25, na.rm = T),1), ":",round(quantile(secondary$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(secondary$BMI)), ")"
  , sep = "") 

#dev.off()
bmiplot = ggplot(d, aes(x = BMI, fill = patient_type))+
  geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  geom_histogram(col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  #  facet_grid(rows = vars(group))+
  tt+
  ylab("No. of patients")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
  
# geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 2)+
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0)
bmiplot

#Test for equivalence
eqb = sd(primary$BMI, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = TOSTER::wilcox_TOST(
  formula = BMI ~ patient_type,
  data = d,
  eqb = c(-eqb, eqb),
  eqbound_type = "raw",
  alternative = "e")
res1a

p = max(res1a$TOST$p.value[2], res1a$TOST$p.value[3])
baseline$p_value[4] = p
baseline$Conclusion[4] = res1a$decision$combined

#### Smoking habits
#Summarize per group and smoking habit to get the percentage

d2 = d%>%
  count(smoking, patient_type) %>%       
  group_by(patient_type) %>%
  mutate(pct= prop.table(n)) 

#dev.off() #Closes other graph windows
smokeplot = ggplot(d2, aes(x = smoking, y = pct, fill = patient_type))+
 # geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2, width = 0.6, fill = "dodgerblue3")+
  geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2)+
  tt+
  xlab("Smoking status")+
  ylab("Pct. of patients")+
  scale_y_continuous(labels = scales::percent)+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
smokeplot

#Number of smokers (Current / prior / never / unknown)
table(d$smoking)
baseline$Complete_cohort[5] = paste(
  sum(d$smoking == "Current", na.rm = T), " / ", 
  sum(d$smoking == "Former", na.rm = T), " / ", 
  sum(d$smoking == "Never", na.rm = T), " / ", 
  sum(d$smoking == "Unknown", na.rm = T)
  , sep = "") 

baseline$Primary_RPL[5] = paste(
  sum(primary$smoking == "Current", na.rm = T), " / ", 
  sum(primary$smoking == "Former", na.rm = T), " / ", 
  sum(primary$smoking == "Never", na.rm = T), " / ", 
  sum(primary$smoking == "Unknown", na.rm = T)
  , sep = "") 

baseline$Secondary_RPL[5] = paste(
  sum(secondary$smoking == "Current", na.rm = T), " / ", 
  sum(secondary$smoking == "Former", na.rm = T), " / ", 
  sum(secondary$smoking == "Never", na.rm = T), " / ", 
  sum(secondary$smoking == "Unknown", na.rm = T)
  , sep = "") 

test = TOSTER::twoprop_test(
  p1 = sum(primary$smoking == "Current", na.rm = T)/nrow(primary),
  p2 = sum(secondary$smoking == "Current", na.rm = T)/nrow(secondary),
  n1 = nrow(primary),
  n2 = nrow(secondary),
  alternative = "equivalence",
  null = 0.1
)
p_current = test$p.value

test = TOSTER::twoprop_test(
  p1 = sum(primary$smoking == "Former", na.rm = T)/nrow(primary),
  p2 = sum(secondary$smoking == "Former", na.rm = T)/nrow(secondary),
  n1 = nrow(primary),
  n2 = nrow(secondary),
  alternative = "equivalence",
  null = 0.1
)
p_Former = test$p.value

test = TOSTER::twoprop_test(
  p1 = sum(primary$smoking == "Never", na.rm = T)/nrow(primary),
  p2 = sum(secondary$smoking == "Never", na.rm = T)/nrow(secondary),
  n1 = nrow(primary),
  n2 = nrow(secondary),
  alternative = "equivalence",
  null = 0.1
)
p_Never = test$p.value

test = TOSTER::twoprop_test(
  p1 = sum(primary$smoking == "Unknown", na.rm = T)/nrow(primary),
  p2 = sum(secondary$smoking == "Unknown", na.rm = T)/nrow(secondary),
  n1 = nrow(primary),
  n2 = nrow(secondary),
  alternative = "equivalence",
  null = 0.1
)
p_Unknown = test$p.value

baseline$p_value[5] = paste(
  scales::scientific(p_current, 2), "/",
  scales::scientific(p_Former, 2),"/",
  scales::scientific(p_Never, 2),"/",
  scales::scientific(p_Unknown, 2),
  sep = "") 

#Export the table
#write.table(x = baseline, file = "../Artikel/Tabeller og figurer/Output_from_simons_code/Table1.tsv", sep ="\n", col.names = T, row.names = F)
openxlsx::write.xlsx(baseline, "Results/Tables/Table1.xlsx")
# 
# 

library(patchwork)
TypesOfLosses + priorlossplot + ageplot + bmiplot + smokeplot  + guide_area() +
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", 
        legend.justification = "left",
        legend.direction = "vertical") 

ggsave( filename = "Results/Figures/Baseline_characteristics.pdf", device = "pdf",
              width = 5, height = 6)

