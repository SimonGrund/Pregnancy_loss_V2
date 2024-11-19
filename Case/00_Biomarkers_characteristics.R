library(tidyverse)
library(data.table)
library(TOSTER)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

d = readxl::read_excel("Data/AbHab2 19-11-2024.xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
d = filter(d, group == "Cases")
d = rename(d, glucose = glucose_middel)
table(d$patient_type)

d$patient_type = factor(d$patient_type, levels = c(1,2), 
                        labels = c(
                          "Primary \nRPL",
                          "Secondary \nRPL"))

primary = filter(d, patient_type == "Primary \nRPL")
secondary = filter(d, patient_type == "Secondary \nRPL")

#Make empty dataframe
vars = c(
  "HbA1c, mean (SD)
(mmol/mol)",
  
  "Estimated average glucose, mean (SD)
(mmol/l)",
  "Total cholesterol, mean (SD)
(mmol/l)",
  "LDL, mean (SD)
(mmol/l)",
  
  "HDL, mean (SD)
(mmol/l)",
  
  "Triglycerides, mean (SD) (mmol/l)"
)

t2 = data.frame(
  Biomarker = vars,
  Complete_cohort  = NA,
  Primary_RPL = NA,
  Secondary_RPL = NA,
  p_val = NA,
  Conclusion = NA
)

t2$Complete_cohort[1] = paste(round(mean(d$hba1c, na.rm =T),1), " (", round(sd(d$hba1c, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[1] = paste(round(mean(primary$hba1c, na.rm =T),1), " (", round(sd(primary$hba1c, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[1] = paste(round(mean(secondary$hba1c, na.rm =T),1), " (", round(sd(secondary$hba1c, na.rm=T), 1), ")", sep = "")

hba1c = ggplot(d, aes(x = hba1c, fill = patient_type))+
  geom_histogram(col = "black", size = 0.2, bins = 20)+
  tt+
  ylab("No. of patients")+
  xlab("HbA1c")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
hba1c

#Test for equivalence
eqb = sd(primary$hba1c, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = hba1c ~ patient_type,
                     data = d,
                     eqb = c(-eqb, eqb),
               eqbound_type = "raw",
                     alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[1] = p
t2$Conclusion[1] = res1a$decision$combined


#### Glucose
t2$Complete_cohort[2] = paste(round(mean(d$glucose, na.rm =T),1), " (", round(sd(d$glucose, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[2] = paste(round(mean(primary$glucose, na.rm =T),1), " (", round(sd(primary$glucose, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[2] = paste(round(mean(secondary$glucose, na.rm =T),1), " (", round(sd(secondary$glucose, na.rm=T), 1), ")", sep = "")

glucose = ggplot(d, aes(x = glucose, fill = patient_type))+
  geom_histogram( col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("Glucose")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
glucose

#Test for equivalence
eqb = sd(primary$glucose, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = glucose ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[2] = p
t2$Conclusion[2] = res1a$decision$combined

#Kolesterol
t2$Complete_cohort[3] = paste(round(mean(d$total_kolesterol, na.rm =T),1), " (", round(sd(d$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[3] = paste(round(mean(primary$total_kolesterol, na.rm =T),1), " (", round(sd(primary$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[3] = paste(round(mean(secondary$total_kolesterol, na.rm =T),1), " (", round(sd(secondary$total_kolesterol, na.rm=T), 1), ")", sep = "")

total_kolesterol = ggplot(d, aes(x = total_kolesterol, fill = patient_type))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("Total cholesterol")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
total_kolesterol

#Test for equivalence
eqb = sd(primary$total_kolesterol, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = total_kolesterol ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[3] = p
t2$Conclusion[3] = res1a$decision$combined

#LDL
t2$Complete_cohort[4] = paste(round(mean(d$ldl, na.rm =T),1), " (", round(sd(d$ldl, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[4] = paste(round(mean(primary$ldl, na.rm =T),1), " (", round(sd(primary$ldl, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[4] = paste(round(mean(secondary$ldl, na.rm =T),1), " (", round(sd(secondary$ldl, na.rm=T), 1), ")", sep = "")

ldl = ggplot(d, aes(x = ldl, fill = patient_type))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("LDL")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
ldl

#Test for equivalence
eqb = sd(primary$ldl, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = ldl ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[4] = p
t2$Conclusion[4] = res1a$decision$combined

#HDL
t2$Complete_cohort[5] = paste(round(mean(d$hdl, na.rm =T),1), " (", round(sd(d$hdl, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[5] = paste(round(mean(primary$hdl, na.rm =T),1), " (", round(sd(primary$hdl, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[5] = paste(round(mean(secondary$hdl, na.rm =T),1), " (", round(sd(secondary$hdl, na.rm=T), 1), ")", sep = "")

hdl = ggplot(d, aes(x = hdl, fill = patient_type))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("HDL")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
hdl

#Test for equivalence
eqb = sd(primary$hdl, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = hdl ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[5] = p
t2$Conclusion[5] = res1a$decision$combined

#triglycerid
t2$Complete_cohort[6] = paste(round(mean(d$triglycerid, na.rm =T),1), " (", round(sd(d$triglycerid, na.rm=T), 1), ")", sep = "")
t2$Primary_RPL[6] = paste(round(mean(primary$triglycerid, na.rm =T),1), " (", round(sd(primary$triglycerid, na.rm=T), 1), ")", sep = "")
t2$Secondary_RPL[6] = paste(round(mean(secondary$triglycerid, na.rm =T),1), " (", round(sd(secondary$triglycerid, na.rm=T), 1), ")", sep = "")

triglycerid = ggplot(d, aes(x = triglycerid, fill = patient_type))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("Triglycerides")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("dodgerblue3","goldenrod" ))
triglycerid

#Test for equivalence
eqb = sd(primary$triglycerid, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = triglycerid ~ patient_type,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[6] = p
t2$Conclusion[6] = res1a$decision$combined

colnames(t2) = c("Biomarker", "All RPL samples", "Primary RPL", "Secondary RPL", "TOST p value", "Test conclusion")

openxlsx::write.xlsx(t2, "Results/Tables/Table2.xlsx")


# Patchwork
library(patchwork)
hba1c + 
#  glucose + 
  total_kolesterol + 
  ldl + 
  hdl + 
  triglycerid + 
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom", 
                                          legend.justification = "left",
                                          legend.direction = "vertical") 
ggsave(filename = "Results/Figures/Biomarker_characteristics.pdf", device = cairo_pdf,
       width = 5, height = 6)