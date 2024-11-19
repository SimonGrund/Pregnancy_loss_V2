library(tidyverse)
library(data.table)
library(TOSTER)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

d = fread("Data/matched_data.tsv")
table(d$group)
d = rename(d, glucose = glucose_middel)

Cases = filter(d, group == "Cases")
Controls = filter(d, group == "Control")

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
  Cases = NA,
  Controls = NA,
  p_val = NA,
  Conclusion = NA
)

t2$Complete_cohort[1] = paste(round(mean(d$hba1c, na.rm =T),1), " (", round(sd(d$hba1c, na.rm=T), 1), ")", sep = "")
t2$Cases[1] = paste(round(mean(Cases$hba1c, na.rm =T),1), " (", round(sd(Cases$hba1c, na.rm=T), 1), ")", sep = "")
t2$Controls[1] = paste(round(mean(Controls$hba1c, na.rm =T),1), " (", round(sd(Controls$hba1c, na.rm=T), 1), ")", sep = "")
t2

hba1c = ggplot(d, aes(x = hba1c, fill = group))+
  geom_histogram(col = "black", size = 0.2, bins = 15)+
  tt+
  ylab("No. of patients")+
  xlab("HbA1c")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
hba1c

#Test for equivalence
eqb = sd(Cases$hba1c, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = hba1c ~ group,
                     data = d,
                     eqb = c(-eqb, eqb),
               eqbound_type = "raw",
                     alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[1] = p
t2$Conclusion[1] = res1a$decision$combined


#### Glucose
t2$Complete_cohort[2] = paste(round(mean(d$glucose, na.rm =T),1), " (", round(sd(d$glucose, na.rm=T), 1), ")", sep = "")
t2$Cases[2] = paste(round(mean(Cases$glucose, na.rm =T),1), " (", round(sd(Cases$glucose, na.rm=T), 1), ")", sep = "")
t2$Controls[2] = paste(round(mean(Controls$glucose, na.rm =T),1), " (", round(sd(Controls$glucose, na.rm=T), 1), ")", sep = "")

glucose = ggplot(d, aes(x = glucose, fill = group))+
  geom_histogram( col = "black", size = 0.2, bins = 20)+
  tt+
  ylab("No. of patients")+
  xlab("Glucose")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
glucose

#Test for equivalence
eqb = sd(Cases$glucose, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = glucose ~ group,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[2] = p
t2$Conclusion[2] = res1a$decision$combined

#Kolesterol
t2$Complete_cohort[3] = paste(round(mean(d$total_kolesterol, na.rm =T),1), " (", round(sd(d$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$Cases[3] = paste(round(mean(Cases$total_kolesterol, na.rm =T),1), " (", round(sd(Cases$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$Controls[3] = paste(round(mean(Controls$total_kolesterol, na.rm =T),1), " (", round(sd(Controls$total_kolesterol, na.rm=T), 1), ")", sep = "")

total_kolesterol = ggplot(d, aes(x = total_kolesterol, fill = group))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("Total Cholesterol")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
total_kolesterol

# #Test for equivalence
eqb = sd(Cases$total_kolesterol, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = total_kolesterol ~ group,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[3] = p
t2$Conclusion[3] = res1a$decision$combined

#LDL
t2$Complete_cohort[4] = paste(round(mean(d$ldl, na.rm =T),1), " (", round(sd(d$ldl, na.rm=T), 1), ")", sep = "")
t2$Cases[4] = paste(round(mean(Cases$ldl, na.rm =T),1), " (", round(sd(Cases$ldl, na.rm=T), 1), ")", sep = "")
t2$Controls[4] = paste(round(mean(Controls$ldl, na.rm =T),1), " (", round(sd(Controls$ldl, na.rm=T), 1), ")", sep = "")

ldl = ggplot(d, aes(x = ldl, fill = group))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("LDL Cholesterol")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
ldl

#Test for equivalence
eqb = sd(Cases$ldl, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = ldl ~ group,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[4] = p
t2$Conclusion[4] = res1a$decision$combined

#HDL
t2$Complete_cohort[5] = paste(round(mean(d$hdl, na.rm =T),1), " (", round(sd(d$hdl, na.rm=T), 1), ")", sep = "")
t2$Cases[5] = paste(round(mean(Cases$hdl, na.rm =T),1), " (", round(sd(Cases$hdl, na.rm=T), 1), ")", sep = "")
t2$Controls[5] = paste(round(mean(Controls$hdl, na.rm =T),1), " (", round(sd(Controls$hdl, na.rm=T), 1), ")", sep = "")

hdl = ggplot(d, aes(x = hdl, fill = group))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("HDL Cholesterol")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
hdl

# #Test for equivalence
eqb = sd(Cases$hdl, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = hdl ~ group,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[5] = p
t2$Conclusion[5] = res1a$decision$combined

#triglycerid
t2$Complete_cohort[6] = paste(round(mean(d$triglycerid, na.rm =T),2), " (", round(sd(d$triglycerid, na.rm=T), 1), ")", sep = "")
t2$Cases[6] = paste(round(mean(Cases$triglycerid, na.rm =T),2), " (", round(sd(Cases$triglycerid, na.rm=T), 1), ")", sep = "")
t2$Controls[6] = paste(round(mean(Controls$triglycerid, na.rm =T),2), " (", round(sd(Controls$triglycerid, na.rm=T), 1), ")", sep = "")

triglycerid = ggplot(d, aes(x = triglycerid, fill = group))+
  geom_histogram(col = "black", size = 0.2)+
  tt+
  ylab("No. of patients")+
  xlab("Triglycerides")+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c("forestgreen","grey" ))
triglycerid

#Test for equivalence
eqb = sd(Cases$triglycerid, na.rm = T) #set the bounds of the equivalence to be 1 SD
res1a = t_TOST(formula = triglycerid ~ group,
               data = d,
               eqb = c(-eqb, eqb),
               eqbound_type = "raw",
               alternative = "e")
p = max(res1a$TOST[4]$p.value[2], res1a$TOST[4]$p.value[3])
t2$p_val[6] = p
t2$Conclusion[6] = res1a$decision$combined

colnames(t2) = c("Biomarker", "All RPL samples", "Cases", "Controls", "P-value", "Conclusion")
openxlsx::write.xlsx(t2, "Results/Tables/Table4.xlsx")


# Patchwork
library(patchwork)
hba1c +
  #glucose + 
  total_kolesterol + ldl + hdl + triglycerid + 
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom", 
                                          legend.justification = "left",
                                          legend.direction = "vertical") 

ggsave(filename = "Results/Figures/Biomarker_characteristics_after_matching.pdf", device = cairo_pdf,
       width = 5, height = 6)
