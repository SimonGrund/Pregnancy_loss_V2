library(tidyverse)
library(data.table)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

d = readxl::read_excel("Data/AbHab 29-02-2024.xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
d = filter(d, group == "Cases")

d = rename(d, Glucose = glucose_middel, 
           HbA1c = hba1c,
           'Total_cholesterol' = total_kolesterol, 
           LDL = ldl, 
           HDL = hdl, 
           Triglycerides = triglycerid)

biomarkers = c(
  "Glucose",
  "HbA1c",
  "Total_cholesterol",
  "LDL",
  "HDL",
  "Triglycerides"
)


confounders = c(
  "age",
  "BMI",
  "smoking"
)

y = c("N_losses_before_ref")

### 
library(patchwork)
#par(mfrow=c(3,3))
for(biom in biomarkers){
  ll = lm(
    data = d,
    formula = paste0("N_losses_before_ref ~ age + BMI + smoking + ", biom)
  )
  ss = summary(ll)
  pval = ss$coefficients[20]
  #cat(biom, ", p<", ss$coefficients[20], "\n" , sep = "")
  sig = ""; if(pval < 0.05){sig = " *"}; if(pval < 0.01){sig = ", (**)"}; if(pval < 0.001){sig = ", (***)"}
  sig2 = "NS"; if(pval < 0.05){sig2 = "p < 0.05"}; if(pval < 0.01){sig2 = "p < 0.01"}; if(pval < 0.001){sig2 = "p < 0.001"}
  
  gtmp = ggplot(d, aes_string(x = y, y = biom))+
    #geom_boxplot(aes(x = as.factor(N_losses_before_ref)))+
    geom_point(col = "grey")+
    geom_smooth(method = "lm", lty = 2, col = "blue")+
    xlab("No. losses before referral")+
    #ggtitle(biom, subtitle = paste0("p<", scales::scientific(pval, digits = 3), sig2))+
    ggtitle(biom, subtitle = paste0(sig2))+
    tt
  
  assign(paste0("plot", biom), gtmp)
  
  ##Make table
  tmp = data.frame(biomarkers = biom, coef = ss$coefficients[5,1], pval = pval)
  if(biom == biomarkers[1]){
    out_tab = tmp
  }else{
    out_tab = rbind(out_tab, tmp)
  }
}

plotGlucose + plotHbA1c + plotHDL + 
  plotLDL + plotTotal_cholesterol + plotTriglycerides +
  plot_layout(ncol=3)

#ggsave("Results/Figures/Losses_before_reference.pdf", device = "pdf", height = 4, width = 6)
#openxlsx::write.xlsx(out_tab, "Results/Tables/Biomarkers_function_of_loss_COMÅLETE_COHORT.xlsx")
