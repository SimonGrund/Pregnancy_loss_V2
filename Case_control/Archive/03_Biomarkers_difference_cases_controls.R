#Summary statistics
library(tidyverse)
library(data.table)
source("Code/Plot_themes.R")

run_all = function(case_group = c(1:3)){
  
  #Load data
  d = fread("Data/matched_data_290124.tsv")
  d$group = factor(d$group, levels = c("Cases", "Control"), labels = rev(c(": Cases", ": Control")))
  table(d$group)
  d = dplyr::filter(d, patient_type %in% c(0,case_group)) #Now, only subgroup 0 (control) and patients (1)
  
  ### Biomarkers
  d = d%>%
    dplyr::select(ID = record_id, group, age, BMI, smoking, hba1c, glucose = glucose_middel, total_kolesterol, ldl, hdl, triglycerider = triglycerid, vldl)
  
  
  #glucose
  gg = glm(
    data = d,
    formula = "group ~ age + BMI + smoking + glucose",
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
    formula = "group ~ age + BMI + smoking + total_kolesterol",
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
    formula = "group ~ age + BMI + smoking + ldl",
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
    formula = "group ~ age + BMI + smoking + hdl",
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
    formula = "group ~ age + BMI + smoking + triglycerider",
    family = binomial(link = "logit") #Logistic
  )
  
  odds_ratio = exp(coef(gg))[5] #Exponential of the coefficient of being elevated 
  triglycerider = data.frame(feature = "triglycerider",
                             pval = coef(summary(gg))[,4][5],
                             odds_ratio_glm = odds_ratio,
                             'confint_2,5%' = exp(confint(gg)[[5]]),
                             'confint_97,5%' = exp(confint(gg)[[10]]))
  
  
  ###Assemble output
  out = bind_rows(hba1c, glucose, total_kolesterol, ldl, hdl, triglycerider)
  out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "glucose", "Total kolesterol", "ldl", "hdl", "triglycerider")), 
                       labels = rev(c("Age", "BMI","HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL","Triglycerides")))
  
  
  out = out%>%
    mutate(
      pval2 = "NS",
      pval2 = ifelse(pval < 0.05, "p \u2264 0.05", pval2),
      pval2 = ifelse(pval < 0.01, "p \u2264 0.01", pval2),
      pval2 = ifelse(pval < 0.001, "p \u2264 0.001", pval2)
    )
  
  g1 = ggplot(out, aes(x = odds_ratio_glm, y = feature))+
    geom_point(shape = 18, size = 4)+
    geom_text(aes(label = pval2), nudge_y = 0.5, size = 3)+ #Fixing the pval labels
    
    #geom_text(aes(label = paste("p = ", scales::scientific(pval,3), sep ="")), nudge_y = 0.5, size = 3)+
    geom_errorbarh(aes(xmin = confint_2.5., xmax = confint_97.5.), height = 0.1)+
    ggpubr::theme_classic2()+
    xlab("Odds ratio of having recurrent pregnancy loss, with 95%-confidence interval")+
    ggtitle("Association with reccurent pregnancy loss")+
    geom_vline(xintercept = 1, lty = 2, col = "grey20")
  
  return(g1)
  
}

g = run_all()
plot(g)
