#Summary statistics
library(tidyverse)
library(data.table)
source("Code/Plot_themes.R")
library(broom)

  #Load data
  d = fread("Data/matched_data.tsv")
#  d$group = factor(d$group, levels = c("Cases", "Control"), labels = c(": Cases", ": Control"))
  d$group = factor(d$group, levels = rev(c("Cases", "Control")))
  
  table(d$group)
#  d = dplyr::filter(d, patient_type %in% c(0,case_group)) #Now, only subgroup 0 (control) and patients (1)
  
  ### Biomarkers
  d = d%>%
    dplyr::select(ID = record_id, group, age, BMI, smoking, hba1c, 
                  glucose = glucose_middel, total_kolesterol, ldl, hdl, 
                  triglycerider = triglycerid, vldl,
                  subclass)
  
  ggplot(d, aes(x = group, y = glucose))+
    geom_boxplot(width = 0.4)+
    geom_point(alpha = 0.1)+
    geom_line(aes( group = subclass), alpha = 0.3)

  #hba1c
  gg = glm(
    data = d,
    formula = "group ~ hba1c",
    family = binomial(link = "logit") #Logistic
  )
  
  #Test the coefficient using cluster robust standard error
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  #coefficients - Why are the estimates not centered!?
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  hba1c = data.frame(feature = "hba1c", 
                       pval = coefficients[8],
                       odds_ratio_glm = odds_ratio,
                       'confint_2,5%' = exp(confint(coefficients)[[2]]),
                       'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  
  #glucose
  gg = glm(
    data = d,
    formula = "group ~ glucose",
    family = binomial(link = "logit") #Logistic
  )
  
  #Test the coefficient using cluster robust standard error
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  glucose = data.frame(feature = "glucose", 
                       pval = coefficients[8],
                       odds_ratio_glm = odds_ratio,
                       'confint_2,5%' = exp(confint(coefficients)[[2]]),
                       'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  #total_kolesterol
  gg = glm(
    data = d,
    formula = "group ~ total_kolesterol",
    family = binomial(link = "logit") #Logistic
  )
  
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  odds_ratio = exp(coef(coefficients))[2] #Exponential of the coefficient of being elevated 
  total_kolesterol = data.frame(feature = "Total kolesterol", 
                                pval = coefficients[8],
                                odds_ratio_glm = odds_ratio,
                                'confint_2,5%' = exp(confint(coefficients)[[2]]),
                                'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  
  #ldl
  gg = glm(
    data = d,
    formula = "group ~ ldl",
    family = binomial(link = "logit") #Logistic
  )
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  ldl = data.frame(feature = "ldl", 
                   pval = coefficients[8],
                   odds_ratio_glm = odds_ratio,
                   'confint_2,5%' = exp(confint(coefficients)[[2]]),
                   'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  
  #hdl
  gg = glm(
    data = d,
    formula = "group ~ hdl",
    family = binomial(link = "logit") #Logistic
  )
  
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  hdl = data.frame(feature = "hdl", 
                   pval = coefficients[8],
                   odds_ratio_glm = odds_ratio,
                   'confint_2,5%' = exp(confint(coefficients)[[2]]),
                   'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  #triglycerider
  gg = glm(
    data = d,
    formula = "group ~ triglycerider",
    family = binomial(link = "logit") #Logistic
  )
  coefficients = coeftest(gg, vcov. = vcovCL, cluster = ~subclass)
  
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  triglycerider = data.frame(feature = "triglycerider",
                             pval = coefficients[8],
                             odds_ratio_glm = odds_ratio,
                             'confint_2,5%' = exp(confint(coefficients)[[2]]),
                             'confint_97,5%' = exp(confint(coefficients)[[4]]))
  
  
  # ###Assemble output
  # out = bind_rows(hba1c, glucose, total_kolesterol, ldl, hdl, triglycerider)
  # out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "glucose", "Total kolesterol", "ldl", "hdl", "triglycerider")), 
  #                      labels = rev(c("Age", "BMI","HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL","Triglycerides")))
  # 
  ###Assemble output
  out = bind_rows(hba1c, total_kolesterol, ldl, hdl, triglycerider)
  out$feature = factor(out$feature, levels = rev(c("Age", "BMI","hba1c", "Total kolesterol", "ldl", "hdl", "triglycerider")), 
                       labels = rev(c("Age", "BMI","HbA1c", "Total Cholesterol", "LDL Cholesterol", "HDL Cholesterol","Triglycerides")))
  
  
  out = out%>%
    mutate(
      pval2 = "NS",
      pval2 = ifelse(pval < 0.05, "p \u2264 0.05", pval2),
      pval2 = ifelse(pval < 0.01, "p \u2264 0.01", pval2),
      pval2 = ifelse(pval < 0.001, "p \u2264 0.001", pval2)
    )
  
  g1 = ggplot(out, aes(x = odds_ratio_glm, y = feature))+
    geom_vline(xintercept = 1, lty = 2, col = "grey80")+
    geom_point(shape = 18, size = 4)+
    geom_text(aes(label = pval2), nudge_y = 0.5, size = 3)+ #Fixing the pval labels
    #geom_text(aes(label = paste("p = ", scales::scientific(pval,3), sep ="")), nudge_y = 0.5, size = 3)+
    geom_errorbarh(aes(xmin = confint_2.5., xmax = confint_97.5.), height = 0.1)+
    ggpubr::theme_classic2()+
    xlab("Odds ratio\nwith 95%-confidence intervals")+
    ggtitle("")+
    expand_limits(y = 6)+
    ylab("")
  
#g1
quartz(type = 'pdf', file = "Results/Figures/case_control_RPL.pdf", width = 4, height = 3)
g1
dev.off()

ggplot(d, aes(x = group, y = ldl))+
  geom_boxplot()

openxlsx::write.xlsx(out, "Results/Tables/Case_control_comparrisons.xlsx")


