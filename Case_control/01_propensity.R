library(tidyverse)
library(tableone)
library(data.table)
setwd("/Users/simon/Documents/Project/COPL_Marie_Louise/")
source("Code/Plot_themes.R")

#The MatchIt, lmtest and sandwich libraries are used.
library(MatchIt)
library(lmtest)
library(sandwich)

d = readxl::read_excel("Data/AbHab 29-02-2024.xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))

# Calculate standardized mean difference for unmatched data
## Covariates
vars <- c("age","BMI", "smoking")

## Construct a table
tabUnmatched <- CreateTableOne(vars = vars, strata = "group", data = d, test = FALSE)
## Show table with SMD
print(tabUnmatched, smd = TRUE)

# ## 
d2 = d %>% tidyr::drop_na(age, BMI, smoking)
match_obj <- matchit(group ~ age + BMI + smoking,
                     data = d2,
                     method = "nearest",
                     distance ="glm",
                     ratio = 1,
                     caliper = 0.1, #0.1 standard deviation of propensity score
                     replace = FALSE)

summary(match_obj)
matched_data <- match.data(match_obj)

# Calculate standardized mean difference for matched data
## Construct a table
tabMatched <- CreateTableOne(vars = vars, strata = "group", data = matched_data, test = FALSE)
## Show table with SMD
print(tabMatched, smd = TRUE)


# matched_data <- match.data(match_obj)

#plotting the balance between smokers and non-smokers
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

g1 = ggplot(matched_data, aes(x = age, fill = group))+
  geom_density( alpha= 0.2)

g2 = ggplot(matched_data, aes(x = smoking, fill = group))+
  geom_density( alpha= 0.2)

g3 = ggplot(matched_data, aes(x = BMI, fill = group))+
  geom_density( alpha= 0.2)

library(patchwork)
g1+g2+g3

write.table(matched_data, "Data/matched_data.tsv", sep = "\t", col.names = T, row.names = F)
