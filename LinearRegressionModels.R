#packages needed
library(tidyr)
library(tidyverse)
library(dplyr)
library(NormPsy)
library(MASS)
library(stargazer)
library(sandwich)

##remove NA's in diabetes and MMSE scores and remove columns
data1 <- data1 %>% drop_na(MMSE_TOT)
data1 <- data1 %>% drop_na(dmadas)
data1 <- data1 %>% drop_na(dm_sahs_bl)
data1 <- dplyr::select(data1, -c(11:28)) #do not need these columns for models
##ensure categorical variables are factors
data1$n_hood <- as.factor(data1$n_hood) 
data1$sex <- as.factor(data1$sex)
data1$neweth <- as.factor(data1$neweth)
data1$dmadas <- as.factor(data1$dmadas)
data1$dm_sahs_bl <- as.factor(data1$dm_sahs_bl)

#use NormPsy NormMMSE Package to transform MMSE Scores and include in model
data1$MMSEnorm <- normMMSE(data1$MMSE_TOT)

##normalization of data (MMSE) using boxcox
model <- lm(MMSEnorm ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood + neweth, data=data1)
bc <- boxcox(model)
lambda <- bc$x[which(bc$y==max(bc$y))]
data1$MMSEnorm_trans <- (data1$MMSEnorm ^ lambda - 1) / lambda

##models and stargazer
#latelifediabetesmodel1
latemodel1 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p, data=data1)
#latelifediabetesmodel2
latemodel2 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood + neweth, data=data1)
#midlifediabetesmodel1
midmodel1 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p, data=data1)
#midlifediabetesmodel2
midmodel2 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p + HHINC + n_hood + neweth, data=data1)

#saves tables of models as html files using stargazer library
cat(
  stargazer(latemodel1, latemodel2,
            type="html",
            style="ajps",
            digits = 3,
            header = F),
  file="latemodels.html"
)

cat(
  stargazer(midmodel1, midmodel2,
            type="html",
            style="ajps",
            digits = 3,
            header = F),
  file="midmodels.html"
)

####stratify dataset for models by ethnicity
ma_data <- data1[ which(data1$neweth=='1'),]
ea_data <- data1[ which(data1$neweth=='2'),]
#remove transformed MMSE
ma_data$MMSEnorm_trans <- NULL
ea_data$MMSEnorm_trans <- NULL

#creating new transformation for each dataset
#MA Dataset
mamodel <- lm(MMSEnorm ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood, data=ma_data)
mabc <- boxcox(mamodel)
malambda <- mabc$x[which(mabc$y==max(mabc$y))]
ma_data$MMSEnorm_trans <- (ma_data$MMSEnorm ^ malambda - 1) / malambda
#EA Dataset
eamodel <- lm(MMSEnorm ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood, data=ea_data)
eabc <- boxcox(eamodel)
ealambda <- eabc$x[which(eabc$y==max(eabc$y))]
ea_data$MMSEnorm_trans <- (ea_data$MMSEnorm ^ ealambda - 1) / ealambda

##models
#mamodel1
mamod1 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p, data=ma_data)
#mamodel2
mamod2 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood, data=ma_data)
#mamodel3
mamod3 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p, data=ma_data)
#mamodel4
mamod4 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p + HHINC + n_hood, data=ma_data)

#eamodel1
eamod1 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p, data=ea_data)
#eamodel2
eamod2 <- lm(MMSEnorm_trans ~ dmadas + age_pba + sex + edu_p + HHINC + n_hood, data=ea_data)
#eamodel3
eamod3 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p, data=ea_data)
#eamodel4
eamod4 <- lm(MMSEnorm_trans ~ dm_sahs_bl + age_pba + sex + edu_p + HHINC + n_hood, data=ea_data)

#saves tables of models as html files using stargazer library
cat(
  stargazer(mamod1, mamod2, mamod3, mamod4, 
            type="html",
            style="ajps",
            digits = 3,
            header = F),
  file="MAmodels.html"
)
cat(
  stargazer(eamod1, eamod2, eamod3, eamod4,
            type="html",
            style="ajps",
            digits = 3,
            header = F),
  file="EAmodels.html"
)

#check skewnewss of residuals to see if better distribution
library(e1071)
skewness(latemodel1$resid)
skewness(latemodel2$residuals)
skewness(midmodel1$residuals)
skewness(midmodel2$residuals)
skewness(mamod1$residuals)
skewness(mamod2$residuals)
skewness(mamod3$residuals)
skewness(mamod4$residuals)
skewness(eamod1$residuals)
skewness(eamod2$residuals)
skewness(eamod3$residuals)
skewness(eamod4$residuals)
