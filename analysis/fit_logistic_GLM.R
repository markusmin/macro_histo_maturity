### Fitting logistic curves to maturity ###

# This code calculates L50 for all three species and produces Figure 1

library(tidyverse)
library(here)
library(faraway)

##--Sablefish-------------------------------------------------------------------

SABL.mat<-read.csv(here("data", "sablefish_maturity.csv"))
SABL.cert<-subset(SABL.mat,Certainty==1) #subset only certain sammples#
Data.in<-SABL.cert

### LOGISTIC REGRESSION - ALL MATURITY TYPES


SABL.cert %>% 
  dplyr::select(Ovary_ID, Length, Biological_maturity, Functional_maturity, Macro_maturity) %>% 
  pivot_longer(cols = c(Biological_maturity, Functional_maturity, Macro_maturity), names_to = "maturity_type") -> SABL.mat.forglm

fit.mat.glm.SABL.2 <- glm (value ~ Length:maturity_type, data = SABL.mat.forglm,
                             family = binomial(link ="logit"))
faraway::sumary(fit.mat.glm.SABL.2)








### BIOLOGICAL MATURITY ###

###glm fit###
fit.mat.glm.bio.SABL <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                        family = binomial(link ="logit"))
vector.bio.SABL = c(fit.mat.glm.bio.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.SABL <- vector.bio.SABL[1] 
B_glm.bio.SABL <- vector.bio.SABL[2]
Lmat50.bio.SABL = -(A_glm.bio.SABL/B_glm.bio.SABL)
Lmat50.bio.SABL

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio.SABL <- summary (fit.mat.glm.bio.SABL)$coef[1,2] 
sB.bio.SABL <- summary (fit.mat.glm.bio.SABL)$coef[2,2]    
r.bio.SABL <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio.SABL <- ((sA.bio.SABL^2)/(B_glm.bio.SABL^2))- ((2*A_glm.bio.SABL*sA.bio.SABL*sB.bio.SABL*r.bio.SABL)/(B_glm.bio.SABL^3))+ (((A_glm.bio.SABL^2)*(sB.bio.SABL^2))/(B_glm.bio.SABL^4))
deltamethod.bio.SABL

### 95% Confidence Interval equation

con.bio.SABL <- 1.96*(sqrt(deltamethod.bio.SABL)/sqrt(n))
bio.SABL_mat = c(Lmat50.bio.SABL,con.bio.SABL)



### FUNCTIONAL MATURITY ###

# count number of skipped spawners
SABL_skip <- sum(SABL.cert$Biological_maturity) - sum(SABL.cert$Functional_maturity)

###glm fit###

fit.mat.glm.fun.SABL <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                        family = binomial(link ="logit"))
vector.fun.SABL = c(fit.mat.glm.fun.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.SABL <- vector.fun.SABL[1] 
B_glm.fun.SABL <- vector.fun.SABL[2]
Lmat50.fun.SABL = -(A_glm.fun.SABL/B_glm.fun.SABL)
Lmat50.fun.SABL

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun.SABL <- summary (fit.mat.glm.fun.SABL)$coef[1,2] 
sB.fun.SABL <- summary (fit.mat.glm.fun.SABL)$coef[2,2]    
r.fun.SABL <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun.SABL <- ((sA.fun.SABL^2)/(B_glm.fun.SABL^2))- ((2*A_glm.fun.SABL*sA.fun.SABL*sB.fun.SABL*r.fun.SABL)/(B_glm.fun.SABL^3))+ (((A_glm.fun.SABL^2)*(sB.fun.SABL^2))/(B_glm.fun.SABL^4))
deltamethod.fun.SABL

### 95% Confidence Interval equation 

con.fun.SABL <- 1.96*(sqrt(deltamethod.fun.SABL)/sqrt(n))
fun.SABL_mat = c(Lmat50.fun.SABL,con.fun.SABL)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro.SABL <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                          family = binomial(link ="logit"))
vector.macro.SABL = c(fit.mat.glm.macro.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.SABL <- vector.macro.SABL[1] 
B_glm.macro.SABL <- vector.macro.SABL[2]
Lmat50.macro.SABL = -(A_glm.macro.SABL/B_glm.macro.SABL)
Lmat50.macro.SABL

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro.SABL <- summary (fit.mat.glm.macro.SABL)$coef[1,2] 
sB.macro.SABL <- summary (fit.mat.glm.macro.SABL)$coef[2,2]    
r.macro.SABL <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro.SABL <- ((sA.macro.SABL^2)/(B_glm.macro.SABL^2))- ((2*A_glm.macro.SABL*sA.macro.SABL*sB.macro.SABL*r.macro.SABL)/(B_glm.macro.SABL^3))+ (((A_glm.macro.SABL^2)*(sB.macro.SABL^2))/(B_glm.macro.SABL^4))
deltamethod.macro.SABL

##### 95% Confidence Interval equation#######

con.macro.SABL <- 1.96*(sqrt(deltamethod.macro.SABL)/sqrt(n))
macro.SABL_mat = c(Lmat50.macro.SABL,con.macro.SABL)

### RESULTS ###

GLM.maturity.data = rbind(bio.SABL_mat,fun.SABL_mat,macro.SABL_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
sablefish_maturity_results <- GLM.maturity.data
sablefish_maturity_results$species <- "Sablefish"
sablefish_maturity_results <- rownames_to_column(sablefish_maturity_results, "mat_type")
sablefish_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> sablefish_maturity_results

sablefish_maturity_results$alpha <- c(paste0(round(A_glm.bio.SABL,2), " (", round(sA.bio.SABL,2), ")"), paste0(round(A_glm.fun.SABL,2), 
                                            " (", round(sA.fun.SABL,2), ")"), paste0(round(A_glm.macro.SABL,2), " (", round(sA.macro.SABL, 2), ")"))
sablefish_maturity_results$beta <- c(paste0(round(B_glm.bio.SABL,2), " (", round(sB.bio.SABL,2), ")"), paste0(round(B_glm.fun.SABL,2), 
                                            " (", round(sB.fun.SABL,2), ")"), paste0(round(B_glm.macro.SABL,2), " (", round(sB.macro.SABL, 2), ")"))


# round values
sablefish_maturity_results$Lmat50 <- round(sablefish_maturity_results$Lmat50, 2)

### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.bio.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.fun.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.macro.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))




##### Get parameters for logistic maturity curves #####

# Macro maturity #
bin_macro.SABL <- SABL.cert$size
bin_macro.SABL <- bin_macro.SABL[!is.na(bin_macro.SABL)]
prop_macro.SABL <- SABL.cert$prop_macro
prop_macro.SABL <- prop_macro.SABL[!is.na(prop_macro.SABL)]
n_macro.SABL <- SABL.cert$N
n_macro.SABL <- n_macro.SABL[!is.na(n_macro.SABL)]
fit.mat.glm.macro.SABL[1]
df.fit.mat.glm.macro.SABL = as.data.frame(fit.mat.glm.macro.SABL[1])
A_glm.macro.SABL = df.fit.mat.glm.macro.SABL$coefficients[1]
B_glm.macro.SABL = df.fit.mat.glm.macro.SABL$coefficients[2]

# Biological maturity #
bin_bio.SABL <- SABL.cert$size
bin_bio.SABL <- bin_bio.SABL[!is.na(bin_bio.SABL)]
prop_bio.SABL <- SABL.cert$prop_bio
prop_bio.SABL <- prop_bio.SABL[!is.na(prop_bio.SABL)]
n_bio.SABL <- SABL.cert$N
n_bio.SABL <- n_bio.SABL[!is.na(n_bio.SABL)]
fit.mat.glm.bio.SABL[1]
df.fit.mat.glm.bio.SABL = as.data.frame(fit.mat.glm.bio.SABL[1])
A_glm.bio.SABL = df.fit.mat.glm.bio.SABL$coefficients[1]
B_glm.bio.SABL = df.fit.mat.glm.bio.SABL$coefficients[2]

# Functional maturity #
bin_fun.SABL <- SABL.cert$size
bin_fun.SABL <- bin_fun.SABL[!is.na(bin_fun.SABL)]
prop_fun.SABL <- SABL.cert$prop_fun
prop_fun.SABL <- prop_fun.SABL[!is.na(prop_fun.SABL)]
n_fun.SABL <- SABL.cert$N
n_fun.SABL <- n_fun.SABL[!is.na(n_fun.SABL)]
fit.mat.glm.fun.SABL[1]
df.fit.mat.glm.fun.SABL = as.data.frame(fit.mat.glm.fun.SABL[1])
A_glm.fun.SABL = df.fit.mat.glm.fun.SABL$coefficients[1]
B_glm.fun.SABL = df.fit.mat.glm.fun.SABL$coefficients[2]

##--Canary Rockfish-------------------------------------------------------------

CNRY.mat<-read.csv(here("data", "canary_maturity.csv"))
CNRY.cert<-subset(CNRY.mat,Certainty==1) #subset only certain sammples#
Data.in<-CNRY.cert


### BIOLOGICAL MATURITY ###

###glm fit###
fit.mat.glm.bio.CNRY <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                             family = binomial(link ="logit"))
vector.bio.CNRY = c(fit.mat.glm.bio.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.CNRY <- vector.bio.CNRY[1] 
B_glm.bio.CNRY <- vector.bio.CNRY[2]
Lmat50.bio.CNRY = -(A_glm.bio.CNRY/B_glm.bio.CNRY)
Lmat50.bio.CNRY

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio.CNRY <- summary (fit.mat.glm.bio.CNRY)$coef[1,2] 
sB.bio.CNRY <- summary (fit.mat.glm.bio.CNRY)$coef[2,2]    
r.bio.CNRY <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio.CNRY <- ((sA.bio.CNRY^2)/(B_glm.bio.CNRY^2))- ((2*A_glm.bio.CNRY*sA.bio.CNRY*sB.bio.CNRY*r.bio.CNRY)/(B_glm.bio.CNRY^3))+ (((A_glm.bio.CNRY^2)*(sB.bio.CNRY^2))/(B_glm.bio.CNRY^4))
deltamethod.bio.CNRY

### 95% Confidence Interval equation

con.bio.CNRY <- 1.96*(sqrt(deltamethod.bio.CNRY)/sqrt(n))
bio.CNRY_mat = c(Lmat50.bio.CNRY,con.bio.CNRY)



### FUNCTIONAL MATURITY ###

# count number of skipped spawners
CNRY_skip <- sum(CNRY.cert$Biological_maturity) - sum(CNRY.cert$Functional_maturity)

###glm fit###

fit.mat.glm.fun.CNRY <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                             family = binomial(link ="logit"))
vector.fun.CNRY = c(fit.mat.glm.fun.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.CNRY <- vector.fun.CNRY[1] 
B_glm.fun.CNRY <- vector.fun.CNRY[2]
Lmat50.fun.CNRY = -(A_glm.fun.CNRY/B_glm.fun.CNRY)
Lmat50.fun.CNRY

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun.CNRY <- summary (fit.mat.glm.fun.CNRY)$coef[1,2] 
sB.fun.CNRY <- summary (fit.mat.glm.fun.CNRY)$coef[2,2]    
r.fun.CNRY <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun.CNRY <- ((sA.fun.CNRY^2)/(B_glm.fun.CNRY^2))- ((2*A_glm.fun.CNRY*sA.fun.CNRY*sB.fun.CNRY*r.fun.CNRY)/(B_glm.fun.CNRY^3))+ (((A_glm.fun.CNRY^2)*(sB.fun.CNRY^2))/(B_glm.fun.CNRY^4))
deltamethod.fun.CNRY

### 95% Confidence Interval equation 

con.fun.CNRY <- 1.96*(sqrt(deltamethod.fun.CNRY)/sqrt(n))
fun.CNRY_mat = c(Lmat50.fun.CNRY,con.fun.CNRY)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro.CNRY <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                               family = binomial(link ="logit"))
vector.macro.CNRY = c(fit.mat.glm.macro.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.CNRY <- vector.macro.CNRY[1] 
B_glm.macro.CNRY <- vector.macro.CNRY[2]
Lmat50.macro.CNRY = -(A_glm.macro.CNRY/B_glm.macro.CNRY)
Lmat50.macro.CNRY

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro.CNRY <- summary (fit.mat.glm.macro.CNRY)$coef[1,2] 
sB.macro.CNRY <- summary (fit.mat.glm.macro.CNRY)$coef[2,2]    
r.macro.CNRY <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro.CNRY <- ((sA.macro.CNRY^2)/(B_glm.macro.CNRY^2))- ((2*A_glm.macro.CNRY*sA.macro.CNRY*sB.macro.CNRY*r.macro.CNRY)/(B_glm.macro.CNRY^3))+ (((A_glm.macro.CNRY^2)*(sB.macro.CNRY^2))/(B_glm.macro.CNRY^4))
deltamethod.macro.CNRY

##### 95% Confidence Interval equation#######

con.macro.CNRY <- 1.96*(sqrt(deltamethod.macro.CNRY)/sqrt(n))
macro.CNRY_mat = c(Lmat50.macro.CNRY,con.macro.CNRY)

### RESULTS ###

GLM.maturity.data = rbind(bio.CNRY_mat,fun.CNRY_mat,macro.CNRY_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
canary_maturity_results <- GLM.maturity.data
canary_maturity_results$species <- "Canary rockfish"
canary_maturity_results <- rownames_to_column(canary_maturity_results, "mat_type")
canary_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> canary_maturity_results

canary_maturity_results$alpha <- c(paste0(round(A_glm.bio.CNRY,2), " (", round(sA.bio.CNRY,2), ")"), paste0(round(A_glm.fun.CNRY,2), 
                                                                                                               " (", round(sA.fun.CNRY,2), ")"), paste0(round(A_glm.macro.CNRY,2), " (", round(sA.macro.CNRY, 2), ")"))
canary_maturity_results$beta <- c(paste0(round(B_glm.bio.CNRY,2), " (", round(sB.bio.CNRY,2), ")"), paste0(round(B_glm.fun.CNRY,2), 
                                                                                                              " (", round(sB.fun.CNRY,2), ")"), paste0(round(B_glm.macro.CNRY,2), " (", round(sB.macro.CNRY, 2), ")"))
# round values
canary_maturity_results$Lmat50 <- round(canary_maturity_results$Lmat50, 2)



### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.bio.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.fun.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.macro.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



##### Get parameters for logistic maturity curves #####

# Macro maturity #
bin_macro.CNRY <- CNRY.cert$size
bin_macro.CNRY <- bin_macro.CNRY[!is.na(bin_macro.CNRY)]
prop_macro.CNRY <- CNRY.cert$prop_macro
prop_macro.CNRY <- prop_macro.CNRY[!is.na(prop_macro.CNRY)]
n_macro.CNRY <- CNRY.cert$N
n_macro.CNRY <- n_macro.CNRY[!is.na(n_macro.CNRY)]
fit.mat.glm.macro.CNRY[1]
df.fit.mat.glm.macro.CNRY = as.data.frame(fit.mat.glm.macro.CNRY[1])
A_glm.macro.CNRY = df.fit.mat.glm.macro.CNRY$coefficients[1]
B_glm.macro.CNRY = df.fit.mat.glm.macro.CNRY$coefficients[2]

# Biological maturity #
bin_bio.CNRY <- CNRY.cert$size
bin_bio.CNRY <- bin_bio.CNRY[!is.na(bin_bio.CNRY)]
prop_bio.CNRY <- CNRY.cert$prop_bio
prop_bio.CNRY <- prop_bio.CNRY[!is.na(prop_bio.CNRY)]
n_bio.CNRY <- CNRY.cert$N
n_bio.CNRY <- n_bio.CNRY[!is.na(n_bio.CNRY)]
fit.mat.glm.bio.CNRY[1]
df.fit.mat.glm.bio.CNRY = as.data.frame(fit.mat.glm.bio.CNRY[1])
A_glm.bio.CNRY = df.fit.mat.glm.bio.CNRY$coefficients[1]
B_glm.bio.CNRY = df.fit.mat.glm.bio.CNRY$coefficients[2]

# Functional maturity #
bin_fun.CNRY <- CNRY.cert$size
bin_fun.CNRY <- bin_fun.CNRY[!is.na(bin_fun.CNRY)]
prop_fun.CNRY <- CNRY.cert$prop_fun
prop_fun.CNRY <- prop_fun.CNRY[!is.na(prop_fun.CNRY)]
n_fun.CNRY <- CNRY.cert$N
n_fun.CNRY <- n_fun.CNRY[!is.na(n_fun.CNRY)]
fit.mat.glm.fun.CNRY[1]
df.fit.mat.glm.fun.CNRY = as.data.frame(fit.mat.glm.fun.CNRY[1])
A_glm.fun.CNRY = df.fit.mat.glm.fun.CNRY$coefficients[1]
B_glm.fun.CNRY = df.fit.mat.glm.fun.CNRY$coefficients[2]


##--Arrowtooth Flounder-------------------------------------------------------------

ARTH.mat<-read.csv(here("data", "arrowtooth_maturity.csv"))
ARTH.cert<-subset(ARTH.mat,Certainty==1) #subset only certain sammples#
Data.in<-ARTH.cert


### BIOLOGICAL MATURITY ###

###glm fit###
fit.mat.glm.bio.ARTH <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                             family = binomial(link ="logit"))
vector.bio.ARTH = c(fit.mat.glm.bio.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.ARTH <- vector.bio.ARTH[1] 
B_glm.bio.ARTH <- vector.bio.ARTH[2]
Lmat50.bio.ARTH = -(A_glm.bio.ARTH/B_glm.bio.ARTH)
Lmat50.bio.ARTH

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio.ARTH <- summary (fit.mat.glm.bio.ARTH)$coef[1,2] 
sB.bio.ARTH <- summary (fit.mat.glm.bio.ARTH)$coef[2,2]    
r.bio.ARTH <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio.ARTH <- ((sA.bio.ARTH^2)/(B_glm.bio.ARTH^2))- ((2*A_glm.bio.ARTH*sA.bio.ARTH*sB.bio.ARTH*r.bio.ARTH)/(B_glm.bio.ARTH^3))+ (((A_glm.bio.ARTH^2)*(sB.bio.ARTH^2))/(B_glm.bio.ARTH^4))
deltamethod.bio.ARTH

### 95% Confidence Interval equation

con.bio.ARTH <- 1.96*(sqrt(deltamethod.bio.ARTH)/sqrt(n))
bio.ARTH_mat = c(Lmat50.bio.ARTH,con.bio.ARTH)



### FUNCTIONAL MATURITY ###

# count number of skipped spawners
ARTH_skip <- sum(ARTH.cert$Biological_maturity) - sum(ARTH.cert$Functional_maturity)

###glm fit###

fit.mat.glm.fun.ARTH <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                             family = binomial(link ="logit"))
vector.fun.ARTH = c(fit.mat.glm.fun.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.ARTH <- vector.fun.ARTH[1] 
B_glm.fun.ARTH <- vector.fun.ARTH[2]
Lmat50.fun.ARTH = -(A_glm.fun.ARTH/B_glm.fun.ARTH)
Lmat50.fun.ARTH

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun.ARTH <- summary (fit.mat.glm.fun.ARTH)$coef[1,2] 
sB.fun.ARTH <- summary (fit.mat.glm.fun.ARTH)$coef[2,2]    
r.fun.ARTH <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun.ARTH <- ((sA.fun.ARTH^2)/(B_glm.fun.ARTH^2))- ((2*A_glm.fun.ARTH*sA.fun.ARTH*sB.fun.ARTH*r.fun.ARTH)/(B_glm.fun.ARTH^3))+ (((A_glm.fun.ARTH^2)*(sB.fun.ARTH^2))/(B_glm.fun.ARTH^4))
deltamethod.fun.ARTH

### 95% Confidence Interval equation 

con.fun.ARTH <- 1.96*(sqrt(deltamethod.fun.ARTH)/sqrt(n))
fun.ARTH_mat = c(Lmat50.fun.ARTH,con.fun.ARTH)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro.ARTH <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                               family = binomial(link ="logit"))
vector.macro.ARTH = c(fit.mat.glm.macro.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.ARTH <- vector.macro.ARTH[1] 
B_glm.macro.ARTH <- vector.macro.ARTH[2]
Lmat50.macro.ARTH = -(A_glm.macro.ARTH/B_glm.macro.ARTH)
Lmat50.macro.ARTH

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro.ARTH <- summary (fit.mat.glm.macro.ARTH)$coef[1,2] 
sB.macro.ARTH <- summary (fit.mat.glm.macro.ARTH)$coef[2,2]    
r.macro.ARTH <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro.ARTH <- ((sA.macro.ARTH^2)/(B_glm.macro.ARTH^2))- ((2*A_glm.macro.ARTH*sA.macro.ARTH*sB.macro.ARTH*r.macro.ARTH)/(B_glm.macro.ARTH^3))+ (((A_glm.macro.ARTH^2)*(sB.macro.ARTH^2))/(B_glm.macro.ARTH^4))
deltamethod.macro.ARTH

##### 95% Confidence Interval equation#######

con.macro.ARTH <- 1.96*(sqrt(deltamethod.macro.ARTH)/sqrt(n))
macro.ARTH_mat = c(Lmat50.macro.ARTH,con.macro.ARTH)

### RESULTS ###

GLM.maturity.data = rbind(bio.ARTH_mat,fun.ARTH_mat,macro.ARTH_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
arrowtooth_maturity_results <- GLM.maturity.data
arrowtooth_maturity_results$species <- "Arrowtooth Flounder"
arrowtooth_maturity_results <- rownames_to_column(arrowtooth_maturity_results, "mat_type")
arrowtooth_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> arrowtooth_maturity_results

arrowtooth_maturity_results$alpha <- c(paste0(round(A_glm.bio.ARTH,2), " (", round(sA.bio.ARTH,2), ")"), paste0(round(A_glm.fun.ARTH,2), 
                                                                                                            " (", round(sA.fun.ARTH,2), ")"), paste0(round(A_glm.macro.ARTH,2), " (", round(sA.macro.ARTH, 2), ")"))
arrowtooth_maturity_results$beta <- c(paste0(round(B_glm.bio.ARTH,2), " (", round(sB.bio.ARTH,2), ")"), paste0(round(B_glm.fun.ARTH,2), 
                                                                                                           " (", round(sB.fun.ARTH,2), ")"), paste0(round(B_glm.macro.ARTH,2), " (", round(sB.macro.ARTH, 2), ")"))
  
# round values
arrowtooth_maturity_results$Lmat50 <- round(arrowtooth_maturity_results$Lmat50, 2)



### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.bio.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.fun.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.macro.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



##### Get parameters for logistic maturity curves #####

# Macro maturity #
bin_macro.ARTH <- ARTH.cert$size
bin_macro.ARTH <- bin_macro.ARTH[!is.na(bin_macro.ARTH)]
prop_macro.ARTH <- ARTH.cert$prop_macro
prop_macro.ARTH <- prop_macro.ARTH[!is.na(prop_macro.ARTH)]
n_macro.ARTH <- ARTH.cert$N
n_macro.ARTH <- n_macro.ARTH[!is.na(n_macro.ARTH)]
fit.mat.glm.macro.ARTH[1]
df.fit.mat.glm.macro.ARTH = as.data.frame(fit.mat.glm.macro.ARTH[1])
A_glm.macro.ARTH = df.fit.mat.glm.macro.ARTH$coefficients[1]
B_glm.macro.ARTH = df.fit.mat.glm.macro.ARTH$coefficients[2]

# Biological maturity #
bin_bio.ARTH <- ARTH.cert$size
bin_bio.ARTH <- bin_bio.ARTH[!is.na(bin_bio.ARTH)]
prop_bio.ARTH <- ARTH.cert$prop_bio
prop_bio.ARTH <- prop_bio.ARTH[!is.na(prop_bio.ARTH)]
n_bio.ARTH <- ARTH.cert$N
n_bio.ARTH <- n_bio.ARTH[!is.na(n_bio.ARTH)]
fit.mat.glm.bio.ARTH[1]
df.fit.mat.glm.bio.ARTH = as.data.frame(fit.mat.glm.bio.ARTH[1])
A_glm.bio.ARTH = df.fit.mat.glm.bio.ARTH$coefficients[1]
B_glm.bio.ARTH = df.fit.mat.glm.bio.ARTH$coefficients[2]

# Functional maturity #
bin_fun.ARTH <- ARTH.cert$size
bin_fun.ARTH <- bin_fun.ARTH[!is.na(bin_fun.ARTH)]
prop_fun.ARTH <- ARTH.cert$prop_fun
prop_fun.ARTH <- prop_fun.ARTH[!is.na(prop_fun.ARTH)]
n_fun.ARTH <- ARTH.cert$N
n_fun.ARTH <- n_fun.ARTH[!is.na(n_fun.ARTH)]
fit.mat.glm.fun.ARTH[1]
df.fit.mat.glm.fun.ARTH = as.data.frame(fit.mat.glm.fun.ARTH[1])
A_glm.fun.ARTH = df.fit.mat.glm.fun.ARTH$coefficients[1]
B_glm.fun.ARTH = df.fit.mat.glm.fun.ARTH$coefficients[2]



##--Create combined logistic maturity curve plot----------------------------------------------------------

setEPS()
postscript(here("figures", "Fig1_v2.eps"), height = 8, width = 24)
par(mfrow = c(1,3), xpd=TRUE,mar = c("bottom" = 1, "left" = 1, "top" = 1, "right" = 1))

###### Plot CNRY ######
par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(25,60),ylim=c(-0.01,1.1),xlab="Length (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.CNRY, y = prop_macro.CNRY,cex=sqrt(n_macro.CNRY), pch=16, col = "#984ea3") # macro
points(x = bin_bio.CNRY, y = prop_bio.CNRY,cex=sqrt(n_bio.CNRY), pch=16, col = "#377eb8") # bio
points(x = bin_fun.CNRY, y = prop_fun.CNRY,cex=sqrt(n_fun.CNRY), pch=16, col = "#4daf4a") # fun


lines(25:60, 1/(1+(exp(-(A_glm.macro.CNRY+(B_glm.macro.CNRY*(25:60)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3)####Macro#####
lines(25:60, 1/(1+(exp(-(A_glm.bio.CNRY+(B_glm.bio.CNRY*(25:60)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3)####bio#####
lines(25:60, 1/(1+(exp(-(A_glm.fun.CNRY+(B_glm.fun.CNRY*(25:60)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)####fun#####

# GLM Confidence Bands #

plotdat <- data.frame(length=(25:60))

# macro
predat.1 <- predict(fit.mat.glm.macro.CNRY,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(25:60,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#984ea3"))
with(predat.1, lines(25:60,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#984ea3"))

# bio
predat.2 <- predict(fit.mat.glm.bio.CNRY,newdata=plotdat, se.fit=TRUE)
with(predat.2, lines(25:60,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#377eb8"))
with(predat.2, lines(25:60,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#377eb8"))

# fun
predat.1 <- predict(fit.mat.glm.fun.CNRY,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(25:60,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#4daf4a"))
with(predat.1, lines(25:60,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#4daf4a"))


# L50 line #
segments(24,0.5,61,0.5,lty=5)

#Legend
legend.a <- c(1, 5, 10)

legend.cex <- sqrt(legend.a)

legend(25 ,1.1,legend=legend.a,pch=19,col="gray20",
       cex = 2, pt.cex=legend.cex,ncol=1,bty='n', y.intersp = 1.5)
text(26.3, 1.095, "N",cex = 2.3)


legend(60-(60-25)*0.3,0.2, legend=c("Biological", "Functional", "Macroscopic"), col=c("#377eb8", "#4daf4a", "#984ea3"), lwd=c(3,3),
       lty= c(1,1,1), cex=2, bty="n")

# Add lettering for figure
text(59, 1.08, "(a)",cex = 4)

###### Plot SABL ######
par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(35,75),ylim=c(-0.01,1.1),xlab="Length (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.SABL, y = prop_macro.SABL,cex=sqrt(n_macro.SABL), pch=16, col = "#984ea3") # macro
points(x = bin_bio.SABL, y = prop_bio.SABL,cex=sqrt(n_bio.SABL), pch=16, col = "#377eb8") # bio
points(x = bin_fun.SABL, y = prop_fun.SABL,cex=sqrt(n_fun.SABL), pch=16, col = "#4daf4a") # fun


lines(35:75, 1/(1+(exp(-(A_glm.macro.SABL+(B_glm.macro.SABL*(35:75)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3)####Macro#####
lines(35:75, 1/(1+(exp(-(A_glm.bio.SABL+(B_glm.bio.SABL*(35:75)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3)####bio#####
lines(35:75, 1/(1+(exp(-(A_glm.fun.SABL+(B_glm.fun.SABL*(35:75)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)####fun#####

# GLM Confidence Bands #

plotdat <- data.frame(length=(35:75))

# macro
predat.1 <- predict(fit.mat.glm.macro.SABL,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(35:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#984ea3"))
with(predat.1, lines(35:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#984ea3"))

# bio
predat.2 <- predict(fit.mat.glm.bio.SABL,newdata=plotdat, se.fit=TRUE)
with(predat.2, lines(35:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#377eb8"))
with(predat.2, lines(35:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#377eb8"))

# fun
predat.1 <- predict(fit.mat.glm.fun.SABL,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(35:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#4daf4a"))
with(predat.1, lines(35:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#4daf4a"))


# L50 line #
segments(33.5,0.5,76.5,0.5,lty=5)

#Legend
legend.a <- c(1,5,10)

legend.cex <- sqrt(legend.a)

legend(35 ,1.1,legend=legend.a,pch=19,col="gray20",
       cex = 2, pt.cex=legend.cex,ncol=1,bty='n', y.intersp = 1.5)
text(36.3, 1.095, "N",cex = 2.3)


legend(75-(75-35)*0.3,0.2, legend=c("Biological", "Functional", "Macroscopic"), col=c("#377eb8", "#4daf4a", "#984ea3"), lwd=c(3,3),
       lty= c(1,1,1), cex=2, bty="n")

# Add lettering for figure
text(73.5, 1.08, "(b)",cex = 4)

###### Plot ARTH ######
par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(20,75),ylim=c(-0.01,1.1),xlab="Length (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.ARTH, y = prop_macro.ARTH,cex=sqrt(n_macro.ARTH), pch=16, col = "#984ea3") # macro
points(x = bin_bio.ARTH, y = prop_bio.ARTH,cex=sqrt(n_bio.ARTH), pch=16, col = "#377eb8") # bio
points(x = bin_fun.ARTH, y = prop_fun.ARTH,cex=sqrt(n_fun.ARTH), pch=16, col = "#4daf4a") # fun



lines(20:75, 1/(1+(exp(-(A_glm.macro.ARTH+(B_glm.macro.ARTH*(20:75)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3)####Macro#####
lines(20:75, 1/(1+(exp(-(A_glm.bio.ARTH+(B_glm.bio.ARTH*(20:75)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3)####bio#####
lines(20:75, 1/(1+(exp(-(A_glm.fun.ARTH+(B_glm.fun.ARTH*(20:75)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)####fun#####

# GLM Confidence Bands #

plotdat <- data.frame(length=(20:75))

# macro
predat.1 <- predict(fit.mat.glm.macro.ARTH,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(20:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#984ea3"))
with(predat.1, lines(20:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#984ea3"))

# bio
predat.2 <- predict(fit.mat.glm.bio.ARTH,newdata=plotdat, se.fit=TRUE)
with(predat.2, lines(20:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#377eb8"))
with(predat.2, lines(20:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#377eb8"))

# fun
predat.1 <- predict(fit.mat.glm.fun.ARTH,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(20:75,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#4daf4a"))
with(predat.1, lines(20:75,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#4daf4a"))


# L50 line #
segments(18,0.5,77,0.5,lty=5)

#Legend
legend.a <- c(1, 5, 10)

legend.cex <- sqrt(legend.a)

legend(20 ,1.1,legend=legend.a,pch=19,col="gray20",
       cex = 2, pt.cex=legend.cex,ncol=1,bty='n', y.intersp = 1.5)
text(22, 1.095, "N",cex = 2.3)


legend(75-(75-20)*0.3,0.2, legend=c("Biological", "Functional", "Macroscopic"), col=c("#377eb8", "#4daf4a", "#984ea3"), lwd=c(3,3),
       lty= c(1,1,1), cex=2, bty="n")

# Add lettering for figure
text(74, 1.08, "(c)",cex = 4)

# end plot
dev.off()

##--Store GLM parameters to report in table--##

## Join all species for logistic maturity parameters, export
canary_maturity_results %>% 
  bind_rows(., sablefish_maturity_results) %>% 
  bind_rows(., arrowtooth_maturity_results) -> full_maturity_results

write.csv(full_maturity_results, here("tables", "logistic_parameters_table.csv"), row.names = FALSE)

##--Join results, plot (L50 + CI)----------------------------------------------------------

combined_maturity_results <- union(union(sablefish_maturity_results, canary_maturity_results), arrowtooth_maturity_results)

# Create a color scheme
mat_type_cols <- c("bio_mat" = "#377eb8", "fun_mat" = "#4daf4a", "macro_mat" = "#984ea3")

maturity_plot <- ggplot(combined_maturity_results, aes(x =  species, y = Lmat50, color = mat_type))+
  geom_point(position = position_dodge(width = 0.4), size = 6)+
  geom_errorbar(aes(ymin = (Lmat50 - CI_95), ymax = (Lmat50+CI_95)), position = position_dodge(width = 0.4), width = 0.3)+
  xlab("Species")+
  ylab(expression("L"[50]~" (cm)"))+
  scale_color_manual(values = mat_type_cols)+
  theme(panel.background = element_rect(color = "black", fill = "white", size = 1),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 22),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2, "cm"))+
  # Create a legend manually
  annotate(geom = "rect", ymin = 58, ymax = 66, xmin = 0.5, xmax = 1.6, color = "black", fill = "white")+
  # Title
  annotate(geom = "text", x = 0.55, y = 65, hjust = 0, label = "Maturity Type", size = 7)+
  # Biological Maturity
  annotate(geom = "text", x = 0.7, y = 63, hjust = 0, label = "Biological", size = 6)+
  annotate(geom = "point", x = 0.6, y = 63, color = "#377eb8", size = 4.5)+
  # Functional Maturity
  annotate(geom = "text", x = 0.7, y = 61, hjust = 0, label = "Functional", size = 6)+
  annotate(geom = "point", x = 0.6, y = 61, color = "#4daf4a", size = 4.5)+
  # Macroscopic Maturity
  annotate(geom = "text", x = 0.7, y = 59, hjust = 0, label = "Macroscopic", size = 6)+
  annotate(geom = "point", x = 0.6, y = 59, color = "#984ea3", size = 4.5)

maturity_plot

ggsave(here("figures", "Fig1.eps"), maturity_plot, width = 8, height = 8, device = "eps")


## count total number of skipped spawners
SABL_skip
ARTH_skip
CNRY_skip


