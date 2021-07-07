### Fitting logistic curves to maturity - sensitivity testing ###

# Here we are looking at the sensitivity of the logistic curves to including vs. excluding stage 7. 
# This may give us more justification for excluding them from the historical analysis

# This code calculates L50 for all three species and produces Figure 1
# This is v2 because I'm now making sure to use the re-read files

library(tidyverse)
library(here)
library(faraway)
library(janitor)

##--Sablefish-------------------------------------------------------------------

SABL.mat<-read_excel(here("data", "2015_2016 ODFW Sablefish maturity_updated.xlsx"))
SABL.mat <- clean_names(SABL.mat)
SABL.cert<-subset(SABL.mat,certainty==1) #subset only certain samples and those that were staged macroscopically#
SABL.cert <- subset(SABL.cert, !(is.na(maturity_code)))
SABL.cert <- clean_names(SABL.cert)

# Move spent to stage 11 and resting/recovering/regenerating to stage 12
SABL.cert %>% 
  dplyr::rename("x11" = "spent_post_spawn", "x12" = "regenerating_recovering") %>% 
  # rename maturity_code as macro_maturity_code
  dplyr::rename(macro_maturity_code = maturity_code) -> SABL.cert

# Compute macroscopic maturity (binary)
SABL.cert %>% 
  mutate(macro_maturity = ifelse(macro_maturity_code %in% c(1,2), 0, 1)) -> SABL.cert

# Re-calculate biological maturity (binary)
# If it has any mature histological stage (4.1 or higher), it's mature
SABL.cert %>% 
  mutate(biological_maturity = ifelse(x12 == "Y" | x11 == "Y" | x10 == "Y" | x9 == "Y" | x8 == "Y" | x7 == "Y" |
                                        x6 == "Y" | x5 == "Y" | x4_2 == "Y" | x4_1 == "Y", 1, 0)) -> SABL.cert

# Functional maturity has already been calculated by Melissa in the doc when she was reading the slides.


#####BIOLOGICAL MATURITY#####

###glm fit###
fit.mat.glm.bio.SABL <- glm (maturity ~ length, data = data.frame(length = SABL.cert$length_cm, maturity = SABL.cert$biological_maturity),
                             family = binomial(link ="logit"))
vector.bio.SABL = c(fit.mat.glm.bio.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.SABL <- vector.bio.SABL[1] 
B_glm.bio.SABL <- vector.bio.SABL[2]
Lmat50.bio.SABL = -(A_glm.bio.SABL/B_glm.bio.SABL)
Lmat50.bio.SABL

cor(SABL.cert$length_cm, SABL.cert$biological_maturity)

sA.bio.SABL <- summary (fit.mat.glm.bio.SABL)$coef[1,2] 
sB.bio.SABL <- summary (fit.mat.glm.bio.SABL)$coef[2,2]    
r.bio.SABL <-cor(SABL.cert$length_cm, SABL.cert$biological_maturity)
n <- sum(SABL.cert$certainty)

#Variance estimator
deltamethod.bio.SABL <- ((sA.bio.SABL^2)/(B_glm.bio.SABL^2))- ((2*A_glm.bio.SABL*sA.bio.SABL*sB.bio.SABL*r.bio.SABL)/(B_glm.bio.SABL^3))+ (((A_glm.bio.SABL^2)*(sB.bio.SABL^2))/(B_glm.bio.SABL^4))
deltamethod.bio.SABL

### 95% Confidence Interval equation

con.bio.SABL <- 1.96*(sqrt(deltamethod.bio.SABL)/sqrt(n))
bio.SABL_mat = c(Lmat50.bio.SABL,con.bio.SABL)



#####FUNCTIONAL MATURITY#####

# count number of skipped spawners
# simply doing functional minus biological isn't right, because an individual not all functionally immature but biologically mature 
# individuals are skip spawners; could also only be stage 4.1
# SABL_skip <- sum(SABL.cert$biological_maturity) - sum(SABL.cert$functional_maturity)
SABL_skip <- dim(subset(SABL.cert, percent_atresia >=25))[1]

###glm fit###

fit.mat.glm.fun.SABL <- glm (maturity ~ 1 + length, data <-data.frame(length = SABL.cert$length_cm, maturity <- SABL.cert$functional_maturity),
                             family = binomial(link ="logit"))
vector.fun.SABL = c(fit.mat.glm.fun.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.SABL <- vector.fun.SABL[1] 
B_glm.fun.SABL <- vector.fun.SABL[2]
Lmat50.fun.SABL = -(A_glm.fun.SABL/B_glm.fun.SABL)
Lmat50.fun.SABL

cor(SABL.cert$length_cm, SABL.cert$functional_maturity)

sA.fun.SABL <- summary (fit.mat.glm.fun.SABL)$coef[1,2] 
sB.fun.SABL <- summary (fit.mat.glm.fun.SABL)$coef[2,2]    
r.fun.SABL <-cor(SABL.cert$length_cm, SABL.cert$functional_maturity)
n <- sum(SABL.cert$certainty)

#Variance estimator
deltamethod.fun.SABL <- ((sA.fun.SABL^2)/(B_glm.fun.SABL^2))- ((2*A_glm.fun.SABL*sA.fun.SABL*sB.fun.SABL*r.fun.SABL)/(B_glm.fun.SABL^3))+ (((A_glm.fun.SABL^2)*(sB.fun.SABL^2))/(B_glm.fun.SABL^4))
deltamethod.fun.SABL

### 95% Confidence Interval equation 

con.fun.SABL <- 1.96*(sqrt(deltamethod.fun.SABL)/sqrt(n))
fun.SABL_mat = c(Lmat50.fun.SABL,con.fun.SABL)



#####MACRO MATURITY - ALL CODES#####

###glm fit###

fit.mat.glm.macro.SABL <- glm (maturity ~ 1 + length, data <-data.frame(length = SABL.cert$length_cm, maturity <- SABL.cert$macro_maturity),
                               family = binomial(link ="logit"))
vector.macro.SABL = c(fit.mat.glm.macro.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.SABL <- vector.macro.SABL[1] 
B_glm.macro.SABL <- vector.macro.SABL[2]
Lmat50.macro.SABL = -(A_glm.macro.SABL/B_glm.macro.SABL)
Lmat50.macro.SABL

cor(SABL.cert$length_cm, SABL.cert$macro_maturity)

sA.macro.SABL <- summary (fit.mat.glm.macro.SABL)$coef[1,2] 
sB.macro.SABL <- summary (fit.mat.glm.macro.SABL)$coef[2,2]    
r.macro.SABL <-cor(SABL.cert$length_cm, SABL.cert$macro_maturity)
n <- sum(SABL.cert$certainty)

#Variance estimator
deltamethod.macro.SABL <- ((sA.macro.SABL^2)/(B_glm.macro.SABL^2))- ((2*A_glm.macro.SABL*sA.macro.SABL*sB.macro.SABL*r.macro.SABL)/(B_glm.macro.SABL^3))+ (((A_glm.macro.SABL^2)*(sB.macro.SABL^2))/(B_glm.macro.SABL^4))
deltamethod.macro.SABL

### 95% Confidence Interval equation 

con.macro.SABL <- 1.96*(sqrt(deltamethod.macro.SABL)/sqrt(n))
macro.SABL_mat = c(Lmat50.macro.SABL,con.macro.SABL)

### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.bio.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.fun.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.macro.SABL,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))





#####MACRO MATURITY - NO 7#####

SABL.cert.no_7 <- subset(SABL.cert, macro_maturity_code != 7)

###glm fit###

fit.mat.glm.macro_no7.SABL <- glm (maturity ~ 1 + length, data <-data.frame(length = SABL.cert.no_7$length_cm, maturity <- SABL.cert.no_7$macro_maturity),
                                   family = binomial(link ="logit"))
vector.macro_no7.SABL = c(fit.mat.glm.macro_no7.SABL$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro_no7.SABL <- vector.macro_no7.SABL[1] 
B_glm.macro_no7.SABL <- vector.macro_no7.SABL[2]
Lmat50.macro_no7.SABL = -(A_glm.macro_no7.SABL/B_glm.macro_no7.SABL)
Lmat50.macro_no7.SABL

cor(SABL.cert.no_7$length_cm, SABL.cert.no_7$macro_maturity)

sA.macro_no7.SABL <- summary (fit.mat.glm.macro_no7.SABL)$coef[1,2] 
sB.macro_no7.SABL <- summary (fit.mat.glm.macro_no7.SABL)$coef[2,2]    
r.macro_no7.SABL <-cor(SABL.cert.no_7$length_cm, SABL.cert.no_7$macro_maturity)
n <- sum(SABL.cert.no_7$certainty)

#Variance estimator
deltamethod.macro_no7.SABL <- ((sA.macro_no7.SABL^2)/(B_glm.macro_no7.SABL^2))- ((2*A_glm.macro_no7.SABL*sA.macro_no7.SABL*sB.macro_no7.SABL*r.macro_no7.SABL)/(B_glm.macro_no7.SABL^3))+ (((A_glm.macro_no7.SABL^2)*(sB.macro_no7.SABL^2))/(B_glm.macro_no7.SABL^4))
deltamethod.macro_no7.SABL

### 95% Confidence Interval equation 

con.macro_no7.SABL <- 1.96*(sqrt(deltamethod.macro_no7.SABL)/sqrt(n))
macro_no7.SABL_mat = c(Lmat50.macro_no7.SABL,con.macro_no7.SABL)


##### Get parameters for logistic maturity curves #####

# MACRO MATURITY #

# get length bins
bin_macro.SABL <- as.numeric(as.character(as.data.frame(table(SABL.cert$length_cm))$Var1))

# get proportion mature at each length bin
SABL.cert %>% 
  group_by(length_cm) %>% 
  count(macro_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_macro = n/total_samples) -> SABL.macro.counts 

SABL.macro.counts[!rev(duplicated(rev(SABL.macro.counts$length_cm))),] %>% 
  mutate(prop_macro = ifelse(macro_maturity == 0, 0, prop_macro)) -> SABL.macro.props

prop_macro.SABL <- SABL.macro.props$prop_macro

# get number at each length bin
n_macro.SABL <- as.data.frame(table(SABL.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.macro.SABL = as.data.frame(fit.mat.glm.macro.SABL[1])
A_glm.macro.SABL = df.fit.mat.glm.macro.SABL$coefficients[1]
B_glm.macro.SABL = df.fit.mat.glm.macro.SABL$coefficients[2]

# BIOLOGICAL MATURITY #

# get length bins
bin_bio.SABL <- as.numeric(as.character(as.data.frame(table(SABL.cert$length_cm))$Var1))

# get proportion mature at each length bin
SABL.cert %>% 
  group_by(length_cm) %>% 
  count(biological_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_bio = n/total_samples) -> SABL.bio.counts 

SABL.bio.counts[!rev(duplicated(rev(SABL.bio.counts$length_cm))),] %>% 
  mutate(prop_bio = ifelse(biological_maturity == 0, 0, prop_bio)) -> SABL.bio.props

prop_bio.SABL <- SABL.bio.props$prop_bio

# get number at each length bin
n_bio.SABL <- as.data.frame(table(SABL.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.bio.SABL = as.data.frame(fit.mat.glm.bio.SABL[1])
A_glm.bio.SABL = df.fit.mat.glm.bio.SABL$coefficients[1]
B_glm.bio.SABL = df.fit.mat.glm.bio.SABL$coefficients[2]

# FUNCTIONAL MATURITY #

# get length bins
bin_fun.SABL <- as.numeric(as.character(as.data.frame(table(SABL.cert$length_cm))$Var1))

# get proportion mature at each length bin
SABL.cert %>% 
  group_by(length_cm) %>% 
  count(functional_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_fun = n/total_samples) -> SABL.fun.counts 

SABL.fun.counts[!rev(duplicated(rev(SABL.fun.counts$length_cm))),] %>% 
  mutate(prop_fun = ifelse(functional_maturity == 0, 0, prop_fun)) -> SABL.fun.props

prop_fun.SABL <- SABL.fun.props$prop_fun

# get number at each length bin
n_fun.SABL <- as.data.frame(table(SABL.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.fun.SABL = as.data.frame(fit.mat.glm.fun.SABL[1])
A_glm.fun.SABL = df.fit.mat.glm.fun.SABL$coefficients[1]
B_glm.fun.SABL = df.fit.mat.glm.fun.SABL$coefficients[2]



#####SUMMARISE SABLEFISH RESULTS#####

GLM.maturity.data = rbind(bio.SABL_mat,fun.SABL_mat,macro.SABL_mat, macro_no7.SABL_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
sablefish_maturity_results <- GLM.maturity.data
sablefish_maturity_results$species <- "Sablefish"
sablefish_maturity_results <- rownames_to_column(sablefish_maturity_results, "mat_type")
sablefish_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> sablefish_maturity_results

sablefish_maturity_results$alpha <- c(paste0(round(A_glm.bio.SABL,2), " (", round(sA.bio.SABL,2), ")"), paste0(round(A_glm.fun.SABL,2), 
                                                                                                               " (", round(sA.fun.SABL,2), ")"), paste0(round(A_glm.macro.SABL,2), " (", round(sA.macro.SABL, 2), ")"),
                                      paste0(round(A_glm.macro_no7.SABL,2), " (", round(sA.macro_no7.SABL, 2), ")"))
sablefish_maturity_results$beta <- c(paste0(round(B_glm.bio.SABL,2), " (", round(sB.bio.SABL,2), ")"), paste0(round(B_glm.fun.SABL,2), 
                                                                                                              " (", round(sB.fun.SABL,2), ")"), paste0(round(B_glm.macro.SABL,2), " (", round(sB.macro.SABL, 2), ")"),
                                     paste0(round(B_glm.macro_no7.SABL,2), " (", round(sB.macro_no7.SABL, 2), ")"))


# round values
sablefish_maturity_results$Lmat50 <- round(sablefish_maturity_results$Lmat50, 2)


##--Canary Rockfish-------------------------------------------------------------
CNRY.mat <-read.csv(here("data", "2015_2017_ODFW_canary_maturity_reread.csv"))
CNRY.mat <- clean_names(CNRY.mat)
CNRY.cert<-subset(CNRY.mat,certainty==1) #subset only certain samples and those that were staged macroscopically#
CNRY.cert <- subset(CNRY.cert, !(is.na(maturity_code)))
CNRY.cert <- clean_names(CNRY.cert)


# Move spent to stage 11 and resting/recovering/regenerating to stage 12
CNRY.cert %>% 
  dplyr::rename("x11" = "spent", "x12" = "reorganizing_regenerating") %>% 
  # rename maturity_code as macro_maturity_code
  dplyr::rename(macro_maturity_code = maturity_code) -> CNRY.cert

# Compute macroscopic maturity (binary)
CNRY.cert %>% 
  mutate(macro_maturity = ifelse(macro_maturity_code %in% c(1,2), 0, 1)) -> CNRY.cert

# Re-calculate biological maturity (binary)
# If it has any mature histological stage (4.1 or higher), it's mature
CNRY.cert %>% 
  mutate(biological_maturity = ifelse(x12 == "Y" | x11 == "Y" | x10 == "Y" | x9 == "Y" | x8 == "Y" | x7 == "Y" |
                                        x6 == "Y" | x5 == "Y" | x4_2 == "Y" | x4_1 == "Y", 1, 0)) -> CNRY.cert

# Functional maturity has already been calculated by Melissa in the doc when she was reading the slides.

#####BIOLOGICAL MATURITY#####

###glm fit###
fit.mat.glm.bio.CNRY <- glm (maturity ~ length, data = data.frame(length = CNRY.cert$length_cm, maturity = CNRY.cert$biological_maturity),
                             family = binomial(link ="logit"))
vector.bio.CNRY = c(fit.mat.glm.bio.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.CNRY <- vector.bio.CNRY[1] 
B_glm.bio.CNRY <- vector.bio.CNRY[2]
Lmat50.bio.CNRY = -(A_glm.bio.CNRY/B_glm.bio.CNRY)
Lmat50.bio.CNRY

cor(CNRY.cert$length_cm, CNRY.cert$biological_maturity)

sA.bio.CNRY <- summary (fit.mat.glm.bio.CNRY)$coef[1,2] 
sB.bio.CNRY <- summary (fit.mat.glm.bio.CNRY)$coef[2,2]    
r.bio.CNRY <-cor(CNRY.cert$length_cm, CNRY.cert$biological_maturity)
n <- sum(CNRY.cert$certainty)

#Variance estimator
deltamethod.bio.CNRY <- ((sA.bio.CNRY^2)/(B_glm.bio.CNRY^2))- ((2*A_glm.bio.CNRY*sA.bio.CNRY*sB.bio.CNRY*r.bio.CNRY)/(B_glm.bio.CNRY^3))+ (((A_glm.bio.CNRY^2)*(sB.bio.CNRY^2))/(B_glm.bio.CNRY^4))
deltamethod.bio.CNRY

### 95% Confidence Interval equation

con.bio.CNRY <- 1.96*(sqrt(deltamethod.bio.CNRY)/sqrt(n))
bio.CNRY_mat = c(Lmat50.bio.CNRY,con.bio.CNRY)



#####FUNCTIONAL MATURITY#####

# count number of skipped spawners
# simply doing functional minus biological isn't right, because an individual not all functionally immature but biologically mature 
# individuals are skip spawners; could also only be stage 4.1
# CNRY_skip <- sum(CNRY.cert$biological_maturity) - sum(CNRY.cert$functional_maturity)
CNRY_skip <- dim(subset(CNRY.cert, percent_atresia >=25))[1]

###glm fit###

fit.mat.glm.fun.CNRY <- glm (maturity ~ 1 + length, data <-data.frame(length = CNRY.cert$length_cm, maturity <- CNRY.cert$functional_maturity),
                             family = binomial(link ="logit"))
vector.fun.CNRY = c(fit.mat.glm.fun.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.CNRY <- vector.fun.CNRY[1] 
B_glm.fun.CNRY <- vector.fun.CNRY[2]
Lmat50.fun.CNRY = -(A_glm.fun.CNRY/B_glm.fun.CNRY)
Lmat50.fun.CNRY

cor(CNRY.cert$length_cm, CNRY.cert$functional_maturity)

sA.fun.CNRY <- summary (fit.mat.glm.fun.CNRY)$coef[1,2] 
sB.fun.CNRY <- summary (fit.mat.glm.fun.CNRY)$coef[2,2]    
r.fun.CNRY <-cor(CNRY.cert$length_cm, CNRY.cert$functional_maturity)
n <- sum(CNRY.cert$certainty)

#Variance estimator
deltamethod.fun.CNRY <- ((sA.fun.CNRY^2)/(B_glm.fun.CNRY^2))- ((2*A_glm.fun.CNRY*sA.fun.CNRY*sB.fun.CNRY*r.fun.CNRY)/(B_glm.fun.CNRY^3))+ (((A_glm.fun.CNRY^2)*(sB.fun.CNRY^2))/(B_glm.fun.CNRY^4))
deltamethod.fun.CNRY

### 95% Confidence Interval equation 

con.fun.CNRY <- 1.96*(sqrt(deltamethod.fun.CNRY)/sqrt(n))
fun.CNRY_mat = c(Lmat50.fun.CNRY,con.fun.CNRY)



#####MACRO MATURITY - ALL CODES#####

###glm fit###

fit.mat.glm.macro.CNRY <- glm (maturity ~ 1 + length, data <-data.frame(length = CNRY.cert$length_cm, maturity <- CNRY.cert$macro_maturity),
                               family = binomial(link ="logit"))
vector.macro.CNRY = c(fit.mat.glm.macro.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.CNRY <- vector.macro.CNRY[1] 
B_glm.macro.CNRY <- vector.macro.CNRY[2]
Lmat50.macro.CNRY = -(A_glm.macro.CNRY/B_glm.macro.CNRY)
Lmat50.macro.CNRY

cor(CNRY.cert$length_cm, CNRY.cert$macro_maturity)

sA.macro.CNRY <- summary (fit.mat.glm.macro.CNRY)$coef[1,2] 
sB.macro.CNRY <- summary (fit.mat.glm.macro.CNRY)$coef[2,2]    
r.macro.CNRY <-cor(CNRY.cert$length_cm, CNRY.cert$macro_maturity)
n <- sum(CNRY.cert$certainty)

#Variance estimator
deltamethod.macro.CNRY <- ((sA.macro.CNRY^2)/(B_glm.macro.CNRY^2))- ((2*A_glm.macro.CNRY*sA.macro.CNRY*sB.macro.CNRY*r.macro.CNRY)/(B_glm.macro.CNRY^3))+ (((A_glm.macro.CNRY^2)*(sB.macro.CNRY^2))/(B_glm.macro.CNRY^4))
deltamethod.macro.CNRY

### 95% Confidence Interval equation 

con.macro.CNRY <- 1.96*(sqrt(deltamethod.macro.CNRY)/sqrt(n))
macro.CNRY_mat = c(Lmat50.macro.CNRY,con.macro.CNRY)

### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.bio.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.fun.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.macro.CNRY,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



#####MACRO MATURITY - NO 7#####

CNRY.cert.no_7 <- subset(CNRY.cert, macro_maturity_code != 7)

###glm fit###

fit.mat.glm.macro_no7.CNRY <- glm (maturity ~ 1 + length, data <-data.frame(length = CNRY.cert.no_7$length_cm, maturity <- CNRY.cert.no_7$macro_maturity),
                                   family = binomial(link ="logit"))
vector.macro_no7.CNRY = c(fit.mat.glm.macro_no7.CNRY$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro_no7.CNRY <- vector.macro_no7.CNRY[1] 
B_glm.macro_no7.CNRY <- vector.macro_no7.CNRY[2]
Lmat50.macro_no7.CNRY = -(A_glm.macro_no7.CNRY/B_glm.macro_no7.CNRY)
Lmat50.macro_no7.CNRY

cor(CNRY.cert.no_7$length_cm, CNRY.cert.no_7$macro_maturity)

sA.macro_no7.CNRY <- summary (fit.mat.glm.macro_no7.CNRY)$coef[1,2] 
sB.macro_no7.CNRY <- summary (fit.mat.glm.macro_no7.CNRY)$coef[2,2]    
r.macro_no7.CNRY <-cor(CNRY.cert.no_7$length_cm, CNRY.cert.no_7$macro_maturity)
n <- sum(CNRY.cert.no_7$certainty)

#Variance estimator
deltamethod.macro_no7.CNRY <- ((sA.macro_no7.CNRY^2)/(B_glm.macro_no7.CNRY^2))- ((2*A_glm.macro_no7.CNRY*sA.macro_no7.CNRY*sB.macro_no7.CNRY*r.macro_no7.CNRY)/(B_glm.macro_no7.CNRY^3))+ (((A_glm.macro_no7.CNRY^2)*(sB.macro_no7.CNRY^2))/(B_glm.macro_no7.CNRY^4))
deltamethod.macro_no7.CNRY

### 95% Confidence Interval equation 

con.macro_no7.CNRY <- 1.96*(sqrt(deltamethod.macro_no7.CNRY)/sqrt(n))
macro_no7.CNRY_mat = c(Lmat50.macro_no7.CNRY,con.macro_no7.CNRY)


##### Get parameters for logistic maturity curves #####

# MACRO MATURITY #

# get length bins
bin_macro.CNRY <- as.numeric(as.character(as.data.frame(table(CNRY.cert$length_cm))$Var1))

# get proportion mature at each length bin
CNRY.cert %>% 
  group_by(length_cm) %>% 
  count(macro_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_macro = n/total_samples) -> CNRY.macro.counts 

CNRY.macro.counts[!rev(duplicated(rev(CNRY.macro.counts$length_cm))),] %>% 
  mutate(prop_macro = ifelse(macro_maturity == 0, 0, prop_macro)) -> CNRY.macro.props

prop_macro.CNRY <- CNRY.macro.props$prop_macro

# get number at each length bin
n_macro.CNRY <- as.data.frame(table(CNRY.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.macro.CNRY = as.data.frame(fit.mat.glm.macro.CNRY[1])
A_glm.macro.CNRY = df.fit.mat.glm.macro.CNRY$coefficients[1]
B_glm.macro.CNRY = df.fit.mat.glm.macro.CNRY$coefficients[2]



#####SUMMARIZE CANARY RESULTS#####

GLM.maturity.data = rbind(bio.CNRY_mat,fun.CNRY_mat,macro.CNRY_mat, macro_no7.CNRY_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
canary_maturity_results <- GLM.maturity.data
canary_maturity_results$species <- "Canary rockfish"
canary_maturity_results <- rownames_to_column(canary_maturity_results, "mat_type")
canary_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> canary_maturity_results

canary_maturity_results$alpha <- c(paste0(round(A_glm.bio.CNRY,2), " (", round(sA.bio.CNRY,2), ")"), paste0(round(A_glm.fun.CNRY,2), 
                                                                                                            " (", round(sA.fun.CNRY,2), ")"), paste0(round(A_glm.macro.CNRY,2), " (", round(sA.macro.CNRY, 2), ")"),
                                   # no 7
                                   paste0(round(A_glm.macro_no7.CNRY,2), " (", round(sA.macro_no7.CNRY, 2), ")"))
canary_maturity_results$beta <- c(paste0(round(B_glm.bio.CNRY,2), " (", round(sB.bio.CNRY,2), ")"), paste0(round(B_glm.fun.CNRY,2), 
                                                                                                           " (", round(sB.fun.CNRY,2), ")"), paste0(round(B_glm.macro.CNRY,2), " (", round(sB.macro.CNRY, 2), ")"),
                                  paste0(round(B_glm.macro_no7.CNRY,2), " (", round(sB.macro_no7.CNRY, 2), ")"))
# round values
canary_maturity_results$Lmat50 <- round(canary_maturity_results$Lmat50, 2)




# BIOLOGICAL MATURITY #

# get length bins
bin_bio.CNRY <- as.numeric(as.character(as.data.frame(table(CNRY.cert$length_cm))$Var1))

# get proportion mature at each length bin
CNRY.cert %>% 
  group_by(length_cm) %>% 
  count(biological_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_bio = n/total_samples) -> CNRY.bio.counts 

CNRY.bio.counts[!rev(duplicated(rev(CNRY.bio.counts$length_cm))),] %>% 
  mutate(prop_bio = ifelse(biological_maturity == 0, 0, prop_bio)) -> CNRY.bio.props

prop_bio.CNRY <- CNRY.bio.props$prop_bio

# get number at each length bin
n_bio.CNRY <- as.data.frame(table(CNRY.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.bio.CNRY = as.data.frame(fit.mat.glm.bio.CNRY[1])
A_glm.bio.CNRY = df.fit.mat.glm.bio.CNRY$coefficients[1]
B_glm.bio.CNRY = df.fit.mat.glm.bio.CNRY$coefficients[2]

# FUNCTIONAL MATURITY #

# get length bins
bin_fun.CNRY <- as.numeric(as.character(as.data.frame(table(CNRY.cert$length_cm))$Var1))

# get proportion mature at each length bin
CNRY.cert %>% 
  group_by(length_cm) %>% 
  count(functional_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_fun = n/total_samples) -> CNRY.fun.counts 

CNRY.fun.counts[!rev(duplicated(rev(CNRY.fun.counts$length_cm))),] %>% 
  mutate(prop_fun = ifelse(functional_maturity == 0, 0, prop_fun)) -> CNRY.fun.props

prop_fun.CNRY <- CNRY.fun.props$prop_fun

# get number at each length bin
n_fun.CNRY <- as.data.frame(table(CNRY.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.fun.CNRY = as.data.frame(fit.mat.glm.fun.CNRY[1])
A_glm.fun.CNRY = df.fit.mat.glm.fun.CNRY$coefficients[1]
B_glm.fun.CNRY = df.fit.mat.glm.fun.CNRY$coefficients[2]


##--Arrowtooth Flounder-------------------------------------------------------------

ARTH.mat<-read_excel(here("data", "2016_2017 ODFW Arrowtooth maturity reread.xlsx"))
ARTH.cert<-subset(ARTH.mat,Certainty==1) #subset only certain samples and those that were staged macroscopically#
ARTH.cert <- clean_names(ARTH.cert)
ARTH.cert <- subset(ARTH.cert, !(is.na(maturity_code)))
ARTH.cert <- clean_names(ARTH.cert)


# Move spent to stage 11 and resting/recovering/regenerating to stage 12
ARTH.cert %>% 
  dplyr::rename("x11" = "post_spawn", "x12" = "regenerating_recovering") %>% 
  # rename maturity_code as macro_maturity_code
  dplyr::rename(macro_maturity_code = maturity_code) -> ARTH.cert

# Compute macroscopic maturity (binary)
ARTH.cert %>% 
  mutate(macro_maturity = ifelse(macro_maturity_code %in% c(1,2), 0, 1)) -> ARTH.cert

# Re-calculate biological maturity (binary)
# If it has any mature histological stage (4.1 or higher), it's mature
ARTH.cert %>% 
  mutate(biological_maturity = ifelse(x12 == "Y" | x11 == "Y" | x10 == "Y" | x9 == "Y" | x8 == "Y" | x7 == "Y" |
                                        x6 == "Y" | x5 == "Y" | x4_2 == "Y" | x4_1 == "Y", 1, 0)) -> ARTH.cert



#####BIOLOGICAL MATURITY#####

###glm fit###
fit.mat.glm.bio.ARTH <- glm (maturity ~ length, data = data.frame(length = ARTH.cert$length_cm, maturity = ARTH.cert$biological_maturity),
                             family = binomial(link ="logit"))
vector.bio.ARTH = c(fit.mat.glm.bio.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio.ARTH <- vector.bio.ARTH[1] 
B_glm.bio.ARTH <- vector.bio.ARTH[2]
Lmat50.bio.ARTH = -(A_glm.bio.ARTH/B_glm.bio.ARTH)
Lmat50.bio.ARTH

cor(ARTH.cert$length_cm, ARTH.cert$biological_maturity)

sA.bio.ARTH <- summary (fit.mat.glm.bio.ARTH)$coef[1,2] 
sB.bio.ARTH <- summary (fit.mat.glm.bio.ARTH)$coef[2,2]    
r.bio.ARTH <-cor(ARTH.cert$length_cm, ARTH.cert$biological_maturity)
n <- sum(ARTH.cert$certainty)

#Variance estimator
deltamethod.bio.ARTH <- ((sA.bio.ARTH^2)/(B_glm.bio.ARTH^2))- ((2*A_glm.bio.ARTH*sA.bio.ARTH*sB.bio.ARTH*r.bio.ARTH)/(B_glm.bio.ARTH^3))+ (((A_glm.bio.ARTH^2)*(sB.bio.ARTH^2))/(B_glm.bio.ARTH^4))
deltamethod.bio.ARTH

### 95% Confidence Interval equation

con.bio.ARTH <- 1.96*(sqrt(deltamethod.bio.ARTH)/sqrt(n))
bio.ARTH_mat = c(Lmat50.bio.ARTH,con.bio.ARTH)



#####FUNCTIONAL MATURITY#####

# count number of skipped spawners
# simply doing functional minus biological isn't right, because an individual not all functionally immature but biologically mature 
# individuals are skip spawners; could also only be stage 4.1
# ARTH_skip <- sum(ARTH.cert$biological_maturity) - sum(ARTH.cert$functional_maturity)
ARTH_skip <- dim(subset(ARTH.cert, percent_atresia >=25))[1]

###glm fit###

fit.mat.glm.fun.ARTH <- glm (maturity ~ 1 + length, data <-data.frame(length = ARTH.cert$length_cm, maturity <- ARTH.cert$functional_maturity),
                             family = binomial(link ="logit"))
vector.fun.ARTH = c(fit.mat.glm.fun.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun.ARTH <- vector.fun.ARTH[1] 
B_glm.fun.ARTH <- vector.fun.ARTH[2]
Lmat50.fun.ARTH = -(A_glm.fun.ARTH/B_glm.fun.ARTH)
Lmat50.fun.ARTH

cor(ARTH.cert$length_cm, ARTH.cert$functional_maturity)

sA.fun.ARTH <- summary (fit.mat.glm.fun.ARTH)$coef[1,2] 
sB.fun.ARTH <- summary (fit.mat.glm.fun.ARTH)$coef[2,2]    
r.fun.ARTH <-cor(ARTH.cert$length_cm, ARTH.cert$functional_maturity)
n <- sum(ARTH.cert$certainty)

#Variance estimator
deltamethod.fun.ARTH <- ((sA.fun.ARTH^2)/(B_glm.fun.ARTH^2))- ((2*A_glm.fun.ARTH*sA.fun.ARTH*sB.fun.ARTH*r.fun.ARTH)/(B_glm.fun.ARTH^3))+ (((A_glm.fun.ARTH^2)*(sB.fun.ARTH^2))/(B_glm.fun.ARTH^4))
deltamethod.fun.ARTH

### 95% Confidence Interval equation 

con.fun.ARTH <- 1.96*(sqrt(deltamethod.fun.ARTH)/sqrt(n))
fun.ARTH_mat = c(Lmat50.fun.ARTH,con.fun.ARTH)



#####MACRO MATURITY - ALL CODES#####

###glm fit###

fit.mat.glm.macro.ARTH <- glm (maturity ~ 1 + length, data <-data.frame(length = ARTH.cert$length_cm, maturity <- ARTH.cert$macro_maturity),
                               family = binomial(link ="logit"))
vector.macro.ARTH = c(fit.mat.glm.macro.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.ARTH <- vector.macro.ARTH[1] 
B_glm.macro.ARTH <- vector.macro.ARTH[2]
Lmat50.macro.ARTH = -(A_glm.macro.ARTH/B_glm.macro.ARTH)
Lmat50.macro.ARTH

cor(ARTH.cert$length_cm, ARTH.cert$macro_maturity)

sA.macro.ARTH <- summary (fit.mat.glm.macro.ARTH)$coef[1,2] 
sB.macro.ARTH <- summary (fit.mat.glm.macro.ARTH)$coef[2,2]    
r.macro.ARTH <-cor(ARTH.cert$length_cm, ARTH.cert$macro_maturity)
n <- sum(ARTH.cert$certainty)

#Variance estimator
deltamethod.macro.ARTH <- ((sA.macro.ARTH^2)/(B_glm.macro.ARTH^2))- ((2*A_glm.macro.ARTH*sA.macro.ARTH*sB.macro.ARTH*r.macro.ARTH)/(B_glm.macro.ARTH^3))+ (((A_glm.macro.ARTH^2)*(sB.macro.ARTH^2))/(B_glm.macro.ARTH^4))
deltamethod.macro.ARTH

### 95% Confidence Interval equation 

con.macro.ARTH <- 1.96*(sqrt(deltamethod.macro.ARTH)/sqrt(n))
macro.ARTH_mat = c(Lmat50.macro.ARTH,con.macro.ARTH)


### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.bio.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.fun.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.macro.ARTH,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))




#####MACRO MATURITY - NO 7#####

ARTH.cert.no_7 <- subset(ARTH.cert, macro_maturity_code != 7)

###glm fit###

fit.mat.glm.macro_no7.ARTH <- glm (maturity ~ 1 + length, data <-data.frame(length = ARTH.cert.no_7$length_cm, maturity <- ARTH.cert.no_7$macro_maturity),
                                   family = binomial(link ="logit"))
vector.macro_no7.ARTH = c(fit.mat.glm.macro_no7.ARTH$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro_no7.ARTH <- vector.macro_no7.ARTH[1] 
B_glm.macro_no7.ARTH <- vector.macro_no7.ARTH[2]
Lmat50.macro_no7.ARTH = -(A_glm.macro_no7.ARTH/B_glm.macro_no7.ARTH)
Lmat50.macro_no7.ARTH

cor(ARTH.cert.no_7$length_cm, ARTH.cert.no_7$macro_maturity)

sA.macro_no7.ARTH <- summary (fit.mat.glm.macro_no7.ARTH)$coef[1,2] 
sB.macro_no7.ARTH <- summary (fit.mat.glm.macro_no7.ARTH)$coef[2,2]    
r.macro_no7.ARTH <-cor(ARTH.cert.no_7$length_cm, ARTH.cert.no_7$macro_maturity)
n <- sum(ARTH.cert.no_7$certainty)

#Variance estimator
deltamethod.macro_no7.ARTH <- ((sA.macro_no7.ARTH^2)/(B_glm.macro_no7.ARTH^2))- ((2*A_glm.macro_no7.ARTH*sA.macro_no7.ARTH*sB.macro_no7.ARTH*r.macro_no7.ARTH)/(B_glm.macro_no7.ARTH^3))+ (((A_glm.macro_no7.ARTH^2)*(sB.macro_no7.ARTH^2))/(B_glm.macro_no7.ARTH^4))
deltamethod.macro_no7.ARTH

### 95% Confidence Interval equation 

con.macro_no7.ARTH <- 1.96*(sqrt(deltamethod.macro_no7.ARTH)/sqrt(n))
macro_no7.ARTH_mat = c(Lmat50.macro_no7.ARTH,con.macro_no7.ARTH)


##### Get parameters for logistic maturity curves #####

# MACRO MATURITY #

# get length bins
bin_macro.ARTH <- as.numeric(as.character(as.data.frame(table(ARTH.cert$length_cm))$Var1))

# get proportion mature at each length bin
ARTH.cert %>% 
  group_by(length_cm) %>% 
  count(macro_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_macro = n/total_samples) -> ARTH.macro.counts 

ARTH.macro.counts[!rev(duplicated(rev(ARTH.macro.counts$length_cm))),] %>% 
  mutate(prop_macro = ifelse(macro_maturity == 0, 0, prop_macro)) -> ARTH.macro.props

prop_macro.ARTH <- ARTH.macro.props$prop_macro

# get number at each length bin
n_macro.ARTH <- as.data.frame(table(ARTH.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.macro.ARTH = as.data.frame(fit.mat.glm.macro.ARTH[1])
A_glm.macro.ARTH = df.fit.mat.glm.macro.ARTH$coefficients[1]
B_glm.macro.ARTH = df.fit.mat.glm.macro.ARTH$coefficients[2]

# BIOLOGICAL MATURITY #

# get length bins
bin_bio.ARTH <- as.numeric(as.character(as.data.frame(table(ARTH.cert$length_cm))$Var1))

# get proportion mature at each length bin
ARTH.cert %>% 
  group_by(length_cm) %>% 
  count(biological_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_bio = n/total_samples) -> ARTH.bio.counts 

ARTH.bio.counts[!rev(duplicated(rev(ARTH.bio.counts$length_cm))),] %>% 
  mutate(prop_bio = ifelse(biological_maturity == 0, 0, prop_bio)) -> ARTH.bio.props

prop_bio.ARTH <- ARTH.bio.props$prop_bio

# get number at each length bin
n_bio.ARTH <- as.data.frame(table(ARTH.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.bio.ARTH = as.data.frame(fit.mat.glm.bio.ARTH[1])
A_glm.bio.ARTH = df.fit.mat.glm.bio.ARTH$coefficients[1]
B_glm.bio.ARTH = df.fit.mat.glm.bio.ARTH$coefficients[2]

# FUNCTIONAL MATURITY #

# get length bins
bin_fun.ARTH <- as.numeric(as.character(as.data.frame(table(ARTH.cert$length_cm))$Var1))

# get proportion mature at each length bin
ARTH.cert %>% 
  group_by(length_cm) %>% 
  count(functional_maturity) %>% 
  mutate(total_samples = sum(n)) %>% 
  mutate(prop_fun = n/total_samples) -> ARTH.fun.counts 

ARTH.fun.counts[!rev(duplicated(rev(ARTH.fun.counts$length_cm))),] %>% 
  mutate(prop_fun = ifelse(functional_maturity == 0, 0, prop_fun)) -> ARTH.fun.props

prop_fun.ARTH <- ARTH.fun.props$prop_fun

# get number at each length bin
n_fun.ARTH <- as.data.frame(table(ARTH.cert$length_cm))$Freq

# get parameters
df.fit.mat.glm.fun.ARTH = as.data.frame(fit.mat.glm.fun.ARTH[1])
A_glm.fun.ARTH = df.fit.mat.glm.fun.ARTH$coefficients[1]
B_glm.fun.ARTH = df.fit.mat.glm.fun.ARTH$coefficients[2]




#####SUMMARIZE ARROWTOOTH RESULTS#####

GLM.maturity.data = rbind(bio.ARTH_mat,fun.ARTH_mat,macro.ARTH_mat, macro_no7.ARTH_mat)
colnames(GLM.maturity.data) = c("Lmat50", "plusminusCI95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
arrowtooth_maturity_results <- GLM.maturity.data
arrowtooth_maturity_results$species <- "Arrowtooth Flounder"
arrowtooth_maturity_results <- rownames_to_column(arrowtooth_maturity_results, "mat_type")
arrowtooth_maturity_results %>% 
  mutate(., CI_95 = paste0(round(Lmat50-plusminusCI95,2), "-", round(Lmat50+plusminusCI95,2))) -> arrowtooth_maturity_results

arrowtooth_maturity_results$alpha <- c(paste0(round(A_glm.bio.ARTH,2), " (", round(sA.bio.ARTH,2), ")"), paste0(round(A_glm.fun.ARTH,2), 
                                                                                                                " (", round(sA.fun.ARTH,2), ")"), paste0(round(A_glm.macro.ARTH,2), " (", round(sA.macro.ARTH, 2), ")"),
                                       paste0(round(A_glm.macro_no7.ARTH,2), " (", round(sA.macro_no7.ARTH, 2), ")"))
arrowtooth_maturity_results$beta <- c(paste0(round(B_glm.bio.ARTH,2), " (", round(sB.bio.ARTH,2), ")"), paste0(round(B_glm.fun.ARTH,2), 
                                                                                                               " (", round(sB.fun.ARTH,2), ")"), paste0(round(B_glm.macro.ARTH,2), " (", round(sB.macro.ARTH, 2), ")"),
                                      paste0(round(B_glm.macro_no7.ARTH,2), " (", round(sB.macro_no7.ARTH, 2), ")"))

# round values
arrowtooth_maturity_results$Lmat50 <- round(arrowtooth_maturity_results$Lmat50, 2)


##--Create combined logistic maturity curve plot----------------------------------------------------------

setEPS()
postscript(here("figures", "Fig1_v3.eps"), height = 8, width = 24)
par(mfrow = c(1,3), xpd=TRUE,mar = c("bottom" = 1, "left" = 1, "top" = 1, "right" = 1))

###### Plot CNRY ######
par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(25,60),ylim=c(-0.01,1.1),xlab="length_cm (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.CNRY, y = prop_macro.CNRY,cex=sqrt(n_macro.CNRY), pch=16, col = "#984ea3") # macro
points(x = bin_bio.CNRY, y = prop_bio.CNRY,cex=sqrt(n_bio.CNRY), pch=16, col = "#377eb8") # bio
points(x = bin_fun.CNRY, y = prop_fun.CNRY,cex=sqrt(n_fun.CNRY), pch=16, col = "#4daf4a") # fun


lines(25:60, 1/(1+(exp(-(A_glm.macro.CNRY+(B_glm.macro.CNRY*(25:60)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3) # Macro
lines(25:60, 1/(1+(exp(-(A_glm.bio.CNRY+(B_glm.bio.CNRY*(25:60)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3) # Bio
lines(25:60, 1/(1+(exp(-(A_glm.fun.CNRY+(B_glm.fun.CNRY*(25:60)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)# fun

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
plot(0, type ='n', xlim=c(35,75),ylim=c(-0.01,1.1),xlab="length_cm (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.SABL, y = prop_macro.SABL,cex=sqrt(n_macro.SABL), pch=16, col = "#984ea3") # macro
points(x = bin_bio.SABL, y = prop_bio.SABL,cex=sqrt(n_bio.SABL), pch=16, col = "#377eb8") # bio
points(x = bin_fun.SABL, y = prop_fun.SABL,cex=sqrt(n_fun.SABL), pch=16, col = "#4daf4a") # fun


lines(35:75, 1/(1+(exp(-(A_glm.macro.SABL+(B_glm.macro.SABL*(35:75)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3)# Macro
lines(35:75, 1/(1+(exp(-(A_glm.bio.SABL+(B_glm.bio.SABL*(35:75)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3)# bio
lines(35:75, 1/(1+(exp(-(A_glm.fun.SABL+(B_glm.fun.SABL*(35:75)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)# fun

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
plot(0, type ='n', xlim=c(20,75),ylim=c(-0.01,1.1),xlab="length_cm (cm)",ylab="Proportion mature", cex.lab = 2.4, cex.axis = 1.8)

points(x = bin_macro.ARTH, y = prop_macro.ARTH,cex=sqrt(n_macro.ARTH), pch=16, col = "#984ea3") # macro
points(x = bin_bio.ARTH, y = prop_bio.ARTH,cex=sqrt(n_bio.ARTH), pch=16, col = "#377eb8") # bio
points(x = bin_fun.ARTH, y = prop_fun.ARTH,cex=sqrt(n_fun.ARTH), pch=16, col = "#4daf4a") # fun



lines(20:75, 1/(1+(exp(-(A_glm.macro.ARTH+(B_glm.macro.ARTH*(20:75)))))), type = "l", col="#984ea3", pch = 19, lty =1,lwd = 3)# Macro
lines(20:75, 1/(1+(exp(-(A_glm.bio.ARTH+(B_glm.bio.ARTH*(20:75)))))), type = "l", col="#377eb8", pch = 19, lty =1,lwd = 3)# bio
lines(20:75, 1/(1+(exp(-(A_glm.fun.ARTH+(B_glm.fun.ARTH*(20:75)))))), type = "l", col="#4daf4a", pch = 19, lty =1,lwd = 3)# fun

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

#####--Store GLM parameters to report in table--#####

## Join all species for logistic maturity parameters, export
canary_maturity_results %>% 
  bind_rows(., sablefish_maturity_results) %>% 
  bind_rows(., arrowtooth_maturity_results) -> full_maturity_results

write.csv(full_maturity_results, here("tables", "logistic_parameters_table.csv"), row.names = FALSE)

# count number of skip spawners
SABL_skip
CNRY_skip
ARTH_skip