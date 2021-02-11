### Fitting logistic curves to maturity ###

# This code calculates L50 for all three species and produces Figure 1

library(tidyverse)
library(here)

##--Sablefish-------------------------------------------------------------------

SABL.mat<-read.csv(here("data", "sablefish_maturity.csv"))
SABL.cert<-subset(SABL.mat,Certainty==1) #subset only certain sammples#
Data.in<-SABL.cert

### BIOLOGICAL MATURITY ###

###glm fit###

fit.mat.glm.bio <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                        family = binomial(link ="logit"))
vector.bio = c(fit.mat.glm.bio$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio <- vector.bio[1] 
B_glm.bio <- vector.bio[2]
Lmat50.bio = -(A_glm.bio/B_glm.bio)
Lmat50.bio

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio <- summary (fit.mat.glm.bio)$coef[1,2] 
sB.bio <- summary (fit.mat.glm.bio)$coef[2,2]    
r.bio <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio <- ((sA.bio^2)/(B_glm.bio^2))- ((2*A_glm.bio*sA.bio*sB.bio*r.bio)/(B_glm.bio^3))+ (((A_glm.bio^2)*(sB.bio^2))/(B_glm.bio^4))
deltamethod.bio

### 95% Confidence Interval equation

con.bio <- 1.96*(sqrt(deltamethod.bio)/sqrt(n))
bio_mat = c(Lmat50.bio,con.bio)



### FUNCTIONAL MATURITY ###

###glm fit###

fit.mat.glm.fun <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                        family = binomial(link ="logit"))
vector.fun = c(fit.mat.glm.fun$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun <- vector.fun[1] 
B_glm.fun <- vector.fun[2]
Lmat50.fun = -(A_glm.fun/B_glm.fun)
Lmat50.fun

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun <- summary (fit.mat.glm.fun)$coef[1,2] 
sB.fun <- summary (fit.mat.glm.fun)$coef[2,2]    
r.fun <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun <- ((sA.fun^2)/(B_glm.fun^2))- ((2*A_glm.fun*sA.fun*sB.fun*r.fun)/(B_glm.fun^3))+ (((A_glm.fun^2)*(sB.fun^2))/(B_glm.fun^4))
deltamethod.fun

### 95% Confidence Interval equation 

con.fun <- 1.96*(sqrt(deltamethod.fun)/sqrt(n))
fun_mat = c(Lmat50.fun,con.fun)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                          family = binomial(link ="logit"))
vector.macro = c(fit.mat.glm.macro$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro <- vector.macro[1] 
B_glm.macro <- vector.macro[2]
Lmat50.macro = -(A_glm.macro/B_glm.macro)
Lmat50.macro

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro <- summary (fit.mat.glm.macro)$coef[1,2] 
sB.macro <- summary (fit.mat.glm.macro)$coef[2,2]    
r.macro <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro <- ((sA.macro^2)/(B_glm.macro^2))- ((2*A_glm.macro*sA.macro*sB.macro*r.macro)/(B_glm.macro^3))+ (((A_glm.macro^2)*(sB.macro^2))/(B_glm.macro^4))
deltamethod.macro

##### 95% Confidence Interval equation#######

con.macro <- 1.96*(sqrt(deltamethod.macro)/sqrt(n))
macro_mat = c(Lmat50.macro,con.macro)

### RESULTS ###

GLM.maturity.data = rbind(bio_mat,fun_mat,macro_mat)
colnames(GLM.maturity.data) = c("Lmat50", "CI_95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
sablefish_maturity_results <- GLM.maturity.data
sablefish_maturity_results$species <- "Sablefish"
sablefish_maturity_results <- rownames_to_column(sablefish_maturity_results, "mat_type")


### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.bio,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.fun,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_sablefish <- c(as.numeric(predict.glm(fit.mat.glm.macro,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



##--Canary Rockfish-------------------------------------------------------------

CNRY.mat<-read.csv(here("data", "canary_maturity.csv"))
CNRY.cert<-subset(CNRY.mat,Certainty==1) #subset only certain sammples#
Data.in<-CNRY.cert

### BIOLOGICAL MATURITY ###

###glm fit###

fit.mat.glm.bio <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                        family = binomial(link ="logit"))
vector.bio = c(fit.mat.glm.bio$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio <- vector.bio[1] 
B_glm.bio <- vector.bio[2]
Lmat50.bio = -(A_glm.bio/B_glm.bio)
Lmat50.bio

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio <- summary (fit.mat.glm.bio)$coef[1,2] 
sB.bio <- summary (fit.mat.glm.bio)$coef[2,2]    
r.bio <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio <- ((sA.bio^2)/(B_glm.bio^2))- ((2*A_glm.bio*sA.bio*sB.bio*r.bio)/(B_glm.bio^3))+ (((A_glm.bio^2)*(sB.bio^2))/(B_glm.bio^4))
deltamethod.bio

### 95% Confidence Interval equation

con.bio <- 1.96*(sqrt(deltamethod.bio)/sqrt(n))
bio_mat = c(Lmat50.bio,con.bio)



### FUNCTIONAL MATURITY ###

###glm fit###

fit.mat.glm.fun <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                        family = binomial(link ="logit"))
vector.fun = c(fit.mat.glm.fun$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun <- vector.fun[1] 
B_glm.fun <- vector.fun[2]
Lmat50.fun = -(A_glm.fun/B_glm.fun)
Lmat50.fun

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun <- summary (fit.mat.glm.fun)$coef[1,2] 
sB.fun <- summary (fit.mat.glm.fun)$coef[2,2]    
r.fun <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun <- ((sA.fun^2)/(B_glm.fun^2))- ((2*A_glm.fun*sA.fun*sB.fun*r.fun)/(B_glm.fun^3))+ (((A_glm.fun^2)*(sB.fun^2))/(B_glm.fun^4))
deltamethod.fun

### 95% Confidence Interval equation 

con.fun <- 1.96*(sqrt(deltamethod.fun)/sqrt(n))
fun_mat = c(Lmat50.fun,con.fun)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                          family = binomial(link ="logit"))
vector.macro = c(fit.mat.glm.macro$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro <- vector.macro[1] 
B_glm.macro <- vector.macro[2]
Lmat50.macro = -(A_glm.macro/B_glm.macro)
Lmat50.macro

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro <- summary (fit.mat.glm.macro)$coef[1,2] 
sB.macro <- summary (fit.mat.glm.macro)$coef[2,2]    
r.macro <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro <- ((sA.macro^2)/(B_glm.macro^2))- ((2*A_glm.macro*sA.macro*sB.macro*r.macro)/(B_glm.macro^3))+ (((A_glm.macro^2)*(sB.macro^2))/(B_glm.macro^4))
deltamethod.macro

##### 95% Confidence Interval equation#######

con.macro <- 1.96*(sqrt(deltamethod.macro)/sqrt(n))
macro_mat = c(Lmat50.macro,con.macro)

### RESULTS ###

GLM.maturity.data = rbind(bio_mat,fun_mat,macro_mat)
colnames(GLM.maturity.data) = c("Lmat50", "CI_95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
canary_maturity_results <- GLM.maturity.data
canary_maturity_results$species <- "Canary"
canary_maturity_results <- rownames_to_column(canary_maturity_results, "mat_type")


### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.bio,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.fun,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_canary <- c(as.numeric(predict.glm(fit.mat.glm.macro,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



##--Arrowtooth Flounder-------------------------------------------------------------

ARTH.mat<-read.csv(here("data", "arrowtooth_maturity.csv"))
ARTH.cert<-subset(ARTH.mat,Certainty==1) #subset only certain sammples#
Data.in<-ARTH.cert

### BIOLOGICAL MATURITY ###

###glm fit###

fit.mat.glm.bio <- glm (maturity ~ length, data = data.frame(length = Data.in$Length, maturity = Data.in$Biological_maturity),
                        family = binomial(link ="logit"))
vector.bio = c(fit.mat.glm.bio$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.bio <- vector.bio[1] 
B_glm.bio <- vector.bio[2]
Lmat50.bio = -(A_glm.bio/B_glm.bio)
Lmat50.bio

cor(Data.in$Length, Data.in$Biological_maturity)

sA.bio <- summary (fit.mat.glm.bio)$coef[1,2] 
sB.bio <- summary (fit.mat.glm.bio)$coef[2,2]    
r.bio <-cor(Data.in$Length, Data.in$Biological_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.bio <- ((sA.bio^2)/(B_glm.bio^2))- ((2*A_glm.bio*sA.bio*sB.bio*r.bio)/(B_glm.bio^3))+ (((A_glm.bio^2)*(sB.bio^2))/(B_glm.bio^4))
deltamethod.bio

### 95% Confidence Interval equation

con.bio <- 1.96*(sqrt(deltamethod.bio)/sqrt(n))
bio_mat = c(Lmat50.bio,con.bio)



### FUNCTIONAL MATURITY ###

###glm fit###

fit.mat.glm.fun <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Functional_maturity),
                        family = binomial(link ="logit"))
vector.fun = c(fit.mat.glm.fun$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.fun <- vector.fun[1] 
B_glm.fun <- vector.fun[2]
Lmat50.fun = -(A_glm.fun/B_glm.fun)
Lmat50.fun

cor(Data.in$Length, Data.in$Functional_maturity)

sA.fun <- summary (fit.mat.glm.fun)$coef[1,2] 
sB.fun <- summary (fit.mat.glm.fun)$coef[2,2]    
r.fun <-cor(Data.in$Length, Data.in$Functional_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.fun <- ((sA.fun^2)/(B_glm.fun^2))- ((2*A_glm.fun*sA.fun*sB.fun*r.fun)/(B_glm.fun^3))+ (((A_glm.fun^2)*(sB.fun^2))/(B_glm.fun^4))
deltamethod.fun

### 95% Confidence Interval equation 

con.fun <- 1.96*(sqrt(deltamethod.fun)/sqrt(n))
fun_mat = c(Lmat50.fun,con.fun)



### MACRO MATURITY ###

###glm fit###

fit.mat.glm.macro <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in$Length, maturity <- Data.in$Macro_maturity),
                          family = binomial(link ="logit"))
vector.macro = c(fit.mat.glm.macro$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro <- vector.macro[1] 
B_glm.macro <- vector.macro[2]
Lmat50.macro = -(A_glm.macro/B_glm.macro)
Lmat50.macro

cor(Data.in$Length, Data.in$Macro_maturity)

sA.macro <- summary (fit.mat.glm.macro)$coef[1,2] 
sB.macro <- summary (fit.mat.glm.macro)$coef[2,2]    
r.macro <-cor(Data.in$Length, Data.in$Macro_maturity)
n <- sum(Data.in$Certainty)

#Variance estimator
deltamethod.macro <- ((sA.macro^2)/(B_glm.macro^2))- ((2*A_glm.macro*sA.macro*sB.macro*r.macro)/(B_glm.macro^3))+ (((A_glm.macro^2)*(sB.macro^2))/(B_glm.macro^4))
deltamethod.macro

##### 95% Confidence Interval equation#######

con.macro <- 1.96*(sqrt(deltamethod.macro)/sqrt(n))
macro_mat = c(Lmat50.macro,con.macro)

### RESULTS ###

GLM.maturity.data = rbind(bio_mat,fun_mat,macro_mat)
colnames(GLM.maturity.data) = c("Lmat50", "CI_95")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
arrowtooth_maturity_results <- GLM.maturity.data
arrowtooth_maturity_results$species <- "Arrowtooth"
arrowtooth_maturity_results <- rownames_to_column(arrowtooth_maturity_results, "mat_type")

### Get maturity parameters for use in stock assessment model

# Change seq() to match length bins in data file

# Biological maturity 
bio_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.bio,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Functional maturity
fun_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.fun,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))

# Macroscopic maturity
mac_params_arrowtooth <- c(as.numeric(predict.glm(fit.mat.glm.macro,type=c("response"),newdata=data.frame(length=c(seq(12,80,2)))),row.names=FALSE))



##--Join results, plot----------------------------------------------------------

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

