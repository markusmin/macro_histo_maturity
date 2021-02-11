### Plotting changes in L50 for canary rockfish and sablefish ###

# This code compares historical L50 values and produces figures 2 and 3

library(tidyverse)
library(here)

##--Canary-------------------------------------------------------------------

# Read data
CNRY.mat.1<-read.csv(here("data", "CNRY_O_1980_1984_11_3_no7_no8.csv"))
Data.in.1<-CNRY.mat.1
CNRY.mat.2<-read.csv(here("data", "CNRY_O_2010_2018_no7_11_3.csv"))
Data.in.2<-CNRY.mat.2

### Time Period 1 ###

###glm fit###

fit.mat.glm.macro.1 <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in.1$Length, maturity <- Data.in.1$Macro_maturity),
                            family = binomial(link ="logit"))
vector.macro.1 = c(fit.mat.glm.macro.1$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.1 <- vector.macro.1[1] 
B_glm.macro.1 <- vector.macro.1[2]
Lmat50.macro.1 = -(A_glm.macro.1/B_glm.macro.1)
Lmat50.macro.1

cor(Data.in.1$Length, Data.in.1$Macro_maturity)

sA.macro.1 <- summary (fit.mat.glm.macro.1)$coef[1,2] 
sB.macro.1 <- summary (fit.mat.glm.macro.1)$coef[2,2]    
r.macro.1 <-cor(Data.in.1$Length, Data.in.1$Macro_maturity)
n.1 <- length(Data.in.1$Length)

#Variance estimator
deltamethod.macro.1 <- ((sA.macro.1^2)/(B_glm.macro.1^2))- ((2*A_glm.macro.1*sA.macro.1*sB.macro.1*r.macro.1)/(B_glm.macro.1^3))+ (((A_glm.macro.1^2)*(sB.macro.1^2))/(B_glm.macro.1^4))
deltamethod.macro.1

## 95% Confidence Interval equation

con.macro.1 <- 1.96*(sqrt(deltamethod.macro.1)/sqrt(n.1))
macro_mat.1 = c(Lmat50.macro.1,con.macro.1)



### Time Period 2 ###

###glm fit###

fit.mat.glm.macro.2 <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in.2$Length, maturity <- Data.in.2$Macro_maturity),
                            family = binomial(link ="logit"))
vector.macro.2 = c(fit.mat.glm.macro.2$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.2 <- vector.macro.2[1] 
B_glm.macro.2 <- vector.macro.2[2]
Lmat50.macro.2 = -(A_glm.macro.2/B_glm.macro.2)
Lmat50.macro.2

cor(Data.in.2$Length, Data.in.2$Macro_maturity)

sA.macro.2 <- summary (fit.mat.glm.macro.2)$coef[1,2] 
sB.macro.2 <- summary (fit.mat.glm.macro.2)$coef[2,2]    
r.macro.2 <-cor(Data.in.2$Length, Data.in.2$Macro_maturity)
n.2 <- length(Data.in.2$Length)

#Variance estimator
deltamethod.macro.2 <- ((sA.macro.2^2)/(B_glm.macro.2^2))- ((2*A_glm.macro.2*sA.macro.2*sB.macro.2*r.macro.2)/(B_glm.macro.2^3))+ (((A_glm.macro.2^2)*(sB.macro.2^2))/(B_glm.macro.2^4))
deltamethod.macro.2

## 95% Confidence Interval equation

con.macro.2 <- 1.96*(sqrt(deltamethod.macro.2)/sqrt(n.2))
macro_mat.2 = c(Lmat50.macro.2,con.macro.2)

### RESULTS ###

GLM.maturity.data = rbind(macro_mat.1,macro_mat.2)
colnames(GLM.maturity.data) = c("Lmat50", "95% CI +/-")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
GLM.maturity.data





### CREATE PLOT
setEPS()
postscript(here("figures", "Fig2.eps"), height = 8, width = 8)

par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(20,70),ylim=c(-0.01,1.05),xlab="Length (cm)",ylab="Proportion mature", cex.lab = 1.6, cex.axis = 1.3)

#Points and Line
# CNRY.mat.1<-read.csv(here("data", "CNRY_O_1980_1984_11_3.csv"))
# Data.in.1<-CNRY.mat.1
# 
# CNRY.mat.2<-read.csv(here("data", "CNRY_O_2010_2018_no7_11_3.csv"))
# Data.in.2<-CNRY.mat.2

# Time Period 1 #
bin_m.1 <- Data.in.1$size
prop_m.1 <- Data.in.1$prop_macro
n_m.1 <- Data.in.1$N
fit.mat.glm.macro.1[1]
df.fit.mat.glm.macro.1 = as.data.frame(fit.mat.glm.macro.1[1])
A_glm.macro.1 = df.fit.mat.glm.macro.1$coefficients[1]
B_glm.macro.1 = df.fit.mat.glm.macro.1$coefficients[2]

# Time Period 2 #
bin_m.2 <- Data.in.2$size
prop_m.2 <- Data.in.2$prop_macro
n_m.2 <- Data.in.2$N
fit.mat.glm.macro.2[1]
df.fit.mat.glm.macro.2 = as.data.frame(fit.mat.glm.macro.2[1])
A_glm.macro.2 = df.fit.mat.glm.macro.2$coefficients[1]
B_glm.macro.2 = df.fit.mat.glm.macro.2$coefficients[2]

# points(x = bin_m.1, y = prop_m.1,cex=0.7*sqrt(n_m.1), pch=16, col = rgb(1,0,0,0.2))
# points(x = bin_m.2, y = prop_m.2,cex=0.7*sqrt(n_m.2), pch=16, col = rgb(0,0,1,0.2))
points(x = bin_m.1, y = prop_m.1,cex=0.7*sqrt(n_m.1), pch=16, col = "#f4a582")
points(x = bin_m.2, y = prop_m.2,cex=0.7*sqrt(n_m.2), pch=16, col = "#92c5de")


lines(20:70, 1/(1+(exp(-(A_glm.macro.1+(B_glm.macro.1*(20:70)))))), type = "l", col='#b2182b', pch = 19, lty =1,lwd = 3)####Time Period 1#####
lines(20:70, 1/(1+(exp(-(A_glm.macro.2+(B_glm.macro.2*(20:70)))))), type = "l", col='#2166ac', pch = 19, lty =1,lwd = 3)####Time Period 2#####

# GLM Confidence Bands #

plotdat <- data.frame(length=(20:70))
predat.1 <- predict(fit.mat.glm.macro.1,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(20:70,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#b2182b"))
with(predat.1, lines(20:70,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#b2182b"))

predat.2 <- predict(fit.mat.glm.macro.2,newdata=plotdat, se.fit=TRUE)
with(predat.2, lines(20:70,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#2166ac"))
with(predat.2, lines(20:70,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#2166ac"))


# L50 line #
segments(18,0.5,72,0.5,lty=5)

#Legend
legend.a <- c(5, 10,     
              15, 20)

legend.cex <- 0.7*sqrt(legend.a)

legend(20 ,1.1,legend=legend.a,pch=19,col="gray20",
       cex = 1.5, pt.cex=legend.cex,ncol=1,title ="N",bty='n', y.intersp = 1.3)


legend(54,0.1, legend=c("1980-1984","2010-2018"), col=c("#b2182b", "#2166ac"), lwd=c(3,3),
       lty= c(1,1,1), cex=1.5, bty="n")

dev.off()


##--Sablefish-------------------------------------------------------------------

# Read data
SABL.mat.1<-read.csv(here("data", "SABL_O_1995_2000_no7_no8_10_2_edit.csv"))
Data.in.1<-SABL.mat.1
SABL.mat.2<-read.csv(here("data", "SABL_O_2010_2018_no7_10_2.csv"))
Data.in.2<-SABL.mat.2

### Time Period 1 ###

###glm fit###

fit.mat.glm.macro.1 <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in.1$Length, maturity <- Data.in.1$Macro_maturity),
                            family = binomial(link ="logit"))
vector.macro.1 = c(fit.mat.glm.macro.1$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.1 <- vector.macro.1[1] 
B_glm.macro.1 <- vector.macro.1[2]
Lmat50.macro.1 = -(A_glm.macro.1/B_glm.macro.1)
Lmat50.macro.1

cor(Data.in.1$Length, Data.in.1$Macro_maturity)

sA.macro.1 <- summary (fit.mat.glm.macro.1)$coef[1,2] 
sB.macro.1 <- summary (fit.mat.glm.macro.1)$coef[2,2]    
r.macro.1 <-cor(Data.in.1$Length, Data.in.1$Macro_maturity)
n.1 <- length(Data.in.1$Length)

#Variance estimator
deltamethod.macro.1 <- ((sA.macro.1^2)/(B_glm.macro.1^2))- ((2*A_glm.macro.1*sA.macro.1*sB.macro.1*r.macro.1)/(B_glm.macro.1^3))+ (((A_glm.macro.1^2)*(sB.macro.1^2))/(B_glm.macro.1^4))
deltamethod.macro.1

## 95% Confidence Interval equation

con.macro.1 <- 1.96*(sqrt(deltamethod.macro.1)/sqrt(n.1))
macro_mat.1 = c(Lmat50.macro.1,con.macro.1)



### Time Period 2 ###

###glm fit###

fit.mat.glm.macro.2 <- glm (maturity ~ 1 + length, data <-data.frame(length = Data.in.2$Length, maturity <- Data.in.2$Macro_maturity),
                            family = binomial(link ="logit"))
vector.macro.2 = c(fit.mat.glm.macro.2$coefficients)

# A_glm = intercept
# B_glm = slope

A_glm.macro.2 <- vector.macro.2[1] 
B_glm.macro.2 <- vector.macro.2[2]
Lmat50.macro.2 = -(A_glm.macro.2/B_glm.macro.2)
Lmat50.macro.2

cor(Data.in.2$Length, Data.in.2$Macro_maturity)

sA.macro.2 <- summary (fit.mat.glm.macro.2)$coef[1,2] 
sB.macro.2 <- summary (fit.mat.glm.macro.2)$coef[2,2]    
r.macro.2 <-cor(Data.in.2$Length, Data.in.2$Macro_maturity)
n.2 <- length(Data.in.2$Length)

#Variance estimator
deltamethod.macro.2 <- ((sA.macro.2^2)/(B_glm.macro.2^2))- ((2*A_glm.macro.2*sA.macro.2*sB.macro.2*r.macro.2)/(B_glm.macro.2^3))+ (((A_glm.macro.2^2)*(sB.macro.2^2))/(B_glm.macro.2^4))
deltamethod.macro.2

## 95% Confidence Interval equation

con.macro.2 <- 1.96*(sqrt(deltamethod.macro.2)/sqrt(n.2))
macro_mat.2 = c(Lmat50.macro.2,con.macro.2)

### RESULTS ###

GLM.maturity.data = rbind(macro_mat.1,macro_mat.2)
colnames(GLM.maturity.data) = c("Lmat50", "95% CI +/-")
GLM.maturity.data = as.data.frame(GLM.maturity.data)
GLM.maturity.data





### CREATE PLOT
setEPS()
postscript(here("figures", "Fig3.eps"), height = 8, width = 8)

par(mfrow=c(1,1))
par(xpd=TRUE,mar = c("bottom" = 5, "left" = 5, "top" = 1, "right" = 1))
plot(0, type ='n', xlim=c(30,85),ylim=c(-0.01,1.05),xlab="Length (cm)",ylab="Proportion mature", cex.lab = 1.6, cex.axis = 1.3)

# Time Period 1 #
bin_m.1 <- Data.in.1$size
prop_m.1 <- Data.in.1$prop_macro
n_m.1 <- Data.in.1$N
fit.mat.glm.macro.1[1]
df.fit.mat.glm.macro.1 = as.data.frame(fit.mat.glm.macro.1[1])
A_glm.macro.1 = df.fit.mat.glm.macro.1$coefficients[1]
B_glm.macro.1 = df.fit.mat.glm.macro.1$coefficients[2]

# Time Period 2 #
bin_m.2 <- Data.in.2$size
prop_m.2 <- Data.in.2$prop_macro
n_m.2 <- Data.in.2$N
fit.mat.glm.macro.2[1]
df.fit.mat.glm.macro.2 = as.data.frame(fit.mat.glm.macro.2[1])
A_glm.macro.2 = df.fit.mat.glm.macro.2$coefficients[1]
B_glm.macro.2 = df.fit.mat.glm.macro.2$coefficients[2]

# points(x = bin_m.1, y = prop_m.1,cex=0.7*sqrt(n_m.1), pch=16, col = rgb(1,0,0,0.2))
# points(x = bin_m.2, y = prop_m.2,cex=0.7*sqrt(n_m.2), pch=16, col = rgb(0,0,1,0.2))
points(x = bin_m.1, y = prop_m.1,cex=0.7*sqrt(n_m.1), pch=16, col = "#f4a582")
points(x = bin_m.2, y = prop_m.2,cex=0.7*sqrt(n_m.2), pch=16, col = "#92c5de")


lines(30:85, 1/(1+(exp(-(A_glm.macro.1+(B_glm.macro.1*(30:85)))))), type = "l", col="#b2182b", pch = 19, lty =1,lwd = 3)####Time Period 1#####
lines(30:85, 1/(1+(exp(-(A_glm.macro.2+(B_glm.macro.2*(30:85)))))), type = "l", col="#2166ac", pch = 19, lty =1,lwd = 3)####Time Period 2#####

# GLM Confidence Bands #
plotdat <- data.frame(length=(30:80))
predat.1 <- predict(fit.mat.glm.macro.1,newdata=plotdat, se.fit=TRUE)
with(predat.1, lines(30:80,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#b2182b"))
with(predat.1, lines(30:80,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#b2182b"))

predat.2 <- predict(fit.mat.glm.macro.2,newdata=plotdat, se.fit=TRUE)
with(predat.2, lines(30:80,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)),lty=2,col="#2166ac"))
with(predat.2, lines(30:80,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)),lty=2,col="#2166ac"))


#Legend
legend.a <- c(10, 20, 30)

legend.cex <- 0.7*sqrt(legend.a)

legend(30 ,1.1,legend=legend.a,pch=19,col="gray20",
       cex = 1.5, pt.cex=legend.cex,ncol=1,title ="N",bty='n', y.intersp = 1.3)

# legend(30,1.01,legend="20",pch=19,col="gray20",cex = 1, pt.cex=0.7*sqrt(20),ncol=1,bty='n')
# legend(30,0.95,legend="30",pch=19,col="gray20",cex = 1, pt.cex=0.7*sqrt(30),ncol=1,bty='n')

legend(67.5,0.1, legend=c("1995-2000","2010-2018"), col=c("#b2182b", "#2166ac"), lwd=c(3,3),
       lty= c(1,1,1), cex=1.5, bty="n")


# # L50 line intercept #
segments(28,0.5,87,0.5,lty=5)



dev.off()

