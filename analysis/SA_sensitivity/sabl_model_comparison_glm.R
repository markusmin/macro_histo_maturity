## SABL ##

library(r4ss)
library(gplots)
library(tidyverse)

dir.plots<-"/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/sablefish/sablefish_plots"
BC<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/sablefish/sablefish_reference_model",ncols=210)
BC.M1<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/sablefish/sablefish_bio",covar=FALSE,ncols=210)
BC.M2<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/sablefish/sablefish_fun",covar=FALSE,ncols=210)
BC.M3<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/sablefish/sablefish_macro",covar=FALSE,ncols=210)

spr.series.ref = BC$sprseries
spr.series.bio = BC.M1$sprseries
spr.series.fun = BC.M2$sprseries
spr.series.mac = BC.M3$sprseries

mod.names <- c("Base model","Biological","Functional","Macro")
mod.cols  <- rich.colors(length(mod.names))

base.summary <- SSsummarize(list(BC,BC.M1,BC.M2,BC.M3))

# Compare absolute spawning biomass by maturity type
spbio_comp_sabl <- as.data.frame(base.summary$SpawnBio)
colnames(spbio_comp_sabl) <- c("ref", "bio", "fun", "macro", "label", "year")
# Add column for comparisons between macro maturity and other maturity types
spbio_comp_sabl %>% 
  mutate(., macro_v_bio = macro/bio) %>% 
  mutate(., macro_v_fun = macro/fun) %>% 
  mutate(., macro_v_ref = macro/ref) -> spbio_comp_sabl

# Compare biological and functional
spbio_comp_sabl %>% 
  mutate(., fun_v_bio = fun/bio) -> spbio_comp_sabl

# compare in 2015
subset(spbio_comp_sabl, year == 2015)

dev.off()
SSplotComparisons(base.summary, plot=TRUE, print=TRUE, plotdir=dir.plots,
                  spacepoints=20,  # years between points on each line
                  initpoint=0,     # "first" year of points (modular arithmetic)
                  staggerpoints=0, # points aligned across models
                  endyrvec=2013,   # final year to show in time series
                  legendlabels=mod.names, filenameprefix="base_", col=mod.cols)


# SO.comps<-cbind(BC$timeseries$SpawnBio,BC.M1$timeseries$SpawnBio[1:100],BC.M2$timeseries$SpawnBio[1:100],BC.M3$timeseries$SpawnBio[1:100])
# colnames(SO.comps)<-c("Year",mod.names)
# write.csv(SO.comps,file="C:/Users/Jason.Cope/Desktop/Aurora_maturity_runs/S0_comps.csv")
