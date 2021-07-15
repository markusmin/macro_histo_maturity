## ARTH ##

library(r4ss)
library(gplots)

dir.plots<-"/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/arrowtooth/arrowtooth_plots"
BC<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/arrowtooth/arrowtooth_reference_model",ncols=210)
BC.M1<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/arrowtooth/arrowtooth_bio",covar=FALSE,ncols=210)
BC.M2<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/arrowtooth/arrowtooth_fun",covar=FALSE,ncols=210)
BC.M3<-SS_output("/Users/markusmin/Documents/Hollings_NOAA/new_SA_model_runs/arrowtooth/arrowtooth_macro",covar=FALSE,ncols=210)


mod.names <- c("Base model","Biological","Functional","Macro")
mod.cols  <- rich.colors(length(mod.names))

base.summary <- SSsummarize(list(BC,BC.M1,BC.M2,BC.M3))

# Compare absolute spawning biomass by maturity type
spbio_comp_arth <- as.data.frame(base.summary$SpawnBio)
colnames(spbio_comp_arth) <- c("ref", "bio", "fun", "macro", "label", "year")
# Add column for comparisons between macro maturity and other maturity types
spbio_comp_arth %>% 
  mutate(., macro_v_bio = macro/bio) %>% 
  mutate(., macro_v_fun = macro/fun) %>% 
  mutate(., macro_v_ref = macro/ref) -> spbio_comp_arth

# Compare biological and functional
spbio_comp_arth %>% 
  mutate(., fun_v_bio = fun/bio) -> spbio_comp_arth

# compare in 2015
subset(spbio_comp_arth, year == 2015)



# Look at differences in spawning biomass by maturity type


dev.off()
SSplotComparisons(base.summary, plot=TRUE, print=TRUE, plotdir=dir.plots,
                  spacepoints=20,  # years between points on each line
                  initpoint=0,     # "first" year of points (modular arithmetic)
                  staggerpoints=0, # points aligned across models
                  endyrvec=2013,   # final year to show in time series
                  legendlabels=mod.names, filenameprefix="base_", col=mod.cols)


SO.comps<-cbind(BC$timeseries$SpawnBio,BC.M1$timeseries$SpawnBio[1:100],BC.M2$timeseries$SpawnBio[1:100],BC.M3$timeseries$SpawnBio[1:100])
# colnames(SO.comps)<-c("Year",mod.names)
# write.csv(SO.comps,file="C:/Users/Jason.Cope/Desktop/Aurora_maturity_runs/S0_comps.csv")
