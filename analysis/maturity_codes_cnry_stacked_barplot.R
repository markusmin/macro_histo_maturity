## Script to create canary maturity code usage stacked barplot (Fig. 4 in Min et al. 2021)

library(here)
library(RColorBrewer)

cnry.codes<-read.csv(here("data", "cnry_codes.csv"),check.names = FALSE, row.names = 1)
cnry.n<-read.csv(here("data", "cnry_n.csv"))
mypalette <- brewer.pal(8, "Spectral")


# Create plot and save as .eps
setEPS()
postscript(here("figures", "Fig4.eps"), width = 10, height = 6)

par(mar = c(4,4,1,9))
par(xpd=TRUE)
barplot(as.matrix(cnry.codes),col=mypalette,width = c(cnry.n$N),space = 0)
legend(6800,0.9,legend = c("Immature (1)","Maturing (2)","Vitellogenesis (3)","Fertilized (4)","Ripe (5)","Spent (6)","Resting (7)","Mature (8)"),
       col=c(mypalette),pch=15,y.intersp = 2.1,bty="n",cex = 1.2)
# Legend title
text(7200,0.9, "Stage", cex = 1.3)
mtext("Proportion",side=2,outer=TRUE,line=-1.5, cex = 1.4, at = 0.55)
mtext("Year", side = 1, outer = TRUE, line = -1.5, cex = 1.4, at = 0.45)
# mtext("Canary Rockfish Maturity Code Usage",side=3,outer=TRUE,line=-3,cex=1.5)

dev.off()
