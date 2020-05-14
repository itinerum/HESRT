###########################################################################
###########################################################################
########################### HEE ALONGSIDE A RCT ###########################
###########################################################################
###########################################################################
#clean all variables and objects first
rm(list=ls())
par(mfrow=c(1,1)) #Print only one plot by window
#
# Using BCEA to model to analyse the results of a RCT
#
#Loading the packages
library(BCEA)
library(ggplot2) #ggplot2 will be used later to plot the results
library(hrbrthemes) #hrbrthemes goes with ggplot2
#
#Some statisticas packages may be needed
library(openintro)
library(dplyr)
library(Hmisc)
library(pastecs)
library(psych)
library(DescTools)
library(modeest)
library(moments)
library(PearsonDS)
#
#
#Load data using the web interface
#BCEAweb()
#Load data from excel using the utility in rstudio
#load costs, and name it "costs"
#always remember is the columns are c1-c2 or c2-c1
#in my case it is c1-c2 (c1 is the cost of the old treatment, c2 is the cost of the new treatment)
c <- as.matrix(costs)
class(c)
c
#load effects, and name it "effects"
#always remember is the columns are e1-e2 or e2-e1
#in my case it is e1-e2 (e1 is effect the old treatment, e2 is effect of the new treatment)
e <- as.matrix(effects)
class(e)
e
#
#statistics for columns in c and e
colMeans(c)
colMeans(e)
#
# USE THE BCEA PACKAGE TO ANALYSE THE RESULTS OF THE BOOTSTRAPPING OR MONTECARLO SIMULATION STORED
# IN THE TWO MATRICES LOADED AT THE BEGINNING OF THE SCRIPT
treats <- c("OldTreatment", "NewTreatment") #depending on the order of the columns
m <- bcea(e,c, ref=2, interventions = treats, Kmax=50000, wtp=NULL,plot = TRUE)
m
summary(m,wtp=50000)
# PLOT THE COST-EFFECTIVENESS PLANE
# different settings used here
ceplane.plot(m,comparison=1,wtp=25000, xlim = c(-0.25,0.25), ylim = c(-2500,3000))
ceplane.plot(m,comparison=1,wtp=25000)
ceplane.plot(m,
             graph="ggplot2",
             pos="top",
             size=1,
             ICER_sizes=2,
             label.pos=FALSE,
             opt.theme=ggplot2::theme(text=ggplot2::element_text(size=8))
)
#
# EXPECTED INCREMENTAL BENEFIT PLOT
eib.plot(m)
#
# COST-EFFECTIVENESS ACCEPTABILITY CURVE (CEAC) PLOT
ceac.plot(m)
#
# EXPECTED VALUE OF INFORMATION (EVI) PLOT
evi.plot(m)
# INCREMENTAL BENEFIT (IB) DISTRIBUTION
ib.plot(m)
#
# COST-EFFECTIVENESS EFFICIENCY FRONTIER (CEEF)
# Produces a plot and a summary of resuts
ceef.plot(m)
#
# COST-EFFECTIVENESS ACCEPTABILITY FRONTIER (CEAF)
ceaf1<- multi.ce(m)
ceaf.plot(ceaf1)
mce.plot(ceaf1)
#
# CONTOUR PLOTS
# The concentric ellipses represent contours of equal probability or the joint posterior density of ∆
contour(m, comparison = 1, scale = 0.5, nlevels = 2, levels = NULL , 
        pos = c(1,0), xlim = c(-0.25,0.25), ylim = c(-2500,3000), graph=c("base","ggplot2"))
# contour2 gives the ceplane and the contour in one plot
contour2(m,       # uses the results of the economic evalaution 
         #  (a "bcea" object)
         scale = 0.5, nlevels = 3, levels = NULL,
         wtp=25000,  # selects the willingness-to-pay threshold
         xl=c(-0.05,0.25),    # assumes default values
         yl=c(-600,3000)     # assumes default values
)


make.report(m, ext = "docx",echo=TRUE,filename="")
riskav<- CEriskav(m, r=NULL, comparison=1)
plot.CEriskav(riskav)
