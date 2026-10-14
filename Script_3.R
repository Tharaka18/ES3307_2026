#####################################################################################
#####################################################################################

# Tharaka S. Priyadarshana (ORCID: 0000-0003-3962-5465)
# contact: thilakarathna.p@ntu.edu.sg; tharakas001@e.ntu.edu.sg

# last edit: 14 Oct 2026

#####################################################################################
#####################################################################################

# reference

# The indirect effects of habitat disturbance on the bird communities in a tropical African forest 
# Mammides et al., 2015
# Biodiversity Conservation
# https://doi.org/10.1007/s10531-015-1001-x 

#####################################################################################
#####################################################################################

rm(list=ls()) # clean the environment panel  

# packages
library(lavaan) # for global (covariance-based) SEM 
library(lavaanPlot) # for plotting "lavvan" fit objects
library(piecewiseSEM) # for local (path-by-path) SEM
library(jpeg) # for reading images

setwd("~/Desktop/1PostDocWork/ppt/20_22_Oct_2026") # change according to your file directory 

#####################################################################################
#####################################################################################

# read your DAG
img <- readJPEG("DAG3.jpg")
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}

#####################################################################################
#####################################################################################

# global (covariance-based) SEM  ----------------------------------------------------

dat <- read.csv("SEM_Variables.csv",header=TRUE)

Global_Est <- "
StumpsTotalLog  ~ DistFEdLog
CO  ~ StumpsTotalLog
FHD  ~ StumpsTotalLog
PDExp  ~ StumpsTotalLog
SSp ~  CO + FHD + PDExp + StumpsTotalLog + DistFEdLog
"         
             
fit_GE <- sem(Global_Est, data = dat)

summary(fit_GE, standardized=TRUE, rsquare=TRUE)

# plot the results 
lavaanPlot(model=fit_GE, coefs=TRUE, stand=TRUE, sig=0.05)

#####################################################################################
#####################################################################################

# local (path-by-path) SEM  ---------------------------------------------------------

Local_Est <- psem(
lm(StumpsTotalLog  ~ DistFEdLog, data=dat), 
lm(CO  ~ StumpsTotalLog, data=dat),
lm(FHD  ~ StumpsTotalLog, data=dat),
lm(PDExp  ~ StumpsTotalLog, data=dat),
lm(SSp ~  CO + FHD + PDExp + StumpsTotalLog + DistFEdLog, data=dat)
)

summary(Local_Est, intercepts=TRUE, conditioning=TRUE, conserve = TRUE, standardize="scale")

# plot the results 

plot(Local_Est)

#####################################################################################
#####################################################################################

# let's double check whether the model output ---------------------------------------

# free parameters (p+q) = 
# 9 path coefficient + 
# 1 exogenous variances 
# 5 residual variances
# = 15

# p = number of path coefficient
# q = number of variances
# v = number of variables 
# degree of freedom (DF) = v(v+1)/2 - (p+q) = 6(7)/2 - (10+7) = 21-15 = 6

#####################################################################################
#####################################################################################