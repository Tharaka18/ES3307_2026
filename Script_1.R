#####################################################################################
#####################################################################################

# Tharaka S. Priyadarshana (ORCID: 0000-0003-3962-5465)
# contact: thilakarathna.p@ntu.edu.sg; tharakas001@e.ntu.edu.sg

# last edit: 14 Oct 2026

#####################################################################################
#####################################################################################

rm(list=ls()) # clean the environment panel 

# packages 
library(car) # for Anova test
library(MuMIn) # for model averaging
library(datasets) # this package comes with R, so you do not have install it

#####################################################################################
#####################################################################################

## stepwise regression ----------------------------------------------------

student_df<-read.csv("https://raw.githubusercontent.com/Tharaka18/ES3307_2026/refs/heads/main/student.csv") # you can directly read the dataset through my GitHub repository  
# or you can download the dataset, but make sure you setup the file directory correclty 
# student_df<-read.csv(file="student.csv",header=T) # if you download the dataset
head(student_df)

# backward
F1 <- lm(math ~ ., data = student_df)
F1_step_back <- step(F1, direction = "backward")

# forward
F2 <- lm(math ~ 1, data = student_df)
F2_step_forward <- step(F2, direction = "forward", scope = formula(F1))

# both backward and forward
F3_step_both <- step(F1, direction = "both")
# or
F4_step_both <- step(F2, direction = "both", scope = formula(F1))

#####################################################################################
#####################################################################################

## model averaging ---------------------------------------------------------

# I use swiss datset to illustrate this
# library(datasets)
data(swiss) # get the dataset
head(swiss)

Q1 <- lm(Fertility ~ ., data = swiss, na.action = "na.fail")
mmQ1 <- dredge(Q1)
mmQ1

mmQ2_avg <- model.avg(mmQ1, subset = delta < 4)
summary(mmQ2_avg)

#####################################################################################
#####################################################################################

## anova vs Anova (car) ----------------------------------------------------

# so let's say I fit a model like below 
M1 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
summary(M1)

# this is sequential anova or Type I sums of squares
anova(M1) 

# this is what is happening in here 
sum(residuals(lm(Fertility ~ 1, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2) # see it is 894.8391

sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2) # see it is 2329.85

sum(residuals(lm(Fertility ~ Education + Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)# see it is 1385.386

sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2) # see it is 2567.88 (note: this is RSS)

# and note if you change the order of the variables it changes the results because then the compared models are different
M2 <- lm(Fertility ~ Education + Agriculture + Catholic, data = swiss) # note only difference here is that I put Education first  

# so if I now do sequential anova, you see I get different results 
anova(M1)
anova(M2) 

#####################################################################################
#####################################################################################

## now look at Anova (default is Type II tests)  -------------------------

# M1 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
# M2 <- lm(Fertility ~ Education + Agriculture + Catholic, data = swiss)
Anova(M1)
Anova(M2)

# this is what is happening in here, you see it is completely different from sequential anova 
sum(residuals(lm(Fertility ~ Education + Catholic, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2) # see it is 486.2846

sum(residuals(lm(Fertility ~ Agriculture + Catholic, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2) # see it is 2827.941

sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2) #see it's 1385.386

drop1(M1, scope = ~ Agriculture + Education + Catholic, test = "F") # this is the same as car:: Anova 

#####################################################################################
#####################################################################################