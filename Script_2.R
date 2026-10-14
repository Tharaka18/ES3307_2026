#####################################################################################
#####################################################################################

# Tharaka S. Priyadarshana (ORCID: 0000-0003-3962-5465)
# contact: thilakarathna.p@ntu.edu.sg; tharakas001@e.ntu.edu.sg

# last edit: 14 Oct 2026

#####################################################################################
#####################################################################################

# reference

# Shipley, B. (2016). Cause and correlation in biology: A user's guide to path analysis, structural equations and causal inference with R. Cambridge university press

#####################################################################################
#####################################################################################

rm(list=ls()) # clean the environment panel  

# packages
library(ggm) # for dsep tests
library(jpeg) # for reading images 
library(dagitty) # for drawing DAGs

setwd("~/Desktop/1PostDocWork/ppt/20_22_Oct_2026") # change according to your file directory 

#####################################################################################
#####################################################################################

## d-sep tests ----------------------------------------------------

# you might get an error message when trying to install the “ggm” package
# you may be asked to install the “graph” package first
# if you get into this issue please run the following codes to install the “graph” package and then install the “ggm” package

# if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("graph")

#####################################################################################
#####################################################################################

### exercise 1

## entering the DAG ----------------------------------------------------

# read your DAG
img1 <- readJPEG("DAG1.jpg")
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img1,1,1,2,2)
}

# or you can draw the DAG structure using the "dagitty" package, please learn this!
graph1 <- dagitty('dag {
  X [pos="1,2"]
  U [pos="2,2"]
  V [pos="3,2"]
  W [pos="4,2"]
  Y [pos="5,2"]
  S1 [pos="2,3"]
  S2 [pos="4,3"]
  
  X -> U
  V -> U
  V -> W
  Y -> W
  U -> S1
  W -> S2
}')

plot(graph1)

my_dag<-DAG(U~X+V,S1~U,W~V+Y,S2~W)

# evaluating a d-separation claim
# U_||_S2|{X,W}
dSep(amat=my_dag,first="U",second="S2",cond=c("X","W"))

#####################################################################################
#####################################################################################

### exercise 2

# read your DAG
img2 <- readJPEG("DAG2.jpg")
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img2,1,1,2,2)
}

# or draw the DAG using the "dagitty" package
graph2 <- dagitty('dag {
  A [pos="1,2"]
  B [pos="2,2"]
  C [pos="3,2"]
  D [pos="2,3"]
  E [pos="3,3"]
  
  A -> B
  B -> C
  B -> D
  C -> E
  D -> E
}')

plot(graph2)

# obtaining the union basis set of a given DAG 
my_dag2<-DAG(B~A,C~B,D~B,E~C+D)
union_basis_set<-basiSet(amat=my_dag2)
union_basis_set

#####################################################################################
#####################################################################################

# now conducting a d-sep test using this DAG

# here is the data set:
lecture3_dataset1<-read.table(file="dat.txt",header=T) # make sure you download this dataset into the same file directory 
# or you can directly read the dataset through my GitHub repository   
# lecture3_dataset1<-read.table("https://raw.githubusercontent.com/Tharaka18/ES3307_2026/refs/heads/main/dat.txt", header=T) 
head(lecture3_dataset1)
pairs(lecture3_dataset1)

# we will save the p values of each dsep claim in "p.values"
p_values<-rep(NA,5)

# first dsep claim:
union_basis_set[1]
# we do a linear regression of D on A conditional on B
# is the slope associated with A is zero or not
# following the causal structure
summary(lm(D~B+A,data=lecture3_dataset1))

# here is a quick way of extracting the null probability of the slope of A
summary(lm(D~B+A,data=lecture3_dataset1))$coefficients[3,4]
p_values[1]<-summary(lm(D~B+A,data=lecture3_dataset1))$coefficients[3,4]

# second dsep claim
union_basis_set[2]
# we do a linear regression of C on A, conditional on B
summary(lm(C~B+A,data=lecture3_dataset1))
p_values[2]<-summary(lm(C~B+A,data=lecture3_dataset1))$coefficients[3,4]

# third dsep claim
union_basis_set[3]
# we do a linear regression of E on A, conditional on D and C
summary(lm(E~C+D+A,data=lecture3_dataset1))
p_values[3]<-summary(lm(E~C+D+A,data=lecture3_dataset1))$coefficients[4,4]
# this may suggest that A is not completely independent from E conditioning on C and D, but remember when we do multiple tests so the null probability change 

# forth dsep claim
union_basis_set[4]
# we do a linear regression of E on B, conditional on A, C and D
summary(lm(E~C+D+A+B,data=lecture3_dataset1))
p_values[4]<-summary(lm(E~C+D+A+B,data=lecture3_dataset1))$coefficients[5,4]

# fifth dsep claim
union_basis_set[5]
# we do a linear regression of E on B, conditional on A, C and D
summary(lm(C~B+D,data=lecture3_dataset1))
p_values[5]<-summary(lm(C~B+D,data=lecture3_dataset1))$coefficients[3,4]

# here are the null probabilities associated with these 5 dsep claims:
p_values
# here is the C statistic
C_stat<--2*sum(log(p_values))
C_stat

# this C statistic has 2*5=10 degrees of freedom (2*number od d-sepration tests in the basis set).  Here is the null probability
# associated with this C statistic:
1-pchisq(C_stat,10) # this is how get the chisq for a null probability 
# 31% chance at least observing that amount of difference in multivariate data you collected and the one predicted by the DAG

#####################################################################################
#####################################################################################

# CONCLUSION: 
# the data are not unusual assuming that they were generated by this DAG
# now, since we have not rejected our DAG, we can estimate the casual effect 

#####################################################################################
#####################################################################################