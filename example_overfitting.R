# Script: example_overfitting.R ----
# Simulate WN series and try to fit up to ARMA(Pmax,Qmax) using auto.arima function from forecast package
# NOTE: To run this script, you also need "R_help_functions.R" available at: https://github.com/4db83/code-TSEF.
# clear screen/workspace
cat("\014"); rm(list = ls()); gc()
# SET WORKING DIRECTORY PATH IF NEED
# this is mine, your need to set your path
# setwd("D:/_teaching/_current.teaching/_SU.TSEF/lectures/code-TSEF")

# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED. 
# (Note: you may need to disable windows firewall to allow installation)
if (!"pacman" %in% installed.packages()){install.packages("pacman"); cat("pacman installed\n")}
# LOAD REQUIRED PACKAGES
pacman::p_load(polynom, matlab, forecast)
source("./R_utility_functions.R")

Pmax  = 5;
Qmax  = 5;
T     = 3e3;
a0    = rep(0,Pmax);
b0    = rep(0,Qmax);
T0    = proc.time();
Nsim  = 10;

# space allocation;
arma.terms = matrix(0,Nsim,2)    
# set simulation seed
set.seed(1)
dx = matrix( rnorm(T*Nsim), T, Nsim )

# start Matlab style timer for entire simulation ----
tic()
for (i in 1:Nsim){
    aout = auto.arima( dx[,i],
        d=0, D=0, max.Q=0, max.P=0, 
        max.p=Pmax, max.q=Qmax,
        start.p=a0, start.q=b0,
        seasonal=FALSE, trace=FALSE, stepwise=FALSE,
        ic=c("aic"), 
        max.order=20 )
    
    # print iterations to screen
    t.tmp = Nsim*(proc.time() - T0)
    if (i == 1){
      cat("---------------------------------------------------------------------\n")
      cat("    Total time is approximately: ", t.tmp[3], "Seconds\n")
      cat("---------------------------------------------------------------------\n")
    }
    # store the ARMA terms
    arma.terms[i,] = aout$arma[1:2]
    cat("Iteration number =", sprintf("%03d", i), "ARMA terms:", aout$arma[1:2], "\n")
}

#  tT = proc.time() - T0 ----
cat("Elapased time is:", (proc.time() - T0)[3], "\n" )
print(arma.terms)
N_AR_terms = sum(arma.terms[,1]>0)
N_MA_terms = sum(arma.terms[,2]>0)
# print to screen the number of wrongly identified ARMA terms
cat(paste0("True model is ARMA(0,0), yet Auto.arima finds \n", 
           N_AR_terms, "× non-zero AR terms detected, and \n",
           N_MA_terms, "× non-zero MA terms detected!!! \n") )
toc()
