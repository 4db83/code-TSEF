# Script: example_acf_non_invertible_ma2.R ----
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
pacman::p_load(polynom, matlab)
source("./R_utility_functions.R")

# Non-invertible MA Lag polynomial ----
b1 = -3.5 ; b2 = -2;
bL = c(1, +b1, +b2);   

# plot theoretical ACF/PACF of non-ivertible MA
plot.acf0(1,bL,50)

# lag-polynomial and roots
cat("For the non-invertible process \n")
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(bL)), "\n")
lag.roots = round(polyroot(bL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag polynomial roots are:    ", Re(lag.roots), "\n") } else {
	cat("Lag polynomial roots are:", lag.roots, "\n" ) 
}
# factored-polynomial and roots ----
fact.roots = round(polyroot(fliplr(bL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n\n" ) 
}

# Invertibel MA Lag polynomial ----
# make process invertible 
b1_plus = -( Re(fact.roots[1]) + 1/Re(fact.roots[2]))
b2_plus =  ( Re(fact.roots[1])*1/Re(fact.roots[2]))
bL_plus = c(1, +b1_plus, +b2_plus)

cat("For the invertible process \n")
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(bL_plus)), "\n")

lag.roots_plus = round(polyroot(bL_plus),4)
if (sum(Im(lag.roots_plus)==0)) {
  cat("Lag polynomial roots are:", Re(lag.roots_plus), "\n") } else {
  cat("Lag polynomial roots are:", lag.roots_plus, "\n" ) 
}

# factored-polynomial and roots ----
fact.roots_plus = round(polyroot(fliplr(bL_plus)),4)
if (sum(Im(fact.roots_plus)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots_plus), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots_plus, "\n\n" ) 
}

# plot theoretical ACF/PACF of non-ivertible MA
plot.acf0(1,bL_plus,50)


