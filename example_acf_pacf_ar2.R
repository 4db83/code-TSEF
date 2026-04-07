# Script: example_acf_pacf_ar2.R ----
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
# source("https://raw.githubusercontent.com/4db83/TSEF-code/main/R_help_functions.R") # source dirctly from Github.

# AR Lag polynomial ----
a1 = 1.50 ; a2 = -0.56 
aL = c(1, -a1, -a2)
# companion matrix Phi
Phi = matrix( c(a1,a2,1,0), nrow = 2,ncol = 2)

# lag-polynomial and roots
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(aL)), "\n")
lag.roots = round(polyroot(aL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag Polynomial roots are:", Re(lag.roots), "\n\n") } else {
	cat("Lag Polynomial roots are:", lag.roots, "\n\n" ) 
}

# factored-polynomial and roots ----
cat("Factored Polynomial: ", gsub("x","\u03BB",as.polynomial(fliplr(aL))), '\n')
fact.roots = round(polyroot(fliplr(aL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n\n" ) 
}

# companion matrix Phi and its roots/eigenvalues
char.roots = eigen(Phi)[1] # returns a list
cat("Characteristics roots (eigenvalues of Phi) are:", unlist(char.roots), "\n")

# plot lag-polynomial -----
p2 = par(mfrow=c(1,2), mar = c(3,4,2,3), mgp = c(2.2,0.7,0) )
plot(polynomial(aL), xlim = c(1,1.5), ylim = c(-0.01, 0.01), col="dodgerblue", lwd=2, 
     xlab = "(a) Roots of lag polynomial", ylab = "")
abline(h = 0); abline(v = Re(lag.roots), col="red", lwd=2, lty=2)
# and factored-polynomial
plot(polynomial(fliplr(aL)), xlim = c(0.5,.9), ylim = c(-0.01, 0.01), col="dodgerblue", lwd=2,
     xlab = "(b) Characteristic roots", ylab = "")
abline(h = 0); abline(v = Re(fact.roots), col="red", lwd=2, lty=2)

# plot theoretical ACF/PACF -----
plot.acf0(aL,1,50)