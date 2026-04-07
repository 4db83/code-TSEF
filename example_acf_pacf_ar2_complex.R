# Script: example_acf_pacf_ar2_complex.R ----
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

# AR Lag polynomial ----
a1 = 1.4 ; a2 = -0.85
aL = c(1, -a1, -a2)
# companion matrix Phi
Phi = matrix( c(a1,a2,1,0), nrow = 2,ncol = 2)

# lag-polynomial and roots
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(aL)), "\n")
lag.roots = round(polyroot(aL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag polynomial roots are:", Re(lag.roots), "\n") } else {
	cat("Lag polynomial roots are:", lag.roots, "\n" ) 
}
cat("Modulus of Lag Polynomial roots is:", round(Mod(lag.roots)[1], digits = 4), "\n\n")

# factored-polynomial and roots ----
cat("Factored Polynomial: ", gsub("x","\u03BB",as.polynomial(fliplr(aL))), '\n')
fact.roots = round(polyroot(fliplr(aL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n" ) 
}
cat("Modulus of Factored Polynomial roots is:", round(Mod(fact.roots)[1], digits = 4), "\n\n")

# companion matrix Phi and its roots/eigenvalues
char.roots = eigen(Phi)[1] # returns a list
cat("Characteristics roots (eigenvalues of Phi) are:", unlist(char.roots), "\n")

# plot theoretical ACF/PACF -----
plot.acf0(aL,1,50)

# plot of simulated series of sample size 200 ----
set.seed(123);
B = 2e2   # B = is the burn in period
T = 2e2   # sample size
c = 0.3   # constant
u = rnorm(B+T)
x = zeros(B+T,1)

for (t in 3:(B+T)){
  x[t] = c + a1*x[t-1] + a2*x[t-2] + u[t]
}
# using inbuild function: arima.sim simulates a zero mean ARIMA model -> need to add mean Mu = c/a(1) to it.
# x = c/sum(aL) + arima.sim(n = T+B, list(ar = -aL[-1], ma = 0) );
x = x[-(1:B)]; # remove the burn-in period
par(mfrow=c(1,1))
plot(x, type='l', col="dodgerblue", lwd = 2)
abline(h=c(0), lty=c(1), col=c(1))












