# %% cho_moreno_var1_replication.R
# Replication of the reduced-form VAR(1) part of Cho & Moreno (2006)
# Focus: data download, sample construction, VAR(1), Cholesky orthogonalization
#
# Sample: 1980:Q4 - 2000:Q1 by default
# Data sources: FRED
#
# Variables:
#   inflation    = 400 * diff(log(GDP deflator))
#   output gap   = linear detrend OR quadratic detrend OR CBO gap
#   fed funds    = quarterly average of monthly FEDFUNDS
#
# Output:
#   - VAR(1) estimation
#   - residual covariance matrix
#   - Cholesky identification
#   - orthogonalized impulse responses
# %%  CLEAR THE CONSOLE
cat("\014"); rm(list = ls()); gc()
# SET DEFAULTS: DISPLAY OPTIONS, FONT AND Y AXIS LABEL ROTATION
options(digits = 8); options(scipen = 999);  options(max.print=10000)
windowsFonts("Palatino" = windowsFont("Palatino Linotype")); par(las = 1, family = "Palatino")
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD HELPER FUNCTIONS STORED IN LOCAL DIRECTORY CALLED: ./local.Functions/
functions_path = c("./local.functions/"); if (dir.exists(functions_path)){
invisible( lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ) ) }
# LOAD REQUIRED PACKAGES
pacman::p_load(	tidyverse,tsibble,readxl,data.table,arrow,vroom,tictoc,matlab,
								zoo,sandwich,lmtest,vars,quantmod,xts,tseries)
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, (down)loads online GoogleDrive version
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R";  # use file before to avoid Positron issues not opening files
local.Rfunction 	= "R_utility_functions.R";
if (file.exists(local.Rfunction)){source(local.Rfunction)
} else if(file.exists(substr(utility.functions,9,nchar(utility.functions)))){source(utility.functions)
} else {download.file("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc", destfile = local.Rfunction, mode = "wb")
  source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc"))}
# define some printing to pdf parameters and graphics directory
print2pdf = 0
path2graphics = "./graphics/"
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')

# %% Some settings
output_gap_method <- "cbo"   # options: "linear", "quadratic", "cbo"
start.sample = as.Date("1980-12-31")
end.sample   = as.Date("2000-04-01")

# Variable ordering for Cholesky:
VAR.order <- c("inflation", "output.gap", "fedfunds")

# get fred datas
cat("Downloading FRED data...\n")
getSymbols(c("GDPDEF", "GDPC1", "GDPPOT", "FEDFUNDS"), src = "FRED")

# Paper: inflation is log difference of GDP deflator, annualized in percent
inflation = 400*( log(GDPDEF) - lag(log(GDPDEF)) )
colnames(inflation) = "inflation"
# Construct output gap
# output_gap <- linear_detrend(log(GDPC1))
# output_gap <- quadratic_detrend(log(GDPC1))
output_gap = 100* (log(GDPC1) - log(GDPPOT))
colnames(output_gap) = "output.gap"
# Paper: use the average of the Federal funds rate over the previous quarter.
# Since FRED FEDFUNDS is monthly, we take quarterly averages.
fedfunds = aggregate(FEDFUNDS, as.yearqtr(index(FEDFUNDS)), mean, na.rm = TRUE)
colnames(fedfunds) = "fedfunds"

# join the data and convert to tsibble
data.all = merge(inflation, output_gap, fedfunds)
data.all = xts_to_tibble(data.all)

# Choose sample period
print(data.all)
# data = data.all %>%
# 					filter(	date >= as.Date("1960-01-01"),
# 									date <= as.Date("2019-12-31")) %>%
# 					select(date, output.gap, inflation, fedfunds)

data <- data.all %>%
  dplyr::filter(date >= start.sample,
                date <= end.sample) %>%
  dplyr::select(date, dplyr::all_of(VAR.order))
head(data)
write.csv(data, file = "US.data.csv")

# %% Estimate reduced-form VAR(1)
VAR.fit1 = VAR(data[,2:4], p = 1, type = "const")
print(summary(VAR.fit1))
# Residual covariance matrix
Sigma.U = cov(resid(VAR.fit1), use = "complete.obs")
print(Sigma.U)
# Chol Sigma = PP's
P <- as.matrix(t(chol(Sigma.U)))
print(P)

# Structural shocks implied by Cholesky orthogonalization:
# u_t = C * e_t, so e_t = C^{-1} u_t
U.hat <- resid(VAR.fit1)
E.hat <- t(solve(P, t(U.hat)))
colnames(E.hat) <- paste0("shock_", 1:ncol(E.hat))

# Orthogonalized IRFs using Cholesky identification
irf.chol <- irf(
  VAR.fit1,
  n.ahead = 40,
  boot = FALSE,
  ortho = TRUE,
  cumulative = FALSE,
  ci = 0.90
)

plot(irf.chol)

# FEVD
fevd.chol <- fevd(VAR.fit1, n.ahead = 20)

# A coefficient matrices
A1 = as.matrix(Acoef(VAR.fit1))
print(A1)

A1 <- as.matrix(A1[[1]])
P  <- as.matrix(P)

AP <- A1 %*% P
