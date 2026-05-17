# cho_moreno_var1_replication.R
# Replication of the reduced-form VAR(1) part of Cho & Moreno (2006)
# Focus: data download, sample construction, VAR(1), Cholesky orthogonalization
#
# Data sources: FRED
# Sample: 1980:Q4 - 2000:Q1 by default
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

# %%  Main
# ----------------------------
# 1) User settings
# ----------------------------
output_gap_method <- "cbo"   # options: "linear", "quadratic", "cbo"
start_sample <- as.yearqtr("1980 Q4")
end_sample   <- as.yearqtr("2000 Q1")

# Variable ordering for Cholesky:
# paper's reduced-form variables are inflation, output gap, interest rate
var_order <- c("inflation", "output_gap", "fedfunds")

# ----------------------------
# 2) Helper functions
# ----------------------------

quarterly_avg <- function(x) {
  aggregate(x, as.yearqtr(index(x)), mean, na.rm = TRUE)
}

# make_quarter_index <- function(x) {
#   as.yearqtr(index(x))
# }

linear_detrend <- function(y_xts) {
  df <- data.frame(
    date = as.Date(index(y_xts)),
    y = as.numeric(y_xts)
  )
  df$t <- seq_len(nrow(df))
  fit <- lm(y ~ t, data = df)
  res <- residuals(fit)
  xts(res, order.by = df$date)
}

quadratic_detrend <- function(y_xts) {
  df <- data.frame(
    date = as.Date(index(y_xts)),
    y = as.numeric(y_xts)
  )
  df$t <- seq_len(nrow(df))
  fit <- lm(y ~ t + I(t^2), data = df)
  res <- residuals(fit)
  xts(res, order.by = df$date)
}

annualized_q_growth <- function(x) {
  400 * diff(log(x))
}

# ----------------------------
# 3) Download data from FRED
# ----------------------------
cat("Downloading FRED data...\n")

# Quarterly series
# GDPDEF  = GDP Deflator
# GDPC1   = Real GDP
# GDPPOT  = Potential GDP (for CBO output gap)
# Monthly series
# FEDFUNDS = Effective Federal Funds Rate

getSymbols(c("GDPDEF", "GDPC1", "GDPPOT", "FEDFUNDS"), src = "FRED")

# ----------------------------
# 4) Construct inflation
# ----------------------------
# Paper: inflation is log difference of GDP deflator, annualized in percent
inflation <- annualized_q_growth(GDPDEF)
colnames(inflation) <- "inflation"

# ----------------------------
# 5) Construct output gap
# ----------------------------
if (output_gap_method == "linear") {
  # Linear detrend of log real GDP
  output_gap <- linear_detrend(log(GDPC1))
  colnames(output_gap) <- "output_gap"
  output_gap_label <- "Linear detrend of log real GDP"

} else if (output_gap_method == "quadratic") {
  # Quadratic detrend of log real GDP
  output_gap <- quadratic_detrend(log(GDPC1))
  colnames(output_gap) <- "output_gap"
  output_gap_label <- "Quadratic detrend of log real GDP"

} else if (output_gap_method == "cbo") {
  # CBO output gap approximation using FRED GDPPOT
  # Paper uses output detrended with CBO potential GDP measure.
  # A standard implementation is:
  #   100 * (log(GDPC1) - log(GDPPOT))
  # This gives a percentage log gap approximation.
  output_gap <- 100 * (log(GDPC1) - log(GDPPOT))
  colnames(output_gap) <- "output_gap"
  output_gap_label <- "CBO-style output gap: 100*(log(GDPC1)-log(GDPPOT))"

} else {
  stop("output_gap_method must be one of: 'linear', 'quadratic', 'cbo'")
}

# ----------------------------
# 6) Construct fed funds rate
# ----------------------------
# Paper: use the average of the Federal funds rate over the previous quarter.
# Since FRED FEDFUNDS is monthly, we take quarterly averages.
# This produces a quarterly series aligned to the quarter in which the average occurs.
fedfunds_q <- aggregate(FEDFUNDS, as.yearqtr(index(FEDFUNDS)), mean, na.rm = TRUE)
colnames(fedfunds_q) <- "fedfunds"

# ----------------------------
# 7) Merge all series and align sample
# ----------------------------
data_all <- merge(inflation, output_gap, fedfunds_q, join = "inner")
data_all <- na.omit(data_all)

# Restrict sample
sample_data <- data_all[paste0(start_sample, "/", end_sample)]
sample_data <- na.omit(sample_data)

# Ensure column order
sample_data <- sample_data[, var_order]

cat("\nSpecification:\n")
cat("Output gap method:", output_gap_method, "\n")
cat("Output gap label:", output_gap_label, "\n")

cat("\nSample period:\n")
print(start(sample_data))
print(end(sample_data))
cat("Observations:", nrow(sample_data), "\n")

# ----------------------------
# 8) Estimate reduced-form VAR(1)
# ----------------------------
var1_fit <- VAR(sample_data, p = 1, type = "const")

cat("\n==============================\n")
cat("VAR(1) Summary\n")
cat("==============================\n")
print(summary(var1_fit))

# Residual covariance matrix
Sigma_u <- cov(resid(var1_fit), use = "complete.obs")
cat("\n==============================\n")
cat("Residual covariance matrix\n")
cat("==============================\n")
print(Sigma_u)

# ----------------------------
# 9) Cholesky decomposition
# ----------------------------
# Lower-triangular factor C such that Sigma_u = C %*% t(C)
# In R, chol() returns upper triangular R with t(R) %*% R = Sigma
# So lower triangular factor is t(chol(Sigma))
C_chol <- t(chol(Sigma_u))

cat("\n==============================\n")
cat("Cholesky factor (lower triangular)\n")
cat("==============================\n")
print(C_chol)

# Structural shocks implied by Cholesky orthogonalization:
# u_t = C * e_t, so e_t = C^{-1} u_t
U <- resid(var1_fit)
E_struct <- t(solve(C_chol, t(U)))
colnames(E_struct) <- paste0("shock_", 1:ncol(E_struct))

# ----------------------------
# 10) Diagnostics
# ----------------------------
cat("\n==============================\n")
cat("Serial correlation test\n")
cat("==============================\n")
print(serial.test(var1_fit, lags.pt = 12, type = "PT.asymptotic"))

cat("\n==============================\n")
cat("ARCH test\n")
cat("==============================\n")
print(arch.test(var1_fit, lags.multi = 5))

cat("\n==============================\n")
cat("Normality test\n")
cat("==============================\n")
print(normality.test(var1_fit))

# ----------------------------
# 11) Orthogonalized IRFs using Cholesky identification
# ----------------------------
irf_chol <- irf(
  var1_fit,
  n.ahead = 20,
  boot = TRUE,
  ortho = TRUE,
  cumulative = FALSE,
  ci = 0.95
)

cat("\nIRFs computed using Cholesky identification.\n")
cat("Use plot(irf_chol) to display them.\n")

# ----------------------------
# 12) Optional: FEVD
# ----------------------------
fevd_chol <- fevd(var1_fit, n.ahead = 20)
cat("\nFEVD computed.\n")

# ----------------------------
# 13) Save outputs
# ----------------------------
dir.create("output", showWarnings = FALSE)

write.csv(
  data.frame(date = as.Date(index(sample_data)), coredata(sample_data)),
  file = "output/cho_moreno_var1_sample_data.csv",
  row.names = FALSE
)

write.csv(
  data.frame(date = as.Date(index(E_struct)), coredata(E_struct)),
  file = "output/cho_moreno_var1_cholesky_shocks.csv",
  row.names = FALSE
)

saveRDS(var1_fit, file = "output/cho_moreno_var1_fit.rds")
saveRDS(C_chol, file = "output/cho_moreno_var1_cholesky_factor.rds")
saveRDS(irf_chol, file = "output/cho_moreno_var1_irf_cholesky.rds")
saveRDS(fevd_chol, file = "output/cho_moreno_var1_fevd_cholesky.rds")

cat("\nSaved files to output/\n")

# ----------------------------
# 14) Print coefficient matrices
# ----------------------------
cat("\n==============================\n")
cat("VAR coefficient matrices\n")
cat("==============================\n")
print(coef(var1_fit))

cat("\nDone.\n")
