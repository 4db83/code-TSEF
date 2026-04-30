# %% Example of ARMA model fitted to US Real GDP
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
								zoo,sandwich,reshape2,OpenSourceAP.DownloadR,quantmod,pracma,forecast)
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
set.seed(1234)   			# fix seed if needed for reproducibility of results

# read US Data
us.data = read_parquet("./data/USdata.parquet") %>%
  rename(NBER = USRECQ, gdp = GDPC1) 
write_csv(us.data, "./data/USdata.csv")
head(us.data)
plot_acf(us.data$dy, fntsize = 1.4)

# %% PLOT THE DATA
# break in volatility
great.moderation = us.data$date[which(us.data$date == as.Date("1984-01-01"))]

# use_dark_plots()
if (print2pdf == 1){ pdf( paste0(pdf.file.name, ".pdf"),
                          width = 11.1, height = 6.0, onefile = TRUE) }
fnt = 1.5 # font size
op = par( mfrow = c(2,1), family = "serif", las = 1,
          mgp = c(  4,.5, 0),      # axis title, labels, line, position of y,x labels to axis
          mar = c(  4, 6, 1, 4),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(  1, 1, 1, 1)  ) # outer margins for overall spacing

# use white background when printing to pdf
if (print2pdf == 1) {use_dark_plots("white") }

# axis labels
y.grid = seq(7.5,10.5,.5)                  # y-axis labels
date.ticks = seq.Date(
  from = min(us.data$date),
  to   = max(us.data$date),
  by   = "4 years"   # adjust frequency here
)

plot(	us.data$date, 
      us.data$y,
      type = 'l', lwd  = 2,
      col  = "steelblue",
      xlab = "",
      ylab = "US Real GDP (log)",
      pch  = 16,
      # Font sizes
      cex.lab  = fnt + .1,
      # Remove default y,x-axis labels
      xaxt = "n", yaxt = "n",
      ylim = c(7.5,10.5),                    # set y-axis limits
      xlim = c(	min(date.ticks)+2  *365,      # set x-axis limits for dates with off-sets
								max(date.ticks)-1.0*365) )

# add grid [solid, dashed, dotted, dotdash, longdash, twodash]
abline(lty = "dotdash", lwd = .5, h = y.grid[-1], v = date.ticks, col="lightgray")
# Add horizontal line at y=0
abline(h=0)
abline(v=great.moderation, col = "red", lty = "dotted", lwd = 1.5)

# add cross axis ticks
cross_ticks( side = 1, at = date.ticks, labels = quarterly_dates(date.ticks), tcl = .2, cex = fnt )
cross_ticks( side = 2, at = y.grid, tcl = .2, cex = fnt )
cross_ticks( side = 4, at = y.grid, tcl = .2, cex = fnt )

#  Plot 2: GDP growth (dy) with grid lines
y2.grid = seq(-12, 16, 4)
plot(	us.data$date,
      us.data$dy,
      type = 'l', lwd  = 2,
      col  = "steelblue",
      xlab = "",
      ylab = "GDP Growth Rate",
      cex.lab  = fnt + .1,
      xaxt = "n", yaxt = "n",
      ylim = c(-12, 16),
      xlim = c(	min(date.ticks) + 2*365,
								max(date.ticks) - 1.0*365))

abline(lty = "dotdash", lwd = .5, h = y2.grid, v = date.ticks, col = "lightgray")
abline(h = 0)
abline(v=great.moderation, col = "red", lty = "dotted", lwd = 1.5)

cross_ticks(side = 1, at = date.ticks, labels = quarterly_dates(date.ticks), tcl = .2, cex = fnt)
cross_ticks(side = 2, at = y2.grid, tcl = .2, cex = fnt)
cross_ticks(side = 4, at = y2.grid, tcl = .2, cex = fnt)
# print to pdf if needed
if (print2pdf == 1){ par(op);dev.off() }

# %% ESTIMATE THE ARMA MODELS
P <- 2  # max AR order
Q <- 1  # max MA order

# Initialize matrices for information criteria
BIC.pq <- matrix(NA, nrow = P + 1, ncol = Q + 1)
AIC.pq <- matrix(NA, nrow = P + 1, ncol = Q + 1)
HQC.pq <- matrix(NA, nrow = P + 1, ncol = Q + 1)

# Loop over AR and MA orders
for (p in 0:P) {
  for (q in 0:Q) {
    arma.fit <- arima(us.data$dy, order = c(p, 0, q), method = "ML")
    K <- p + q + 1  		# number of parameters (including mean)
    T = arma.fit$nobs 	# number of observations
    # AIC.pq[p + 1, q + 1] <- AIC(arma.fit)
    AIC.pq[p + 1, q + 1] <- log(arma.fit$sigma2) + 2*K/T
    BIC.pq[p + 1, q + 1] <- log(arma.fit$sigma2) + K*log(T)/T
    # Hannan-Quinn Criterion
    n <- length(us.data$dy)
    HQC.pq[p + 1, q + 1] <- log(arma.fit$sigma2) + 2*K*log(log(T))/T
  }
}

rownames(AIC.pq) = paste0("p=",0:P); colnames(AIC.pq) = paste0("q=",0:Q)
rownames(BIC.pq) = paste0("p=",0:P); colnames(BIC.pq) = paste0("q=",0:Q)
rownames(HQC.pq) = paste0("p=",0:P); colnames(HQC.pq) = paste0("q=",0:Q)
cat("------ AIC ------\n");print(round(AIC.pq,4))
cat("------ BIC ------\n");print(round(BIC.pq,4))
cat("------ HQC ------\n");print(round(HQC.pq,4))
cat("----------------------------------------- \n")
# Find best models
aic_min <- which(AIC.pq == min(AIC.pq, na.rm = TRUE), arr.ind = TRUE)
bic_min <- which(BIC.pq == min(BIC.pq, na.rm = TRUE), arr.ind = TRUE)
hqc_min <- which(HQC.pq == min(HQC.pq, na.rm = TRUE), arr.ind = TRUE)

p_aic <- aic_min[1] - 1
q_aic <- aic_min[2] - 1
p_bic <- bic_min[1] - 1
q_bic <- bic_min[2] - 1
p_hqc <- hqc_min[1] - 1
q_hqc <- hqc_min[2] - 1

cat(sprintf("AIC best fitting ARMA model is: ARMA(%d,%d)\n", p_aic, q_aic))
cat(sprintf("BIC best fitting ARMA model is: ARMA(%d,%d)\n", p_bic, q_bic))
cat(sprintf("HQC best fitting ARMA model is: ARMA(%d,%d)\n", p_hqc, q_hqc))

# Estimate the final 'best' models
arma_aic <- arima(us.data$dy, order = c(p_aic, 0, q_aic))
arma_bic <- arima(us.data$dy, order = c(p_bic, 0, q_bic))
arma_hqc <- arima(us.data$dy, order = c(p_hqc, 0, q_hqc))

# %% Print full results
source(utility.functions)
print_results(arma_aic)

y_aic = fitted(arma_aic)
y_bic = fitted(arma_bic)
y_hqc = fitted(arma_hqc)
matplot(us.data$date, 
  cbind(us.data$dy, y_aic, y_bic), 
  type = "l", col = c("black", "red", "green"), lwd = 2)
abline(h = 0)


# my_style <- function() {
#   par(bg = "#FAFAFA", col.axis = "#555", col.lab = "#333",
#       col.main = "#111", font.main = 1, cex.main = 1.3,
#       las = 1, tck = -0.02, mgp = c(2, 0.5, 0), family = "Helvetica")
# }
# 
# my_style()
# plot(us.data$date, y1, type = "l", col = "#E63946", lwd = 2,
#      ylim = range(c(y1, y2)), bty = "l",
#      xlab = "Time", ylab = "Value", main = "My Custom Style",
#      panel.first = grid(col = "#DDDDDD", lty = 1, lwd = 1))
# lines(x, y2, col = "#457B9D", lwd = 2)
# legend("topleft", legend = c("Series A", "Series B"),
#        col = c("#E63946", "#457B9D"), lwd = 2, bty = "n")

# print(arma_bic)
# print(arma_hqc)
# 
# # %% PLOT ACTUAL AND FITTED VALUES/RESIDUALS
# # Get fitted values and residuals
# us.data <- us.data %>%
#   mutate(
#     fitted_aic = dy - residuals(arma_aic),
#     fitted_bic = dy - residuals(arma_bic),
#     resid_aic = residuals(arma_aic),
#     resid_bic = residuals(arma_bic)
#   )
# 
# ## % Plotting actual vs fitted values and residuals
# # Actual vs fitted plot
# p3 <- ggplot(us.data, aes(x = date)) +
#   add_recession_bars(us.data, c(-12, 16)) +
#   geom_line(aes(y = dy, color = "Actual"), size = 1) +
#   geom_line(aes(y = fitted_aic, color = "AIC-ARMA(2,2)"), size = 1) +
#   geom_line(aes(y = fitted_bic, color = "BIC-ARMA(1,0)"), linetype = "dashed", size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
#   geom_vline(xintercept = us.data$date[T2], color = "red", linetype = "dashed") +
#   labs(title = "(a) Actual and fitted values", y = "GDP Growth") +
#   ylim(-12, 16) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_manual(name = "Series", 
#                      values = c("Actual" = "blue", 
#                                 "AIC-ARMA(2,2)" = "red", 
#                                 "BIC-ARMA(1,0)" = "green"))
# 
# # Residuals plot
# p4 <- ggplot(us.data, aes(x = date)) +
#   add_recession_bars(us.data, c(-12, 16)) +
#   geom_line(aes(y = resid_aic, color = "AIC-ARMA(2,2)"), size = 1) +
#   geom_line(aes(y = resid_bic, color = "BIC-ARMA(1,0)"), linetype = "dashed", size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
#   geom_vline(xintercept = us.data$date[T2], color = "red", linetype = "dashed") +
#   labs(title = "(b) Residuals", y = "Residuals") +
#   ylim(-12, 16) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_manual(name = "Model", 
#                      values = c("AIC-ARMA(2,2)" = "red", 
#                                 "BIC-ARMA(1,0)" = "green"))
# 
# # Display the plots
# print(p3 / p4)
# 
# # POST ESTIMATION PLOTS - ACF/PACF of residuals
# par(mfrow = c(2, 2))
# acf(residuals(arma_aic), main = "ACF of AIC model residuals", lag.max = 20)
# pacf(residuals(arma_aic), main = "PACF of AIC model residuals", lag.max = 20)
# acf(residuals(arma_bic), main = "ACF of BIC model residuals", lag.max = 20)
# pacf(residuals(arma_bic), main = "PACF of BIC model residuals", lag.max = 20)
# 
# # Function to plot theoretical ACF/PACF
# plot_acf0 <- function(ar_coefs, ma_coefs, n_lags = 20) {
#   # Convert to ARMA model specification for arima.sim
#   # This creates the theoretical ACF
#   ar_model <- arima.sim(n = 10000, model = list(ar = ar_coefs, ma = ma_coefs))
#   par(mfrow = c(2, 1))
#   acf(ar_model, main = "Theoretical ACF", lag.max = n_lags)
#   pacf(ar_model, main = "Theoretical PACF", lag.max = n_lags)
# }
# 
# # Theoretical ACF/PACF for BIC model (if AR coefficients exist)
# if (p_bic > 0) {
#   ar_coefs_bic <- coef(arma_bic)[grepl("ar", names(coef(arma_bic)))]
#   ma_coefs_bic <- coef(arma_bic)[grepl("ma", names(coef(arma_bic)))]
#   if (length(ma_coefs_bic) == 0) ma_coefs_bic <- NULL
#   
#   plot_acf0(ar_coefs_bic, ma_coefs_bic)
# }
# 
# # Ljung-Box test for residuals
# Box.test(residuals(arma_aic), lag = 10, type = "Ljung-Box")
# Box.test(residuals(arma_bic), lag = 10, type = "Ljung-Box")
# 
#   # EOF