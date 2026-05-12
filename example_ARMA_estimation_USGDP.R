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
pacman::p_load(	tidyverse,tsibble,readxl,data.table,arrow,vroom,tictoc,matlab,zoo,patchwork,
                sandwich,reshape2,OpenSourceAP.DownloadR,quantmod,pracma,forecast)
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
  rename(NBER = USRECQ, gdp = GDPC1) %>%
  filter( date < as.Date("2020-01-01") )
# write_csv(us.data, "./data/USdata.csv")
head(us.data)

# %% PLOT THE DATA
# break in volatility
great.moderation = us.data$date[which(us.data$date == as.Date("1984-01-01"))]
# set date breaks and fonts size
fnt = 14;  date_breaks = make_dates(us.data$date, freq = 73); 

# plot raw series
p1 = ggplot( us.data, aes(x = date, y = y) ) +
      theme_db( font_size = fnt ) + 
      theme(axis.text.x = element_text(angle = 00, vjust = 1)) + 
      add_recessions( data = recession_periods(us.data) ) +
      gg_yaxis( ylims = c(7.45,10.05) ) + 
      geom_line( color = "#5B8FD1", linewidth = 3/4 ) +
      geom_vline( xintercept = great.moderation, color = "#e62828", linetype = "dashed", linewidth = 1/2) +
      gg_dates(date_breaks, "q")

# plot differences
p2 = ggplot( us.data, aes(x = date, y = dy) ) +
      theme_db( font_size = fnt ) + 
      theme(axis.text.x = element_text(angle = 00, vjust = 1)) + 
      add_recessions( data = recession_periods(us.data) ) +
      gg_yaxis( ylims = c(-12,16) ) + 
      geom_line( color = "#5B8FD1", linewidth = 3/4 ) +
      geom_vline( xintercept = great.moderation, color = "#e62828", linetype = "dashed", linewidth = 1/2) +
      geom_hline( yintercept = 0, linewidth = 1/3) +
      gg_dates(date_breaks, "q")

p12 = p1 / p2; print(p12)
# ggsave("US-GDP.pdf", p12, height = 10, width = 12 ) 	# to save to pdf

# %% plot ACF/PACF
# pdf("acf_pacf.pdf", height = 6, width = 10) 					# to save to pdf
plot_acf(us.data$dy, fntsize = 1.2);  
# dev.off()

# %% ESTIMATE THE ARMA MODELS
P = 2  # max AR order
Q = 1  # max MA order

# Initialize matrices for information criteria
BIC.pq = matrix(NA, nrow = P + 1, ncol = Q + 1)
AIC.pq = matrix(NA, nrow = P + 1, ncol = Q + 1)
HQC.pq = matrix(NA, nrow = P + 1, ncol = Q + 1)

# Loop over AR and MA orders
for (p in 0:P) {
  for (q in 0:Q) {
    arma.fit = arima(us.data$dy, order = c(p, 0, q), method = "ML")
    K = p + q + 1  		# number of parameters (including mean)
    T = arma.fit$nobs 	# number of observations
    # AIC.pq[p + 1, q + 1] = AIC(arma.fit)
    AIC.pq[p + 1, q + 1] = log(arma.fit$sigma2) + 2*K/T
    BIC.pq[p + 1, q + 1] = log(arma.fit$sigma2) + K*log(T)/T
    # Hannan-Quinn Criterion
    n = length(us.data$dy)
    HQC.pq[p + 1, q + 1] = log(arma.fit$sigma2) + 2*K*log(log(T))/T
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
aic.min = which(AIC.pq == min(AIC.pq, na.rm = TRUE), arr.ind = TRUE)
bic.min = which(BIC.pq == min(BIC.pq, na.rm = TRUE), arr.ind = TRUE)
hqc.min = which(HQC.pq == min(HQC.pq, na.rm = TRUE), arr.ind = TRUE)

p.aic = aic.min[1] - 1
q.aic = aic.min[2] - 1
p.bic = bic.min[1] - 1
q.bic = bic.min[2] - 1
p.hqc = hqc.min[1] - 1
q.hqc = hqc.min[2] - 1

cat( paste0( "AIC best fitting ARMA model is: ARMA(",p.aic,",", q.aic,")\n") ) 
cat( paste0( "BIC best fitting ARMA model is: ARMA(",p.bic,",", q.bic,")\n") ) 
cat( paste0( "HQC best fitting ARMA model is: ARMA(",p.hqc,",", q.hqc,")\n") ) 
# cat(sprintf("BIC best fitting ARMA model is: ARMA(%d,%d)\n", p.bic, q.bic))
# cat(sprintf("HQC best fitting ARMA model is: ARMA(%d,%d)\n", p.hqc, q.hqc))

# %% Estimate the final 'best' models
arma.aic = arima(us.data$dy, order = c(p.aic, 0, q.aic))
arma.bic = arima(us.data$dy, order = c(p.bic, 0, q.bic))
arma.hqc = arima(us.data$dy, order = c(p.hqc, 0, q.hqc))

# check model residuals --> post estiamtion examination --> which model is best?
uhat.aic = residuals(arma.aic)
uhat.bic = residuals(arma.bic)
uhat.hqc = residuals(arma.hqc)

# plot ACF/PACF of residuals
plot_acf(uhat.aic, title = paste0("ARMA(",p.aic,",", q.aic,") residuals Model selected with AIC") )
plot_acf(uhat.bic, title = paste0("ARMA(",p.bic,",", q.bic,") residuals Model selected with BIC") )
plot_acf(uhat.hqc, title = paste0("ARMA(",p.hqc,",", q.hqc,") residuals Model selected with HQC") )

# Test serial correlation in residuals using Ljung-Box test 
Box.test( uhat.aic, lag = 10, type = "Ljung-Box")
Box.test( uhat.bic, lag = 10, type = "Ljung-Box")
Box.test( uhat.hqc, lag = 10, type = "Ljung-Box")

# Print full for the relevatn model 
aout.aic = print_results(arma.aic)
aout.bic = print_results(arma.bic)
# print_results(arma.hqc)

# %% Plot compare fitted as well as residuals from biggest and smallest model

source(utility.functions)

r1 = ggplot(us.data, aes(x = date, y = dy)) +
      theme_db(font_size = fnt) + 
      theme(axis.text.x = element_text(angle = 0, vjust = 1) ) +
      add_recessions(data = recession_periods(us.data)) +
  
      geom_line(aes(											color = "GDP growth"), linewidth = 3/4) +
      geom_line(aes(y = fitted(arma.aic), color = "AIC")) + 
      geom_line(aes(y = fitted(arma.bic), color = "BIC"), linetype = "dashed") +
      scale_color_manual(
        breaks = c("GDP growth", "AIC", "BIC"),  # controls order
        values = c( "GDP growth" = "#5B8FD1", 
                    "AIC" = "#e2123c", 
                    "BIC" = "#059a1c"), 
        name = NULL) +
      gg_yaxis( ylims = seq(-12, 16, by = 4) ) + 
      geom_vline( xintercept = great.moderation, color = "#e62828", linetype = "dashed", linewidth = 1/2) +
      geom_hline( yintercept = 0, linewidth = 1/3) +
      gg_dates(date_breaks, "q")

r2 = ggplot(us.data, aes(x = date, y = residuals(arma.aic))) +
      theme_db(font_size = fnt) + 
      theme(axis.text.x = element_text(angle = 0, vjust = 1)) + 
      add_recessions(data = recession_periods(us.data)) +
      geom_line(color = "#5B8FD1", linewidth = 3/4) +
      # geom_line( aes(y = fitted(arma.aic)), color = "#e2123c") + 
      geom_line( aes(y = residuals(arma.bic)), color = "#059a1c", linetype = "dashed") + 
      gg_yaxis( ylims = seq(-12, 16, by = 4) ) + 
      geom_vline( xintercept = great.moderation, color = "#e62828", linetype = "dashed", linewidth = 1/2) +
      geom_hline( yintercept = 0, linewidth = 1/3) +
      gg_dates(date_breaks, "q")

r12 = r1 / r2; print(r12)
# ggsave("US-GDP-fit.pdf", r12, height = 10, width = 12 ) 	# to save to pdf

# theoretical ACF/PACF of fitted models
# plot_acf0(aout.aic$aL,aout.aic$bL)
# plot_acf0(aout.bic$aL,aout.aic$bL)



# %% using auto-arima (not recommended)
# convert first to time-series object
ts.dy = ts(us.data$dy, start = c(1947, 1), frequency = 4)
arma.auto = auto.arima( window( ts.dy, start = c(1947,1), end	= c(2019,4) ), 
																ic = c("aic") )  			# ic = c("aicc", "aic", "bic"),
yhat.auto = fitted(arma.auto)
print_results(arma.auto)
plot( forecast(arma.auto,h=40) )
lines( yhat.auto, col = "blue" )
abline(h=0)


# EOF