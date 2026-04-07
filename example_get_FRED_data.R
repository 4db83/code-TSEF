# CLEAR THE CONSOLE
cat("\014"); rm(list = ls()); gc()
# SET DEFAULTS: DISPLAY OPTIONS, FONT AND Y AXIS LABEL ROTATION
options(digits = 8); options(scipen = 999);  options(max.print=10000)
windowsFonts("Palatino" = windowsFont("Palatino Linotype")); par(las = 1, family = "Palatino")
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED 
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# SET WORKING DIRECTORY 
# setwd('D:/_research/_current/LW03_2024/code')
# LOAD HELPER FUNCTIONS STORED IN LOCAL DIRECTORY CALLED: ./local.Functions/
functions_path = c("./local.Functions/"); if (dir.exists(functions_path)){
invisible( lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ) ) }
# UNCOMMENT TO LOAD R BASELINE R_utility_functions.R FROM D:/matlab.tools/db.toolbox
# source("D:/matlab.tools/db.toolbox/R_utility_functions.R")
source("./R_utility_functions.R")
# LOAD REQUIRED PACKAGES
pacman::p_load(tictoc,matlab,tis,tsibble,dplyr,zoo) 
set.seed(1234)

## SCRIPT STARTS HERE ----
# 1) read in data as DF
gdp.df = getFREDdata('https://fred.stlouisfed.org/data/GDPC1.txt', convert2tif = FALSE)
# aa = as.matrix(gdp.df$VALUE)
# size(aa)

# colnames(gdp) = c("Date","GDP")
# gdp$Date = yearquarter(gdp$Date)
# gdp.ts = as_tsibble(gdp, index = Date)
# head2tail(gdp)
# 
# ts = tsibble(Date = yearquarter(gdp$DATE), gdp = gdp$VALUE)


# datastart = which(gsub(' ', '',readLines('https://fred.stlouisfed.org/data/GDPC1.txt'))=='DATEVALUE') - 2
# 
# #data <- read.table('FREDtemp.txt', skip = datastart, header = TRUE)
# data <- read.table('https://fred.stlouisfed.org/data/GDPC1.txt', skip = datastart, header = TRUE)
# 
# 
# # convert to tsibble
# as.Date(as.POSIXlt(gdp))
# PCE
pce.m = getFREDdata('https://fred.stlouisfed.org/data/PCEPILFE.txt',"pce.txt","Monthly")
### ----
# to convert to end of q
pce.q = convert(pce.m, tif = 'quarterly', method = 'constant', observed. = 'end')
head2tail(pce.q)
head2tail(pce.m)

pce.df <- data.frame(
  Date = as.Date(ti(start(pce.m):end(pce.m), tif(pce.m))),
  value = coredata(pce.m)
)

# Generate a sequencce of quarterly dates
# function = tis2df(tis.Object){
tis.object = pce.m
  freq.Num = frequency(tis.object)
  if (freq.Num == 1){
    freq.Str = "day"
  } else if (freq.Num == 5) {
    freq.Str = "week"
  } else if (freq.Num == 12) {
    freq.Str = "month"
  } else if (freq.Num == 4) {
    freq.Str = "quarter"
  }
  
  
  
  date <- seq.Date(from = as.Date(start(tis.object)), 
                    by = freq.Str, 
                    length.out = length(tis.object))

  df.out = data.frame(date)



# Print the quarterly dates ----
# print(quarterly_dates)
# pce.ts = tsibble(quarterly_dates)
# head(pce.ts)


# pce.ts = as_tsibble(pce.m, index = DATE)
# pce.q = pce.ts %>%
#   # as_tsibble(index = DATE) %>%
#   index_by(year_quarter = ~ yearquarter(.)) %>% 
#   summarise(PCE = last(VALUE))


# as.data.frame(pce.m)

# as.quarterly(pce.m, FUN=sum)

# # to conver to end of q
# pce.q = convert(pce.m, tif = 'quarterly', method = 'constant', observed. = 'end')

# plot.ts(gdp, type = 'l')
# 
# ts.gdp = tsibble(Date,pgd)


# 
# ## SCRIPT: starts here ----
# # read the data
# # load US GPD data from txt file 
# df <- fred(y = "GDPC1", "unrate", all=FALSE)
# 
# 
# ts = tsibble(Date = yearquarter(df$date),gdp = df$y, uen = df$UNRATE)
# # colnames(ts) = c("Date","GDP")
# ts
# plot(log(ts$GDP),type = "l")
# 
# gdp = filter_index(ts, ~ "2019 Q4")
# tail(gdp)
