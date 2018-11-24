# ******************************************************************************
# Exchange Rates Common Factors
# ------------------------------------------------------------------------------
# Last edited: Mar-2018
# Rodrigo Mariscal (rmariscal@iif.com)
# ******************************************************************************

# ------------------------------------------------------------------------------
## Upload required libraries 
# ------------------------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(tidyverse)
library(tibbletime)
library(timetk)
library(PerformanceAnalytics)
library(highcharter)
library(RColorBrewer)
library(readxl)
library(readr)
library(foreign)
library(tseries)
library(xts)
library(dlm)
library(fpp)


### Set working directory ###
rm(list = ls(all = TRUE))
#setwd("~/(#) Resarch/(#) FX DynF")


### Some references ###
# https://stackoverflow.com/questions/24477814/how-to-embed-a-highcharts-chart-to-jekyll-blog-post
# https://medium.com/auquan/time-series-analysis-for-finance-arch-garch-models-822f87f1d755
# https://es.wikipedia.org/wiki/Anexo:Chile_en_2018
# https://es.wikipedia.org/wiki/2018



# ------------------------------------------------------------------------------
## Get data
# ------------------------------------------------------------------------------


### Get Data from Yahoo ###
# tickers <- c("USDARS=X","USDBRL=X","USDCLP=X",
#              "USDCNY=X","USDCOP=X","USDINR=X",
#              "USDMXN=X","USDMYR=X","USDPKR=X",
#              "USDPLN=X","USDRUB=X","USDTHB=X",
#              "USDTRY=X","USDZAR=X")
# 
# cnames <- c("Argentina","Brazil","Chile","China",
#             "Colombia","India","Mexico","Malaysia",
#             "Pakistan","Poland","Russia","Thailand",
#             "Turkey","South Africa")
# 
# snames <- c("ARS","BRL","CLP","CNY","COP","INR","MXN","MYR","PKR","PLN","RUB","THB","TRY","ZAR")
# 
# source <- c("yahoo","google","FRED","Oanda")
# 
# ddate <- "2010-01-01"
# bdays <- "15 day"
# 
# 
# # data.fx <- getFX(tickers, from = "2018-07-01", to = Sys.Date(), auto.assign = TRUE, warnings = FALSE) %>% 
# #   map(~get(.)) %>% 
# #   reduce(merge) %>% 
# #   'colnames<-'(snames)
# 
# data.fx <- getSymbols(tickers, src = source[1], from = ddate, to = Sys.Date(), auto.assign = TRUE, warnings = FALSE) %>% 
#   map(~Ad(get(.))) %>% 
#   reduce(merge) %>% 
#   'colnames<-'(snames)
# 
# for (x in tickers) {
#   dname <- x
#   assign(dname,x,envir=.GlobalEnv)
#   rm(x)
# }
# 
# write.table(data.fx, "./data_raw/data_fx.csv", col.names = T, row.names = F)



### Get Data from Haver ###

data.fx <- read_excel("C:/Users/rmariscal/Documents/(#) Resarch/(#) FX DynF/data_raw/EMs - Exchange Rates.xlsx", 
                      sheet = "data.fx", col_names = T, na = ".")

data.fx <- data.fx %>%
  na.locf() %>%
  mutate(date = ymd(date)) %>%
  tk_xts()

data.fx.0 <- data.fx


# ------------------------------------------------------------------------------
## Handle Data
# ------------------------------------------------------------------------------

cnames <- c("ARS","BRL","CLP","MXN","COP","PEN","UYU","CNY","INR","IDR","MYR","THB","PHP","PKR","PLN","HUF","LEV","RON","RUB","TRY","ZAR","EGP")
date <- date(data.fx)


for (x in cnames) {
  
  data.fx[,x] <- (data.fx[,x] - mean(data.fx[,x], na.rm = T)) / sd(data.fx[,x], na.rm = T)
  #data.fx[,x] <- (data.fx[,x] - mean(data.fx[,x], na.rm = T)) 
  #data.fx[,x] <- log(data.fx[,x])
  #data.fx[,x] <- (data.fx[,x] - mean(data.fx[,x], na.rm = T))
  #data.fx[which(data.fx[,x] < -2.0 | data.fx[,x] > 3.0),x] <- NA
  
}

data.fx <- na.locf(data.fx)



# ------------------------------------------------------------------------------
## Specify Model and Estimation
# ------------------------------------------------------------------------------


### LatAm Factor ###

cnames.lat <- c("BRL","CLP","COP","MXN")
data.fx.lat <- data.fx[,cnames.lat]

mm <- ncol(data.fx.lat)
nn <- mm + 1
ps1 <- nn
ps2 <- ps1 + 1
ps3 <- ps2 + mm - 1
ps4 <- ps3 + 1
ps5 <- ps3 + mm

mod.set <- dlmModPoly(order = 2)
mod.set$FF <- cbind(diag(mm),rep(1,mm))
mod.set$V <- diag(mm)
mod.set$GG <- diag(nn)
mod.set$W <- matrix(0, nn, nn)
mod.set$m0 <- rep(0, nn) 
mod.set$C0 <- diag(1e7, nr = nn) 

mod.func <- function(psi) {
  
  # Note Cholesky parametrization of the covariance matrix W
  #psi <- 1:50
  U <- matrix(0, nrow = nn, ncol = nn)
  # making sure that the diagonal element of U are positive
  #diag(U) <- exp(0.5 * psi[1 : ps1])
  diag(U) <- (psi[1 : ps1])^2
  # Constructing the matrix W as the cross product of the U - equivalent to t(U) %*% U
  W(mod.set) <- crossprod(U)
  # parametrization of the covariance matrix V
  #diag(V(mod.set)) <- exp(0.5 * psi[ps2 : ps3])
  diag(V(mod.set)) <- (psi[ps2 : ps3])^2
  # coefficients
  mod.set$FF[,5] <- psi[ps4:ps5]
  
  return(mod.set)
}

# MLE estimate
mod.MLE <- dlmMLE(data.fx.lat, rep(1, ps5), mod.func, control = list(maxit = 1000))

# Checking convergence
mod.MLE$conv

# Building the optimal model
mod.est <- mod.func(mod.MLE$par)

# Covariance matrix
W(mod.est)

# correlations (slopes)
cov2cor(W(mod.est))

# observation standard deviations
sqrt(diag(V(mod.est)))

# Looking at the smoothed series
mod.sm <- dlmSmooth(data.fx.lat, mod.est)

data.est <- data.frame(dropFirst(mod.sm$s))
colnames(data.est) <- c(cnames.lat,"Factor")
data.est <- xts(data.est, order.by = date(rownames(data.est)), frequency = 360)



# data.fx.lat$min <- NA
# data.fx.lat$max <- NA
# data.fx.lat$med <- NA
# data.fx.lat$mea <- NA
# 
# for (t in 1:nrow(data.fx.lat)) {
#   data.fx.lat$min[t,] <- min(data.fx.lat[t,cnames.lat], na.rm = T)
#   data.fx.lat$max[t,] <- max(data.fx.lat[t,cnames.lat], na.rm = T)
#   data.fx.lat$mea[t,] <- mean(data.fx.lat[t,cnames.lat], na.rm = T)
#   data.fx.lat$med[t,] <- median(data.fx.lat[t,cnames.lat], na.rm = T)
# }


for (x in cnames.lat) {
  
  data.est[,x] <- (data.est[,x] - mean(data.est[,x], na.rm = T)) / sd(data.est[,x], na.rm = T)

}

write_excel_csv(data.frame(date=index(data.fx.lat),data.fx.lat), 
                "C:/Users/rmariscal/Documents/blog/content/blog/data/data.fx.lat.csv")
write_excel_csv(data.frame(index(data.est),data.est), 
                "C:/Users/rmariscal/Documents/blog/content/blog/data/data.est.csv")


# ------------------------------------------------------------------------------
## Display Charts
# ------------------------------------------------------------------------------
#col.char <- brewer.pal(n = 4, name = "Set1")
col.char <- c("goldenrod","red","blue","darkgreen")

highchart(type = "stock") %>%
  hc_title(text = "LatAm Exchange Rates without Common Factor, (normalized series)") %>%
  hc_add_series(data.est[,"BRL"], name = "Brazil", color = col.char[1]) %>%
  hc_add_series(data.est[,"CLP"], name = "Chile", color = col.char[2]) %>%
  hc_add_series(data.est[,"COP"], name = "Colombia", color = col.char[3]) %>%
  hc_add_series(data.est[,"MXN"], name = "Mexico", color = col.char[4]) %>%
  hc_yAxis(showFirstLabel = T, showLastLabel = T) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_yAxis( labels = list(format = "{value}"), opposite = F, showFirstLabel = T, showLastLabel = T) %>% 
  hc_navigator(enabled = F) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled= F) %>% 
  hc_legend(enabled = T)


### FX Time Series ###

ggplot() +
  geom_line(data = data.est, aes(x = Index, y = BRL, color = "Brazil"), size = 1) +
  geom_line(data = data.est, aes(x = Index, y = CLP, color = "Chile"), size = 1) +
  geom_line(data = data.est, aes(x = Index, y = COP, color = "Colombia"), size = 1) +
  geom_line(data = data.est, aes(x = Index, y = MXN, color = "Mexico"), size = 1) +
  scale_color_manual(labels = c("Brazil","Chile","Colombia","Mexico"), 
                    breaks = c("Brazil","Chile","Colombia","Mexico"),
                    values = col.char) +
  scale_y_continuous(limits=c(-2,4), breaks=seq(-2,4,1)) +
  scale_x_date(limits = c(as.Date("2009-12-31"), as.Date("2018-12-31")), date_breaks = "1 year", date_labels = "%Y") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.box.background = element_rect(),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey", linetype = 3),
        panel.grid.major.x = element_line(size = 0.2, colour = "transparent", linetype = 3)
  ) +
  labs(x = "", y = "", color = "", 
       title = "LatAm Exchange Rates without Common Factor", 
       subtitle = "Normalized series",
       caption = "Source: finance.yahoo.com.")




