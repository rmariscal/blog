# ******************************************************************************
# MX Exchange Rate Analysis
# ------------------------------------------------------------------------------
# Last edited: Oct-2018
# Rodrigo Mariscal (rmariscalparedes@imf.org)
# ******************************************************************************

# ------------------------------------------------------------------------------
## Upload required libraries 
# ------------------------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)
library(PerformanceAnalytics)
library(dplyr)
library(broom)
library(purrr)
library(readxl)
library(foreign)
library(psych)
library(xts)
library(tseries)

### Set working directory ###
rm(list = ls(all = TRUE))
setwd("C:/Users/rmariscal/Documents/(#) Blog Posts/Rcode")
#setwd("D:/(#) Blog Posts/2018_03")


### Interesting Links ###
# https://rviews.rstudio.com/2017/07/12/introduction-to-volatility/
# https://stackoverflow.com/questions/26694042/how-to-get-currency-exchange-rates-in-r
# https://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/
# https://ocw.mit.edu/courses/mathematics/18-s096-topics-in-mathematics-with-applications-in-finance-fall-2013/lecture-notes/MIT18_S096F13_lecnote9.pdf
# https://beta.vu.nl/nl/Images/werkstuk-ladokhin_tcm235-91388.pdf
# https://www.supplychaindive.com/news/NAFTA-timeline-how-USMCA-happened/538663/
# https://medium.com/auquan/time-series-analysis-for-finance-arch-garch-models-822f87f1d755
# https://ftalphaville.ft.com/2018/06/14/1528970896000/NAFTA-la-vista--baby/


# ------------------------------------------------------------------------------
## Get data
# ------------------------------------------------------------------------------

snames <- c("CAD","MXN","BRL","CLP","COP","BIT")
source <- c("yahoo","google","FRED","Oanda")
ddate <- "2018-05-02"
#ddate <- "2018-08-27"
bdays <- "20 day"

symbols <- c("USD/CAD","USD/MXN","USD/BRL","USD/CLP","USD/COP","USD/BTC")
data.fx2 <- getFX(symbols, from = "2018-07-01", to = Sys.Date(), auto.assign = TRUE, warnings = FALSE) %>%
  map(~get(.)) %>%
  reduce(merge) %>%
  'colnames<-'(snames)
data.fx2 <- na.locf(data.fx2)

symbols <- c("USDCAD=X","USDMXN=X","USDBRL=X","USDCLP=X","USDCOP=X","USDBTC=X")
data.fx <- getSymbols(symbols, src = source[1], from = ddate, to = Sys.Date(), auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'(snames)
data.fx <- na.locf(data.fx)

bench <- c("EWW","EWZ","EWC","ECH","GXG","SPY")
data.stk <- getSymbols(bench, src = source[1], from = ddate, to = Sys.Date(), auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'(bench)
data.stk <- na.locf(data.stk)


write_excel_csv(data.frame(date=index(data.fx),data.fx), "./data.fx.csv")
write_excel_csv(data.frame(date=index(data.stk),data.stk), "./data.stk.csv")


# ------------------------------------------------------------------------------
## Estimate Volatility
# ------------------------------------------------------------------------------

mxn.ts <- na.omit(log(data.fx$MXN)-log(lag(data.fx$MXN,1)))
t(describe(100*sqrt(252)*mxn.ts))

mex.garch <- garch(mxn.ts, order = c(1,1), grad = "numerical", trace = F)
mex.garch.res <- mex.garch$residuals[-1]
mex.garch.fit <- 100*sqrt(252)*mex.garch$fitted.values[-1,1]
mex.garch.fit <- xts(mex.garch.fit, order.by = date(mxn.ts[-2]), frequency = 360)
mex.vol <- rollapply(100*sqrt(252)*mxn.ts, FUN=sd, width=15)


cop.ts <- na.omit(log(data.fx$COP)-log(lag(data.fx$COP,1)))
cop.garch <- garch(cop.ts, order = c(1,1), grad = "numerical", trace = F)
cop.garch.res <- cop.garch$residuals[-1]
cop.garch.fit <- 100*sqrt(252)*cop.garch$fitted.values[-1,1]
cop.garch.fit <- xts(cop.garch.fit, order.by = date(cop.ts[-2]), frequency = 360)
cop.vol <- rollapply(100*sqrt(252)*cop.ts, FUN=sd, width=15)

clp.ts <- na.omit(log(data.fx$CLP)-lag(log(data.fx$CLP),1))
clp.garch <- garch(clp.ts, order = c(1,1), grad = "numerical", trace = F)
clp.garch.res <- clp.garch$residuals[-1]
clp.garch.fit <- 100*sqrt(252)*clp.garch$fitted.values[-1,1]
clp.garch.fit <- xts(clp.garch.fit, order.by = date(clp.ts[-2]), frequency = 360)
clp.vol <- rollapply(100*sqrt(252)*clp.ts, FUN=sd, width=15)

cad.ts <- na.omit(log(data.fx$CAD)-log(lag(data.fx$CAD,1)))
cad.garch <- garch(cad.ts, order = c(1,1), grad = "numerical", trace = F)
cad.garch.res <- cad.garch$residuals[-1]
cad.garch.fit <- 100*sqrt(252)*cad.garch$fitted.values[-1,1]
cad.garch.fit <- xts(cad.garch.fit, order.by = date(cad.ts[-2]), frequency = 360)
cad.vol <- rollapply(100*sqrt(252)*cad.ts, FUN=sd, width=15)

brl.ts <- na.omit(log(data.fx$BRL)-log(lag(data.fx$BRL,1)))
brl.garch <- garch(brl.ts, order = c(1,1), grad = "numerical", trace = F)
brl.garch.res <- brl.garch$residuals[-1]
brl.garch.fit <- 100*sqrt(252)*brl.garch$fitted.values[-1,1]
brl.garch.fit <- xts(brl.garch.fit, order.by = date(brl.ts[-2]), frequency = 360)
brl.vol <- rollapply(100*sqrt(252)*brl.ts, FUN=sd, width=15)


#data.vol <- cbind(cad.garch.fit, mex.garch.fit, clp.garch.fit, cop.garch.fit, brl.garch.fit)
data.vol <- cbind(cad.vol, mex.vol, clp.vol, cop.vol, brl.vol)
vnames <- c("CAD","MXN","CLP","COP","BRL")
colnames(data.vol) <- vnames

#summary(mex.garch)
#plot(mex.garch.fit)



data.fx <- data.fx["2018-07-01/"]
data.stk <- data.stk["2018-07-01/"]
data.vol <- data.vol["2018-07-01/"]




# ------------------------------------------------------------------------------
## Charts
# ------------------------------------------------------------------------------

pic.save <- 1500
# Facebook dpi = 1500
# Twitter dpi = 2500
  
  
survey.1 <- "2018-04-18"
debate.1 <- "2018-04-22"
survey.2 <- "2018-05-01"
debate.2 <- "2018-05-20"
debate.3 <- "2018-06-12"
elect.MEX <- "2018-07-01"

rate.fed.1 <- "2018-08-01"
nafta.1 <- "2018-08-27"

rate.fed.2 <- "2018-09-26"
nafta.2 <- "2018-09-30"

min.fed.2 <- "2018-10-17"

elect.BRA <- "2018-10-07"
fitch.pemex <- "2018-10-19"

naicm.1 <- "2018-10-26"
naicm.2 <- "2018-10-29"

banks.fees <- "2018-11-08"
poll.2 <- "2018-11-24"



vlevel <- 18
ggplot() +
  geom_line(data = data.fx, aes(x = Index, y = MXN, color = "Mexico"), size = 1) +
  scale_color_manual(labels = c("Mexico"), 
                     breaks = c("Mexico"),
                     values = c("Mexico"="darkgreen")) +
  
  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_y_continuous(limits=c(17,21), breaks=seq(17,21,1)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.background = element_rect(fill="transparent"),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "transparent", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Mexico's Local Currency per USD", 
       caption = "Source: finance.yahoo.com.")
ggsave(file="Exchange.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500


vlevel <- 11
ggplot() +
  geom_line(data = data.vol, aes(x = Index, y = MXN, color = "Mexico"), size = 1) +
  
  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_color_manual(labels = c("Mexico","Colombia","Canada","Chile"), 
                     breaks = c("Mexico","Colombia","Canada","Chile"),
                     values = c("Mexico"="darkgreen","Colombia"="red","Canada"="orange","Chile"="black")) +
  
  scale_y_continuous(limits=c(6,22), breaks=seq(6,22,2)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.background = element_rect(fill="transparent"),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "transparent", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Mexico's Exchange Rate Volatility, Annualized (%)", 
       caption = "Note: Rolling standard deviation of a daily returns.\nSource: finance.yahoo.com.")
ggsave(file="Exchange Vol.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500





vlevel <- 2
ggplot() +
  geom_line(data = data.vol, aes(x = Index, y = MXN, color = "Mexico"), size = 1) +
  geom_line(data = data.vol, aes(x = Index, y = COP, color = "Colombia"), size = 0.8) +
  geom_line(data = data.vol, aes(x = Index, y = CAD, color = "Canada"), size = 0.8) +
  geom_line(data = data.vol, aes(x = Index, y = CLP, color = "Chile"), size = 0.8) +
  geom_line(data = data.vol, aes(x = Index, y = BRL, color = "Brazil"), size = 1) +
  
  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_color_manual(labels = c("Mexico","Colombia","Canada","Chile","Brazil"), 
                     breaks = c("Mexico","Colombia","Canada","Chile","Brazil"),
                     values = c("Mexico"="darkgreen","Colombia"="red","Canada"="orange","Chile"="black","Brazil"="blue")) +
  
  scale_y_continuous(limits=c(0,24), breaks=seq(0,24,4)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = c(0.75,0.94), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.box.background = element_rect(),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "transparent", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Exchange Rate Volatility, Annualized (%)", 
       caption = "Note: Rolling standard deviation of a daily returns.\nSource: finance.yahoo.com.")
ggsave(file="Exchange Vol_pull.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500






vlevel <- 90
ggplot() +
  geom_line(data = data.fx, aes(x = Index, y = 100*(MXN/MXN[1]), color = "Mexico"), size = 1) +
  geom_line(data = data.fx, aes(x = Index, y = 100*(COP/COP[1]), color = "Colombia"), size = 1) +
  geom_line(data = data.fx, aes(x = Index, y = 100*(CAD/CAD[1]), color = "Canada"), size = 1) +
  geom_line(data = data.fx, aes(x = Index, y = 100*(CLP/CLP[1]), color = "Chile"), size = 1) +
  geom_line(data = data.fx, aes(x = Index, y = 100*(BRL/BRL[1]), color = "Brazil"), size = 1) +
  
  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_color_manual(labels = c("Mexico","Colombia","Canada","Chile","Brazil"), 
                     breaks = c("Mexico","Colombia","Canada","Chile","Brazil"),
                     values = c("Mexico"="darkgreen","Colombia"="red","Canada"="orange","Chile"="black","Brazil"="blue")) +
  scale_y_continuous(limits=c(85,115), breaks=seq(85,115,5)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = c(0.38,0.94), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.box.background = element_rect(),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "transparent", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Local Currency per USD, Jul-2 = 100",
       caption = "Source: finance.yahoo.com.")
ggsave(file="Exchange_indx.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500







vlevel <- 55

ggplot() +
  geom_line(data = data.stk, aes(x = Index, y = EWW, color = "Mexico"), size = 1) +
  
  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_color_manual(labels = c("Mexico"), 
                     breaks = c("Mexico"),
                     values = c("Mexico"="darkgreen")) +
  
  scale_y_continuous(limits=c(40,60), breaks=seq(40,60,5)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.background = element_rect(fill="transparent"),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Mexico's Stock Price Index, Country ETF", 
       caption = "Source: finance.yahoo.com.")
ggsave(file="Stock_Exchange.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500




vlevel <- 70

ggplot() +
  geom_line(data = data.stk, aes(x = Index, y = 100*(EWW/EWW[1]), color = "Mexico"), size = 1) +
  geom_line(data = data.stk, aes(x = Index, y = 100*(GXG/GXG[1]), color = "Colombia"), size = 1) +
  geom_line(data = data.stk, aes(x = Index, y = 100*(EWC/EWC[1]), color = "Canada"), size = 1) +
  geom_line(data = data.stk, aes(x = Index, y = 100*(ECH/ECH[1]), color = "Chile"), size = 1) +
  geom_line(data = data.stk, aes(x = Index, y = 100*(EWZ/EWZ[1]), color = "Brazil"), size = 1) +  

  geom_vline(xintercept = as.numeric(as.Date(elect.MEX)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(elect.MEX), label="\nMEX election", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(fitch.pemex)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(fitch.pemex), label="\nPemex downgrade", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.1), label="\nAirport poll", y = vlevel), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(naicm.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(naicm.2), label="\nAirport Canceled", y = vlevel, vjust=-0.25), colour="black", angle=-90, size = 3) +
  geom_vline(xintercept = as.numeric(as.Date(banks.fees)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(banks.fees), label="\nBank Fees Bill", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(poll.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(poll.2), label="\nTrain poll", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(nafta.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.1), label="\nNUSA-MEX deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(nafta.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(nafta.2), label="\nUSMCA deal", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.1)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.1), label="\nFED leaves 2.0%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(rate.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(rate.fed.2), label="\nFED raise 2.25%", y = vlevel), colour="black", angle=-90, size = 3) +  
  geom_vline(xintercept = as.numeric(as.Date(min.fed.2)), linetype = 2, col = "black") +
  geom_text(aes(x = as.Date(min.fed.2), label="\nFED Minutes", y = vlevel), colour="black", angle=-90, size = 3) +  
  
  scale_color_manual(labels = c("Mexico","Colombia","Canada","Chile","Brazil"), 
                     breaks = c("Mexico","Colombia","Canada","Chile","Brazil"),
                     values = c("Mexico"="darkgreen","Colombia"="red","Canada"="orange","Chile"="black","Brazil"="blue")) +
  scale_y_continuous(limits=c(60,140), breaks=seq(60,140,10)) +
  scale_x_date(date_breaks = bdays, date_labels = "%b-%d") +
  theme_gdocs(base_size = 9) +
  theme(legend.position = c(0.35,0.90), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.box.background = element_rect(),
        #panel.grid.major.y = element_line(size = 0.10, colour = "white", linetype = 1),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(size = 0.10, colour = "transparent", linetype = 1),
        panel.grid.minor.x = element_line(colour = "transparent")) +
  labs(x = "", y = "", color = "", 
       title = "Stock Price Indexes, Country ETFs (Jul-2 = 100)",
       caption = "Source: finance.yahoo.com.")
ggsave(file="Stock_Exchange_indx.png", width=6, height=4.8, dpi=pic.save)
# export as png: 1360 x 800
# Facebook dpi = 1500
# Twitter dpi = 2500







# ------------------------------------------------------------------------------
## Factor Analysis
# ------------------------------------------------------------------------------

pca <- princomp(data.fx, scores = T, cor = T)
summary(pca)
loadings(pca)
#screeplot(pca, type='line', main='Scree Plot')
#biplot(pca)

varimax(pca$loadings, normalize = F)
promax(pca$loadings)


fac.1 <- factanal(data.fx, factors = 2, rotation = "none")
fac.2 <- factanal(data.fx, factors = 2, rotation = "varimax")
fac.3 <- factanal(data.fx, factors = 2, rotation = "varimax", scores = "regression")


fac.2
B.mat <- t(fac.2$loadings[,])
sig.e <- diag(fac.2$uniquenesses)
sig.tot <- t(B.mat) %*% B.mat + sig.e







