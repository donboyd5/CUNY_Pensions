
# notes re code folding:
#   alt-L, alt-shift-L  one section
#   alt-O, alt-shift-O  all sections
#   alt-R run current code section (remapped from Ctrl-alt-T)

#****************************************************************************************************
# Notes ####
#****************************************************************************************************

#****************************************************************************************************
# Libraries ####
#****************************************************************************************************
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times
library("lubridate") # lubridate, for date/times
library("vctrs")

library("grDevices")
library("knitr")
library("kableExtra")

library("btools")
library("pdata")

library("DT") # for datatable

library("zoo") # for rollapply


#****************************************************************************************************
# globals ####
#****************************************************************************************************
figwidth <- 8
figheight <- 6
# 8 6

#****************************************************************************************************
# create data ####
#****************************************************************************************************

sal1 <- 50e3
salgrow <- .035
benfactor <- .02
cola <- .01

agestart <- 25
ageret <- 60 # last year of work is 1 less
agedeath <- 85
(yos <- ageret - agestart)
final_yos <- (ageret - agestart)

# salary
dfsal <- tibble(age=agestart:agedeath) %>%
  mutate(year=age - min(age) + 1,
         salary=ifelse(year > yos, 0, sal1 * (1 + salgrow)^(year - 1)))

# initial benefit calc
(fas <- mean(dfsal$salary[(final_yos - 2):final_yos]))
(benefit1 <- benfactor * yos * fas)

dfben <- dfsal %>%
  mutate(benefit=ifelse(year > yos, benefit1 * (1 + cola)^(year - yos - 1), 0))

dfbase <- expand_grid(drate=seq(.02, .08, .005), age=agestart:agedeath)


df <- dfbase %>%
  right_join(dfben) %>%
  group_by(drate) %>%
  mutate(pvsalary=salary / (1 + drate)^(year -1),
         cumpvsal=cumsum(pvsalary),
         pvben=benefit / (1 + drate)^(year -1),
         cumpvben=cumsum(pvben),
         totpvsal=cumpvsal[year==max(year)],
         totpvben=cumpvben[year==max(year)],
         salratio=cumpvsal / totpvsal,
         aal=salratio * totpvben,
         earned=ifelse(year > 1, diff(aal), aal[1]),
         nc=totpvben / totpvsal) %>%
  ungroup %>%
  arrange(drate, year)

glimpse(df)
ht(df)

df %>% filter(age==85)
df %>% filter(age==25)
df %>% filter(drate==.02)


# AAL ----
c1 <- "Assumes:  Benefit = 2%  x  3-year final average salary  x  years of service. COLA:  guaranteed 1% per year."
c2 <- "Worker hired at 25, retires at 60, lives until 85. Salary grows 3.5% per year. Entry age cost method."
capt <- paste0(c1, "\n", c2)

pdata <- df %>%
  filter(year == 20) %>%
  mutate(iaal=aal / aal[drate==.07]) %>%
  select(drate, iaal)
write_csv(pdata, here::here("results", "aal.csv"))

p <- 
  pdata %>%
  ggplot(aes(drate, iaal)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept=.035, colour="darkgrey", linetype="dotted") +
  geom_vline(xintercept=.07, colour="darkgrey", linetype="dotted") +
  scale_y_continuous(name="Ratio of liability to liability at 7% discount rate", breaks=seq(0, 10, 0.5), limits=c(0, NA)) +
  scale_x_continuous(name="Discount rate", breaks=seq(0, 1, .005), labels=scales::percent_format(accuracy=.1)) +
  labs(caption=capt) +
  ggtitle("Ratio of actuarial liability at different discount rates to liablity at 7% discount rate",
          subtitle = "For an employee who has worked 20 years")  +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "aal.png"), plot=p, width=figwidth, height=figheight, scale=1)



# Funded ratio ----
c1 <- "Assumes:  Benefit = 2%  x  3-year final average salary  x  years of service. COLA:  guaranteed 1% per year."
c2 <- "Worker hired at 25, retires at 60, lives until 85. Salary grows 3.5% per year. Entry age cost method."
c3 <- "Funded ratio shown for a plan that is 75% funded when actuarial liability is calculated at a 7% discount rate."
capt <- paste0(c1, "\n", c2, "\n", c3)

pdata <- df %>%
  filter(year == 20) %>%
  mutate(assets=aal[drate==.07] * .75,
         fr=assets / aal) %>%
  select(drate, fr)
write_csv(pdata, here::here("results", "fr.csv"))

p <- pdata %>%
  ggplot(aes(drate, fr)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept=.035, colour="darkgrey", linetype="dotted") +
  geom_vline(xintercept=.07, colour="darkgrey", linetype="dotted") +
  scale_y_continuous(name="Reported funded ratio", breaks=seq(0, 2, 0.25), limits=c(0, NA), labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous(name="Discount rate", breaks=seq(0, 1, .005), labels=scales::percent_format(accuracy=.1)) +
  labs(caption=capt) +
  ggtitle("Funded ratio at different discount rates",
          subtitle = "For an employee who has worked 20 years")  +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "fr.png"), plot=p, width=figwidth, height=figheight, scale=1)


# Supportable benefits ----
glimpse(df)
df %>%
  filter(drate %in% c(.035, .07), age %in% c(25, 50, 65)) %>%
  select(drate, age, year, nc)
(nc_drate07 <- df %>% filter(drate==.07, year==1) %>% .$nc)

df %>%
  filter(year == 1) %>%
  mutate(nctarget=nc_drate07,
         ratio=nctarget / nc,
         replace=benfactor * yos * ratio)

c1 <- "Assumes:  Benefit = benefit factor  x  3-year final average salary  x  years of service. COLA:  guaranteed 1% per year."
c2 <- "Worker hired at 25, retires at 60, lives until 85. Salary grows 3.5% per year. Entry age cost method."
c3 <- "Assumes a fixed 13.5 percent contribution rate with benefit factor allowed to vary."
# capt <- paste0(c1, "\n", c2, "\n", c3)
capt <- paste0(c1, "\n", c2)

pdata <- df %>%
  filter(year == 1) %>%
  mutate(nctarget=nc_drate07,
         ratio=nctarget / nc,
         bensupport=benfactor * yos * ratio)  %>%
  select(drate, bensupport)
write_csv(pdata, here::here("results", "bensupport.csv"))

p <- pdata %>%
  ggplot(aes(drate, bensupport)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_vline(xintercept=.035, colour="darkgrey", linetype="dotted") +
  geom_vline(xintercept=.07, colour="darkgrey", linetype="dotted") +
  scale_y_continuous(name="Initial retirement benefit as percent of final salary", breaks=seq(0, 2, 0.05), limits=c(0, NA), labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous(name="Investment return assumption", breaks=seq(0, 1, .005), labels=scales::percent_format(accuracy=.1)) +
  labs(caption=capt) +
  ggtitle("Retirement benefit supported by different investment return assumptions",
          subtitle=c3)  +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "bensupport.png"), plot=p, width=figwidth, height=figheight, scale=1)


# Employer contributions ----
c1 <- "Assumes:  Benefit = 2%  x  3-year final average salary  x  years of service. COLA:  guaranteed 1% per year."
c2 <- "Worker hired at 25, retires at 60, lives until 85. Salary grows 3.5% per year. Entry age cost method."
capt <- paste0(c1, "\n", c2)

pdata <- df %>%
  filter(year == 1) %>%
  mutate(erc=nc /nc[drate==.07]) %>%
  select(drate, erc)
write_csv(pdata, here::here("results", "erc.csv"))

p <- pdata %>%
  ggplot(aes(drate, erc)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept=.035, colour="darkgrey", linetype="dotted") +
  geom_vline(xintercept=.07, colour="darkgrey", linetype="dotted") +
  scale_y_continuous(name="Ratio of required contributions to contributions at 7% assumption", 
                     breaks=seq(0, 6, 0.25), limits=c(0, NA), labels=scales::comma_format(accuracy=.01)) +
  scale_x_continuous(name="Investment return assumption", breaks=seq(0, 1, .005), labels=scales::percent_format(accuracy=.1)) +
  labs(caption=capt) +
  ggtitle("Required contributions at different investment return assumptions", 
          subtitle="Relative to contributions required at 7 percent")  +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "erc.png"), plot=p, width=figwidth, height=figheight, scale=1)


# Underpayment ----
ns(ppd)
glimpse(ppd)
count(ppd, fy)

ppd %>%
  group_by(fy) %>%
  do(qtiledf(.$RequiredContribution))

ppd %>%
  group_by(fy) %>%
  do(qtiledf(.$PercentReqContPaid))

ppd %>% 
  select(fy, ppd_id, plan=PlanName, arc=RequiredContribution, arcpct=PercentReqContPaid) %>%
  filter(!is.na(arcpct)) %>%
  filter(arcpct < .70, fy==2018) %>%
  mutate(upay=(1 - arcpct) * arc)

pdat <-  ppd %>% 
  select(fy, ppd_id, plan=PlanName, arc=RequiredContribution, arcpct=PercentReqContPaid) %>%
  filter(!is.na(arcpct)) %>%
  mutate(arcpct=arcpct * 100,
         arcpct=pmin(arcpct, 100)) %>%
  left_join(expand.grid(fy=2001:2018, arcpct_threshold=c(0, 10, 20, 25, 30, 33, 50, 67, 70, 75, 80, 85, 90, 100) %>% as.integer)) %>%
  mutate(upaid=arcpct < arcpct_threshold) %>%
  group_by(fy, arcpct_threshold) %>%
  summarise(n=n(), n_upaid=sum(upaid)) %>%
  mutate(pct_upaid=n_upaid / n * 100)
glimpse(pdat)

pdat %>% filter(fy==2018)
pdat %>% filter(arcpct_threshold==75)
pdat %>% filter(arcpct_threshold==85)

vals <- c(10, 20, 30)
(labs <- paste0(vals, "%"))
cols <- c("#fed98e", "orange", "red")

pdat2 <- pdat %>%
  mutate(upay_threshold=100 - arcpct_threshold) %>%
  filter(upay_threshold %in% vals) %>%
  mutate(upay_threshold=factor(upay_threshold, levels=vals, labels=labs)) %>%
  select(fy, upay_threshold, pct_upaid)
write_csv(pdat2, here::here("results", "upay.csv"))

p <- pdat2 %>%
  ggplot(aes(fy, pct_upaid, colour=upay_threshold)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="Percent of plans (%)", breaks=seq(0, 200, 5), limits=c(0, NA)) +
  scale_x_continuous(name="Plan fiscal year", breaks=seq(2000, 2020, 2), limits=c(2000.8, 2018.1)) +
  scale_color_manual(values=cols) +
  ggtitle(label="Percentage of plans receiving less than the actuarially determined contribution",
          subtitle="By size of underpayment") +
  labs(caption="Source: Authors' analysis of the Public Plans Database") +
  guides(colour=guide_legend(title="Underpayment\nof at least:")) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p

ggsave(here::here("results", "upay.png"), plot=p, width=figwidth, height=figheight, scale=1)



# Amortization methods ----
# closed dollar 15
# closed dollar 30
# closed pct 30
# open dollar 30
# open pct 30

pmt <- function (p, i, n, end = FALSE) {
  # p	principal
  # i	interest rate
  # m	number of periods
  # end	if TRUE, payment at the end of period
  if (end) p <- p * (1 + i)
  a_n <- (1 - (1 + i)^(-n)) / 
    (1 - 1/(1 + i))
  pmt <- p/a_n
  return(pmt)
  # pmt(100, .075, 15)
}

gaip <- function (p, i, n, g, end = FALSE) {
  # graduated annuity initial payment
  # p principal
  # i interest
  # n amortization period
  # g growth rate
  if (end) 
    p <- p * (1 + i)
  k <- (1 + g)/(1 + i)
  a_sn <- (1 - k^n)/(1 - k)
  pmt <- p/a_sn
  return(pmt)
}

near_zero <- function(value){
  (value > -1e-9) & (value < 1e-9)
}
p <- 100; i <- .07; n <- 15

# the following functions return a df with atype, year, liab_boy, payment, liab_eoy
cld <- function(p, i, n, nyears){
  # closed level dollar
  initpay <- pmt(p, i, n)
  atype <- paste0("cld", n)
  # df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA_real_, payment=NA_real_, liab_eoy=NA_real_)
  # df[1, c("liab_boy", "payment", "liab_eoy")] <- c(p, initpay, (p - initpay) * (1 + i))
  df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA_real_, payment=NA_real_, liab_eoy=NA_real_)
  df[1, ] <- df[1, ] %>%
    mutate(liab_boy=p, payment=initpay, liab_eoy=(p - initpay) * (1 + i))
  for(y in 2:nyears){
    df$liab_boy[y] <- df$liab_eoy[y - 1]
    df$payment[y] <- ifelse(y <= n, initpay, 0)
    df$liab_eoy[y] <- (df$liab_boy[y] - df$payment[y]) * (1 + i)
    if(near_zero(df$liab_eoy[y])) df$liab_eoy[y] <- 0
  }
  df
  # cld(100, .07, 15, 30)
}

old <- function(p, i, n, nyears){
  # open level dollar
  initpay <- pmt(p, i, n)
  atype <- paste0("old", n)
  # df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA, payment=NA, liab_eoy=NA)
  # df[1, c("liab_boy", "payment", "liab_eoy")] <- c(p, initpay, (p - initpay) * (1 + i))
  df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA_real_, payment=NA_real_, liab_eoy=NA_real_)
  df[1, ] <- df[1, ] %>%
    mutate(liab_boy=p, payment=initpay, liab_eoy=(p - initpay) * (1 + i))
  for(y in 2:nyears){
    df$liab_boy[y] <- df$liab_eoy[y - 1]
    df$payment[y] <- pmt(df$liab_boy[y], i, n)
    df$liab_eoy[y] <- (df$liab_boy[y] - df$payment[y]) * (1 + i)
    if(near_zero(df$liab_eoy[y])) df$liab_eoy[y] <- 0
  }
  df
  # old(100, .07, 15, 30)
}


clp <- function(p, i, n, g, nyears){
  # closed level percent
  initpay <- gaip(p, i, n, g)
  atype <- paste0("clp", n)
  # df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA, payment=NA, liab_eoy=NA)
  # df[1, c("liab_boy", "payment", "liab_eoy")] <- c(p, initpay, (p - initpay) * (1 + i))
  df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA_real_, payment=NA_real_, liab_eoy=NA_real_)
  df[1, ] <- df[1, ] %>%
    mutate(liab_boy=p, payment=initpay, liab_eoy=(p - initpay) * (1 + i))
  for(y in 2:nyears){
    df$liab_boy[y] <- df$liab_eoy[y - 1]
    df$payment[y] <- ifelse(y <= n, df$payment[y - 1] * (1 + g), 0)
    df$liab_eoy[y] <- (df$liab_boy[y] - df$payment[y]) * (1 + i)
    if(near_zero(df$liab_eoy[y])) df$liab_eoy[y] <- 0
  }
  df
  # clp(100, .07, 15, .035, 50)
}

olp <- function(p, i, n, g, nyears){
  # open level percent
  initpay <- gaip(p, i, n, g)
  atype <- paste0("olp", n)
  # df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA, payment=NA, liab_eoy=NA)
  # df[1, c("liab_boy", "payment", "liab_eoy")] <- c(p, initpay, (p - initpay) * (1 + i))
  df <- tibble(atype=rep(atype, nyears), year=1:nyears, liab_boy=NA_real_, payment=NA_real_, liab_eoy=NA_real_)
  df[1, ] <- df[1, ] %>%
    mutate(liab_boy=p, payment=initpay, liab_eoy=(p - initpay) * (1 + i))
  for(y in 2:nyears){
    df$liab_boy[y] <- df$liab_eoy[y - 1]
    df$payment[y] <- gaip(df$liab_boy[y], i, n, g)
    df$liab_eoy[y] <- (df$liab_boy[y] - df$payment[y]) * (1 + i)
    if(near_zero(df$liab_eoy[y])) df$liab_eoy[y] <- 0
  }
  df
  # olp(100, .07, 15, .035, 50)
}

liab0 <- 100
nyears <- 50
i <- .07
liab <- bind_rows(cld(liab0, i, 15, nyears),
                  cld(liab0, i, 30, nyears),
                  old(liab0, i, 30, nyears),
                  clp(liab0, i, 30, .035, nyears),
                  olp(liab0, i, 30, .035, nyears))

levs <- c("cld15", "cld30", "clp30", "old30", "olp30")
labs <- c("Closed, level dollar, 15 years",
          "Closed, level dollar, 30 years",
          "Closed, level percent, 30 years",
          "Open, level dollar, 30 years",
          "Open, level percent, 30 years")
cols <- c("lightblue", "grey", "orange", "red", "blue")

pdata <- liab %>%
  mutate(atype=factor(atype, levels=levs, labels=labs)) %>%
  select(year, liab_boy, atype)
write_csv(pdata, here::here("results", "amort.csv"))

p <- pdata %>%
  ggplot(aes(year, liab_boy, colour=atype)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_x_continuous(name="Year", breaks=c(1, seq(5, 100, 5))) +
  scale_y_continuous(name="Remaining liability", breaks=seq(0, 500, 50), labels=scales::dollar) +
  scale_colour_manual(values=cols) +
  ggtitle("Liability remaining at beginning of year, different amortization methods",
          subtitle="Initial liability of $100") +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Authors' calculations. All methods assume 7% interest rate. Level percent methods assume 3.5% payroll growth.") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "amort.png"), plot=p, width=figwidth, height=figheight, scale=1)



# Amortization policies in use ----
glimpse(ppd)
# don't use:
#   TotAmortPeriod missing
#   AssetValMeth_GASB too much text
# AssetSmoothingPeriod_GASB missing both years
# 188 obs
methods <- ppd %>%
  # filter(fy %in% 2010:2018) %>%
  select(ppd_id, PlanName, fy, StateAbbrev, 
         FundingMeth_GASB, AssetSmoothingPeriod_GASB,
         RemainingAmortPeriod, 
         ActFundedRatio_GASB, ActFundedRatio_GASB67, UAAL_GASB, NetPensionLiability,
         MktAssets_net, pctdollf, openclosedf, assetmethf)
summary(methods)

methods %>%
  filter(is.na(AssetSmoothingPeriod_GASB), fy==2017) %>%
  arrange(-MktAssets_net) %>%
  as.data.frame

svar <- "AssetSmoothingPeriod_GASB"
svar <- "pctdollf"
svar <- "openclosedf"
methods %>%
  filter(fy==2018) %>%
  rename(var=all_of(svar)) %>%
  filter(!is.na(var)) %>%
  mutate(groupvar=svar) %>%
  group_by(groupvar, var) %>%
  summarise(n=n(), assets=sum(MktAssets_net, na.rm=TRUE), uaal=sum(UAAL_GASB, na.rm=TRUE)) %>%
  janitor::adorn_totals() %>%
  mutate(npct=n / n[groupvar=="Total"] * 100,
         assetpct=assets / assets[groupvar=="Total"] * 100,
         uaalpct=uaal / uaal[groupvar=="Total"] * 100)

svar1 <- "pctdollf"
svar2 <- "openclosedf"
methods %>%
  filter(fy==2012) %>%
  rename(var1=all_of(svar1),
         var2=all_of(svar2)) %>%
  filter(!is.na(var1), !is.na(var2)) %>%
  mutate(groupvar1=svar1,
         groupvar2=svar2) %>%
  group_by(groupvar1, groupvar2, var1, var2) %>%
  summarise(n=n(), assets=sum(MktAssets_net, na.rm=TRUE), uaal=sum(UAAL_GASB, na.rm=TRUE)) %>%
  janitor::adorn_totals() %>%
  mutate(npct=n / n[groupvar1=="Total"] * 100,
         assetpct=assets / assets[groupvar1=="Total"] * 100,
         uaalpct=uaal / uaal[groupvar1=="Total"] * 100)



check <- methods %>%
  rename(var1=all_of(svar1),
         var2=all_of(svar2)) %>%
  filter(!is.na(var1), !is.na(var2)) %>%
  mutate(groupvar1=svar1,
         groupvar2=svar2) %>%
  group_by(fy, groupvar1, groupvar2, var1, var2) %>%
  summarise(n=n(), assets=sum(MktAssets_net, na.rm=TRUE), uaal=sum(UAAL_GASB, na.rm=TRUE)) %>%
  group_by(fy) %>%
  mutate(npct=n / sum(n) * 100,
         assetpct=assets / sum(assets) * 100,
         uaalpct=uaal / sum(uaal) * 100)
check %>%
  filter(var1=="levpercent", var2=="open") %>%
  ggplot(aes(fy, uaalpct)) +
  geom_line() +
  geom_point()


# Assumed investment returns and risk-free returns ----
rates.all <- readRDS("C:/RPrograms PC/Pensions/_MasterPresentation/data/rates.all.rds")
levs <- c("public", "private", "DGS10")
labs <- c("State-local average assumed return", "Private average assumed return", "10-year Treasury yield")

pdata <- rates.all %>% 
  filter(fyear >= 1990, fyear <= 2018) %>%
  select(-DGS30) %>%
  mutate(public=na.approx(public, na.rm=FALSE)) %>% # interpolate a few missing years
  pivot_longer(-fyear, names_to = "series") %>%
  mutate(seriesf=factor(series, levels=levs, labels=labs),
         fyear=as.integer(fyear),
         value=value / 100) %>%
  select(fyear, value, seriesf)
write_csv(pdata, here::here("results", "rates.csv"))

gtitle1 <- "Assumed investment returns and risk-free returns"
gtitle2 <- "Public and private retirement systems"
xlab <- "Pension fund fiscal year"

c1 <- "Public plan assumptions for 2001+ from Public Plans Database, Center for Retirement Research. Earlier years from multiple sources."
c2 <- "Private plan assumptions from Andonov, Aleksandar and Bauer, Rob and Cremers, Martijn, Pension Fund Asset Allocation and Liability Discount Rates (2016), via corresondence."
c3 <- "10-Year Treasury yield from Federal Reserve Bank of St. Louis (FRED)"
src <- paste0("\n", c1, "\n", c2, "\n", c3)

p <- ggplot(data=pdata, aes(x=fyear, y=value, colour=seriesf)) +
  geom_line(size=1) +
  geom_point(size=0.8) +
  scale_colour_manual(values = c("blue", "darkgreen", "grey")) +
  scale_y_continuous(name=NULL, breaks=seq(0, 0.30, 0.01), limits=c(0, NA), labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous(name=xlab, breaks=seq(1975, 2020, 5), limits=c(NA, 2021)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption=src) +
  ggtitle(label=gtitle1, subtitle=gtitle2) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.5))) +
  theme(legend.justification=c(0, 0), 
        legend.position=c(0.05, 0.15))
p
ggsave(here::here("results", "rates.png"), plot=p, width=figwidth, height=figheight, scale=1)

rates.all %>%
  filter(fyear %in% c(1990, 2000, 2005, 2010, 2012, 2015, 2018))


# Equity investment share ----
fof.a <- readRDS("C:/RPrograms PC/Pensions/_MasterPresentation/data/fof.a.rds")

pdata <- fof.a %>% 
  filter(year(date) >= 1990) %>% 
  mutate(equityshare=equityshare / 100,
         slgppf=factor(slgppf, 
                       levels=c("slgdb", "ppfdb"),
                       labels=c("State & local", "Private"))) %>%
  select(date, equityshare, slgppf)
write_csv(pdata, here::here("results", "equity_shares.csv"))

gtitle1 <- "Equity-like investments as percentage of invested assets"
gtitle2 <- "State and local government and private sector defined benefit pension plans"

src <- "\nSource: Authors' analysis of Z.1 Financial Accounts of the United States,\nFederal Reserve Board, Tables L.118.b, L.120.b, and L.122"

p <- pdata %>%
  ggplot(aes(x=date, y=equityshare, colour=slgppf)) +
  geom_line(size=1) +
  geom_point(size=0.8) +
  scale_y_continuous(name=NULL, breaks=seq(0, 1, 0.05), limits=c(0, NA), labels=scales::percent_format(accuracy=1)) +
  scale_x_date(name=NULL, breaks=seq.Date(as.Date("1900-01-01"), as.Date("2020-01-01"), "5 years"), date_labels = "%Y") +
  scale_colour_manual(values=c("blue", "darkgreen")) +
  guides(colour=guide_legend(title=NULL)) +
  labs(x=xlab, caption=src) +
  ggtitle(label=gtitle1, subtitle=gtitle2) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p
ggsave(here::here("results", "equity_shares.png"), plot=p, width=figwidth, height=figheight, scale=1)

