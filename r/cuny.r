
# notes re code folding:
#   alt-L, alt-shift-L  one section
#   alt-O, alt-shift-O  all sections
#   alt-R run current code section (remapped from Ctrl-alt-T)

# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(readr)

library(scales)
library(hms) # hms, for times
library(lubridate) # lubridate, for date/times
library(vctrs)

library(grDevices)
library(knitr)
library(kableExtra)

library(btools)
library(pdata)

library(DT) # for datatable

library(zoo) # for rollapply

library(fredr)


# globals -----------------------------------------------------------------
figwidth <- 8
figheight <- 6
# 8 6

# ppd api approach ----
library(httr)
library(jsonlite)

# callcsv <- "http://publicplansdata.org/api/?q=ListDataSets&format=csv"
# res = GET(callcsv)
# 
# call <- "http://publicplansdata.org/api/?q=ListDataSets&format=json"
# res = GET(call)
# str(res)
# str(res$content)
# data = fromJSON(rawToChar(res$content))
# data |> as_tibble()

#.. api documentation ----
# https://publicplansdata.org/public-plans-database/api/

#.. datasets ----
call <- "http://publicplansdata.org/api/?q=ListDataSets&format=json"
res <- jsonlite::fromJSON(call) 
res
dim(res)
res[1, ]
res[-1, 6:8]
dsets <- res[-1, 6:8] |> 
  as_tibble()
ht(dsets)

#.. variable list ----
call <- "http://publicplansdata.org/api/?q=ListVariables&format=json"
res <- jsonlite::fromJSON(call) 
res
dim(res)
res[1, ]
res[-1, 6:8]
vars <- res[-1, -c(1:5)] |> 
  as_tibble()
ht(vars)
vars |> filter(FieldName=="InvestmentReturnAssumption_GASB")
dsets |> filter(id==4)
vars |> filter(str_detect(FieldName, coll("plan", ignore_case=TRUE)))

#..vars ----
callbase <- "https://publicplansdata.org/api/?q=QVariables&variables="
vnames <- "fy,PlanName,InvestmentReturnAssumption_GASB"
fmt <- "&format=json"
call <- paste0(callbase, vnames, fmt)
call
res <- jsonlite::fromJSON(call) 
res
dim(res)
res[1, ]
data <- res[-1, -c(1:5)] |> 
  as_tibble() |> 
  mutate(fy=as.integer(fy),
         air=as.numeric(InvestmentReturnAssumption_GASB))

#.. quantiles of assumed returns ----

qtiles <- c(0, .1, .25, .5, .75, .9, 1)
q <- data |>
  filter(fy>=2001) |> # start of real data
  arrange(fy) |> 
  summarise(n=n(), 
            nnmiss=sum(!is.na(air)),
            res=list(quantile(air, probs = qtiles, na.rm=TRUE)), .by = fy) |> 
  unnest_wider(col = c(res)) |> 
  mutate(d9010=`90%` - `10%`)

q |> 
  pivot_longer(-c(fy, n, nnmiss)) |> 
  # filter(!name %in% c("0%", "100%")) |>
  filter(name %in% c("10%", "50%", "90%")) |> 
  ggplot(aes(fy, value, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 1, .005),
                     labels = label_percent(),
                     limits = c(0, NA_real_)) +
  theme_bw()

# irs
# https://www.irs.gov/retirement-plans/minimum-present-value-segment-rates
# https://www.irs.gov/retirement-plans/archive-of-published-guidance
# https://www.irs.gov/retirement-plans/interest-rates-tables
# https://www.irs.gov/retirement-plans/pension-plan-funding-segment-rates

# https://www.irs.gov/retirement-plans/historical-funding-segment-rate-tables


# ppd ---------------------------------------------------------------------
# download the file manually from here: 
#  https://publicplansdata.org/public-plans-database/download-full-data-set/#
# it looks like the file is created on the fly from an api call

url <- "https://publicplansdata.org/public-plans-database/download-full-data-set/#"
url <- "https://publicplansdata.org/api/ppd-data-latest.csv"
url <- "https://publicplansdata.org/ppd-data-latest.csv"
url <- "https://publicplansdata.org/public-plans-database/download-full-data-set/ppd-data-latest.csv"
url <- "https://publicplansdata.org/public-plans-database/download-full-data-set/#/ppd-data-latest.csv"

url <- "https://publicplansdata.org/public-plans-database/download-full-data-set/previous-version-downloads/#"

url <- "https://publicplansdata.org/public-plans-database/download-full-data-set/previous-version-downloads/#/ppd-data-2023-04-07.csv"

download.file(url, here::here("data", "temp.csv"))

ppd1 <- read_csv(here::here("data", "ppd-data-latest.csv"))
ns(ppd1)
glimpse(ppd1)
str_subset(names(ppd1), coll("asset", ignore_case = TRUE))

ppd2 <- ppd1 |> 
  select(ppd_id, PlanName, fy,
         InvestmentReturnAssumption_GASB,
         MktAssets_net, MktAssets_ActRpt)
summary(ppd2)

library(ergm)

ppd3 <- ppd2 |> 
  rename(fyear=fy) |> 
  filter(fyear>=2001) |> 
  summarise(n=n(), 
            # public=median(InvestmentReturnAssumption_GASB, na.rm=TRUE),
            public=weighted.mean(InvestmentReturnAssumption_GASB,
                                  na.rm = TRUE, 
                                  weight = MktAssets_net),
            .by=fyear)

ppd3 |> 
  filter(fyear >= 2001)


# private rates -----------------------------------------------------------
private <- read_csv(
  "fyear, private
  1993, 8.214
  1994, 8.145
  1995, 7.987
  1996, 7.972
  1997, 7.71
  1998, 7.495
  1999, 7.961
  2000, 8.052
  2001, 7.761
  2002, 7.469
  2003, 7.267
  2004, 6.816
  2005, 6.643
  2006, 6.472
  2007, 6.307
  2008, 6.512
  2009, 6.181
  2010, 5.721
  2011, 5.112
  2012, 4.36") |> 
  mutate(private=private / 100)


# 10-year treasury --------------------------------------------------------
dgs10 <- fredr(series_id="DGS10",
               frequency = "a")
dgs10b <- dgs10 |> 
  mutate(fyear=year(date),
         DGS10=value / 100) |> 
  select(fyear, DGS10)

moodysaaa  <- fredr(series_id="AAA",
              frequency = "a")

moodysaaa2 <- moodysaaa |> 
  mutate(fyear=year(date),
         aaa=value / 100) |> 
  select(fyear, aaa)

moodysaaa2 |> 
  filter(fyear >= 2000) |> 
  ggplot(aes(fyear, aaa)) +
  geom_line() +
  geom_point()

  

# private slg plot --------------------------------------------------------

# Assumed investment returns and risk-free returns ----
# rates.all <- readRDS("C:/RPrograms PC/Pensions/_MasterPresentation/data/rates.all.rds")
rates <- readRDS(here::here("data", "rates.all.rds"))
# use public for years through 2000, and private


# # count(rates, series)
# rates2 <- rates |> 
#   filter(series %in% c("nycrf", "survey")) |> 
#   select(fyear, value, series) |> 
#   mutate(value=value / 100) |> 
#   pivot_wider(names_from = series)
  
pdata1 <- tibble(fyear=1990:2022) |> 
  left_join(rates |> 
              select(fyear, public1=public, private) |> 
              mutate(public1=public1 / 100, 
                     private=private / 100), 
            by = join_by(fyear)) |> 
  left_join(ppd3 |> select(fyear, public2=public), by = join_by(fyear)) |> 
  left_join(dgs10b, by = join_by(fyear)) |> 
  mutate(public=ifelse(fyear < 2001,
                       public1,
                       public2),
         public=na.approx(public, na.rm=FALSE)) |>  # interpolate a few missing years) |> 
  select(fyear, public, private, DGS10)
  

levs <- c("public", "private", "DGS10")
labs <- c("State-local average assumed return", "Private average assumed return", "10-year Treasury yield")

pdata <- pdata1 |> 
  filter(fyear >= 1990) |> 
  pivot_longer(-fyear, names_to = "series") |> 
  mutate(seriesf=factor(series,
                        levels=levs, 
                        labels=labs),
         fyear=as.integer(fyear)) %>%
  select(fyear, value, seriesf)
# write_csv(pdata, here::here("results", "rates.csv"))

gtitle1 <- "Assumed investment returns and risk-free returns"
gtitle2 <- "Public and private retirement systems"
xlab <- "Pension fund fiscal year"

c1 <- "Public plan assumptions for 2001+ from Public Plans Database, Center for Retirement Research. Earlier years from multiple sources."
c2 <- "Private plan assumptions from Andonov, Aleksandar and Bauer, Rob and Cremers, Martijn, Pension Fund Asset Allocation and Liability Discount Rates (2016), via corresondence."
c3 <- "10-Year Treasury yield from Federal Reserve Bank of St. Louis (FRED)"
src <- paste0("\n", c1, "\n", c2, "\n", c3)

p <- pdata |> 
  ggplot(aes(x=fyear, y=value, colour=seriesf)) +
  geom_line(linewidth=1) +
  geom_point(size=0.8) +
  scale_colour_manual(values = c("blue", "darkgreen", "grey")) +
  scale_y_continuous(name=NULL,
                     breaks=seq(0, 0.30, 0.005),
                     limits=c(0, NA),
                     labels=scales::percent_format(accuracy=.1)) +
  scale_x_continuous(name=xlab, breaks=seq(1975, 2025, 2)) + # , limits=c(NA, 2022)
  guides(colour=guide_legend(title=NULL)) +
  labs(caption=src) +
  ggtitle(label=gtitle1, subtitle=gtitle2) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=rel(.5))) +
  theme(legend.justification=c(0, 0), 
        legend.position=c(0.05, 0.15))
p

ggsave(here::here("results", "rates.png"), plot=p, width=11, height=6, scale=1)


# fof ---------------------------------------------------------------------
# https://www.federalreserve.gov/releases/z1/default.htm
# https://www.federalreserve.gov/releases/z1/20221209/html/default.htm
library(fs)

zpath <- here::here("data", "z1_csv_files.zip")

# FL224090045.Q
# State and local government employee defined benefit retirement funds; total financial assets
# Line 1	Table L.120.b State and Local Government Employee Retirement Funds: Defined Benefit Plans
# Millions of dollars; not seasonally adjusted
vars <- read_tsv(unz(zpath, "data_dictionary/l120b.txt"),
                 col_names = c("series", "description", "line", "table", "units"))
glimpse(vars)

df1 <- read_csv(unz(zpath, "csv/l120b.csv"))
glimpse(df1)

df2 <- df1 |> 
  pivot_longer(-date, names_to="series") |> 
  mutate(date=yq2(str_sub(date, 1, 4), str_sub(date, 7, 7)),
         value=as.numeric(value)) |> 
  left_join(vars,
            by = join_by(series))
  
x <- "1945:Q4"
z <- yq2(str_sub(x, 1, 4), str_sub(x, 7, 7))
as.Date()
qday("1945:Q4")



# pctgdp ------------------------------------------------------------------
gdp1 <- fredr(series_id="GDP",
               frequency = "q")
gdp <- gdp1 |> 
  select(date, gdp=value)
gdp

pdata <- df2 |> 
  filter(series=="FL223073045.Q") |> 
  select(date, ufl=value) |> 
  left_join(gdp, by = join_by(date)) |> 
  mutate(pct=ufl / (gdp * 1000))

pdata |> 
  ggplot(aes(date, pct)) +
  geom_line()

gtitle1 <- "Unfunded liability of state and local government defined benefit pension plans"
gtitle2 <- "As percentage of Gross Domestic Product"
srcnote <- "\nSource: Federal Reserve Board, Financial Accounts of the United States, Tables L.120.b and F.2
Note: Liabilities are as valued by the Bureau of Economic Analysis, not actuaries."
p <- pdata  |> 
  filter(year(date)>=1955, year(date)<=2022) |> 
  ggplot(aes(x=date, y=pct)) +
  geom_line(linewidth=rel(1), colour="blue") +
  geom_point(size=rel(1), colour="blue") +
  geom_hline(yintercept = 0, linewidth=rel(1.1), colour="black") +
  scale_y_continuous(name="Percent (%)", breaks=seq(-1, 1, .02),
                     labels = label_percent(accuracy=.1)) +
  scale_x_date(name=NULL,
               breaks=c(seq.Date(as.Date("1900-01-01"),
                                 as.Date("2030-01-01"),
                                 "5 years")),
               minor_breaks = seq.Date(as.Date("1900-01-01"),
                                       as.Date("2030-01-01"),
                                       "1 year"),
               date_labels = "%Y") +
  labs(caption=srcnote) +
  ggtitle(label=gtitle1, subtitle=gtitle2) +
  theme_bw() +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  theme(panel.grid.major.x = element_line(size=0.3, colour="lightgrey"),
        panel.grid.minor.x = element_line(linetype="dashed", size=0.1, colour="#d9d9d9"))
p
ggsave("./results/ufl_gdp.png", p, width=11, height=6, units="in")

gdp |> 
  filter(year(date) %in% c(2010, 2021))

  
# equity investments ------------------------------------------------------

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




# OLD BELOW HERE ----------------------------------------------------------





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

