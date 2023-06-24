
library(purrr)

# fget <- "https://www.federalreserve.gov/releases/z1/20190920/z1_csv_files.zip"
fget <- "https://www.federalreserve.gov/releases/z1/20230608/z1_csv_files.zip"
zfn <- path_file(fget)
zpath <- here::here("data", zfn)
temp <- tempfile()
download.file(fget, temp, mode="wb")
# download.file(fget, zpath, mode="wb")

lfiles <- unzip(temp, list=TRUE) # data frame of files in the zip archive
lfiles
# csvfiles <- str_subset(lfiles$Name, "csv/")
# ddfiles <- str_subset(lfiles$Name, "data_dictionary/")

tmpdir <- here::here("data", "temp")
# tmpdir <- tempdir()
unzip(zpath, exdir=tmpdir)
csvdir <- path(tmpdir, "csv")
dictdir <- path(tmpdir, "data_dictionary")
dictfiles <- dir_ls(dictdir)
csvfiles <- dir_ls(csvdir)


## get the dictionary files ----
fdict <- function(fn) {
  print(fn)
  cnames <- c("name", "description", "location", "table", "units")
  tmp <- read_tsv(fn, col_names=cnames)
  tmp <- tmp %>% mutate(src=path_file(fn))#,
                        #variable=str_sub(variable, 1, -3))
  return(tmp)
}

dictdf <- purrr::map(dictfiles, fdict, .progress=TRUE) |> 
  list_rbind()
saveRDS(dictdf, here::here("data", "fofdict_all.rds"))

ddslim1 <- dictdf |> 
  arrange(name, table) |> 
  mutate(n=n(), ndesc=length(unique(description)), .by=name)
tmp <- ddslim1 |> 
  filter(n > 1)

ddslim2 <- dictdf |> 
  select(name, description, units) |> 
  distinct()
saveRDS(ddslim2, here::here("data", "fofdict_slim.rds"))



## get the csv files ----

fcsv <- function(fn) {
  print(fn)
  
  # files have duplicate column names
  tmp <- vroom(unz(temp, fn), 
               col_types = cols(.default = col_character()),
               .name_repair="minimal") # much faster if we use minimual
  
  freq <- str_sub(names(tmp)[2], -1) # last character of first variable name in the file
  
  tmpl1 <- tmp |>
    pivot_longer(-date) |> 
    distinct() |> # get rid of duplicates
    mutate(value=as.numeric(value),
           src=path_file(fn), 
           freq=freq)
  
  if(freq=="A"){
    tmpl <- tmpl1 |> 
      mutate(date=as.Date(paste0(date, "-01-01")))
  } else if(freq=="Q") {
    tmpl <- tmpl1 |> 
      mutate(date=as.Date(as.yearqtr(date, format = "%Y:Q%q")))
  } else {
    print(freq)
    tmpl <- tmpl1
  }
  return(tmpl)
}

csvdf <- purrr::map(csvfiles, fcsv, .progress=TRUE) |> 
  list_rbind()

saveRDS(csvdf, here::here("data", "fof_all.rds"))

csvslim <- csvdf |> 
  select(date, name, value, freq) |> 
  filter(!is.na(value)) |> 
  distinct()
saveRDS(csvslim, here::here("data", "fof_slim.rds"))

## combine the slim files ----
csvslim <- readRDS(here::here("data", "fof_slim.rds"))
ddslim <- readRDS(here::here("data", "fofdict_slim.rds"))

fof <- csvslim |> 
  left_join(ddslim, by = join_by(name))
skim(fof)
tmp <- fof |> filter(is.na(description))

saveRDS(fof, here::here("data", "fof.rds"))


# vnames for pensions ----

vnames <- vroom(I("
# general_variables
FA216210001, other_slgperscurtax
FA216240001, other_slgprodimptax
FA216231001, other_slgcorptax
FA086902005, other_gdp
FL213162005, other_munisec
FL213162400, other_stdebt
FL214090005, other_slgfinass
FL214190005, other_slgfinliab
LM653064100, other_mfcorpequity
LM654090000, other_mfassets

# private_DB_pension_funds
FL574090045, ppfdb_finassets
FL573065043, ppfdb_mortgages
LM573064143, ppfdb_corpequity
LM573064243, ppfdb_mfshares
FL573073005, ppfdb_claims
FL573093043, ppfdb_otherassets
FL574190043, ppfdb_entitlement

# SLG_DB_funds
FL224090045, slgdb_finassets
FL223065043, slgdb_mortgages
LM223064145, slgdb_corpequity
LM223064243, slgdb_mfshares
FL223073045, slgdb_claims
LM223093043, slgdb_otherassets
FL224190043, slgdb_entitlement
"), delim=",", comment="#", col_names=c("snamex", "shortname"))
vnames

fof <- readRDS(here::here("data", "fof.rds"))

df <- fof |> 
  filter(name %in% paste0(vnames$snamex, ".Q"))




# get the tables that the variables of interest appear in, to make documentation easier
glimpse(fof)
tabs <- fof %>% filter(variable %in% vnames$snamex) %>%
  group_by(freq, table, lineno) %>%
  filter(date==max(date)) %>%
  ungroup %>%
  select(freq, date, variable, table, lineno, description, units, value) %>%
  arrange(freq, variable, table, lineno)


# quarterly data calculations ####
df.q <- df
dfother.q <- filter(df.q, slgppf=="other") %>%
  spread(vname, value) %>%
  mutate(mfstockshare=mfcorpequity / mfassets, # economywide share of mutual fund assets in corp equities
         slgtax=slgperscurtax + slgprodimptax + slgcorptax)

# redirect has disappeared from the data, so compute
fof.q <- df.q %>% filter(slgppf!="other") %>%
  spread(vname, value) %>%
  left_join(select(dfother.q, date, gdp, mfstockshare, slgtax)) %>%
  mutate(invassets=entitlement - claims,
         redirect=invassets - (finassets - claims), # no longer a reported variable
         fr.mv=invassets / entitlement * 100,
         equity = corpequity + (mfshares + otherassets) * mfstockshare + redirect,
         equityshare = equity / invassets * 100,
         equityslgtax = equity / slgtax * 100,
         equitygdpshare = equity / gdp *100)

