# ---- DATA : AMECO ----
# http://pages.stern.nyu.edu/~dbackus/BCH/data/AMECO/ameco.R
# [AMECO Online] (https://ec.europa.eu/economy_finance/ameco/user/serie/SelectSerie.cfm)

download.file("http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip",
              destfile=here("DATA",
                            "AMECO.zip"),
              method="auto")

# for unzip to work : https://cran.r-project.org/bin/windows/Rtools/
unzip(here("DATA",
           "AMECO.zip"),
      unzip="unzip",
      exdir=here("DATA"),
      junkpaths=TRUE)

data.AMECO = do.call(
  rbind,
  lapply(as.list(Sys.glob(paste("DATA","AMECO*.TXT",sep="/"))),
         readr::read_delim,
               delim=";",
               trim_ws=TRUE)) %>%
  select(-starts_with("x")) %>%
  left_join(EU.PO %>%
              rename(COUNTRY.CODE=COUNTRY),
            by=c("COUNTRY"="Country.Name")) %>%
  select(-c(COUNTRY,Protocol.Order)) %>%
  rename(COUNTRY=COUNTRY.CODE) %>%
  filter(!is.na(COUNTRY)) %>%
  pivot_longer(-c(CODE,COUNTRY,'SUB-CHAPTER',TITLE,UNIT),
               names_to="YEAR",
               values_to="level") %>%
  mutate(YEAR=as.numeric(YEAR)) %>%
  filter(YEAR>=2004) %>%
  separate(col=CODE,
           into=c(paste0("CODE.",1:6)),
           remove=FALSE,
           convert=TRUE) %>%
  rename(INDICATOR=CODE.6) %>%
  select(-starts_with("CODE")) %>%
  filter(INDICATOR %in% c("NPTD",names(var.XPL)),
         UNIT %in% c("1000 persons","Mrd ECU/EUR","(1000 EUR)","(EUR: 2015 = 100)"))

var.XPL.long = data.AMECO %>%
  mutate(description=paste0("* **",INDICATOR,
                            "** : ",var.XPL[INDICATOR],
                            "<br><i>",TITLE,
                            " - UNIT : [",gsub("[()]","",UNIT),"]","</i>",
                            "\n")) %>%
  select(INDICATOR,description) %>%
  filter(INDICATOR %in% names(var.XPL)) %>%
  unique() %>%
  pull(description,INDICATOR)

data.AMECO = data.AMECO %>%
  filter(INDICATOR !="NPTD") %>%
  left_join(data.AMECO %>%
              filter(INDICATOR=="NPTD") %>%
              rename(POP=level) %>%
              select(COUNTRY,YEAR,POP)) %>%
  mutate(level=ifelse(INDICATOR %in% c("UWSH","UYNH","UCTRH"),
                    1000*level/POP,
                    level),
         file="XPL") %>%
  group_by(COUNTRY,INDICATOR) %>%
  mutate(yoy=100*(level/lag(level,order_by=YEAR)-1)) %>%
  ungroup() %>%
  pivot_longer(cols=c(level,yoy),
               values_to="val",
               names_to="input") %>%
  select(file,COUNTRY,YEAR,INDICATOR,input,val)


save(data.AMECO,file=here("DATA","data_AMECO.Rdata"))

file.remove(Sys.glob(paste("DATA","AMECO*.*",sep="/")))
