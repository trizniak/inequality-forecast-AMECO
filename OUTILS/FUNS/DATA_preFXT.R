# ---- SETUP ----
source("./OUTILS/CONFIG/SETUP.R");source("./OUTILS/CONFIG/SETUP.R")


# EUROSTAT ----
F.data.estat = function(datafile,lag=0) {
  # http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di01&lang=en
  get_eurostat(datafile,
               time_format="num",
               keepFlags=TRUE) %>%
    rename(COUNTRY=geo,
           YEAR=time,
           level=values) %>%
    filter(COUNTRY %in% country.list,
           YEAR>=2005,
           !is.na(level)) %>%
    mutate(YEAR=YEAR-lag,
           d.break=ifelse(substr(flags,1,1)=="b",1,NA),
           COUNTRY=as.character(COUNTRY),
           source="OBS")
}

# * Deciles -----
data.X = F.data.estat("ilc_di01",lag=1) %>%
  filter(quantile %in% c("D1","D3","D5","D7","D9"),
         indic_il=="TC",
         currency=="EUR") %>%
  mutate(INDICATOR=as.character(recode_factor(quantile,D5="MEDIAN")),
         file="POZ") %>%
  group_by(COUNTRY,INDICATOR) %>%
  mutate(yoy=100*(level/lag(level,order_by=YEAR)-1)) %>%
  ungroup() %>%
  pivot_longer(cols=c(level,yoy),
               values_to="val",
               names_to="input")

# * AROP ----
data.A = F.data.estat("ilc_li02",lag=1) %>%
  filter(unit=="PC",
         indic_il=="LI_R_MD60",
         sex=="T",
         age %in% c("TOTAL",
                    "Y_LT16",
                    "Y16-24",
                    "Y25-49",
                    "Y50-64",
                    "Y_GE65")) %>%
  group_by(COUNTRY,age) %>%
  mutate(yoy=level-lag(level,order_by=YEAR)) %>%
  ungroup() %>%
  pivot_longer(cols=c(level,yoy),
               values_to="val",
               names_to="input") %>%
  mutate(INDICATOR="AROP",
         file="POV",
         Age5G=as.character(recode_factor(age,
                                          `Y_LT16`="<16",
                                          `Y16-24`="16-24",
                                          `Y25-49`="25-49",
                                          `Y50-64`="50-64",
                                          `Y_GE65`="65+")))


# * GDP : Main GDP aggregates per capita [nama_10_pc] ----
# NA_ITEM : B1GQ	Gross domestic product at market prices
# UNIT : CLV_PCH_PRE_HAB	Chain linked volumes, percentage change on previous period, per capita
data.G = F.data.estat("nama_10_pc") %>%
  filter(na_item=="B1GQ",
         unit=="CLV_PCH_PRE_HAB") %>%
  rename(val=level) %>%
  mutate(INDICATOR="GDPpc",
         input="yoy",
         file="XPL")


# * Employment : Population and employment [nama_10_pe] ----
# NA_ITEM : EMP_DC Total employment domestic concept
# UNIT : PCH_PRE_PER	Percentage change on previous period (based on persons)
data.E = F.data.estat("nama_10_pe") %>%
  filter(na_item=="EMP_DC",
         unit=="PCH_PRE_PER") %>%
  rename(val=level) %>%
  mutate(INDICATOR="EMP",
         input="yoy",
         file="XPL")


# * Nominal unit labour cost [nama_10_lp_ulc]----
# NA_ITEM : NULC_PER	Nominal unit labour cost based on persons
# UNIT : PCH_PRE Percentage change on previous period
data.L = F.data.estat("nama_10_lp_ulc") %>%
  filter(na_item=="NULC_PER",
         unit=="PCH_PRE") %>%
  rename(val=level) %>%
  mutate(INDICATOR="ULC",
         input="yoy",
         file="XPL")

# * Employee Compensation per capita ----

# Compensation of employees : GDP and main components (output, expenditure and income) [nama_10_gdp]
# NA_ITEM : D1 Compensation of employees
# UNIT : CP_MEUR Current prices, million EUR

# Employment : Population and employment [nama_10_pe] ----
# NA_ITEM : EMP_DC Total employment domestic concept
# UNIT : THS_PER Thousand persons
#data.C = bind_rows(F.data.estat("nama_10_gdp") %>%
#                     filter(na_item=="D1",
#                            unit=="CP_MEUR") %>%
#                     mutate(unit="EUR"),
#                   F.data.estat("nama_10_pe") %>%
#                     filter(na_item=="EMP_NC",
#                            unit=="THS_PER") %>%
#                     mutate(unit="pers")) %>%
#  select(-c(na_item,flags)) %>%
#  pivot_wider(names_from=unit,
#              values_from=level) %>%
#  group_by(COUNTRY) %>%
#  mutate(INDICATOR="EmpC",
#         input="yoy",
#         file="XPL",
#         level=EUR/pers,
#         val=100*(level/lag(level,order_by=YEAR)-1)) %>%
#  select(-c(EUR,pers,level)) %>%
#  ungroup() %>%
#  arrange(COUNTRY,YEAR)


# * Population : Population on 1 January by age and sex [demo_pjan] ----
POP = F.data.estat("demo_pjan") %>%
  filter(sex=="T",
         str_detect(age,"Y")) %>%
  mutate(age=as.character(recode(age,
                                 Y_LT1="Y0",
                                 Y_OPEN="Y100")),
         age=as.numeric(str_remove(age,"Y")),
         Age5G=case_when(age<16 ~ "<16",
                         age>=16 & age<=24 ~ "16-24",
                         age>=25 & age<=49 ~ "25-49",
                         age>=50 & age<=64 ~ "50-64",
                         age>=65 ~ "65+")) %>%
  group_by(COUNTRY,YEAR,Age5G) %>%
  summarise(level=sum(level),
            d.break=as.numeric(sum(d.break,na.rm=TRUE)>0)) %>%
  ungroup() %>%
  select(COUNTRY,YEAR,Age5G,level,d.break)
POP = POP %>%
  group_by(COUNTRY,YEAR) %>%
  summarise(TOTAL=sum(level)) %>%
  ungroup() %>%
  right_join(POP) %>%
  mutate(sh.pop=level/TOTAL) %>%
  select(-c(TOTAL,level))


# FXT predictors ----
# https://ec.europa.eu/info/sites/info/files/economy-finance/ecfin_forecast_spring_2020_statist_annex_en.pdf
data.FXT = read_delim("DATA/EC_FXT2005.txt","\t",
                      escape_double=FALSE,
                      trim_ws=TRUE) %>%
  left_join(EU.PO) %>%
  select(-c(Country.Name,Protocol.Order)) %>%
  pivot_longer(-c(COUNTRY,INDICATOR),
               values_to="val") %>%
  separate(col=name,
           into=c("YEAR","fxt")) %>%
  mutate(YEAR=as.numeric(YEAR))
save(data.FXT,file=here("DATA","data_FXT.Rdata"))
data.FXT = data.FXT %>%
  filter(fxt=="S20",
         as.numeric(YEAR)>2019) %>%
  mutate(source="FXT",
         input="yoy",
         file="XPL")


# ---- DATA ----
data = bind_rows(mget(ls(pattern="^data."))) %>%
  select(file,COUNTRY,YEAR,INDICATOR,Age5G,input,source,val,d.break) %>%
  filter(COUNTRY !="EU") %>%
  mutate(Age5G=replace_na(Age5G,"TOTAL"))
data = data %>%
  filter(file %in% c("POV","POZ"),
         source=="OBS") %>%
  group_by(COUNTRY) %>%
  summarize(LY.OBS=max(YEAR,na.rm=TRUE)) %>%
  right_join(data) # %>% mutate(source=ifelse(YEAR>LY.OBS & file=="XPL","FXT",source))
data = data %>%
  group_by(COUNTRY,INDICATOR) %>%
  summarize(firstY=min(YEAR,na.rm=TRUE)) %>%
  right_join(data) %>%
  mutate(d.break=replace_na(d.break,0),
         d.break=ifelse(YEAR==firstY,0,d.break)) %>%
  ungroup() %>%
  select(-firstY)


rm(list=ls(pattern="^data."))
