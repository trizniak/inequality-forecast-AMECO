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


# AMECO ----
source(here("OUTILS","FUNS","DATA_AMECO.R"))


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
  right_join(data) %>%
  mutate(source=ifelse(YEAR<=LY.OBS,"OBS","FXT"))
data = data %>%
  group_by(COUNTRY,INDICATOR) %>%
  summarize(firstY=min(YEAR,na.rm=TRUE)) %>%
  right_join(data) %>%
  mutate(d.break=replace_na(d.break,0),
         d.break=ifelse(YEAR==firstY,0,d.break)) %>%
  ungroup() %>%
  select(-firstY)


# * Add METS forecast ----
source(here("OUTILS","FUNS","Fpam_METS_FXT.R"))
data = data %>%
  bind_rows(FXT)


# * Fill in level based on estimated (fitted) YoY ----
data = data %>%
  mutate(name=paste0(".",input,".",source)) %>%
  select(-c(input,source,LY.OBS)) %>%
  pivot_wider(names_from=name,
              values_from=val) %>%
  group_by(COUNTRY,INDICATOR,Age5G) %>%
  mutate(.level.EST=ifelse(INDICATOR=="AROP",
                           lag(.level.OBS,1,
                               order_by=YEAR)+.yoy.EST,
                           lag(.level.OBS,1,
                               order_by=YEAR)*(1+.yoy.EST/100))) %>%
  pivot_longer(cols=starts_with("."),
               names_to="name",
               values_to="val") %>%
  filter(!is.na(val)) %>%
  mutate(name=sub(".","",name)) %>%
  separate(col=name,
           sep="\\.",
           into=c("input","source"))%>%
  pivot_wider(names_from=input,
              values_from=val)

data = data %>%
  filter(file %in% c("POV","POZ"),
         source=="OBS") %>%
  group_by(COUNTRY) %>%
  summarize(LY.OBS=max(YEAR,na.rm=TRUE)) %>%
  right_join(data) %>%
  group_by(COUNTRY,INDICATOR,Age5G) %>%
  arrange(YEAR)


# * Fill in level based on forecasted YoY ----
for (i in 1:(max(data$YEAR)-min(data$LY.OBS,na.rm=TRUE)+1)) {
  data = data %>%
    mutate(level=ifelse(is.na(level) & source=="FXT",
                        ifelse(INDICATOR=="AROP",
                               lag(level,1,
                                   order_by=YEAR)+yoy,
                               lag(level,1,
                                   order_by=YEAR)*(1+yoy/100)),
                        level))
}


# * Add composite forecast ----
source(here("OUTILS","FUNS","Fpam_compFXT.R"))
data = bind_rows(mget(ls(pattern="^data.cf"))) %>%
  bind_rows(data)
  

# * SAVE ----
data = data %>%
  pivot_longer(cols=c(level,yoy),
               names_to="input",
               values_to="val") %>%
  group_by(COUNTRY,INDICATOR,input,Age5G) %>%
  arrange(YEAR) %>%
  mutate(break.g=cumsum(d.break==1)) %>%
  ungroup() %>%
  mutate(INDICATOR=factor(INDICATOR,
                          levels=c("AROP","D1","D3","MEDIAN","D7","D9",
                                   names(var.XPL))),
         model=factor(as.character(replace_na(model,"OBS")),
                      levels=c("OBS","direct","deciles","ageg"),
                      labels=c("Observed","Direct","Decile-based","Age-group Composite"))) %>%
  filter(!is.na(val),
         !is.na(INDICATOR))
save(data,file=here("DATA","data.Rdata"))


rm(list=c("FXT","POP",ls(pattern="^data"))) # use a caret ^ to match the beginning of the string
