# ---- PAM : METS Forecast ----

data.x = data %>%
        filter(input=="yoy") %>%
        group_by(COUNTRY,INDICATOR,Age5G) %>%
        mutate(val=ifelse(d.break==1,
                          mean(val[source=="OBS"],
                               na.rm=TRUE),
                          val)) %>%
        ungroup() %>%
        mutate(INDICATOR=paste0(INDICATOR,
                                ifelse(INDICATOR=="AROP",
                                       paste0(".",Age5G),""))) %>%
        select(file,COUNTRY,YEAR,INDICATOR,source,val,LY.OBS)

data.x = data.x %>%
        filter(file=="POV") %>%
        group_by(COUNTRY,YEAR) %>%
        summarize(n.miss=sum(is.na(val))) %>%
        right_join(data.x) %>%
        filter(n.miss==0 | YEAR>LY.OBS) %>%
        select(-c(file,n.miss)) %>%
        mutate(source=ifelse(YEAR<=LY.OBS,"OBS","FXT"))

data.x = data.x %>%
        group_by(COUNTRY,INDICATOR) %>%
        summarize(n.miss=sum(is.na(val) & YEAR>LY.OBS)) %>%
        right_join(data.x) %>%
        mutate(val=ifelse(n.miss>0,0,val)) %>%
        select(-n.miss) %>%
        filter(!is.na(val)) %>%
        pivot_wider(names_from=INDICATOR,
                    values_from=val) %>%
        mutate(source=ifelse(YEAR<=LY.OBS,"OBS","FXT")) %>%
        as_tsibble(index=YEAR,
                   key=COUNTRY)

#v.dep="AROP.25-49"

F.pam.FXT = function (v.dep) {
       model = data.x %>%
                filter(YEAR<=LY.OBS) %>%
                rename(v.dep=!!rlang::sym(v.dep)) %>%
                model(fable::ARIMA(formula(paste0("v.dep ~ ",
                                                  paste(names(var.XPL),collapse=" + "),
                                                  " + PDQ(0,0,0)"))))
       
       data.est = model %>%
               fitted() %>%
               unnest() %>%
               rename(val=.fitted) %>%
               mutate(source="EST",
                      model="direct",
                      input="yoy",
                      name=v.dep,
                      d.break=0) %>%
               separate(col=name,
                        sep="\\.",
                        into=c("INDICATOR","Age5G")) %>%
               mutate(file=ifelse(INDICATOR=="AROP","POV","POZ")) %>%
               select(-starts_with("."))
       
       data.fxt = model %>%
               forecast(new_data=data.x %>%
                                filter(YEAR>LY.OBS) %>%
                                select(COUNTRY,YEAR,
                                       any_of(names(var.XPL)))) %>%
               #hilo(.distribution,level=95) %>%
               #unnest() %>%
               rename(.distribution=v.dep,
                      val=.mean) %>%
               mutate(source="FXT",
                      input="yoy",
                      model="direct",
                      name=v.dep,
                      d.break=0) %>%
               separate(col=name,
                        sep="\\.",
                        into=c("INDICATOR","Age5G")) %>%
               mutate(file=ifelse(INDICATOR=="AROP","POV","POZ")) %>%
               select(-any_of(names(var.XPL))) %>%
               filter(!COUNTRY %in% countries.out) %>%
               select(-starts_with("."))
       
       bind_rows(data.est,
                 data.fxt)
}

FXT=as.data.frame(bind_rows(lapply(names(data.x)[!names(data.x) %in% c("COUNTRY","YEAR","source","LY.OBS",
                                            names(var.XPL))],
                     F.pam.FXT))) %>%
        rename_with(~gsub("value.","",.x)) %>%
        mutate(Age5G=replace_na(Age5G,"TOTAL"),
               source=replace_na(source,"OBS"))

rm(data.x)
