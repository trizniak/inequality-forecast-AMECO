# ---- PAM : Composite Forecast ----

# * based on ARPT ----

data.x = data %>%
        ungroup() %>%
        pivot_longer(cols=c(level,yoy),
                     names_to="input",
                     values_to="val") %>%
        filter(input=="level",
               Age5G=="TOTAL",
               !INDICATOR %in% c("D7","D9"),
               !is.na(val),
               !is.na(source),
               d.break !=1) %>%
        select(COUNTRY,YEAR,INDICATOR,source,val) %>%
        pivot_wider(names_from=INDICATOR,
                    values_from=val) %>%
        mutate(ARPT=100*(MEDIAN*0.6-D1)/(D3-D1))
data.x = data.x %>%
        group_by(COUNTRY) %>%
        do({
                mod=lm(AROP~ARPT, data=filter(.,
                                              source=="OBS"))
                data.frame(intercept=coef(mod)[1],
                           slope=coef(mod)[2])
        }) %>%
        right_join(data.x) %>%
        mutate(AROP.cf=intercept+ARPT*slope)

data.x %>%
        ggplot(aes(x=AROP,
                   y=AROP.cf)) +
        geom_point(aes(color=source)) +
        geom_abline(slope=1,
                    intercept=0) +
        facet_wrap(facets=vars(COUNTRY),
                   nrow=3)

data.cf1 = data.x %>%
        select(COUNTRY,YEAR,source,AROP.cf) %>%
        rename(level=AROP.cf) %>%
        mutate(INDICATOR="AROP",
               file="POV",
               Age5G="TOTAL",
               d.break=0,
               model="deciles",
               yoy=level-lag(level,order_by=YEAR)) %>%
        filter(source !="OBS")


# * based on age groups ----

data.x = data %>%
        ungroup() %>%
        pivot_longer(cols=c(level,yoy),
                     names_to="input",
                     values_to="val") %>%
        filter(input=="level",
               INDICATOR=="AROP",
               !is.na(val),
               d.break !=1) %>%
        select(COUNTRY,YEAR,INDICATOR,Age5G,source,val,LY.OBS) %>%
        left_join(POP,
                  by=c("COUNTRY","YEAR","Age5G"))

data.cf2 = data.x %>%
        filter(source !="OBS",
               Age5G !="TOTAL") %>%
        group_by(COUNTRY,Age5G) %>%
        mutate(sh.pop=zoo::na.locf(sh.pop)) %>%
        group_by(COUNTRY,YEAR,source) %>%
        summarize(level=sum(val*sh.pop)) %>%
        left_join(data.x %>%
                          filter(source=="OBS",
                                 Age5G=="TOTAL") %>%
                          group_by(COUNTRY) %>%
                          mutate(YEAR=YEAR+1) %>%
                          rename(level.LY=val) %>%
                          select(-source),
                  by=c("COUNTRY","YEAR")) %>%
        group_by(COUNTRY) %>%
        mutate(INDICATOR="AROP",
               file="POV",
               Age5G="TOTAL",
               d.break=0,
               model="ageg",
               yoy=ifelse(is.na(level.LY),
                          level-lag(level,order_by=YEAR),
                          level-level.LY)) %>%
        filter(source !="OBS") %>%
        select(-c(LY.OBS,sh.pop,level.LY))
