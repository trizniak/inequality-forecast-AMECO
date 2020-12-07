# ---- FXT predictors ----
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
        mutate(YEAR=as.numeric(YEAR)) %>%
        mutate(val= # https://ec.europa.eu/info/sites/info/files/economy-finance/saee_spring_2020_0_0.pdf [Tab 2]
                       case_when(COUNTRY=="ES" & YEAR==2020 & INDICATOR=="EMP" & fxt=="S20" ~ -5.6,
                                 COUNTRY=="ES" & YEAR==2021 & INDICATOR=="EMP" & fxt=="S20" ~ 2.9,
                                 COUNTRY=="FR" & YEAR==2020 & INDICATOR=="EMP" & fxt=="S20" ~ -1.2,
                                 COUNTRY=="FR" & YEAR==2021 & INDICATOR=="EMP" & fxt=="S20" ~ 1.0,
                                 COUNTRY=="IT" & YEAR==2020 & INDICATOR=="EMP" & fxt=="S20" ~ -1.9,
                                 COUNTRY=="IT" & YEAR==2021 & INDICATOR=="EMP" & fxt=="S20" ~ 1.5,
                                 TRUE ~ val))
save(data.FXT,file=here("DATA","data_FXT.Rdata"))
rm(data.FXT)