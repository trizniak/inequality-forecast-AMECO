# ---- VIZ : Time Series of Estimates by country ----

F.viz.EST = function (country,
                      facet="Age5G", # model (AROP.TOTAL) / Age5G (AROP) / INDICATOR (deciles)
                      npt="level") {
        
        data.viz = data %>%
                filter(COUNTRY==country,
                       input==npt,
                       {if (facet=="model")
                               INDICATOR=="AROP" & Age5G=="TOTAL"
                               else TRUE},
                       {if (facet=="Age5G")
                               INDICATOR=="AROP" & Age5G !="TOTAL" & (model=="direct" | source=="OBS")
                               else TRUE},
                       {if (facet=="INDICATOR")
                               file=="POZ" & (model=="direct" | source=="OBS")
                               else TRUE})
        
        data.viz = data.viz %>%
                select(-c(d.break,break.g)) %>%
                {if (facet !="model") select(.data=.,-model) else .} %>%
                filter({if (facet=="model")
                        source !="OBS"
                        else TRUE}) %>%
                pivot_wider(names_from=source,
                            values_from=val) %>%
                left_join(data.viz %>%
                                  filter(source=="OBS") %>%
                                  rename(OBS=val) %>%
                                  {if (facet=="model") select(.data=.,c(YEAR,INDICATOR,Age5G,break.g,OBS))
                                          else select(.data=.,c(YEAR,INDICATOR,Age5G,break.g))}) #%>%
                #{if (facet=="model") mutate(.data=.,
                #                            model=factor(as.character(model),
                #                                         levels=c("direct","deciles","ageg"),
                #                                         labels=c("Direct","Decile-based","Age-group Composite")))
                #        else .}
        
        geom_OBS=eval(parse(
                text=paste0("geom_",
                            ifelse(npt=="level","line","step"),
                            "(aes(y=OBS,
                            group=break.g)",
                            ifelse(npt=="level",")",",\ndirection=\"mid\")"))
                ))
        
        geom_EST=eval(parse(
                text=paste0("geom_",
                            ifelse(npt=="level","point","step"),
                            "(aes(y=EST,
                            group=break.g),
                            color=\"darkkhaki\"",
                            ifelse(npt=="level",")",",\ndirection=\"mid\")"))
        ))
        
        geom_FXT=eval(parse(
                text=paste0("geom_",
                            ifelse(npt=="level","line","step"),
                            "(aes(y=FXT,
                            group=break.g),
                            color=\"orangered\"",
                            ifelse(npt=="level",")",",\ndirection=\"mid\")"))
        ))
        
        ggplot(data=data.viz,
               aes(x=substr(YEAR,3,4),
                   group=1)) +
                {if (npt=="yoy") geom_hline(yintercept=0,
                                            color="grey")} +
                geom_OBS +
                geom_EST +
                geom_FXT +
                theme(axis.text.x=element_text(size=6,
                                               angle=90),
                      axis.text.y=element_markdown(size=6)) +
                scale_y_continuous(breaks=scales::breaks_pretty(high.u.bias=0.5),
                                   limits=c(ifelse(npt=="level",0,NA),NA),
                                   labels=f.label.color,
                                   expand=expansion(mult=c(ifelse(npt=="level",0,0.05),.05))) +
                labs(title=ifelse(facet=="model",
                                  "Modeling",
                                  "Modeling : Direct"),
                     x="Year (20..)",
                     y=ifelse(npt=="level","","YoY change")) +
                facet_grid(cols=vars((.data[[facet]])))
}

F.viz.EST(country="FR",facet="model",npt="level")
F.viz.EST(country="FR",facet="Age5G",npt="level")
F.viz.EST(country="FR",facet="INDICATOR",npt="level")

F.viz.EST(country="FR",facet="model",npt="yoy")
F.viz.EST(country="FR",facet="Age5G",npt="yoy")
F.viz.EST(country="FR",facet="INDICATOR",npt="yoy")
