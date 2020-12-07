# ---- VIZ : Time Series ----

F.viz.TS = function (country=NULL,
                     fltr="INDICATOR", # file / INDICATOR
                     grp="break.g", # break.g / Age5G / INDICATOR
                     ind="AROP", # a value from {fltr}
                     npt="yoy", # level / yoy
                     other="line", # line / dot
                     incl.TOTAL=TRUE,
                     incl.EU=FALSE,
                     incl.other=TRUE,
                     incl.EST=FALSE) {
        age="Age5G"
        
        data.viz = data %>%
                filter(source !="EST",
                       {if (fltr==age)
                        file !="POP"
                        else TRUE},
                       .data[[fltr]]==ind,
                       input==npt,
                       {if (!is.null(country))
                               COUNTRY==country
                               else TRUE},
                       {if (is.null(country) & fltr !=age & grp !=age)
                               .data[[age]]=="TOTAL"
                               else TRUE})
        data.viz = data.viz %>%
                group_by(COUNTRY) %>%
                summarize(mean=mean(val,na.rm=TRUE))%>%
                right_join(data.viz) %>%
                mutate(interaction.g={if (grp=="break.g" & is.null(country))
                        interaction(break.g,source,model)
                        else interaction(break.g,.data[[ifelse(grp=="break.g",age,grp)]],source,model)}) %>%
                {if (npt=="yoy") mutate(.data=.,
                                        val=ifelse(d.break==1,NA,val))
                        else .} %>%
                {if (ind=="XPL") mutate(.data=.,
                                        INDICATOR=factor(INDICATOR,
                                                         levels=names(var.XPL),
                                                         labels=unname(var.XPL)))
                        else .}
        
        geom_main=eval(parse(
                text=paste0("geom_",
                            ifelse(npt=="level","line","step"),
                            "(#data=data.viz %>%
                            #mutate(val=ifelse(source==\"OBS\",val,NA)),
                            aes(y=val,
                            group=interaction.g",
                            ifelse(grp==age,",\ncolor=.data[[age]]),\nshow_guide=TRUE",")"),
                            ifelse(npt=="level",")",",\ndirection=\"mid\")"))
        ))
        
        geom_total=eval(parse(
                text=paste0("geom_",
                            ifelse(npt=="level","line","step"),
                            "(data=data.viz %>%
                            filter(.data[[age]]==\"TOTAL\",
                            model==\"direct\") %>%
                            select(YEAR,val) %>%
                            right_join(expand(data.viz,YEAR,.data[[age]])) %>%
                            left_join(select(data.viz,YEAR,interaction.g)) %>%
                            mutate(val=ifelse(.data[[age]]==\"TOTAL\",NA,val)),
                            aes(y=val,
                            group=interaction.g),",
                            ifelse(npt=="level","","direction=\"mid\",\n"),
                            "color=\"red\",
                            linetype=\"21\")")
        ))
                
        geom_other=eval(parse(
                text=paste0("geom_",
                            ifelse(other=="dot","point",
                                   ifelse(npt=="level","line","step")),
                            "(data=data.viz %>%
                            filter(!str_detect(source,\"cFXT.\"),
                            {if (!is.null(country) & grp==\"break.g\")
                            Age5G !=\"TOTAL\"
                            else TRUE}) %>%
                            select(-.data[[ifelse(is.null(country),\"COUNTRY\",
                            ifelse(grp==\"break.g\",age,grp))]]),
                            aes(y=val,
                            group=interaction.g),",
                            ifelse(npt=="yoy" & other=="line",
                            "direction=\"mid\",\n",""),
                            "color=\"slategrey\",
                            size=",ifelse(other=="dot",1,0.1),
                            ifelse(other=="dot",",\nalpha=0.1",""),
                            ")")
        ))
        
        
        if (count(data.viz)>0) ggplot(data=data.viz,
                                      aes(x=substr(YEAR,3,4),
                                          group=1)) +
                {if (npt=="yoy")
                        geom_hline(yintercept=0,
                                   color="grey")} +
                {if (is.null(country)) geom_vline(aes(xintercept=ifelse(d.break==1,substr(YEAR,3,4),NA)),
                                                  size=0.3,color="firebrick")} +
                {if (!is.null(country) &
                     incl.TOTAL &
                     count(filter(data.viz,.data[[age]] !="TOTAL"))>0)
                        geom_total} +
                {if (incl.other) geom_other} +
                geom_main +
                {if (count(filter(data.viz,source=="FXT"))>0)
                        geom_point(data=data.viz %>%
                                           mutate(val=ifelse(source=="FXT",val,NA)),
                                   aes(y=val),
                                   color="orangered",
                                   size=1)} +
                theme(axis.text.x=element_text(size=6,
                                               angle=90),
                      axis.text.y=element_markdown(size=6)) +
                scale_y_continuous(breaks=scales::breaks_pretty(high.u.bias=0.5),
                                   limits=c(ifelse(npt=="level",0,NA),NA),
                                   labels=f.label.color,
                                   expand=expansion(mult=c(ifelse(npt=="level",0,0.05),.05))) +
                {if (grp==age) scale_color_manual(values=palette.Age5G,
                                                  guide="legend")} +
                labs(title=ind,
                     #subtitle=f.ST(eu=EU,other=incl.other),
                     x="Year (20..)",
                     y=ifelse(npt=="level","","YoY change")) +
                facet_wrap(facets=ifelse(is.null(country),
                                         ifelse(npt=="level",
                                                vars(reorder(COUNTRY,mean)),
                                                vars(COUNTRY)),
                                         ifelse(grp=="break.g",
                                                vars(.data[[age]]),
                                                vars(.data[[grp]]))),
                           nrow=ifelse(is.null(country),3,1))
}
