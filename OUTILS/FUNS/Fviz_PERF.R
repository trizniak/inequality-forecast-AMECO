
# ---- VIZ : Performance (tile grid) ----

F.viz.PERF = function (metric) {
        data.x = data %>%
                filter(INDICATOR=="AROP",
                       Age5G=="TOTAL",
                       source %in% c("OBS","EST"),
                       input=="yoy") %>%
                select(COUNTRY,YEAR,model,source,val)
        data.x = data.x %>%
                filter(source=="EST") %>%
                select(COUNTRY,YEAR,model,val) %>%
                rename(EST=val) %>%
                left_join(data.x %>%
                                  filter(source=="OBS") %>%
                                  select(COUNTRY,YEAR,val) %>%
                                  rename(OBS=val),
                          by=c("COUNTRY","YEAR")) %>%
                group_by(COUNTRY,model) %>%
                summarize(RMSE=sqrt(mean((OBS-EST)^2)),
                          RSQ=cor(OBS,EST)^2) %>%
                ungroup()
        data.x = data.x %>%
                group_by(COUNTRY) %>%
                summarize(best.RMSE=min(RMSE),
                          best.RSQ=max(RSQ)) %>%
                right_join(data.x) %>%
                mutate(is.best=as.numeric({if (metric=="RMSE")
                        RMSE-best.RMSE<0.1
                        else best.RSQ-RSQ<0.05})) %>%
                mutate(model=factor(recode_factor(model,
                                                  "Age-group Composite"="Age-group\nComposite"),
                                    levels=c("Direct","Decile-based","Age-group\nComposite")))
        
        range=paste0("Range : ",
                     round(min(subset(data.x,select=metric)),digits=1)," to ",
                     round(max(subset(data.x,select=metric)),digits=1))
        multix=f.pretty.round(max(subset(data.x,select=metric)))
        
        girafe(ggobj=data.x %>%
                       ggplot(aes(x=multix*as.numeric(as.factor(COUNTRY)),
                                  y=multix*as.numeric(as.factor(model)))) +
                       ggiraph::geom_rect_interactive(aes(xmin=multix*as.numeric(as.factor(COUNTRY))-abs(.data[[metric]])/2,
                                                          xmax=multix*as.numeric(as.factor(COUNTRY))+abs(.data[[metric]])/2,
                                                          ymin=multix*as.numeric(as.factor(model))-abs(.data[[metric]])/2,
                                                          ymax=multix*as.numeric(as.factor(model))+abs(.data[[metric]])/2,
                                                          tooltip=round(.data[[metric]],digits=2),
                                                          fill=as.factor(is.best))) +
                       theme(axis.title.x=element_blank(),
                             axis.text=element_text(),
                             axis.text.y.right=element_blank(),
                             axis.title.y.left=element_blank(),
                             axis.title.y.right=element_text(size=9,
                                                             hjust=0.5),
                             panel.border=element_blank(),
                             axis.line.x.bottom=element_blank(),
                             axis.line.y.left=element_blank()) +
                       scale_fill_manual(values=c("darkblue","firebrick"),
                                         guide=FALSE) +
                       scale_y_continuous(#lim=c(0,NA),
                               breaks=multix*(1:length(unique(data.x$model))),
                               labels=unique(data.x$model),
                               position="right",
                               sec.axis=dup_axis()) +
                       scale_x_continuous(#lim=c(0,NA),
                               breaks=multix*(1:length(unique(data.x$COUNTRY))),
                               labels=unique(data.x$COUNTRY)) +
                       coord_fixed() +
                       labs(title=case_when(metric=="RMSE" ~ "RMSE of YoY estimates",
                                            metric=="RSQ" ~ "Correlation of YoY estimates"),
                            y=range),
               width_svg=9,
               height_svg=1.7)
}
