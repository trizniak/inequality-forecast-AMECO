# ---- VIZ : Scatter ----

F.viz.SXR = function (country) {
        data.viz = data %>%
                filter(COUNTRY==country,
                       file %in% c("POV","POZ"),
                       Age5G=="TOTAL",
                       input=="level",
                       source=="OBS" | model=="Direct") %>%
                pivot_wider(id_cols=c(COUNTRY,YEAR,source),
                            names_from=INDICATOR,
                            values_from=val) %>%
                mutate(ARPT=100*(MEDIAN*0.6-D1)/(D3-D1))
        data.viz %>%
                ggplot(aes(x=ARPT,y=AROP)) +
                geom_point(aes(color=source)) +
                geom_smooth(data=filter(data.viz,
                                        source=="OBS"),
                            method="lm",
                            se=FALSE) +
                stat_ellipse(aes(color=source),
                             data=filter(data.viz,
                                         source=="OBS"),
                             level=0.9) +
                stat_ellipse(aes(color=source),
                             data=filter(data.viz,
                                         source=="EST"),
                             level=0.9) +
                stat_ellipse(aes(color=source),
                             data=filter(data.viz,
                                         source=="FXT"),
                             level=0.9) +
                scale_color_manual(values=palette.FOE,
                                   guide=FALSE) +
                labs(title="Modeling : Direct",
                     x="Relative position of ARPT (60% of MEDIAN)\nbetween D1 and D3")
}