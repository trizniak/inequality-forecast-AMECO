# ---- VIZ : Forecasted values ----

source(here("OUTILS","FUNS","DATA_FXT.R"))
load(here("DATA","data_FXT.Rdata"))

F.viz.FXT = function() {
        
        data.viz = data.FXT %>%
                filter(fxt %in% c("S20","A19"),
                       YEAR>2019) %>%
                mutate(YEAR=paste0("Y",substr(YEAR,3,4))) %>%
                pivot_wider(names_from=YEAR,
                            values_from=val) %>%
                mutate(Y21=Y20+Y21) %>%
                pivot_longer(cols=starts_with("Y"),
                             names_to="YEAR") %>%
                pivot_wider(names_from=fxt,
                            values_from=value) %>%
                mutate(YEAR=2000+as.numeric(substr(YEAR,2,3)),
                       IMPACT=S20-A19)
        
        foo = function(ind) {
                data.viz %>%
                        filter(INDICATOR==ind) %>%
                        mutate(klr=ifelse(IMPACT>0,"darkblue","red")) %>%
                        ggplot() +
                        geom_rect(aes(xmin=-abs(IMPACT)/2,
                                      xmax=abs(IMPACT)/2,
                                      ymin=-abs(IMPACT)/2,
                                      ymax=abs(IMPACT)/2,
                                      fill=klr)) +
                        #coord_fixed() +
                        facet_grid(rows=vars(COUNTRY),
                                   cols=vars(YEAR),
                                   switch="x") +
                        labs(title=ind) +
                        scale_fill_identity() +
                        theme(axis.line.x.bottom=element_blank(),
                              axis.line.y.left=element_blank(),
                              panel.border=element_blank(),
                              plot.background=element_rect(color="grey",
                                                           size=0.1),
                              strip.text.x=element_text(size=6),
                              strip.text.y=element_blank())
        }
        
        ggplot(data=data.viz,
               aes(x=as.factor(YEAR),
                   group=1)) +
                geom_hline(yintercept=0,
                           color="lightgrey",
                           size=0.3) +
                geom_step(aes(y=A19),
                          direction="mid",
                          color="khaki4") +
                geom_step(aes(y=S20),
                          direction="mid",
                          color="sienna") +
                facet_grid(rows=vars(COUNTRY),
                           cols=vars(INDICATOR)) +
                labs(title="Forecast comparison",
                     y="% change vs. 2019") +
                scale_y_continuous(labels=f.label.color) +
                theme(axis.line.x.bottom=element_blank(),
                      axis.text.x=element_text(size=6,
                                               angle=90),
                      axis.text.y=element_markdown(size=4),
                      axis.title.x=element_blank(),
                      strip.text.y=element_text(angle=0)) +
                lapply(unique(data.viz$INDICATOR),foo) +
                plot_layout(nrow=1) &
                theme(strip.placement=NULL) # https://github.com/thomasp85/patchwork/issues/132
}
