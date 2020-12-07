# ---- TAB : FXT ----

F.tab.FXT = function () {
        data.tab = data %>%
                filter(source=="FXT",
                       file %in% c("POZ","POV")) %>%
                pivot_wider(names_from=input,
                            values_from=val) %>%
                mutate(COUNTRY=as_factor(COUNTRY),
                       YEAR=as_factor(YEAR),
                       #INDICATOR=as_factor(INDICATOR),
                       Age5G=as_factor(Age5G),
                       model=factor(as.character(model),
                                    levels=c("Direct","Decile-based","Age-group Composite")),
                       level=formatC(ifelse(file=="POV",
                                            level,
                                            round(level,
                                                  digits=-1)),
                                     format="f",
                                     big.mark=" ",
                                     digits=ifelse(file=="POV",1,0),
                                     drop0trailing=TRUE),
                       yoy=paste0(ifelse(yoy<0,
                                         "<span style=\"color:red\">",
                                         ""),
                                  sprintf(ifelse(file=="POV",
                                                 "%.2f",
                                                 "%.1f %%"),
                                          yoy),
                                  ifelse(yoy<0,
                                         "</span>",
                                         ""))) %>%
                select(-c(source,file,d.break,LY.OBS,break.g))
        
        
        DT::datatable(data.tab,
                      class="display",
                      filter="top",
                      extensions=c('Buttons'),
                      colnames=c(#'#',
                              'Country code',
                              'Year',
                              'Indicator',
                              'Age group',
                              'Forecast',
                              'Level',
                              "YoY change"),
                      rownames=FALSE,
                      caption="Forecasted values",
                      options=list(dom='Brtip',
                                   buttons=list('copy',
                                                list(extend='collection',
                                                     buttons=c('csv','excel'),
                                                     text='Download')),
                                   pageLength=50,
                                   autoWidth=TRUE,
                                   scrollX=TRUE,
                                   scrollY="200px",
                                   scrollCollapse=TRUE,
                                   paging=FALSE,
                                   searchable=FALSE),
                      escape=FALSE) %>%
                DT::formatStyle(c('level','yoy'),`text-align`='right')
}
