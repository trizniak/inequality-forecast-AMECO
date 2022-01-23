# ==== SETUP ====
# #### ~~~~~ ####

# ~~~ OPTIONS ~~~ ####
# ... Code appearance ([Tools] [Global Options] [Appearance]) : Tomorrow Night Blue

options(repr.plot.width=49,
	repr.plot.height=36,
	scipen=999,
	digits=1,
	warn=-1)
my.color.dark="#273749"
my.color.medium="#446699"
my.color.light="#a1b3c9"
# scales::show_col(c(my.color.dark,my.color.medium,my.color.light),ncol=1)


# ~~~ PACKAGES ~~~ ####
if (!require("pacman")) utils::install.packages("pacman")
pacman::p_load(
  devtools,		    #[.KEY]         https://cran.r-project.org/web/packages/devtools/index.html
  here,			      #[.KEY]         https://cran.r-project.org/web/packages/here/index.html
  readr,			    #[.KEY]         https://cran.r-project.org/web/packages/readr/index.html
  scales,			    #[.KEY]         https://cran.r-project.org/web/packages/scales/index.html
  # smacof,		    #[ANALYTICS]    https://cran.r-project.org/web/packages/smacof/index.html
  # pheatmap,		  #[ANALYTICS]	  https://cran.r-project.org/web/packages/pheatmap/index.html
  # docxtractr,	  #[DATA]			    https://cran.r-project.org/web/packages/docxtractr/index.html
  eurostat,		    #[DATA]			    https://cran.r-project.org/web/packages/eurostat/index.html
  readxl,			    #[DATA]		    	https://cran.r-project.org/web/packages/readxl/index.html
  # restatapi,	  #[DATA]         https://cran.r-project.org/web/packages/restatapi/index.html
  writexl,		    #[DATA]		    	https://cran.r-project.org/web/packages/writexl/index.html
  # crosstalk,	  #[INTERACTIVE]  https://cran.r-project.org/web/packages/crosstalk/index.html
  # glue,			    #[OUTILS]		    https://cran.r-project.org/web/packages/glue/index.html
  janitor,		    #[OUTILS]	    	https://cran.r-project.org/web/packages/janitor/index.html
  rvest,        	#[OUTILS]       https://cran.r-project.org/web/packages/rvest/index.html
  # webshot,		  #[OUTILS]		    https://cran.r-project.org/web/packages/webshot/index.html
  withr,			    #[OUTILS]   		https://cran.r-project.org/web/packages/withr/index.html
  zip,			      #[OUTILS]   		https://cran.r-project.org/web/packages/zip/index.html
  # DT,			      #[TAB]    			https://cran.r-project.org/web/packages/DT/index.html
  flextable,	    #[TAB]    			https://cran.r-project.org/web/packages/flextable/index.html
  ftExtra,			#[TAB]    			https://cran.r-project.org/web/packages/ftExtra/index.html
  # kableExtra,	  #[TAB]    			https://cran.r-project.org/web/packages/kableExtra/index.html
  # reactable,	  #[TAB]    			https://cran.r-project.org/web/packages/reactable/index.html
  # dtwclust,		  #[TIME SERIES]	https://cran.r-project.org/web/packages/dtwclust/index.html
  # fable,        #[TIME SERIES]	https://cran.r-project.org/web/packages/fable/index.html
  # feasts,		    #[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  # slider,		    #[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  # tsibble,		  #[TIME SERIES]	https://cran.r-project.org/web/packages/tsibble/index.html
  # urca,        	#[TIME SERIES]	https://cran.r-project.org/web/packages/urca/index.html
  # zoo,       	  #[TIME SERIES]	https://cran.r-project.org/web/packages/zoo/index.html
  # ggiraph,		  #[VIZ]    			https://cran.r-project.org/web/packages/ggiraph/index.html
  # ggplotify,	  #[VIZ]    			https://cran.r-project.org/web/packages/ggplotify/index.html
  # ggpubr,		    #[VIZ]    			https://cran.r-project.org/web/packages/ggpubr/index.html
  # ggrepel,		  #[VIZ]    			https://cran.r-project.org/web/packages/ggrepel/index.html
  # gplots,		    #[VIZ]    			https://cran.r-project.org/web/packages/gplots/index.html
  ggtext,	  	  	#[VIZ]    			https://cran.r-project.org/web/packages/ggtext/index.html
  # highcharter,	#[VIZ]    			https://cran.r-project.org/web/packages/highcharter/index.html
  # magick,		    #[VIZ]    			https://cran.r-project.org/web/packages/magick/index.html
  patchwork,		  #[VIZ]    			https://cran.r-project.org/web/packages/patchwork/index.html
  # plotly,		    #[VIZ]    			https://cran.r-project.org/web/packages/plotly/index.html
  # plotrix,		  #[VIZ]    			https://cran.r-project.org/web/packages/plotrix/index.html
  # ragg,        	#[VIZ]    			https://cran.r-project.org/web/packages/ragg/index.html
  tidyverse		    #[.KEY]		    	https://cran.r-project.org/web/packages/tidyverse/index.html
)
# ... websjot ####
# webshot::install_phantomjs(force=FALSE)

# ... Necessary on business machine ¯\_(ツ)_/¯ ####
library(dplyr)
library(tibble)


# ~~~ FONTS ~~~ ####
pacman::p_load(extrafont)
#extrafont::font_import(prompt=FALSE)


# ~~~ THEME ~~~ ####
my.theme = function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(text=element_text(family="Calibri",
                                     color=my.color.dark),
                   axis.line.x.bottom=element_line(color="grey",
                                                   size=.3),	# set as element_blank to remove : axis.line is ignored
                   axis.line.y.left=element_line(color="grey",
                                                 size=.3),	# set as element_blank to remove : axis.line is ignored
                   axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title=element_text(face="italic"),
                   legend.title=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_rect(size=0.1,
                                             color="grey",
                                             fill=NA),
                   panel.grid=element_blank(),
                   panel.spacing=unit(0.1,"lines"),
                   plot.title=element_markdown(),
                   plot.title.position="plot",
                   plot.subtitle=element_markdown(),
                   strip.background=element_blank(),
                   strip.placement="outside",
                   strip.text=element_text(color=my.color.dark,
                                           face="italic"))
}

ggplot2::theme_set(my.theme())


# ~~~ COLOR PALETTES ~~~ ####

palette.ESTAT = # Theme 3: Population and social conditions
  c(orange.3="#faa519", # dark orange
    orange.2="#fcc975", # regular orange
    orange.1="#fddba3", # light orange
    blue.3="#286eb4", # dark blue
    blue.2="#71a8df", # regular blue
    blue.1="#a0c5ea", # light blue
    red.3="#f06423", # dark red
    red.2="#f6a27b", # regular red
    red.1="#f9c1a7", # light red
    khaki.3="#b9c31e", # dark khaki
    khaki.2="#e1e86b", # regular khaki
    khaki.1="#ebf09c", # light khaki
    green.3="#5fb441", # dark green
    green.2="#9ed58a", # regular green
    green.1="#bee3b1", # light green
    teal.3="#32afaf", # dark teal
    teal.2="#7ad9d9", # regular teal
    teal.1="#a6e6e6") # light teal
# scales::show_col(palette.ESTAT,labels=FALSE,ncol=3)


# ~~~ LABELS ~~~ ####


# ~~~ CODING ~~~ ####

# * CODING : Age groups ####
agegroup.schema = list(TOTAL=c("TOTAL"),
                       UNK=c("UNK"),
                       groups.3=c("Y_LT15",
                                   "Y15-64",
                                   "Y_GE65"),
                       years.5.100=c("Y_LT5",
                                     "Y5-9",
                                     "Y10-14",
                                     "Y15-19",
                                     "Y20-24",
                                     "Y25-29",
                                     "Y30-34",
                                     "Y35-39",
                                     "Y40-44",
                                     "Y45-49",
                                     "Y50-54",
                                     "Y55-59",
                                     "Y60-64",
                                     "Y65-69",
                                     "Y70-74",
                                     "Y75-79",
                                     "Y80-84",
                                     "Y85-89",
                                     "Y90-94",
                                     "Y95-99",
                                     "Y_GE100"),
                       years.5.85=c("Y_LT5",
                                    "Y5-9",
                                    "Y10-14",
                                    "Y15-19",
                                    "Y20-24",
                                    "Y25-29",
                                    "Y30-34",
                                    "Y35-39",
                                    "Y40-44",
                                    "Y45-49",
                                    "Y50-54",
                                    "Y55-59",
                                    "Y60-64",
                                    "Y65-69",
                                    "Y70-74",
                                    "Y75-79",
                                    "Y80-84",
                                    "Y_GE85"))


# ~~~ AUX FUNS ~~~ ####

# * FUN : Label Color ####
f.label.color = function(x,
                         color.negative=palette.ESTAT["red.3"], # "red"
                         color.neutral=palette.ESTAT["teal.3"], # "lightgrey"
                         color.pozitive=palette.ESTAT["green.3"]) { # "green4"
  paste0("<b><span style='color:",
         dplyr::case_when(x<0~color.negative,
                          x>0~color.pozitive,
                          TRUE~color.neutral),
         "'>",x,"</span>")}
# scale_y_continuous(labels=function (X) f.label.color(X,"midnightblue","#273749","magenta") / labels=f.label.color)

# * FUN : Pretty Rounding ####
f.pretty.round = function (x,step=5) {
  E=ifelse(x==0,0,floor(log10(abs(x))-1))
  F=x/10^E
  step*ceiling(F/step)*10^E
}

# * FUN : Get Eurostat data ####
# https://stackoverflow.com/questions/59796178/r-curlhas-internet-false-even-though-there-are-internet-connection
f.data.estat = function(.datafile,
                        .lag=0,
		        .filter=FALSE) {
  # http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di01&lang=en
  eurostat::get_eurostat(.datafile,
                         time_format="num",
                         keepFlags=TRUE) %>%
    dplyr::rename(#COUNTRY=geo,
                  YEAR=time,
                  level=values) %>%
    dplyr::mutate(COUNTRY=stringr::str_sub(geo,
                                           start=1,
                                           end=2)) %>%
    {if (.filter)
    dplyr::filter(COUNTRY %in% names(country.list),
                  YEAR>=2005,
                  !is.na(level))
      else .} %>%
    dplyr::mutate(YEAR=YEAR-.lag,
                  d.break=ifelse(substr(flags,1,1)=="b",1,NA),
                  COUNTRY=as.character(COUNTRY))
}
