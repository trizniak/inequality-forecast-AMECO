# ---- SETUP ----

# Code appearance ([Tools] [Global Options] [Appearance]) : Tomorrow Night Blue

options(repr.plot.width=49,
        repr.plot.height=36,
        scipen=999,
        digits=1,
        warn=-1)
my.color="#273749" # 214263 0B4279 0b2131 1b3142


# ---- PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  devtools,	#[.KEY]		https://cran.r-project.org/web/packages/devtools/index.html
  here,		#[.KEY]		https://cran.r-project.org/web/packages/here/index.html
  readr,		#[.KEY]		https://cran.r-project.org/web/packages/readr/index.html
  scales,		#[.KEY]		https://cran.r-project.org/web/packages/scales/index.html
  # smacof,	#[ANALYTICS]	https://cran.r-project.org/web/packages/smacof/index.html
  # pheatmap,	#[ANALYTICS]	https://cran.r-project.org/web/packages/pheatmap/index.html
  # docxtractr,	#[DATA]		https://cran.r-project.org/web/packages/docxtractr/index.html
  eurostat,	#[DATA]		https://cran.r-project.org/web/packages/eurostat/index.html
  # readxl,	#[DATA]		https://cran.r-project.org/web/packages/readxl/index.html
  # restatapi,	#[DATA]		https://cran.r-project.org/web/packages/restatapi/index.html
  # crosstalk,	#[INTERACTIVE]	https://cran.r-project.org/web/packages/crosstalk/index.html
  # glue,		#[OUTILS]	https://cran.r-project.org/web/packages/glue/index.html
  rvest,        	#[OUTILS]	https://cran.r-project.org/web/packages/rvest/index.html
  janitor,	#[OUTILS]	https://cran.r-project.org/web/packages/janitor/index.html
  zip,	#[OUTILS]	https://cran.r-project.org/web/packages/zip/index.html
  # DT,		#[TAB]		https://cran.r-project.org/web/packages/DT/index.html
  # kableExtra,	#[TAB]		https://cran.r-project.org/web/packages/kableExtra/index.html
  # reactable,	#[TAB]		https://cran.r-project.org/web/packages/reactable/index.html
  # dtwclust,	#[TIME SERIES]	https://cran.r-project.org/web/packages/dtwclust/index.html
  fable,        	#[TIME SERIES]	https://cran.r-project.org/web/packages/fable/index.html
  feasts,	        #[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  #slider,	#[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  tsibble,	#[TIME SERIES]	https://cran.r-project.org/web/packages/tsibble/index.html
  urca,        	#[TIME SERIES]	https://cran.r-project.org/web/packages/urca/index.html
  zoo,       	#[TIME SERIES]	https://cran.r-project.org/web/packages/zoo/index.html
  ggiraph,	#[VIZ]		https://cran.r-project.org/web/packages/ggiraph/index.html
  # ggplotify,	#[VIZ]		https://cran.r-project.org/web/packages/ggplotify/index.html
  # ggpubr,	#[VIZ]		https://cran.r-project.org/web/packages/ggpubr/index.html
  # ggrepel,	#[VIZ]		https://cran.r-project.org/web/packages/ggrepel/index.html
  # gplots,	#[VIZ]		https://cran.r-project.org/web/packages/gplots/index.html
  ggtext,		#[VIZ]		https://cran.r-project.org/web/packages/ggtext/index.html
  # highcharter,	#[VIZ]		https://cran.r-project.org/web/packages/highcharter/index.html
  patchwork,	#[VIZ]		https://cran.r-project.org/web/packages/patchwork/index.html
  # plotly,	#[VIZ]		https://cran.r-project.org/web/packages/plotly/index.html
  # plotrix,	#[VIZ]		https://cran.r-project.org/web/packages/plotrix/index.html
  ragg,        	#[VIZ]		https://cran.r-project.org/web/packages/ragg/index.html
  tidyverse	#[.KEY]]	https://cran.r-project.org/web/packages/tidyverse/index.html
)


# ---- THEME ----
my.theme = function() {
  theme_minimal() +
    theme(text=element_text(family="Calibri",
                            color=my.color),
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
                                    fill=NA),
          panel.grid=element_blank(),
          panel.spacing=unit(0.1,"lines"),
          plot.title=element_markdown(),
          plot.title.position="plot",
          plot.subtitle=element_markdown(),
          strip.background=element_blank(),
          strip.placement="outside",
          strip.text=element_text(color=my.color,
                                  face="italic"))
}

theme_set(my.theme())


# ---- Color Palette : OBS, EST, FXT ----
palette.FOE=c("orangered",
              "royalblue4",
              "darkkhaki")
# scales::show_col(palette.FOE)
names(palette.FOE)=c("FXT",
                     "OBS",
                     "EST")


# ---- Color Palette : Age groups ----
# 6 groups
palette.Age6G=c("royalblue4",
                "brown4",
                "darkorange",
                "gold",
                "tan",
                "khaki4",
                "darkgreen")
# scales::show_col(palette.Age6G)
names(palette.Age6G)=c("ALL",
                       "<18",
                       "18-24",
                       "25-34",
                       "35-54",
                       "55-64",
                       "65+")
# 5 groups
palette.Age5G=c("royalblue4",
                "brown4",
                "darkorange",
                "tan",
                "khaki4",
                "darkgreen")
# scales::show_col(palette.Age5G)
names(palette.Age5G)=c("TOTAL",
                       "<16",
                       "16-24",
                       "25-49",
                       "50-64",
                       "65+")
# 4 groups
palette.Age4G=c("royalblue4",
                "brown4",
                "darkorange",
                "khaki4",
                "darkgreen")
# scales::show_col(palette.Age4G)
names(palette.Age4G)=c("ALL",
                       "<18",
                       "18-34",
                       "35-64",
                       "65+")

# ---- Color Palette : Clusters ----
palette.clu = c("firebrick",
                "mediumorchid4",
                "forestgreen",
                "darkblue")
# scales::show_col(palette.clu)


# ---- FUNCTION : Label Color ----
f.label.color = function(x,
                         color=TRUE,
                         color.poz="green3",
                         color.neg="red") {
  paste0("<b><span style='color:",
         ifelse(color,
                case_when(x<0~color.neg,
                          x>0~color.poz,
                          TRUE~"#273749"),
                "#273749"),
         "'>",
         format(x,
                digits=1,
                big.mark=" ",
                replace.zero=TRUE,
                zero.print="-",
                drop0trailing=TRUE),
         "</span>")}


# ---- FUNCTION : Pretty Rounding ----
f.pretty.round=function (x) {
  E=ifelse(x==0,0,floor(log10(abs(x))-1))
  F=x/10^E
  5*ceiling(F/5)*10^E
}


# ---- EU MS : Protocol Order ----
EU.PO = read_delim(here("OUTILS","REF","EU.PO.txt"),"\t",
                   escape_double=FALSE,
                   trim_ws=TRUE) %>%
  as.data.frame() %>%
  filter(COUNTRY!="UK")

country.list=c(as.list(unique(as.character(EU.PO$COUNTRY))),"EU")
