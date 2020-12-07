# ---- Titles, Legends, Kaptions, Variable Names ----

# f.tab=function(n=1) {strrep("\U0009",n)} # inserts <tab> character n times

f.KTarrows=function(RUI.lim=NULL) {
  paste0("\U2B08\U2B0A Significant YoY change",
         "\t\t\U2B00\U2B02 NON-significant YoY change",
         ifelse(is.numeric(RUI.lim),paste0(" above ",RUI.lim),""),
         "\t\t\U21E8 ",
         ifelse(is.numeric(RUI.lim),
                paste0("NON-significant YoY change below ",RUI.lim),
                "RUI centared on zero"))}

KT.nosig="\t\t\U2B1C Range of NON-significant Values"

f.ST.SILC=function(s,f) {
  #' Creates the 'SILC' part of the subtitle
  #'
  #' \param s : Start year of the series
  #' \param f : End year of the series - usually year.FE
  #' SOURCE : \url{http://r-pkgs.had.co.nz/man.html}
  paste0("<span style='color:steelblue'>",s,
         ifelse(s>=f-1,"",paste0("-",substr(f-1,3,4))),
         " SILC + Confidence Interval (CI)</span>")}

f.ST.breaks=function(bx) {
  #' Creates the 'Breaks' part of the subtitle
  #'
  #' \param bx : Existence of breaks
  if (bx) " + <span style='color:firebrick'>Breaks in data series</span>"}

f.ST.EU=function(s,f,eu=TRUE) {
  #' Creates the 'EU average' part of the subtitle
  #'
  #' \param s : Start year of the series
  #' \param f : End year of the series - usually year.FE
  #' \param eu : EU average included
  if (eu) paste0(" & <span style='color:tan'>",s,
                 ifelse(s==f-1,"",paste0("-",substr(f-1,3,4))),
                 " EU average (SILC)</span>")}

f.ST.FE=function(f) {
  #' Creates the 'FE' part of the subtitle
  #'
  #' \param f : End year of the series - usually year.FE
  paste0("<span style='color:orange'>",f,
         " Flash Estimates (FE) as Rounded Uncertainty Interval (RUI)</span>")}


# ---- SOURCE ----
ref.source=paste0("SILC ",param.list$year.min,"-",substr(max(data$YEAR),3,4))


# ---- Variable Names ----

var.names=list(
  AROP="AROP after Social Transfers",
  AROP.preST="AROP before Social Transfers",
  IMPACT="Impact of Social Transfers on AROP (pp)",
  pc_IMPACT="Impact of Social Transfers on AROP\nas % of AROP before Social Transfers",
  az_AROP="AROP after Social Transfers\n[Overall Value]",
  az_AROP.preST="AROP before Social Transfers\n[Overall Value]",
  az_IMPACT="Impact of Social Transfers on AROP (pp)\n[Overall Value]",
  az_pc_IMPACT="Impact of Social Transfers on AROP\nas % of AROP before Social Transfers\n[Overall Value]",
  rs_AROP="AROP after Social Transfers\n[Ratio to Overall Value]",
  rs_AROP.preST="AROP before Social Transfers\n[Ratio to Overall Value]",
  rs_IMPACT="Impact of Social Transfers on AROP (pp)\n[Ratio to Overall Value]",
  rs_pc_IMPACT="Impact of Social Transfers on AROP\nas % of AROP before Social Transfers\n[Ratio to Overall Value]"
)

# ---- CHART CAPTIONS ----
ChK.Interactive=paste0("INTERACTIVE CHART : Hover over points to see details. Click on legend entries to (de)activate them.","<br>",
"Double-click on points to highlight group and deactivate the rest; double-click on chart to reactivate all.")

# ---- BREAKS in RESCALED (rs) values ----
# https://jrgraphix.net/r/Unicode
rs.breaks=log2(c(1.05,1.25,1.5,2,4,Inf))
rs.breaks=c(rev(-rs.breaks),rs.breaks)
names(rs.breaks)=c("\u23ec","\u2bc6","\u25bd","\u2304","\u2ae7",
                   "\u23db",
                   "\u2ae8","\u2303","\u25b3","\u2bc5","\u23eb")
