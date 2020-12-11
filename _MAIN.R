
# ---- *** COVID *** ----

update=TRUE # update main data files (data.breaks, DATAFILE, data)? TRUE/FALSE
report=TRUE # generate output? TRUE/FALSE

# ---- PARAMETERS ----
#countries.out=c("DE") # delete countries but KEEP file ("HR","LT","MT")
countries.out=c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE") # delete countries but KEEP file ("HR","LT","MT")

# Explanatory Variables ----
var.XPL = c(HVGDP="GDP (per capita)",
            NETD="Employees",
            PLCD="Unit Labour Costs"
            #,NSTD="Self-employed"
            #,NWTN="Employees"
            ,UWSH="Gross wages and salaries (per capita)"
            #,UYNH="Net property income (per capita)"
            #,UCTRH="Transfers received (per capita)"
            #,HCPHP="Private consumption (per capita)"
            #,HVGTP="Gross national disposable income (per capita)"
            #,HWCDW="Nominal compensation per employee"
            )

# ---- START ----
source("./OUTILS/CONFIG/SETUP.R")#;source("./OUTILS/CONFIG/SETUP.R")
if (update) source("./OUTILS/CONFIG/UPDATES.R")

# ---- data ----
# source(here("OUTILS","FUNS","DATA.R"))
load(here("DATA","data.Rdata"))

# ---- FUNS ----
source(here("OUTILS","FUNS","Fviz_FXT.R"))
source(here("OUTILS","FUNS","Fviz_TS.R"))
source(here("OUTILS","FUNS","Fviz_EST.R"))
source(here("OUTILS","FUNS","Ftab_FXT.R"))
source(here("OUTILS","FUNS","Fviz_SXR.R"))
source(here("OUTILS","FUNS","Fviz_PERF.R"))

# ---- OUTPUT ----
if(report) rmarkdown::render(here("OUTILS","BLOX","REPORT.Rmd"),
                             output_file=paste0("COVID ",Sys.Date(),".html"),
                             output_dir=here("OUTPUT"),
                             intermediates_dir=here("T E M P"),
                             quiet=TRUE,
                             clean=TRUE)
