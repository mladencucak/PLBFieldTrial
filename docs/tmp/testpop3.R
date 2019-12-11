

#' 
#' 
#'  Discriminant analysis of principal comonents will give us some more insights into possible clustering within the population.  
#'  We would like to se eif there are effects of our treatments, variety and fungicide programmes.  
#'  Another interesting thing to look at is year to year variation of the pathogen population structure. 
#' 
#' ## Load packages
## ----libs, warning = FALSE, message= FALSE-------------------------------
list.of.packages <-
  c(
    "tidyverse",
    "devtools",
    "here",
    "readxl",
    "poppr",
    "egg",
    "parallel",
    "conflicted"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != TRUE], sep = ", "), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}
rm(list.of.packages, new.packages, packages_load)

#if instal is not working try 
#install.packages("package_name", repos = c(CRAN="https://cran.r-project.org/"))

##############################################
#Varieties  DPCA
#############################################
#Reorder levels of the factor for plotting

pramx_var <- 
  loadRDS( file = here::here("results", "gen","dapc",  "dapc_var.RDS"))

pramx_var$DAPC$grp <- 
  factor(pramx_var$DAPC$grp, levels = c("KE", "BQ", "RO", "SE", "CL", "SM"))

png(filename=here::here("results", "gen","dapc", "variety.png"),
    width = 600, height= 600)


scatter(
  pramx_var$DAPC,
  legend = TRUE,
  cex = 3,
  # pch=18:23,
  posi.leg = "topleft",
  scree.da = FALSE,
  clab =2,
  cleg = 2.1,
  cell = 1, #zoom
  xax = 1,
  yax = 2,
  inset.solid = 1,
  cstar=0.5,
  lwd=.1
)

dev.off ()
shell.exec(here::here("results", "gen","dapc", "year.png"))


##############################################
#Treatment  DPCA
#############################################
#Reorder levels of the factor for plotting

pramx_trt <- 
  readRDS( file = here::here("results", "gen","dapc",  "dapc_trt.RDS"))


png(filename=here::here("results", "gen","dapc", "trt.png"),
    width = 600, height= 600)


scatter(
  pramx_trt$DAPC,
  legend = TRUE,
  cex = 3,
  # pch=18:23,
  posi.leg = "topleft",
  scree.da = FALSE,
  clab =2,
  cleg = 2.1,
  cell = 1, #zoom
  xax = 1,
  yax = 2,
  inset.solid = 1,
  cstar=0.5,
  lwd=.1
)

dev.off ()
shell.exec(here::here("results", "gen","dapc", "trt.png"))


##############################################
#Year  DPCA
#############################################
#Reorder levels of the factor for plotting


pramx_yr <- 
  readRDS( file = here::here("results", "gen","dapc",  "dapc_yr.RDS"))

pramx_yr$DAPC$grp <- 
  factor(pramx_yr$DAPC$grp, levels = c("2016", "2017", "2018", "2019"))

png(filename=here::here("results", "gen","dapc", "year.png"),
    width = 600, height= 600)


scatter(
  pramx_yr$DAPC,
  legend = TRUE,
  cex = 3,
  # pch=18:23,
  posi.leg = "topleft",
  scree.da = FALSE,
  clab =2,
  cleg = 2.1,
  cell = 1, #zoom
  xax = 1,
  yax = 2,
  inset.solid = 1,
  cstar=0.5,
  lwd=.1
)

dev.off ()
shell.exec(here::here("results", "gen","dapc", "year.png"))

