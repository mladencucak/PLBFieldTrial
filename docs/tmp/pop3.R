## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::knit_hooks$set(
  source = function(x, options) {
    hook.r = function(x, options) {
      fence <- "```"
      language = tolower(options$engine)
      if (language == 'node') language = 'javascript'
      if (!options$highlight) language = 'text'
      if(!is.null(options$foldcode)) {
      paste0('\n\n', "<details><summary>The code</summary>\n", fence, language, '\n', x, fence,  '\n\n', "</details>\n")
      } else {
              paste0('\n\n', fence, language, '\n', x, fence,  '\n\n')
      }
    }
    x = knitr:::hilight_source(x, 'markdown', options)
    hook.r(
      paste(c(
        x, 
        ''
      ), collapse = '\n'), 
      options
    )
  }
)



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


## ----load_data-----------------------------------------------------------
path <- here::here("data", "gen data", "final", "pop.csv")

monpop <- read.genalex(path, ploidy = 3)

splitStrata(monpop) <- ~variety/genotype/year

monpop




## ------------------------------------------------------------------------
#Determine the OS to assign apropriate character string for parallel processing
 parallel_proc <- 
  ifelse(Sys.info()['sysname'] == "Windows", "snow", "multicore") 

cpus <-  ifelse(detectCores()>1, c(detectCores()-1), 1)
cpus


## ----dapc_var------------------------------------------------------------
setPop(monpop) <- ~variety


set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 5:20, n.rep = 100,
                              parallel = parallel_proc, 
                              ncpus = cpus))
pramx[2:6]
pramx[-1]
pramx[-1]$`Number of PCs Achieving Highest Mean Success`

#Reorder levels of the factor for plotting
pramx$DAPC$grp <- 
factor(pramx$DAPC$grp, levels = c("KE", "BQ", "RO", "SE", "CL", "SM"))


scatter(
  pramx$DAPC,
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


dev.copy(png,filename=here::here("results", "gen","dapc", "var.png"),
         width =600, height= 600);
dev.off ()



## ----genalex_new---------------------------------------------------------
fin <-  readRDS(file = here::here("data", "gen data", "final", "gendata.rds") )

fin <- 
  unite(fin, "Ind", idd, year, remove = F)

fin <- 
  unite(fin, "Pop", treatment, Genotype, year) 


 popcol <- match("Pop",names(fin))
 idcol <- match("Ind",names(fin))


gen <- 
fin[ , c(idcol,popcol,match("D13",names(fin)):match("X__24",names(fin)))]


path <- here::here("data", "gen data", "final", "pop2.csv")

data.frame(ssr = c(12,"PInf_Ireland"), 
           samples = nrow(gen), 
           pop =  length(unique(fin$Pop)))

write.table(data.frame(ssr = 12, samples = nrow(gen), pop =  length(unique(fin$Pop))),
            path,
            sep=",",  col.names=FALSE, row.names = FALSE)

write.table( "PInf_Ireland",
            path,
            sep = ",", row.names = FALSE,
            col.names = FALSE, append = T)
names(gen)[grep("X__", names(gen))] <- ""

write.table(gen,
            path,
            sep = ",", 
            row.names = FALSE,
            col.names = !file.exists("myDF.csv"), append = T)



## ----import_genalex------------------------------------------------------
path <- here::here("data", "gen data", "final", "pop2.csv")

monpop <- read.genalex(path, ploidy = 3)

splitStrata(monpop) <- ~treatment/genotype/year

monpop


## ----dapc_trt------------------------------------------------------------
setPop(monpop) <- ~treatment

set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 5:20, 
                              n.rep = 1000,
                              parallel = parallel_proc, 
                              ncpus = cpus))
pramx[-1]

scatter(
  pramx$DAPC,
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


dev.copy(png,filename=here::here("results", "gen","dapc", "treatment.png"),
         width = 600, height= 600);
dev.off ()



## ----dapc_year-----------------------------------------------------------
setPop(monpop) <- ~year

set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 5:20,
                              n.rep = 1000,
                              parallel = parallel_proc, 
                              ncpus = cpus))
pramx[-1]

scatter(
  pramx$DAPC,
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


dev.copy(png,filename=here::here("results", "gen","dapc", "year.png"),
         width = 600, height= 600);
dev.off ()




## ------------------------------------------------------------------------



## ------------------------------------------------------------------------



## ------------------------------------------------------------------------



## ------------------------------------------------------------------------
session_info()

