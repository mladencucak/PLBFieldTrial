path <- here::here("data", "gen data", "final", "pop.csv")

monpop <- read.genalex(path, ploidy = 3)



splitStrata(monpop) <- ~variety/genotype/year

monpop

monpop$strata<-
  monpop$strata %>% 
  mutate(genotype = ifelse( genotype ==  "8A1-1","EU_8_1_A1",
                            ifelse(genotype ==  "6A1","EU_6_A1",
                                   ifelse(genotype ==  "13A2","EU_13_A2",
                                          ifelse(genotype ==  "8A1","EU_8_A1","" )))))




scatter(
  pramx$DAPC,
  # col = other(monpop)$comparePal,
  cex = 2 ,
  legend = TRUE,
  clabel = FALSE,
  posi.leg = "bottomleft",
  scree.pca = TRUE,
  posi.pca = "topright",
  cleg = .9,
  xax = 1,
  yax = 2,
  inset.solid = 1
)


#VAriety
set.seed(999)
setPop(monpop) <- ~variety
pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop))


#HArd to determine the peek number of PC
set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 10:20, n.rep = 1000,
                              parallel = parallel_proc, 
                              ncpus = 4L))
pramx[2:6]

pramx[-1]
pramx[-1]$`Number of PCs Achieving Highest Mean Success`
#


scatter(
  pramx$DAPC,
  # col = other(monpop)$comparePal,
  cex = 1.5 ,
  pch=18:23, cstar=0, lwd=1, 
  legend = TRUE,
  clabel = TRUE,
  posi.leg = "topright",
  scree.pca = TRUE,
  posi.da = "bottom",
  posi.pca = "topleft",
  cleg = .9,
  xax = 1,
  yax = 2,
  inset.solid = 1,
  ratio.da = .2
)


# 
# par(xpd=TRUE)
# points(pramx$grp.coord[,1], pramx$grp.coord[,2], pch=4,
#        cex=3, lwd=8, col="black")
# points(pramx$grp.coord[,1], pramx$grp.coord[,2], pch=4,
#        cex=3, lwd=2
#        # col=myCol
#        )
# myInset <- function(){
#   temp <- dapc1$pca.eig
#   temp <- 100* cumsum(temp)/sum(temp)
#   plot(temp, col=rep(c("black","lightgrey"),
#                      c(dapc1$n.pca,1000)), ylim=c(0,100),
#        xlab="PCA axis", ylab="Cumulated variance (%)",
#        cex=1, pch=20, type="h", lwd=2)
# }
# add.scatter(myInset(), posi="bottomright",
#             inset=c(-0.03,-0.01), ratio=.28,
#             bg=transp("white"))


dev.copy(png,filename=here::here("results", "gen","dapc", "var.png"),
         width =600, height= 600);
dev.off ()




#Year
set.seed(999)
setPop(monpop) <- ~year



#HArd to determine the peek number of PC
set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 5:30, 
                              n.rep = 5000,
                              parallel = parallel_proc, 
                              ncpus = 3L))
pramx[-1]
pramx[-1]$`Number of PCs Achieving Highest Mean Success`
#
scatter(
  pramx$DAPC,
  # col = other(monpop)$comparePal,
  cex = 1.3 ,
  legend = TRUE,
  clabel = TRUE,
  posi.leg = "topleft",
  scree.pca = FALSE,
  posi.pca = "topright",
  cleg = .9,
  xax = 1,
  yax = 2,
  inset.solid = 1
)

dev.copy(png,filename=here::here("results", "gen","dapc", "year.png"),
         width = 1500, height= 1500);
dev.off ()






#genotype/year
set.seed(999)
setPop(monpop) <- ~genotype/year


set.seed(999)
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 10:20, 
                              n.rep = 1000,
                              parallel = parallel_proc, 
                              ncpus = 4L))
pramx[-1]
pramx[-1]$`Number of PCs Achieving Highest Mean Success`
#
scatter(
  pramx$DAPC,
  # col = other(monpop)$comparePal,
  cex = 1.4 ,
  legend = TRUE,
  clabel = FALSE,
  posi.leg = "bottomleft",
  scree.pca = FALSE,
  posi.da = "topleft",
  posi.pca = "topleft",
  cleg = .9,
  xax = 1,
  yax = 2,
  inset.solid = 1
)

dev.copy(png,filename=here::here("results", "gen","dapc", "genotype_year.png"),
         width = 1000, height= 1000);
dev.off ()






#genotype/variety
set.seed(999)
setPop(monpop) <- ~genotype/variety
system.time(pramx <- xvalDapc(tab(monpop, NA.method = "mean"), pop(monpop),
                              n.pca = 10:20, 
                              n.rep = 1000,
                              parallel = parallel_proc, 
                              ncpus = 4L))
pramx[-1]
pramx[-1]$`Number of PCs Achieving Highest Mean Success`
#
scatter(pramx$DAPC, 
        # col = other(monpop)$comparePal, 
        cex =1.4 , 
        legend = TRUE,
        clabel = FALSE, 
        posi.leg = "none",
        scree.pca = TRUE,
        posi.da = "topleft",
        posi.pca = "topleft", 
        cleg = .9, xax = 1, 
        yax = 2, inset.solid = 1)

dev.copy(png,filename=here::here("results", "gen","dapc", "genotype_variety.png"),
         width = 1000, height= 1000, dpi=820);
dev.off ()







plotls <- list(s_var, s_var, s_var)
ggpubr::ggarrange(plotlist = plotls, 
                  heights = c(1,1,1),
                  # labels = c("r","rr"),
                  nrow = 3)+
  ggsave(filename= here::here("results", "gen", "dapc", "DAPC_all.png"),
         width = 2.9, height =9, dpi = 820)

png(here::here("results", "gen","dapc", "var.png"))

op <- par(mfrow=c(1,3))
par(op)
dev.off()


samples %>%
  mutate(Genotype = ifelse(Genotype == "8A1-1", "8A1", Genotype)) %>%
  mutate(Genotype = factor(Genotype, levels = c( "8A1","13A2","6A1" ))) %>% 
  group_by(date) %>% 
  count(Genotype) %>%
  rename(counts = n) %>%
  mutate(prop = prop.table(counts)) %>% 
  mutate(perc = round(prop * 100,1)) %>% 
  select(date, Genotype, perc) %>% 
  spread(., Genotype, perc) %>% 
  replace(is.na(.), 0) %>% 
  add_column(., Year = format(.$date, "%Y"), .before= date)





png(filename=here::here("results", "gen","dapc", "year.png"),
         width = 600, height= 600)

scatter(
  pramx_trt$DAPC,
  legend = TRUE,
  cex = 3,
  # pch=18:23,
  posi.leg = "topleft",
  scree.da = FALSE,
  clab =1.8,
  cleg = 2,
  cell = 1, #zoom
  xax = 1,
  yax = 2,
  inset.solid = 1,
  cstar=0.5,
  lwd=.1
)

# dev.copy(png,filename=here::here("results", "gen","dapc", "year.png"),
#          width = 600, height= 600);
dev.off ()
shell.exec(here::here("results", "gen","dapc", "year.png"))

#########################################################
#Tuber Blight
############################################################

#plot genotpes TUBER BLIGHT
tubercbbPalette <- c (
  # "#3399ff",
  "#ff33cc", "#ffff33", "#FDFD8D")

filter(fin,   sr == "TU") %>% 
  ggplot(., 
         aes(variety, fill = Genotype, guide_legend=FALSE ))+
  geom_bar(stat = "count", position = "stack", width = 0.6,colour = "black", size = 0.1)+
  scale_fill_manual(values=tubercbbPalette)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # axis.text.x = element_text(angle = 15, hjust = 1),
        text = element_text(size=17))+
  xlab("")+
  ylab("")+
  ggtitle("Tuber blight")+
  coord_equal(1/10)+
  ggsave(filename= here::here("results", "gen", "freq", "gtuber_blight.png"), 
         width = 4, height = 4, dpi = 620)



#########################################################
#Anim
############################################################

gen_prop <- 
  samples %>%
  mutate(Genotype = ifelse(Genotype == "8A1-1", "8A1", Genotype)) %>%
  mutate(Genotype = factor(Genotype, levels = c( "8A1","13A2","6A1" ))) %>% 
  group_by(variety) %>% 
  count(Genotype, variety) %>%
  rename(counts = n) %>%
  mutate(prop = prop.table(counts)) %>% 
  mutate(perc = round(prop * 100,1)) %>% 
  arrange(desc(variety))



 labels <- 
  samples %>%
  mutate(Genotype = ifelse(Genotype == "8A1-1", "8A1", Genotype)) %>%
  count( variety) %>%
  rename(counts = n) %>%
  mutate(prop = prop.table(counts)) %>% 
  dplyr::select(counts) %>% 
  sapply( .,  function(x) paste0("n=", x )) %>% 
  as.vector()
  
  ggplot(gen_prop, aes(variety, prop, fill = Genotype)) +
    geom_hline(
      yintercept = seq(0 , 1, 0.1),
      size = 0.2,
      alpha = 0.8,
      color = "gray",
      linetype = "dotted"
    )+
    geom_bar(stat = "identity",
             width = 0.6,
             position = position_fill(reverse = TRUE),
             color = "black",
             size = .1) +
    ylab("Frequency") +
    xlab("Variety")  +
    annotate(
      geom = "text",
      label = rev(labels),
      x = unique(gen_prop$variety),
      y = lab_pos_y,
      size = lab_size
    )+ 
    scale_fill_manual(values=GenPalette)+
    theme_bw()+
    egg::theme_article()+
    scale_y_continuous(limits = c(-0.09, 1),
                       # expand = c(0, 0),
                       breaks = seq(0, 1, 0.1))+
    theme(legend.position = "top",
          axis.title.y=element_blank()
    )
  
  
  
  
  
  
  
  
  
  
  
  #########################################################
  #REove the 
  ############################################################
  
  
  
  
  samples <-  readRDS(file = here::here("data", "gen data", "final", "gendata.rds") )
  
  
  
  scatter(
    pramx$DAPC,
    # col = other(monpop)$comparePal,
    cex = 1.5 ,
    pch=18:23,
    legend = TRUE,
    clabel = TRUE,
    posi.leg = "topright",
    scree.pca = TRUE,
    posi.da = "bottom",
    posi.pca = "topleft",
    cleg = .9,
    xax = 1,
    yax = 2,
    inset.solid = 1,
    ratio.da = .2
  )
  
str(pramx$DAPC)




ggscatter <- function(DAPC, STRATA, color = "Year", filter = NULL){
  
  # DAPC <- pramx$DAPC
  STRATA <-  monpop
  
  RYD <- bind_cols(Population = DAPC$grp, 
                    STRATA, 
                   as.data.frame(DAPC$ind.coord)) %>%
    as_tibble()
  RYD_pop <- 
    RYD %>% 
    group_by(Population) %>% 
    summarize_if(is.numeric, mean, na.rm = TRUE) %>%
    rename_all(function(x) gsub("LD", "mean", x))
  RYD <- full_join(RYD, RYD_pop, by = "Population")
  yminor <- pretty(RYD$LD2)
  xminor <- pretty(RYD$LD1)
  RYD <- if (!is.null(filter)) dplyr::filter(RYD, !!filter) else RYD
  RYD_PLOT <- 
    
    ggplot(RYD, aes_string(x = "LD1", y = "LD2")) + 
    # geom_text(aes_string(label = transp(myPal(6))), alpha = 0.75) +
    geom_segment(aes(x = mean1, y = mean2, xend = LD1, yend = LD2), alpha = 0.5) +
    stat_ellipse(type = "norm", level = 0.66, alpha = 0.75) + 
    theme_bw(base_size = 16, base_family = "Helvetica") +  
    theme(aspect.ratio = 0.8) +
    theme(legend.position = "bottom") +
    theme(axis.text = element_blank()) + 
    theme(axis.title = element_blank()) + 
    theme(axis.ticks = element_blank()) + 
    viridis::scale_color_viridis(discrete = TRUE, option = "C", direction = -1) +
    viridis::scale_fill_viridis(discrete = TRUE,  option = "C", direction = -1) +
    scale_y_continuous(breaks = 0, minor_breaks = yminor) + 
    scale_x_continuous(breaks = 0, minor_breaks = xminor) + 
    theme(panel.background = element_rect(fill = "grey95")) +
    theme(panel.grid.major = element_line(color = "grey20")) +
    theme(panel.grid.minor = element_line(color = "white")) 
  RYD_PLOT
}

ggscatter(pramx$DAPC, strata(monpop)) +
  # facet_wrap(~variety) +
  theme(legend.position = c(0.75, 0.1)) +
  guides(color = guide_legend(nrow = 4)) +
  # scale_color_manual(values =  yearscale, breaks = names(yearscale)) +
  theme(legend.direction = "horizontal") +
  theme(strip.background = element_rect(color = NA, fill = "grey90")) +
  theme(strip.text = element_text(face = "bold", hjust = 0.05)) +
  theme(panel.border = element_blank())

gg_region_year








png(filename=here::here("results", "gen","dapc", "var.png"),
    width =600, height= 600)
scatter(
  pramx$DAPC,
  clab =9,
  # col = other(monpop)$comparePal,
   cex = 1.3 ,
  pch=18:23,
  legend = TRUE,
  clabel = TRUE,
  posi.leg = "topleft",
  scree.pca = FALSE,
  scree.da = "bottom",
  cleg = 1.9,
 cell = 1, #zoom
    xax = 1,
  yax = 2,
  inset.solid = 1,
 cstar=.5
)

dev.off ()
shell.exec(here::here("results", "gen","dapc", "var.png"))




myPal <- colorRampPalette(c("blue","gold","red"))

pramx$DAPC$grp <- 
factor(pramx$DAPC$grp, levels = c("KE", "BQ", "RO", "SE", "CL", "SM"))


png(filename=here::here("results", "gen","dapc", "var.png"),
    width =600, height= 600)
scatter(
  pramx$DAPC,
  legend = TRUE,
  col=transp(myPal(6)),
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
shell.exec(here::here("results", "gen","dapc", "var.png"))



