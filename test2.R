plot_poppr_msn(monpop,
               min_span_net,
               mlg = FALSE,
               gadj = 7,
               nodescale = 4,
               nodelab = 1,
               palette =cbbPalette,
               cutoff = 11,
               inds = "none",
               quantiles = FALSE,
               beforecut = TRUE,
               pop.leg = TRUE,
               size.leg = TRUE,
               scale.leg = TRUE,
               layfun = igraph::layout_nicely)

dev.copy(png,filename=here::here("results", "gen","bruvo", "Pop.png"),
         width = 700, height= 700 );
dev.off ()

ggplot() +
geom_bar(
  data = samples_fig,
  aes(julian_day, fill = Genotype),
  stat = "count",
  position = "fill",
  width = 4.7,
  colour = "black",
  size = line_size
) +
  geom_line(
    data = dis_obs,
    aes(
      x = as.numeric(julian_day),
      y = rating / 100,
      colour = variety,
      group = variety
    ),
    size = .4,
    alpha = .6,
    linetype = "solid"
  ) +
  facet_grid( year~ set  ) +
  xlab("Date")+
  ylab("Proportion of foliar disease and MLGs")+
  scale_y_continuous(limits = c(-0.132, 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values=cbbPalette)+
  theme_article()+
  labs(fill = "MLG:",
       color = "Variety")+
  scale_colour_brewer("Variety:",
                      palette = "Dark2")+
  # ## Uncomment for vertical plot
  # geom_text(data = samples_fig,
  #         aes(x = julian_day, y = -0.07, label = labels),
  #         size = 2.2,
  #         check_overlap = FALSE) +
  
  # theme( legend.position = "bottom",
  #        legend.direction = "horizontal")+
  # guides(fill=guide_legend(nrow=1,byrow=TRUE,title.position = "top"),
  #        color=guide_legend(nrow=2,byrow=TRUE,title.position = "top"))+
  # ggsave(filename= here::here("results", "gen", "freq", "Genotype and DPC per sampling vertical.png"),
#        width = 5, height =7, dpi = 620)
# Uncomment for horisontal plot
facet_grid( set~ year  ) +
  geom_text(data = samples_fig,
            aes(x = julian_day, y = -0.1, label = labels),
            size = 3.1,
            angle = -38) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE,title.position = "top"),
         color=guide_legend(nrow=2,byrow=TRUE,title.position = "top"))+
  theme(text = element_text(size = 14),
        legend.position = "bottom")

