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
