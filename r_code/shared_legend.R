# Title     : TODO
# Objective : TODO
# Created by: saul
# Created on: 4/14/20

grid_arrange_shared_legend <- function(...,
                                       nrow = 1,
                                       ncol = length(list(...)),
                                       position = c("bottom", "right"),
                                       file_name='woman_confidence_bands.eps') {
  golden_ratio <- 1.61803398875
  golden_width <- 116
  golden_height <- golden_width * golden_ratio
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  #
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  g <- grid.draw(combined)
  figure <- ggarrange(plots[[1]],
                      plots[[2]],
                      plots[[3]],
                      plots[[4]],
                      labels = c("A", "B", "C"),
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom"
                      )
  ggsave(file_name, 
         width=golden_width,
         height=golden_height,
         device="ps",
         units="mm",
         figure)
  return(g)
}