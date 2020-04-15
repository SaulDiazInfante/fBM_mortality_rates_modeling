#confidence_intervals_plots <- function(file_name = 'ht_hat_samples.csv'){
require("ggplot2")
require("ggthemes")
require("dplyr")
require("gridExtra")
require("reshape")
require("data.table")
require("RColorBrewer")
require("grid")
####
####
####
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
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
  # g grid.draw(combined)
  ggsave("woman_confidence_bands", width=golden_width, height=golden_height,
         device="ps", units="mm", combined)
}
golden_width <- 190
golden_ratio <- 1.61803398875
golden_height <- golden_width / golden_ratio
file_name <- 'ht_hat_samples.csv'
ht_hat <- fread(file_name, header = T, sep = ',')
file_name <- 'data_mortality_rate.csv'
italy_mr_df <-  fread(file_name, header = T, sep = ',')
#
melted_data_mr <- melt(ht_hat, id=c("Age", "Sex"), measured = names[1:63], stringsAsFactors = FALSE )
#
names(melted_data_mr)[3] <- "Year"
names(melted_data_mr)[4] <- "mr"
#
querry_melted <- filter(melted_data_mr, Age==0, Sex=='F' )
querry_melted <- transform(melted_data_mr, Year = as.numeric(Year), stringsAsFactors = FALSE)
mr_mean_by_age <- melted_data_mr %>%
  group_by(Age, Sex, Year) %>%
    summarise(mean = mean(mr))
#
mr_sd_by_age <- melted_data_mr %>%
  group_by(Age, Sex, Year) %>%
    summarise(sd = sd(mr))
#
mr_confidence_band_by_age <- melted_data_mr %>%
  group_by(Age, Sex,Year) %>%
    summarise(mean=mean(mr), LoCI = mean(mr) - 1.96 * sd(mr), HiCI = mean(mr) + 1.96 * sd(mr))
#
index <- seq(from = 1, to = nrow(mr_mean_by_age),  by=3)
#
p <- ggplot(mr_mean_by_age[index,], aes(x = Year,
                             y = mean,
                             color = Age,
                             shape = Sex,
                             group = interaction(Age, Sex))) +
  geom_point(size=3) +
  geom_line() + # scale_color_grey()+
  theme_classic() +
  theme(axis.text.x = element_text(size=12, angle=90)) +
  scale_color_gradientn(colours = rainbow(5))

ggsave("mean_mortality_rate_by_ages", width=golden_width, height=golden_height,
         device="ps", units="mm")

ages <- c(0, 20, 25, 30)
colors <- c("Simulation-mean" = "red", "Confidence at 95%" = "grey80", "Italy Data" = "black")
#
plotlist <- list()
cont <- 1
for (k in ages){
  woman_mr_by_age_k_summary <- filter(mr_confidence_band_by_age, Age==k, Sex=='F')
  woman_mr_by_age_k_summary$Year <- as.numeric(as.character(woman_mr_by_age_k_summary$Year))
  mr_italy_by_age_k <- filter(italy_mr_df, Age==k)
  woman_mr_italy_by_age_k <- select(mr_italy_by_age_k, Year, Female )
  str_title <- paste("Age:", toString(k))
  #
  if (cont==1){
    p_age_k <- ggplot(woman_mr_by_age_k_summary, aes(Year, mean)) +
      geom_line(aes(y = LoCI)) +
      geom_line(aes(y = HiCI)) +
      geom_ribbon( aes(ymin=LoCI, ymax=HiCI, fill="Confidence at 95%")) +
      geom_line(aes(y = mean, colour = "Simulation-mean"), size=1.5) +
      geom_point(data = woman_mr_italy_by_age_k, aes(x = Year, y=Female, colour="Italy Data")) +
      ggtitle(str_title) +
      scale_color_manual(values = colors,  name = "", guide = guide_legend(override.aes=aes(fill=NA))) +
      scale_fill_manual(values = c("grey80"), name="") +
      labs(x = "Year", y = "Mortality Rate (MR)",  color = "Legend") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(size=12, angle=90))
  }
  else{
    p_age_k <- ggplot(woman_mr_by_age_k_summary, aes(Year, mean)) +
      geom_line(aes(y = LoCI)) +
      geom_line(aes(y = HiCI)) +
      geom_ribbon( aes(ymin=LoCI, ymax=HiCI, fill="Confidence at 95%")) +
      geom_line(aes(y = mean, color="Simulation-mean"), size=1.5) +
      geom_point(data = woman_mr_italy_by_age_k, aes(x = Year, y=Female, color="Italy Data")) +
      #
      ggtitle(str_title) +
      scale_fill_manual(values = c("grey80"), name="Band") +
      labs(x = "Year", y = "Mortality Rate (MR)",  color = "Legend") +
      scale_color_manual(values = colors, name = "",guide = guide_legend(override.aes=aes(fill=NA))) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(size=12, angle=90))
      #theme(legend.position = "none")
  }
   plotlist[[length(plotlist) + 1]] <- p_age_k
   cont <- cont + 1
}
golden_height <- golden_width * golden_ratio
grid_arrange_shared_legend(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], nrow = 2, ncol = 2)
