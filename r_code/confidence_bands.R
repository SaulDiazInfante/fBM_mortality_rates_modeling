# Title     : TODO
# Objective : TODO
# Created by: saul
# Created on: 4/14/20
confidence_intervals_plots <- 
  function(data_file_name = 'ht_hat_samples.csv'){
  require("ggplot2")
  require("ggthemes")
  require("dplyr")
  require("gridExtra")
  require("reshape")
  require("data.table")
  require("RColorBrewer")
  require("grid")
  require("ggpubr")
  require("cowplot")  
  source("shared_legend.R")
    ####
    ####
    ####
  golden_width <- 116
  golden_ratio <- 1.61803398875
  golden_height <- golden_width / golden_ratio
  ht_hat <- fread(data_file_name, header = T, sep = ',')
  data_file_name <- 'data_mortality_rate.csv'
  raw_italy_mr_df <- fread(data_file_name, header = T, sep = ',')
  italy_mr_df <- filter(raw_italy_mr_df, Year <= 2004)
  italy_mr_df_forcast <- filter(raw_italy_mr_df, Year > 2004, Year <= 2012)
  #
  melted_data_mr <- melt(ht_hat, id = c("Age", "Sex"), measured = names[1:63], 
                         stringsAsFactors = FALSE )
  #
  names(melted_data_mr)[3] <- "Year"
  names(melted_data_mr)[4] <- "mr"
  #
  mr_mean_by_age <- melted_data_mr %>%
    group_by(Age, Sex, Year) %>%
      summarise(mean = mean(mr))
  #
  #
  mr_confidence_band_by_age <- melted_data_mr %>%
    group_by(Age, Sex,Year) %>%
      summarise(mean = mean(mr), LoCI = mean(mr) - 1.96 * sd(mr), 
                HiCI = mean(mr) + 1.96 * sd(mr))
  #
  index <- seq(from = 1, to = nrow(mr_mean_by_age),  by = 3)
  #
  p <- ggplot(mr_mean_by_age[index,], aes(x = Year,
                             y = mean,
                             color = Age,
                             shape = Sex,
                             group = interaction(Age, Sex))) +
    geom_point(size = 3) +
    geom_line() + # scale_color_grey()+
    theme_classic() +
    theme(axis.text.x = element_text(size = 12, angle = 90)) +
    scale_color_gradientn(colours = rainbow(5))
  #
  ggsave("mean_mortality_rate_by_ages.eps", width = golden_width, 
         height = golden_height,
         device = "ps", units = "mm")
  #### Woman confidence band plot ####
  theme_set(
    theme_bw() +
      theme(legend.position = "top")
  )
  ages <- c(0, 20, 40, 60)
  colors <- c("Mean" = "red", 
              "Confidence" = "grey80", 
              "Italy Data" = "black",
              "Forecasting" = "blue")
  plotlist <- list()
  cont <- 1
  for (k in ages) {
    woman_mr_by_age_k_summary <- filter(mr_confidence_band_by_age, 
                                        Age == k, Sex == 'F')
    woman_mr_by_age_k_summary$Year <- 
      as.numeric(as.character(woman_mr_by_age_k_summary$Year))
    mr_italy_by_age_k <- filter(italy_mr_df, Age == k)
    mr_italy_forecast_by_age_k <- filter(italy_mr_df_forcast, Age == k)
    woman_mr_italy_by_age_k <- select(mr_italy_by_age_k, Year, Female )
    woman_mr_italy_forcast_by_age_k <- 
      select(mr_italy_forecast_by_age_k, Year, Female )
    str_title <- paste("Age:", toString(k))
    #
    if (cont == 1) {
      p_age_k <- ggplot(woman_mr_by_age_k_summary, aes(Year, mean)) +
        geom_line(aes(y = LoCI)) +
        geom_line(aes(y = HiCI)) +
        geom_ribbon( aes(ymin = LoCI, ymax = HiCI, fill = "Confidence")) +
        geom_line(aes(y = mean, colour = "Mean"), size = 1.0) +
        geom_point(data = woman_mr_italy_by_age_k,
                   aes(x = Year,
                       y = Female,
                       colour = "Italy Data")) +
        geom_point(data = woman_mr_italy_forcast_by_age_k,
                   aes(x = Year, y = Female, colour = "Forecasting")) +
        ggtitle(str_title) +
        scale_color_manual(values = colors,  
                           name = "", 
                           guide = guide_legend(override.aes = 
                                                  aes(fill = NA))) +
        scale_fill_manual(values = "grey80", name = "") +
        labs(x = "Year", y = "Mortality Rate", color = "Legend") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(size = 10, angle = 90))
    }
    else{
      p_age_k <- ggplot(woman_mr_by_age_k_summary, aes(Year, mean)) +
        geom_line(aes(y = LoCI)) +
        geom_line(aes(y = HiCI)) +
        geom_ribbon( aes(ymin = LoCI, ymax = HiCI, fill = "Confidence")) +
        geom_line(aes(y = mean, color = "Mean"), size = 1) +
        geom_point(data = woman_mr_italy_by_age_k, 
                   aes(x = Year, y = Female, color = "Italy Data")) +
        geom_point(data = woman_mr_italy_forcast_by_age_k,
                   aes(x = Year, y = Female, colour = "Forecasting")) +
        #
        ggtitle(str_title) +
        scale_fill_manual(values = "grey80", name = "Band") +
        labs(x = "Year", y = "Mortality Rate",  color = "Legend") +
        scale_color_manual(values = colors, 
                           name = "",
                           guide = guide_legend(override.aes = 
                                                  aes(fill = NA))) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(size = 10, angle = 90))
    }
     plotlist[[length(plotlist) + 1]] <- p_age_k
     cont <- cont + 1
  }
  #
  grid_arrange_shared_legend(plotlist[[1]],
                             plotlist[[2]],
                             plotlist[[3]],
                             plotlist[[4]],
                             nrow = 2,
                             ncol = 2,
                             position = c("bottom", "right"),
                             common_title_plot = "Woman",
                             file_name = 'woman_confidence_bands.eps')

  #### Man confidence band plot ####
  k <- 0
  plotlist <- list()
  cont <- 1
  for (k in ages) {
    man_mr_by_age_k_summary <- filter(mr_confidence_band_by_age, 
                                      Age == k, Sex == 'M')
    man_mr_by_age_k_summary$Year <- 
      as.numeric(as.character(man_mr_by_age_k_summary$Year))
    mr_italy_by_age_k <- filter(italy_mr_df, Age == k)
    man_mr_italy_by_age_k <- select(mr_italy_by_age_k, Year, Male )
    man_mr_italy_forcast_by_age_k <- 
      select(mr_italy_forecast_by_age_k, Year, Male)
    str_title <- paste("Age:", toString(k))
    #
    if (cont == 1) {
      p_age_k <- ggplot(man_mr_by_age_k_summary, aes(Year, mean)) +
        geom_line(aes(y = LoCI)) +
        geom_line(aes(y = HiCI)) +
        geom_ribbon( aes(ymin = LoCI, 
                         ymax = HiCI, fill = "Confidence")) +
        geom_line(aes(y = mean, colour = "Mean"), size = 1.5) +
        geom_point(data = man_mr_italy_by_age_k,
                   aes(x = Year, y = Male, colour = "Italy Data")) +
        geom_point(data = man_mr_italy_forcast_by_age_k,
                   aes(x = Year, y = Male, colour = "Forecasting")) +
        ggtitle(str_title) +
        scale_color_manual(values = colors,
                           name = "",
                           guide = guide_legend(override.aes = 
                                                  aes(fill = NA))) +
        scale_fill_manual(values = "grey80", name = "") +
        labs(x = "Year", y = "Mortality Rate",  color = "Legend") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(size = 10, angle = 90))
    }
    else{
      p_age_k <- ggplot(man_mr_by_age_k_summary, aes(Year, mean)) +
        geom_line(aes(y = LoCI)) +
        geom_line(aes(y = HiCI)) +
        geom_ribbon( aes(ymin = LoCI, 
                         ymax = HiCI, fill = "Confidence")) +
        geom_line(aes(y = mean, color = "Mean"), size = 1.5) +
        geom_point(data = man_mr_italy_by_age_k,
                   aes(x = Year, y = Male, color = "Italy Data")) +
        geom_point(data = man_mr_italy_forcast_by_age_k,
                   aes(x = Year, y = Male, colour = "Forecasting")) +
        #
        ggtitle(str_title) +
        scale_fill_manual(values = "grey80", name = "Band") +
        labs(x = "Year", y = "Mortality Rate",  color = "Legend") +
        scale_color_manual(values = colors,
                           name = "",
                           guide = guide_legend(override.aes = 
                                                  aes(fill = NA))) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(size = 10, angle = 90))
    }
     plotlist[[length(plotlist) + 1]] <- p_age_k
     cont <- cont + 1
  }
   grid_arrange_shared_legend(plotlist[[1]],
                             plotlist[[2]],
                             plotlist[[3]],
                             plotlist[[4]],
                             nrow = 2,
                             ncol = 2,
                             position = c("bottom", "right"),
                             common_title_plot = "Man",
                             file_name = 'man_confidence_bands.eps')
}
