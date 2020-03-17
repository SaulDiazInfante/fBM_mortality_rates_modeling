source("hurst_estimation.R")
plot_hurts_estimation <- function(file_name="Hurst-Women.eps", H, golden_width) 
{
  golden_ratio = 1.61803398875
  golden_height = golden_width / golden_ratio
  age = H[, 1]
  hurst_exp = H[, 2]
  fd_whittle = H[, 3]
  rovers = H[, 4]
  len <- length(age)

  df_hurst <- data.frame(age=rep(age, 3), 
                               method=rep(c("Hurst_exp", "FDWhittle", "Rovers"), each=len),
                               hurts_estimation=c(hurst_exp, fd_whittle, rovers))
    hurst_plot <- ggplot(data=df_hurst, aes(x=age, y=hurts_estimation, 
                                                      group=method)) + 
    geom_line(aes(color=method, linetype=method)) +
    theme(legend.title=element_blank()) +
    theme(legend.position="top") +
    labs(y = "Hurts parameter estimation")
    ggsave(file_name, width=golden_width, height=golden_height,
         device="ps", units="in")
}
