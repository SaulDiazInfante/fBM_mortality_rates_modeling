source("hurst_estimation.R")
golden_ratio = 1.61803398875
golden_width = 4.72
golden_height = golden_width / golden_ratio
H_est <- hurst_estimation()
H_est_woman <- H_est[1]
H_est_man <- H_est[2]

plot_hurts_estimation <-function(file_name="Hurst-Women.eps", H){
  H <- matrix(unlist(H), ncol = 4)
  age = H[, 1]
  hurst_exp = H[, 2]
  fd_whittle = H[, 3]
  rovers = H[, 4]
  len <- length(age)
  df_hurst <- data.frame(age=rep(age, 3), 
                               method=rep(c("Hurst_exp", "FDWhittle", "Rovers"), each=len),
                               hurts_estimation=c(hurst_exp, fd_whittle, rovers))
  
  women_hurst_plot <- ggplot(data=df_hurst, aes(x=age, y=hurts_estimation, 
                                                      group=method)) + 
    geom_line(aes(color=method, linetype=method)) +
    theme(legend.title=element_blank()) +
    theme(legend.position="bottom") +
    labs(y = "Hurts parameter estimation")
  
    
  ggsave(file_name, width=golden_width, height=golden_height,
         device="ps", units="in")
  women_hurst_plot
}
plot_hurts_estimation("Hurts-Women.eps", H_est_woman)
plot_hurts_estimation("Hurts-Man.eps", H_est_man)