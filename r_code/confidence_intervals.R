confidence_intervals_plots <- function(file_name='ht_hat_samples.cvs'){
  ht_hat <- read.csv(file_name, header = TRUE)
  ht_hat_woman <- ht_hat[, 1 : 64]
  ht_hat_man <- ht_hat[, 64:128]

  ht_hat_woman_mean <- colMeans(ht_hat_woman)  
  ht_hat_woman_residuals <- (ht_hat_woman - ht_hat_woman_mean) ^ 2
  ht_hat_woman_puntual_variance <- colMeans(ht_hat_woman_residuals)
  L <- 2014 - 1950
  ht_hat_woman_sd <- colMeans(ht_hat_woman_residuals)
  
  ht_hat_man_mean <- colMeans(ht_hat_man)
  ht_hat_man_residuals <- (ht_hat_man - ht_hat_man_mean) ^ 2
  ht_hat_man_puntual_variance <- colMeans(ht_hat_man_residuals)
  ht_hat_man_sd <- colMeans(ht_hat_man_residuals)
  
  ages <- c()
} 
