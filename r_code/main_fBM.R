library("fractal")
library("pracma")
library("somebm")
library("ggplot2")
library("dplyr")
#
source("load_data.R")
source("hurst_estimation.R")
source("hurst_plots.R")
source("sigma_lambda_estimation.R")
source("fBM_mortality_rate_sampler.R")
source("mortality_rate_estimation.R")

data <- load_data_mortality_rate()
golden_width <- 4.72
H_est <- hurst_estimation()

data2 <- data.frame(H_est[5])
H_est_woman <- matrix(unlist(H_est[1]), ncol = 4)
H_est_man <- matrix(unlist(H_est[2]), ncol = 4)

# plot_hurts_estimation("Hurst-Women.eps", H_est_woman, golden_width)
# plot_hurts_estimation("Hurst-Man.eps", H_est_man, golden_width)

alpha_woman <- matrix(unlist(H_est[3]), ncol = 1)
alpha_man <- matrix(unlist(H_est[4]), ncol = 1)

sigma_lambda <- sigma_lambda_estimation(H_est)
sigma_woman <- matrix(unlist(sigma_lambda[1]), ncol = 1)
sigma_man <- matrix(unlist(sigma_lambda[2]), ncol = 1)

lambda_woman <- matrix(unlist(sigma_lambda[3]), ncol = 1)
lambda_man <- matrix(unlist(sigma_lambda[2]), ncol = 1)

write.csv(data2, 'data_mortality_rate2.csv')
fBM_mortality_samples <- fBM_mortality_rate_sampler(H_est_woman, H_est_man,
                                                    100, alpha_woman, 
                                                    alpha_man, lambda_woman,
                                                    lambda_man)


