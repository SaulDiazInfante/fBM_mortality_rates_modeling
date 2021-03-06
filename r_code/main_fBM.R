library("fractal")
library("pracma")
library("yuima")
library("somebm")
library("plotrix")
library("ggplot2")
source("load_data.R")
source("hurst_estimation.R")
source("hurst_plots.R")
source("sigma_lambda_estimation.R")
source("fBM_mortality_rate_sampler.R")
source("mortality_rate_estimation.R")

data <- load_data()
golden_ratio <- 1.61803398875
golden_width <- 4.72
golden_height <- golden_width / golden_ratio
H_est <- hurst_estimation()
H_est_woman <- matrix(unlist(H_est[1]), ncol = 4)
H_est_man <- matrix(unlist(H_est[2]), ncol = 4)
plot_hurts_estimation("Hurst-Women.eps",
                      title_label =  "Women",
                      H_est_woman, golden_width)
plot_hurts_estimation("Hurst-Men.eps",
                      title_label =  "Men",
                      H_est_man, 
                      golden_width)
alpha_woman <- matrix(unlist(H_est[3]), ncol = 1)
alpha_man <- matrix(unlist(H_est[4]), ncol = 1)
data2 <- matrix(unlist(H_est[5]), ncol = 4)
sigma_lambda <- sigma_lambda_estimation(H_est)
sigma_woman <- matrix(unlist(sigma_lambda[1]), ncol = 1)
sigma_man <- matrix(unlist(sigma_lambda[2]), ncol = 1)
lambda_woman <- matrix(unlist(sigma_lambda[3]), ncol = 1)
lambda_man <- matrix(unlist(sigma_lambda[2]), ncol = 1)
fBM_mortality_rate_sampler(data,
                           data2,
                           H_est_woman,
                           H_est_man,
                           100000,
                           alpha_woman,
                           alpha_man)
#### TODO: Figures title according to sex ####