library("fractal")
library("pracma")
library("yuima")
library("somebm")
library("plotrix")
library("ggplot2")

source("load_data.R")
source("hurst_estimation.R")
source("hurst_plots.R")
<<<<<<< HEAD
source("sigma_estimation.R")
=======
source("sigma_lambda_estimation.R")
>>>>>>> ae487bacaa6d21b6088d6638b760b1425aa33dbb

data <- load_data()
golden_ratio = 1.61803398875
golden_width = 4.72
golden_height = golden_width / golden_ratio

H_est <- hurst_estimation()
H_est_woman <- matrix(unlist(H_est[1]), ncol = 4)
H_est_man <- matrix(unlist(H_est[2]), ncol = 4)
plot_hurts_estimation("Hurst-Women.eps", H_est_woman, golden_width)
plot_hurts_estimation("Hurst-Man.eps", H_est_man, golden_width)
<<<<<<< HEAD
sigma <- sigma_estimation(H_est)
=======
sigma_lambda <- sigma_lambda_estimation(H_est)
>>>>>>> ae487bacaa6d21b6088d6638b760b1425aa33dbb
