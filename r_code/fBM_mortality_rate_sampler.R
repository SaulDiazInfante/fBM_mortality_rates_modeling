library("fractal")
library("fractal")
library("pracma")
library("yuima")
library("somebm")
library("plotrix")

fBM_mortality_rate_sampler <- function(data_mortality_rate, data_mortality_rate2,
                                       H_est_woman,
                                       H_est_man, n_paths = 100000,
                                       alpha_woman, alpha_man) { 
  SDW <- mat.or.vec(n_paths, 65)
  SDM <- mat.or.vec(n_paths, 65)

  ages <- 0:91
  ages1 <- c(0, seq(5, 90, by = 5))

  for (A in ages1) { 
    age <- ages[[A + 1]]
    HW_est <- H_est_woman[age + 1, 2]
    HM_est <- H_est_man[age + 1, 2]
    for (i in 1:n_paths) {

      #d<-ts(fbm(hurst=0.7, n=75),start=c(1930, 1),end=c(2004,1),frequency=1)
      SDW[i,] <- ts(fbm(hurst = HW_est, n = 65),
                    start = c(1950, 1),
                    end = c(2014, 1),
                    frequency = 1)
      SDM[i,] <- ts(fbm(hurst = HM_est, n = 65),
                    start = c(1950, 1),
                    end = c(2014, 1),
                    frequency = 1)
    }
    DW <- SDW[, 1:55]
    DM <- SDM[, 1:55]
    Dmed <- colMeans(SDW)
    Res1 <- (SDW - Dmed)^2
    Var_Point <- colMeans(Res1)
    L <- 2014 - 1950

    Desv_stan <- sqrt(Var_Point / L)
    
    data <- data_mortality_rate[data_mortality_rate$Year %in% 1950:2014 & data_mortality_rate$Age == age,]


    ## initial condition Women and men
    hw0 <- data$Female[data$Age == age][1]
    hm0 <- data_mortality_rate2$Male[data$Age == age][1]

    htWomen <- mat.or.vec(L + 1, 2)
    htMen <- mat.or.vec(L + 1, 2)
    htWomen[1, 2] <- hw0
    htMen[1, 2] <- hm0
    H1 <- HW_est[[1]]
    H2 <- HM_est[[2]]

    for (i in 1931:2014) {
      htWomen[i - 1949, 1] <- i # time variable
      htMen[i - 1949, 1] <- i
      htWomen[i - 1949, 2] <- hw0 * exp(alpha_woman[age + 1] * i + SDW[3, i - 1949])
      htMen[i - 1949, 2] <- hm0 * exp(alpha_man[age + 1] * i + SDM[1, i - 1949])
    }
  }
  sampler_time <- htWomen[, 1]
  samples <- cbind(sampler_time, htWomen[, 2], htMen[, 2])
}