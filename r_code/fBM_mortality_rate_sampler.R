fBM_mortality_rate_sampler <- function( H_est_woman,
                                       H_est_man, n_paths = 100000,
                                       alpha_woman, alpha_man,
                                       lambda_woman, lambda_man,
                                       file_name='ht_hat_samples.cvs') 
{ 
  #
  data_mortality_rate <- read.csv('data_mortality_rate.csv', header = TRUE)
  data_mortality_rate2 <- read.csv('data_mortality_rate2.csv', header = TRUE)
  
  print(head(data_mortality_rate))
  # print(head(data_mortality_rate2))
  SDW <- mat.or.vec(n_paths, 65)
  SDM <- mat.or.vec(n_paths, 65)

  ages <- 0:91
  ages1 <- c(0, seq(5, 90, by = 5))
  years1 <- 1872:2014
  cy1 <- length(years1[79:142])
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
    Res1 <- (SDW - Dmed) ^ 2
    Var_Point <- colMeans(Res1)
    L <- 2014 - 1950
    Desv_stan <- sqrt(Var_Point / L)
    #
    querry <- filter(data_mortality_rate, Year >= 1950 & Year <= 2014 &
                       Age == age)
    # initial condition Women and men
    hw0 <- querry$Female[1]
    hm0 <- querry$Male[1]
    htWomen <- mat.or.vec(L + 1, 2)
    htMen <- mat.or.vec(L + 1, 2)
    htWomen[1, 2] <- hw0
    htMen[1, 2] <- hm0
    H1 <- HW_est[[1]]
    H2 <- HM_est[[1]]
    #
    for (i in 1931:2014) {
      htWomen[i - 1949, 1] <- i # time variable
      htMen[i - 1949, 1] <- i
      htWomen[i - 1949, 2] <- hw0 * exp(alpha_woman[age + 1] * i +
                                          SDW[3, i - 1949])
      htMen[i - 1949, 2] <- hm0 * exp(alpha_man[age + 1] * i +
                                        SDM[1, i - 1949])
    }
  }
  # ht_hat computation
  ht_hat_woman <- mat.or.vec(n_paths, cy1)
  ht_hat_man <- mat.or.vec(n_paths, cy1)
  for (H in 1:n_paths) { 
    Yt_hat <- mat.or.vec(cy1, 1)
    Yt_hatM <- mat.or.vec(cy1, 1)
    Yt_hat[1] <- 0
    Yt_hatM[1] <- 0
    sum1 <- 0
    sum2 <- 0
    for (i in 1:(cy1 - 1)) ## i-loop build the function of t
    {
      for (k in 1:i)  ## k-loop build the function of u
      {
        sum1 <- sum1 + 
          exp(-lambda_woman[age + 1] * (i - k)) * (SDW[H, k + 1] - SDW[H, k])
        sum2 <- sum2 + 
          exp(-lambda_man[age + 1] * (i - k)) * (SDM[H, k + 1] - SDM[H, k])
      }
      Yt_hat[i + 1] <- sum1 * sigma_woman[age + 1] / (cy1 ^ H1)
      Yt_hatM[i + 1] <- sum1 * sigma_man[age + 1] / (cy1 ^ H2)
    }
    ht_hat_woman[H, 1] <- querry$Female[1]
    ht_hat_man[H, 1] <- querry$Male[1]
    
    for (j in 2:cy1 - 1) {
      ht_hat_woman[H, j] <- querry$Female[1] * 
        exp(alpha_woman[age + 1] * j + Yt_hat[j])
      ht_hat_man[H, j] <- querry$Male[1] * 
        exp(alpha_man[age + 1] * j + Yt_hatM[j])
    }
  } 
  #
  sampler_time <- htWomen[, 1]
  # samples <- cbind(sampler_time, htWomen[, 2], htMen[, 2])
  ht_hat_samples <- data.frame("ht_hat_woman" = ht_hat_woman,
                               "ht_hat_man" = ht_hat_man)
  write.csv(ht_hat_samples, file_name, row.names = FALSE)
  return(ht_hat_samples)
}
