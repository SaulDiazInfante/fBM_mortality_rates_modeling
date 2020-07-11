fBM_mortality_rate_estimation <- function(data_mortality_rate, H_est_woman, H_est_man, n_paths = 10000, sigma_woman, sigma_man, lambda_woman, lambda_man, alpha_woman, alpha_man) { ht_hat <- mat.or.vec(n_paths, cy1)
  ht_hatM <- mat.or.vec(n_paths, cy1)
  for (H in 1:n_paths) { Yt_hat <- mat.or.vec(cy1, 1)
    Yt_hatM <- mat.or.vec(cy1, 1)
    Yt_hat[1] <- 0
    Yt_hatM[1] <- 0
    sum1 <- 0
    sum2 <- 0
    for (i in 1:(cy1 - 1)) { ## i makes the function of t
      for (k in 1:i) {  ##?? k makes the function of u
        sum1 <- sum1 + exp(-lambda_woman[age + 1] * (i - k)) * (SDW[H, k + 1] - SDW[H, k])
        sum2 <- sum2 + exp(-lambda_man[age + 1] * (i - k)) * (SDM[H, k + 1] - SDM[H, k]) }
      Yt_hat[i + 1] <- sum1 * sigma_woman[age + 1] / (cy1^H1)
      Yt_hatM[i + 1] <- sum1 * sigma_man[age + 1] / (cy1^H2) }
    ht_hat[H, 1] <- data_mortality_rate$Female[1]
    ht_hatM[H, 1] <- data_mortality_rate$Male[1]
    for (j in 2:cy1 - 1) { if (j > 1) { ht_hat[H, j] <- data_mortality_rate$Female[1] * exp(alpha_woman[age + 1] * (j) + Yt_hat[j])
      ht_hatM[H, j] <- data_mortality_rate$Male[1] * exp(alpha_man[age + 1] * (j) + Yt_hatM[j]) } } } ## end for H   (1/sigma_woman[age+1])*  (cy/H1)*

  ht_hat_mean <- colMeans(ht_hat)
  Res1 <- (ht_hat - ht_hat_mean)^2
  Var_Point <- colMeans(Res1)

  ### Men
  ht_hat_meanM <- colMeans(ht_hatM)
  Res1M <- (ht_hatM - ht_hat_meanM)^2
  Var_PointM <- colMeans(Res1M)
  L <- 2014 - 1950

  Desv_stan <- sqrt(Var_Point / L)
  Desv_stanM <- sqrt(Var_PointM / L) }