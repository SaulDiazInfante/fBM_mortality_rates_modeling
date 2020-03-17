ht_hat <- mat.or.vec(NS, cy1)
ht_hatM <- mat.or.vec(NS, cy1)
for (H in 1:NS) {
  Yt_hat <- mat.or.vec(cy1, 1)
  Yt_hatM <- mat.or.vec(cy1, 1)
  Yt_hat[1] <- 0
  Yt_hatM[1] <- 0
  sum1 <- 0
  sum2 <- 0
  for (i in 1:(cy1 - 1)) {## i makes the function of t
    for (k in 1:i) {  ##?? k makes the function of u
      sum1 <- sum1 + exp(-lambdaNMuj[age + 1] * (i - k)) *
        (SDW[H, k + 1] - SDW[H, k])
      sum2 <- sum2 + exp(-lambdaNHom[age + 1] * (i - k)) *
        (SDM[H, k + 1] - SDM[H, k])
    }
    Yt_hat[i + 1] <- sum1 * sigmaMuj[age + 1] / (cy1 ^ H1)
    Yt_hatM[i + 1] <- sum1 * sigmaHom[age + 1] / (cy1 ^ H2)
  }
  ht_hat[H, 1] <- datos$Female[1]
  ht_hatM[H, 1] <- datos$Male[1]
  for (j in 2:cy1 - 1) {
    if (j > 1) {
      ht_hat[H,j] <- datos$Female[1] * exp(alphaMuj[age + 1] * (j) + Yt_hat[j])
      ht_hatM[H,j] <- datos$Male[1] * exp(alphaHom[age + 1] * (j) + Yt_hatM[j])
    }
  }
} ## end for H   (1/sigmaMuj[age+1])*  (cy/H1)*

ht_hat_mean <- colMeans(ht_hat)
Res1 <- (ht_hat - ht_hat_mean) ^ 2
Var_Point <- colMeans(Res1)

### Men
ht_hat_meanM <- colMeans(ht_hatM)
Res1M <- (ht_hatM - ht_hat_meanM) ^ 2
Var_PointM <- colMeans(Res1M)
L <- 2014 - 1950

Desv_stan <- sqrt(Var_Point / L)
Desv_stanM <- sqrt(Var_PointM / L)
