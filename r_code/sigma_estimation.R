sigma_estimation <- function(Hurst_est) {

  ages <- 0:91
  years <- 1950:2004

  K <- 6
  ak <- mat.or.vec(1, K + 1)
  for (i in 0:K) {
    ak[i + 1] <- (((-1)^(1 - i)) * factorial(K)) /
      ((2^K) * factorial(i) * factorial(K - i))
  }

  b <- c(0.48296291314453, -0.8365163037378, 0.22414386804201, 0.12940952255126)
  bk <- b / sqrt(2)
  K1 <- 2 * (K + 1)
  ak2 <- mat.or.vec(1, K1 + 1)

  for (i in 0:K + 1) {
    ak2[2 * i + 1] <- 0
    ak2[2 * i] <- ak[i]

  }

  bk2 <- mat.or.vec(1, 8 + 1)
  for (i in 1:4) {
    bk2[2 * i + 1] <- 0
    bk2[2 * i] <- bk[i]
  }
  #
  c <- c(1, -2, 1)
  ck <- c / 4
  ck2 <- mat.or.vec(1, 5)

  for (i in 1:3) {
    #ck2[2*i+1]<-0
    ck2[2 * i - 1] <- ck[i]
  }
  #
  EneN <- length(years)
  # Computing V_N,a 
  K <- length(bk)   ## descomentar este cuando se use bk
  K1 <- length(bk2) - 1
  N1 <- EneN - K1
  N <- EneN - K

  NNHMuj <- ages
  HHom <- ages
  VnaMuj <- ages - ages
  VnaHom <- ages - ages
  VnaMuj_a2 <- VnaMuj
  VnaHom_a2 <- VnaHom
  #

  Hurst_est_woman <- matrix(unlist(H_est[1]), ncol = 4)
  Hurst_est_man <- matrix(unlist(H_est[2]), ncol = 4)

  H_size <- length(Hurst_est_woman[1,])
  sigma_woman <- mat.or.vec(H_size, 1)
  sigma_man <- mat.or.vec(H_size, 1)

  for (h in ages) {
    sum1 <- 0
    sum2 <- 0
    for (k in 1:K) {
      for (l in 1:K) {
        if (k != l) {
          sum1 <- sum1 + ak[k] *
            ak[l] *
            (abs(k - l))^
              (2 * Hurst_est_woman[[h + 1, 2]])
          sum2 <- sum2 + ak[k] *
            ak[l] *
            (abs(k - l))^
              (2 * Hurst_est_man[[h + 1, 2]])
        }
        if (k == l) {
          sum1 <- sum1 + 0
          sum2 <- sum2 + 0
        }
      }
    }
    sigma_woman[h + 1] <- abs(2 * VnaMuj[h + 1] / sum1)^(1 / 2)
    sigma_man[h + 1] <- abs(2 * VnaHom[h + 1] / sum2)^(1 / 2)
  } ## fin del for h
  sigma <- list(sigma_woman, sigma_man)
  return(sigma)
}