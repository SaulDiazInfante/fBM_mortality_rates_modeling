sigma_lambda_estimation <- function(Hurst_est){
  H_est_woman <- matrix(unlist(Hurst_est[1]), ncol = 4)
  H_est_man <- matrix(unlist(Hurst_est[2]), ncol = 4)
  alpha_woman <- matrix(unlist(Hurst_est[2]), ncol = 4)
  alpha_man <- matrix(unlist(Hurst_est[2]), ncol = 4)
  ages <- 0:91
  years <- 1950:2004
  cy <- length(years)
  
  K <- 6
  ak <- mat.or.vec(1, K + 1)
  for (i in 0:K) {
    ak[i + 1] <- (((-1)^(1 - i)) *
                    factorial(K)) / 
                      ((2 ^ K) * factorial( i ) * factorial(K - i)) 
  }
  
  b <- c(0.48296291314453, 
         -0.8365163037378, 
         0.22414386804201, 
         0.12940952255126)
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
  c <- c(1, -2, 1)
  
  ck <- c / 4
  ck2 <- mat.or.vec(1, 5)
  
  for (i in 1:3) {
    ck2[2 * i - 1] <- ck[i]
  }
  
  EneN <- length(years)
  
  
  #### calculo de V_N,a
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
  
  datos2 <- data.frame()
  drates <- read.table("Deaths_Rates_Italy.txt",
                       dec = ".",
                       header = TRUE,
                       na.strings = ".")
  datos <- drates[drates$Age %in% c(0:91) & drates$Year %in% c(1950:2004) , ]
  datos$FemalePost <- datos$Female
  datos$MalePost <- datos$Male
  
  for (A in ages) {
    datos1 <- datos[datos$Year %in% c(1950:2004) & datos$Age == ages[A + 1], ]
    datos1$FemalePost <- log(datos1$Female) - log(datos1$Female[1]) - 
      alpha_woman[A + 1] * (datos1$Year)
    datos1$MalePost <- log(datos1$Male) -
      log(datos1$Male[1]) - alpha_man[A + 1] * (datos1$Year)
    datos2 <- rbind(datos2, datos1)
  }

  K <- 6
  ak <- mat.or.vec(1, K + 1)
  for (i in 0:K) {
    ak[i + 1] <- (((-1)^(1 - i)) * 
                    factorial(K)) / ((2 ^ K) * factorial(i) *
                                       factorial(K - i)) 
  }
  
  b <- c(0.48296291314453, 
         -0.8365163037378, 
         0.22414386804201,
         0.12940952255126)
  
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
  c <- c(1, -2, 1)
  ck <- c/4
  ck2 <- mat.or.vec(1,5)
  
  for (i in 1:3) {
    #ck2[2*i+1]<-0
    ck2[2 * i - 1] <- ck[i]
  }
  EneN <- length(years)
  #### calculo de V_N,a
  K <- length(bk)   ## descomentar este cuando se use bk
  K1 <- length(bk2) - 1
  N1 <- EneN - K1
  N <- EneN - K
  
  ### comentar la siguiente linea si se usa bk
  #N<-EneN-K
  
  NNHMuj <- ages
  HHom <- ages  
  VnaMuj <- ages - ages
  VnaHom <- ages - ages
  VnaMuj_a2 <- VnaMuj
  VnaHom_a2 <- VnaHom
  
  for (h in ages) {
    datos <- datos2[datos2$Year %in% c(1950:2004) & datos2$Age == ages[h + 1], ]
    sumMuj <- 0
    sumHom <- 0
    
    for (i in 0:N) {
      sum5 <- 0
      sum6 <- 0
      for (j in 1:K) {
        ik <- i + 1950 + j - 1
        sum5 <- sum5 + (bk[j] * datos$FemalePost[datos$Year == ik])
        sum6 <- sum6 + (bk[j] * datos$MalePost[datos$Year == ik])    
        
      } # fin for j
      sumMuj <- sumMuj + sum5 ^ 2 
      sumHom <- sumHom + sum6 ^ 2 
    } #fin for i
    VnaMuj[h + 1] <- sumMuj / N
    VnaHom[h + 1] <- sumHom / N
    
    
    #### calculo de V_N,a^2, aqui continua el for de h
    
    
    sumMuj_a2 <- 0
    sumHom_a2 <- 0
    for (i in 1:N1)
    {
      sum2 <- 0
      sum3 <- 0
      for (j in 1:K1)
      {
        ik <- i + 1950 + j - 1
        sum2 <- sum2 + (bk2[j] * datos$FemalePost[datos$Year == ik])
        sum3 <- sum3 + (bk2[j] * datos$MalePost[datos$Year == ik])    
        
      }
      sumMuj_a2 <- sumMuj_a2 + sum2 ^ 2 
      sumHom_a2 <- sumHom_a2 + sum3 ^ 2 
    }  ## fin for i
    
    VnaMuj_a2[h + 1] <- sumMuj_a2 / N
    VnaHom_a2[h + 1] <- sumHom_a2 / N
    
    
  } 
  
  
  
  
  HMuj <- 0.5 * log(VnaMuj_a2[1:91] / VnaMuj[1:91], 2)
  HHom <- 0.5 * log(VnaHom_a2[1:91] / VnaHom[1:91], 2)
  datos2$Fem1 <- datos2$FemalePost
  datos2$Mal1 <- datos2$MalePost
  for (h in ages) {
    H1 <- H_est_woman[[h + 1, 2]]
    H2 <- H_est_man[[h + 1, 2]]
    datos2$FemalePost[datos2$Age == ages[h + 1]] <- 
      datos2$FemalePost[datos2$Age == ages[h + 1]]
    datos2$MalePost[datos2$Age == ages[h + 1]] <- 
      datos2$FemalePost[datos2$Age == ages[h + 1]]
  }
  ####* * * * * * * * Sigma estimation * * * *  #####
  
  H <- length(H_est_woman[1,])
  
  sigmaMuj <- mat.or.vec(H,1)
  sigmaHom <- mat.or.vec(H,1)
  
  for (h in ages) {
    sum1 <- 0
    sum2 <- 0
    for (k in 1:K) {
      for (l in 1:K) {
        if (k != l) {
          sum1 <- sum1 + ak[k] * 
            ak[l] * (abs(k - l)) ^ (2 * H_est_woman[[h + 1, 2]])
          sum2 <- sum2 + ak[k] * 
            ak[l] * (abs(k - l)) ^ (2 * H_est_man[[h + 1, 2]])
        } 
        if (k == l) {
          sum1 <- sum1 + 0
          sum2 <- sum2 + 0
        } 
      } 
    } 
    sigmaMuj[h + 1] <- 
      abs(2 * VnaMuj[h + 1] / sum1) ^ (1 / 2)
    sigmaHom[h + 1] <- 
      abs(2 * VnaHom[h + 1] / sum2) ^ (1 / 2)
    
  }
  
  sigmaMuj
  sigmaHom
  
  ###### #####  *  *   *  *  *   *   *  * begin estimating \alpha_1
  
  alpha1Muj <- 0:91
  alpha1Hom <- 0:91
  
  for (A in ages) {
    sum1 <- 0
    sum2 <- 0
    sum3 <- 0
    sum4 <- 0
    
    datos <- 
      drates[drates$Year %in% c(1950:2004) & drates$Age == ages[A + 1], ]
    
    for (B in years) {
      sum1 <- sum1 +  log(datos$Female[datos$Year == B])        
      sum2 <- sum2 + (datos$Year[datos$Year == B])
      sum3 <- sum3 + (datos$Year[datos$Year == B])
      sum4 <- sum4 +  log(datos$Male[datos$Year == B])        
    }  ## fin for B
    alpha1Muj[A + 1] <- (sum1 - 
                           log(datos$Female[datos$Year == 1950]) * cy) / sum2
    alpha1Hom[A + 1] <- (sum4 - log(datos$Male[datos$Year == 1950]) * cy ) / sum2
    
  }
  mu2N <- mat.or.vec(length(ages),1)
  lambdaNMuj <- mat.or.vec(length(ages),1)
  lambdaNHom <- mat.or.vec(length(ages),1)
  
  for (h in ages[1:91]) {
    
    datos <- datos2[datos2$Year
                    %in% c(1950:2004) & datos2$Age == ages[h + 1], ]
    sum7 <- 0
    sum8 <- 0
    for (i in years) {
      sum7 <- sum7 +  (datos$FemalePost[datos$Year == i]) ^ 2
      sum8 <- sum8 + (datos$MalePost[datos$Year == i]) ^ 2
      
    }
    mu2N[h + 1] <- sum7
    H1 <- H_est_woman[[j + 1, 2]]
    H2 <- H_est_man[[j + 1, 2]]
    lambdaNMuj[h + 1] <- ((2 * mu2N[h + 1]) /
(((sigmaMuj[h + 1]) ^ 2) *
gamma(2 * H1 + 1))) ^ (-0.5 / H1)
lambdaNHom[h + 1] <-
((2 * mu2N[h + 1]) / (((sigmaHom[h + 1]) ^ 2) *
gamma(2 * H2 + 1)))^(-0.5 / H2)
} # fin de for h

### * * * * * * *  estimation of lambda_N * * * * * #####
lambdaNMuj
lambdaNHom
sigma_lambda<- list(sigmaMuj, sigmaHom, lambdaNMuj, lambdaNHom)
return(sigma_lambda) }