library("fractal")
library("pracma")
library("yuima")
library("somebm")
library("plotrix")
library("ggplot2")

hurst_estimation <- function(){
  drates <- read.table("Deaths_Rates_Italy.txt",
                       dec = ".",
                       header = TRUE,
                       na.strings = ".")
#
  ages <- 0:91
  years <- 1950:2004
  alpha_woman <- 0:91
  alpha_man <- 0:91

  ### womens
  for (A in ages) {
    sum1 <- 0
    sum2 <- 0
    sum3 <- 0
    sum4 <- 0
    mortality_rate_data <- 
      drates[drates$Year %in% 1950:2004 & drates$Age == ages[A + 1],]
    for (B in years) {
      sum1 <- sum1 + ((mortality_rate_data$Year[mortality_rate_data$Year == B])
                      - 1950) *
                  log(mortality_rate_data$Female[mortality_rate_data$Year == B])
      sum2 <- sum2 + ((mortality_rate_data$Year[mortality_rate_data$Year == B])
                      - 1950)
      sum3 <- sum3 + ((mortality_rate_data$Year[mortality_rate_data$Year == B])
                      - 1950) ^ 2
      sum4 <- sum4 + ((mortality_rate_data$Year[mortality_rate_data$Year == B])
                      - 1950) *
        log(mortality_rate_data$Male[mortality_rate_data$Year == B])
    }
    alpha_woman[A + 1] <- (sum1 -
                             log(
                               mortality_rate_data$Female[
                                 mortality_rate_data$Year == 1950]) *
                             sum2) / sum3
    alpha_man[A + 1] <- (sum4 -
                           log(mortality_rate_data$Male[
                             mortality_rate_data$Year == 1950]) * sum2 ) / sum3
  }
  mortality_rate_data <- drates[drates$Age %in% c(0:91) &
                                  drates$Year %in% c(1950:2004) , ]
  mortality_rate_data$FemalePost <- mortality_rate_data$Female
  mortality_rate_data$MalePost <- mortality_rate_data$Male
  mortality_rate_data2 <- data.frame()

  for (A in ages) {
    mortality_rate_data1 <- mortality_rate_data[mortality_rate_data$Year %in%
                                                  c(1950:2004) &
                                                  mortality_rate_data$Age ==
                                                  ages[A + 1], ]
    mortality_rate_data1$FemalePost <- log(mortality_rate_data1$Female) -
      log(mortality_rate_data1$Female[1]) - alpha_woman[A + 1] *
      (mortality_rate_data1$Year)
    mortality_rate_data1$MalePost <- log(mortality_rate_data1$Male) -
      log(mortality_rate_data1$Male[1]) -
      alpha_man[A + 1] * (mortality_rate_data1$Year)
    mortality_rate_data2 <- rbind(mortality_rate_data2, mortality_rate_data1)
  }
##### * * * * * Section 3.2, estimating Hurst parameter H with the help of
# * * *  * several R libraries    *  * *
# * * * * * Second we estimated H's directly for the first model
# * * *  *   i.e.    Y_t = B_t^H    *  *   *   *   *
#
  HM1 <- data.frame(0, 0, 0, 0, 0, 0)
  HH1 <- data.frame(0, 0, 0, 0, 0, 0)
  colnames(HM1) <- colnames(HH1) <- c("Age", "Hs", "Hrs", "He", "Hal", "Ht")
  HM2 <- data.frame(0, 0, 0)
  colnames(HM2) <- c("Age", "HWoman", "HMan")
  H3 <- data.frame(0, 0, 0)
  colnames(H3) <- c("Age", "Higuchi_Mu", "Higuchi_Ho")
  H4 <- data.frame(0, 0, 0)
  colnames(H4) <- c("Age", "Rovers_Mu", "Rovers_Ho")
  for (i in ages) {
    HM1[i + 1, 1] <- HH1[i + 1, 1] <- ages[i + 1]
    XX <- hurstexp(mortality_rate_data2$FemalePost[
      mortality_rate_data2$Age == ages[i + 1]], d = 20, display = F)
    XY <- hurstexp(mortality_rate_data2$MalePost[
      mortality_rate_data2$Age == ages[i + 1]],  d = 20, display = F)
    HM2[i + 1, 1] <- ages[i + 1]
    HM2[i + 1, 2] <- FDWhittle(mortality_rate_data2$FemalePost[
      mortality_rate_data2$Age == ages[i + 1]],
                               method = "continuous")
    HM2[i + 1, 3] <- FDWhittle(mortality_rate_data2$MalePost[
      mortality_rate_data2$Age == ages[i + 1]],
                               method = "continuous")
    xx <- mortality_rate_data2$FemalePost[
      mortality_rate_data2$Age == ages[i + 1]]
    xy <- mortality_rate_data2$MalePost[
      mortality_rate_data2$Age == ages[i + 1]]
    H3[i + 1, 1] <- ages[i + 1]
    H3[i + 1, 2] <- hurstBlock(xx, method = "higuchi")[1]
    H3[i + 1, 3] <- hurstBlock(xy, method = "higuchi")[1]

    H4[i + 1, 1] <- ages[i + 1]
    H4[i + 1, 2] <- RoverS(mortality_rate_data2$FemalePost[
      mortality_rate_data2$Age == ages[i + 1]])
    H4[i + 1, 3] <- RoverS(mortality_rate_data2$MalePost[
      mortality_rate_data2$Age == ages[i + 1]])

    for (j in 1:5) {
      HM1[i + 1, j + 1] <- XX[j]
      HH1[i + 1, j + 1] <- XY[j]
    }
  }
##### * * * * * Section 3.2,second part of estimating Hurst parameter H with the
# * * *  * several R libraries    *  * *
# * * * * * Here we estimated H's by using equation (5.1) * * *  *  *  *   *   *
  M <- 1
  BH2Fem <- data.frame(0, 0, 0)
  BH2Mal <- data.frame(0, 0, 0)
  names(BH2Fem) <- c("Age","Year","FemalePost")
  names(BH2Mal) <- c("Age","Year","MalePost")
  for (h in ages) {
    YMale <- 
      mortality_rate_data2[mortality_rate_data2$Age == ages[h + 1], c(1, 7)]
    YFemale <- 
      mortality_rate_data2[mortality_rate_data2$Age == ages[h + 1], c(1, 6)]
    sumInt1 <- 0
    sumInt2 <- 0
    for (i in years) {
      BH2Fem[M, 3] <- YMale$MalePost[YMale$Year == i] + sumInt1
      BH2Fem[M, 1] <- h
      BH2Fem[M, 2] <- i
      BH2Mal[M, 3] <- YFemale$FemalePost[YFemale$Year == i] + sumInt2
      BH2Mal[M, 1] <- h
      BH2Mal[M, 2] <- i
      sumInt1 <- 0
      sumInt2 <- 0
      for (j in years) {
        if (j > 1950) {
          sumInt1 <- sumInt1 + (YMale$MalePost[YMale$Year == j]
                              + YMale$MalePost[YMale$Year == j - 1]) / 2
          sumInt2 <- sumInt2 + (YFemale$FemalePost[YFemale$Year == j]
                              + YFemale$FemalePost[YFemale$Year == j - 1]) / 2
        }
      }
      M <- M + 1
    }
  }

  HM1B <- data.frame(0, 0, 0, 0, 0, 0, 0)
  HH1B <- data.frame(0, 0, 0, 0, 0, 0, 0)
  colnames(HM1B) <- colnames(HH1B) <- 
    c("Age",
      "Hs_hurstexp",
      "Hal_hurstexp",
      "FDDwhitte",
      "aggVar",
      "higuchi",
      "RoverS")
  HM2B <- data.frame(0, 0, 0)
  colnames(HM2B) <- c("Age", "HWoman", "HMan")
  H3 <- data.frame(0, 0, 0, 0, 0)
  colnames(H3) <- c("Age", "Higuchi_Mu","Higuchi_Ho")
  H4 <- data.frame(0, 0, 0)
  colnames(H4) <- c("Age", "Rovers_Mu","Rovers_Ho")

  for (i in ages) {
    HM1B[i + 1, 1] <- HH1B[i + 1, 1] <- ages[i + 1]
    XX <- hurstexp(BH2Fem$FemalePost[BH2Fem$Age == ages[i + 1]],
                   d = 20, display = F)
    XY <- hurstexp(BH2Mal$MalePost[BH2Mal$Age == ages[i + 1]],
                   d = 20, display = F)
    HM2B[i + 1, 1] <- ages[i + 1]
    HM2B[i + 1, 2] <- FDWhittle(BH2Fem$FemalePost[BH2Fem$Age == ages[i + 1]],
                                method = "discrete")
    HM2B[i + 1, 3] <- FDWhittle(BH2Mal$MalePost[BH2Mal$Age == ages[i + 1]],
                              method = "discrete")
    xx <- BH2Fem$FemalePost[BH2Fem$Age == ages[i + 1]]
    xy <- BH2Mal$MalePost[BH2Mal$Age == ages[i + 1]]
  #
    H3[i + 1, 1] <- ages[i + 1]
    H3[i + 1, 2] <- hurstBlock(xx, method = "aggVar")[1]
    H3[i + 1, 3] <- hurstBlock(xx, method = "aggVar")[1]
    H3[i + 1, 4] <- hurstBlock(xy, method = "higuchi")[1]
    H3[i + 1, 5] <- hurstBlock(xy, method = "higuchi")[1]

    H4[i + 1, 1] <- ages[i + 1]
    H4[i + 1, 2] <- RoverS(BH2Fem$FemalePost[BH2Fem$Age == ages[i + 1]])
    H4[i + 1, 3] <- RoverS(BH2Mal$MalePost[BH2Mal$Age == ages[i + 1]])
  ##
    HM1B[i + 1, 2] <- XX[1]
    HM1B[i + 1, 3] <- XX[4]
    HM1B[i + 1, 4] <- HM2B[i + 1, 2]
    HM1B[i + 1, 5] <- H3[i + 1, 2]
    HM1B[i + 1, 6] <- H3[i + 1, 4]
    HM1B[i + 1, 7] <- H4[i + 1, 2]
  ##
    HH1B[i + 1, 2] <- XY[1]
    HH1B[i + 1, 3] <- XY[4]
    HH1B[i + 1, 4] <- HM2B[i + 1, 3]
    HH1B[i + 1, 5] <- H3[i + 1, 3]
    HH1B[i + 1, 6] <- H3[i + 1, 5]
    HH1B[i + 1, 7] <- H4[i + 1, 3]

  }

  ## here we have the estimated H's with the libraries.
  ###  ***** HERE WE NEED TO MAKE SOME GRAPHS FOR HM1B AND HH1B ***********
  ##### * * * * * Section 5.2, estimating Hurst parameter H with the help of
  #####  * * *  * several R libraries    *  * *
  ##### * * * * * Second we estimated H's directly for the first model
  #####* * *  *   i.e.    Y_t = B_t^H    *  *   *   *   *

  ## filtros para estimar sigma
  K <- 6
  ak <- mat.or.vec(1, K + 1)
  for (i in 0:K) {
    ak[i + 1] <- (((-1) ^ (1 - i)) * factorial(K)) /
    ((2 ^ K) * factorial(i) * factorial(K - i))
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
  bk2 <- mat.or.vec(1, 9)
  for (i in 1:4) {
    bk2[2 * i + 1] <- 0
    bk2[2 * i] <- bk[i]
  }
  c <- c(1, -2, 1)
  ck <- c / 4
  ck2 <- mat.or.vec(1, 5)

  for (i in 1:3) {
  #ck2[2*i+1]<-0
    ck2[2 * i - 1] <- ck[i]
  }
 #
  EneN <- length(years)
  #### calculo de V_N,a
  K <- length(bk)   ## descomentar este cuando se use bk
  K1 <- length(bk2) - 1
  N1 <- EneN - K1
  N <- EneN - K
  ### comentar la siguiente linea si se usa bk
  #N<-EneN-K
  #NNHWoman <- ages
  HMan <- ages
  VnaWoman <- ages - ages
  VnaMan <- ages - ages
  VnaWoman_a2 <- VnaWoman
  VnaMan_a2 <- VnaMan
  for (h in ages) {
    mortality_rate_data <- 
      mortality_rate_data2[mortality_rate_data2$Year
                                                %in% c(1950:2004) &
                                                  mortality_rate_data2$Age ==
                                                  ages[h + 1], ]
    sumWoman <- 0
    sumMan <- 0
    for (i in 0:N) { ## N=EneN-K=75-4
      sum5 <- 0
      sum6 <- 0
      for (j in 1:K) {
        ik <- i + 1950 + j - 1
        sum5 <- sum5 + (bk[j] * mortality_rate_data$FemalePost[
          mortality_rate_data$Year == ik])
        sum6 <- sum6 + (bk[j] * mortality_rate_data$MalePost[
          mortality_rate_data$Year == ik])
      }
      sumWoman <- sumWoman + sum5 ^ 2
      sumMan <- sumMan + sum6 ^ 2
    } #fin for i
    VnaWoman[h + 1] <- sumWoman / N
    VnaMan[h + 1] <- sumMan / N
    #### calculo de V_N,a^2, aqui continua el for de h
    sumWoman_a2 <- 0
    sumMan_a2 <- 0
    for (i in 1:N1) {
      sum2 <- 0
      sum3 <- 0
      for (j in 1:K1) {
        ik <- i + 1950 + j - 1
        sum2 <- sum2 +
          (bk2[j] *
             mortality_rate_data$FemalePost[mortality_rate_data$Year == ik])
        sum3 <- sum3 +
          (bk2[j] *
             mortality_rate_data$MalePost[mortality_rate_data$Year == ik])
      }
      sumWoman_a2 <- sumWoman_a2 + sum2 ^ 2
      sumMan_a2 <- sumMan_a2 + sum3 ^ 2
    }  ## fin for i
    VnaWoman_a2[h + 1] <- sumWoman_a2 / N
    VnaMan_a2[h + 1] <- sumMan_a2 / N
  } ## fin for h
  HWoman <- 0.5 * log(VnaWoman_a2[1:91] / VnaWoman[1:91], 2)
  HMan <- 0.5 * log(VnaMan_a2[1:91] / VnaMan[1:91], 2)
  H_woman <- cbind(HH1$Age, HM1$Hal, HM2$HWoman, H4$Rovers_Mu)
  H_man <- cbind(HH1$Age, HH1$Hal, HM2$HMan, H4$Rovers_Ho)
  colnames(H_woman) <- colnames(H_man) <- c("Age",
                                              "hurstexp_lib",
                                              "FDWhittle_lib", "Rovers_Lib")
  mortality_rate_data2$Fem1 <- mortality_rate_data2$FemalePost
  mortality_rate_data2$Mal1 <- mortality_rate_data2$MalePost
  for (h in ages) { 
    mortality_rate_data2$FemalePost[mortality_rate_data2$Age == ages[h + 1]] <-
      mortality_rate_data2$FemalePost[mortality_rate_data2$Age == ages[h + 1]]
    mortality_rate_data2$MalePost[mortality_rate_data2$Age == ages[h + 1]] <-
    mortality_rate_data2$FemalePost[mortality_rate_data2$Age == ages[h + 1]]
  }
  H_estimation <- 
    list(H_woman,
         H_man,
         alpha_woman,
         alpha_man,
         mortality_rate_data2)
  return(H_estimation)
}
