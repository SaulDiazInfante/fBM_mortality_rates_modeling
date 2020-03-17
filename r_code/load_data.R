library("fractal")
library("pracma")
library("yuima")
library("somebm")
library("plotrix")
library("ggplot2")

load_data <- function(data_file="Deaths_Rates_Italy.txt") {
  drates <- read.table(data_file, dec = ".", header = TRUE, na.strings = ".")
  head(drates)
  table(drates$Year)
  table(drates$Age)
  dim(drates)

  drates <- drates[drates$Age != "110+", ]
#*Rates matrix       x=age y=years
  mrates <- wrates <- arates <- mat.or.vec(110, 143)
  rownames(mrates) <- rownames(wrates) <- rownames(arates) <- 0:109
  colnames(mrates) <- colnames(wrates) <- colnames(arates) <- 1872:2014

  for (y in 1:138) {
    for (x in 1:110) {
      wrates[x,y] <- drates[(y - 1) * 110 + x, 3]
      mrates[x,y] <- drates[(y - 1) * 110 + x, 4]
      arates[x,y] <- drates[(y - 1) * 110 + x, 5]      
    }
  }

  lwrates <- log(wrates)
  lmrates <- log(mrates)
  larates <- log(arates)

  ages <- as.numeric(rownames(wrates))
  years1 <- as.numeric(colnames(wrates))
  years <- years1[1:133]
  fa <- ages[1]; 
  ca <- length(ages); 
  la <- ages[ca]
  fy <- years[1];
  cy <- length(years); 
  ly <- years[cy]
  color <- rainbow(cy)
# * * * *  Giacometti data: ages (x = 0,...,91) and N years (t = 1930,...,2004)
  drates$Female <- drates$Female * 100
  drates$Male <- drates$Male * 100

  ages <- 0:91
  years <- 1950:2004

  fa <- ages[1]
  ca <- length(ages)
  la <- ages[ca]

  fy <- years[1]
  cy <- length(years)
  cy1 <- length(years1[79:142])

  ly <- years[cy]

  data_mortality_rates <- drates[drates$Age %in% c(0:90) & 
                                  drates$Year %in% c(1950:2004), ]
  return(data_mortality_rates)
}