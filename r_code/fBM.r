library("fractal")
library("fractaldim")
library("pracma")
library("yuima")
library("somebm")
library("nlme")
library("expm")

drates<-read.table("Deaths_Rates_Italy.txt", dec = ".", header = TRUE, na.strings = ".")
head(drates)
table(drates$Year)
table(drates$Age)
dim(drates)

drates <- drates[drates$Age != "110+", ]
#### Rates matrix       x=age y=years
mrates <- wrates <- arates <- mat.or.vec(110, 138)

rownames(mrates) <- rownames(wrates) <- rownames(arates) <- 0:109
colnames(mrates) <- colnames(wrates) <- colnames(arates) <- 1872:2009

for(y in 1:138){
  for(x in 1:110){
    wrates[x,y] <- drates[(y-1)*110+x,3]
    mrates[x,y] <- drates[(y-1)*110+x,4]
    arates[x,y] <- drates[(y-1)*110+x,5]      
  }
}

lwrates <- log(wrates)
lmrates <- log(mrates)
larates <- log(arates)

ages <- as.numeric(rownames(wrates))
years <- as.numeric(colnames(wrates))
fa <- ages[1]; 
ca <- length(ages); 
la <- ages[ca]
fy <- years[1]; 
cy <- length(years); 
ly <- years[cy]

##### * * * * * Graph of the raw data  * * *  *  * * 
##### * * * * * * * *  *  *  *  *  *   *  *   *   *
color <- rainbow(cy)

#### Giacometti data: ages (x = 0,...,91) and N years (t = 1930,...,2004)
drates$Female <- drates$Female
drates$Male <- drates$Male

ages <- 0:91
years <- 1930:2004

fa <- ages[1]
ca <- length(ages)
la <- ages[ca]

fy <- years[1]
cy <- length(years)
ly <- years[cy]

datos <- drates[drates$Age%in%c(0:90) & drates$Year%in%c(1930:2004), ]

##### * * * * *  Section 3.1. estimating \alpha_s  * * *  *  * * 
##### * * * * * * * *  *  *  *  *  *   *  *   *   * 
alphaMuj <- mat.or.vec(92,4)
alphaHom <- mat.or.vec(92,4)
alphaMujPost <- mat.or.vec(92,4)
alphaHomPost <- mat.or.vec(92,4)

HM <- data.frame(0,0,0)
HMPost <- data.frame(0,0,0)
colnames(HMPost) <- c("Age","H_women","H_Men")
#HH2<-data.frame(0,0)
colnames(HM) <- c("Age","H_Women","H_Men")

# to make converge the H.
epsil <- 0.000001

for(A in ages) {
  datos <- drates[drates$Year%in%c(1930:2004) & drates$Age==ages[A+1], ]
  datos$Female <- datos$Female * 100
  datos$Male <- datos$Male
  
  Mal_1 <- Femal_1 <- cbind(datos$Year, datos$Female-datos$Female)
  Mal_2 <- Femal_2 <- cbind(datos$Year, datos$Female-datos$Female)
  
  for(B in years) {
    Femal_1[datos$Year==B+1,2] <- datos$Female[datos$Year==B]
    Mal_1[datos$Year==B+1,2] <- datos$Male[datos$Year==B]
    Femal_2[datos$Year==B+2,2] <- datos$Female[datos$Year==B]
    Mal_2[datos$Year==B+2,2] <- datos$Male[datos$Year==B]
  }
  
  # lm for female
  LM1fem <- lm(datos$Female ~ datos$Year + Femal_1[,2])  
  # lm for male
  LM1Male <- lm(datos$Male ~ datos$Year + Mal_1[,2])  
  
  alphaMuj[A+1, 2:4] <- summary(LM1fem)$coefficients[1:3, 1]
  
  alphaHom[A+1, 2:4] <- summary(LM1Male)$coefficients[1:3, 1]
  
  alphaHom[A+1, 1] <- alphaMuj[A+1, 1] <- HM[A+1, 1] <- ages[A + 1]
  # H estimated for women
  HM[A + 1, 2] <- H1 <- RoverS(LM1fem$residuals)
  # H estimated for men
  HM[A+1, 3] <- H2 <- RoverS(LM1Male$residuals)
  # Here we estimated the Covariance matrix for each age
  Gamm_Wom<-mat.or.vec(cy, cy) 
  Gamm_Men<-mat.or.vec(cy, cy) 
 
 # x1<-rep(datos$Female[1], cy)
  x2 <- 1930:2004
  x3W <- c(0, datos$Female[1:74])
  x4W <- c(0, 0, datos$Female[1:73])
  XW<-cbind(x2, x3W)
 
  #x1<-rep(datos$Female[1], cy)
  x3M <- c(0, datos$Male[1:74])
  x4M <- c(0, 0, datos$Male[1:73])
  XM<-cbind(x2, x3M)
  
  #here we reestimate the parameters, including H
  H_prev_Wom <- RoverS(LM1fem$residuals)
  H_prev_Men <- RoverS(LM1Male$residuals)
  
  ##### ** ** ** ** REPEAT ** ** **
  repeat{
    H1 <- H_prev_Wom
    H2 <- H_prev_Men
    
    Gamm_Wom <- mat.or.vec(cy,cy) 
    Gamm_Men <- mat.or.vec(cy,cy) 
    
    for(B in 1:cy)
    {
      for(Z in 1:cy)
      { 
        Gamm_Wom[B,Z] <- 0.5 * ((years[B]) ^ H1 + years[Z] ^ H1 - (abs(years[B] - years[Z])) ^ H1 ) 
        Gamm_Men[B,Z] <- 0.5 * ((years[B]) ^ H2 + years[Z] ^ H2 - (abs(years[B] - years[Z])) ^ H2 )  
      }
    }
    
    K_Wom <- sqrtm(Gamm_Wom)
    K_Men <- sqrtm(Gamm_Men)
    K_Wom_1 <- solve(K_Wom)	
    K_Men_1 <- solve(K_Men)	
  
    ZpWo <- K_Wom_1 %*% datos$Female
    XpWo <- K_Wom_1 %*% XW
    
    ZpMe <- K_Wom_1 %*% datos$Male
    XpMe <- K_Wom_1 %*% XM
    
    datZX_W <- as.data.frame(cbind(ZpWo,XpWo))
    colnames(datZX_W) <- c("y", "x1", "x2")
    
    datZX_M <- as.data.frame(cbind(ZpMe,XpMe))
    colnames(datZX_M)<-c("y", "x1","x2")
    
    RegPostWom<-lm(y ~ x1 + x2 , data=datZX_W)
    RegPostMen<-lm(y ~ x1 + x2 , data=datZX_M)
    
    
    # H estimated POST for women
    HMPost[A+1, 2] <- RoverS(RegPostWom$residuals) 
    # H estimated POST for men
    HMPost[A+1, 3] <- RoverS(RegPostMen$residuals )
    
    alphaHomPost[A + 1, 1] <- alphaMujPost[A+1, 1] <- HMPost[A + 1, 1] <- ages[A + 1]
    if(abs(HMPost[A + 1, 2] - H1) < epsil & abs(HMPost[A + 1, 3] - H2) < epsil)
     {
      break 
     }
    if(abs(HMPost[A + 1, 2] - H1) > epsil || abs(HMPost[A + 1, 3] - H2) > epsil)
      {
      H_prev_Wom <- HMPost[A + 1, 2]
      H_prev_Men <- HMPost[A + 1, 3]
      }
  } ## end for the repeat
   
  
  ### Number of simulations
  NS <- 2
  DW <- mat.or.vec(NS, 75)
  DM <- mat.or.vec(NS, 75)
  #D<-as.data.frame(D)
  
  for(i in 1:NS){
    
    #d<-ts(fbm(hurst=0.7, n=75),start=c(1930, 1),end=c(2004,1),frequency=1)
    DW[i,]<-ts(fbm(hurst=HMPost$H_women, n=75), start=c(1930, 1), end=c(2004,1), frequency=1) 
    DM[i,]<-ts(fbm(hurst=HMPost$H_Men, n=75), start=c(1930, 1), end=c(2004,1), frequency=1) 
  }
  
  datZX_W <- as.data.frame(cbind(ZpWo, XpWo, DW[1,]))
  colnames(datZX_W) <- c("y","x1", "x2")
  
  datZX_M <- as.data.frame(cbind(ZpMe, XpMe, DW[2,]))
  colnames(datZX_M)<-c("y", "x1","x2")
  
  RegPostWom <- lm(y ~ x1 + x2 , data=datZX_W)
  RegPostMen <- lm(y ~ x1 + x2 , data=datZX_M)
  
  
  alphaMujPost[A + 1, 2:3] <- summary(RegPostWom)$coefficients[1:2, 1]
  alphaHomPost[A + 1, 2:3] <- summary(RegPostMen)$coefficients[1:2, 1]
  
} ### end for A



alphaMujPost <- as.data.frame(alphaMujPost)
colnames(alphaMujPost) <- colnames(alphaHomPost) <- c("ages", "al1", "al2", "al3")
##### * * * * * * Simulation of mortalities rates for the first model with a given age * * * *  
age <- 65
### Number of simulations
NS <- 1000
DW <- mat.or.vec(NS, 75)
DM <- mat.or.vec(NS, 75)
#D<-as.data.frame(D)

HW_est<-HMPost[age + 1, 2]
HM_est<-HMPost[age + 1, 3]
for(i in 1:NS){
  #d<-ts(fbm(hurst=0.7, n=75),start=c(1930, 1),end=c(2004,1),frequency=1)
  DW[i,] <- ts(fbm(hurst=HMPost$H_women, n=75), start=c(1930, 1), end=c(2004, 1), frequency=1) 
  DM[i,] <- ts(fbm(hurst=HMPost$H_Men, n=75), start=c(1930, 1), end=c(2004, 1), frequency=1) 
}

Dmed <- colMeans(DW)
Res1 <- (DW - Dmed) ^ 2
Var_Point <- colMeans(Res1)
L <- 2004 - 1930
Desv_stan <- sqrt(Var_Point / L)

datos <- drates[drates$Year%in%c(1930:2004) & drates$Age==age, ]

## initial condition Women and men
hw0 <- datos$Female[datos$Age==age][1]
hm0 <- datos$Male[datos$Age==age][1]

htWomen <- mat.or.vec(L + 1,2)
htMen <- mat.or.vec(L + 1,2)
htWomen[1,2] <- hw0
htMen[1,2] <- exp(hm0)
for(i in 1931:2004)
{  
  htWomen[i-1929, 2] <- alphaMujPost$al1[alphaMujPost$age==age]*i + 
                        alphaMujPost$al2[alphaMujPost$age==age] * htWomen[i - 1930, 2]+
                        alphaMujPost$al2[alphaMujPost$age==age] * htWomen[i - 1929, 2]
  #+DM[1,i-1929]
 # htMen[i-1929,2]<-hm0*exp(alphaHom[age+1]*i + DM[1,i-1929]
  htWomen[i-1929, 1] <- i
  htMen[i-1929, 1] <- i
}

x11(); par(las=1)
plot(years,datos$Female,type="l", col="blue")
lines(years,htWomen[,2], type="o", col="red")  

x11(); 
par(las=1)
plot(years, datos2$Female[datos2$Age==age], type="o", col=color[2])  

fit <- auto.arima(datos$Female)
qqnorm(fit$residuals)
fit$arma