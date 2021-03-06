setwd('/home/sauldiazinfantevelasco/sauld@cimat.mx/UNISON/Articles/fBM-MortalityRates/gitfBM-MortalityRates')
library("fractal")
library("fractal")
library("pracma")
library("yuima")
library("somebm")
library(ggplot2)

##### * * * * * Reading the data  * * *  *  * * 
drates <- read.table("Deaths_Rates_Italy.txt", dec = ".", header = TRUE, na.strings = ".")
df <- read.table("Deaths_Rates_Italy.txt", dec = ".", header = TRUE, na.strings = ".")
head(drates)

table(drates$Year)
table(drates$Age)
dim(drates)
drates<-drates[drates$Age!="110+" ,]

#### Rates matrix       x=age y=years
mrates <- wrates <- arates <- mat.or.vec(110, 138)
rownames(mrates) <- rownames(wrates) <- rownames(arates) <- 0:109
colnames(mrates) <- colnames(wrates) <- colnames(arates) <- 1872:2009


for(y in 1:138){
   for(x in 1:110){
      wrates[x, y] <- drates[(y-1) * 110 + x, 3]
      mrates[x, y] <- drates[(y-1) * 110 + x, 4]
      arates[x, y] <- drates[(y-1) * 110 + x, 5]      
     }
   }

lwrates <- log(wrates)
lmrates <- log(mrates)
larates <- log(arates)
ages <- as.numeric(rownames(wrates))
years <- as.numeric(colnames(wrates))
fa <- ages[1]; ca <- length(ages); la <- ages[ca]
fy <- years[1]; cy <- length(years); ly <- years[cy]

##### * * * * * Graph of the raw data  * * *  *  * * 
##### * * * * * * * *  *  *  *  *  *   *  *   *   *
color <- rainbow(cy)
# 
# x11(); par(las=1)
p <- ggplot(df, aes(x=Year, y=Male), group=Age) + 
geom_point(aes(group=Age), ) + 
geom_line(aes(group = Age), colour = "#3366FF", alpha = 0.5) +
xlim(2000, 2014)
p
# plot(ages, lwrates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
#   for(i in 2:cy){
#        lines(ages, lwrates[,i], col=color[i])
#       }
#       
# x11(); par(las=1)
# plot(ages, lmrates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
#   for(i in 2:cy){
#        lines(ages, lmrates[,i], col=color[i])
#       }
#       
# x11(); par(las=1)
# plot(ages, larates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
#   for(i in 2:cy){
#        lines(ages, larates[,i], col=color[i])
#       }            
# 

#### Giacometti data: ages (x = 0,...,91) and N years (t = 1930,...,2004)
drates$Female<-drates$Female*100
drates$Male<-drates$Male*100


ages<-0:91
years<-1950:2004

fa<-ages[1]
ca<-length(ages)
la<-ages[ca]

fy<-years[1]
cy<-length(years)
ly<-years[cy]


datos<-drates[drates$Age%in%c(0:90) & drates$Year%in%c(1950:2004) , ]


##### * * * * *  Section 3.1. estimating \alpha_0  * * *  *  * * 
##### * * * * * * * *  *  *  *  *  *   *  *   *   * 
alphaMuj<-0:91
alphaHom<-0:91

### mujeres


for(A in ages) {
  sum1<-0
  sum2<-0
  sum3<-0
  sum4<-0

  datos<-drates[drates$Year%in%c(1950:2004) & drates$Age==ages[A+1], ]
  
  
   for(B in years) {
      sum1<-sum1 +  ((datos$Year[datos$Year==B])-1950)*log(datos$Female[datos$Year==B])        
      sum2<-sum2 + ((datos$Year[datos$Year==B])-1950)
      sum3<- sum3 + ((datos$Year[datos$Year==B])-1950)^2
      sum4<-sum4 +  ((datos$Year[datos$Year==B])-1950)*log(datos$Male[datos$Year==B])        
     }  ## fin for B
   alphaMuj[A+1]<- (sum1-log(datos$Female[datos$Year==1950])*sum2)/ sum3
   alphaHom[A+1]<- (sum4-log(datos$Male[datos$Year==1950])*sum2 )/ sum3
  
} ### fin for A

#####  *  *   *  *  *   *   *  * End estimating \alpha_0





##### * * * * * transforming the data to adjust an fractional noise i.e a Y_t  * * *  *  * * 
##### * * * * * * * *  *  *  *  *  *   *  *   *   * 

datos<-drates[drates$Age%in%c(0:91) & drates$Year%in%c(1950:2004) , ]
datos$FemalePost<-datos$Female
datos$MalePost<-datos$Male

datos2<-data.frame()
#names(datos2)<-names(datos)



for(A in ages) {
  datos1<-datos[datos$Year%in%c(1950:2004) & datos$Age==ages[A+1], ]
  datos1$FemalePost<-log(datos1$Female)-log(datos1$Female[1])-alphaMuj[A+1]*(datos1$Year)
  datos1$MalePost<-log(datos1$Male)-log(datos1$Male[1])-alphaHom[A+1]*(datos1$Year)
  datos2<-rbind(datos2,datos1)
  
  #print(summary(datos2))
 }
# 
# 
# x11(); par(las=1)
# plot(years,datos2$FemalePost[datos2$Age==ages[1]], type='l', col=color[1],  ylim=c(-2,2), xlab='Tiempo', ylab='Log')
# 
# for (i in ages) {
#   lines(years,datos2$FemalePost[datos2$Age==ages[i+1]], col=color[i])  
# }
# 
# 
# x11(); par(las=1)
# plot(years,datos2$MalePost[datos2$Age==ages[1]], type='l', col=color[1],  ylim=c(-2,2), xlab='Tiempo', ylab='Log')
# 
# for (i in ages) {
#   lines(years,datos2$MalePost[datos2$Age==ages[i+1]], col=color[i])  
# }
# 


##### * * * * * Section 3.2, estimating Hurst parameter H with the help of
#####  * * *  * several R libraries    *  * * 
##### * * * * * Second we estimated H's directly for the first model
#####* * *  *   i.e.    Y_t = B_t^H    *  *   *   *   * 


######## Estimacion de H usando diferentes funciones: hurstexp(x), FDDwhite, Rovers
HM1<-data.frame(0,0,0,0,0,0)
HH1<-data.frame(0,0,0,0,0,0)
colnames(HM1)<-  colnames(HH1)<-c("Edad","Hs", "Hrs", "He",  "Hal", "Ht")

HM2<-data.frame(0,0,0)
#HH2<-data.frame(0,0)
colnames(HM2)<-c("Edad","HMuj","HHom")

#HM3<-data.frame(0,0,0)
H3<-data.frame(0,0,0)
colnames(H3)<- c("Edad", "Higuchi_Mu","Higuchi_Ho")


H4<--data.frame(0,0,0)
colnames(H4)<- c("Edad", "Rovers_Mu","Rovers_Ho")


for(i in ages) {
  HM1[i+1,1]<- HH1[i+1,1]<-ages[i+1]
  XX<-hurstexp(datos2$FemalePost[datos2$Age==ages[i+1]], d = 20, display = F)
  XY<-hurstexp(datos2$MalePost[datos2$Age==ages[i+1]], d = 20, display = F)
  HM2[i+1,1]<-ages[i+1]      
  HM2[i+1,2]<-FDWhittle(datos2$FemalePost[datos2$Age==ages[i+1]], method="continuous")
  HM2[i+1,3]<-FDWhittle(datos2$MalePost[datos2$Age==ages[i+1]], method="continuous")
  xx<-datos2$FemalePost[datos2$Age==ages[i+1]]
  xy<-datos2$MalePost[datos2$Age==ages[i+1]]
  
  H3[i+1,1]<-ages[i+1] 
  H3[i+1,2]<-hurstBlock(xx, method="higuchi")[1]  
  H3[i+1,3]<-hurstBlock(xy, method="higuchi")[1]  
  
  
  H4[i+1,1]<-ages[i+1] 
  H4[i+1,2]<-RoverS(datos2$FemalePost[datos2$Age==ages[i+1]])
  H4[i+1,3]<-RoverS(datos2$MalePost[datos2$Age==ages[i+1]])
  
  for (j in 1:5){
    HM1[i+1,j+1]<-XX[j]      
    HH1[i+1,j+1]<-XY[j]      
  }
  
}

HM1
HH1

##### * * * * * Section 3.2,second part of estimating Hurst parameter H with the help of
#####  * * *  * several R libraries    *  * * 
##### * * * * * Here we estimated H's by using equation (5.1) * * *  *  *  *   *   *   * 

M<-1

BH2Fem<-data.frame(0,0,0)
BH2Mal<-data.frame(0,0,0)
names(BH2Fem)<-c("Age","Year","FemalePost")
names(BH2Mal)<-c("Age","Year","MalePost")
for (h in ages) {
  YMale<-datos2[datos2$Age==ages[h+1],c(1,7)]
  YFemale<-datos2[datos2$Age==ages[h+1],c(1,6)]
  sumInt1<-0
  sumInt2<-0
  for (i in years) {
    BH2Fem[M,3]<-YMale$MalePost[YMale$Year==i]+sumInt1
    BH2Fem[M,1]<-h
    BH2Fem[M,2]<-i
    BH2Mal[M,3]<-YFemale$FemalePost[YFemale$Year==i]+sumInt2
    BH2Mal[M,1]<-h
    BH2Mal[M,2]<-i
    sumInt1<-0
    sumInt2<-0
    for (j in years) {
      if(j>1950)
         {
          sumInt1<- sumInt1+(YMale$MalePost[YMale$Year==j]+YMale$MalePost[YMale$Year==j-1])/2 ## aqui se deberia multiplicar por 1 que es = t_n - t_{n-1}
          sumInt2<-sumInt2+(YFemale$FemalePost[YFemale$Year==j]+YFemale$FemalePost[YFemale$Year==j-1])/2
          }
        } ## end for j
    M<-M+1
  }## end for i
} ## end for h

##### estimarion of H using some R packages applied to BH2Fem and BH2Mal
######## we use the libreries; hurstexp(x), FDDwhite, Rovers
HM1B<-data.frame(0,0,0,0,0,0,0)
HH1B<-data.frame(0,0,0,0,0,0,0)
colnames(HM1B)<-  colnames(HH1B)<-c("Edad","Hs_hurstexp", "Hal_hurstexp", "FDDwhitte",  "aggVar", "higuchi" ,"RoverS")

HM2B<-data.frame(0,0,0)
#HH2<-data.frame(0,0)
colnames(HM2B)<-c("Edad","HMuj","HHom")

#HM3<-data.frame(0,0,0)
H3<-data.frame(0,0,0,0,0)
colnames(H3)<- c("Edad", "Higuchi_Mu","Higuchi_Ho")


H4<--data.frame(0,0,0)
colnames(H4)<- c("Edad", "Rovers_Mu","Rovers_Ho")


for(i in ages) {
  HM1B[i+1,1]<- HH1B[i+1,1]<-ages[i+1]
  XX<-hurstexp(BH2Fem$FemalePost[BH2Fem$Age==ages[i+1]], d = 20, display = F)
  XY<-hurstexp(BH2Mal$MalePost[BH2Mal$Age==ages[i+1]], d = 20, display = F)
  HM2B[i+1,1]<-ages[i+1]      
  HM2B[i+1,2]<-FDWhittle(BH2Fem$FemalePost[BH2Fem$Age==ages[i+1]], method="discrete")
  HM2B[i+1,3]<-FDWhittle(BH2Mal$MalePost[BH2Mal$Age==ages[i+1]], method="discrete")
  xx<-BH2Fem$FemalePost[BH2Fem$Age==ages[i+1]]
  xy<-BH2Mal$MalePost[BH2Mal$Age==ages[i+1]]
  
  H3[i+1,1]<-ages[i+1] 
  H3[i+1,2]<-hurstBlock(xx, method="aggVar")[1]  
  H3[i+1,3]<-hurstBlock(xx, method="aggVar")[1]  
  H3[i+1,4]<-hurstBlock(xy, method="higuchi")[1]  
  H3[i+1,5]<-hurstBlock(xy, method="higuchi")[1]  
  
  
  H4[i+1,1]<-ages[i+1] 
  H4[i+1,2]<-RoverS(BH2Fem$FemalePost[BH2Fem$Age==ages[i+1]])
  H4[i+1,3]<-RoverS(BH2Mal$MalePost[BH2Mal$Age==ages[i+1]])
  
  
  ## asignamos a mujeres
    HM1B[i+1,2]<-XX[1] 
    HM1B[i+1,3]<-XX[4]
    HM1B[i+1,4]<-HM2B[i+1,2]
    HM1B[i+1,5]<-H3[i+1,2]
    HM1B[i+1,6]<-H3[i+1,4]
    HM1B[i+1,7]<-H4[i+1,2]
    
## asignamos a hombres    
    HH1B[i+1,2]<-XY[1]  
    HH1B[i+1,3]<-XY[4]  
    HH1B[i+1,4]<- HM2B[i+1,3]
    HH1B[i+1,5]<-H3[i+1,3] 
    HH1B[i+1,6]<-H3[i+1,5]  
    HH1B[i+1,7]<-H4[i+1,3]  
     
}

## here we have the estimated H's with the libraries. 
HM1B
HH1B

###  ***** HERE WE NEED TO MAKE SOME GRAPHS FOR HM1B AND HH1B ***********

# plot(HM1B[,1],HM1B[,2],type="l", col="blue",ylim=c(0,1))
# for(i in 3:7){
#   lines(HM1B[,1],HM1B[,i],col=rainbow(i))
# }


##### * * * * * Section 5.2, estimating Hurst parameter H with the help of
#####  * * *  * several R libraries    *  * * 
##### * * * * * Second we estimated H's directly for the first model
#####* * *  *   i.e.    Y_t = B_t^H    *  *   *   *   * 


## filtros para estimar sigma

K<-6

ak<-mat.or.vec(1,K+1)

for(i in 0:K){
    ak[i+1]<- ( ( (-1)^(1-i) )* factorial(K))/ ((2^K)*factorial(i)*factorial(K-i)) 
}


b<-c(0.48296291314453, -0.8365163037378, 0.22414386804201, 0.12940952255126)

bk<-b/sqrt(2)     

K1<-2*(K+1)

ak2<-mat.or.vec(1,K1+1)

for(i in 0:K+1){
   ak2[2*i+1]<-0
   ak2[2*i]<-ak[i]
   
 }

bk2<-mat.or.vec(1,8+1)

for(i in 1:4){
  bk2[2*i+1]<-0
  bk2[2*i]<-bk[i]
  
}



c<-c(1,-2,1)


ck<-c/4
ck2<-mat.or.vec(1,5)

for(i in 1:3){
  #ck2[2*i+1]<-0
  ck2[2*i-1]<-ck[i]
  
}

####


EneN<-length(years)

  
#### calculo de V_N,a
K<-length(bk)   ## descomentar este cuando se use bk
K1<-length(bk2)-1
N1<-EneN-K1
N<-EneN-K

### comentar la siguiente linea si se usa bk
#N<-EneN-K

NNHMuj<-ages
HHom<-ages  
VnaMuj<-ages-ages
VnaHom<-ages-ages
VnaMuj_a2<-VnaMuj
VnaHom_a2<-VnaHom

for(h in ages) {

  datos<-datos2[datos2$Year%in%c(1950:2004) & datos2$Age==ages[h+1], ]
  
sumMuj<-0
sumHom<-0

for(i in 0:N) ## N=EneN-K=75-4
   {
    sum5<-0
    sum6<-0
    for(j in 1:K)
      {
       ik<-i+1950+j-1
       sum5<-sum5 + (bk[j]*datos$FemalePost[datos$Year==ik])
       sum6<-sum6 + (bk[j]*datos$MalePost[datos$Year==ik])    
       
       } # fin for j
 sumMuj<-sumMuj+sum5^2 
 sumHom<-sumHom+sum6^2 
 } #fin for i
VnaMuj[h+1]<-sumMuj/N
VnaHom[h+1]<-sumHom/N


#### calculo de V_N,a^2, aqui continua el for de h


sumMuj_a2<-0
sumHom_a2<-0


for(i in 1:N1)
{
  sum2<-0
  sum3<-0
  for(j in 1:K1)
  {
    ik<-i+1950+j-1
    sum2<-sum2 + (bk2[j]*datos$FemalePost[datos$Year==ik])
    sum3<-sum3 + (bk2[j]*datos$MalePost[datos$Year==ik])    
    
  } ## fin for j
  sumMuj_a2<-sumMuj_a2+sum2^2 
  sumHom_a2<-sumHom_a2+sum3^2 
  
  
}  ## fin for i

VnaMuj_a2[h+1]<-sumMuj_a2/N
VnaHom_a2[h+1]<-sumHom_a2/N


} ## fin for h




HMuj<-0.5*log(VnaMuj_a2[1:91]/VnaMuj[1:91],2)

HHom<-0.5*log(VnaHom_a2[1:91]/VnaHom[1:91],2)


HMuj

HHom



HM1


HH1


## Aqui guardamos los coeficientes de hurst estimados con 3 librerias diferentes para el fBM. 
### Debe ir en la seccion 4  
HMujeres<-cbind(HH1$Edad,HM1$Hal,HM2$HMuj,H4$Rovers_Mu)
HHombres<-cbind(HH1$Edad,HH1$Hal,HM2$HHom,H4$Rovers_Ho)

colnames(HMujeres)<-colnames(HHombres)<-c("Edad","hurstexp_lib","FDWhittle_lib","Rovers_Lib")

HMujeres
HHombres
datos2$Fem1<-datos2$FemalePost
datos2$Mal1<-datos2$MalePost

for(h in ages){
  H1<-HMujeres[[h+1,2]]
  H2<-HHombres[[h+1,2]]
  datos2$FemalePost[datos2$Age==ages[h+1]]<-datos2$FemalePost[datos2$Age==ages[h+1]]
  datos2$MalePost[datos2$Age==ages[h+1]]<-datos2$FemalePost[datos2$Age==ages[h+1]]
}

#x11(); par(las=1)
#tiff(paste("Hurst-Women",".tif",sep=""), width = 4, height = 4, units = 'in', res = 300)  
png("Hurst-Women.png", width = 5, height = 4, units = 'in', res = 300)

plot(HMujeres[,1],HMujeres[,2], type='l', col="blue",  ylim=c(0,1), 
     main= expression(widehat("H")*" for women"),xlab='Age', ylab='',las=1,cex.axis=0.7,cex.lab=0.7)
mtext(expression(widehat("H")),side=2,las=1,line=2.3)

  lines(HMujeres[,1],HMujeres[,3], col="black",lty=3)  
  lines(HMujeres[,1],HMujeres[,4], col="red",lty=2)  
  legend("bottomright",c("hurstexp","FDWhittle","Rovers"),lty=c(1,3,2),col=c("blue","black","red"),
         cex=0.65,bty = "n")
  abline(h=0.5,lty=3)
  

  dev.off()
  
  #### men
 # tiff(, width = 4, height = 4, units = 'in', res = 300)  
  png("Hurst-Men.png", width = 5, height = 4, units = 'in', res = 300)
  
#x11(); par(las=1)
plot(HHombres[,1],HHombres[,2],type='l', col="blue",  ylim=c(0,1), 
     main= expression(widehat("H")*" for men"),xlab='Age', ylab='',las=1,cex.axis=0.7,cex.lab=0.7)
mtext(expression(widehat("H")),side=2,las=1,line=2.3)

  lines(HHombres[,1],HHombres[,3], col="black",lty=3)  
  lines(HHombres[,1],HHombres[,4], col="red",lty=2)  
  legend(60,.24,c("hurstexp RRS","FDWhittle","Rovers R/S"),lty=c(1,3,2),col=c("blue","black","red"),
        cex=0.65 ,bty = "n")
  abline(h=0.5,lty=3)
  
dev.off()
  
    
  
  
  
####* * * * * * * * Sigma estimation * * * *  #####

H<-length(HMujeres[1,])

sigmaMuj<-mat.or.vec(H,1)
sigmaHom<-mat.or.vec(H,1)

for(h in ages) {
  sum1<-0
  sum2<-0
  for (k in 1:K) {
      for(l in 1:K){
         if(k!=l){
             sum1<-sum1+ak[k]*ak[l]*(abs(k-l))^(2*HMujeres[[h+1,2]])
             sum2<-sum2+ak[k]*ak[l]*(abs(k-l))^(2*HHombres[[h+1,2]])
         } 
        if(k==l){
          sum1<-sum1+0
          sum2<-sum2+0
        } 
      } ## fin del for l
   }  ## fin del for k
  sigmaMuj[h+1]<-abs(2*VnaMuj[h+1]/sum1)^(1/2)
  sigmaHom[h+1]<-abs(2*VnaHom[h+1]/sum2)^(1/2)
  
}## fin del for h

sigmaMuj
sigmaHom




###### #####  *  *   *  *  *   *   *  * begin estimating \alpha_1

alpha1Muj<-0:91
alpha1Hom<-0:91

for(A in ages) {
  sum1<-0
  sum2<-0
  sum3<-0
  sum4<-0
  
  datos<-drates[drates$Year%in%c(1950:2004) & drates$Age==ages[A+1], ]
  
  
  for(B in years) {
    sum1<-sum1 +  log(datos$Female[datos$Year==B])        
    sum2<-sum2 + (datos$Year[datos$Year==B])
    sum3<- sum3 + (datos$Year[datos$Year==B])
    sum4<-sum4 +  log(datos$Male[datos$Year==B])        
  }  ## fin for B
  alpha1Muj[A+1]<- (sum1-log(datos$Female[datos$Year==1950])*cy)/ sum2
  alpha1Hom[A+1]<- (sum4-log(datos$Male[datos$Year==1950])*cy )/ sum2
  
} ### fin for A



#####  *  *   *  *  *   *   *  * End estimating \alpha_1




###### Calculo de lambda_N

mu2N<-mat.or.vec(length(ages),1)
lambdaNMuj<-mat.or.vec(length(ages),1)
lambdaNHom<-mat.or.vec(length(ages),1)

for (h in ages[1:91]) {
  
  datos<-datos2[datos2$Year%in%c(1950:2004) & datos2$Age==ages[h+1], ]

  ####Primero calculo de \hat{mu}_{2,N}
  
  sum7<-0
  sum8<-0
  for (i in years) {
      sum7<-sum7+  (datos$FemalePost[datos$Year==i])^2
      sum8<-sum8+ (datos$MalePost[datos$Year==i])^2
      
    }  # fin de for i
  mu2N[h+1]<-sum7
  H1<-HMujeres[[j+1,2]]
  H2<-HHombres[[j+1,2]]
  lambdaNMuj[h+1]<- ( (2*mu2N[h+1])/ ( ((sigmaMuj[h+1])^2)*gamma(2*H1+1) ) )^(-0.5/H1)
  lambdaNHom[h+1]<- ( (2*mu2N[h+1])/ ( ((sigmaHom[h+1])^2)*gamma(2*H2+1) ) )^(-0.5/H2) 
  
} # fin de for h

### * * * * * * *  estimation of lambda_N * * * * * #####


lambdaNMuj

lambdaNHom



##### * * * * * * * Simulation of mortalities rates for a given age with the fBM * * * *  


### Number of simulations
NS<-10000
DW<-mat.or.vec(NS,55)
DM<-mat.or.vec(NS,55)
#D<-as.data.frame(D)

ages1<-c(0,seq(5,90,by=5))

for( A in ages1) 
{
  age<-ages[[A+1]] 

HW_est<-HMujeres[age+1,2]
HM_est<-HHombres[age+1,2]

for(i in 1:NS){
  
  #d<-ts(fbm(hurst=0.7, n=75),start=c(1930, 1),end=c(2004,1),frequency=1)
  DW[i,]<-ts(fbm(hurst=HW_est , n=55),start=c(1950, 1),end=c(2004,1),frequency=1) 
  DM[i,]<-ts(fbm(hurst=HM_est , n=55),start=c(1950, 1),end=c(2004,1),frequency=1) 
  
}



Dmed<-colMeans(DW)
Res1<-(DW-Dmed)^2
Var_Point<-colMeans(Res1)
L<-2004-1950

Desv_stan<-sqrt(Var_Point/L)


datos<-drates[drates$Year%in%c(1950:2004) & drates$Age==age, ]


## initial condition Women and men
hw0<-datos$Female[datos$Age==age][1]
hm0<-datos2$Male[datos$Age==age][1]

htWomen<-mat.or.vec(L+1,2)
htMen<-mat.or.vec(L+1,2)
htWomen[1,2]<-hw0
htMen[1,2]<-hm0
H1<-HW_est[[1]]
H2<-HM_est[[1]]

for(i in 1931:2004)
 {  
   htWomen[i-1949,2]<-hw0*exp(alphaMuj[age+1]*i + DW[3,i-1949])
   htMen[i-1949,2]<-hm0*exp(alphaHom[age+1]*i + DM[1,i-1949])
   htWomen[i-1949,1]<-i
   htMen[i-1949,1]<-i
  }


# 
# 
# x11(); par(las=1)
# plot(years,datos$Female,type="l", col="blue")
# lines(years,htWomen[,2], type="o", col=color[2])  
# 
# x11(); par(las=1)
# plot(years,datos2$Female[datos2$Age==age], type="o", col=color[2])  



#####  Estimation of the rates mortalities.


##  First we approximate the integral 

ht_hat<-mat.or.vec(NS,cy)

ht_hatM<-mat.or.vec(NS,cy)

for(H in 1:NS)
{
 Yt_hat<-mat.or.vec(cy,1)
 Yt_hatM<-mat.or.vec(cy,1)
 
 Yt_hat[1]<-0
 Yt_hatM[1]<-0
 
  #sigmaMuj[age+1]
 sum1<-0
 sum2<-0
 
 for(i in 1:(cy-1)) ## i makes the function of t
   {
   for(k in 1:i)  ##à k makes the function of u
      {
       sum1<-sum1+ exp(-lambdaNMuj[age+1]*(i-k))*(DW[H,k+1]-DW[H,k])#*.5
       sum2<-sum2+ exp(-lambdaNHom[age+1]*(i-k))*(DM[H,k+1]-DM[H,k])#*.5
       }
   Yt_hat[i+1]<-sum1*sigmaMuj[age+1]/(cy^H1)
   Yt_hatM[i+1]<-sum1*sigmaHom[age+1]/(cy^H2)
   
    }
 ht_hat[H,1]<-datos$Female[1]
 ht_hatM[H,1]<-datos$Male[1]
 
 for(j in 2:cy-1)
  {
   if(j>1)
     {
     ht_hat[H,j]<- datos$Female[1]*exp(alphaMuj[age+1]*(j) + Yt_hat[j])
     ht_hatM[H,j]<- datos$Male[1]*exp(alphaHom[age+1]*(j) + Yt_hatM[j])
     }
  }
} ## end for H   (1/sigmaMuj[age+1])*  (cy/H1)*



##   hacer la media muestral y varianza muestral para ht_hat, primero modificar el codigo de 
#### arriba para guardas las 10000 simulaciones de las tasas ht_hat
### Women
ht_hat_mean<-colMeans(ht_hat)
Res1<-(ht_hat-ht_hat_mean)^2
Var_Point<-colMeans(Res1)

### Men
ht_hat_meanM<-colMeans(ht_hatM)
Res1M<-(ht_hatM-ht_hat_meanM)^2
Var_PointM<-colMeans(Res1M)
L<-2004-1950
Desv_stan<-sqrt(Var_Point/L)
Desv_stanM<-sqrt(Var_PointM/L)

#x11(); par(las=1)

### Women
#hacer el cat para hacer el nombre

#tiff(paste("PlotWomen",age,".tif",sep=""), width = 4, height = 4, units = 'in', res = 300)
png(paste("PlotWomen",age,".png",sep=""), width = 4.5, height = 4, units = 'in', res = 300)

plot(years[1:54],datos$Female[1:54],type="l", col="blue",xlab="Years",ylab ="", las=1,
     lty=1, main=bquote(widehat("h(t)")*" for women at age"~ .(age) ),cex.axis=0.7,cex.lab=0.7)
lines(years[1:54],ht_hat_mean[1:54], type="l", lty=2,col="red")
#lines(years[1:54],ht_hat_mean[1:54]-Desv_stan[1:54], type="l", col="brown")
#lines(years[1:54],ht_hat_mean[1:54]+Desv_stan[1:54], type="l", col="brown")
lines(years[1:54],ht_hat_mean[1:54]-2*Desv_stan[1:54], type="l",lty=3, col="green")
lines(years[1:54],ht_hat_mean[1:54]+2*Desv_stan[1:54], type="l",lty=3, col="green")
legend("bottomleft",c("Historical rates","simulations mean","IC  95.5%"),lty=1:3,
       col=c("blue","red","green"),bty = "n",cex = 0.55)
mtext(expression(widehat("h(t)")),side=2,las=1,line=2.3)
dev.off()

#### men
#tiff(paste("PlotMen",age,".tif",sep=""), width = 4, height = 4, units = 'in', res = 300)
png(paste("PlotMen",age,".png",sep=""), width = 4.5, height = 4, units = 'in', res = 300)
plot(years[1:54],datos$Male[1:54],type="l", col="blue",xlab="Years",ylab ="",las=1,
     lty=1, main=bquote(widehat("h(t)")*" for men at age"~ .(age) ),cex.axis=0.7,cex.lab=0.7)
lines(years[1:54],ht_hat_meanM[1:54], type="l", lty=2,col="red")
#lines(years[1:54],ht_hat_meanM[1:54]-Desv_stanM[1:54], type="l", col="brown")
#lines(years[1:54],ht_hat_meanM[1:54]+Desv_stanM[1:54], type="l", col="brown")
lines(years[1:54],ht_hat_meanM[1:54]-2*Desv_stanM[1:54], type="l",lty=3, col="green")
lines(years[1:54],ht_hat_meanM[1:54]+2*Desv_stanM[1:54], type="l",lty=3, col="green")
legend("bottomleft",c("Historical rates","simulations mean","IC  95.5%"),
       lty=1:3,col=c("blue","red","green"),bty = "n",cex = 0.55)
mtext(expression(widehat("h(t)")),side=2,las=1,line=2.3)

dev.off()


qqnorm(log(Desv_stan[1:54]),  main=bquote("QQ-Plot for Women at age"~ .(age) ),las=1)
qqline(log(Desv_stan[1:54]), col = 2)
qqnorm(log(Desv_stanM[1:54]),main=bquote("QQ-Plot for Men at age"~ .(age) ))
qqline(log(Desv_stanM[1:54]), col = 2 )
## anyadir labels a las graficas
}




