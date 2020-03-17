
library(yuima)

mod<-setModel(drift="-theta*x",hurst=NA,diffusion="sigma")
samp<-setSampling(T=100,n=10000)

#### Simulation
fOU1<-setYuima(model=mod,sampling=samp)
fOU1a<-simulate(fOU1,true.param=list(theta=1,sigma=2),hurst=0.7)
plot(fOU1a)


for(i in 2:91)
  {
  #### Data
    original.data<- datos2$FemalePost[datos2$Age==i]  # fOU1a@data@original.data # Here is your data 
    fOU2<-setYuima(data=setData(original.data),model=mod,sampling=samp)
    print(i)
    print(mmfrac(fOU2)) 
 }

Yt<-cbind(datos2$FemalePost[datos2$Age==2],datos2$FemalePost[datos2$Age==3],datos2$FemalePost[datos2$Age==4],datos2$FemalePost[datos2$Age==5])
