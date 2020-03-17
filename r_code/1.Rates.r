
#setwd('C:/Users/M_Guill?n_i_Estany/Dropbox/Research Arelly/data')

# setwd('/home/javier/Dropbox/research/PAPERS/Research Arelly')


drates<-read.table("Deaths_Rates_Italy.txt", dec = ".", header = TRUE, na.strings = ".")
head(drates)
table(drates$Year)
table(drates$Age)
dim(drates)


#### Rates matrix       x=age y=years
mrates<-wrates<-arates<-mat.or.vec(111,138)
                          
rownames(mrates)<-rownames(wrates)<-rownames(arates)<-0:110
colnames(mrates)<-colnames(wrates)<-colnames(arates)<-1872:2009


for(y in 1:138){
   for(x in 1:111){
      wrates[x,y]<- drates[(y-1)*111+x,3]
      mrates[x,y]<- drates[(y-1)*111+x,4]
      arates[x,y]<- drates[(y-1)*111+x,5]      
     }
   }

lwrates<-log(wrates)
lmrates<-log(mrates)
larates<-log(arates)


#### Giacometti data: ages (x = 0,...,91) and years (t = 1930,...,2004) 
lwrates<-lwrates[1:92, 59:133]
lmrates<-lmrates[1:92, 59:133]
larates<-larates[1:92, 59:133]

ages<-as.numeric(rownames(lwrates))
years<-as.numeric(colnames(lwrates))
fa<-ages[1]; na<-length(ages); la<-ages[na]
fy<-years[1]; ny<-length(years); ly<-years[ny]


#### Graph
color<-rainbow(ny)

x11(); par(las=1)
plot(ages, lwrates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
  for(i in 2:ny){
       lines(ages, lwrates[,i], col=color[i])
      }
      
x11(); par(las=1)
plot(ages, lmrates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
  for(i in 2:ny){
       lines(ages, lmrates[,i], col=color[i])
      }
      
x11(); par(las=1)
plot(ages, larates[,1], type='l', col=color[1],  ylim=c(-10,2), xlab='Ages', ylab='Log')
  for(i in 2:ny){
       lines(ages, larates[,i], col=color[i])
      }
                  











