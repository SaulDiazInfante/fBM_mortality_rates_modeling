#setwd('/home/javier/Dropbox/research/PAPERS/Research Arelly')

#setwd('C:/Users/M_Guill?n_i_Estany/Dropbox/Research Arelly/data')



miseln <-function(para){
           sum(plnorm(gv, meanlog=para[1] , sdlog=para[2]) - fn)^2
           return()
         }
         
MISE_para_ln<-mat.or.vec(500,3)
colnames(MISE_para_ln)<-c('logmean', 'logsd', 'mise')

for(i in 2:501){
      fn<-emp[,i]
      n<-freqage[,i]
      op<-optim(par=c(MLE_para[i-1,3], MLE_para[i-1,4]), fn=miseln)
       MISE_para_ln[i-1,1]<-op$par[[1]]
       MISE_para_ln[i-1,2]<-op$par[[2]]
       MISE_para_ln[i-1,3]<-op$value
        }
MISE_para_ln
write.table(MISE_para_ln, 'MISE_para_ln.csv', sep=',', row.names=F)


#for(i in ){
#    lwrates[]
#}    


i=1
j=1
       write.table(length(diag(lmrates[-1:-(na-ny+i),])), 'dlwrates.csv', sep=',', row.names=F)
          

    
diag(lmrates[-1:-(na-ny+i),]) - lmrates[i,1] 
