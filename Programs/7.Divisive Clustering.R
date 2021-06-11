library("car")
library("cluster")
library("dendextend")
data<-carData::Freedman
data<-na.omit(data)
data<-scale(data)
#Diana
hc<-diana(data)
#Divisive coefficient
print(hc$dc)
#Plot dendogram
pltree(hc,cex=0.6,hang=-1,main="Dendogram")