library("car")
library("cluster")
data<-carData::Freedman
data<-na.omit(data)
data<-scale(data)
#Agnes
hc<-agnes(data,method="average")
#Agglomerative coefficient
print(hc$ac)
#Plot dendogram
pltree(hc,cex=0.6,hang=-1,main="Dendogram")
