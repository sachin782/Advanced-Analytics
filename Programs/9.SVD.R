A<-as.matrix(data.frame(c(4,7,-1,8), c(-5,-2,4,2), c(-1,3,-3,6)))
print(A)
A.svd<- svd(A)
print(A.svd)
ATA <- t(A) %*% A
print(ATA)
ATA.e<- eigen(ATA)
v.mat<- ATA.e$vectors
print(v.mat)
v.mat[,1:2] <- v.mat[,1:2] * -1
print(v.mat)
AAT <- A %*% t(A)
print(AAT)
AAT.e<- eigen(AAT)
u.mat<- AAT.e$vectors
print(u.mat)
u.mat<- u.mat[,1:3]
r <- sqrt(ATA.e$values)
r <- r * diag(length(r))[,1:3]
print(r)
svd.matrix<- u.mat %*% r %*% t(v.mat)
print(svd.matrix)
print(A == round(svd.matrix, 0))
