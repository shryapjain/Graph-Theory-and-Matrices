
library(png)
x=readPNG("C:\\Users\\Shraddha\\Desktop\\R final\\abc.png")                          #Load png file into R 
dim(x)                                          # show the dimensions 
x[1:600,1:800,1]->xred                          # Out of 4 layers we'er taking 1 layer
image(xred)
x[1:600,1:800,2]->xred2                         # taking 2 layers                 
image(xred2)
x[1:600,1:800,3]->xred3                         # taking 3 layers
image(xred3)
x[1:600,1:800,4]->xred4                         # taking 4 layers
image(xred4)

max(xred4)
min(xred4)
#take first level 
image(xred, col = grey((0:12)/12))              # convert the image in grey image
image(xred, col = grey((0:12)/12))->r
image(xred2, col = grey((0:12)/12))
image(xred3, col = grey((0:12)/12))
image(xred4, col = grey((0:12)/12))
# SVD
# Y =U*SIGMA*(V^t)
Y = 0.2126*xred + 0.7152*xred2 + 0.0722*xred3
#Y[1:465,1:465]->Y

image(Y)

# SVD
r.svd<-svd(xred)
r.svd
d<-diag(r.svd$d)        #It is a diagonal matrix
d
View(d)
dim(d)


u<-r.svd$u
dim(u)
v<-r.svd$v
dim(v)
plot(1:length(r.svd$d),r.svd$d)                
# To see the distribution of singular values


#first approximation 
u1<-as.matrix(u[-1,1])                       # taking 1st column
dim(u1)
v1<-as.matrix(v[-1,1])                       # taking 1st column
dim(v1)
d1<-as.matrix(d[1,1])                        # taking 1st entry (1,1)
dim(d1)
l1<-u1%*%d1%*%t(v1)
image(l1)

#5th approximation 
depth<-5
u5<-as.matrix(u[,1:depth])                  
v5<-as.matrix(v[,1:depth])
d5<-as.matrix(d[1:depth,1:depth])
l5<-u5%*%d5%*%t(v5)
image(l5)

#20th approximation

depth<-20
u20<-as.matrix(u[,1:depth])
v20<-as.matrix(v[,1:depth])
d20<-as.matrix(d[1:depth,1:depth])
l20<-u20%*%d20%*%t(v20)
image(l20)

#40th approximation

depth<-40
u40<-as.matrix(u[,1:depth])
v40<-as.matrix(v[,1:depth])
d40<-as.matrix(d[1:depth,1:depth])
l40<-u40%*%d40%*%t(v40)
image(l40)
# let us compare our 40th approximation with original image
image(Y)
# we can say that almost all the  information can driven from 40th approximation
# 40*40*4 data can give us our desired result