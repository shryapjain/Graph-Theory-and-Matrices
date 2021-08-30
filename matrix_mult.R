# sequential algorithm

multimatrix<-function(a,b)
{
  if(ncol(a)!=nrow(b))
  {
    print("the matrix multiplication is not possible")
  }else{
    c<-matrix(0,nrow=nrow(a),ncol=ncol(b))
    for(i in 1:nrow(a))
    {
      for(k in 1:ncol(b))
      {
        for(j in 1:ncol(a))
        {
          c[i,k]<-c[i,k]+a[i,j]*b[j,k]
        }
      }
    }
    return(c)
  }
}

a=matrix(1:4,nr=2)
a
b=matrix(1,nr=2,nc=2)
b
c=multimatrix(a,b)
c

a%*%b


##we can have an sequential algorithm in case the matrix is square
#not included seprately

##Block Matrix Multiplication
#divide matrix with block size of (n/q)*(n*q)
# cannon algorithm
block=function(a,b,q)
{
  c=matrix(0,nr=length(a[1,]),nc=length(b[,1]))
  x=round(length(a[1,]/q))
  y=round(length(b[,1]/q))
  for(i in 1:x)
  {
    for(j in 1:y)
    {
      c[i,j]=0
      for(k in 1:y)
      {
        c[i,j]=c[i,j]+a[i,k]*b[k,j]
      }
    }
  }
  return(c)
}

a=matrix(1:16,nr=4)
a
b=matrix(1,nr=4,nc=4)
b

block(a,b,1)

a%*%b

e=matrix(c(2,3,4,5,2,1,4,5,3,2,3,4,4,3,4,5), ncol=4)
e
f=matrix(c(2,3,4,5,3,4,2,3,3,2,4,5,3,7,8,3),nrow=4)
f
block(e,f,2)
e%*%f

e=matrix(abs(runif(36,1,15)), ncol=6)
e
f=matrix(abs(runif(36,1,15)),nrow=6)
f
block(e,f,2)
e%*%f



##matrix multiplication by sampling
# Given two matrices A of size m ??? n and B of size n ??? p ,
# our goal is to produce an approximation to the
# matrix multiplication product A ??? B. We do this by 
# performing c independent trials , where in each trial we
# randomly sample an element of 1, 2, ...., n with an 
# appropriate probability distribution P.We form a matrix
# C of size m ??? c consisting of sampled columns of A and
# a matrix R of size c ??? n consisting of sampled rows of
# B, both scaled appropriately

n<-8
p<-runif(n,0,1)
p<-p/sum(p)
sum(p)
A<-matrix(sample(1:n^2,n^2,replace = T),ncol = n,nrow = n)
B<-t(A)+4
c<-n
C<-matrix(0,nrow = nrow(A),ncol=c)
R<-matrix(0,nrow=c,ncol=ncol(B))
for(t in 1:c){
  sample(1:ncol(A))->i_t
  p[i_t[t]]->p_k
  C_t<-A[,i_t[t]]/sqrt(c*p_k)
  R_t<-B[i_t[t],]/sqrt(c*p_k)
  C[,i_t[t]]<-C_t
  R[i_t[t],]<-R_t
}
R


R%*%C

## strassen algorithm
# this function works for only square matrics of even order
strassen.square<- function(A,B){
  if(nrow(B)!=ncol(A))
    stop("matrices do not conforme")
  a.max<- max(nrow(A),ncol(A))
  b.max<- max(nrow(B),ncol(B))
  
  A.r.split<-floor(nrow(A)/2)
  B.r.split<-floor(nrow(B)/2)
  A.c.split<-floor(ncol(A)/2)
  B.c.split<-floor(ncol(B)/2)
  
  A11 = A [1:A.r.split ,1:A.c.split]
  A12 = A [1:A.r.split ,(A.c.split+1):ncol(A)]
  A21 = A [(A.r.split+1):nrow(A) ,1:A.c.split]
  A22 = A [(A.r.split+1):nrow(A) ,(A.c.split+1):ncol(A)]
  
  B11 = B [1:B.r.split ,1:B.c.split]
  B12 = B [1:B.r.split ,(B.c.split+1):ncol(B)]
  B21 = B [(B.r.split+1):nrow(B) ,1:B.c.split]
  B22 = B [(B.r.split+1):nrow(B) ,(B.c.split+1):ncol(B)]
  #step2
  M1 = ( A11 + A22 ) %*% ( B11 + B22 )
  M2 = ( A21 + A22 ) %*% B11
  M3 = A11 %*% ( B12 - B22 )
  M4 = A22 %*% ( B21 - B11 )
  M5 = ( A11 + A12 ) %*% B22
  M6 = ( A21 - A11 ) %*% ( B11 + B12 )
  M7 = ( A12 - A22 ) %*% ( B21 + B22 )
  
  #Step -3 
  C11 = M1 + M4 - M5 + M7
  C12 = M3 + M5
  C21 = M2 + M4
  C22 = M1 - M2 + M3 + M6
  
  
  #Step -4 
  C = rbind ( cbind ( C11 , C12 ) , cbind ( C21 , C22 ) )
  C
  #A %*% B
  #all ( C == A %*% B )
}
A<-matrix(c(sample(50,35),replace = TRUE),nrow=6)
B<-matrix(c(sample(50,35),replace = TRUE),nrow=6)
strassen.square(A,B)


strassen.square(A,B)==A%*%B


## An algorithm for parallel computing
# load parallel package
library(parallel)
library(foreach)
library(doParallel)
nc <- detectCores()
cl <- makeCluster(nc)
clusterExport(cl,ls(envir = .GlobalEnv),envir = .GlobalEnv)
registerDoParallel(cl,cores=nc)

#  matrix multiplication
nr <- 4
A <- matrix(round(rnorm(nr^2),1),nr=nr)
B <- matrix(round(rnorm(nr^2),1),nr=nr)
A
B

# creating a function for matrix multiplication using parallel computing
matprod.par <- function(cl, A, B){
  if (ncol(A) != nrow(B)) stop("Matrices do not conforme")
  idx <- splitIndices(nrow(A), length(cl))
  Alist <- lapply(idx, function(ii) A[ii,,drop=FALSE])
  ans <- clusterApply(cl, Alist, get("%*%"), B)
  do.call(rbind, ans)
}

matprod.par(cl,A,B)

stopImplicitCluster()
stopCluster(cl)