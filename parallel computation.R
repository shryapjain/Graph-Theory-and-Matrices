# load parallel package
library(parallel)

# define function to test whether an number is prime
is_prime <- function(num)
{
  # if input equals 2 or 3, then we know it's prime
  if(num == 2 | num == 3) 
    return(TRUE)
  # if input equals 1, then we know it's not prime
  if(num == 1) 
    return(FALSE)
   # else if num is greater than 2
  # and divisible by 2, then it is not prime
  if(num %% 2 == 0) 
    return(FALSE)
  
  # else use algorithm to figure out
  # what factors, if any, input has
  
  # get square root of num, rounded down
  root <- ceiling(sqrt(num))
  
  # try to divide each odd number up to root
  # into num; if any leave a remainder of zero,
  # then we know num is not prime
  for(elt in seq(5,root,2))
  { 
    if (num %% elt == 0)
      return(FALSE)
  }
  # otherwise, num has no divisors except 1 and itself
  # thus, num must be prime
  return(TRUE)
  
}
# do a couple checks of function
is_prime(17) # 17 is prime

is_prime(323) # 323 = 17 * 19; not prime

# get random sample of 5000 integers from integers between 1 and 
# 1 million
# set seed so the random sample will be the same every time
set.seed(5)
size <-5000
sample_numbers <- sample(1000000, size)

#Checking whether each number is prime without using parallel computing
system.time(
  result <- sapply(sample_numbers, is_prime)
)
result[1:100]
# create cluster object
nc<-detectCores()
nc
cl <- makeCluster(nc)
clusterExport(cl,ls(envir = .GlobalEnv),envir = .GlobalEnv)

# test each number in sample_numbers is prime using parallel computing
system.time(
  result.parallel <- parSapply(cl , sample_numbers , is_prime)
)
result.parallel[1:100]

# close
stopCluster(cl)

# doing the same thing using loops, without parallel computing
result.loop<-numeric(size)
system.time(
  for (i in 1:size){
    result.loop[i]<-is_prime(sample_numbers[i])
  }
)
result.loop[1:100]

# creating a cluster
#install.packages('foreach')
#install.packages('doParallel')
library(foreach)
library(doParallel)
nc <- detectCores()
cl <- makeCluster(nc)
clusterExport(cl,ls(envir = .GlobalEnv),envir = .GlobalEnv)
registerDoParallel(cl,cores=nc)


# doing the same thing using loops, with parallel computing
system.time(
  result.loop.parallel <- foreach(i=1:size,.combine='c')%dopar%{
    ans <- is_prime(sample_numbers[i])
  }
)
result.loop.parallel[1:100]



#  matrix multiplication

nr <- 4
A <- matrix(round(rnorm(nr^2),1),nr=nr)
B <- matrix(round(rnorm(nr^2),1),nr=nr)
A
B
A %*% B

## Doing it manually step by step
A1<-A[1,,drop=FALSE] %*% B
A2<-A[2,,drop=FALSE] %*% B
A3<-A[3,,drop=FALSE] %*% B
A4<-A[4,,drop=FALSE] %*% B
rbind(A1,A2,A3,A4)

A %*% B==rbind( A1,A2,A3,A4)


# creating a function for matrix multiplication using parallel computing
matprod.par <- function(cl, A, B){
  if (ncol(A) != nrow(B)) stop("Matrices do not conforme")
  idx <- splitIndices(nrow(A), length(cl))
  Alist <- lapply(idx, function(ii) A[ii,,drop=FALSE])
  ans <- clusterApply(cl, Alist, get("%*%"), B)
  do.call(rbind, ans)
}

matprod.par(cl,A,B)





# More examples
#Creating a random 100*100 matrix
A = matrix(rnorm(10000,1000,100),nrow=100)

# finding row sums without using parallel computation
system.time(
  a<-sapply(as.data.frame(A),FUN=sum)
)

# dividing each element of matirx by row mean each row without using parallel computation
system.time(
  foreach(i=1:nrow(A), .combine=rbind) %do%
    (A[i,] /mean(A[i,]))
)


# finding row sums using parallel computation
system.time(
  a.loop<-parSapply(cl,as.data.frame(A),FUN=sum)
)

# dividing each element of matrix by row mean each row without using parallel computation
system.time(
  foreach(i=1:nrow(A), .combine=rbind) %dopar%
    (A[i,] / mean(A[i,]))
)




stopImplicitCluster()
stopCluster(cl)

