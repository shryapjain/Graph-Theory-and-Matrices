A = matrix(c(1, 1, 2, 0), 2, 2, byrow=TRUE)
A
x0 = rnorm(2)
x0
#x0=matrix(c(1,1),nrow=2,ncol=1)

thresh = 1e-22


eigen(A)

# simple power method which is based on number of iterations as input
# we can give starting x0, and if not given, it will generate automatically

power_method_simple=function(A,n_rep,b_0 = NULL) {
 #Initialize with a random column of the matrix
  if(is.null(b_0))
      b_0=A[,sample(1:ncol(A),size=1)]

  for (k in 1:n_rep) {

  b_0=A%*%b_0
  b_0_norm=sqrt(t(b_0)%*%b_0)
  b_0=b_0/b_0_norm[1,1]
  print(b_0)
  }

eigenvalue=(t(b_0)%*%A%*%b_0)[1,1]
res_list=list(b_0,eigenvalue)
names(res_list)=c("vector","eigenvalue")
return(res_list)

}



power_method_simple(A,10)

power_method_simple(A,10,x0)

# power method that depends on given threshold. This might generate a problem if the 
# algorithm does not converge even after mamy iterations

# this only returns the eigen value.

powerm = function(A, x0, thresh)
{
  m0 = x0[which.max(abs(x0))]
  x1 = A %*% (x0 / m0)
  m1 = x1[which.max(abs(x1))]
 # cat(m1, '\n')
  if(abs(m1 - m0) < thresh)
  {
    return(m1)
  }
  else
  {
    powerm(A, x1, thresh)
  }
}

ev1 = powerm(A, x0, thresh)
ev1


# to find other eigen value
A
A1 = A - diag(2)*ev1
A1
ev2 = ev1 + powerm(A1, x0, thresh)
ev2

x0 = c(1,1,1)
B= matrix(c(1,2,3,5,3,6,2,1,5), nrow = 3)
eigen(B)
ev1 = powerm(B,x0,thresh)
ev1


x0 = c(1,1,1,1)
B= matrix(c(1,2,3,5,3,6,2,1,5,2,3,1,6,1,2,3), nrow = 4)
eigen(B)
ev1 = powerm(B,x0,thresh)
ev1



# power method without recursion - same working as above, without recursion
powerm_nr = function(A, x0, thresh)
{
  m0 = x0[which.max(abs(x0))]
  x1 = A %*% (x0 / m0)
  m1 = x1[which.max(abs(x1))]
 # cat(m1, '\n')
  while(abs(m1 - m0) > thresh)
  {
    m0 = m1
    x1 = A %*% (x1 / m1)
    m1 = x1[which.max(abs(x1))]
  }
  m1
}
ev1 = powerm_nr(A, x0, thresh)
ev1

# with threshold, and maxiter, a fuction that returns bot dominant eigen value and eigen vector

power.method=function(A,n_rep=100,b_0 = NULL,thresh = 0.0000000001) {
 #Initialize with a random column of the matrix
  if(is.null(b_0))
      b_0=A[,sample(1:ncol(A),size=1)]

  for (k in 1:n_rep) {

  b_1=A%*%b_0
  b_1_norm=sqrt(t(b_1)%*%b_1)
  b_1=b_1/b_1_norm[1,1]
  print(b_1)
  if(max(abs(b_1 - b_0)) < thresh)
      break
  else
      b_0 = b_1
  }

eigenvalue=(t(b_0)%*%A%*%b_0)[1,1]
res_list=list(b_0,eigenvalue)
names(res_list)=c("vector","eigenvalue")
return(res_list)

}


x0=c(1,1)
power.method(A,1000,x0,0.001)

#install.packages('matlib')

# using library 
library('matlib')             
powerMethod(A,x0)


