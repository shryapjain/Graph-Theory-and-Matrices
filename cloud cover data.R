# model cloud cover data

## predicting or modeling whether it will rain or not

set.seed(12)
P <- matrix(c(0,0,1,.5,.5,0,.5,.5,0),nrow=3)
P

# Add rows:
P%*%rep(1,3)

# We want to simulate n values of Markov chain having transition
# matrix P, starting at x1.

# function : MC.sim
# input: n,P,x1
# Output: vector of length n

set.seed(12453)

# Markoc chain simulation
MC.sim <- function(n,P,x1){
  sim<-as.numeric(n)
  m <- ncol(P)
  if(missing(x1)){
    sim[1]<-sample(1:m,1) # random start
  } else{
    sim[1]<-x1
  }
  for (i in 2:n){
    newstate <- sample(1:m,1,prob = P[sim[i-1],])
    sim[i]<-newstate  
  }
  sim
}


MC.sim(5,P,1)


# Analysing cloud cover data
# 1 -> sunny
# 2-> cloudy

# simulating data
P.mat <- matrix(c(0.7,0.3,0.4,0.6),ncol=2,byrow=TRUE)
x <- MC.sim(1000, P.mat)
x
 


# Fit a 2 state MC to vector x
MC2 <- function(x){
  n<-length(x)
  N1 = sum(x[-n]==1)  # total number of sunny days
  N2 = sum(x[-n]==2)  # total number of cloudy days
  N11 = sum(x[-n]==1 & x[-1]==1) 
  # this basically gives number of times the weather of today was same 
  # as yesterday and both days were sunny
  N12 = sum(x[-n]==1 & x[-1]==2)
  N21 = sum(x[-n]==2 & x[-1]==1)
  N22 = sum(x[-n]==2 & x[-1]==2)
  P<- matrix(c(N11/N1,N21/N2,N12/N1,N22/N2),nrow = 2)
  return(P)
}

P<-MC2(x)
P


# A function to check our predictions

MC2.chk <- function(x) {
   P <- MC2(x)       # probability matrix of x
   x.sim <- MC.sim(length(x), P)  # simulate a MC with transition matrix P
   x.runs <- rle(x)
  # lengths	an integer vector containing the length of each run.
  # values a vector of the same length as lengths with the corresponding values.
   x.len <- length(x.runs[[1]])
  # lengths for which we had sunny days 
   x.1 <- x.runs[[1]][seq(1,x.len,2)]   
  # lengths for wich we had cloudy days 
   x.2 <- x.runs[[1]][seq(2,x.len,2)]
   
   # similarly we do for our simulation
   x.sim.runs <- rle(x.sim)
   x.sim.len <- length(x.sim.runs[[1]])
   x.sim.1 <- x.sim.runs[[1]][seq(1,x.sim.len,2)]  
   x.sim.2 <- x.sim.runs[[1]][seq(2,x.sim.len,2)]

   # plotting the results
   #par(mfrow=c(1,2))
   qqplot(x.1, x.sim.1, xlab="observed state 1 runlengths",
          ylab="simulated state 1 runlengths")
   abline(0,1)
   qqplot(x.2, x.sim.2, xlab="observed state 2 runlengths",
            ylab="simulated state 2 runlengths")
   abline(0,1)
}

MC2.chk(x)
