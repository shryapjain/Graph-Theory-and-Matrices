
##***simple random walk***##

#### function to simulate the simple random walk of n steps#####
rw <- function(n){
  x=numeric(n)
  step=c(1,-1)
  for (i in 2:n)
    x[i] <- x[i - 1] + sample(step, 1)
  return(x) 
}

##simulating 100 different walks#####
v=replicate(100, rw(100))
v

####ploting the 100 different simple random walks##
plot(v[,1],type='l',ylim = c(min(v)-1,max(v)+1), xlab='number of steps', ylab='position', main='simple random walk', col='blue')
for(i in 2:100){
lines(v[,i], col=i)}


#*********************************************************************#

##random walk in 1-D##

steps <- 100
no._of_walks <- 100
start.pt<- 0

###function to generate the walk####

rw1 <- function() {
  # Add a standard normal at each step
  start.pt + c(0, cumsum(rnorm(steps)))
}


# Matrix of random walks
val <- replicate(no._of_walks, rw1())
val
end=val[100,100]
end #the end position

#plotting the random walks
plot(val[,1],type='l',ylim = c(min(val)-1,max(val)+1), col='blue' ,xlab="step",ylab="distance", main='random walk')
for(i in 2:100){
  lines(val[,i], col=i)}

#plotting the curve
for (sign in c(-1, 1)) {
  curve(start.pt + sign * 1.96 * sqrt(x), from=0, to=steps,
        n=2*T, col="darkred", lty=2, lwd=2, add=TRUE)
}

#*********************************************************************#

##2-D random walk####
#attaching necessary libraries

library(ggplot2)
library(gganimate)

#####genreating 2-D random walk#####
random_walk <- function(n.org, steps, left.p = .5, up.p = .5)
  
{
  
  #require(ggplot2)
  
  whereto <- matrix(ncol = 2)
  
  for(x in 1:n.org){
    walker <- matrix(c(0,0), nrow = steps+1, ncol = 2, byrow = T)
    
    for(i in 1:steps){
      # deciding on left or right movement = 1/0
      horizontal <- rbinom(1, 1, left.p) #using random binomial
      
      # genrating the distance moved using 
      h.dist <- abs(rnorm(1, 0, 1)) 
      
      # adding Horizontal Movement
      if(horizontal == 0){
        walker[i+1,1] <- walker[i,1] + h.dist
      }
      if(horizontal == 1){
        walker[i+1,1] <- walker[i,1] - h.dist
      }
      
      #deciding on left or right movement = 1/0
      vertical <- rbinom(1, 1, up.p)
      
      #distance to be moved is decided by random value from normal distribution
      v.dist <- abs(rnorm(1, 0, 1))
      
      # Adding Vertical Movement
      if(vertical == 1){
        walker[i+1,2] <- walker[i,2] + v.dist
      }
      if(vertical == 0){
        walker[i+1,2] <- walker[i,2] - v.dist
      }
    }
    
    whereto <- rbind(whereto, walker)
  }
  
  id <- rep(1:n.org, each = 1001)
  colnames(whereto) <- c("x" , "y")
  whereto <- as.data.frame(whereto)
  
  whereto <- cbind(whereto[2:nrow(whereto),], org = factor(id),step<-c(0:1000) )

  return(whereto)
}

##generating random walk
rw.test <- random_walk(1, 1000, .5, .5)

rw.test

##start and end points
start=c(rw.test$x[1],rw.test$y[1])
start
end=c(rw.test$x[1001],rw.test$y[1001])
end

####ploting animated graph using ggplot and gganimate
p <- ggplot(rw.test, aes(x = x,y =y, colour = org))
p <- p + geom_path()+ theme(legend.position = "top")
p<-p+transition_reveal(rw.test$step)+geom_point(size=4)
print(p)

##generating 2nd random walk
rw.test <- random_walk(2, 1000, .5, .5)

#start and end points
start1=c(rw.test$x[1002],rw.test$y[1002])
start1
end1=c(rw.test$x[2002],rw.test$y[2002])
end1

##plotting the file
p <- ggplot(rw.test, aes(x = x,y =y, colour = org))
p <- p + geom_path()+ theme(legend.position = "top")
p<-p+transition_reveal(rw.test$step)+geom_point(size=4)
print(p)

##to covert it into tex file
knitr::knit('random walk.R','randomwalkese.tex')
 