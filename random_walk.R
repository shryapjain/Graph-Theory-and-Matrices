# generate n steps:

n.step<- 100
jump <- 1
x <- 0:n.step

#compute y steps:
dy <- jump*sample(c(-1,1),size=n.step,replace = TRUE)
plot(dy)

#compute cumulative y positions
y<-c(0,cumsum(dy))
y
plot(x,y,type='s')


## 100000 steps
n.step<- 100000
jump <- 1
x <- 0:n.step

#compute y steps:
dy <- jump*sample(c(-1,1),size=n.step,replace = TRUE)

#compute cumulative y positions
y<-c(0,cumsum(dy))
y



###############

plot(x,y,type='s')
abline(h=0,lty=2)
points(n.step,y[1],pch=16,cex=1.5, col = 'red')
points(n.step,y[n.step+1],pch=16,cex=1.5, col = 'red')
lines(c(n.step,n.step),c(0,y[n.step+1]))

### MC simulation -> run these 1000 times

# compute distance from start

r<- y[n.step+1]-y[1]
r

label <- paste('distance=',signif(r,3))
label


###############2nd random walk#####
f=replicate(2,cumsum(jump*sample(c(-1,1),size=n.step,replace = TRUE) ))
f
plot(f[,1],type='l',ylim = c(min(f)-1,max(f)+1), col='blue' ,xlab="step",ylab="distance", main='random walk')
for(i in 1:2){
  lines(f[,i], col=i)}



###########2-D##################
# install.packages('ggplot2')
library(ggplot2)
#library(gganimate)

n.steps<-100
theta <- runif(n.steps,0,2*pi)

jump<-1

dx<-jump*cos(theta)
dy<- jump*sin(theta)


x<-c(0,cumsum(dx))
y<-c(0,cumsum(dy))


def=data.frame(x,y)
def
i=c(0:100)
id <- rep(1, each = 101)
whereto <- cbind(def, org = factor(id),step<-c(0:100) )

p <- ggplot(whereto, aes(x = x,y =y, colour = org))
p <- p + geom_path()+ theme(legend.position = "top")+geom_point(size=4)
print(p)

end=c(whereto$x[101],whereto$y[101])
end

r<-sqrt(whereto$x[n.steps+1]**2+whereto$y[n.steps+1]^2)
r

###############
# MC

n.step<-100
n.walk <-10000
jump<-1
x<-array(0,dim=n.walk)
y<-array(0,dim=n.walk)

for(i in 1:n.walk){
  theta <- runif(n.step,0,2*pi)
  dx<-jump*cos(theta)
  dy<- jump*sin(theta)
  x[i]<-sum(dx)
  y[i]<-sum(dy)
}

r<-sqrt(x^2+y^2)
# histogram of distances
hist(r)
plot(x,y)

hist(x,breaks=20,col='blue',prob=TRUE)

x.norm<-seq(-50,50,by=0.2)
y.norm<-dnorm(x.norm,mean = 0, sd = sqrt(n.step)*2/pi)

lines(x.norm,y.norm, lwd = 4)
      
      