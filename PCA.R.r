phy<-c(45.83286, 66.24545, 54.84116, 52.73309, 55.22804, 36.71771, 70.60068, 46.58481,
       37.61019, 54.44590, 42.65069, 54.37827, 55.48877, 54.75712, 58.49035, 36.50214,
       57.46308, 50.88379, 66.00721, 40.21083, 62.80092, 52.67780, 46.37766, 62.18870,
       67.25034, 46.94243, 40.73630, 44.16992, 63.06225, 21.46127,53.06325, 46.75746,
      42.62306, 37.17180, 35.69836, 45.89923, 58.04152, 54.64092, 41.41039, 63.12466,
       41.00973, 44.99261, 44.93524, 37.29267, 58.34537, 53.43469, 45.36934, 52.06818,
       49.16137, 51.86147, 56.58516, 41.69534, 62.34281, 39.80478, 56.03820, 44.01542,
       45.22847, 61.32475, 45.68322, 48.14822, 57.17597, 46.44202, 35.24607, 55.56036,
       41.04521, 51.67046, 52.93516, 35.84404, 47.15922, 60.08049, 55.10556, 50.66494,
       63.17677, 49.47325, 44.17665, 35.61826, 47.60780, 62.02337, 47.17575, 59.63255,
       46.66867, 41.92247, 58.06207, 63.70322, 53.54859, 54.28997, 61.45739, 48.42434,
       47.02817, 56.68283, 47.39154, 53.48607, 57.12145, 62.14516, 52.54617, 56.72894,
       45.37159, 29.57351, 46.26462, 53.31890)
stat<-c(49.73412, 64.87357,50.45485, 52.98077, 57.51052, 38.12940, 71.31107, 43.07550,
        37.06575, 52.86622, 48.93422, 56.17238, 54.14389, 54.90254, 54.28524, 37.76160,
        56.18982, 50.22294, 63.74446, 40.01258, 65.38431, 53.11069, 44.08331, 59.14436,
        68.91049, 46.75340, 44.26910, 40.07929, 61.06836, 27.51049, 56.62534, 44.27409,
        40.81727, 38.58767, 30.63126, 43.99144, 59.51573, 53.74517, 36.76394, 62.46416,
        40.00774, 46.69961, 46.71632, 37.14477, 55.47421, 55.79567, 47.71729, 46.96069,
        50.08758, 56.27546, 55.46879, 41.89212, 62.01932, 38.39744, 57.49428, 44.95353,
        45.33766, 61.32643, 47.66341, 47.13410, 57.60514, 41.33925, 35.90807, 49.76122,
        45.28572, 50.16363, 49.51824, 33.17955, 44.82814, 61.18417, 54.57818, 46.00166,
        60.00211, 51.27673, 51.33226, 37.75184, 49.91266, 64.34087, 49.49582, 54.04946,
        47.50897, 43.92537, 59.25163, 63.36367, 54.65436, 52.04134, 62.29298, 49.98111,
        48.76383, 54.01947, 49.38044, 51.11642, 52.61662, 59.79395, 47.44484, 56.57045,
        43.39910, 35.60014, 41.10685, 48.86226)

cbind(phy,stat)-> my.classes

plot(my.classes,cex=0.9,col="blue", main = 'Phusics vs Stats marks')

options(digits = 3)

# Scale the data

standardize <- function(x) {(x - mean(x))}
my.scaled.classes <- apply(my.classes,2,function(x) (x-mean(x)))
plot(my.scaled.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores",sub="Mean Scaled",xlim=c(-30,30))

# Find Eigen values of covariance matrix

# cov matrix
my.cov <- cov(my.scaled.classes)
my.cov
# eigen values and eigen vectors of covariance matrix
my.eigen <- eigen(my.cov)
my.eigen
rownames(my.eigen$vectors) <- c("Physics","Stats")
colnames(my.eigen$vectors) <- c("PC1","PC")



# Note that the sum of the eigen values equals 
# the total variance of the data

sum(my.eigen$values)
var(my.scaled.classes[,1]) + var(my.scaled.classes[,2])

# The Eigen vectors are the principal components. 
# We see to what extent each variable contributes

loadings <- my.eigen$vectors

# Let's plot them 
plot(my.scaled.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores",sub="Mean Scaled",xlim=c(-30,30))
pc1.slope <- my.eigen$vectors[1,1]/my.eigen$vectors[2,1]
pc2.slope <- my.eigen$vectors[1,2]/my.eigen$vectors[2,2]

abline(0,pc1.slope,col="red")
abline(0,pc2.slope,col="green")
text(12,10,"(-0.710,-0.695)",cex=0.9,dcol="red")
text(-12,10,"(0.695,-0.719)",cex=0.9,dcol="green")
 

# Multiply the scaled data by the eigen vectors (principal components)

scores <- my.scaled.classes %*% loadings
sd <- sqrt(my.eigen$values)
rownames(loadings) = colnames(my.classes)

# See how much variation each eigenvector accounts for

pc1.var <- 100*round(my.eigen$values[1]/sum(my.eigen$values),digits=2)
pc2.var <- 100*round(my.eigen$values[2]/sum(my.eigen$values),digits=2)
xlab=paste("PC1 - ",pc1.var," % of variation",sep="")
ylab=paste("PC2 - ",pc2.var," % of variation",sep="")
plot(scores,ylim=c(-10,10),main="Data in terms of EigenVectors / PCs",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

scores.min <- min(scores[,1:2])
scores.max <- max(scores[,1:2])

plot(scores[,1]/sd[1],scores[,2]/sd[2],main="My First BiPlot",xlab=xlab,ylab=ylab,type="n")
rownames(scores)=seq(1:nrow(scores))
abline(0,0,col="red")
abline(0,90,col="green")

# This is to make the size of the lines more apparent
factor <- 5

# First plot the variables as vectors
arrows(0,0,loadings[,1]*sd[1]/factor,loadings[,2]*sd[2]/factor,length=0.1, lwd=2,angle=20, col="red")
text(loadings[,1]*sd[1]/factor*1.2,loadings[,2]*sd[2]/factor*1.2,rownames(loadings), col="red", cex=1.2)

# Second plot the scores as points
text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),col="blue", cex=0.7)

somelabs <- paste(round(my.classes[,1],digits=1),round(my.classes[,2],digits=1),sep=" , ")
#identify(scores[,1]/sd[1],scores[,2]/sd[2],labels=somelabs,cex=0.8)


#A loadings plot is a plot of two columns of the Eigenvectors table. 
#The lines show the loading plot.
    
# score plots indicate the projection of the data onto the span of the 
#principal components.

# Teir project values on each PC show how much weight they have on 
#that PC

# Another nice thing about loading plots: the angles between the 
# vectors tell us how characteristics correlate with one another. 
# When two vectors are close, forming a small angle, the two variables
# they represent are positively correlated.
# If they meet each other at 90°, they are not likely to be 
# correlated.
# When they diverge and form a large angle (close to 180°), they are negative correlated.



mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)
biplot(mtcars.pca,cex=0.75)
