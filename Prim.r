
C = matrix(0,nrow=7,ncol=7)
C[1,2]<-5 #Manually inputting C matrix for one way
C[1,3]<-6
C[1,4]<-7
C[2,3]<-2
C[2,5]<-6
C[2,6]<-3
C[4,7]<-5
C[4,6]<-9
C[3,4]<-7
C[6,7]<-7
C[5,6]<-2

# Fill adjacency matrix up 
for(j in 1:ncol(C))
{ 
  # This adds the C of edges going the opposite ways
  for(i in 1:nrow(C))
  {
    # Sets the Cs of the edges travelling the opposite way
    C[i,j]<-C[j,i]
  }
}



Prim<-function(C){
# Manual first step of algorithm 
#and then loop to generate spanning tree. 

# Num of points
num_pts = dim(C)[1]

# Makes df that will hold edges 
#of tree (as pairs pt = from and to = next point in tree) 
points_paths = data.frame(pt = rep(0, num_pts - 1), to = rep(-1,num_pts - 1))

# Starting point 
pt0 = 1

# Set first point to starting position  
tree_pts = matrix(0, nrow = num_pts, ncol = num_pts)
non_pts = diag(num_pts)

# Push our first point into tree and take first point out of non tree set
tree_pts[pt0, pt0] = 1
non_pts[pt0, pt0] = 0 

# Nice matrix calculation to keep track of possible additions 
poss_edges = tree_pts %*% C %*% non_pts

# Find the minimum distance edge to add
edge = which(poss_edges == min(poss_edges[poss_edges > 0]), arr.ind = T)

# Add edge to tracker 
points_paths[1, ] = edge 

# Update matrices as needed 
tree_pts[edge[2], edge[2]] = 1
non_pts[edge[2], edge[2]] = 0

# Next we will repeat these calcs in a loop 
for(i in 2:(num_pts-1))
{
  # Get possible edges to add
  poss_edges = tree_pts %*% C %*% non_pts
  
  # Prim Algo step
  min_edges = which(poss_edges == min(poss_edges[poss_edges > 0]), arr.ind = T)
  
  # Only grab 1 edge of possible min edges
  edge = min_edges[1,]
  
  # Add to edge tracker
  points_paths[i,] = edge
  
  # Correct our matrices that track our "sets"
  tree_pts[edge[2], edge[2]] = 1
  non_pts[edge[2], edge[2]] = 0
  
}
points_paths
}

A<- Prim(C)
A

library(igraph)

g1<- graph_from_adjacency_matrix(C, mode =  "undirected", weighted = TRUE,
  diag = TRUE, add.colnames = NULL, add.rownames = NA)

E(g1)$weight = c(5,6,7,2,6,3,5,9,7,7,2)

plot(g1)

edges<-A

str(edges)

edge = c()
for(i in A)
    for (j in i)
        edge <- c(edge,j)

g <- graph(edge,n=max(edge), directed = FALSE)

plot(g)

