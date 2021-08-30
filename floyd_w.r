# A function that returns both the distance matrix, and S
fw <- function(graph_matrix){
  n = length(graph_matrix[,1])
  D = graph_matrix
  s = matrix(0,n,n)
  #print(s)
  for(k in 1:n){
    for (i in 1:n){
      for(j in 1:n){
        if(D[i,j]>D[i,k]+D[k,j]){
          D[i,j]=D[i,k]+D[k,j]
          s[i,j]=k
        }
      }
    }
  }
  A = list(D,s)
  return(A)
}



S=matrix(999,7,7)
S[1,2]=1
S[1,3]=2
S[2,5]=2
S[3,4]=1
S[4,5]=1
S[4,6]=3
S[5,6]=2
S[6,7]=1

S

fw(S)

# A function that returns the distance matrix
floyd<-function(w)
{
  n = length(w[,1])  
  for(k in 1:n)
  {
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        w[i,j]=min(w[i,j],w[i,k]+ w[k,j])
      }
    }
  }
  return(w)
}


    
A = floyd(S)
A

library(igraph)
S.adj = replace(S,S == 999,0)
g1<-graph.adjacency(S.adj,mode = 'directed',weighted = TRUE)
plot(g1,edge.label = E(g1)$weight)

# creating a random graph for input and running the fw function
set.seed(100)
S = matrix(999,10,10)
for(i in 1:50){
  a = round(runif(1,1,10))
  b = round(runif(1,1,10))
#  print(a,b)
  S[a,b] = round(runif(1,1,20))
}
S

fw(S)

S.adj = replace(S,S == 999,0)
g1<-graph.adjacency(S.adj,mode = 'directed',weighted = TRUE)
plot(g1,edge.label = E(g1)$weight)
