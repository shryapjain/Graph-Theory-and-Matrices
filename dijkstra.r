dijkastra1 <-function(cost,start,end){
    # Setting the inital table
    n = length(cost[,1])
    label = numeric(n)
    flag = numeric(n)
    prev = numeric(n)
    
    for (i in 1:n){
        prev[i]=-1
        label[i]=cost[start,i]
     #   if(label[i]<999){
     #       prev[i]=1
      #  }
    }
    flag[start]=1
   # prev[start]=NA
    
    count=1
    while(count<=n){
        min = 999
        # finding minimum dist vertie and flagging
        for (j in 1:n){
            if(label[j]<min && !flag[j]){
                min = label[j]
                u = j
                
            }
        }
        flag[u]=1
        count=count+1
        # updating the table
        for(j in 1:n){
            if(label[j]>cost[u,j]+label[u]){
               label[j]=cost[u,j]+label[u]
               prev[j]=u 
            }
        }
    }

     sp = c()
  node = end
  while(node != -1){
    sp = c(node,sp)
    node = prev[node]
  }
  sp = c(start,sp)
    print(sp)
  

    node_name = c(1:n)
    t = data.frame(node_name,label,flag,prev)
    return (list(t(t),sp))
    
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

dijkastra1(S,1,7)

library(igraph)
adj.S = replace(S,S == 999,0)
g1<-graph.adjacency(adj.S,mode = 'directed',weighted = TRUE)
plot(g1,edge.label = E(g1)$weight)

## usig inbuilt function
g <-graph.adjacency(S,weighted = TRUE)
plot(g)
shortest.paths(g,algorithm = 'dijkstra')

# Creating a distance matrix 
# creating a random graph for input
set.seed(100)
S = matrix(999,10,10)
for(i in 1:50){
  a = round(runif(1,1,10))
  b = round(runif(1,1,10))
#  print(a,b)
  S[a,b] = round(runif(1,1,20))
}
S

dijkastra1(S,1,10)

library(igraph)
adj.S = replace(S,S == 999,0)
g1<-graph.adjacency(adj.S, mode = 'directed',weighted = TRUE)
plot(g1,edge.label = E(g1)$weight)


