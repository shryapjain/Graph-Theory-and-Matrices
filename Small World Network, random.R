#------------------------------- Q-1 SMALL WORLD GRAPH -------------------#

# In Real World graph - millions of node and billions of edges -> to complex and to large
# To Generate a network which is similar to real-world networks

# For generating graphs and computing their properties
library(igraph)

# Erdos-Rényi Model
# Erdos : generates networks randomly depending on probability distributions (G(n,p) and G(n,m)) 
# G(n->nodes,p->prob): generates uniderected graph
# G(n->nodes,m->edges): edges selected uniformly at random
g1 <- erdos.renyi.game(50, 0.03)
plot(g1,layout = layout.fruchterman.reingold)

# retrieving edge list
get.edgelist(g1)
degree_distribution(g1)

# LIMITATOINS: To0 Random, does not follow power law.

# barabasi albert model
g3 <- sample_pa(10)
degree_distribution(g3)
plot(g3)
# this is scale-free model

# Using Watts-Strogatz Model
g2 <- watts.strogatz.game(1, 6, 2, 0.05)
g2
plot(g2)

#sample_smallworld(dim, size, nei, p)
g3<-sample_smallworld(1,7,2,0.05)
plot(g3)


#------------------------------- Q-2 RANDOMLY GENERATED ADJACENCY MATRIX -------------------#

DAG.random <- function(v, nedges=1) 
  {
    edges.max <- v*(v-1)/2
    ### Assert length(v)==1 && 1 <= v
    ### Assert 0 <= nedges <= edges.max
    
    index.edges <- lapply(list(1:(v-1)), function(k) rep(k*(k+1)/2, v-k)) 
    index.edges <- index.edges[[1]] + 1:edges.max
    graph.adjacency <- matrix(0, ncol=v, nrow=v)
    graph.adjacency[sample(index.edges, nedges)] <- 1
    graph.adjacency
  }

set.seed(17)

n <- 6; e <- 4
a <- DAG.random(n, e)
a
a[seq(from=2, by=n+1, length.out=n-1)] <- 1
a


# another way of randomly generating adjacency matrix for simple graph
v = 5
adj.matrix <- function(v,e){
  if(e>v*(v-1)/2)
    stop('number of edges cannot be greater than v(v-1)/2 in simple graph')
  adj <- matrix(0,nrow=v,ncol=v)
  e.count = 0
  while (e.count < e){
    x <- sample(1:v,1)
    y <- sample(1:v,1)
    if(x!=y){
    adj[x,y] <- 1
    e.count = sum(adj)
    }
  }
  adj
}
adj.matrix(5,10)

#-------------------------------  PLOTTING CONNECTED COMPONENTS -------------------#

# Example 1

set.seed(1)

g <- erdos.renyi.game(20, 1/20)
V(g)$name <- letters[1:20]
par(mar=rep(0,4))

plot(g)

# get components
cl <- components(g)
cl

# loop through to extract vertices of each component of size > 1
lapply(seq_along(cl$csize)[cl$csize > 1], function(x) V(g)$name[cl$membership %in% x])

# Example 2
# Weakly and strongly connected components of directed graphs
n = 20
p = 3/n
g = sample_gnp(n, p, directed=TRUE)
coords = layout_with_fr(g)
plot(g, layout=coords, vertex.size = 6, vertex.label=NA, edge.arrow.size = 0.5, edge.curved = TRUE)

#components
comp <- components(g)
comp
# weak components
wk_compo <- components(g, mode="weak")
wk_compo

# strong components
c = components(g, mode="strong")
c
nodes = which(c$membership == which.max(c$csize))

# color in red the nodes in the giant component
V(g)$color = "white"
V(g)[nodes]$color = "red"
plot(g, layout=coords, vertex.size = 6, vertex.label=NA, edge.arrow.size = 0.5, edge.curved = TRUE)
