library(igraph)

n = 100
p = 1.75/n
g = sample_gnp(n, p)
coords = layout_with_fr(g)
plot(g, layout=coords, vertex.size = 3, vertex.label=NA)

is.connected(g)

c = components(g)
c


c$csize

table(c$csize)

# get the giant component
nodes = which(c$membership == which.max(c$csize))
nodes
# color in red the nodes in the giant component
V(g)$color = "white"
V(g)[nodes]$color = "red"
plot(g, layout=coords, vertex.size = 3, vertex.label=NA)


n = 100
p = 4/n
g = sample_gnp(n, p)
coords = layout_with_fr(g)
plot(g, layout=coords, vertex.size = 3, vertex.label=NA)

bc = biconnected_components(g)

# number of bicomponents
bc$no

bc


# number of biconnected components
bc$no

# edges of biconnected components
bc$component_edges

# nodes of biconnected components
bc$components

cl = bc$components
size = sapply(cl, length)
length(size)
size
table(size)

# highlight the largest bi-component
giant = cl[[which.max(size)]]
V(g)$color  = "white"
V(g)[giant]$color = "red"
plot(g, layout=coords, vertex.label=NA, vertex.size=5)

# This is computationally heavy
b = cohesive_blocks(g)

# print hieararchy of blocks
print(b)

# plot hieararchy of blocks
plot_hierarchy(b)

# nodes from blocks
blocks(b)

# graphs from blocks
bg = graphs_from_cohesive_blocks(b, g)
bg

# plot most cohesive graph
c = cohesion(b)
h = bg[[which.max(c)]]
plot(h, layout=layout_with_fr(h), vertex.size = 3, vertex.label=NA)

# connectivity of the graph -> number of vertices to be deleted for the graph will become disconnected
vertex_connectivity(h)

n = 10
p = 2/n
g = sample_gnp(n, p, directed=TRUE)
coords = layout_with_fr(g)
plot(g, layout=coords, vertex.size = 6, vertex.label=NA, edge.arrow.size = 0.5, edge.curved = TRUE)

# weak components
components(g, mode="weak")

#strong components
c = components(g, mode="strong")
c
nodes = which(c$membership == which.max(c$csize))

# color in red the nodes in the giant component
V(g)$color = "white"
V(g)[nodes]$color = "red"
plot(g, layout=coords, vertex.size = 6, vertex.label=NA, edge.arrow.size = 0.5, edge.curved = TRUE)

# creating a function that:
# percolation removes nodes from a graph and computes 
# the size of the giant connected component
# INPUT
# g: graph to percolate
# size: number of nodes to remove 
# d: removal vector
# OUTPUT
# giant: a vector with sizes of giant components when nodes are removed
percolate = function(g, size, d) {
  
  giant = vector()
  
  # initial size of giant component
  c = components(g)
  giant[1] = max(c$csize)
  
  names(d) = 1:length(d)
  d = sort(d, decreasing=TRUE)
  vital = as.integer(names(d[1:size]))
  
  for (i in 1:size) {
    c = components(delete_vertices(g, vital[1:i]))
    giant[i+1] = max(c$csize)
  }
  
  return(giant)
  
}

# Preferential attachment graph
g = sample_pa(n = 100, m = 2, directed=FALSE)
coords = layout_with_fr(g)
plot(g, layout=coords, vertex.label=NA, vertex.size = 5)

# resilience
size = vcount(g)/2
# random
rand = percolate(g, size, d = sample(V(g), size))    
# degree
deg = percolate(g, size, d = degree(g))    
# pagerank
pr = percolate(g, size, d=page_rank(g)$vector)
## pagerank -> all searching algo work on this, 
## we have matrix of webpage graph, and we can find eigen 
# values of the graph, find eigen vector correponding to 
# largest eigen value, and the first entry of that eigen 
#vector is the value of page-rank


# betweenness
bet = percolate(g, size, d = betweenness(g))    

plot(0:size, deg, type = "l", col=1, xlab="Number of removed nodes", ylab="Size of giant component")
lines(0:size, pr, col=2)
lines(0:size, bet, col=3)
lines(0:size, rand, col=4)
lines(0:size, rep(vcount(g)/2, size+1), lty=2)
legend(x = "bottomleft", legend = c("deg", "pr", "btw", "rand"), lty = 1, col = 1:4)



