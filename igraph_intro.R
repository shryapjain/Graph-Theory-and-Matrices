# Key packages to install: 
#install.packages("igraph") 
library(igraph)

#Basics of igraph

G=graph.empty(n=10,directed = TRUE)
G
#creating complete graph:
g1=graph.full(n=10,directed = FALSE , loops = FALSE)
g1
plot(g1)

#creating inner and outer star graph:
g2=graph.star(n=10,mode = "out")
g2
plot(g2)
g3=graph.star(n=10,mode = "in")
plot(g3)

#creating cycle
g4=graph.ring(n=10)
plot(g4)

#connect the node with vertices not farther than given limit

size=50
g=connect.neighborhood(graph.ring(size),10)
plot(g,layout=layout.fruchterman.reingold)

# a graph with given edges
edges=c(1,2, 3,1, 2,4, 3,5) 
g5=graph(edges,n=max(edges),directed = TRUE)
plot(g5)
vcount(g5) #for finding vector size
ecount(g5) #for finding ege size

neighbors(g5,vcount(g5)[1],mode=1)
get.edgelist(g5)

#Modification of graph
new_edges=c(1,3, 1,5 , 2,5, 4,5)
new_edges
g6=add.vertices(g5,5)
g6
plot(g6)
g6=add.edges(g6,new_edges)
plot(g6)
E(g6)$weight=runif(ecount(g6))
plot(g6)
get.adjacency(g6,attr = "weight")

incident(g5,2,mode = 'total')
incident(g5,2,mode = 'out')

is.directed(g5)

is.bipartite(g5)

is.connected(g5)

is.igraph(g2)

is.simple(g4)

is.weighted(g5)

are.connected(g5,1,3)

get.edgelist(g5)

get.edge.attribute(g5)

V(g5)

E(g5)


# chabging vertex labels
V(g6)$name <- letters[1:vcount(g6)]
plot(g6)

# Adding weights
E(g6)$weight <- runif(ecount(g6),1,5)
is.weighted(g6)

# get adjacency matrix
get.adjacency(g6, attr = 'weight')

#GARPHS IMPORTS
A<-read.graph("C:\\Users\\Shraddha\\Desktop\\R final\\graph.txt",format="edgelist")
plot.igraph(A)

setwd("Z:\\")
getwd

#EXPORT GRAPH
write.graph(A,file = 'my_graph.txt',format="edgelist")

getwd()


# different layouts for plotting in igraph
net.bg <- barabasi.game(80)
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- ""
V(net.bg)$size <- 10
E(net.bg)$arrow.mode <- 0


plot(net.bg)
plot(net.bg, layout=layout.random)

l <- layout.circle(net.bg)
plot(net.bg, layout=l)

l <- layout.sphere(net.bg)
plot(net.bg, layout=l)

plot(net.bg, layout=layout.fruchterman.reingold)

l <- layout.kamada.kawai(net.bg)
plot(net.bg, layout=l)

l <- layout.spring(net.bg, mass=.5)
plot(net.bg, layout=l)


# We can also get components of graph
is.connected(g)
c = components(g)
c

c$csize

table(c$csize)

# get the giant component
nodes = which(c$membership == which.max(c$csize))

# color in red the nodes in the giant component
V(g)$color = "white"
V(g)[nodes]$color = "red"
plot(g, layout=coords, vertex.size = 3, vertex.label=NA)


# connectivity -> number of vertices to be deleted for graph to be disconnected
h<-graph.full(5)
plot(h)
vertex_connectivity(h)

