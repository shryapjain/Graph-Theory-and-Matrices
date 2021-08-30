
# Key packages to install: 

install.packages("igraph") 
install.packages("network") 
install.packages("ndtv")


# ================ Read the example data ================

getwd()
setwd('C:/Users/Shraddha/Desktop/R final')
# DATASET 1: edgelist 

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


# DATASET 2: matrix 

nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

# Examine the data:
head(nodes2)
head(links2)

# links2 is an adjacency matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)


# ================ Plotting networks with igraph ================
 
library(igraph)

# Converting the data to an igraph object:
net <- graph.data.frame(links, nodes, directed=T) 

# Examine the resulting object:
class(net)
net 

# It's easy to access nodes, edges, and their attributes:
E(net)
V(net)
E(net)$type
V(net)$media

# You can also manipulate the network matrix:
net[1,]
net[5,7]

# First attempt to plot the graph:
plot(net) # not pretty!

# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)


# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set node color to orange and the border color to hex #555555
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.4, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 


# The second way to set attributes is to add them to the igraph object.

# Generate colors base on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degree (#links) and use it to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

plot(net) 

# We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50") 


# We can also add a legend explaining the meaning of the colors we used:
plot(net) 
legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)


# Sometimes, especially with semantic networks, we may be interested in 
# plotting only the labels of the nodes:

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=1.2, edge.color="gray90")


# Let's color the edges of the graph based on their source node color.
# We'll get the starting node for each edge with "get.edges"
edge.start <- get.edges(net, 1:ecount(net))[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)

#  ------->> Network Layouts --------

# Network layouts are algorithms that return coordinates for each
# node in a network.

# Let's generate a slightly larger 80-node graph.

net.bg <- barabasi.game(80) 
V(net.bg)$size <- 8
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

# You can set the layout in the plot function:
plot(net.bg, layout=layout.random)

# Or calculate the vertex coordinates in advance:
l <- layout.circle(net.bg)
plot(net.bg, layout=l)

# l is simply a matrix of x,y coordinates (N x 2) for the N 
# nodes in the graph. You can generate your own:
l
l <- matrix(c(1:vcount(net.bg), c(1, vcount(net.bg):2)), vcount(net.bg), 2)
plot(net.bg, layout=l)

# This layout is just an example and not very helpful - thankfully
# igraph has a number of built-in layouts, including:

# Randomly placed vertices
l <- layout.random(net.bg)
plot(net.bg, layout=l)

# Circle layout
l <- layout.circle(net.bg)
plot(net.bg, layout=l)

# 3D sphere layout
l <- layout.sphere(net.bg)
plot(net.bg, layout=l)

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices. 

l <- layout.fruchterman.reingold(net.bg, repulserad=vcount(net.bg)^3, 
                                      area=vcount(net.bg)^2.4)
par(mfrow=c(1,2)) # plot two figures - 1 row, 2 columns
plot(net.bg, layout=layout.fruchterman.reingold)
plot(net.bg, layout=l)

l <- layout.kamada.kawai(net.bg)
plot(net.bg, layout=l)

l <- layout.spring(net.bg, mass=.5)
plot(net.bg, layout=l)


# LGL algorithm for large connected graphs. Here you can specify a root - 
# the node that will be placed in the middle of the layout.
plot(net.bg, layout=layout.lgl)

# By default, igraph uses a layout called layout.auto which selects
# an appropriate layout algorithm based on the properties of the graph. 


# ------->> Highlighting aspects of the network --------

plot(net)

# Notice that this network plot is still not too helpful.
# We can identify the type and size of nodes, but cannot see
# much about the structure since the links we're examining are so dense.
# One way to approach this is to see if we can sparsify the network.

hist(links$weight)
mean(links$weight)
sd(links$weight)

# There are more sophisticated ways to extract the key edges,
# but for the purposes of this excercise we'll only keep ones
# that have weight higher than the mean for the network.

# We can delete edges using delete.edges(net, edges)
cut.off <- mean(links$weight) 
net.sp <- delete.edges(net, E(net)[weight<cut.off])
plot(net.sp) 

l <- layout.fruchterman.reingold(net.sp, repulserad=vcount(net.sp)^2.1)
plot(net.sp, layout=l) 


# Another way to think about this is to plot the two tie types 
# (hyperlik & mention) separately:

E(net)$width <- 2
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
      vertex.color="gray40", layout=layout.circle)

# Another way to delete edges:  
net.m <- net - E(net)[E(net)$type=="hyperlink"]
net.h <- net - E(net)[E(net)$type=="mention"]

# Plot the two links separately:
par(mfrow=c(1,2))

plot(net.h, vertex.color="orange", main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")


# Make sure the nodes stay in place in both plots:
par(mfrow=c(1,2),mar=c(1,1,4,1))

l <- layout.fruchterman.reingold(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")



# We can also try to make the network map more useful by
# showing the communities within it:
optimal.community(net)
V(net)$community <- optimal.community(net)$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])




# ------->> Highlighting specific nodes or links --------


# Sometimes we want to focus the visualization on a particular node
# or a group of nodes. Let's represent distance from the NYT:
shortest.paths(net, algorithm="unweighted")
dist.from.NYT <- shortest.paths(net, algorithm="unweighted")[1,]

oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")


# Or we can show all the immediate neighbors of the WSJ:
col <- rep("grey40", vcount(net))
col[V(net)$media=="Wall Street Journal"] <- "#ff5100"

# The neighbors function finds all nodes one step out from the focal actor:
# (the corresponding function that finds all edges for a node is "incident")
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

col[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=col)


# Another way to draw attention to a group of nodes:
plot(net, mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA)
# Mark multiple groups:
plot(net, mark.groups=list(c(1,4,5,8), c(15:17)), 
          mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)

# Highlight a path in the network
news.path <- get.shortest.paths(net, 
                                V(net)[media=="MSNBC"], 
                                V(net)[media=="New York Post"],
                                mode="all", output="both")

# Generate edge color variable:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"

# Generate edge width variable:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4

# Generate node color variable:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)
      

# ------->> Other ways to represent a network -------- 

# One reminder that there are other ways to represent a network:

# Heatmap of the network matrix:
netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(20), 
        scale="none", margins=c(10,10) )

# Degree distribution
dd <- degree.distribution(net, cumulative=T, mode="all")
plot(dd, pch=19, cex=2, col="orange", xlab="Degree", ylab="Cumulative Frequency")



# ================ Plotting two-mode networks with igraph ================

head(nodes2)
head(links2)

net2 <- graph.incidence(links2)
plot(net2)

# This time we will make nodes look different based on their type.
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 

plot(net2, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 

 
# Using text as nodes:
par(mar=c(0,0,0,0))
plot(net2, vertex.shape="none", vertex.label=nodes2$media,
     vertex.label.color=V(net2)$color, vertex.label.font=2, 
     vertex.label.cex=.95, edge.color="gray70",  edge.width=2)

detach(package:igraph)
