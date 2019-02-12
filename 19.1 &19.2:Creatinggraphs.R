19.1

A)
library(igraph)
# define links in data
edges <- rbind(
  c("A", "B"), c("A", "C"), c("B", "C"),
  c("C", "D"), c("D", "E")
)


g <- graph.edgelist(edges, directed = FALSE)
plot(g, vertex.size = 1, vertex.label.dist = 0.5)


B)
You would have to remove D and E to make it a clique.

C)
degree(g)
Degree for node A is 2

D)
E has the lowest degree

E)
degree_distribution(g)
0.0 0.2 0.6 0.2


F)
Yes it is a connected graph
***???***Unsure

G)
betweenness(g)
A=0
C=4

H)
edge_density(g)
0.5


19.2)
A)
7/5 - 1
=40%


B)
****????***
  Unsure- proceding with the assumption that max potential meaning that the two nodes
will connect to every existing node including each other.

12 new edges
5 existing edges
(16/5 - 1) * 100
=220%

C & D)
All connections:
1, 2, 2, 2, 3
2 is the median

in the book it says new node even though it says it is adding two in the beginning part
so I going to continue as if I am adding two nodes
#I am going to make a network with the two added points with two connections for each.
edges <- rbind(
  c("A", "B"), c("A", "C"), c("B", "C"),
  c("C", "D"), c("D", "E"), c("F", "D"), c("G", "F"), c("G", "E")
)


g <- graph.edgelist(edges, directed = FALSE)
plot(g, vertex.size = 1, vertex.label.dist = 0.5)
edge_density(g)

This decreases the edge density and hence is a smaller network than the first.


E)
degree_distribution(g)
0.0000000 0.0000000 0.7142857 0.2857143
