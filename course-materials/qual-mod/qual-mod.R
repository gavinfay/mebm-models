# MAR 580 Fall 2022, MEBM, qualitative modeling example
# September 2022, Gavin Fay

#load libraries
library(tidyverse)
library(LoopAnalyst)
#library(permute)
library(igraph)

#User created functions
# adjoint via  matrix inverse
adjoint3 <- function(A) det(A)*solve(A)


# create the community matrix
# columns are the direct effect of an increase in that element on the rows
A <- matrix(c(-1,-1, 0, 0, 0, 0, 0,
               1,-1,-1, 1,-1, 0,-1,
               0, 1,-1,-1,-1, 0, 0,
               0, 0, 1,-1, 0, 0, 0,
               0, 0, 1, 0,-1, 1, 0,
               0, 0, 0, 1, 0,-1, 1,
               0, 0, 0, 0, 0, 0,-1), byrow=TRUE, nrow = 7)
n_nodes <- nrow(A)

node_names <- letters[1:n_nodes]

#graph the network
g1 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
V(g1)$size = 20
plot(g1,edge.arrow.size=.4)

#show the adjacency matrix
image(t(A[n_nodes:1,]))

# create the adjoint matrix (net effect of press perturbation)
  adj_A <- adjoint3(A)
  adj_A
# enumerate the T matrix
  Tmat <- make.T(A,status=TRUE)
  
# calculate the reliability weights
  Wmat <- abs(adj_A)/Tmat
  Wmat
  
#image showing sign of adjoint and weights
  colfunc <- colorRampPalette(c("white", "steelblue"))
  par(mar=c(1,3,3,1),las=0)
  image(1:n_nodes,1:n_nodes,t(Wmat[n_nodes:1,]),col = colfunc(7),
  xlab = "",
  ylab = "", axes=F)
  box()
  axis(3,labels=node_names,at=seq(1,n_nodes),tcl=-0.2,cex=0.8)
  axis(2,labels=node_names,at=seq(n_nodes,1,-1),tcl=-0.2,cex=0.8)
  text(1,0,"+",col="white",cex=2)
  pick <- which(adj_A>0)
  x <- ceiling(pick/n_nodes)
  y <- (n_nodes+1)-pick%%n_nodes
  y[y==(n_nodes+1)] <- 1
  text(x,y,"+",col="white",cex=1.5)
  pick <- which(adj_A<0)
  x <- ceiling(pick/n_nodes)
  y <- (n_nodes+1)-pick%%n_nodes
  y[y==(n_nodes+1)] <- 1
  text(x,y,"-",col="white",cex=1.5)
  
  
###
#QPress
# snowshoe hare vignette https://swotherspoon.github.io/QPress/articles/Snowshoe.html

library(QPress)

  ## Read a textual model description
  modelA <- parse.digraph(c("V-*V", "P-*P", "V*->H", "H*->P"))

  # ## Write/read a model description to a text file
  # write.digraph(modelA,"modelA.txt")
  # modelA <- read.digraph("modelA.txt")
  modelA
  
  