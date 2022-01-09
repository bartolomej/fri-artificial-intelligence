library(igraph)

set.seed(1)

draw <- function(path)
{
  x <- read.table(path, sep=",", header=F)
  x <- as.matrix(x)
  colnames(x) <- LETTERS[1:nrow(x)]
  rownames(x) <- LETTERS[1:nrow(x)]
  G <- graph_from_adjacency_matrix(
    x,
    mode = c("undirected"),
    weighted = TRUE,
    diag = TRUE,
    add.colnames = FALSE,
    add.rownames = NA
  )
  V(G)$label=1:ncol(x)
  Isolated = which(degree(G)==0)
  G2 = delete.vertices(G, Isolated)
  
  nV = (nrow(table(V(G2))) + 1)

  # https://www.r-graph-gallery.com/248-igraph-plotting-parameters.html
  plot(G2,
       layout=layout.circle,
       vertex.label=1:nV,                    # Character vector used to label the nodes
       vertex.label.color="white",
       vertex.label.family="Times",                   # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.cex=1,                 # Font size (multiplication factor, device-dependent)
       vertex.label.dist=0,                           # Distance between the label and the vertex
       vertex.label.degree=0 ,                        # The position of the label in relation to the vertex (use pi)
       vertex.size=30,
       edge.color=rep(c("orange", "red"), nV),           # Edge color
       edge.width=seq(1,10),                        # Edge width, defaults to 1
       edge.arrow.size=1,                           # Arrow size, defaults to 1
       edge.arrow.width=1,                          # Arrow width, defaults to 1
       edge.lty=c("solid")                           # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
  )
}

draw("./seminar-2/subgraphs/labyrinth_1.txt")