library(ggraph) 
library(igraph) 
library(cols4all)
setwd("/Users/lina/Dropbox/My_Doctoral_thesis/R-files")
## create a node table
tbl_vertices <- read.csv("nodes.csv", na.string = "NA")
tbl_edges <- read.csv("edges.csv", na.string = "NA")
graph <- graph_from_data_frame(tbl_edges, tbl_vertices, directed = TRUE)

ggraph(graph, layout = 'igraph', algorithm = 'tree') + 
  geom_edge_diagonal(edge_width = 0.5, alpha =.4) +
  geom_node_label(aes(label=tbl_vertices$node, fill= tbl_vertices$type), 
                  col = "white", fontface = "bold", hjust = "inward") +
  scale_color_brewer(palette='Set1') +
  guides(fill = "none") +
  theme_void() +
  coord_flip()



