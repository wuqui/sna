library(tidygraph)
library(igraph)
library(ggraph)
# install.packages("devtools")
# devtools::install_github("RMHogervorst/gephi")
library(gephi)


# create edges df
extract_edges <- function (tweets) {
  edges <- tweets %>%
    select(FROM=username, TO=mentions, DATE=date) %>%
    ## remove tweets without target
    filter(!is.na(TO))
  
  ## extract targets
  targets <- str_extract_all(edges$TO, "'\\w+'")
  indices <- rep.int(seq_len(nrow(edges)), sapply(targets, length))
  edges <- edges[indices,]
  edges$TO <- unlist(targets)
  
  # remove quotes
  edges <- edges %>%
    mutate(TO=str_sub(TO, start=2, end=-2)) %>%
    arrange(DATE)
  return(edges)
}



# subset edges
# deprecated: subsetting by tweets instead
# subset_edges <- function (edges, subset='last', size=1000) {
#   if (subset == 'full') {
#     edges_sub <- edges
#   } else if (subset == 'first') {
#     edges_sub <- edges %>%
#       slice(1:size)
#   } else if (subset == 'mean') {
#     edges_sub <- edges %>%
#       filter(DATE > mean_date) %>%
#       slice(1:size)
#   } else if (subset == 'max') {
#     edges_sub <- edges %>%
#       filter(DATE > max_date) %>%
#       slice(1:size)
#   } else if (subset == 'last') {
#     edges_sub <- edges %>%
#       arrange(desc(DATE)) %>%
#       slice(1:size) %>%
#       arrange(DATE)
#   } else {
#     print('error: no valid subsetting option given')
#   }
#   return(edges_sub)
# }


get_net_window_dates <- function (edges) {
  paste(edges$DATE[1], "--", tail(edges$DATE, n=1), sep="")
}


create_net <- function (edges, directed) {
  # add edge weights
  net_igr <- graph_from_data_frame(edges, directed=directed)
  E(net_igr)$WEIGHT <- 1
  net_igr <- igraph::simplify(net_igr, edge.attr.comb=list(WEIGHT="sum", DATE="first"))
  # create tidygraph network
  net_tdg <- as_tbl_graph(net_igr, directed=directed) %>%
    activate(edges) %>%
    mutate(DATE=as_date(DATE))
  return(net_tdg)
}


# export to Gephi
net_to_gephi <- function (net, lemma, subset) {
  dir <- 'out/edges/'
  fname <- paste0(lemma, '_', subset, '.csv')
  gephi_write_edges(net, path=paste0(dir, fname))
}


add_node_info <- function (net, directed) {
  net %>%
    activate(nodes) %>%
    mutate(STRENGTH = strength(net, mode="in")) %>%
    mutate(COMMUNITY = as.factor(group_edge_betweenness(directed=directed)))
  # mutate(CENT_DEGREE = centrality_degree(weights=WEIGHT)) %>%
  # mutate(CENT_PAGERANK = centrality_pagerank(weights=WEIGHT)) %>%
  # mutate(GROUP_LOUVAIN = as.factor(group_louvain(weights=WEIGHT)))
}


plt_net <- function (net, lemma, subset, net_window, layout='kk') {
  ggraph(net, layout=layout) +
    geom_edge_link(aes(width=WEIGHT), show.legend=FALSE) +
    scale_edge_width(range = c(0.2, 1.5)) +
    geom_node_point(aes(size=STRENGTH, colour=COMMUNITY), show.legend=FALSE) +
    geom_node_text(aes(label=name, size=.3*STRENGTH, filter=STRENGTH > median(STRENGTH)), show.legend=FALSE, check_overlap=TRUE) +
    theme_graph() +
    ggtitle(
      label=lemma,
      subtitle=paste0("subset: ", subset, " (", net_window, ")")
    )
}


save_net_plt <- function (net_plt, lemma, subset) {
  dir_out <- 'out/net/'
  fname <- paste0('net_', lemma, '_', subset, '.pdf')
  ggsave(paste0(dir_out, fname), net_plt, device=cairo_pdf, width=5, height=5)
}
