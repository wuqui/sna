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




get_sources <- function (tweets) {
  tweets %>%
      select(username) %>%
      distinct(username)
}


get_targets <- function (edges) {
  edges %>%
    select(TO) %>%
    distinct(TO) %>%
    rename(username = TO)
}


get_nodes <- function (tweets, edges) {
  sources <- get_sources(tweets)
  targets <- get_targets(edges)
  bind_rows(sources, targets) %>%
    distinct(username)
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


# deprecated, irrelevant when subsetting is not based on edges
# get_net_window_dates <- function (edges) {
  # paste(edges$DATE[1], "--", tail(edges$DATE, n=1), sep="")
# }


create_net <- function (edges, nodes, directed) {
  # add edge weights
  net_igr <- graph_from_data_frame(edges, vertices=nodes, directed=directed)
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


get_net_metrics <- function (net, directed, subset, subsetting) {
  net_metrics <- list(
      'edges_n' = length(E(net)),
      'nodes_n' = length(V(net)),
      'cent_degree' = centralization.degree(net, normalized=TRUE)$centralization, # could use `mode=IGRAPH_IN` to only consider in-degree
      'cent_ev' = centralization.evcent(net, normalized=TRUE)$centralization,
      'modularity' = 'NA',
      'communities_n'= 'NA'
  )
  if (subset[['sub']] != 'full' & subsetting == 'freq') {
    communities <- cluster_edge_betweenness(net, directed=directed, weights=NULL)
    net_metrics[['modularity']] = modularity(communities)
    net_metrics[['communities_n']] = length(communities)
  } 
  return(net_metrics)
  # currently not used
    # cent_between = centralization.betweenness(net, normalized=TRUE)$centralization,
    # cent_close = centralization.closeness(net, normalized=TRUE)$centralization,
    # 'reciprocity' = reciprocity(net), # proportion of mutual connections
    # 'transitivity'= transitivity(net), # probability that two neighbors of a vertex are connected, i.e. ratio of triangles and connected triples
    # 'density' = igraph::graph.density(igraph::simplify(net, remove.multiple=TRUE)), # ratio of observed vs. potential edges
    # 'assortativity' = assortativity_degree(net), # propensity of similar nodes to be connected
    # 'clique_size_max' = clique_num(net), # size of biggest clique
    # 'comp_unconn' = count_components(net) # n. of unconnected components
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

plt_net <- function (net, lemma, subset, win_start, win_end, layout) {
  ggraph(net, layout=layout) +
    geom_edge_link(aes(width=WEIGHT), show.legend=FALSE) +
    scale_edge_width(range = c(0.2, 1.5)) +
    geom_node_point(aes(size=STRENGTH, colour=COMMUNITY), show.legend=FALSE) +
    geom_node_text(aes(label=name, size=.3*STRENGTH, filter=STRENGTH > median(STRENGTH)), show.legend=FALSE, check_overlap=TRUE) +
    theme_graph() +
    ggtitle(
      label=lemma,
      subtitle=paste0("subset: ", subset, " (", win_start, " -- ", win_end, ")")
    )
}


save_net_plt <- function (net_plt, lemma, subset) {
  dir_out <- 'out/net/'
  fname <- paste0('net_', lemma, '_', subset, '.pdf')
  ggsave(paste0(dir_out, fname), net_plt, device=cairo_pdf, width=5, height=5)
}
