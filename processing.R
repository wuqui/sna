library(magrittr)

source('src/load-data.R')
source('src/postproc.R')
source('src/uses.R')
source('src/users.R')
source('src/sna.R')



# variables ----
corpus <- '/Volumes/qjd/twint/'
# lemma <- 'climate emergency'
# lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)


for (lemma in c('baecation')) {
  
print(paste0('processing ', lemma))
stamp = now()
  
if (exists('df_comp') == FALSE) {
  df_comp <- read_csv('out/df_comp.csv')
}
  

# load data ----
tweets <- load_data(corpus, lemma)

# post-processing ----
tweets <- postproc(tweets)

# uses ----
uses <- get_uses(tweets)
uses_tot <- get_uses_tot(uses)
age = get_age(uses)
coef_var <- get_coef_var(uses)
mean_date <- get_mean_date(uses)
max_date <- get_max_date(uses)
uses_month <- conv_uses_month(uses)
uses_plt <- plt_uses(uses_month, lemma, mean_date, max_date)
save_uses_plt(uses_plt, lemma)


# users ----
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
users_plt <- plt_users(users_month)
save_users_plt(users_plt, lemma)


# social network analysis ----

edges <- extract_edges(tweets)
directed = TRUE
net_window_size = 1000

subsets <- c('first', 'mean', 'max', 'last', 'full')
# subset <- 'first'

for (subset in subsets) {
  
  print(paste0('processing ', lemma, ' / ', subset))
  
  edges_sub <- subset_edges(edges, subset=subset, size=net_window_size)
  net_window_dates <- get_net_window_dates(edges_sub)
  net <- create_net(edges_sub, directed=directed)
  
  if (subset != 'full') {
    net <- add_node_info(net, directed=directed)
    net_plt <- plt_net(net, lemma, subset, net_window_dates, layout='kk')
    save_net_plt(net_plt, lemma, subset)
    
    communities <- cluster_edge_betweenness(net, directed=directed, weights=NULL)
    modularity <- modularity(communities)
    communities_n <- length(communities)
    cent_between = centralization.betweenness(net, normalized=TRUE)$centralization
    cent_close = centralization.closeness(net, normalized=TRUE)$centralization
    cent_ev = centralization.evcent(net, normalized=TRUE)$centralization
    reciprocity <- reciprocity(net) # proportion of mutual connections
    transitivity <- transitivity(net) # probability that two neighbors of a vertex are connected, i.e. ratio of triangles and connected triples
    density <- igraph::graph.density(igraph::simplify(net, remove.multiple=TRUE)) # ratio of observed vs. potential edges
    assortativity = assortativity_degree(net) # propensity of similar nodes to be connected
    clique_size_max = clique_num(net) # size of biggest clique
    comp_unconn = count_components(net) # n. of unconnected components
    
  } else {
    
    modularity <- 'NA'
    communities_n <- 'NA'
    cent_between = 'NA'
    cent_close = 'NA'
    cent_ev = 'NA'
    reciprocity <- 'NA'
    transitivity <- 'NA'
    density = 'NA'
    assortativity = 'NA'
    clique_size_max = 'NA'
    comp_unconn = 'NA'
    
  }
  
  edges_n <- length(E(net))
  nodes_n <- length(V(net))
  cent_degree <- centralization.degree(net, normalized=TRUE)$centralization # could use `mode=IGRAPH_IN` to only consider in-degree
  
  df_comp <- df_comp %>% add_row(
      LEMMA = lemma,
      USES = uses_tot,
      USERS = users_tot,
      AGE = age,
      COEF_VAR = coef_var,
      SUBSET = subset,
      NET_WINDOW_DATES = net_window_dates,
      EDGES = edges_n,
      NODES = nodes_n,
      COMMUNITIES = communities_n,
      MODULARITY = modularity,
      RECIPROCITY = reciprocity, 
      DENSITY = density,
      CENT_DEGREE = cent_degree,
      CENT_BETWEEN = cent_between,
      CENT_CLOSE = cent_close,
      CENT_EV = cent_ev,
      TRANSITIVITY = transitivity,
      ASSORTATIVITY = assortativity,
      CLIQUE_SIZE_MAX = clique_size_max,
      COMP_UNCONN = comp_unconn,
      SKIP = 'NO',
      NROWS = 'NA',
      STAMP = stamp
    )
  
  df_comp <- df_comp %>%
    # remove duplicates
    group_by(LEMMA) %>%
    arrange(desc(STAMP)) %>%
    distinct(SUBSET, .keep_all=TRUE) %>%
    ungroup() %>%
    # arrange
    arrange(LEMMA, match(SUBSET, c('first', 'mean', 'max', 'last', 'full'), desc(SUBSET))) %>%
    # save
    write_csv('out/df_comp.csv')
  
} # end of subset loop

} # end of lemma loop






