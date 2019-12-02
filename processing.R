source('src/load-data.R')
source('src/postproc.R')
source('src/uses.R')
source('src/users.R')
source('src/sna.R')


# variables ----
corpus <- '/Volumes/qjd/twint/'
# lemma <- 'alt-left'
lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)
# lemmas <- c('poppygate')

for (lemma in sample(lemmas, 10)) {
  
  print(paste0('processing ', lemma))

# load data ----
tweets <- load_data(corpus, lemma)


# post-processing ----
tweets <- postproc(tweets)


# uses ----
uses = get_uses(tweets)
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

subsets <- c('first', 'mean', 'max', 'last')
# subset <- 'first'

for (subset in subsets) {
  print(paste0('processing ', lemma, ' / ', subset))
  edges_sub <- subset_edges(edges, subset=subset, size=net_window_size)
  # net_to_gephi(net, lemma, subset)
  net_window_dates <- get_net_window_dates(edges_sub)
  
  net <- create_net(edges_sub, directed=directed)
  net <- add_node_info(net, directed=directed)
  net_plt <- plt_net(net, lemma, subset, net_window_dates, layout='kk')
  save_net_plt(net_plt, lemma, subset)
  
  # modularity and communities
  communities <- cluster_edge_betweenness(net, directed=directed, weights=NULL)
  modularity <- modularity(communities)
  communities_n <- length(communities)
  
  edges_n <- length(E(net))
  nodes_n <- length(V(net))
  reciprocity <- reciprocity(net) # proportion of mutual connections
  density <- igraph::graph.density(igraph::simplify(net, remove.multiple=TRUE)) # ratio of observed vs. potential edges
  cent_degree <- centralization.degree(net, normalized=TRUE)$centralization # could use `mode=IGRAPH_IN` to only consider in-degree
  transitivity <- transitivity(net) # probability that two neighbors of a vertex are connected, i.e. ratio of triangles and connected triples
  
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
      TRANSITIVITY = transitivity,
      # ASSORTATIVITY = assortativity,
      # WEAK_TIES = weak_ties_prop,
      # CENT_BETWEEN = cent_between,
      # CENT_CLOSE = cent_close,
      # CENT_EV = cent_ev,
      # CLIQUE_SIZE_MAX = clique_size_max,
      # COMP_UNCONN = comp_unconn,
      STAMP=now()
    )
  
}

}

df_comp %>%
  # remove duplicates
  group_by(LEMMA) %>%
  arrange(desc(STAMP)) %>%
  distinct(SUBSET, .keep_all=TRUE) %>%
  arrange(LEMMA, match(SUBSET, c('first', 'mean', 'max', 'last'), desc(SUBSET))) %>%
  # save
  write_csv('out/df_comp.csv') %>%
  # do analysis
  select(LEMMA, SUBSET, CENT_DEGREE, COMMUNITIES, TRANSITIVITY, RECIPROCITY, DENSITY, EDGES, USES) %>%
  filter(SUBSET == 'last') %>%
  arrange(desc(CENT_DEGREE)) %>%
  View()
