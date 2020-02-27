# load code

## source files

source('src/load-data.R')
source('src/postproc.R')
source('src/uses.R')
source('src/users.R')
source('src/sna.R')

## packages

library(magrittr)


# variables ----
corpus <- '/Volumes/qjd/twint/'
lemmas <- 'solopreneur'
# lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)


for (lemma in c('ghosting')) {
  
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
age = get_age(tweets)
coef_var <- get_coef_var(uses)
uses_month <- bin_uses(uses, 'month')


# subsets ---- 
subs = list()
subs[['first']] = list()
subs[['first']][['sub']] <- 'first'
subs[['mean']] = list()
subs[['mean']][['sub']] <- 'mean'
subs[['max']] = list()
subs[['max']][['sub']] <- 'max'
subs[['last']] = list()
subs[['last']][['sub']] <- 'last'


## determine cut-offs ----
subs[['first']][['cut']] <- get_start_date(tweets)
subs[['mean']][['cut']] <- get_sub_mean_max_cut(uses_month)$mean_date
subs[['max']][['cut']] <- get_sub_mean_max_cut(uses_month)$max_date
subs[['last']][['cut']] <- get_end_date(tweets)


## set window size ----
win_size = 1000


## get subsets ----
subs[['first']][['tweets']] <- get_sub_first_tweets(tweets, win_size)
subs[['mean']][['tweets']] <- get_sub_mean_tweets(tweets, sub_mean_cut, win_size)
subs[['max']][['tweets']] <- get_sub_max_tweets(tweets, sub_max_cut, win_size)
subs[['last']][['tweets']] <- get_sub_last_tweets(tweets, win_size)


## get subset infos
subs[['first']][['start']] <- get_start_date(sub_first_tweets)
subs[['first']][['end']] <- get_end_date(sub_first_tweets)
subs[['mean']][['start']] <- get_start_date(sub_mean_tweets)
subs[['mean']][['end']] <- get_end_date(sub_mean_tweets)
subs[['max']][['start']] <- get_start_date(sub_max_tweets)
subs[['max']][['end']] <- get_end_date(sub_max_tweets)
subs[['last']][['start']] <- get_start_date(sub_last_tweets)
subs[['last']][['end']] <- get_end_date(sub_last_tweets)


# plot uses ---- 
uses_plt <- plt_uses(
  lemma=lemma,
  uses_month=uses_month,
  sub_first_start=sub_first_start, sub_first_cut=sub_first_cut, sub_first_end=sub_first_end,
  sub_mean_start=sub_mean_start, sub_mean_cut=sub_mean_cut, sub_mean_end=sub_mean_end,
  sub_max_start=sub_max_start, sub_max_cut=sub_max_cut, sub_max_end=sub_max_end,
  sub_last_start=sub_last_start, sub_last_cut=sub_last_cut, sub_last_end=sub_last_end
  )

uses_plt
# save_uses_plt(uses_plt, lemma)


# users ----
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
users_plt <- plt_users(users_month)
# save_users_plt(users_plt, lemma)




# social network analysis ----
directed = TRUE
layout = 'kk'

# sub = subs[['first']]

for (sub in subs) {
  
  print(paste0('processing ', lemma, ' / ', sub[['sub']]))
  
  edges_sub = extract_edges(sub[['tweets']])
  net_window_dates <- get_net_window_dates(edges_sub)
  net <- create_net(edges_sub, directed=directed)
  
  if (sub[['sub']] != 'full') {
    net <- add_node_info(net, directed=directed)
    net_plt <- plt_net(net, lemma, sub[['sub']], net_window_dates, layout=layout)
    # save_net_plt(net_plt, lemma, subset)
    
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
      SUBSET = sub[['sub']],
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






