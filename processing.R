source('src/load-data.R')
source('src/postproc.R')
source('src/uses.R')
source('src/users.R')
source('src/sna.R')


# variables ----
corpus <- '/Volumes/qjd/twint/'
lemma <- 'alt-left'
lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)

for (lemma in sample(lemmas, 2)) {
  
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
subset <- 'first'

for (subset in subsets) {
  print(paste0('processing ', lemma, ' / ', subset))
  edges_sub <- subset_edges(edges, subset=subset, size=net_window_size)
  # net_to_gephi(net, lemma, subset)
  net_window_dates <- get_net_window_dates(edges_sub)
  
  net <- create_net(edges_sub, directed=directed)
  net <- add_node_info(net, directed=directed)
  net_plt <- plt_net(net, lemma, subset, net_window_dates, layout='kk')
  save_net_plt(net_plt, lemma, subset)
}

}






