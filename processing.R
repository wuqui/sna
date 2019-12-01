# variables ----
corpus <- "/Volumes/qjd/twint/"
lemma <- "shareable"
# cases <- c(
#   'poppygate', 
#   'alt-left'
#   'alt-right', 
#   'shareable'
# )


# pipeline ----
proc_lemma <- function (lemma, corpus="/Volumes/qjd/twint/") {
  tweets <- load_data(corpus, lemma)
  tweets <- postproc(tweets)
  uses = get_uses(tweets)
  uses_month <- conv_uses_month(uses)
  uses_plt <- plt_uses(uses_month)
  exp_uses_plt(uses_plt, lemma)
  # exp_users_plt(users_plt)
  # exp_net_plt(net_plt)
}

lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)

for (lemma in sample(lemmas, 3)) {
  print(paste0('processing ', lemma))
  proc_lemma(lemma)
}


# load data ----
source('src/load-data.R')
tweets <- load_data(corpus, lemma)


# post-processing ----
source('src/postproc.R')
tweets <- postproc(tweets)


# uses ----
source('src/uses.R')
uses = get_uses(tweets)
uses_tot <- get_uses_tot(uses)
age = get_age(uses)
coef_var <- get_coef_var(uses)
mean_date <- get_mean_date(uses)
max_date <- get_max_date(uses)
uses_month <- conv_uses_month(uses)
uses_plt <- plt_uses(uses_month)



# users ----
source('src/users.R')
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
users_plt <- plt_users(users_month)


# social network analysis ----
source('src/sna.R')
edges <- extract_edges(tweets)
directed = TRUE
subset = 'last'
edges_sub <- subset_edges(edges, subset=subset, size=1000)
net_window <- get_net_window(edges_sub)
net <- create_net(edges_sub, directed=TRUE)
# net_to_gephi(net, lemma, subset)
net <- add_node_info(net, directed)
net_plt <- plt_net(net, lemma, subset, net_window, layout='mds')
