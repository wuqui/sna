# load code ----

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
lemmas = list.dirs(corpus, full.names=FALSE, recursive=FALSE)
# lemmas <- c('ghosting', 'lituation', 'alt-left', 'solopreneur')
# lemma = 'alt-left'


for (lemma in lemmas) {
  
print(paste0('processing ', lemma))
  
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
subs[['full']] = list()
subs[['full']][['sub']] <- 'full'


## determine cut-offs ----
subs[['first']][['cut']] <- get_start_date(tweets)
subs[['mean']][['cut']] <- get_sub_mean_max_cut(uses_month)$mean_date
subs[['max']][['cut']] <- get_sub_mean_max_cut(uses_month)$max_date
subs[['last']][['cut']] <- get_end_date(tweets)
subs[['full']][['cut']] <- 'NA'


## set window size ----
win_size = 1000


## get subsets ----
subs[['first']][['tweets']] <- get_sub_first_tweets(tweets, win_size)
subs[['mean']][['tweets']] <- get_sub_mean_tweets(tweets, subs[['mean']][['cut']], win_size)
subs[['max']][['tweets']] <- get_sub_max_tweets(tweets, subs[['max']][['cut']], win_size)
subs[['last']][['tweets']] <- get_sub_last_tweets(tweets, win_size)
subs[['full']][['tweets']] <- tweets

  
## get subset infos
subs[['first']][['start']] <- get_start_date(subs[['first']][['tweets']])
subs[['first']][['end']] <- get_end_date(subs[['first']][['tweets']])
subs[['mean']][['start']] <- get_start_date(subs[['mean']][['tweets']])
subs[['mean']][['end']] <- get_end_date(subs[['mean']][['tweets']])
subs[['max']][['start']] <- get_start_date(subs[['max']][['tweets']])
subs[['max']][['end']] <- get_end_date(subs[['max']][['tweets']])
subs[['last']][['start']] <- get_start_date(subs[['last']][['tweets']])
subs[['last']][['end']] <- get_end_date(subs[['last']][['tweets']])
subs[['full']][['start']] <- get_start_date(subs[['full']][['tweets']])
subs[['full']][['end']] <- get_end_date(subs[['full']][['tweets']])


# plot uses ---- 
uses_plt <- plt_uses(
  lemma=lemma,
  uses_month=uses_month,
  sub_first_start=subs[['first']][['start']], sub_first_cut=subs[['first']][['cut']], sub_first_end=subs[['first']][['end']],
  sub_mean_start=subs[['mean']][['start']], sub_mean_cut=subs[['mean']][['cut']], sub_mean_end=subs[['mean']][['end']],
  sub_max_start=subs[['max']][['start']], sub_max_cut=subs[['max']][['cut']], sub_max_end=subs[['max']][['end']],
  sub_last_start=subs[['last']][['start']], sub_last_cut=subs[['last']][['cut']], sub_last_end=subs[['last']][['end']]
  )

uses_plt
save_uses_plt(uses_plt, lemma)


# users ----
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
users_plt <- plt_users(users_month)
# save_users_plt(users_plt, lemma)


# social network analysis ----
directed = TRUE
layout = 'kk'

for (sub in subs) {
  
  print(paste0('processing ', lemma, ' / ', sub[['sub']]))
  
  edges <- extract_edges(sub[['tweets']])
  net <- create_net(edges, directed=directed)
  
  sub[['net_metrics']] <- get_net_metrics(net, directed, sub)
  
  if (sub[['sub']] != 'full') {
    net <- add_node_info(net, directed=directed)
    net_plt <- plt_net(net, lemma, sub[['sub']], sub[['start']], sub[['end']], layout)
    net_plt
    save_net_plt(net_plt, lemma, sub[['sub']])
  }
  
  # df_comp <- tibble(
  df_comp  %<>% add_row(
    LEMMA = lemma,
    USES = uses_tot,
    USERS = users_tot,
    AGE = age,
    COEF_VAR = coef_var,
    SUBSET = sub[['sub']],
    START = sub[['start']],
    END = sub[['end']],
    EDGES = sub[['net_metrics']][['edges_n']],
    NODES = sub[['net_metrics']][['nodes_n']],
    COMMUNITIES = sub[['net_metrics']][['communities_n']],
    MODULARITY = sub[['net_metrics']][['modularity']],
    CENT_DEGREE = sub[['net_metrics']][['cent_degree']],
    CENT_EV = sub[['net_metrics']][['cent_ev']],
    SKIP = 'NO',
    NROWS = 'NA',
    STAMP = now()
    )

  df_comp %<>%
    # remove duplicates
    group_by(LEMMA) %>%
    arrange(desc(STAMP)) %>%
    distinct(SUBSET, .keep_all=TRUE) %>%
    ungroup() %>%
    # arrange
    arrange(LEMMA, match(SUBSET, c('first', 'mean', 'max', 'last', 'full'), desc(SUBSET)))
    
  df_comp %>% write_csv('out/df_comp.csv')
  
} # end of subset loop

} # end of lemma loop







