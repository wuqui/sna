# load code ----

## source files
source('src/load-data.R')
source('src/postproc.R')
source('src/uses.R')
source('src/users.R')
source('src/sna.R')
source('src/analysis.R')

## packages
library(magrittr)
 
# variables ----
corpus <- '/Volumes/qjd/twint/'
lemmas <- list.dirs(corpus, full.names=FALSE, recursive=FALSE)
cases <- c('upskill', 'hyperlocal', 'solopreneur', 'alt-right', 'alt-left', 'poppygate')

directed <- TRUE

win_size <- 1000
layout <- 'kk'

subsetting <- 'time'
diff_start_method <- 'edges'
diff_start_limit <- 3

export_edges <- FALSE


for (lemma in c('alt-right')) {
  
print(paste0(match(c(lemma), lemmas), ' / ', lemma))
skip <- FALSE
  
if (exists('df_comp') == FALSE) {
  df_comp <- read_df_comp(f_path='out/df_comp.csv')
}

# load data ----
tweets <- load_data(corpus, lemma)
tweets <- postproc(tweets)
# tweets <- filter_tweets(tweets)


# uses ----
uses <- get_uses(tweets)
uses_tot <- get_uses_tot(uses)
age = get_age(tweets)
coef_var <- get_coef_var(uses)
uses_month <- bin_uses(uses, 'month')


# subsetting ---- 

subs = list()
subs[['one']] = list()
subs[['two']] = list()
subs[['three']] = list()
subs[['four']] = list()
subs[['full']] = list()

subs[['one']][['sub']] <- 'one'
subs[['two']][['sub']] <- 'two'
subs[['three']][['sub']] <- 'three'
subs[['four']][['sub']] <- 'four'
subs[['full']][['sub']] <- 'full'


## determine cut-offs ----

if (subsetting == 'freq') {
  
  sub_cuts <- get_cuts_freq(uses_month)
  subs[['one']][['cut']] <- sub_cuts %>% filter(CUT=='one') %>% pull(DATE)
  subs[['two']][['cut']] <- sub_cuts %>% filter(CUT=='two') %>% pull(DATE)
  subs[['three']][['cut']] <- sub_cuts %>% filter(CUT=='three') %>% pull(DATE)
  subs[['four']][['cut']] <- sub_cuts %>% filter(CUT=='four') %>% pull(DATE)
  subs[['full']][['cut']] <- 'NA'
  
  subs[['one']][['tweets']] <- get_slice_freq(tweets, subs[['one']], win_size)
  subs[['two']][['tweets']] <- get_slice_freq(tweets, subs[['two']], win_size)
  subs[['three']][['tweets']] <- get_slice_freq(tweets, subs[['three']], win_size)
  subs[['four']][['tweets']] <- get_slice_freq(tweets, subs[['three']], win_size)
  subs[['full']][['tweets']] <- tweets
  
  sub_limits <- get_limits_freq(subs)
  
} else if (subsetting == 'time') {
  
  diff_start <- get_diff_start(tweets, diff_start_method, diff_start_limit)
  sub_cuts <- get_cuts_time(tweets, diff_start)
  
  subs[['one']][['cut']] <- sub_cuts %>% filter(CUT=='one') %>% pull(DATE)
  subs[['two']][['cut']] <- sub_cuts %>% filter(CUT=='two') %>% pull(DATE)
  subs[['three']][['cut']] <- sub_cuts %>% filter(CUT=='three') %>% pull(DATE)
  subs[['four']][['cut']] <- sub_cuts %>% filter(CUT=='four') %>% pull(DATE)
  subs[['full']][['cut']] <- 'NA'
  
  sub_limits <- get_limits_time(sub_cuts)
  
  subs[['one']][['start']] <- sub_limits %>% filter(SUB=='one', LIMIT=='start') %>% pull(DATE)
  subs[['one']][['end']] <- sub_limits %>% filter(SUB=='one', LIMIT=='end') %>% pull(DATE)
  subs[['two']][['start']] <- sub_limits %>% filter(SUB=='two', LIMIT=='start') %>% pull(DATE)
  subs[['two']][['end']] <- sub_limits %>% filter(SUB=='two', LIMIT=='end') %>% pull(DATE)
  subs[['three']][['start']] <- sub_limits %>% filter(SUB=='three', LIMIT=='start') %>% pull(DATE)
  subs[['three']][['end']] <- sub_limits %>% filter(SUB=='three', LIMIT=='end') %>% pull(DATE)
  subs[['four']][['start']] <- sub_limits %>% filter(SUB=='four', LIMIT=='start') %>% pull(DATE)
  subs[['four']][['end']] <- sub_limits %>% filter(SUB=='four', LIMIT=='end') %>% pull(DATE)
  subs[['full']][['start']] <- sub_limits %>% filter(SUB=='full', LIMIT=='start') %>% pull(DATE)
  subs[['full']][['end']] <- sub_limits %>% filter(SUB=='full', LIMIT=='end') %>% pull(DATE)
  
  subs[['one']][['tweets']] <- get_slice_time(tweets, subs[['one']][['start']], subs[['one']][['end']])
  subs[['two']][['tweets']] <- get_slice_time(tweets, subs[['two']][['start']], subs[['two']][['end']])
  subs[['three']][['tweets']] <- get_slice_time(tweets, subs[['three']][['start']], subs[['three']][['end']])
  subs[['four']][['tweets']] <- get_slice_time(tweets, subs[['four']][['start']], subs[['four']][['end']])
  subs[['full']][['tweets']] <- tweets
  
  n_tweets <- nrow(tweets)
  n_subs = 0
  for (sub in subs) {
    if (sub[['sub']] != 'full') {
      n_subs = n_subs + nrow(sub[['tweets']])
    }
  }
  n_tweets == n_subs

} # sliding method selection


# plot uses ---- 
uses_plt <- plt_uses(lemma=lemma, uses_month=uses_month, sub_cuts, sub_limits)
uses_plt
save_uses_plt(uses_plt, lemma, subsetting)


# users ----
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
users_plt <- plt_users(users_month)
save_users_plt(users_plt, lemma)


# social network analysis ----

for (sub in subs) {
  # sub = subs[['one']]
  
  print(paste0('processing ', lemma, ' / ', sub[['sub']]))
  
  edges <- extract_edges(sub[['tweets']])
  if (nrow(edges) == 0) {
    skip <- TRUE
    next
    }
  
  sources <- get_sources(sub[['tweets']])
  targets <- get_targets(edges)
  nodes <- get_nodes(sources, targets)
  
  net <- create_net(edges, nodes, directed=directed)
  
  if (export_edges == TRUE) {net_to_gephi(net, lemma, sub)}
  
  sub[['net_metrics']] <- get_net_metrics(net, directed, sub, subsetting)
  
  if (sub[['sub']] != 'full' & subsetting == 'freq') {
    net <- add_node_info(net, directed=directed)
    net_plt <- plt_net(
      net,
      lemma,
      sub[['sub']],
      sub_limits %>% filter(SUB == sub[['sub']], LIMIT == 'start') %>% pull(DATE),
      sub_limits %>% filter(SUB == sub[['sub']], LIMIT == 'end') %>% pull(DATE),
      layout)
    net_plt
    save_net_plt(net_plt, lemma, subsetting, sub)
  }

  # save in df_comp ----
  
  # df_comp <- tibble(
  df_comp <- df_comp %>% add_row(
    LEMMA = lemma,
    USES_TOT = uses_tot,
    USERS_TOT = users_tot,
    AGE = age,
    COEF_VAR = coef_var,
    SUBSET = factor(sub[['sub']], levels=c('one', 'two', 'three', 'four', 'full')),
    START = sub_limits %>% filter(SUB == sub[['sub']], LIMIT == 'start') %>% pull(DATE),
    END = sub_limits %>% filter(SUB == sub[['sub']], LIMIT == 'end') %>% pull(DATE),
    USES = nrow(sub[['tweets']]),
    EDGES = sub[['net_metrics']][['edges_n']],
    NODES = sub[['net_metrics']][['nodes_n']],
    COMMUNITIES = sub[['net_metrics']][['communities_n']],
    MODULARITY = sub[['net_metrics']][['modularity']],
    DENSITY = sub[['net_metrics']][['density']],
    CENT_DEGREE = sub[['net_metrics']][['cent_degree']],
    CENT_EV = sub[['net_metrics']][['cent_ev']],
    SUBSETTING = subsetting,
    DIFF_START_METHOD = diff_start_method,
    DIFF_START_LIMIT = diff_start_limit,
    SKIP = skip,
    STAMP = now()
    )

  df_comp %<>%
    # remove duplicates
    group_by(LEMMA) %>%
    arrange(desc(STAMP)) %>%
    distinct(SUBSETTING, SUBSET, .keep_all=TRUE) %>%
    ungroup() %>%
    # arrange
    arrange(LEMMA, match(SUBSET, c('one', 'two', 'three', 'four', 'full'), desc(SUBSET))
      )
    
  df_comp %>% write_csv('out/df_comp.csv')
  
} # end of subset loop

} # end of lemma loop
