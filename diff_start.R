source('src/uses.R')
source('src/load-data.R')

library(magrittr)


get_diff_start <- function (tweet, method, limit) {
  if (method == 'edges') {
    tweets %>%
      filter(mentions != '[]') %>%
      select(date, username, tweet, mentions) %>%
      mutate(week = as_date(cut(date, 'week'))) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise(uses = n()) %>%
      filter(uses >= limit) %>%
      slice(1:1) %>%
      pull(week)
  } else if (method == 'users') {
    tweets %>% 
      select(date, username) %>%
      distinct(username, .keep_all=TRUE) %>%
      mutate(week = as_date(cut(date, 'week'))) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise(users = n()) %>%
      filter(users >= limit) %>%
      slice(1:1) %>%
      pull(week)
  }
}


# diff_starts %<>% slice(0:0)
lemmas = c('ghosting', 'lituation', 'alt-left', 'solopreneur')
lemmas = c('poppygate', 'microflat', 'upcycling', 'bromance')
lemmas = c('blockchain', 'cherpumple', 'Anglo-Saxon')
methods = c('edges')
limits <- c(0, 1, 2, 3, 4, 5)
limits <- factor(limits, levels=limits)

for (lemma in lemmas) {
  tweets <- load_data(corpus, lemma)
  tweets <- postproc(tweets)
  
  for (method in methods) {
    for (limit in limits) {
      
    # diff_starts %<>% slice(0:0)
    diff_starts %<>% add_row(
      LEMMA = lemma,
      TWEETS_START = get_start_date(tweets),
      DIFF_START = get_diff_start(tweets, method, limit),
      METHOD = method,
      LIMIT = limit
      )
    print(diff_starts)
    
    }
  }
}


diff_starts %<>%
  mutate(DISCREP = DIFF_START - TWEETS_START) %>%
  distinct(LEMMA, METHOD, LIMIT, .keep_all=TRUE) %>%
  arrange(LEMMA)


diff_starts %>%
  write_csv('out/diff_starts.csv')


plt <- diff_starts %>% 
  filter(!LIMIT %in% c(1, 4, 5)) %>%
  ggplot(aes(x=LEMMA, y=DIFF_START, color=LEMMA)) +
    geom_line() +
    coord_flip() +
    theme(legend.position='none')

ggplotly(plt)


diff_starts %>%
  filter(LIMIT == 3) %>%
  arrange(desc(DISCREP))


diff_starts %>%
  filter(LIMIT %in% c(2, 3)) %>%
  mutate(DISCREP_TWO_THREE = DIFF_START - lag(DIFF_START, n=1)) %>%
  filter(LIMIT == 3) %>%
  arrange(desc(DISCREP_TWO_THREE))


