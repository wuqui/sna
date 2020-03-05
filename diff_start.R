source('src/uses.R')

# diff_starts %<>% slice(0:0)
lemmas = c('microflat', 'baecation')
methods = c('users', 'edges')

for (method in methods) {
  for (lemma in lemmas) {
    tweets <- load_data(corpus, lemma)
    tweets <- postproc(tweets)
    
    diff_starts %<>% add_row(
    # diff_starts <- tibble(
      LEMMA = lemma,
      TWEETS_START = get_start_date(tweets),
      DIFF_START = get_diff_start(tweets, method),
      METHOD = method
    )
    print(diff_starts)
  }
}

diff_starts %<>%
  mutate(DISCREP = DIFF_START - TWEETS_START) %>%
  distinct(LEMMA, METHOD, .keep_all=TRUE) %>%
  arrange(LEMMA)

diff_starts %>%
  write_csv('out/diff_starts.csv')

diff_starts %>% 
  ggplot(aes(x=LEMMA, y=DISCREP, fill=METHOD)) +
    geom_bar(stat='identity', position='dodge') +
    coord_flip()
