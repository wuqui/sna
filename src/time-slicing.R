
get_time_cuts <- function (tweets) {
  age <- get_age(tweets)
  zero <- get_start_date(tweets)
  hundred <- get_end_date(tweets)
  tibble(
    CUT = c(
      'zero',
      'twentyfive',
      'fifty',
      'seventyfive',
      'hundred'
    ),
    DATE = c(
      get_start_date(tweets),
      zero + age / 4,
      zero + age / 2,
      hundred - age / 4,
      get_end_date(tweets)
    )
  )
}

time_cuts <- get_time_cuts(tweets)

ggplot(data=uses_month, aes(x=DATE, y=USES)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  ggtitle(lemma) +
  scale_y_continuous("Tweets / month") + 
  scale_x_date("") +
  geom_vline(data=time_cuts, aes(xintercept=DATE, color=CUT))


get_time_slice <- function (tweets, start, end) {
  tweets %>%
    arrange(date) %>%
    filter(
      date >= start, 
      date <= end
      )
}

subs = list()
subs[['one']] = list()
subs[['one']][['sub']] <- 'one'
subs[['two']] = list()
subs[['two']][['sub']] <- 'two'
subs[['three']] = list()
subs[['three']][['sub']] <- 'three'
subs[['four']] = list()
subs[['four']][['sub']] <- 'four'

subs[['one']][['start']] <- time_cuts %>% filter(CUT=='zero') %>% pull(DATE)
subs[['one']][['end']] <- time_cuts %>% filter(CUT=='twentyfive') %>% pull(DATE)
subs[['two']][['start']] <- time_cuts %>% filter(CUT=='twentyfive') %>% pull(DATE) + 1
subs[['two']][['end']] <- time_cuts %>% filter(CUT=='fifty') %>% pull(DATE)
subs[['three']][['start']] <- time_cuts %>% filter(CUT=='fifty') %>% pull(DATE) + 1
subs[['three']][['end']] <- time_cuts %>% filter(CUT=='seventyfive') %>% pull(DATE)
subs[['four']][['start']] <- time_cuts %>% filter(CUT=='seventyfive') %>% pull(DATE) + 1
subs[['four']][['end']] <- time_cuts %>% filter(CUT=='hundred') %>% pull(DATE)

subs[['one']][['tweets']] <- get_time_slice(tweets, subs[['one']][['start']], subs[['one']][['end']])
subs[['two']][['tweets']] <- get_time_slice(tweets, subs[['two']][['start']], subs[['two']][['end']])
subs[['three']][['tweets']] <- get_time_slice(tweets, subs[['three']][['start']], subs[['three']][['end']])
subs[['four']][['tweets']] <- get_time_slice(tweets, subs[['four']][['start']], subs[['four']][['end']])

n_tweets <- nrow(tweets)
n_subs = 0
for (sub in subs) {
  n_subs = n_subs + nrow(sub[['tweets']])
}
n_tweets == n_subs
