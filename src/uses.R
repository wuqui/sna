library(lubridate)
library(plotly)
library(ggplot2)


# uses info ----

get_uses <- function (tweets) {
  tweets %>%
    select(ID=id, DATE=date) %>%
    group_by(DATE) %>%
    dplyr::summarise(USES = n())
}


get_diff_start <- function (tweets, diff_start_method, diff_start_limit) {
  if (diff_start_method == 'edges') {
    tweets %>%
      filter(mentions != '[]') %>%
      select(date, username, tweet, mentions) %>%
      mutate(week = as_date(cut(date, 'week'))) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise(uses = n()) %>%
      filter(uses >= diff_start_limit) %>%
      slice(1:1) %>%
      pull(week)
  } else if (diff_start_method == 'users') {
    tweets %>% 
      select(date, username) %>%
      distinct(username, .keep_all=TRUE) %>%
      mutate(week = as_date(cut(date, 'week'))) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise(users = n()) %>%
      filter(users >= diff_start_limit) %>%
      slice(1:1) %>%
      pull(week)
  }
}


bin_uses <- function (uses, binsize) {
  uses %>%
    mutate(DATE = as_date(cut(DATE, binsize))) %>%
    group_by(DATE) %>%
    dplyr::summarize(USES = sum(USES))
}


get_uses_tot <- function (uses) {
  uses %>%
    summarise(sum(USES)) %>%
    pull()
}


get_coef_var <- function (uses) {
  uses %>%
    summarise(sd(USES) / mean(USES)) %>%
    pull()
}


get_start_date <- function (tweets) {
  tweets %>%
    head(date, n=1) %>%
    pull(date)
}


get_end_date <- function (tweets) {
  tweets %>%
    tail(date, n=1) %>%
    pull(date)
}


get_age <- function (uses) {
  first_date <- get_start_date(uses)
  last_date <- get_end_date(uses)
  age <- last_date - first_date
  return(age)
}


get_cuts_freq <- function (uses_month) {
  max <- max(uses_month$USES)
  max_idx <- which(uses_month$USES >= max)[1]
  max_date <- uses_month[[max_idx,"DATE"]]
  mean <- mean(uses_month$USES)
  mean_idx <- which(uses_month$USES >= mean)[1]
  mean_date <- uses_month[[mean_idx,"DATE"]]
  if (mean_date >= max_date) {
    uses_desc <- uses_month %>% arrange(desc(DATE))
    mean_idx <- which(uses_desc$USES >= mean)[1]
    mean_date <- uses_desc[[mean_idx,"DATE"]]
  }
  cuts_freq <- tibble(
    CUT = c(
      'one',
      'two',
      'three',
      'four'
    ),
    DATE = c(
      get_start_date(tweets),
      mean_date,
      max_date,
      get_end_date(tweets)
    )
  )
}


get_cuts_time <- function (tweets, diff_start) {
  diff_end <- get_end_date(tweets)
  diff_dur <- diff_end - diff_start
  cuts_time <- tibble(
    CUT = c(
      'one',
      'two',
      'three',
      'four'
    ),
    DATE = c(
      diff_start,
      diff_start + diff_dur / 4,
      diff_start + diff_dur / 2,
      diff_end - diff_dur / 4
    )
  )
  cuts_time %<>% 
    mutate(CUT = factor(CUT, levels=c('one', 'two', 'three', 'four')))
}


# subset tweets ----
get_slice_freq <- function (tweets, subset, win_size) {
  if (subset[['sub']] == 'one') {
    tweets %>%
      arrange(date) %>%
      slice(1:win_size)
  } else if (subset[['sub']] == 'two') {
    tweets %>%
      arrange(date) %>%
      filter(date >= subset[['cut']]) %>%
      slice(1:win_size)
  } else if (subset[['sub']] == 'three') {
    tweets %>%
      filter(date >= subset[['cut']]) %>%
      arrange((date)) %>%
      slice(1:win_size) %>%
      arrange(date)
  } else if (subset[['sub']] == 'four') {
    tweets %>%
      arrange(desc(date)) %>%
      slice(1:win_size) %>%
      arrange(date)
  }
}


get_slice_time <- function (tweets, start, end) {
  tweets %>%
    arrange(date) %>%
    filter(
      date >= start, 
      date <= end
      )
}
  

get_limits_freq <- function (subs) {
  df <- tibble(
    'SUB' = character(),
    'LIMIT' = character(),
    'DATE' = character(),
  )
  df %<>% mutate(
    DATE = as_date(DATE),
    SUB = factor(SUB, levels=c('one', 'two', 'three', 'four')),
    LIMIT = factor(LIMIT, levels=c('start', 'end'))
    )
  df %<>% add_row(SUB = 'one', LIMIT = 'start', DATE = get_start_date(subs[['one']][['tweets']]))
  df %<>% add_row(SUB = 'one', LIMIT = 'end', DATE = get_end_date(subs[['one']][['tweets']]))
  df %<>% add_row(SUB = 'two', LIMIT = 'start', DATE = get_start_date(subs[['two']][['tweets']]))
  df %<>% add_row(SUB = 'two', LIMIT = 'end', DATE = get_end_date(subs[['two']][['tweets']]))
  df %<>% add_row(SUB = 'three', LIMIT = 'start', DATE = get_start_date(subs[['three']][['tweets']]))
  df %<>% add_row(SUB = 'three', LIMIT = 'end', DATE = get_end_date(subs[['three']][['tweets']]))
  df %<>% add_row(SUB = 'four', LIMIT = 'start', DATE = get_start_date(subs[['four']][['tweets']]))
  df %<>% add_row(SUB = 'four', LIMIT = 'end', DATE = get_end_date(subs[['four']][['tweets']]))
  df %<>% add_row(SUB = 'full', LIMIT = 'start', DATE = get_start_date(subs[['full']][['tweets']]))
  df %<>% add_row(SUB = 'full', LIMIT = 'end', DATE = get_end_date(subs[['full']][['tweets']]))
  return(df)
}


get_limits_time <- function (sub_cuts) {
  df <- tibble(
    'SUB' = character(),
    'LIMIT' = character(),
    'DATE' = character(),
  )
  df %<>% mutate(
    DATE = as_date(DATE),
    SUB = factor(SUB, levels=c('one', 'two', 'three', 'four')),
    LIMIT = factor(LIMIT, levels=c('start', 'end'))
    )
  df %<>% add_row(SUB = 'one', LIMIT = 'start', DATE = sub_cuts %>% filter(CUT=='one') %>% pull(DATE))
  df %<>% add_row(SUB = 'one', LIMIT = 'end', DATE = sub_cuts %>% filter(CUT=='two') %>% pull(DATE))
  df %<>% add_row(SUB = 'two', LIMIT = 'start', DATE = sub_cuts %>% filter(CUT=='two') %>% pull(DATE) + 1)
  df %<>% add_row(SUB = 'two', LIMIT = 'end', DATE = sub_cuts %>% filter(CUT=='three') %>% pull(DATE))
  df %<>% add_row(SUB = 'three', LIMIT = 'start', DATE = sub_cuts %>% filter(CUT=='three') %>% pull(DATE) + 1)
  df %<>% add_row(SUB = 'three', LIMIT = 'end', DATE = sub_cuts %>% filter(CUT=='four') %>% pull(DATE))
  df %<>% add_row(SUB = 'four', LIMIT = 'start', DATE = sub_cuts %>% filter(CUT=='four') %>% pull(DATE) + 1)
  df %<>% add_row(SUB = 'four', LIMIT = 'end', DATE = get_end_date(tweets))
  df %<>% add_row(SUB = 'full', LIMIT = 'start', DATE = get_start_date(tweets))
  df %<>% add_row(SUB = 'full', LIMIT = 'end', DATE = get_end_date(tweets))
  return(df)
}


# plot uses ----
plt_uses <- function (
  lemma,
  uses_month,
  sub_cuts,
  sub_limits
) {
  USES_TOT <- uses_month %>% summarize(USES_TOT = sum(USES)) %>% pull(USES_TOT)
  sub_limits %<>% filter(SUB != 'full')
  sub_cuts %<>% rename(subset = CUT)
  ggplot(data=uses_month, aes(x=DATE, y=USES)) +
    geom_line() +
    geom_point() +
    geom_smooth() +
    ggtitle(paste0('n = ', USES_TOT)) +
    scale_y_continuous("Tweets / month") + 
    scale_x_date("") +
    # geom_vline(data=sub_limits, aes(xintercept=DATE, color=SUB)) +
    geom_vline(data=sub_cuts, aes(xintercept=DATE), color='royalblue4', linetype='dashed')
}


save_uses_plt <- function (uses_plt, lemma, subsetting, dir_out='out/uses/') {
  fname <- paste0('ui_', lemma, '_', subsetting, '.pdf')
  ggsave(paste0(dir_out, fname), uses_plt, width=6, height=4)
}
