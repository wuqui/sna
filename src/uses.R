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


get_sub_mean_max_cut <- function (uses_month) {
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
  return(
    list(
      'mean' = mean,
      'mean_idx' = mean_idx,
      'mean_date' = mean_date,
      'max_idx' = max_idx,
      'max_date' = max_date
    )
  )
}


# subset tweets ----
get_sub_first_tweets <- function (tweets, win_size) {
  tweets %>%
    arrange(date) %>%
    slice(1:win_size)
}

get_sub_mean_tweets <- function (tweets, sub_mean_cut, win_size) {
  tweets %>%
    arrange(date) %>%
    filter(date >= sub_mean_cut) %>%
    slice(1:win_size)
}

get_sub_max_tweets <- function (tweets, sub_max_cut, win_size) {
  tweets %>%
    filter(date >= sub_max_cut) %>%
    arrange((date)) %>%
    slice(1:win_size) %>%
    arrange(date)
}

get_sub_last_tweets <- function (tweets, win_size) {
  tweets %>%
    arrange(desc(date)) %>%
    slice(1:win_size) %>%
    arrange(date)
}


# plot uses ----
plt_uses <- function (
  lemma,
  uses_month,
  sub_first_start, sub_first_cut, sub_first_end,
  sub_mean_start, sub_mean_cut, sub_mean_end,
  sub_max_start, sub_max_cut, sub_max_end,
  sub_last_start, sub_last_cut, sub_last_end
) {
  ggplot(data=uses_month, aes(x=DATE, y=USES)) +
    geom_line() +
    geom_point() +
    geom_smooth() +
    ggtitle(lemma) +
    scale_y_continuous("Tweets / month") + 
    scale_x_date("") +
    geom_vline(xintercept=sub_first_start, linetype="longdash", color='purple') +
    geom_vline(xintercept=sub_first_cut, linetype="solid", color='purple') +
    annotate('text', x=sub_first_cut, y=500, label='first', size=4, angle=90) +
    geom_vline(xintercept=sub_first_end, linetype="longdash", color='purple') +
    geom_vline(xintercept=sub_mean_start, linetype="longdash", color='red') +
    geom_vline(xintercept=sub_mean_cut, linetype="solid", color='red') +
    annotate('text', x=sub_mean_cut, y=500, label='mean', size=4, angle=90) +
    geom_vline(xintercept=sub_mean_end, linetype="longdash", color='red') +
    geom_vline(xintercept=sub_max_start, linetype="longdash", color='blue') +
    geom_vline(xintercept=sub_max_cut, linetype="solid", color='blue') +
    annotate('text', x=sub_max_cut, y=500, label='max', size=4, angle=90) +
    geom_vline(xintercept=sub_max_end, linetype="longdash", color='blue') +
    geom_vline(xintercept=sub_last_start, linetype="longdash", color='green') +
    geom_vline(xintercept=sub_last_cut, linetype="solid", color='green') +
    annotate('text', x=sub_last_cut, y=500, label='last', size=4, angle=90) +
    geom_vline(xintercept=sub_last_end, linetype="longdash", color='green')
}

save_uses_plt <- function (uses_plt, lemma, dir_out='out/uses/') {
  fname <- paste0('ui_', lemma, '.pdf')
  ggsave(paste0(dir_out, fname), uses_plt, width=6, height=4)
}
