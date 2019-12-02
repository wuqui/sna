
# get unique users
get_users_month <- function (tweets) {
  tweets %>%
    select(USER=username, DATE=date) %>%
    # reduce to unique users
    distinct(USER, .keep_all=TRUE) %>%
    mutate(MONTH = as_date(cut(DATE, "month"))) %>%
    group_by(MONTH) %>%
    summarize(USERS_MONTH=n())
}


# total number of unique users that have used the word
get_users_tot <- function(tweets) {
  users_month %>%
    summarise(sum(USERS_MONTH)) %>%
    pull()
}


plt_users <- function (users_month) {
  ggplot(data=users_month, aes(x=MONTH, y=cumsum(USERS_MONTH))) +
    geom_line() +
    scale_y_continuous("unique speakers (cumulative)") +
    scale_x_date('') +
    ggtitle(lemma)
}


save_users_plt <- function (users_plt, lemma, dir_out='out/users/') {
  fname <- paste0('uu_', lemma, '.pdf')
  ggsave(paste0(dir_out, fname), users_plt)
}
