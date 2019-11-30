
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


# plot users
plt_users <- function (users_month) {
  ggplot(data=users_month, aes(x=MONTH, y=cumsum(USERS_MONTH))) +
    geom_line() +
    scale_y_continuous("unique speakers (cumulative)") +
    ggtitle(lemma)
}