library(stringr)

rem_dupls <- function (tweets) {
  tweets %<>% distinct(id, .keep_all=TRUE)
}


rem_handles <- function(tweets) {
  tweets %<>%
    mutate(tweet = str_replace_all(tweet, paste0('@', lemma), ''))
}


rem_notoks <- function (tweets) {
  tweets <- rem_handles(tweets)
  tweets %<>% filter(grepl(lemma, tweet))
  return(tweets)
}


postproc <- function(tweets) {
  tweets <- rem_dupls(tweets)
  tweets <- rem_notoks(tweets)
  return(tweets)
}


filter_tweets <- function (tweets) {
  tweets <- tweets %>%
    filter(date <= '2018-12-31')
  return(tweets)
}