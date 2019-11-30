
rem_dupls <- function (tweets) {
  tweets %>% distinct(id, .keep_all=TRUE)
  return(tweets)
}

rem_notoks <- function (tweets) {
  tweets <- tweets %>% filter(grepl(lemma, tweet))
  return(tweets)
}

postproc <- function(tweets) {
  tweets <- rem_dupls(tweets)
  tweets <- rem_notoks(tweets)
  return(tweets)
}