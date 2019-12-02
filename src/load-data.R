library(stringr)
library(purrr)
library(readr)
library(dplyr)


get_path_dir <- function (corpus, lemma) {
  path_dir <- str_c(corpus, lemma)
  return(path_dir)
}

get_fpaths <- function (path_dir) {
  fpaths <- list.files(path_dir, full.names=TRUE, pattern = "\\.csv$")
  return(fpaths)
}

read_f <- function(fpath) {
  print(paste0('reading ', fpath))
  df <- read_csv(
    fpath, 
    col_types=cols_only(
      id = col_double(),
      # conversation_id = col_double(),
      # created_at = col_double(),
      date = col_date(format = ""),
      # time = col_time(format = ""),
      # timezone = col_character(),
      # user_id = col_double(),
      username = col_character(),
      # name = col_character(),
      # place = col_character(),
      tweet = col_character(),
      mentions = col_character()
      # urls = col_character(),
      # photos = col_character(),
      # replies_count = col_double(),
      # retweets_count = col_double(),
      # likes_count = col_double(),
      # location = col_logical(),
      # hashtags = col_character(),
      # link = col_character(),
      # retweet = col_logical()
      # quote_url = col_character(),
      # video = col_double()
    )
  ) %>%
    arrange(date)
  return(df)
}

read_fs <- function (fpaths) {
  df <- fpaths %>%
    map(~ read_f(.)) %>%
    reduce(rbind) %>%
    arrange(date)
  return(df)
}

load_data <- function (corpus, lemma) {
  path_dir <- get_path_dir(corpus, lemma)
  fpaths <- get_fpaths(path_dir)
  tweets <- read_fs(fpaths)
  return(tweets)
}
