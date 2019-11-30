# variables ----
corpus <- "/Volumes/qjd/twint/"
lemma <- "poppygate"
cases <- c(
  'poppygate', 
  'alt-left'
  # 'alt-right', 
  # 'shareable'
)


# pipeline ----
proc_lemma <- function (lemma, corpus="/Volumes/qjd/twint/") {
  tweets <- load_data(corpus, lemma)
  print(paste(nrow(tweets), lemma))
}

for (lemma in cases) {
  proc_lemma(lemma)
}


# load data ----
source('src/load-data.R')
tweets <- load_data(corpus, lemma)


# post-processing ----
source('src/postproc.R')
tweets <- postproc(tweets)


# uses ----
source('src/uses.R')
uses = get_uses(tweets)
uses_tot <- get_uses_tot(uses)
age = get_age(uses)
coef_var <- get_coef_var(uses)
mean_date <- get_mean_date(uses)
max_date <- get_max_date(uses)
uses_month <- conv_uses_month(uses)
plt_ui <- plt_ui(uses_month)


# users ----
source('src/users.R')
users_month <- get_users_month(tweets)
users_tot <- get_users_tot(tweets)
plt_users <- plt_users(users_month)
