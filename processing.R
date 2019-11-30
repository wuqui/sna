# variables ----
corpus <- "/Volumes/qjd/twint/"
lemma <- "alt-left"
# cases <- c('poppygate', 'alt-left', 'alt-right', 'shareable')

# load data ----
source('src/load-data.R')
tweets <- load_data(corpus, lemma)

# post-processing ----
source('src/postproc.R')
tweets <- postproc(tweets)


# uses ----
source('src/usage-intensity.R')
uses = get_uses(tweets)
uses_tot <- get_uses_tot(uses)
age = get_age(uses)


coef_var <- get_coef_var(uses)

# cut-offs
mean_date <- get_mean_date(uses)
max_date <- get_max_date(uses)
