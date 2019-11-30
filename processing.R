# variables ----
corpus <- "/Volumes/qjd/twint/"
lemma <- "alt-left"
# cases <- c('poppygate', 'alt-left', 'alt-right', 'shareable')

# load data ----
source('src/load-data.R')
tweets <- load_data(corpus, lemma)
