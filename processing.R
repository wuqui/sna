# variables ----
corpus <- "/Volumes/qjd/twint/"
lemma <- "bromance"
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
mean_date <- get_mean_date(uses)
max_date <- get_max_date(uses)
uses_month <- conv_uses_month(uses)


ggplot(data=uses, aes(x=MONTH, y=USES)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept=as.numeric(as.Date(get_mean_date(uses))), linetype="longdash") +
  geom_vline(xintercept=as.numeric(as.Date(get_max_date(uses))), linetype="longdash") +
  ggtitle(lemma) +
  scale_y_continuous("Tweets / month") +
  scale_x_date(
    "",
    # limits=c(as.Date("2016-01-01"), as.Date("2018-12-30"))
  )
