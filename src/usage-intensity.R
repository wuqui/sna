library(lubridate)
library(plotly)
library(ggplot2)


get_uses <- function (tweets) {
  tweets %>%
    select(ID=id, DATE=date) %>%
    group_by(DATE) %>%
    summarize(USES = n())
}


get_uses_tot <- function (uses) {
  uses %>%
    summarise(sum(USES)) %>%
    pull()
}


get_age <- function (uses) {
  first_use = uses %>%
    head(DATE, n=1) %>%
    pull(DATE)
  last_use <- uses %>%
    tail(DATE, n=1) %>%
    pull(DATE)
  age <- last_use - first_use
  return(age)
}


get_coef_var <- function (uses) {
  uses %>%
    summarise(sd(USES) / mean(USES)) %>%
    pull()
}


# cut-offs ----
get_mean_date <- function (uses) {
  mean <- mean(uses$USES)
  mean_idx <- which(uses$USES >= mean)[1]
  mean_date <- uses[[mean_idx,"DATE"]]
  return(mean_date)
}

get_max_date <- function (uses) {
  max <- max(uses$USES)
  max_idx <- which(uses$USES >= max)[1]
  max_date <- uses[[max_idx,"DATE"]]
  return(max_date)
}


conv_uses_month <- function (uses) {
  uses %>%
    mutate(MONTH = as_date(cut(DATE, "month"))) %>%
    group_by(MONTH) %>%
    summarize(USES = sum(USES))
}


plt_ui <- function (uses_month) {
  ggplot(data=uses_month, aes(x=MONTH, y=USES)) +
    geom_line() +
    geom_point() +
    geom_smooth() +
    geom_vline(xintercept=as.numeric(as.Date(get_mean_date(uses))), linetype="longdash") +
    geom_vline(xintercept=as.numeric(as.Date(get_max_date(uses))), linetype="longdash") +
    ggtitle(lemma) +
    scale_y_continuous("Tweets / month")
}

