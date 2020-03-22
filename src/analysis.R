
read_df_comp <- function (f_path) {
  read_csv(f_path) %>%
    mutate(SUBSET = factor(SUBSET, levels=c('one', 'two', 'three', 'four', 'full')))
}


get_uses_tot_lemma <- function (df_comp, lemma) {
  df_comp %>%
  filter(LEMMA == lemma) %>%
  summarize(USES_TOT = max(USES_TOT)) %>%
  pull(USES_TOT)
}

