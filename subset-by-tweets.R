
# load requirements
source('src/load-data.R')
source('src/postproc.R')


# analysis ----

## load data
df <- read_csv2('subset-by-tweets.csv')


## list
df %>%
  mutate(diff = first - last) %>%
  arrange((full)) %>%
  View()


## plot: frequency vs. networks
plt <- df %>%
  filter(
    uses %in% (5000:2000000),
    !lemma %in% c('shareable', 'dotard')
    ) %>%
  ggplot(., aes(x=full, y=uses)) +
    geom_point() +
    geom_text(aes(label=lemma)) +
    scale_y_continuous(trans='log')
    # scale_x_continuous(trans='log')
    # geom_smooth(method=lm)

ggplotly(plt)

ggsave(plot=plt, width=6, height=4, file='~/Desktop/plt.pdf')

# ggsave('~/Desktop/net-vs-freq.pdf', plt)


# processing ----

## load data
lemma <- 'lituation'
tweets <- load_data(corpus, lemma)
tweets <- postproc(tweets)


## subsetting

# first
tweets_sub <- tweets %>%
  slice(1:1000)

# last
tweets_sub <- tweets %>%
  arrange(desc(date)) %>%
  slice(1:1000)

# full
tweets_sub <- tweets


## run processing
edges_sub <- extract_edges(tweets_sub)

sources_sub <- tweets_sub %>%
  select(username) %>%
  distinct(username)

targets_sub <- edges_sub %>%
  select(TO) %>%
  distinct(TO) %>%
  rename(username = TO)

nodes_sub <- bind_rows(sources_sub, targets_sub) %>%
  distinct(username)


# net <- tbl_graph(nodes=nodes_sub, edges=edges_sub, directed=TRUE)

net_igr <- graph_from_data_frame(edges_sub, vertices=nodes_sub, directed=directed)
E(net_igr)$WEIGHT <- 1
net_igr <- igraph::simplify(net_igr, edge.attr.comb=list(WEIGHT="sum", DATE="first"))
# create tidygraph network
net <- as_tbl_graph(net_igr, directed=directed) %>%
  activate(edges) %>%
  mutate(DATE=as_date(DATE))
# net <- add_node_info(net, directed=directed)
# net_plt <- plt_net(net, lemma, subset, net_window_dates, layout='kk')
# net_plt
  
ggraph(net, layout='kk') +
  geom_edge_link(aes(), show.legend=FALSE) +
  scale_edge_width(range = c(0.2, 1.5)) +
  geom_node_point(aes(), show.legend=FALSE) +
  # geom_node_text(aes(label=name, size=.3*STRENGTH, filter=STRENGTH > median(STRENGTH)), show.legend=FALSE, check_overlap=TRUE) +
  theme_graph()

centralization.degree(net, normalized=TRUE)$centralization # could use `mode=IGRAPH_IN` to only consider in-degree



