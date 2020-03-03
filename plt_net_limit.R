
df_comp %>%
  filter(SUBSET!='full') %>%
  select(LEMMA, SUBSET, EDGES) %>%
  arrange(desc(EDGES))

edges_limit <- 10000
edges_sub <- edges %>% slice(1:edges_limit)

layout <- 'kk'

# create net
net_igr <- graph_from_data_frame(edges_sub, directed=directed)
E(net_igr)$WEIGHT <- 1
net_igr <- igraph::simplify(net_igr, edge.attr.comb=list(WEIGHT="sum", DATE="first"))
# create tidygraph network
net <- as_tbl_graph(net_igr, directed=directed) %>%
  activate(edges) %>%
  mutate(DATE=as_date(DATE))

# add node info
net <- net %>%
  activate(nodes) %>%
  mutate(STRENGTH = strength(net, mode="in")) %>%
  mutate(COMMUNITY = as.factor(group_edge_betweenness(directed=directed)))
# mutate(CENT_DEGREE = centrality_degree(weights=WEIGHT)) %>%
# mutate(CENT_PAGERANK = centrality_pagerank(weights=WEIGHT)) %>%
# mutate(GROUP_LOUVAIN = as.factor(group_louvain(weights=WEIGHT)))

# plot net
ggraph(net, layout=layout) +
  geom_edge_link(aes(width=WEIGHT), show.legend=FALSE) +
  scale_edge_width(range = c(0.2, 1.5)) +
  geom_node_point(aes(size=STRENGTH, colour=COMMUNITY), show.legend=FALSE) +
  geom_node_text(aes(label=name, size=.3*STRENGTH, filter=STRENGTH > median(STRENGTH)), show.legend=FALSE, check_overlap=TRUE) +
  theme_graph() +
  ggtitle(
    label=lemma,
    subtitle=paste0("subset: ", sub[['sub']], " (", sub[['start']], " -- ", sub[['end']], ")")
  )

E(net)
