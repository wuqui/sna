# Social networks of lexical innovation
## Investigating the social dynamics of diffusion of neologisms on Twitter

Quirin Würschinger\
LMU Munich\
q.wuerschinger@lmu.de

This repository contains the code used for the paper _Social networks of lexical innovation_, submitted to _Frontiers in Artificial Intelligence_.

# Results

The results presented in the paper can be reproduced from the code in [this notebook](results.md).


# Processing

The results are based on the processing of all tweets using `src/processing.R`. This script performs the full analysis for each neologism in the sample, making use of several other scripts contained in `src/` to ...

- load the tweets: `load_data()`
- post-process the tweets: `postproc()`
- perform the frequency-based analyses
    - determine the `age` of neologisms: `get_age()`
    - calculate their `volatility` using the coefficient of variation: `get_coef_var()`
    - bin tweets in monthly intervals: `bin_uses()`
    - plot their usage frequency: `plt_uses`
- perform the social network analysis
    - determine diffusion offset: `get_diff_start()` 
    - determine subset boundaries: `get_slice_time()`
    - for each subset
        - extract edges: `extract_edges()`
        - extract nodes:
            - sources: `get_sources()` 
            - targets: `get_targets()` 
            - list of all nodes: `get_nodes()`
        - create the network: `create_net()`
        - add node information: `add_node_info()`
        - extract network metrics: `get_net_metrics()`
        - export edges: `net_to_gephi()` 
        - save network date in `out/df_comp.csv`

