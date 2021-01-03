# Social networks of lexical innovation
## Investigating the social dynamics of diffusion of neologisms on Twitter

Quirin Würschinger\
LMU Munich\
q.wuerschinger@lmu.de

This repository contains the code used for the paper _Social networks of lexical innovation_, submitted to _Frontiers in Artificial Intelligence_.

- The tweets were processed using `src/processing.R` which makes use of several other scripts in `src/` to
    - load the data (`load_data()`)
    - post-process the tweets (`postproc()`)
    - determine the `age` of neologisms (`get_age`)

