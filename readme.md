polynesian-canoe-analysis
============

Analysis script for "Inheritance, ecology and the evolution of the canoes of east Oceania", by Bret Beheim and Adrian V. Bell, in Proceedings of the Royal Society B, Volume 278, pp.3089-3095
DOI: 10.1098/rspb.2011.0060

Requirements:
- R (3.3.1 or greater) https://cran.r-project.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/

Instructions:

In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might say

```
    setwd('~/Desktop/polynesian-canoe-analysis')
```

You can tell if you are in the right place by typing in `dir()` and seeing this readme file. The analysis takes as input two data files:

'trait_data.csv' - presence/absence of canoe trait data by archipelago based on ethnographic and physical evidence described in Haddon and Hornell's "The Canoes of Oceania", recoded from Rogers & Ehrlich (2008).

'islanddata.csv' - archipelago-level properties used as predictors in the analysis models

The analysis itself runs by calling

```
    source('./canoe_hierarchical_model.r')
```

with the project folder as the working directory. 

The total time until completion will vary by machine.

The project is maintained by Bret Beheim (beheim@gmail.com) and is hosted at https://github.com/babeheim.