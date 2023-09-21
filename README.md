Running To Do List
=============
**Setting up data**
- [x] Population cohort input values (Statline)
- [X] Incidence of Glaucoma
- [X] Decision tree probabilities (AI performance, ophthalmologist performance, etc)
- [ ] Revisit data
- [ ] Markov model probabilities (transition probabilities)
- [ ] Utilities
- [ ] Costs
- [ ] Utility decrement due to age

**Setting up model**
- [X] Decision tree
- [X] Cohort pipelines
- [X] Markov model first version (base-case)

Preliminaries
=============

-   Install [RStudio](https://www.rstudio.com/products/rstudio/download/)
-   Install `devtools` to install `darthpack` as a package and modify it to generate your own package

``` r
# Install release version from CRAN
install.packages("devtools")

# Or install development version from GitHub
# devtools::install_github("r-lib/devtools")
```

Template obtained from Darthpack <img src='man/figures/logo.png' align="right" height="139" />
=======================================================================
[![DOI](https://zenodo.org/badge/197059951.svg)](https://zenodo.org/badge/latestdoi/197059951)

<!-- <img src="docs/figs/under_const.jpeg" align="center" alt="" width="360" /> -->
[`darthpack`](https://github.com/DARTH-git/darthpack) is an R package that showcases the [Decision Analysis in R for Technologies in Health (DARTH)](https://darthworkgroup.com) coding framework to construct model-based cost-effectiveness analysis in R. The main website of `darthpack` can be [found here](https://darth-git.github.io/darthpack/).

[`darthpack`](https://github.com/DARTH-git/darthpack) is part of the following manuscript:

-   Alarid-Escudero F, Krijkamp E, Pechlivanoglou P, Jalal H, Kao SY, Yang A, Enns EA. ["A need for change! A coding framework for improving transparency in decision modeling"](https://link.springer.com/article/10.1007%2Fs40273-019-00837-x). PharmacoEconomics 2019;37(11):1329–1339. <http://dx.doi.org/10.1007/s40273-019-00837-x>

Relevant Literature
=============
- [Burr (2006)](https://researchonline.lshtm.ac.uk/id/eprint/8383/) The clinical effectiveness and cost-effectiveness of screening for open angle glaucoma: a systematic review and economic evaluation
- [Lemij (2023)](https://www.ophthalmologyscience.org/article/S2666-9145(23)00032-5/fulltext)Characteristics of a Large, Labeled Data Set for the Training of Artificial Intelligence for Glaucoma Screening with Fundus Photographs
- [de Vente (2023)](https://arxiv.org/pdf/2302.01738.pdf)AIROGS: ARTIFICIAL INTELLIGENCE FOR ROBUST GLAUCOMA SCREENING CHALLENGE
