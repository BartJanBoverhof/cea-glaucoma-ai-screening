# Glaucoma AI Screening Cost-Effectiveness Model

This repository contains the R code used in the publication ["The Cost-Effectiveness of an Artificial Intelligence-Based Population-Wide Screening Program for Primary Open-Angle Glaucoma in The Netherlands"](https://www.valueinhealthjournal.com/article/S1098-3015(25)02410-6/fulltext). The model evaluates an artificial intelligence (AI) based screening strategy versus standard of care for detecting glaucoma.

## Model overview

The analysis combines a decision tree with a Markov model. The decision tree captures the initial screening process where individuals may follow different diagnostic pathways depending on the AI test result and compliance. The Markov component simulates disease progression over time through health states ranging from no glaucoma to blindness and death. Health outcomes are expressed in quality-adjusted life years (QALYs) and costs are estimated from either a societal or healthcare perspective. Age-specific cohorts are simulated and aggregated to obtain overall results such as incremental cost-effectiveness ratios (ICERs).

## Repository structure

- `R/` – collection of functions used to run the model, calculate costs and utilities, perform sensitivity analyses and create visualisations.
- `run/` – scripts that execute the base-case analysis, deterministic sensitivity analysis, probabilistic sensitivity analysis and scenario analyses.
- `data/` and `data-raw/` – input parameter values and intermediate data sets.
- `figures/` and `tables/` – output created by the analysis.

To reproduce the results, open the desired script in the `run` folder and execute it in R. The main entry point is the `callModel` function located in `R/0_run.R`.
