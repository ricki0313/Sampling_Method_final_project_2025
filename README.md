# Replication Study: Mixed-Mode Survey Inference Methods

This repository contains a replication and partial simulation study of the paper:

> Yu, Elliott, and Raghunathan (2024).  
> *Three Approaches to Improve Inferences Based on Survey Data Collected with Mixed-Mode Designs*.  
> Journal of Survey Statistics and Methodology.

The project was completed as a final course assignment and focuses on reproducing the simulation results under selected settings, as well as documenting the methodology and implementation details.

---

## Project Overview

Mixed-mode survey designs are widely used to reduce cost and increase coverage, but they introduce **mode effects**, including:
- **Mode selection effects**
- **Mode measurement effects**

The original paper proposes three approaches to improve inference when mode effects may exist:
1. **Testimator**
2. **Bayesian approach**
3. **Model averaging**

This project:
- Summarizes the theoretical framework of the paper
- Reproduces part of the simulation study
- Documents implementation choices where the original paper lacks full details

## Execution Order
The files in this project are organized by stages and should be run in the following order:
- `simulation/data_generaion2.R`
- `simulation/summary.R`
- `simulation/naive_pooled.R`
- `simulation/naive_preferred.R`
- `simulation/testimator.R`
- `simulation/bayes.R`
- `simulation/model_averaging.R`

## Report
1. The note of the paper is summarized in `note.pdf`.
2. The replication of part simlation is summarized in `sim_reproduce_report.pdf`.
3. The briefing report of this study is summarized in `study_briefing.pdf`