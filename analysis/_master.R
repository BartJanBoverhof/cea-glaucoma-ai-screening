################################################################################ 
# This script runs all the components of the DARTH framework using the         #
# Sick-Sicker model as testbed. It computes the cost-effectiveness analysis of #
# a hypothetical treatment for the simulated cohort of the Sick-Sicker         #
# state-transition model (STM)                                                 #
#                                                                              # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              # 
#     - Eline Krijkamp, MSc                                                    #
#     - Petros Pechlivanoglou, PhD                                             #
#     - Hawre Jalal, MD, PhD                                                   #
#     - Eva A. Enns, PhD                                                       #
################################################################################
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################
# rm(list = ls()) # to clean the workspace

#### 00 Install and load packages ####
### Uncomment if you don't have all required packages installed
# source("analysis/app0_package_setup.R", echo = TRUE) 

#### 01 Load inputs ####
source("analysis/01_model_inputs.R", echo = TRUE)

#### 02 Load simulation model and test it ####
source("analysis/02_decision_model.R", echo = TRUE)

#### 03 Calibrate simulation model ####
### Uncomment if you want to rerun the calibration component 
source("analysis/03_calibration.R", echo = TRUE)

#### 04 Validate simulation model ####
### Uncomment if you want to rerun the validation component
source("analysis/04_validation.R", echo = TRUE)

#### 05a Conduct probabilistic analysis ####
source("analysis/05a_probabilistic_analysis.R", echo = TRUE)

#### 05b Conduct deterministic analysis ####
source("analysis/05b_deterministic_analysis.R", echo = TRUE)

#### 05c Conduct value of information analysis ####
source("analysis/05c_value_of_information.R", echo = TRUE)

#### 02.1 Load packages and functions ####
#### 02.1.1 Load packages and functions ####
library(dplyr)    # For data manipulation
library(survival) # For plotting state-transition diagram

#### 02.1.2 Load inputs ####
l_params_all <- load_all_params(file.init = "data-raw/01_init_params.csv",
                                file.mort = "data-raw/01_all_cause_mortality.csv") # function in darthpack

#### 02.1.3 Load functions ####
# no functions required

#### 02.2 Run STM ####
### Create list of model output
l_out_stm <- decision_model(l_params_all = l_params_all)

### Plot Markov cohort trace
png("figs/02_trace_plot.png")
matplot(l_out_stm$m_M,
        xlab = "Cycle", ylab = "Proportion")
legend("right", legend = l_params_all$v_n, 
       pch = as.character(1:4), col = 1:4)
dev.off()

### Plot state-transition diagram
png("figs/02_model_diagram.png")
connect <- (l_out_stm$a_P[,,1] > 0)
survival::statefig(layout = c(2, 2), connect = connect )
dev.off()

