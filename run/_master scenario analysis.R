#------------------------------------------------------------------------------#
####                       0 Prerequisites                           ####
#------------------------------------------------------------------------------#
rm(list = ls()) # to clean the workspace
set.seed(3791) # set seed for reproducibility

# loading packages
#p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub
library(stringr)

# importing Functions
source("R/0_run.R", echo = TRUE) #Load general functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions
source("R/4_costs_and_utilities_functions.R", echo = TRUE) #Load utility functions
source("R/5_sensitivity_and_scenario.R", echo = TRUE) #Load visualization functions
source("R/6_visualisation_functions.R", echo = TRUE) #Load visualization functions

# loading all required data objects
load("data/1_df_mortality.RData")
load("data/2_p_dt.RData")
load("data/3a_p_severity_undiagnosed.RData")
load("data/4_p_transition.RData")
load("data/5a_v_utilities.RData")
load("data/5b_v_utilities_gp.RData")
load("data/6a_v_incidences_of.RData")
load("data/6b_v_incidences_screening.RData")
load("data/6c_v_prevalence.RData")
load("data/7a_df_utilisation_medicine.RData")
load("data/8a_v_cost_dt.RData")
load("data/8b_v_cost_medicine.RData")
load("data/8c_v_cost_utilisation_diagnostics.RData")
load("data/8d_v_cost_utilisation_intervention.RData")
load("data/8f_v_cost_burden_disease.RData")

#------------------------------------------------------------------------------#
####                       1 Define Cohorts                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # age categories to be screened
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

#------------------------------------------------------------------------------#
####                       2 Run model                           ####
#------------------------------------------------------------------------------#
# varying parameters
p_dt <- p_dt
p_severity_undiagnosed <- p_severity_undiagnosed
p_transition <- p_transition
v_utilities <- v_utilities
v_utilities_gp<- v_utilities_gp
screening_interval <- 5

# run base case
strategy <- "base"
base <- callModel(descriptives = TRUE, perspective= "societal")

# run base case
strategy <- "scenario"
icer_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE)
icer_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE)
icer_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE)
icer_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE)
icer_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE)
icer_ai_performance <- runScenario(vary = "ai_performance", perspective = "societal", descriptives = TRUE)
icer_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE)
icer_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE)

library(ggplot2)
library(dplyr)

# Example icer_... lists structure
# icer_screening_age <- list(screening_age_1 = 100, screening_age_2 = 200)
# icer_screening_interval <- list(screening_interval_1 = 150, screening_interval_2 = 250)

# Combine all icer_... lists into a list of lists for easier processing
icer_lists <- list(
    expert_observation = icer_expert_observation,
    expert_utilisation = icer_expert_utilisation,
    ai_performance = icer_ai_performance,
    compliance = icer_compliance,
    utilities = icer_utilities,
    transition = icer_transition,
    screening_interval = icer_screening_interval,
    screening_age = icer_screening_age,
    base = base
)

# Initialize an empty data frame to store ICERs and their types
icer_data <- data.frame(ICER = numeric(), Type = character(), stringsAsFactors = FALSE)

# Loop through each list to extract ICERs and their corresponding types
for (type in names(icer_lists)) {
  for (scenario in names(icer_lists[[type]])) {
    icer_value <- icer_lists[[type]][[scenario]]
    icer_data <- rbind(icer_data, data.frame(ICER = icer_value, Type = paste(type, scenario, sep = "_")))
  }
}

# Modify the Type assignment to ensure all ICERs of the same category share the same y-coordinate
icer_data$Category <- factor(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2), levels = unique(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2)))


# Add "base" ICER to the data frame
icer_data <- rbind(icer_data, data.frame(ICER = base, Type = "base", Category = "base"))

# Assuming 'base' and 'icer_data' are already defined

# Example of adding "Shape" and "Color" columns to the dataframe
# This is a simplified example. You'll need to assign unique shapes and colors based on your actual data
icer_data$Shape <- c(24,21, 24,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
icer_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF",
"#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

# Ensure shapes can be filled (using shapes 21-25)
# If your data already uses shapes 21-25, this step is correct. Otherwise, adjust your shape assignments accordingly.


icer_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%","Compliance 100%", "Compliance literature","Uilities HUI-3", "Utilities Burr (2017)","Untreated transitions Burr (2014)", "Treated transitions Chauhan (2014)", "Transitions Garway (2015)","Screening interval every 3 years", "Screening interval everey 10 years", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

# Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
# Load necessary libraries
library(ggplot2)

# Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
plot <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0.5), size = 7, stroke = 2, color = "black", alpha = 0.8) +
    scale_color_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_fill_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(icer_data$Shape), icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    labs(
        title = "",
        x = "Incremental Cost-Effectiveness Ratio (ICER)",
        y = "",

        shape = "Scenario",
        fill = "Scenario"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.position = "left",
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),

        panel.grid.major = element_line(color = "#adadad"),  
        panel.grid.minor = element_line(color = "#adadad"),
        panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base, linetype = "dashed", color = "black", size = 1.5) +
    scale_x_continuous(breaks = c(10000, 20000, 30000, 40000, 50000, 60000))

# Save plot to figures folder
ggsave("figures/icer_plot.png", plot = plot, width = 9, height = 7)












