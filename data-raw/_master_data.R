rm(list = ls()) # to clean the workspace

# Loading packages
library(tidyverse)


## Code to prepare `all_cause_mortality` dataset
file.init <- "data-raw/01_all_cause_mortality.xlsx" #init statement


all_cause_mortality_wide  <- readxl::read_excel(file.init) %>% #loading csv
  gather(key = "Attribute", value = "Value", -Topic, -Age) %>%  # Spread Topic and Value into separate columns
  spread(key = Topic, value = Value)

  
# Create .rda object for initial set of parameters and store it in 'data' folder
save(all_cause_mortality_wide, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/01_all_cause_mortality.RData")

