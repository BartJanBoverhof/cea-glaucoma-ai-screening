scenarioPlot <- function(){
  # run base case
  strategy <- "base"
  base_icer <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "icer")
  base_qaly <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "qaly")
  base_costs <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "costs")

  # run base case
  strategy <- "scenario"
  icer_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = FALSE, output = "icer")
  icer_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = FALSE, output = "icer")
  icer_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_vi_costs <- runScenario(vary = "vi_costs", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_prevalence <- runScenario(vary = "prevalence", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_discounting <- runScenario(vary = "discount", perspective = "societal", descriptives = TRUE, output = "icer")
    
  qaly_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = FALSE, output = "qaly")
  qaly_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = FALSE, output = "qaly")
  qaly_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_vi_costs <- runScenario(vary = "vi_costs", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_prevalence <- runScenario(vary = "prevalence", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_discounting <- runScenario(vary = "discount", perspective = "societal", descriptives = TRUE, output = "qaly")

  costs_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = FALSE, output = "costs")
  costs_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = FALSE, output = "costs")
  costs_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_vi_costs <- runScenario(vary = "vi_costs", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_prevalence <- runScenario(vary = "prevalence", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_discounting <- runScenario(vary = "discount", perspective = "societal", descriptives = TRUE, output = "costs")

  # Combine all icer_... lists into a list of lists for easier processing
  icer_lists <- list(
      discounting = icer_discounting,
      expert_observation = icer_expert_observation,
      expert_utilisation = icer_expert_utilisation,
      ai_sensitivity = icer_sensitivity,
      ai_specificity = icer_specificity,
      compliance = icer_compliance,
      prevalence = icer_prevalence,
      vi_costs = icer_vi_costs,
      utilities = icer_utilities,
      transition = icer_transition,
      screening_interval = icer_screening_interval,
      screening_age = icer_screening_age,
      base_icer = base_icer
  )

  qaly_lists <- list(
      discounting = qaly_discounting,
      expert_observation = qaly_expert_observation,
      expert_utilisation = qaly_expert_utilisation,
      ai_sensitivity = qaly_sensitivity,
      ai_specificity = qaly_specificity,
      compliance = qaly_compliance,
      prevalence = qaly_prevalence,
      vi_costs = qaly_vi_costs,
      utilities = qaly_utilities,
      transition = qaly_transition,
      screening_interval = qaly_screening_interval,
      screening_age = qaly_screening_age,
      base_qaly = base_qaly
  ) 

  costs_lists <- list(
      discounting = costs_discounting,
      expert_observation = costs_expert_observation,
      expert_utilisation = costs_expert_utilisation,
      ai_sensitivity = costs_sensitivity,
      ai_specificity = costs_specificity,
      compliance = costs_compliance,
      prevalence = costs_prevalence,
      vi_costs = costs_vi_costs,
      utilities = costs_utilities,
      transition = costs_transition,
      screening_interval = costs_screening_interval,
      screening_age = costs_screening_age,
      base_costs = base_costs
  )

  # Initialize an empty data frame to store ICERs and their types
  icer_data <- data.frame(ICER = numeric(), Type = character(), stringsAsFactors = FALSE)
  qaly_data <- data.frame(QALY = numeric(), Type = character(), stringsAsFactors = FALSE)
  costs_data <- data.frame(Costs = numeric(), Type = character(), stringsAsFactors = FALSE)

  # Loop through each list to extract ICERs and their corresponding types
  for (type in names(icer_lists)) {
    for (scenario in names(icer_lists[[type]])) {
      icer_value <- icer_lists[[type]][[scenario]]
      icer_data <- rbind(icer_data, data.frame(ICER = icer_value, Type = paste(type, scenario, sep = "_")))
    }
  }

  for (type in names(qaly_lists)) {
    for (scenario in names(qaly_lists[[type]])) {
      qaly_value <- qaly_lists[[type]][[scenario]]
      qaly_data <- rbind(qaly_data, data.frame(QALY = qaly_value, Type = paste(type, scenario, sep = "_")))
    }
  }

  for (type in names(costs_lists)) {
    for (scenario in names(costs_lists[[type]])) {
      costs_value <- costs_lists[[type]][[scenario]]
      costs_data <- rbind(costs_data, data.frame(Costs = costs_value, Type = paste(type, scenario, sep = "_")))
    }
  }

  # Modify the Type assignment to ensure all ICERs of the same category share the same y-coordinate
  icer_data$Category <- factor(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2), levels = unique(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2)))
  qaly_data$Category <- factor(substr(qaly_data$Type, 1, nchar(qaly_data$Type) - 2), levels = unique(substr(qaly_data$Type, 1, nchar(qaly_data$Type) - 2)))
  costs_data$Category <- factor(substr(costs_data$Type, 1, nchar(costs_data$Type) - 2), levels = unique(substr(costs_data$Type, 1, nchar(costs_data$Type) - 2)))

  # Add "base" ICER to the data frame
  icer_data <- rbind(icer_data, data.frame(ICER = base_icer, Type = "base", Category = "base"))
  qaly_data <- rbind(qaly_data, data.frame(QALY = base_qaly, Type = "base", Category = "base"))
  costs_data <- rbind(costs_data, data.frame(Costs = base_costs, Type = "base", Category = "base"))


  # Example of adding "Shape" and "Color" columns to the dataframe
  # This is a simplified example. You'll need to assign unique shapes and colors based on your actual data
  icer_data$Shape <- c(21, 24,21, 24,21, 24,22,21, 24,22,21, 24,21, 21 ,21,24 ,24,22 ,21,24,22, 21,24, 21,23,24,22, 21)
  icer_data$Color <- c("#2f2f2f","#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", "#964B00",
  "#FFFF00","#FFFF00","#257a25","#828231","#828231","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#A9A9A9", "#A9A9A9","#A9A9A9","#A9A9A9","#FF0000") 

  qaly_data$Shape <- c(21, 24,21, 24,21, 24,22,21, 24,22,21, 24,21, 21 ,21,24 ,24,22 ,21,24,22, 21,24, 21,23,24,22, 21)
  qaly_data$Color <- c("#2f2f2f","#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", "#964B00",
  "#FFFF00","#FFFF00","#257a25","#828231","#828231","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#A9A9A9", "#A9A9A9","#A9A9A9","#A9A9A9","#FF0000") 

  costs_data$Shape <- c(21, 24,21, 24,21, 24,22,21, 24,22,21, 24,21, 21 ,21,24 ,24,22 ,21,24,22, 21,24, 21,23,24,22, 21)
  costs_data$Color <- c("#2f2f2f","#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", "#964B00",
  "#FFFF00","#FFFF00","#257a25","#828231","#828231","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#A9A9A9", "#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")  
  # Ensure shapes can be filled (using shapes 21-25)
  # If your data already uses shapes 21-25, this step is correct. Otherwise, adjust your shape assignments accordingly.
  icer_data$Identifier <- c("No discounting","Min p(no glaucoma <-> observation)", "Max p(no glaucoma <-> observation)","Min resource utilisation", "Max resource utilisation","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","AI specificity 75%","Compliance 100%", "Compliance from literature","Prevalence from E3 study","Costs due to visual impairment: severe" ,"Costs due to visual impairment: blindess" ,"Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transition probabilities Burr (2014)", "Transitions probabilities Chauhan (2014)", "Transitions probabilities Garway-Heath (2015)","Screening interval every 3 years", "Screening interval every 7 years","Screening age 50-85","Screening age 60-75", "Screening age 60-70", "Screening age 50-65", "Base case")

  qaly_data$Identifier <- c("No discounting","Min p(no glaucoma <-> observation)", "Max p(no glaucoma <-> observation)","Min resource utilisation", "Max resource utilisation","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","AI specificity 75%","Compliance 100%", "Compliance from literature","Prevalence from E3 study","Costs due to visual impairment: severe" ,"Costs due to visual impairment: blindess" ,"Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transition probabilities Burr (2014)", "Transitions probabilities Chauhan (2014)", "Transitions probabilities Garway-Heath (2015)","Screening interval every 3 years", "Screening interval every 7 years","Screening age 50-85","Screening age 60-75", "Screening age 60-70", "Screening age 50-65", "Base case")

  costs_data$Identifier <- c("No discounting","Min p(no glaucoma <-> observation)", "Max p(no glaucoma <-> observation)","Min resource utilisation", "Max resource utilisation","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","AI specificity 75%","Compliance 100%", "Compliance from literature","Prevalence from E3 study","Costs due to visual impairment: severe" ,"Costs due to visual impairment: blindess" ,"Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transition probabilities Burr (2014)", "Transitions probabilities Chauhan (2014)", "Transitions probabilities Garway-Heath (2015)","Screening interval every 3 years", "Screening interval every 7 years","Screening age 50-85","Screening age 60-75", "Screening age 60-70", "Screening age 50-65", "Base case")

  # Custom function to format the ICER without extra spaces
  format_icer <- function(icer) {
    formatted_icer <- formatC(icer, format = "f", digits = 0, big.mark = ",")
    formatted_icer <- paste0("€", formatted_icer)
    return(formatted_icer)
  }

  # Apply this formatting to the ICER column in your data
  icer_data$Identifier <- paste0(icer_data$Identifier, " (", format_icer(icer_data$ICER), ")")

  # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
  euro_formatter <- function(x) {
    paste0("€", format(x, big.mark = ".", scientific = FALSE))
  }

    # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
    legend_plot <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
      geom_point(position = position_dodge(width = 0), size = 6, stroke = 1, color = "black", alpha = 1) +
      scale_color_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
      scale_fill_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
      scale_shape_manual(values = setNames(as.numeric(icer_data$Shape), icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
      labs(
        title = "",
        x = "",
        y = "",
        shape = "Scenario (ICER)",
        fill = "Scenario (ICER)"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "left",
        legend.title = element_text(size = 20, hjust = 0, face = "bold"),
        legend.text = element_text(size = 13, hjust = 0),  # Use element_text instead of element_markdown to avoid errors
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.95, "cm"),  # Adjust the size if needed
        legend.box.just = "left",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        panel.grid.major = element_line(color = "#adadad"),  
        panel.grid.minor = element_line(color = "#adadad"),
        panel.background = element_rect(fill = "lightgray")
      ) +
      guides(
        color = guide_legend(ncol = 2),  # Ensure legend is in one column
        fill = guide_legend(ncol = 2),
        shape = guide_legend(ncol = 2)
      ) +
      geom_vline(xintercept = base_icer, linetype = "solid", color = "black", size = 1) +
      scale_x_continuous(
        labels = euro_formatter
      ) +
      geom_point(data = icer_data, aes(x = ICER, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)



    # Create the CE Plane Plot combining Incremental QALYs and Incremental Costs
    ce_plane_plot <- ggplot(icer_data, aes(x = qaly_data$QALY, y = costs_data$Costs, color = Identifier, shape = Identifier, fill = Identifier)) +
      geom_abline(slope = 20000/1, intercept = 0, linetype = "solid", color = "#012591", size = 1) +  # Add ICER line
      geom_abline(slope = 50000/1, intercept = 0, linetype = "solid", color = "#006b0c", size = 1) +  # Add ICER line
      geom_point(size = 5.5, stroke = 1, color = "black", alpha = 0.9) +
      scale_color_manual(values = setNames(icer_data$Color, icer_data$Identifier)) +
      scale_fill_manual(values = setNames(icer_data$Color, icer_data$Identifier)) +
      scale_shape_manual(values = setNames(as.numeric(icer_data$Shape), icer_data$Identifier)) +
      labs(
      title = "Scenario CE-plane",  # Add title to the plot
      x = "Incremental QALYs",
      y = "Incremental Costs (€)",
      color = "Scenario",
      shape = "Scenario",
      fill = "Scenario"
      ) +
      theme_minimal(base_size = 15) +
      theme(
      legend.position = "none",
      axis.title.x = element_text(size = 19),
      axis.title.y = element_text(size = 19),
      axis.text.x = element_text(size = 19),
      axis.text.y = element_text(size = 19),
      panel.grid.major = element_line(color = "#adadad"),
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray"),
      plot.margin = margin(r = 20),  # Adjust the margins here (t = top, r = right, b = bottom, l = left)
      plot.title = element_text(size = 20, face = "bold")
      ) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
      scale_x_continuous(
      minor_breaks = seq(0, 0.04, by = 0.005)
      ) +
      scale_y_continuous(labels = euro_formatter) +
      annotate("label", x = 0.025, y = 500, label = paste("WTP €20,000"), 
        size = 5, color = "White", fill = "#012591", fontface = "bold", alpha =1) +
      annotate("label", x = 0.01, y = 500, label = paste("WTP €50,000"), 
        size = 5, color = "White", fill = "#006b0c", fontface = "bold", alpha =1) 

  g <- ggplotGrob(legend_plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]

  # Arrange the plots in the desired layout
  plot <- grid.arrange(
    legend,ce_plane_plot,  # Legend on top, CE-plane plot below
    ncol = 1,  # Ensure it's one column layout
    widths = c(1),  # Equal width for both legend and plot
    heights = c(1, 1)  # Legend takes 30% of the height, CE-plane takes the rest
  )
  # change the number of columns in the legend
  guides(
      color = guide_legend(ncol = 1),  # Try changing to 1 column instead of 2
      fill = guide_legend(ncol = 1),
      shape = guide_legend(ncol = 1)
  )

  # Save plot to figures folder
  ggsave("figures/scenario.png", plot = plot, width = 10, height = 13)
}
    
tornadoPlot <- function(Parms, Outcomes_se, Outcomes_20, titleName, outcomeName_se, outcomeName_20) {
  library(ggplot2)
  library(reshape2)
  library(scales)
  library(gridExtra)
  
  create_tornado <- function(Outcomes, outcomeName) {
    # Grouped Bar Plot
    paramNames2 <- Parms
    
    # Compute relevant values for the tornado plot
    ymean <- Outcomes[1,1]
    yMin <- Outcomes[,2] - ymean
    yMax <- Outcomes[,3] - ymean
    ySize <- abs(yMax - yMin)
    
    rankY <- order(ySize)
    nParams <- length(paramNames2)
    
    Tor <- data.frame(
      Parameter=c(paramNames2[rankY],paramNames2[rankY]),  
      Level=c(rep("Lower bound",nParams),rep("Upper bound",nParams)),
      value=ymean+c(yMin[rankY],yMax[rankY]),
      sort=seq(1,nParams)
    )
    
    Tor$Parameter2 <- ordered(Tor$Parameter, Tor$Parameter[1:(length(Tor$Parameter)/2)])
    
    offset_trans <- function(offset=0) {
      trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
    }
    
    # Custom label function for euro sign and thousand separator
    euro_format <- function(x) {
      paste0("€", format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    }
    
    # Plot the Tornado diagram.
    txtsize <- 16
    p <- ggplot(Tor, aes(x=Parameter2, y=value, fill=Level)) + 
      geom_bar(data=Tor[Tor$Level=="Lower bound",], aes(x=Parameter2, y=value, fill="Lower bound"), 
               stat="identity", alpha=0.8, stroke=2, colour="black") +
      geom_bar(data=Tor[Tor$Level=="Upper bound",], aes(x=Parameter2, y=value, fill="Upper bound"), 
               stat="identity", alpha=0.8, stroke=2, colour="black") +
      ggtitle("", subtitle=outcomeName) +
      scale_fill_manual(name="Parameter Level", values=c("Lower bound"="blue", "Upper bound"="red")) +
      scale_y_continuous(name="ICER", trans=offset_trans(offset=ymean), 
                         labels=function(x) ifelse(x == ymean, paste(euro_format(x), " (ymean)", sep=""), euro_format(x))) +
      scale_x_discrete(name="Parameter") +
      geom_hline(yintercept=ymean, linetype="dotted", size=0.5) +
      theme_bw(base_size=14) +
      coord_flip() +
      theme_minimal(base_size=15) +
      theme(legend.position=c(0.95, 0.05),
            legend.justification=c(1, 0),
            legend.box.just="right",
            legend.margin=margin(),
            legend.box="vertical",
            legend.title=element_text(size=txtsize, angle=0, hjust=1),
            legend.key=element_rect(colour="black"),
            legend.text=element_text(size=txtsize),
            title=element_text(face="bold", size=15),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=txtsize),
            axis.text.y=element_text(size=txtsize),
            axis.text.x=element_text(size=txtsize, angle=45, hjust=1),
            axis.ticks.y=element_blank(),
            panel.grid.major=element_line(color="#adadad"),  
            panel.grid.minor=element_line(color="#adadad"),
            panel.background=element_rect(fill="lightgray"))
    
    return(p)
  }
  
  # Create tornado plots for both Outcomes_se and Outcomes_20
  plot_se <- create_tornado(Outcomes_se, outcomeName_se)
  plot_20 <- create_tornado(Outcomes_20, outcomeName_20)
  
  # Arrange plots in a grid
  combined_plot <- grid.arrange(plot_se, plot_20, ncol=2)
  
  # Save the plot
  ggsave(filename="figures/tornado_plot.png", plot=combined_plot, width=16, height=8)
  
  # Display the plot
  print(combined_plot)
}



psaPlot <- function(out){

  library(scales)       
  results_df <- data.frame(Simulation = integer(), Incremental_QALY = numeric(), Incremental_Costs = numeric())

  # Loop through the `out` list to populate the dataframe
  for (i in 1:length(out)) {
    # Extract incremental QALYs and Costs for each simulation
    incremental_qaly <- out[[i]][[1]]
    incremental_costs <- out[[i]][[2]]
    
    # Append this information to the dataframe
    results_df <- rbind(results_df, data.frame(Simulation = i, Incremental_QALY = incremental_qaly, Incremental_Costs = incremental_costs))
  }

  PSA_icer <- mean(results_df$Incremental_Costs) / mean(results_df$Incremental_QALY)

  # Assuming 'results_df' contains the data with columns 'Incremental_Costs' and 'Incremental_QALY'
  # and you want to differentiate points by some identifier, for example 'Scenario'

  # Custom euro formatter
  euro_formatter <- function(x) {
    paste0("€", format(x, big.mark = ",", scientific = FALSE))
  }

  # Calculate the percentage of cost-effective samples
  cost_effective_samples_20000 <- sum(results_df$Incremental_Costs / results_df$Incremental_QALY <= 20000) / nrow(results_df) * 100
  cost_effective_samples_50000 <- sum(results_df$Incremental_Costs / results_df$Incremental_QALY <= 50000) / nrow(results_df) * 100

  # obtain base-case ICER 
  base <- callModel(output = "qaly_costs", descriptives = FALSE)
  base_qaly <- base[[1]][[1]]
  base_costs <- base[[1]][[2]]

  # Enhanced Cost-Effectiveness Plane
  ce_plane <- ggplot(results_df, aes(x = Incremental_QALY, y = Incremental_Costs)) +
    geom_point(size = 2, alpha = 0.5) +  # Add points with size and transparency
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add a horizontal line at y=0
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add a vertical line at x=0
    geom_abline(slope = 20000/1, intercept = 0, linetype = "solid", color = "#03217d", size = 1) +  # Add ICER line
    geom_abline(slope = 50000/1, intercept = 0, linetype = "solid", color = "#006b0c", size = 1) +  # Add ICER line
    geom_point(aes(x = base_qaly, y = base_costs, alpha = 0.5), color = "white", size = 2) + # Add base case point
    labs(
      title = "",
      y = "Incremental costs",
      x = "Incremental QALYs",
      shape = "Scenario",
      fill = "Scenario"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",  # Place legend on the right
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 17),
      axis.text.x = element_text(size = 17),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    scale_y_continuous(labels = euro_formatter) +
    annotate("label", x = 0.014, y = -250, label = paste("Cost-effective at €20,000 treshold:", round(cost_effective_samples_20000, 1), "%"), 
            size = 5, color = "White", fill = "#012591", fontface = "bold", alpha =0.7) +
    annotate("label", x = 0.014, y = -180, label = paste("Cost-effective at €50,000 treshold:", round(cost_effective_samples_50000, 1), "%"), 
            size = 5, color = "White", fill = "#006b0c", fontface = "bold", alpha =0.7) 
    #annotate("label", x = 0.025, y = 500, label = "WTP treshold at €20,000", 
    #        size = 5, color = "White", fill = "#012591", fontface = "bold", alpha =0.8) +
    #annotate("label", x = 0.002, y = 500, label = "WTP treshold at €50,000", 
    #        size = 5, color = "White", fill = "#006b0c", fontface = "bold", alpha =0.8) 

  ggsave("figures/ce_plane.png", plot = ce_plane, width = 7, height = 6)

  # Define WTP range
  results_df <- results_df %>% mutate(ICER = Incremental_Costs / Incremental_QALY)

  wtp_values <- seq(0, 100000, length.out = 10000)
  df_ceac <- data.frame(wtp_values, percentage_ce = NA)

  for (i in 1:nrow(df_ceac)){
      df_ceac$percentage_ce[i] <- mean(ifelse(results_df$ICER < df_ceac$wtp_values[i], 1, 0))  
  }

  # Plot the cost acceptability curve
  ceac <- ggplot(df_ceac, aes(x = wtp_values, y = percentage_ce)) +
    geom_line(size = 1.5, color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
    labs(
      title = "",
      x = "Willingness-to-Pay (WTP) threshold",
      y = "Percentage cost-Effective"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "right",  # Place legend on the right
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 17),
      axis.text.x = element_text(size = 17),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    scale_x_continuous(breaks = seq(0, 100000, by = 20000), minor_breaks = seq(0, 100000, by = 10000),labels = euro_formatter) +
    scale_y_continuous(breaks = seq(0,1, by = 0.2), labels = scales::percent) +
    geom_vline(xintercept = 20000, linetype = "solid", color = "#012591", size = 1) +  # Add a vertical line at x=0
    geom_vline(xintercept = 50000, linetype = "solid", color = "#006b0c", size = 1) +  # Add a vertical line at x=0
    annotate("label", x = 20000, y = 0.85, label = "WTP treshold at €20,000", 
            size = 5, color = "White", fill = "#012591", fontface = "bold") +
    annotate("label", x = 50000, y = 0.15, label = "WTP treshold at €50,000", 
            size = 5, color = "White", fill = "#006b0c", fontface = "bold")

  ggsave("figures/ceac.png", plot = ceac, width = 7, height = 6)
  }

ROCPlot <- function(load_grid, cut_off) {
    # Contour part
    # Loading roc data
    data_for_roc <- fromJSON("data-raw/data_for_roc.json")
    data <- data_for_roc[["glaucoma"]]
    
    # Prepare the data for plotting
    plot_data <- do.call(rbind, lapply(1:length(data$fprs), function(i) {
        data.frame(
            fpr = data$fprs[[i]],
            tpr = data$tprs[[i]],
            group = i
        )
    }))
    
    # Select if group = 1
    roc_data <- plot_data[plot_data$group == 1,]
    
    # Contour part
    sensitivity <- seq(0, 1, length.out = 50)
    specificity <- seq(0, 1, length.out = 50)
    
    if (load_grid == FALSE) {
        # Create a grid of sensitivity and specificity values
        grid <- expand.grid(sensitivity = sensitivity, specificity = specificity)
        
        # Create new column called ICER
        grid$icer <- NA
        
        for (i in 1:nrow(grid)) {
            p_dt$ai_spec <- grid$specificity[i]
            p_dt$ai_sens <- grid$sensitivity[i]
            
            grid$icer[i] <- callModel()
            
            # Counter of iterations
            print(paste("Iteration", i, "from", nrow(grid)))
        }
        
        # Save grid
        saveRDS(grid, file = "data/results_grid.rds")
    } else {
        grid <- readRDS("data/results_grid.rds")
    }
    
    grid_a <- grid
    
    # Recode all high ICERS
    grid_a$icer[grid_a$icer > cut_off] <- cut_off
    
    # Calculate False Positive Rate (FPR) from specificity
    grid_a$fpr <- 1 - grid_a$specificity
    
    # Ensure grid_a contains no NA values
    grid_a <- na.omit(grid_a)
    
    # Find the tile with the lowest ICER that intersects the ROC curve
    find_closest_tile <- function(fpr, tpr, grid) {
        distances <- (grid$fpr - fpr)^2 + (grid$sensitivity - tpr)^2
        return(which.min(distances))
    }
    
    closest_tiles <- sapply(1:nrow(roc_data), function(i) {
        find_closest_tile(roc_data$fpr[i], roc_data$tpr[i], grid_a)
    })
    
    lowest_icer_tile_index <- which.min(grid_a$icer[closest_tiles])
    lowest_icer_tile <- grid_a[closest_tiles[lowest_icer_tile_index],]
    
    # Determine the size of the tiles based on the grid resolution
    tile_width <- 1 / 50  # Since we have 50 points along x-axis
    tile_height <- 1 / 50  # Since we have 50 points along y-axis
    
    # Plot the ISO plot with ROC curve
    plot <- ggplot() +
        geom_tile(data = grid_a, aes(x = fpr, y = sensitivity, fill = icer), color = ifelse(grid_a$icer < 20000, "darkblue", NA),size = 0.3 ) +
        geom_rect(data = lowest_icer_tile, 
                  aes(xmin = fpr - tile_width / 2, xmax = fpr + tile_width / 2, 
                      ymin = sensitivity - tile_height / 2, ymax = sensitivity + tile_height / 2, color = "ICER < 20.000"), 
                      fill = NA, size = 0.1, show.legend = TRUE) + 
        geom_rect(data = lowest_icer_tile, 
                  aes(xmin = fpr - tile_width / 2, xmax = fpr + tile_width / 2, 
                      ymin = sensitivity - tile_height / 2, ymax = sensitivity + tile_height / 2, color = "Lowest ICER tile\non ROC curve"), 
                      fill = NA, size = 0.7, show.legend = TRUE) +
        geom_line(data = roc_data, aes(x = fpr, y = tpr), alpha = 0.5, size = 0.8, colour = "black") +
        scale_fill_distiller(palette = "RdYlBu", name = "ICER", labels = label_comma()) +
        geom_point(aes(x = (1 - 0.95), y = 0.85, color = "Base-case\nscenario"), fill = "white", size = 1.5, stroke = 0.5, shape = 21, show.legend = TRUE) + # Add base case point without legend
        labs(x = "1 - Specificity",
             y = "Sensitivity") +
        scale_color_manual(name = "Legend", values = c("ICER < 20.000" = "darkblue", "Base-case\nscenario" = "black", "Lowest ICER tile\non ROC curve" = "red")) +
        guides(color = guide_legend(override.aes = list(
            shape = c(21, NA, NA), 
            fill = c("white", NA, NA), 
            linetype = c(0, 1, 1)
        ))) +
        theme_minimal(base_size = 15) +
        theme(
            legend.position = "right",  # Adjust legend position if necessary
            legend.margin = margin(0, 0, 0, 0, "cm"),  # Reduce space around legend
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.text.y = element_text(size = 17),
            axis.text.x = element_text(size = 17),
            panel.grid.major = element_line(color = "#adadad"),  
            panel.grid.minor = element_line(color = "#adadad"),
            panel.background = element_rect(fill = "lightgray"),
            legend.text = element_text(size = 11)
        )
        
    # Save plot
    ggsave("figures/ROC_plot.png", plot = plot, width = 7, height = 5, dpi = 300)
    print(lowest_icer_tile)
}
