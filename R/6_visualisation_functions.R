scenarioPlot <- function(){
  # run base case
  strategy <- "base"
  base_icer <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "icer")
  base_qaly <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "qaly")
  base_costs <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "costs")

  # run base case
  strategy <- "scenario"
  icer_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "icer")

  qaly_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "qaly")

  costs_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "costs")

  # Combine all icer_... lists into a list of lists for easier processing
  icer_lists <- list(
      expert_observation = icer_expert_observation,
      expert_utilisation = icer_expert_utilisation,
      ai_sensitivity = icer_sensitivity,
      ai_specificity = icer_specificity,
      compliance = icer_compliance,
      utilities = icer_utilities,
      transition = icer_transition,
      screening_interval = icer_screening_interval,
      screening_age = icer_screening_age,
      base_icer = base_icer
  )

  qaly_lists <- list(
      expert_observation = qaly_expert_observation,
      expert_utilisation = qaly_expert_utilisation,
      ai_sensitivity = qaly_sensitivity,
      ai_specificity = qaly_specificity,
      compliance = qaly_compliance,
      utilities = qaly_utilities,
      transition = qaly_transition,
      screening_interval = qaly_screening_interval,
      screening_age = qaly_screening_age,
      base_qaly = base_qaly
  ) 

  costs_lists <- list(
      expert_observation = costs_expert_observation,
      expert_utilisation = costs_expert_utilisation,
      ai_sensitivity = costs_sensitivity,
      ai_specificity = costs_specificity,
      compliance = costs_compliance,
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

  # Assuming 'base' and 'icer_data' are already defined

  # Example of adding "Shape" and "Color" columns to the dataframe
  # This is a simplified example. You'll need to assign unique shapes and colors based on your actual data
  icer_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  icer_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  qaly_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  qaly_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  costs_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  costs_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  # Ensure shapes can be filled (using shapes 21-25)
  # If your data already uses shapes 21-25, this step is correct. Otherwise, adjust your shape assignments accordingly.
  icer_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  qaly_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  costs_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  # Add ICER values to the Identifier string
  icer_data$Identifier <- paste(icer_data$Identifier, " - (",round(icer_data$ICER,0),")", sep = "")
  qaly_data$Identifier <- paste(qaly_data$Identifier, " - (",round(qaly_data$QALY,4),")", sep = "")
  costs_data$Identifier <- paste(costs_data$Identifier, " - (",round(costs_data$Costs,0),")", sep = "")

  # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
  # Load necessary libraries
  library(ggplot2)

  legend_plot <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 1) +
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
      legend.title = element_text(size = 15, face = "bold", hjust = 0.04),
      legend.text = element_text(size = 9.5, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_icer, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(
      breaks = seq(10000, 70000, by = 10000),  # Major breaks for axis labels
      minor_breaks = seq(0, 70000, by = 2500)  # Minor breaks for grid lines
    ) +
    geom_point(data = icer_data, aes(x = ICER, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
  plot_icer <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
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
      legend.position = "none",
      legend.title = element_text(size = 15, face = "bold", hjust = 0.04),
      legend.text = element_text(size = 9.5, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_icer, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(
      breaks = seq(10000, 70000, by = 10000),  # Major breaks for axis labels
      minor_breaks = seq(0, 70000, by = 2500)  # Minor breaks for grid lines
    ) +
    geom_point(data = icer_data, aes(x = ICER, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  plot_qaly <- ggplot(qaly_data, aes(x = QALY, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
    #geom_point(position = position_dodge(width = 0.5), size = 1, stroke = 1, fill = "black", alpha = 1, show.legend = FALSE) +
    scale_color_manual(values = setNames(qaly_data$Color, qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    scale_fill_manual(values = setNames(qaly_data$Color, qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(qaly_data$Shape), qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    labs(
      title = "",
      x = "Incremental QALY",
      y = "",
      shape = "Scenario (QALY)",
      fill = "Scenario (QALY)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 12, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) + 
    geom_vline(xintercept = base_qaly, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(0, 0.1, by = 0.005)) +
    geom_point(data = qaly_data, aes(x = QALY, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  plot_costs <- ggplot(costs_data, aes(x = Costs, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
    scale_color_manual(values = setNames(costs_data$Color, costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    scale_fill_manual(values = setNames(costs_data$Color, costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(costs_data$Shape), costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    labs(
      title = "",
      x = "Incremental Costs",
      y = "",
      shape = "Scenario (Costs)",
      fill = "Scenario (Costs)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",  # Remove legend
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_costs, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(0, 700, by = 50)) +
    geom_point(data = costs_data, aes(x = Costs, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)

  # Extract the legend
  g <- ggplotGrob(legend_plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]

  plot_icer <- plot_icer + theme(legend.position = "none")

  # Arrange the plots in a 2x2 grid with the legend in the bottom left quadrant
  plot <- grid.arrange(
    plot_qaly, plot_costs, legend, plot_icer,
    ncol = 2,
    nrow = 2,
    layout_matrix = rbind(c(1, 2),
                          c(3, 4)),
    heights = c(2, 2),  # Adjust the heights to make the bottom row taller
    widths = c(2, 2)    # Adjust the widths to make the left column wider
  )

  # Save plot to figures folder
  ggsave("figures/scenario.png", plot = plot, width = 12, height = 10)
}
    
tornadoPlot <-function(Parms, Outcomes, titleName, outcomeName){
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Grouped Bar Plot
  # Determine the overall optimal strategy
  paramNames2 <- Parms
  
  # Combine the parameter list with the data
  ymean <- Outcomes[1,1]
  yMin <- Outcomes[,2] - ymean
  yMax <- Outcomes[,3] - ymean
  ySize <- abs(yMax - yMin)  #High value - Low value
  
  
  rankY<- order(ySize)
  nParams <- length(paramNames2)
  
  Tor <- data.frame(
    Parameter=c(paramNames2[rankY],paramNames2[rankY]),  
    Level=c(rep("Lower bound",nParams),rep("Upper bound",nParams)),
    value=ymean+c(yMin[rankY],yMax[rankY]),
    sort=seq(1,nParams)
  )
  
  #re-order the levels in the order of appearance in the data.frame
  Tor$Parameter2 <- ordered(Tor$Parameter, Tor$Parameter[1:(length(Tor$Parameter)/2)])
  # Tor$Parameter2 <- factor(Tor$Parameter, as.character(Tor$Parameter))
  #Define offset as a new axis transformation. Source: http://blog.ggplot2.org/post/25938265813/defining-a-new-transformation-for-ggplot2-scales  
  offset_trans <- function(offset=0) {
    trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
  }
  #Plot the Tornado diagram.
  txtsize<-12
  print(
  ggplot(Tor, aes(x=Parameter2, y=value, fill=Level)) +  # Ensure 'fill' is mapped to 'Level' within 'aes()'
    geom_bar(data=Tor[Tor$Level=="Lower bound",], aes(x=Parameter2, y=value, fill="Lower bound"), stat="identity", alpha=0.8, stroke = 2, colour = "black") +
    geom_bar(data=Tor[Tor$Level=="Upper bound",], aes(x=Parameter2, y=value, fill="Upper bound"), stat="identity", alpha=0.8, stroke = 2, colour = "black") +
    ggtitle("", subtitle = outcomeName) +
    scale_fill_manual(name="Parameter Level", values=c("Lower bound"="blue", "Upper bound"="red")) +  # Define colors for 'Low' and 'High'
    scale_y_continuous(name="ICER", trans=offset_trans(offset=ymean), labels = function(x) ifelse(x == ymean, paste(x, " (ymean)", sep = ""), x)) +
    scale_x_discrete(name="Parameter") +
    geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
    theme_bw(base_size = 14) +
    coord_flip() +
    theme_minimal(base_size = 15) +
    theme(legend.position = c(0.95, 0.05), # This positions the legend at the right bottom corner
          legend.justification = c(1, 0), # This ensures the legend aligns at its bottom-right corner
          legend.box.just = "right", # Aligns the legend box to the right
          legend.margin = margin(), # Adjust margins if necessary
          legend.box = "vertical",
          legend.title=element_text(size = txtsize, angle = 0, hjust = 1),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_line(color = "#adadad"),  
          panel.grid.minor = element_line(color = "#adadad"),
          panel.background = element_rect(fill = "lightgray")
    )
  )
}
