tornadoPlot <-function(parameters, titleName, outcomeName){
  library(ggplot2)
  library(reshape2)
  library(scales)
  
    # Extract the values from the parameters list and convert to a numeric vector
    values <- unlist(parameters)

    # Create the matrix
    data <- cbind(values, values * 0.8, values * 1.2)

    # Grouped Bar Plot
    # Determine the overall optimal strategy
    paramNames2 <- names(values)
    
    # Combine the parameter list with the data
    ymean <- data[1,1]

    yMin <- data[,2] - ymean
    yMax <- data[,3] - ymean
    ySize <- abs(yMax - yMin)  #High value - Low value
    
    rankY<- order(ySize)
    nParams <- length(paramNames2)
    
    Tor <- data.frame(
        Parameter=c(paramNames2[rankY],paramNames2[rankY]),  
        Level=c(rep("Low",nParams),rep("High",nParams)),
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
        ggplot(Tor[Tor$Level=="Low",], aes(x=Parameter2,y=value, fill=level)) +
        geom_bar(stat="identity", fill="blue") +
        ggtitle("Tornado diagram B", subtitle = outcomeName) +
        scale_fill_discrete("Parameter Level: ", l=50)+
        scale_y_continuous(name="ICER (1000???/QALY)", trans=offset_trans(offset=ymean)) +
        scale_x_discrete(name="Parameter") +
        geom_bar(data=Tor[Tor$Level=="High",], aes(x=Parameter2,y=value, fill=level), stat="identity", fill="red", alpha=0.5) +
        geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
        theme_bw(base_size = 14) +
        coord_flip() +
        theme(legend.position="bottom",
                legend.title=element_text(size = txtsize,angle = 0, hjust = 1),
                legend.key = element_rect(colour = "black"),
                legend.text = element_text(size = txtsize),
                title = element_text(face="bold", size=15),
                axis.title.x = element_text(face="bold", size=txtsize),
                axis.title.y = element_text(face="bold", size=txtsize),
                axis.text.y = element_text(size=txtsize),
                axis.text.x = element_text(size=txtsize),
                axis.ticks.y = element_blank())
    )
}
