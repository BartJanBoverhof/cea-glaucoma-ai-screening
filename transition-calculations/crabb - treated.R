# Calculating transition probabilities, toy example.
rm(list = ls()) # to clean the workspace
options(digits = 8)
require(pracma)

g_data <- read.csv("transition-calculations/crabb - treated.csv")
baseline <- read.csv("transition-calculations/crabb - baseline.csv")

#------------------------------------------------------------------------------#
####                        1. Baseline functions                            ####
#------------------------------------------------------------------------------#
# Baseline function - general
x_axis <- baseline$x
y_axis <- baseline$y

f <- function(x) {
    ifelse(x < min(x_axis) | x > max(x_axis), 0, approx(x_axis, y_axis, xout = x)$y) 
}

normalize_f <- integrate(f, lower = -Inf, upper = Inf)$value

f <- function(y) {
    ifelse(y < min(x_axis) | y > max(x_axis), 0, approx(x_axis, y_axis, xout = y)$y) /normalize_f
}

# check if integral sums to 1
integrate(f, lower = -Inf, upper = Inf)$value

# Visualize the function g
#x_test <- seq(-22, 4, by = 0.1)
#y_test <- sapply(x_test, f)

# Plot the PDF
#plot <- ggplot(data.frame(x = x_test, y = y_test), aes(x = x, y = y)) +
#    geom_line() +
#    labs(x = "x", y = "PDF(x)", title = "Baseline distribution of glaucoma")

# Save the plot as a PNG file
#ggsave("transition-calculations/baseline.png", plot, width = 6, height = 6, dpi = 300)

# Baseline function - mild
f_mild <- function(x) {
    ifelse(x < -6 | x > 0, 0, f(x))
}

# Baseline function - mod
f_mod <- function(x) {
    ifelse(x < -12 | x > -6, 0, f(x))
}

# Baseline function - sev
f_sev <- function(x) {
    ifelse(x < -20 | x > -12, 0, f(x))
}

normalise_mild <- integrate(f_mild, lower = -Inf, upper = Inf)$value 
normalise_mod <- integrate(f_mod, lower = -Inf, upper = Inf)$value 
normalise_sev <- integrate(f_sev, lower = -Inf, upper = Inf)$value

# Baseline function - mild
f_mild <- function(x) {
    ifelse(x < -6 | x > 0, 0, f(x)) / normalise_mild
}

# Baseline function - mod
f_mod <- function(x) {
    ifelse(x < -12 | x > -6, 0, f(x)) / normalise_mod
}

# Baseline function - sev
f_sev <- function(x) {
    ifelse(x < -20 | x > -12, 0, f(x)) / normalise_sev
}

# Check if integrals sum to 1 
integrate(f_mild, lower = -Inf, upper = Inf)$value
integrate(f_mod, lower = -Inf, upper = Inf)$value
integrate(f_sev, lower = -Inf, upper = Inf)$value

# Visualisations to check functions
#x_test <- seq(-24, 10, by = 0.1)
#y_test <- sapply(x_test, f_sev)
#plot(x_test, y_test, type = "l", xlab = "x", ylab = "f(x)", main = "Visualization of Function f")



#------------------------------------------------------------------------------#
####                       PROGRESSION FUNCTIONS                           ####
#------------------------------------------------------------------------------#
### FUNCTION PROGRESSION ---- MILD ###
x_axis_mild <- g_data$x
y_axis_mild <- g_data$y

# Define gfoo for normalisation
g <- function(x) {
    ifelse(x < min(x_axis_mild) | x > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = x)$y) 
}

# normalize object save 
normalizer_g_mild <- integrate(g, lower = -Inf, upper = Inf, subdivisions = 1000)$value

g <- function(y) {
    ifelse(y < min(x_axis_mild) | y > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = y)$y) / normalizer_g_mild
}

# check if integral of g sums to 1
integrate(g, lower = -Inf, upper = Inf, subdivisions = 1000)$value

integrate(g, lower = -Inf, upper = Inf, subdivisions = 1000)$value

#plot to check distribution
x_test <- seq(-7, 5, by = 0.1)
y_test <- g(x_test)

library(ggplot2)

mean_g <- integrate(
    function(x) x * g(x),
    lower = min(x_axis_mild),
    upper = max(x_axis_mild),
    subdivisions = 1000
)$value  # Extract the value of the integral

# Plot the PDF
plot <- ggplot(data.frame(x = x_test, y = y_test), aes(x = x, y = y)) +
    geom_line() +
    geom_vline(xintercept = mean_g, linetype = "dashed", color = "red") +
    annotate("text", x = mean_g, y = max(y_test), label = "Mean", vjust = -1, color = "red") +
    labs(x = "x", y = "PDF(x)", title = "PDF Garway treated (latanaprost arm)")

# Save the plot as a PNG file
ggsave("transition-calculations/garway_treated.png", plot, width = 6, height = 6, dpi = 300)




#------------------------------------------------------------------------------#
####                       COMBINED FUNCTIONS                           ####
#------------------------------------------------------------------------------#
# MILD PROGRESSION 
h_mild <- function(x, y) {
        #integrate(f_mild, )
        f_x <- f_mild(x)
        g_y <- g(y)
        
        h_xy <- f_x * g_y

        return(h_xy)
}


y_max_mild <- function(x) -6-x

#integral2(h_mild, xmin = -50, xmax = 50, ymin = -50, ymax = 50, reltol = 0.00000000000000000001, maxlist = 30000)$Q # validation to check to be 1. 
p_mild <- integral2(h_mild, xmin = -6, xmax = 20, ymin = -10, ymax = y_max_mild, reltol = 0.000000000001)$Q # probability

# MODERATE PROGRESSION 
h_mod <- function(x, y) {
        f_x <- f_mod(x)
        g_y <- g(y)
        
        h_xy <- f_x * g_y

        return(h_xy)
}

# boundary 
y_max_mod <- function(x) -12-x

#integral2(h_mod, -50, 50, -50, 50, reltol = 0.0000000000000000001, abstol = 0.0000000000000000001, maxlist = 100000) #check to be 1. 

############### VRAAG: HIER OOK NORMALISEREN? 
############### VRAAG: OVER WELKE BOUNDARIES INTEGREREN WE? 
p_mod <- integral2(h_mod, xmin = -12, xmax = -6, ymin = -10, ymax = y_max_mod, reltol = 0.0000000000000000001, abstol = 0.0000000000000000001, maxlist = 100000)$Q
#ymax kan ook -inf zijn.


# SEVERE PROGRESSION 
h_sev <- function(x, y) {
        f_x <- f_sev(x)
        g_y <- g(y)
        
        h_xy <- f_x * g_y

        return(h_xy)
}

# boundary
y_max_sev <- function(x) -20-x

# Validation to check if the integral of h is 1. Moet 1 uitkomen aangezien die al genormaliseerd is.
#integral2(h_sev, -20, 20, -20, 20, reltol = 0.0000000000000000001, abstol = 0.0000000000000000001, maxlist =  100000)$Q
p_sev <- integral2(h_sev, -20, -12, -10, y_max_sev, reltol = 0.0000000000000000001, abstol = 0.0000000000000000001, maxlist =  100000)$Q

print(p_mild)
print(p_mod)
print(p_sev)








##### Some validation
# mild 
m_progression <- integrate(
    function(x) x * g_mild(x),
    lower = min(x_axis_mild),
    upper = max(x_axis_mild)
)$value  # Extract the value of the integral

m_baseline <- integrate(
    function(x) x * f_mild(x),
    lower = min(x_axis),
    upper = max(x_axis)
)$value  

m_years <- (-6 - m_baseline) / m_progression # amount of years needed
p_mild_alt <- abs(1 / m_years)  # to probability



# moderate 
m_progression <- integrate(
    function(x) x * g_mod(x),
    lower = min(x_axis_mod),
    upper = max(x_axis_mod)
)$value  # Extract the value of the integral

m_baseline <- integrate(
    function(x) x * f_mod(x),
    lower = -12,
    upper = -6
)$value  

m_years <- (-12 - m_baseline) / m_progression # amount of years needed
p_mod_alt <- 1 / m_years  # to probability

# severe
m_progression <- integrate(
    function(x) x * g_mod(x),
    lower = min(x_axis_mod),
    upper = max(x_axis_mod)
)$value  # Extract the value of the integral

m_baseline <- integrate(
    function(x) x * f_sev(x),
    lower = -20,
    upper = -12
)$value

m_years <- (-20 - m_baseline) / m_progression # amount of years needed
p_sev_alt <- 1 / m_years  # to probability

print(p_mild_alt)
print(p_mod_alt)
print(p_sev_alt)
