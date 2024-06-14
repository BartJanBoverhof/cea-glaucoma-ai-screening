# Calculating transition probabilities, toy example.
rm(list = ls()) # to clean the workspace
options(digits = 8)
require(pracma)

g_mild_data <- read.csv("transition-calculations/chuaharan normal group.csv")
g_moderate_data <- read.csv("transition-calculations/chuaharan moderate group.csv")
baseline <- read.csv("transition-calculations/chuaharan baseline.csv")

#------------------------------------------------------------------------------#
####                        1. Baseline functions                            ####
#------------------------------------------------------------------------------#
# Baseline function - general
x_axis_f <- baseline$x
y_axis_f <- baseline$y

f <- function(x) {
    ifelse(x < min(x_axis_f) | x > max(x_axis_f), 0, approx(x_axis_f, y_axis_f, xout = x)$y) 
}

normalize_f <- integrate(f, lower = -Inf, upper = Inf)$value

f <- function(y) {
    ifelse(y < min(x_axis_f) | y > max(x_axis_f), 0, approx(x_axis_f, y_axis_f, xout = y)$y) /normalize_f
}

# check if integral sums to 1
#integrate(f, lower = -Inf, upper = Inf)$value

# Visualize the function g
#x_test <- seq(-24, 10, by = 0.1)
#y_test <- sapply(x_test, f)
#plot(x_test, y_test, type = "l", xlab = "x", ylab = "f(x)", main = "Visualization of Function f")

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
#integrate(f_mild, lower = -Inf, upper = Inf)$value
#integrate(f_mod, lower = -Inf, upper = Inf)$value
#integrate(f_sev, lower = -Inf, upper = Inf)$value

# Visualisations to check functions
#x_test <- seq(-24, 10, by = 0.1)
#y_test <- sapply(x_test, f_mild)
#plot(x_test, y_test, type = "l", xlab = "x", ylab = "f(x)", main = "Visualization of Function f")



#------------------------------------------------------------------------------#
####                       PROGRESSION FUNCTIONS                           ####
#------------------------------------------------------------------------------#
### FUNCTION PROGRESSION ---- MILD ###
x_axis_mild <- g_mild_data$x 
y_axis_mild <- g_mild_data$y

# Define gfoo for normalisation
g_mild <- function(x) {
    ifelse(x < min(x_axis_mild) | x > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = x)$y) 
}

# normalize object save 
normalizer_g_mild <- integrate(g_mild, lower = -Inf, upper = Inf, subdivisions = 1000)$value

g_mild <- function(y) {
    ifelse(y < min(x_axis_mild) | y > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = y)$y) / normalizer_g_mild
}

# Calculate the mean of g_mild
mean_g_mild <- integrate(
    function(x) x * g_mild(x),
    lower = min(x_axis_mild),
    upper = max(x_axis_mild)
)$value  # Extract the value of the integral

# rescale x
x_axis_mild  <- g_mild_data$x * abs((0.6/mean_g_mild)) 

g_mild <- function(y) {
    ifelse(y < min(x_axis_mild) | y > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = y)$y)
}

# normalize again
normalizer_g_mild <- integrate(g_mild, lower = -Inf, upper = Inf, subdivisions = 1000)$value

g_mild <- function(y) {
    ifelse(y < min(x_axis_mild) | y > max(x_axis_mild), 0, approx(x_axis_mild, y_axis_mild, xout = y)$y) / normalizer_g_mild
}

# check if integral of g sums to 1
#integrate(g_mild, lower = -Inf, upper = Inf, subdivisions = 1000)$value

# take mean to check if equal to -0.6
#integrate(function(x) x * g_mild(x),lower = min(x_axis_mild),upper = max(x_axis_mild),subdivisions = 1000)$value  # Extract the value of the integral

#plot to check distribution
x_test <- seq(-15, 5, by = 0.1)
y_test <- g_mild(x_test)

library(ggplot2)

# Plot the PDF
plot <- ggplot(data.frame(x = x_test, y = y_test), aes(x = x, y = y)) +
    geom_line() +
    labs(x = "x", y = "PDF(x)", title = "Probability Density Function Untreated Progression (scaled)")

# Save the plot as a PNG file
ggsave("transition-calculations/plot__scaled.png", plot, width = 6, height = 4, dpi = 300)


### FUNCTION PROGRESSION ---- MODERATE / SEVERE PORGRESSERS ###
x_axis_mod <- g_moderate_data$x
y_axis_mod <- g_moderate_data$y

# Define gfoo for normalisation
g_mod <- function(x) {
    ifelse(x < min(x_axis_mod) | x > max(x_axis_mod), 0, approx(x_axis_mod, y_axis_mod, xout = x)$y) 
}

# normalize object save 
normalizer_g_mod <- integrate(g_mod, lower = -Inf, upper = Inf, subdivisions = 1000)$value

g_mod <- function(x) {
    ifelse(x < min(x_axis_mod) | x > max(x_axis_mod), 0, approx(x_axis_mod, y_axis_mod, xout = x)$y) / normalizer_g_mod
}

# Calculate the mean of g_mod
mean_g_mod <- integrate(
    function(x) x * g_mod(x),
    lower = min(x_axis_mod),
    upper = max(x_axis_mod)
)$value  # Extract the value of the integral

# check if integral of g sums to 1
#integrate(g_mod, lower = -Inf, upper = Inf, subdivisions = 1000)$value

# rescale x
x_axis_mod  <- g_moderate_data$x * abs((0.6/mean_g_mod)) 

g_mod <- function(y) {
    ifelse(y < min(x_axis_mod) | y > max(x_axis_mod), 0, approx(x_axis_mod, y_axis_mod, xout = y)$y)
}

# normalize again
normalizer_g_mod <- integrate(g_mod, lower = -Inf, upper = Inf, subdivisions = 1000)$value

g_mod <- function(y) {
    ifelse(y < min(x_axis_mod) | y > max(x_axis_mod), 0, approx(x_axis_mod, y_axis_mod, xout = y)$y) / normalizer_g_mod
}

# check if integral of g sums to 1
#integrate(g_mod, lower = -Inf, upper = Inf, subdivisions = 1000)$value

# take mean to check if equal to -0.6
#integrate(function(x) x * g_mod(x),lower = min(x_axis_mod),upper = max(x_axis_mod),subdivisions = 1000)$value  # Extract the value of the integral

# Visualize the function g 
#x_test <- seq(-10, 5, by = 0.1)
#y_test <- g_mod(x_test)
#plot(x_test, y_test, type = "l", xlab = "x", ylab = "PDF(x)", main = "Probability Density Function")



#------------------------------------------------------------------------------#
####                       COMBINED FUNCTIONS                           ####
#------------------------------------------------------------------------------#
# MILD PROGRESSION 
h_mild <- function(x, y) {
        f_x <- f_mild(x)
        g_y <- g_mild(y)
        
        h_xy <- f_x * g_y

        return(h_xy)
}

y_max_mild <- function(x) -6-x

#integral2(h_mild, -100, 100, -100, 100, reltol = 0.00000000000000000001, maxlist = 30000)$Q # validation to check to be 1. 
p_mild <- integral2(h_mild, xmin = -6, xmax = 3, ymin = -10, ymax = y_max_mild, reltol = 0.000000000001, maxlist = 100000)$Q # probability

# MODERATE PROGRESSION 
h_mod <- function(x, y) {
        f_x <- f_mod(x)
        g_y <- g_mod(y)
        
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
        g_y <- g_mod(y)
        
        h_xy <- f_x * g_y

        return(h_xy)
}

# boundary
y_max_sev <- function(x) -20-x

# Validation to check if the integral of h is 1. Moet 1 uitkomen aangezien die al genormaliseerd is.
#integral2(h_sev, -1000, 1000, -1000, 1000)$Q #check to be 1. 

p_sev <- integral2(h_sev, -20, -12, -10, y_max_sev, reltol = 0.0000000000000000001, abstol = 0.0000000000000000001, maxlist =  100000)$Q

print(p_mild)
print(p_mod)
print(p_sev)




##### Some validation
m_progression <- integrate(
    function(x) x * g_mod(x),
    lower = min(x_axis_mod),
    upper = max(x_axis_mod)
)$value  # Extract the value of the integral

m_baseline <- integrate(
    function(x) x * f_mod(x),
    lower = min(x_axis_f),
    upper = max(x_axis_f)
)$value  

m_years <- (-12 - m_baseline) / m_progression # amount of years needed
1 / m_years  # to probability
