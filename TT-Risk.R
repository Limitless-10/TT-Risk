# Load relevant libraries
library(tidyverse) # For general data manipulation
library(ggplot2) # For plotting
library(patchwork) # For compiling plots

# Positive Net Gain = Benefits - Cost
# Risk -> Standard deviation of the fluctuation from the mean
# Value at Risk

# Seed
set.seed(2042)

# "Safe" action
mu_safe <- 5 
sigma_safe <- 3

# "Bold" action
mu_bold <- 9
sigma_bold <- 10

# My own dnorm because I'm stubborn (Normal PDF, univariate)
f <- function(x, mu, sigma){
  y <- (1/((sqrt(2*pi))*sigma))*exp(-.5/(sigma^2)*((x-mu)^2))
  return(y)
}

# Function for nicely formatting normal distributions
norm_plot <- function(mu=0, sigma=1, n_intervals=1000) {
  x <- seq(mu-3*sigma,mu+3*sigma,(6*sigma)/n_intervals)
  y <- (1/((sqrt(2*pi))*sigma))*exp(-.5/(sigma^2)*((x-mu)^2))
  
  # y <- pnorm(x,mean=mu,sd=sigma)
  plot_data <- list(x=x,y=y) %>%
    as.data.frame()
  
  p <- ggplot(data=plot_data,aes(x=x,y=y)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Normal Distribution (μ =", mu, ", σ =", sigma, ")"),
         x = "Value",
         y = "Density") +
    theme_minimal()
  
  return(p)
}

# Overlay "Safe" and "Bold" distributions
overlay_plot <- function(mu_safe, sigma_safe, mu_bold, sigma_bold, scenario, n_intervals=1000) {
  x <- seq(min(mu_safe - 3 * sigma_safe, mu_bold - 3 * sigma_bold),
           max(mu_safe + 3 * sigma_safe, mu_bold + 3 * sigma_bold),
           length.out = n_intervals)
  
  y_safe <- f(x, mu = mu_safe, sigma = sigma_safe)
  y_bold <- f(x, mu = mu_bold, sigma = sigma_bold)
  
  # Corrected tibble construction
  plot_data <- tibble(
    x = c(x, x),  # Combine x for both actions
    y = c(y_safe, y_bold),  # Combine y for both actions
    Action = rep(c("Safe", "Bold"), each = n_intervals)  # Correctly repeat labels
  )
  
  ggplot(plot_data, aes(x = x, y = y, fill = Action,color = Action)) +
    geom_area(alpha = 0.4, position = "identity") + 
    geom_line() +
    scale_fill_manual(values = c("Safe" = "orange", "Bold" = "blue")) + 
    scale_color_manual(values = c("Safe" = "orange", "Bold" = "blue")) +
    labs(
      title = paste0("Scenario: ",scenario),
      x = "Benefit",
      y = "Density"
    ) +
    theme_minimal()
}

# Plot the overlay
overlay_plot(mu_safe, sigma_safe, mu_bold, sigma_bold, scenario = "Test")

# Outline scenarios as per blog post
scenario_data <- data.frame(
  Scenario = c(
    "Baseline",
    "Predictable Shock",
    "Unpredictable Shock",
    "Multiplicative Shock"
  ),
  Safe_Mean = c(5, 0, 0, 5),
  Safe_SD = c(3, 3, 10.44, 6),
  Bold_Mean = c(9, 4, 4, 9),
  Bold_SD = c(10, 10, 14.14, 20)
)

plot_list <- list() 
for (i in 1:nrow(scenario_data)) {
  plot_list[[i]] <- overlay_plot(scenario_data$Safe_Mean[i],
                                 scenario_data$Safe_SD[i],
                                 scenario_data$Bold_Mean[i],
                                 scenario_data$Bold_SD[i],
                                 scenario = scenario_data$Scenario[i])
  print(length(plot_list))
}

(plot_list[[1]] + plot_list[[2]]) / (plot_list[[3]] + plot_list[[4]])

# Calculate VAR (Value at Risk)
scenario_data$Safe_VaR <- scenario_data$Safe_SD - scenario_data$Safe_Mean
scenario_data$Bold_VaR <- scenario_data$Bold_SD - scenario_data$Bold_Mean
scenario_data$Decision <- ifelse(
  scenario_data$Safe_VaR < scenario_data$Bold_VaR, "Safe", "Bold"
)

# Print the updated dataframe
print(scenario_data)


