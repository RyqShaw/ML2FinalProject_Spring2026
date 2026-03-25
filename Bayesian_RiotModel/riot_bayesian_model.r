library(MASS)
library(mvnfast)
library(ggplot2)
library(plotly)

player_info <- read.csv("online_gaming_behavior_dataset.csv")
head(player_info, 5)

# Establish Categorical distribution
player_info <- player_info %>%
  mutate(EngagementLevel = factor(EngagementLevel, 
                             levels = c("Low", "Medium", "High"), 
                             ordered = TRUE))
head(player_info, 5)
engagement <- as.integer(player_info$EngagementLevel)
head(engagement)

# Beta(math done in log space)
dirichlet_beta <- function(alpha) {
  exp(sum(lgamma(alpha)) - lgamma(sum(alpha)))
}

# Dirichlet Prior(math done in log space to not encounter overflow)
prior <- function(theta, alpha) {
  exp(-log(dirichlet_beta(alpha)) + sum((alpha - 1) * log(theta)))
}

# Conjugate -> just add counts to alpha
posterior <- function(theta, alpha, categories) {
  counts <- table(categories)
  prior(theta, alpha + counts)
}

# low confidence in each low, medium, and high
alpha <- rep(1, 3)

# Probabilities based on what i think each category is like
sim_theta <- c(0.2, 0.5, 0.3)
prior_dist <- prior(sim_theta, alpha)
post_dist <- posterior(sim_theta, alpha, engagement)

#plotting logic(doesn't work)
plot_df <- data.frame(theta = sim_theta, Prior = prior_dist, Posterior = post_dist)
ggplot(plot_df, aes(x = theta)) +
  geom_line(aes(y = Prior, color = 'red')) +
  geom_line(aes(y = Posterior, color = 'purple')) +
  labs(x = expression(theta), y = expression(p(theta))) +
  ggtitle("Prior and Posterior Distributions(Dirichlet Categorical)") +
  scale_color_manual(name = "Legend", values = c("red" = "red", "purple" = "purple"), labels = c("red" = "Prior", "purple" = "Posterior"))