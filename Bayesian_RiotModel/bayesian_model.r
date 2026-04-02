library(MASS)
library(mvnfast)
library(ggplot2)
library(plotly)
# SYNTHETIC DATA SECTION
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
  # enforce 3 categories
  counts <- table(factor(categories, levels = 1:3))
  prior(theta, alpha + as.numeric(counts))
}

alpha <- c(15, 30, 20)

# Theta init based on provieded alphas
theta_prior <- alpha / sum(alpha)
theta_posterior <- (alpha + as.numeric(table(factor(engagement, levels = 1:3)))) /
  sum(alpha + as.numeric(table(factor(engagement, levels = 1:3))))

# apply methods for all 3 theta values
prior_vals <- sapply(1:3, function(k) prior(theta_prior, alpha))
posterior_vals <- sapply(1:3, function(k) posterior(theta_prior, alpha, engagement))

#means and MLE found on wikipedia
counts <- table(factor(engagement, levels = 1:3))
mle <- as.numeric(counts) / sum(counts)
prior_mean <- alpha / sum(alpha)
posterior_mean <- (alpha + as.numeric(counts)) / sum(alpha + as.numeric(counts))

# bar chart for each of the categories
plot_df <- data.frame(
  Category = rep(c("Low", "Medium", "High"), 2),
  Probability = c(theta_prior, theta_posterior),
  Distribution = rep(c("Prior", "Posterior"), each = 3)
)

# display as low, medium,
plot_df$Category <- factor(plot_df$Category, levels = c("Low", "Medium", "High"))
plot_df$Distribution <- factor(plot_df$Distribution, levels = c("Prior", "Posterior"))

ggplot(plot_df, aes(x = Category, y = Probability, fill = Distribution)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = round(Probability, 4)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Prior" = "red", "Posterior" = "purple")) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1))) +
  labs(x = expression(theta), y = expression(p(theta))) +
  ggtitle("Prior and Posterior Distributions (Dirichlet-Categorical)") +
  theme_minimal()