library(brms)
library(tidyverse)
library(ggplot2)
library(gridExtra)

set.seed(42)
player_info <- read.csv("processed_players.csv")
head(player_info, 5)

# Scale Data
player_info_scaled <- player_info %>%
  mutate(across(c(kills, deaths, assists, goldSpent, totalMinionsKilled,
                  visionScore, dragonKills, baronKills, turretKills,
                  totalHeal, totalHealsOnTeammates, win_ratio, 
                  early_surrender_ratio), scale))

# train and test split for cross validation
index <- sample(nrow(player_info_scaled), size = 0.8 * nrow(player_info_scaled))
train <- player_info_scaled[index, ]
test  <- player_info_scaled[-index, ]

# Getting Default Prior to build an actual Prior
prior <- default_prior(timePlayed ~ kills + deaths + assists + visionScore
                       + totalHeal + totalHealsOnTeammates
                       + win_ratio + early_surrender_ratio + (kills * deaths),
                       data = train,
                       family = lognormal())

prior

# Log Normal Model: initially all set to 0, 0.5, updated based on predictions
manual_prior <- c(prior(student_t(3, 0, 2.5), class = "sigma"),
                  prior(student_t(3, 7.4, 2.5), class = "Intercept"),
                  prior(normal(0.02, 10), class = 'b', coef = 'kills'),
                  prior(normal(0.01, 5), class = 'b', coef = 'deaths'),
                  prior(normal(-0.03, 10), class = 'b', coef = 'assists'),
                  prior(normal(-0.03, 20), class = 'b', coef = 'visionScore'),
                  prior(normal(0.02, 10), class = 'b', coef = 'totalHeal'),
                  prior(normal(-0.01, 5), class = 'b', coef = 'totalHealsOnTeammates'),
                  prior(normal(-0.03, 20), class = 'b', coef = 'win_ratio'),
                  prior(normal(0.1, 20), class = 'b', coef = 'early_surrender_ratio'))

manual_prior

posterior <- brm(timePlayed ~ kills + deaths + assists + visionScore
                 + totalHeal + totalHealsOnTeammates
                 + win_ratio + early_surrender_ratio + (kills * deaths),
                     family = lognormal(),
                     data = train,
                     chains = 4,
                     iter = 2000,
                     warmup = 320,
                     thin = 2,
                     prior = manual_prior)

summary(posterior)
plot(posterior)
bayes_R2(posterior)

post_preds <- posterior_predict(posterior)

#Make into vector for nicer plot
post_preds_vec <- as.vector(post_preds)
cat("95% CI: [", quantile(post_preds_vec, 0.025), ",", quantile(post_preds_vec, 0.975), "]\n")

ggplot(data.frame(x = post_preds_vec), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Posterior Predictive Distribution(Log Normal) for Average Time Played",
    x = "Predicted Value",
    y = "Frequency"
  ) +
  theme_minimal()

# Error/Accuracy Quantification
pred <- posterior_epred(posterior, newdata = test)
post_means <- colMeans(pred)

rmse <- sqrt(mean((post_means - test$timePlayed)^2))

cat("RMSE:", rmse, "\n")

bayes_R2(posterior, newdata = test)

#Example Pred
index1 <- sample(1:nrow(test), 1)
index2 <- sample(1:nrow(test), 1)
index3 <- sample(1:nrow(test), 1)
index4 <- sample(1:nrow(test), 1)

obs1 <- test[index1, ]
obs2 <- test[index2, ]
obs3 <- test[index3, ]
obs4 <- test[index4, ]

# Get posterior predictive draws
example_pred1 <- posterior_predict(posterior, newdata = obs1)
example_pred2 <- posterior_predict(posterior, newdata = obs2)
example_pred3 <- posterior_predict(posterior, newdata = obs3)
example_pred4 <- posterior_predict(posterior, newdata = obs4)

# plot 1
index1
cat("Mean prediction:", mean(example_pred1), "\n")
cat("95% CI: [", quantile(example_pred1, 0.025), ",", quantile(example_pred1, 0.975), "]\n")
cat("Actual value:  ", obs1$timePlayed, "\n")

plot1 <- ggplot(data.frame(x = example_pred1), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Example Prediction Model 1",
    x = "Predicted Value",
    y = "Frequency"
  ) + 
  geom_point(x=obs1$timePlayed, y=0, colour="red", shape=4, size=4) +
  theme_minimal()

# Plot 2
index2
cat("Mean prediction:", mean(example_pred2), "\n")
cat("95% CI: [", quantile(example_pred2, 0.025), ",", quantile(example_pred2, 0.975), "]\n")
cat("Actual value:  ", obs2$timePlayed, "\n")

plot2 <- ggplot(data.frame(x = example_pred2), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Example Prediction Model 2",
    x = "Predicted Value",
    y = "Frequency"
  ) + 
  geom_point(x=obs2$timePlayed, y=0, colour="red", shape=4, size=4) +
  geom_point(x=obs2$timePlayed, y=0, colour="red", shape=4, size=4) +
  theme_minimal()

# Plot 3
index3
cat("Mean prediction:", mean(example_pred3), "\n")
cat("95% CI: [", quantile(example_pred3, 0.025), ",", quantile(example_pred3, 0.975), "]\n")
cat("Actual value:  ", obs3$timePlayed, "\n")

plot3 <- ggplot(data.frame(x = example_pred3), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Example Prediction Model 3",
    x = "Predicted Value",
    y = "Frequency"
  ) + 
  geom_point(x=obs3$timePlayed, y=0, colour="red", shape=4, size=4) +
  theme_minimal()

# Plot 4
index4
cat("Mean prediction:", mean(example_pred4), "\n")
cat("95% CI: [", quantile(example_pred4, 0.025), ",", quantile(example_pred4, 0.975), "]\n")
cat("Actual value:  ", obs4$timePlayed, "\n")

plot4 <- ggplot(data.frame(x = example_pred2), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Example Prediction Model 4",
    x = "Predicted Value",
    y = "Frequency"
  ) + 
  geom_point(x=obs4$timePlayed, y=0, colour="red", shape=4, size=4) +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
