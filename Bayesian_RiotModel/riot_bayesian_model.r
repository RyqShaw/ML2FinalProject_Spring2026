library(brms)
library(tidyverse)
library(ggplot2)

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

# Log Normal Model
manual_prior <- c(prior(student_t(3, 0, 2.5), class = "sigma"),
                  prior(student_t(3, 7.4, 2.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = 'b', coef = 'kills'),
                  prior(normal(0, 0.5), class = 'b', coef = 'deaths'),
                  prior(normal(0, 0.5), class = 'b', coef = 'assists'),
                  prior(normal(0, 0.5), class = 'b', coef = 'visionScore'),
                  prior(normal(0, 0.5), class = 'b', coef = 'totalHeal'),
                  prior(normal(0, 0.5), class = 'b', coef = 'totalHealsOnTeammates'),
                  prior(normal(0, 0.5), class = 'b', coef = 'win_ratio'),
                  prior(normal(0, 0.5), class = 'b', coef = 'early_surrender_ratio'))

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
new_obs <- test[3, ]

# Get posterior predictive draws
example_pred <- posterior_predict(posterior, newdata = new_obs)
cat("Mean prediction:", mean(pred), "\n")
cat("95% CI: [", quantile(pred, 0.025), ",", quantile(pred, 0.975), "]\n")
cat("Actual value:  ", new_obs$timePlayed, "\n")
