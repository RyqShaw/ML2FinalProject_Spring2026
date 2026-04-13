# Exponential
manual_prior <- c(prior(student_t(3, 7.4, 2.5), class = "Intercept"),
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

new_obs <- test[3, ]

# Get posterior predictive draws
pred <- posterior_predict(posterior, newdata = new_obs)

# Summarize
cat("Mean prediction:", mean(pred), "\n")
cat("95% CI: [", quantile(pred, 0.025), ",", quantile(pred, 0.975), "]\n")
cat("Actual value:  ", new_obs$timePlayed, "\n")