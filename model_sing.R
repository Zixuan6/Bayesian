library(cmdstanr)
cmdstanr::cmdstan_version()
library(brms)
library(tidyverse)

# Upload data
data1 <- read.csv("/Users/betty/Desktop/sync-coop-song-speech-0520/analysis/Stage2/keydata_long_20250517.csv")
data1 <- data1[, c("Participant", "group", "time", "score")]
data_sing <- subset(data1, group == "S")
data_conv <- subset(data1, group == "C")
data_reci <- subset(data1, group == "R")
head(data_sing)

# Have a look at the change in bonding scores from pre to post 
library(ggplot2)
data_sing$time <- factor(data_sing$time, levels = c("Pre_Experiment", "Post_Experiment"))
ggplot(data_sing, aes(x = time, y = score)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray") +  
  geom_point(aes(group = Participant), position = position_jitter(width = 0.1), alpha = 0.6) +  
  geom_line(aes(group = Participant), alpha = 0.4) +  
  labs(title = "Bonding Score: Pre vs Post (Singing Group)",
       x = "Time",
       y = "Score") +
  theme_minimal()

#prior1 <- c(
#  prior(normal(0, 2), class = "b"),      
#  prior(cauchy(0, 1), class = "sd"),     
#  prior(cauchy(0, 1), class = "sigma")   
#)

# Bayesian model
model_sing <- brm(
  score ~ time + (1 | Participant),
  data = data_sing,
  family = gaussian(),
  save_pars = save_pars(all = TRUE),
  backend = "cmdstanr",
  chains = 4, iter = 4000, warmup = 1000, seed = 123
)

summary(model_sing)
plot(model_sing)
pp_check(model_sing)

# Fit the null model
model_null <- brm(
  score ~ 1 + (1 | Participant),  # Intercept-only model
  data = data_sing,
  family = gaussian(),
  save_pars = save_pars(all = TRUE),
  backend = "cmdstanr",
  chains = 4, iter = 4000, warmup = 1000, seed = 123
)


#bridge_result <- bridge_sampler(
#  model_sing,
#  recompile = TRUE,  
#  silent = FALSE     
#)
library(bayestestR)
# Compute Bayes factor
bf_result <- bayesfactor_models(model_sing, denominator = model_null)
print(bf_result)

# Optional: ROPE
ROPE <- c(-2, 2)  # 根据你的数据调整
posterior_samples <- as_draws_df(model_sing)
post_minus_pre <- -posterior_samples$b_timePre_Experiment  # 注意负号转换

# Calculate the proportion
prop_in_rope <- mean(post_minus_pre >= ROPE[1] & post_minus_pre <= ROPE[2])
prop_below <- mean(post_minus_pre < ROPE[1])
prop_above <- mean(post_minus_pre > ROPE[2])

cat("Proportion of the posterior distribution within the ROPE::", prop_in_rope, "\n",
    "Proportion of the posterior distribution below the ROPE (less than lower bound):", prop_below, "\n",
    "Proportion of the posterior distribution above the ROPE (greater than upper bound):", prop_above)

library(ggplot2)

# Plot the posterior distribution and mark the ROPE interval
df_plot <- data.frame(post_minus_pre = post_minus_pre)

ggplot(df_plot, aes(x = post_minus_pre)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = ROPE[1], linetype = "dashed", color = "red") +
  geom_vline(xintercept = ROPE[2], linetype = "dashed", color = "red") +
  labs(title = "Posterior Distribution of Difference (Post - Pre)",
       x = "Effect Size (Post - Pre)",
       y = "Density") +
  annotate("text", x = ROPE[1], y = 0, label = paste("ROPE low =", ROPE[1]), hjust = 1.2, color = "red") +
  annotate("text", x = ROPE[2], y = 0, label = paste("ROPE high =", ROPE[2]), hjust = -0.2, color = "red") +
  theme_minimal()
