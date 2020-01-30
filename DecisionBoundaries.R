################################################################################
# Decision Boundaries
# 
# 2020-01-27
################################################################################

# LOADING ######################################################################

library(magrittr)
library(tidyverse)
library(ggplot2)
library(mvtnorm)
library(naivebayes)

# TODO: what happens when py0 shifts or when n shifts?
# Note that changing the class balannce will cause decision boundary to shift towards class with lower probability, to balance out % misclassifications. 

# Parameters to play with:
py0     <- 0.5
n       <- 500
mu0     <- c(3,5)
sigma0  <- matrix(c(1,-0.4,-0.4,1), nrow = 2, byrow = T)
mu1     <- c(5,3)
sigma1  <- matrix(c(1,0.2,0.2,1), nrow = 2, byrow = T)

# Generate sample data
py1 <- 1 - py0
n0  <- rbinom(1, n, py0)
n1  <- n - n0

sample0 <- rmvnorm(n0, mu0, sigma0)
sample1 <- rmvnorm(n1, mu1, sigma1)

# Combine
combined_sample <- rbind2(sample0, sample1) %>% 
  as.data.frame() %>% 
  mutate(class = c(rep(0, n0), rep(1, n1))) %>% 
  rename(
    X1 = V1,
    X2 = V2
  )

# MODEL ########################################################################

## Optimal Bayes

# Generate PDF (likelihood) of data
x1_range <- seq(0, 8, by = 0.05)
x2_range <- seq(0, 8, by = 0.05)
combined_range <- expand.grid(X1 = x1_range, X2 = x2_range)
# conditional probability of (x1, x2) given y = 0
px_y0 <- dmvnorm(combined_range, mean = mu0, sigma = sigma0)
# conditional probability of (x1, x2) given y = 1
px_y1 <- dmvnorm(combined_range, mean = mu1, sigma = sigma1)

# Predicted class (posterior)
py0_x         <- px_y0 * py0
py1_x         <- px_y1 * py1
optimal       <- py1_x - py0_x
predict_class <- ifelse(py0_x > py1_x, 0, 1)
predict_df <- data.frame(
  py0_x         = py0_x,
  py1_x         = py1_x,
  optimal       = optimal,
  predict_class = predict_class
)

# Combine to big DF
combined_result <- combined_range %>% 
  bind_cols(predict_df)

## Naive Bayes

fit_nb       <- naive_bayes(
  formula = factor(class) ~ X1 + X2, 
  data    = combined_sample
)
predict_nb  <- predict(fit_nb, newdata = combined_range, type = "prob")
combined_nb <- combined_range %>% 
  bind_cols(data.frame(predict_class = predict_nb[,2])) %>% 
  mutate(optimal = predict_class - 0.5)

## Logistic Regression

fit_lm       <- glm(
  formula = class ~ X1 + X2, 
  data    = combined_sample, 
  family  = binomial
)
predict_glm  <- predict(fit_lm, newdata = combined_range, type = "response")
combined_glm <- combined_range %>% 
  bind_cols(data.frame(predict_class = predict_glm)) %>% 
  mutate(optimal = predict_class - 0.5)

# PLOT #########################################################################

theme_set(
  theme_minimal() +
	theme(panel.grid.minor = element_blank()) + 
  theme(plot.title = element_text(size=16))
)

# Sample data
ggplot(combined_sample) +
  geom_point(
    aes(x = X1, y = X2, color = factor(class), shape = factor(class)),
    alpha = 0.7
  ) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(type = "div", palette = "Dark2") +
  scale_shape_manual(values = c(4, 18)) +
  scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(title = "Sample Data")

# Sample data + posterior dist
ggplot(combined_result) +
  geom_contour(aes(x = X1, y = X2, z = py0_x), color = "#1b9e77") +
  geom_contour(aes(x = X1, y = X2, z = py1_x), color = "#d95f02") +
  geom_point(
    data = combined_sample,
    aes(x = X1, y = X2, color = factor(class), shape = factor(class)),
    alpha = 0.4
  ) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(type = "div", palette = "Dark2") +
  scale_shape_manual(values = c(4, 18)) +
  scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(title = "Sample Data") +
  labs(subtitle = "Overlaid with contour of actual posterior distribution") + 
  theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))

# Sample data + optimal classifier
ggplot(combined_result) +
  geom_point(
    data = combined_sample,
    aes(x = X1, y = X2, color = factor(class), shape = factor(class)),
    alpha = 0.7
  ) + 
  geom_contour(
    aes(x = X1, y = X2, z = optimal), 
    color    = "black",
    linetype = "dashed",
    breaks   = 0 # layer where optimal = 0
  ) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(type = "div", palette = "Dark2") + 
  scale_shape_manual(values = c(4, 18)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  scale_x_continuous(limits = c(0, 8)) + 
  labs(title = "Decision Boundary for Optimal Bayes Classifier")
# Note that the decision boundary is non-linear!

# NAIVE BAYES
# Note that the decision boundary is non-linear, but would be linear if the likelihood is Gaussian

ggplot(combined_nb) +
  geom_point(
    data = combined_sample,
    aes(x = X1, y = X2, color = factor(class), shape = factor(class)),
    alpha = 0.7
  ) + 
  geom_contour(
    aes(x = X1, y = X2, z = optimal), 
    color    = "black",
    breaks   = 0 # layer where optimal = 0
  ) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(type = "div", palette = "Dark2") + 
  scale_shape_manual(values = c(4, 18)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  scale_x_continuous(limits = c(0, 8)) + 
  labs(title = "Decision Boundary for Naive Bayes")

ggplot(combined_glm) +
  geom_point(
    data = combined_sample,
    aes(x = X1, y = X2, color = factor(class), shape = factor(class)),
    alpha = 0.7
  ) + 
  geom_contour(
    aes(x = X1, y = X2, z = optimal), 
    color    = "black",
    breaks   = 0 # layer where optimal = 0
  ) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(type = "div", palette = "Dark2") + 
  scale_shape_manual(values = c(4, 18)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  scale_x_continuous(limits = c(0, 8)) + 
  labs(title = "Decision Boundary for Logistic Regression")

# EVALUATE #####################################################################

## Optimal Bayes
px_y0_sam <- dmvnorm(combined_sample[c("X1", "X2")], mean = mu0, sigma = sigma0)
px_y1_sam <- dmvnorm(combined_sample[c("X1", "X2")], mean = mu1, sigma = sigma1)
py0_x_sam <- px_y0_sam * py0
py1_x_sam <- px_y1_sam * py1
predict_class_sam <- ifelse(py0_x_sam > py1_x_sam, 0, 1)

## Naive Bayes
sample_nb <- predict(fit_nb, type = "prob")
sample_nb <- ifelse(sample_nb[,2] > sample_nb[,1], 1, 0)

## Logistic Regression
sample_glm <- predict(fit_lm, type = "response")
sample_glm <- ifelse(sample_glm > 0.5, 1, 0)

## Combined Results
sample_classification <- combined_sample %>% 
  bind_cols(data.frame(pred_optimal = predict_class_sam)) %>% 
  bind_cols(data.frame(pred_nb = sample_nb)) %>% 
  bind_cols(data.frame(pred_logistic = sample_glm))

# Errors
error_optimal  <- with(
  sample_classification, 
  as.matrix(table(Predicted = pred_optimal, Actual = class))
)
error_naive    <- with(
  sample_classification, 
  as.matrix(table(Predicted = pred_nb, Actual = class))
)
error_logistic <- with(
  sample_classification, 
  as.matrix(table(Predicted = pred_logistic, Actual = class))
)
error_optimal
error_naive
error_logistic

# misclassification rate
1 - (sum(diag(error_optimal)) / n)
1 - (sum(diag(error_naive)) / n)
1 - (sum(diag(error_logistic)) / n)

# true positive rate (recall, sensitivity)
round(error_optimal[2,2] / sum(error_optimal[,2]), 4)
round(error_naive[2,2] / sum(error_naive[,2]), 4)
round(error_logistic[2,2] / sum(error_logistic[,2]), 4)

# true negative rate (specificity) 
round(error_optimal[1,1] / sum(error_optimal[,1]), 4)
round(error_naive[1,1] / sum(error_naive[,1]), 4)
round(error_logistic[1,1] / sum(error_logistic[,1]), 4)

# positive predictive value (precision)
round(error_optimal[2,2] / sum(error_optimal[2,]), 4)
round(error_naive[2,2] / sum(error_naive[2,]), 4)
round(error_logistic[2,2] / sum(error_logistic[2,]), 4)
