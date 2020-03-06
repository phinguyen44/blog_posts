################################################################################
# Resampling Methods
# 
# 2020-03-05
################################################################################

# LOADING ######################################################################

library(purrr)
library(dplyr)
library(ggplot2)
library(infuser)

set.seed(123)

# generate observations (log-normally distributed)
n     <- 1000
x     <- exp(rnorm(n))
x_df  <- data.frame(x = x)
theta <- median(x)

# BOOTSTRAP ####################################################################

B  <- c(10, 100, 1000, 10000)

run_bootstrap <- function(B) {
  bs <- numeric(length = B)
  for (i in 1:B) {
    samp    <- sample(x, size = n, replace = T)
    theta_i <- median(samp)
    bs[i]   <- theta_i
  }
  
  theta_bar_bs <- mean(bs)
  var_bs       <- (1/(B-1))*sum((bs - theta_bar_bs)^2)
  
  # confidence interval (95%)
  ub_bs <- theta_bar_bs + 1.96*sqrt(var_bs)
  lb_bs <- theta_bar_bs - 1.96*sqrt(var_bs)
  
  df <- data.frame(
    theta_bar = theta_bar_bs,
    var       = var_bs,
    lb        = lb_bs,
    ub        = ub_bs
  )
  return(df)
}

res        <- map_df(B, run_bootstrap)
res$method <- paste0('bootstrap_', B)

# JACKKNIFE ####################################################################

jk <- numeric(length = n)

for (i in 1:n) {
  theta_i   <- median(x[-i])
  jk[i]     <- theta_i
}

theta_bar_jk <- mean(jk)
var_jk       <- ((n-1)/n)*sum((jk - theta_bar_jk)^2)

# bias
bias_jk <- (n - 1)*(theta_bar_jk - theta)

# adjust theta_bar
theta_bar_jk_adjust <- theta_bar_jk - bias_jk

# confidence interval (95%)
ub_jk <- theta_bar_jk + 1.96*sqrt(var_jk)
lb_jk <- theta_bar_jk - 1.96*sqrt(var_jk)

# confidence interval bias-adjusted (95%)
ub_jk_adjust <- theta_bar_jk_adjust + 1.96*sqrt(var_jk)
lb_jk_adjust <- theta_bar_jk_adjust - 1.96*sqrt(var_jk)

res_jk <- data.frame(
  theta_bar = theta_bar_jk_adjust,
  var       = var_jk,
  lb        = lb_jk_adjust,
  ub        = ub_jk_adjust,
  method    = 'jackknife'
)

res <- bind_rows(res, res_jk)
orderr <- res$method
res$method <- factor(res$method, levels = orderr)

# GRAPHS #######################################################################

theme_set(
  theme_minimal() +
	theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16)) + 
  theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
)

# plot histogram
ggplot(x_df, aes(x = x)) + 
  geom_hline(yintercept = 0) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = theta, color = "#d95f02") + 
  geom_label(
    aes(
      x     = theta, 
      y     = 480, 
      label = infuse('median = {{theta}}', theta = round(theta, 2))),
    color = "#d95f02"
  ) + 
  labs(title = 'Histogram of Values')

# show confidence intervals (and how they vary with B)
ggplot() + 
  geom_vline(xintercept = theta, color = "#d95f02") + 
  geom_segment(
    data = res,
    aes(x = lb, xend = ub, y = method, yend = method)
  ) +
  geom_point(
    data = res,
    aes(x = theta_bar, y = method), size = 3
  ) + 
  scale_x_continuous(limits = c(0.85, 1.15)) + 
  theme(panel.grid.major.y = element_blank()) + 
  labs(title = "Confidence Intervals") + 
  labs(subtitle = "Orange line indicates median of original sample") +
  labs(x = "Median Estimate") + 
  labs(y = "Method") + 
  theme(legend.title = element_blank())
