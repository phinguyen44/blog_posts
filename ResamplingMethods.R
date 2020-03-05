################################################################################
# Resampling Methods
# 
# 2020-03-05
################################################################################

# LOADING ######################################################################

library(ggplot2)
library(infuser)

set.seed(123)

# generate observations (log-normally distributed)
n   <- 1000
x   <- exp(rnorm(n))
xdf <- data.frame(x = x)
med <- round(median(x), 2)

theme_set(
  theme_minimal() +
	theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16)) + 
  theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
)

ggplot(xdf, aes(x = x)) + 
  geom_hline(yintercept = 0) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = med, color = "#d95f02") + 
  geom_label(
    aes(x = med, y = 480, label = infuse('median = {{med}}', med = med)),
    color = "#d95f02"
  ) + 
  labs(title = 'Histogram of values')


