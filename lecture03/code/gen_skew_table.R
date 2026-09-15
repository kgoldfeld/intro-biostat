gen_data <- function(sample_size, mu, sigma = NULL, shape = NULL, distribution = "Normal") {
  
  if (distribution == "Normal") {
    x <- rnorm(sample_size, mean = mu, sd = sigma)
  } else if (distribution == "Gamma") {
    x <- rgamma(sample_size, shape = shape, rate = shape/mu)
  }
  
  return(x)
}

gen_stats <- function(sample_size, mu, sigma = NULL, shape = NULL, distribution = "Normal") {
  
  birth_weight <- gen_data(sample_size, mu, sigma, shape, distribution)
  
  x_bar <- mean(birth_weight)
  s <- sd(birth_weight)
  se <- s / sqrt(sample_size)
  t.975 <- qt(0.975, df = sample_size - 1)
  
  t_stat <- (x_bar - mu) / (s / sqrt(sample_size))
  
  lower <- x_bar - t.975 * se
  upper <- x_bar + t.975 * se
  
  data.table(
    x_bar = x_bar,
    t_stat = t_stat,
    lower = lower,
    upper = upper,
    covers = lower <= mu & upper >= mu
  )
}

set.seed(1234)

mu <- 10
replications <- 100000

scenarios <- data.table(
  distribution = c(
    "Normal",
    "Moderate skew",
    "Strong skew"
  ),
  sigma = c(5, NA, NA),
  shape = c(NA, 4, 1)
)

sample_sizes <- c(15, 30, 45, 60, 75, 90)

results <- rbindlist(
  lapply(sample_sizes, function(n) {
    
    rbindlist(
      lapply(1:nrow(scenarios), function(i) {
        
        dist <- ifelse(
          scenarios[i, distribution] == "Normal",
          "Normal",
          "Gamma"
        )
        
        res <- rbindlist(
          lapply(1:replications, function(x) {
            gen_stats(
              sample_size = n,
              mu = mu,
              sigma = scenarios[i, sigma],
              shape = scenarios[i, shape],
              distribution = dist
            )
          })
        )
        
        data.table(
          sample_size = n,
          distribution = scenarios[i, distribution],
          coverage = mean(res$covers)
        )
      })
    )
  })
)

results[, distribution := factor(
  distribution,
  levels = c(
    "Normal",
    "Moderate skew",
    "Strong skew"
  )
)]

results

p <- ggplot(
  results,
  aes(
    x = sample_size,
    y = coverage,
    group = distribution,
    color = distribution
  )
) +
  geom_hline(
    yintercept = 0.95,
    linetype = 2,
    color = "grey60"
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = c(15, 30, 45, 60, 75, 90)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0.90, 0.96)
  ) +
  labs(
    x = "Sample size per simulated data set",
    y = "Coverage",
    color = NULL,
    title = "Coverage of t-based 95% confidence intervals"
  )

ggsave("lectures/images/lecture03/coverage.png", height = 4, width = 8, scale = 1.2)
