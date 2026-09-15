gen_data <- function(sample_size, mu, sigma = NULL, shape = NULL, distribution = "Normal") {
  
  if (distribution == "Normal") {
    x <- rnorm(sample_size, mean = mu, sd = sigma)
  } else if (distribution == "Gamma") {
    x <- rgamma(sample_size, shape = shape, rate = shape/mu)
  }
  
  return(x)
}

gen_stats <- function(sample_size, mu,
                      sigma = NULL, shape = NULL,
                      distribution = "Normal") {
  
  x1 <- gen_data(
    sample_size = sample_size,
    mu = mu,
    sigma = sigma,
    shape = shape,
    distribution = distribution
  )
  
  x2 <- gen_data(
    sample_size = sample_size,
    mu = mu,
    sigma = sigma,
    shape = shape,
    distribution = distribution
  )
  
  test <- t.test(x1, x2)
  
  data.table(
    mean_1 = mean(x1),
    mean_2 = mean(x2),
    estimate = mean(x1) - mean(x2),
    p_value = test$p.value,
    reject = test$p.value < 0.05
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

sample_sizes <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)

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
          type1 = mean(res$reject)
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
    y = type1,
    group = distribution,
    color = distribution
  )
) +
  geom_hline(
    yintercept = 0.05,
    linetype = 2,
    color = "grey60"
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = c(10, 15, 20, 25, 30, 35, 40, 45, 50)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0.03, 0.07)
  ) +
  labs(
    x = "Sample size per simulated data set",
    y = "Type 1 error rate",
    color = NULL,
    title = "Type 1 error rates of t-based hypothesis tests"
  )

ggsave("lectures/images/lecture03/type1.png", height = 4, width = 8, scale = 1.2)
