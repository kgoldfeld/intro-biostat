set.seed(3817)

estimate_power <- function(n, mu_a = 10, mu_b = 12, sd = 3, n_sim = 5000) {
  
  p_values <- numeric(n_sim)
  
  for (s in 1:n_sim) {
    
    y_a <- rnorm(n, mean = mu_a, sd = sd)
    y_b <- rnorm(n, mean = mu_b, sd = sd)
    
    p_values[s] <- t.test(y_b, y_a)$p.value
  }
  
  mean(p_values < 0.05)
}

power_n <- CJ(
  n = seq(5, 100, by = 5),
  delta = c(1, 2, 3),
  sd = c(2, 3, 4)
)

power_n[, power := mapply(
  function(n, delta, sd) {
    estimate_power(
      n = n,
      mu_a = 10,
      mu_b = 10 + delta,
      sd = sd
    )
  },
  n, delta, sd
)]

power_n[, sd := factor(sd)]

p1 <- ggplot(
  power_n,
  aes(x = n, y = power, color = sd, group = sd)
) +
  geom_line(linewidth = 0.5) +
  geom_point(size = .5) +
  geom_hline(yintercept = 0.80, linewidth = .5,  linetype = 3) +
  facet_wrap(
    ~ delta,
    labeller = labeller(
      delta = function(x) paste("Population difference =", x)
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = .2)
  ) +
  scale_color_manual(
    values = c(
      "2" = "#0072B2",  # blue
      "3" = "#D55E00",  # vermillion
      "4" = "#009E73"   # green
    ),
    name = "Population SD"
  ) +
  labs(
    x = "Sample size per group",
    y = "Power"
  )

ggsave(
  "lectures/images/lecture04/power_curves.png",
  width = 10,
  height = 3,
  dpi = 300
)
2