set.seed(123)

n <- 100
n_outcomes <- 5
n_sims <- 100

s_replicate <- function(n, n_outcomes) {
  
  ci <- replicate(n_outcomes, {
    treatment <- rnorm(n, 0, 1)
    control <- rnorm(n, 0, 1)
    t.test(treatment, control)$conf.int
  })
  
  ci <- data.table(t(ci))
  setnames(ci, c("l95", "u95"))
  ci[, `:=`(
    outcome = .I,
    reject = !(l95 <= 0 & u95 >= 0)
  )]
  
  ci
}

results <- lapply(1:n_sims, function(x) s_replicate(n, n_outcomes))
results <- rbindlist(results, idcol = "index")

results[, any_reject := any(reject), by = index]

d_shade <- unique(results[any_reject == TRUE, .(index)])

# Percentage rejected for each outcome
d_reject <- results[, .(
  pct_reject = 100 * mean(reject)
), by = outcome]

facet_labels <- setNames(
  sprintf("Outcome %d: %.0f%% rejected",
          d_reject$outcome, d_reject$pct_reject),
  d_reject$outcome
)

# Percentage of studies with at least one rejection
pct_any_reject <- 100 * unique(results[, .(index, any_reject)])[, mean(any_reject)]

p <- ggplot(results, aes(x = index)) +
  geom_rect(
    data = d_shade,
    aes(xmin = index - 0.5, xmax = index + 0.5),
    ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE,
    fill = "#e6e8ac",
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(
    aes(ymin = l95, ymax = u95, color = reject),
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ outcome,
    ncol = 1,
    labeller = as_labeller(facet_labels)
  ) +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#ed5454"),
    guide = "none"
  ) +
  labs(
    title = sprintf(
      "%.0f%% of studies rejected at least one null hypothesis",
      pct_any_reject
    ),
    x = "Simulated study",
    y = "95% confidence interval"
  ) +
  theme(strip.text = element_text(face = "bold", size = 10))

ggsave(
  "lectures/images/lecture07/multiple_testing_ci.png",
  width = 8,
  height = 5,
  dpi = 300,
  scale = 1.5
)
