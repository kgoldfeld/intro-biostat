library(data.table)
library(ggplot2)
library(patchwork)

theme_set(
  theme_bw(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      axis.title = element_text(size = 12)
    )
)

ages_plot <- seq(20, 80, by = 0.1)

plot_data <- data.table(
  age = ages_plot
)

plot_data[, probability :=
            plogis(-5.5 + 0.075 * age)]

plot_data[, log_probability := log(probability)]

plot_data[, log_odds :=
            -5.5 + 0.075 * age]


highlight <- data.table(
  age = c(30, 50, 70)
)

highlight[, p_age :=
            plogis(-5.5 + 0.075 * age)]

highlight[, p_next :=
            plogis(-5.5 + 0.075 * (age + 5))]

highlight[, log_p_age := log(p_age)]
highlight[, log_p_next := log(p_next)]

highlight[, log_odds_age :=
            -5.5 + 0.075 * age]

highlight[, log_odds_next :=
            -5.5 + 0.075 * (age + 5)]




##

make_plot <- function(yvar, y_start, y_end, ylab, title) {
  
  ggplot(plot_data, aes(x = age, y = .data[[yvar]])) +
    geom_line(linewidth = 1) +
    geom_segment(
      data = highlight,
      aes(
        x = age,
        xend = age + 5,
        y = .data[[y_start]],
        yend = .data[[y_start]]
      ),
      linewidth = 0.7
    ) +
    geom_segment(
      data = highlight,
      aes(
        x = age + 5,
        xend = age + 5,
        y = .data[[y_start]],
        yend = .data[[y_end]]
      ),
      linewidth = 0.7,
      color = "red"
    ) +
    geom_point(
      data = highlight,
      aes(x = age, y = .data[[y_start]]),
      size = 2.5
    ) +
    geom_point(
      data = highlight,
      aes(x = age + 5, y = .data[[y_end]]),
      size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq(20, 80, by = 10)
    ) +
    labs(
      title = title,
      x = "Age",
      y = ylab
    )
}

p1 <- make_plot(
  "probability",
  "p_age",
  "p_next",
  "Probability",
  "Probability scale: RD increases"
)

p2 <- make_plot(
  "log_probability",
  "log_p_age",
  "log_p_next",
  "Log probability",
  "Log probability scale: log(RR) decreases"
)

p3 <- make_plot(
  "log_odds",
  "log_odds_age",
  "log_odds_next",
  "Log odds",
  "Log-odds scale: log(OR) is constant"
)

p <- p1 + p2 + p3

ggsave("lecture06/images/rdrror.png", height = 2, scale = 2)
