library(boot)
library(ggplot2)
set.seed(2022)

# data (`dtf`) ----

dtf <- data.frame(
  Group = rep(c("Treatment", "Control"), each = 60),
  Event = factor(
    sample(
      c("Event", "No Event"), size = 120, replace = TRUE, prob = c(0.2, 0.8)
    ),
    levels = c("Event", "No Event")
  )
)

# effect size plot

bootobj <- boot(
  data = dtf,
  R = 10000,
  statistic = function(d, i) {
    d_i <- d[i, ]
    tb <- table(d_i)
    or <- (tb[1] / tb[2]) / (tb[3] / tb[4])
    
    return(or)
  }
)

bca <- boot.ci(bootobj, type = "bca")$bca[4:5]

bar_plot <- ggplot(dtf, aes(x = Group, fill = Event)) +
  geom_bar(color = "white") +
  geom_text(
    aes(label = paste(after_stat(count))),
    stat = "count",
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  scale_fill_brewer(palette = "Set2", name = "") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Frequency")

or_plot <- ggplot(NULL, aes(x = bootobj$t)) +
  geom_density(fill = "gray", color = "white") +
  geom_pointrange(
    aes(y = 0, x = bootobj$t0, xmin = bca[1], xmax = bca[2]),
    size = 0.75
  ) +
  coord_cartesian(xlim = c(NA, 7)) +
  theme_minimal(base_size = 14) +
  labs(x = "Odds Ratio", y = "")
