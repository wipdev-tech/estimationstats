library(dabestr)
library(ggplot2)
library(flextable)
library(dplyr)

# data (`slp`) ----
slp <- sleep
slp[["group"]] <- factor(
  ifelse(sleep[["group"]] == "1", "Drug 1", "Drug 2"),
  levels = c("Drug 1", "Drug 2")
)

# effect size plot (`es_plot`) ----
eff <- dabest(
  .data = slp,
  x = group,
  y = extra,
  idx = c("Drug 1", "Drug 2")
) %>% mean_diff()

es_plot <- plot(
  eff,
  theme = theme_minimal(),
  tick.fontsize = 14,
  rawplot.ylabel = "Change in sleep hours"
)

# effect size table (`es_flextable`) ----
print_summary <- function(v) sprintf("%2.2f (%2.2f)", mean(v), sd(v))

es_row <- slp %>% 
  split(slp$group) %>% 
  sapply(function(df_subset) print_summary(df_subset$extra)) %>% 
  c(
    eff$result$difference,
    paste0(eff$result$bca_ci_low, " – ", eff$result$bca_ci_high)
  )

es_flextable <- c("Drug 1*", "Drug 2*", "Unpaired mean difference", "CI**") %>% 
  rbind(es_row) %>% 
  as.data.frame() %>% 
  flextable() %>% 
  delete_part("header") %>% 
  add_footer_lines(
    paste0(
      "* Mean (SD)",
      "\n",
      "** Bias-corrected and accelerated (BCa) bootstrap confidence interval"
    )
  )

# t-test table (`t_flextable`) ----
t_obj <- t.test(
  extra ~ group,
  data = mutate(slp, group = relevel(group, ref = "Drug 2"))
)

t_row <- c(
  round(t_obj$statistic, 2),
  round(t_obj$parameter, 2),
  sprintf("%1.2f – %1.2f", t_obj$conf.int[1], t_obj$conf.int[2]),
  round(t_obj$p.value, 2)
)

t_flextable <- c("Statistic", "DF", "CI*", "p-value") %>% 
  rbind(t_row) %>% 
  as.data.frame() %>% 
  flextable() %>% 
  delete_part("header") %>% 
  add_footer_lines("* Theoretical (t-distribution) confidence interval")

# exportable output ----

format_flextable <- function(ft) {
  ft %>%
    hline_top() %>%
    hline_bottom() %>%
    hline(i = 1) %>%
    autofit() %>%
    style(
      part = "all",
      pr_t = officer::fp_text(font.family = "Times New Roman")
    ) %>%
    style(
      part = "footer",
      pr_t = officer::fp_text(font.family = "Times New Roman", font.size = 9)
    )
}

es_flextable %>%
  format_flextable() %>%
  save_as_docx(path = "inst/table1.docx")

t_flextable %>%
  format_flextable() %>% 
  save_as_docx(path = "inst/table2.docx")

ggsave(
  es_plot,
  filename = "inst/fig1.png",
  device = "png",
  width = 6,
  height = 7,
  type = "cairo"
)
