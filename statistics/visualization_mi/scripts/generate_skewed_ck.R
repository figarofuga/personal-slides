# Synthetic visualization exercise; these are not patient observations.
# Run from the repository root:
# pixi run --as-is Rscript statistics/visualization_mi/scripts/generate_skewed_ck.R

set.seed(20261016)
output_dir <- "statistics/visualization_mi/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Hand-designed anchors approximate the attached figure, with small random
# perturbations. This is a teaching example, not an estimated clinical model.
male_anchor <- c(
  100, 120, 130, 140, 150, 800, 850, 1250, 1300, 1350,
  1650, 1700, 1750, 2100, 2750, 2800, 3050, 3100, 3150, 3200,
  3350, 3400, 4150, 5050, 5500, 5550, 5600, 6200, 6250, 6300
)
female_anchor <- c(200, 220, 650, 680, 700, 1100, 1800, 2200, 2250, 2700, 8900)

ck <- round(c(male_anchor, female_anchor) * exp(rnorm(41, sd = 0.025)))
dat <- data.frame(
  synthetic_id = sprintf("SIM%03d", seq_along(ck)),
  sex = rep(c("Male", "Female"), c(length(male_anchor), length(female_anchor))),
  ck_post_pci_u_l = ck
)
write.csv(dat, file.path(output_dir, "synthetic_ck.csv"), row.names = FALSE)

summary_table <- do.call(rbind, lapply(c("Male", "Female"), function(group) {
  x <- dat$ck_post_pci_u_l[dat$sex == group]
  data.frame(
    sex = group, n = length(x), mean = mean(x), median = median(x),
    q1 = unname(quantile(x, 0.25)), q3 = unname(quantile(x, 0.75)),
    min = min(x), max = max(x)
  )
}))
write.csv(summary_table, file.path(output_dir, "synthetic_ck_summary.csv"), row.names = FALSE)

library(ggplot2)
dat$sex <- factor(dat$sex, levels = c("Male", "Female"))
fig <- ggplot(dat, aes(x = sex, y = ck_post_pci_u_l)) +
  geom_dotplot(
    binaxis = "y", binwidth = 250, stackdir = "center", dotsize = 0.85,
    fill = "white", colour = "black"
  ) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 8) +
  scale_y_continuous(breaks = seq(0, 10000, 2500), limits = c(0, 10000)) +
  labs(
    title = "Synthetic post-PCI CK values",
    subtitle = "Male: n = 30; Female: n = 11. Horizontal marks show medians.",
    x = NULL, y = "CK (U/L)",
    caption = "Artificial teaching data; not patient observations."
  ) +
  theme_classic(base_size = 13)
ggsave(file.path(output_dir, "synthetic_ck_preview.png"), fig, width = 6, height = 7, dpi = 160)

stopifnot(
  nrow(dat) == 41L, all(dat$ck_post_pci_u_l > 0),
  sum(dat$sex == "Female" & dat$ck_post_pci_u_l > 8000) == 1L
)
print(summary_table, row.names = FALSE)
