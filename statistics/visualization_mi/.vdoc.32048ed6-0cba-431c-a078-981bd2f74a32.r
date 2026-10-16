#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: anscombes

library(tidyverse)
library(ggpubr)

anscombe_long <- tidyr::pivot_longer(
  anscombe,
  cols = everything(),
  names_to = c(".value", "set"),
  names_pattern = "([xy])(\\d+)",
  names_transform = list(set = as.integer)
) 

anscombe_fig <- ggpubr::ggscatter(data = anscombe_long, x = "x", y = "y", facet.by = "set") + 
  ylim(c(0, 16)) + 
  theme_bw()

anscombe_fig

ggpubr::ggscatter(data = anscombe_long, x = "x", y = "y", facet.by = "set", add = "reg.line") +
  ylim(c(0, 17)) + 
  ggpubr::stat_regline_equation(label.x = 5, label.y = 16) +
  ggpubr::stat_cor(label.x = 5, label.y = 14, digits = 2, p.accuracy = 0.01) + 
  theme_bw()

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: bad-ink-data-ratio

library(ggplot2)
library(dplyr)
library(cowplot)
library(grid)

base_irisplot <- dplyr::summarise(iris, Sepal_Width = mean(Sepal.Width), .by = Species) |> 
  ggplot() + 
  geom_col(aes(x = Species, y = Sepal_Width, fill = Species)) + 
  theme_bw()

g_circle <- grid::roundrectGrob(
  x = 0.85, y = 0.55, width = 0.2, height = 0.32, 
  r = unit(0.02, "snpc"),
  gp = gpar(col = "red", fill = NA, lwd = 3)
)

g_arrow <- grid::segmentsGrob(
  x0 = 0.8, y0 = 1.0,
  x1 = 0.7, y1 = 0.85,
  arrow = arrow(length = unit(0.15, "inches")),
  gp = gpar(col = "red", lwd = 2)
)

ggdraw(base_irisplot) +
  draw_grob(g_circle, x = 0, y = 0, width = 1, height = 1) + 
  draw_grob(g_arrow,  0, 0, 1, 1)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(Rdatasets)
library(marginaleffects)
library(lubridate)
library(dplyr)
library(ggplot2)

nightingale <- marginaleffects::get_dataset("Nightingale") |> 
  dplyr::mutate(Date = lubridate::ymd(Date))

tidyr::pivot_longer(
  nightingale, 
  cols = c("Disease.rate", "Wounds.rate", "Other.rate")
  ) |> 
    ggplot() + 
    geom_line(aes(x = Date, y = value, color = name)) + 
    theme_bw()

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(ggpubr)

ggpubr::ggviolin(
  data = ToothGrowth, 
  x = "dose", 
  y = "len"
  )

#
#
#
#
#
#
#
library(ggpubr)

ggpubr::ggdotplot(
  data = ToothGrowth, 
  x = "dose", 
  y = "len", 
  fill = "supp"
  )

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
set.seed(20261016)

male <- c(
  100, 120, 130, 140, 150, 800, 850, 1250, 1300, 1350,
  1650, 1700, 1750, 2100, 2750, 2800, 3050, 3100, 3150, 3200,
  3350, 3400, 4150, 5050, 5500, 5550, 5600, 6200, 6250, 6300
)

female <- c(
  200, 220, 650, 680, 700, 1100, 1800, 2200, 2250, 2700, 10000,12000
)

# 元の形を保ちながら、少しランダムな変動を加える
male <- round(male * exp(rnorm(length(male), sd = 0.025)))
female <- round(female * exp(rnorm(length(female), sd = 0.025)))

dat <- data.frame(
  sex = factor(
    rep(c("Male", "Female"), c(length(male), length(female))),
    levels = c("Male", "Female")
  ),
  ck_peak = c(male, female)
)


library(ggpubr)
library(dplyr)

dplyr::summarise(
  dat, 
  ck_peak = mean(ck_peak), 
  .by = sex
) |> 
  ggpubr::ggbarplot(
    x = "sex", 
    y = "ck_peak"
)

#
#
#
#
#
#
library(ggpubr)
library(dplyr)

dplyr::summarise(
  dat, 
  ck_peak = median(ck_peak), 
  .by = sex
) |> 
  ggpubr::ggbarplot(
    x = "sex", 
    y = "ck_peak"
)

```
#
#
#
#
#
#
#
#
#
ggbarplot(dat,
  x = "sex", y = "ck_peak",
  add = "mean_se"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
