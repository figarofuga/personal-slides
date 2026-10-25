# ggplot2だけで5つの例を描きます。QuartoのRチャンクでも使用できます。
# source("gestalt_ggplot.R")
# plot_gestalt_ggplot()

library(ggplot2)

plot_gestalt_ggplot <- function(family = "sans", blue = "#3185E8") {
  panels <- c("近接の法則", "類同の法則：色", "類同の法則：形",
              "連結", "囲み：共通領域")
  grid <- expand.grid(x = seq(1.1, 6.6, by = 1.1), y = c(1.5, 2.5, 3.5))
  dots <- do.call(rbind, lapply(panels, function(panel) {
    transform(grid, panel = panel)
  }))
  dots$panel <- factor(dots$panel, levels = panels)

  # 近接：このパネルだけ、点の横方向の間隔を変えます。
  dots$x[dots$panel == panels[1]] <- rep(c(1.1, 1.8, 3.5, 4.2, 5.9, 6.6), 3)

  # 類同：等間隔の点に対して、中央の行の色／形だけを変えます。
  dots$fill <- ifelse(dots$panel == panels[2] & dots$y == 2.5, blue, "white")
  dots$shape <- ifelse(dots$panel == panels[3] & dots$y == 2.5, 22, 21)

  links <- expand.grid(x = c(1.1, 3.3, 5.5), y = c(1.5, 2.5, 3.5))
  links$xend <- links$x + 1.1
  links$panel <- factor(panels[4], levels = panels)

  regions <- data.frame(
    xmin = c(0.7, 2.9, 5.1), xmax = c(2.6, 4.8, 7.0),
    ymin = 1.05, ymax = 3.95,
    panel = factor(panels[5], levels = panels)
  )

  captions <- data.frame(
    panel = factor(panels, levels = panels),
    label = c("近くにあるものがまとまって見える",
              "同じ色のものがまとまって見える",
              "同じ形のものがまとまって見える",
              "線でつながったものがまとまって見える",
              "同じ領域の中のものがまとまって見える")
  )

  ggplot() +
    # 背景の囲み → 線 → 点の順で描きます。
    geom_rect(data = regions,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#DFEDFC", colour = "#7195BD", linewidth = 0.6) +
    geom_segment(data = links, aes(x = x, y = y, xend = xend, yend = y),
                 colour = "#222222", linewidth = 0.7) +
    geom_point(data = dots, aes(x = x, y = y, shape = shape, fill = fill),
               colour = "#222222", size = 4.5, stroke = 0.8) +
    geom_text(data = captions, aes(x = 3.85, y = 0.65, label = label),
              family = family, size = 3.2) +
    # 指定した形番号・色コードをそのまま使います。
    scale_shape_identity() +
    scale_fill_identity() +
    facet_wrap(vars(panel), ncol = 3) +
    coord_fixed(xlim = c(0, 7.7), ylim = c(0.3, 4.4), expand = FALSE) +
    theme_void(base_family = family, base_size = 14) +
    theme(
      strip.text = element_text(face = "bold", margin = margin(b = 12)),
      panel.spacing = grid::unit(1.5, "lines"),
      plot.margin = margin(16, 16, 16, 16),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}
