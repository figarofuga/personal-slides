# 追加パッケージ不要。QuartoのRチャンクからもそのまま呼び出せます。
# source("gestalt_base.R")
# plot_gestalt()

plot_gestalt <- function(family = "sans", blue = "#3185E8") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # 上段に3例、下段に2例。
  layout(rbind(c(1, 1, 2, 2, 3, 3), c(4, 4, 4, 5, 5, 5)))
  par(mar = c(2.5, 0.5, 2.5, 0.5), family = family)

  panel <- function(title_text, caption) {
    plot(NA, xlim = c(0, 7.7), ylim = c(0.6, 4.4), asp = 1,
         axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
    title(main = title_text, cex.main = 1.1, line = 0.6)
    mtext(caption, side = 1, line = 0.5, cex = 0.85)
  }

  dots <- function(d, shape = 21, fill = "white") {
    # pch=21は円、pch=22は四角。bgで塗り色を指定します。
    points(d$x, d$y, pch = shape, bg = fill,
           col = "#222222", cex = 2.2, lwd = 1.6)
  }

  grid <- expand.grid(x = seq(1.1, 6.6, by = 1.1), y = c(1.5, 2.5, 3.5))

  panel("近接の法則", "近くにあるものがまとまって見える")
  close_grid <- expand.grid(x = c(1.1, 1.8, 3.5, 4.2, 5.9, 6.6),
                            y = c(1.5, 2.5, 3.5))
  dots(close_grid)

  panel("類同の法則：色", "同じ色のものがまとまって見える")
  dots(grid, fill = ifelse(grid$y == 2.5, blue, "white"))

  panel("類同の法則：形", "同じ形のものがまとまって見える")
  dots(grid, shape = ifelse(grid$y == 2.5, 22, 21))

  panel("連結", "線でつながったものがまとまって見える")
  # 線を先に描き、その上に点を重ねます。
  links <- expand.grid(x = c(1.1, 3.3, 5.5), y = c(1.5, 2.5, 3.5))
  segments(links$x, links$y, links$x + 1.1, links$y,
           col = "#222222", lwd = 1.6)
  dots(grid)

  panel("囲み：共通領域", "同じ領域の中のものがまとまって見える")
  # 点の間隔は等間隔のまま、2列ずつ囲みます。
  rect(xleft = c(0.7, 2.9, 5.1), ybottom = 1.05,
       xright = c(2.6, 4.8, 7.0), ytop = 3.95,
       col = "#DFEDFC", border = "#7195BD", lwd = 1.2)
  dots(grid)

  invisible(NULL)
}
