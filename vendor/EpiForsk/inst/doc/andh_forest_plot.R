## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(EpiForsk)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(grDevices)

## ----eval = FALSE-------------------------------------------------------------
# windowsFonts(Calibri = windowsFont("TT Calibri"))
# windowsFonts(Helvetica = windowsFont("TT Helvetica"))
# windowsFonts(Arial = windowsFont("TT Arial"))

## -----------------------------------------------------------------------------
plot_data <- andh_forest_data %>%
  mutate(
    order = rev(row_number()) + 1,
    text = paste0(
      case_when(
        indent == 0 ~ "", 
        indent == 1 ~ "  ", 
        indent == 2 ~ "    ",
        TRUE ~ "      "
      ),
      text
      )
    ) %>%
  pivot_longer(
    cols = A_est:C_u, 
    names_to = c("headline", ".value"),
    names_sep = "_",
    values_to = "value")

## -----------------------------------------------------------------------------
plot_options <- data.frame(
  headline       = c(   "A",   "B",   "C"),
  color          = c("red2", "dodgerblue2", "springgreen2"),
  fill           = c("red4", "dodgerblue4", "springgreen4"),
  lower_vertical = c(  -0.2,  -0.4,  -0.2),
  mid_vertical   = c(     0,     0,     0),
  upper_vertical = c(   0.2,   0.1,   0.2),
  xlim_lower     = c(  -0.3,  -0.5,  -0.2),
  xlim_upper     = c(   0.3,   0.3,   0.2),
  headline_text  = c("Language",
                     "Mathematics",
                     "Intelligence"),
  xlab           = c("Difference in language z-score\ncompared with no exposure",
                     "Difference in mathematics z-score\ncompared with no exposure",
                     "Difference in IQ\ncompared with no exposure")
)

## -----------------------------------------------------------------------------
shared_theme <- theme(
  legend.position = "none",
  plot.title = element_text(
    face = "bold", 
    family = "Arial",
    size = 12, 
    hjust = 0.5, 
    vjust = 1
  ),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks.length = unit(0.20,"cm"),
  axis.title.x = element_text(
    margin = margin(15, 0, 0, 0), 
    size = 10, 
    face = "bold", 
    family = "Arial"
  ),
  axis.text.x = element_text(
    size = 12, 
    face = "plain", 
    family = "Arial", 
    margin = margin(5, 0, 0, 0)
  ),
  axis.line.x = element_line(size = 0.5),
  axis.title.y = element_blank(),
  axis.line.y = element_line(color = "white", size = 0.5),
  axis.ticks.y = element_blank()
)

## -----------------------------------------------------------------------------
plot_list <- lapply(plot_options$headline, function(x) {
  option <- plot_options %>% filter(headline == x)
  plot_data %>% 
    filter(headline == x) %>%
    slice(-1) %>%
    ggplot(aes(y = order)) + 
    geom_linerange(
      aes(xmin = l, xmax = u),
      color = option$color,
      alpha = 0.7,
      size = 1.2
    ) +
    geom_point(
      aes(x = est), 
      size = 2, 
      shape = 22, 
      color = "black",
      fill = option$fill
    ) +
    geom_vline(
      xintercept = option$lower_vertical, 
      linetype = 2,
      alpha = 0.5
    ) + 
    geom_vline(
      xintercept = option$mid_vertical, 
      linetype = 2
    ) + 
    geom_vline(
      xintercept = option$upper_vertical, 
      linetype = 2,
      alpha = 0.5
    ) +
    coord_cartesian(
      xlim = c(option$xlim_lower, option$xlim_upper), 
      ylim = c(0, 19), 
      expand = FALSE
      ) + 
    xlab(option$xlab) +
    ggtitle(option$headline_text) +
    annotate(
      "text", 
      label = "suggests benefit", 
      x = 0.025, 
      y = 1, 
      size = 2,
      color = "black", 
      fontface = "italic", 
      hjust = 0
    ) + 
    annotate(
      "text",
      label = "suggests harm", 
      x = -0.025, 
      y = 1, 
      size = 2,
      color = "black", 
      fontface = "italic",
      hjust = 1
    ) + 
    theme_bw() +
    shared_theme + 
    theme(
      plot.title = element_text(color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.text.y = element_blank()
    )
})

## -----------------------------------------------------------------------------
label_data <- plot_data %>%
  group_by(type, text, order) %>%
  slice(1) %>%
  ungroup() 
# %>% mutate(text = str_replace(text, "beta", "\u03b2"))

## -----------------------------------------------------------------------------
label_plot <- label_data %>%
  ggplot(aes(y = order)) + 
  geom_point(aes(x = est)) +
  xlab(plot_options$xlab[1]) +
  ggtitle(plot_options$headline_text[1]) +
  coord_cartesian(
    xlim = c(98,99), 
    ylim = c(0, 19),
    expand = FALSE
    ) + 
  scale_y_continuous(
    breaks = label_data$order, 
    labels = label_data$text
  ) +
  theme_bw() +
  shared_theme + 
  theme(
    plot.title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.line.x = element_line(color = "white"),
    axis.ticks.x = element_line(colour = "white"),
    axis.text.y = element_text(
      color = "black",
      hjust = 0,
      face = label_data$type
    )
  )


## ----fig.height = 5, fig.width = 10, warning = FALSE--------------------------
plot_grid(plotlist = c(list(label_plot), plot_list), 
          rel_widths = c(1.1, 1, 1, 1), 
          ncol = 4)

