## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi = 150,
  warning = FALSE,
  message = FALSE
)

## ----setup, message = FALSE---------------------------------------------------
library(ggcube)

## ----basics-------------------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy, z = drv, color = class)) +
      geom_point() +
      coord_3d()

## ----rotation, fig.height = 4-------------------------------------------------
ggplot(mpg, aes(displ, hwy, drv, color = class)) +
      geom_point() +
      coord_3d(pitch = 0, roll = 60, yaw = 0, dist = 1.4,
               ratio = c(2, 1, 1), panels = "all") +
      theme(panel.border = element_rect(color = "black"),
            panel.foreground = element_rect(alpha = .1))

## ----surface------------------------------------------------------------------
ggplot(mountain, aes(x, y, z)) +
      geom_surface_3d(aes(fill = z, color = z)) +
      scale_fill_viridis_c() + scale_color_viridis_c() +
      coord_3d(ratio = c(1.5, 2, 1), expand = FALSE, panels = "zmin",
               light = light(direction = c(1, 0, 0))) +
      guides(fill = guide_colorbar_3d()) +
      theme_light()

## ----points-------------------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy, z = drv, fill = class)) +
      geom_point_3d(size = 3, shape = 21, color = "black", stroke = .1,
                    ref_lines = TRUE, ref_faces = c("ymax", "xmax")) +
      coord_3d()

## ----paths--------------------------------------------------------------------
x <- seq(0, 20*pi, pi/16)
spiral <- data.frame(x = x, y = sin(x), z = cos(x), time = 1:length(x))
ggplot(spiral, aes(x, y, z, color = time)) +
      geom_path_3d() +
      scale_color_gradientn(colors = c("blue", "purple", "red", "orange")) +
      coord_3d() +
      theme_light()

## ----bar----------------------------------------------------------------------
ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
      geom_bar_3d(bins = 20, width = c(.5, 1)) +
      coord_3d(scales = "fixed", ratio = c(1, 3, .25), yaw = 60) +
      scale_z_continuous(expand = c(0, 0)) +
      theme(legend.position = "none")

## ----hull---------------------------------------------------------------------
ggplot(sphere_points, aes(x, y, z)) +
      geom_hull_3d(method = "convex", fill = "#9e2602", color = "#5e1600") +
      coord_3d()

## ----distributions------------------------------------------------------------
ggplot(iris, aes(y = Sepal.Length, x = Species, fill = Species)) +
      stat_distributions_3d() +
      scale_z_continuous(expand = expansion(mult = c(0, NA))) +
      coord_3d() +
      theme(legend.position = "none")

## ----text---------------------------------------------------------------------
df <- expand.grid(x = c("H", "B"), y = c("a", "o", "u"), z = c("g", "t"))
df$label <- paste0(df$x, df$y, df$z)
ggplot(df, aes(x, y, z, label = label, fill = x)) +
      geom_text_3d(method = "polygon", facing = "zmax",
                   size = 5, weight = "bold") +
      coord_3d(scales = "fixed", light = NULL)

## ----lighting-----------------------------------------------------------------
p <- ggplot(sphere_points, aes(x, y, z)) +
      geom_hull_3d(fill = "#9e2602", color = "#5e1600")

p + coord_3d(light = light(method = "direct", mode = "hsl",
                           direction = c(0, 0, 1)))

## ----zlim, fig.show = "hide"--------------------------------------------------
ggplot(mtcars, aes(mpg, wt, z = qsec)) +
      geom_point() +
      zlim(15, 20) +
      coord_3d()

## ----guide, fig.show = "hide"-------------------------------------------------
ggplot(mountain, aes(x, y, z, fill = z)) +
      stat_surface_3d(light = light(mode = "hsl", direction = c(1, 0, 0))) +
      guides(fill = guide_colorbar_3d()) +
      scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
      coord_3d()

## ----theme--------------------------------------------------------------------
ggplot(sphere_points, aes(x, y, z)) +
      geom_hull_3d() +
      coord_3d(panels = "all") +
      theme(panel.background = element_rect(color = "black"),
            panel.border = element_rect(color = "black"),
            panel.foreground = element_rect(alpha = .3),
            panel.grid.foreground = element_line(color = "gray", linewidth = .25),
            axis.text = element_text(color = "darkblue"),
            axis.text.z = element_text(color = "darkred"),
            axis.title = element_text(margin = margin(t = 30)),
            axis.title.x = element_text(color = "magenta"))

## ----annotate, eval = FALSE---------------------------------------------------
# summit <- filter(mountain, z == max(z))
# ggplot(mountain, aes(x, y, z)) +
#       geom_contour_3d(
#             annotate = list(
#                   annotate_3d("point", x = summit$x, y = summit$y, z = summit$z, color = "red"),
#                   annotate_3d("text", x = summit$x, y = summit$y, z = summit$z, color = "red",
#                               label = "Summit", fontface = "bold", vjust = -1)
#             ), fill = "black"
#       ) +
#       coord_3d(ratio = c(2, 3, 1.5), light = "none")

## ----face_projection----------------------------------------------------------
ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length,
                 color = Species, fill = Species)) +
      coord_3d() + xlim(4, 8) +
      stat_density_2d(position = position_on_face(faces = "zmin", axes = c("x", "y")),
                      geom = "polygon", alpha = .1, linewidth = .25) +
      geom_hull_3d(position = position_on_face("ymax"), alpha = .5) +
      geom_point_3d(shape = 21, color = "black", stroke = .25)

## ----animation, eval = FALSE--------------------------------------------------
# p <- ggplot(mountain, aes(x, y, z)) +
#       geom_contour_3d(fill = "black", color = "white", linewidth = .5) +
#       coord_3d(ratio = c(1.5, 2, 1), light = "none", zoom = 1.5) +
#       theme_void()
# 
# animate_3d(p, yaw = c(0, 360))

