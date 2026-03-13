rm(list = ls())

library(tidyverse)
library(magick)

img_local <- caminhos_imagens <- list.files(
  path = "./fig/plot_ido_mes",
  pattern = "\\.png",
  full.names = TRUE
)

obj_img <- image_read(img_local) |>
  image_animate(fps = 1, optimize = TRUE)

image_write(obj_img, "./fig/gif/test_gif_morth0_fps1.gif")
