# remotes::install_github("GuangchuangYu/hexSticker", force = TRUE)

library(hexSticker)
library(ggplot2)
library(glue)
library(magick)
library(fs)

img <- image_read(path(getwd(), "img", "logo", "img.png"))
logo <- image_ggplot(img, interpolate = TRUE)

sticker(
  logo,
  package = "ropenblas",
  p_size = 20,
  s_width = 1.3,
  s_height = 1.3,
  s_x = 1.1,
  s_y = 0.84,
  h_fill =  "#f8d7a1",
  h_color = "#2c3e50",
  p_color = "#2c3e50",
  p_family = "Aller_Rg",
  h_size = 2.4,
  white_around_sticker = T,
  filename = path(getwd(), "img", "logo", "logo.png"),
  url = "https://prdm0.github.io/ropenblas/",
  u_size = 4.5,
  spotlight = T,
  l_alpha = 0.6,
  dpi = 300,
  u_color = "#0F2536"
)

fs::file_delete(fs::path(getwd(), "logo.png"))

fs::file_copy(
  path = path(getwd(), "img", "logo", "logo.png"),
  new_path = path(getwd(), "logo.png")
)