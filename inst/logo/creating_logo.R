# remotes::install_github("GuangchuangYu/hexSticker")

library(hexSticker)

imgurl <- system.file("~/Downloads/ropenblas/inst/openblas_logo_01.png", package="hexSticker")

sticker("~/Downloads/ropenblas/inst/molde.png", package="", p_size=22, s_x=1.03, s_y=.95, s_width=0.9,
        filename="openblas_logo.png", h_fill="#000000", h_color="#929292", white_around_sticker = T)