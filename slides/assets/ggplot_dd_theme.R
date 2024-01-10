theme_dd <-
  function() {
    theme_minimal() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = "none"
      )
  }

hex_add_alpha <- function(col, alpha) {
  sprintf("%s%02X", col, floor(alpha * 256))
}

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_orange <- "#F38672"
dd_purple <- "#7E43B6"
dd_gray <- gray(0.2)
dd_pink <- "#C6227F"
dd_light_gray <- gray(0.8)


dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"

one_color_palette <- dd_light_blue
two_color_palette <- c(dd_dark_blue, dd_pink)
three_color_palette <- c(dd_light_blue, dd_orange, dd_pink)

one_color_palette_gray <- c(dd_dark_blue, dd_light_gray)
two_color_palette_gray <- c(two_color_palette, dd_light_gray)
three_color_palette_gray <- c(three_color_palette, dd_light_gray)


# plus gray
# 
# one_color_palette <- dd_light_blue
# two_color
# three_color