## @knitr plot-us-county-map

# test <- tribble(
#   ~Method, ~n, ~"50th percentile APE", ~"90th percentile APE",
#   "iTFR", countyeval$tot, paste0(format(round(countyeval$ilagTFR50,1), nsmall=1),"%"),	 paste0(format(round(countyeval$ilagTFR90,1), nsmall=1),"%"),
#   "xTFR", countyeval$tot, paste0(format(round(countyeval$xlagTFR50,1), nsmall=1),"%"),paste0(format(round(countyeval$xlagTFR90,1), nsmall=1),"%")
# )

pal <- c("#4475B5", "#849EBA", "#BFCCBD", "#ffffbf", "#F9B984", "#ED7550", "#D62F26")

ok = is.finite( countiestfr$xTFR)

a <- tm_shape(countiestfr[ok,], projection = "laea_NA") +
  tm_fill("xTFR",
          title = "TFR",
              palette = pal ,
              breaks= c(-Inf, 1.7, 1.9, 2.1, 2.3,2.5, 2.7, Inf),
              auto.palette.mapping = FALSE,
              id = "GEOID",
              alpha = 0.9,
              legend.show = TRUE
  ) +
 tm_shape(states) +
  tm_borders(lwd=0.15, col="white", alpha = 1)  +
  tm_layout(legend.position = c("left","bottom"))

a

# # save_tmap(a, filename="FIGURES/top.pdf")
# 
# stable <- ggtexttable(test, theme = ttheme("blank"), rows=NULL)
# 
# anual <- pdf_render_page('FIGURES/top.pdf',
#                          page = 1, dpi = 300, numeric = FALSE)
# 
# top <- ggdraw() + draw_image(anual)
# 
# plot_grid(top, stable, ncol=1, rel_widths = c(.85, 1), rel_heights = c(1,0.15), scale = c(1,1))