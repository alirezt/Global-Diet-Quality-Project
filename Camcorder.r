library(camcorder)
gg_record(
  dir = file.path("Data/Output/Graphs/Cam/pdf", "Sierra Leone_Yearly-Change_All-Indicators"), 
  device = "png", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

  gg_resize_film(
  height = 12,
  width = 12,
  units = "in",
  dpi = 96
)

gg_playback(
  name = "Data/Output/Graphs/Cam/Sierra Leone_Yearly-Change_All-Indicators.gif",
  first_image_duration = 16,
  last_image_duration = 20,
  frame_duration = .15,
  image_resize = 600
)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5), x3 = NA, x4 = NA)
rowSums(is.na(x[, c("x1", "x3")]), na.rm = T)
