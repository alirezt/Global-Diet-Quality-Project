library(camcorder)
gg_record(
  dir = file.path("Data/Output/Graphs/Cam/pdf", "Income pooled"), 
  device = "pdf", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

gg_resize_film(
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)

gg_playback(
  name = "Data/Output/Graphs/Cam/Sierra Leone_Yearly-Change_All-Indicators.gif",
  first_image_duration = 16,
  last_image_duration = 20,
  frame_duration = .15,
  image_resize = 600
)