library(camcorder)
gg_record(
  dir = file.path("Data/Output/Graphs/Cam/ggdist", "Income_ggdist_dotplot4"), 
  device = "png", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

  gg_resize_film(
  height = 16,
  width = 12,
  units = "in",
  dpi = 96
)

gg_playback(
  name = "Data/Output/Graphs/Cam/ggdist/Income_ggdist_dotplot4.gif",
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .1,
  image_resize = 2400
)
