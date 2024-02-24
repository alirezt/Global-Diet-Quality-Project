library(camcorder)
gg_record(
  dir = file.path("Data/Output/Graphs/Cam/Box plots", "UrbanDiff Pmeat_BarPlot_1"), 
  device = "png", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

  gg_resize_film(
  height = 8,
  width = 5.1,
  units = "in",
  dpi = 300
)

gg_playback(
  name = "Data/Output/Graphs/Cam/DDS_All_2.gif",
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .1,
  image_resize = 2400
)
