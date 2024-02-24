if(.Platform$OS.type == "windows") {
  
  ft.url = "https://www.cufonfonts.com/download/font/whitney-2"
  dir.create(file.path(getwd(), "font"))
  download.file("https://www.cufonfonts.com/download/font/whitney-2", 
                "font/whitney-2.zip", mode = "wb")
  unzip(file.path("font/whitney-2.zip"), exdir = paste("font", basename(ft.url), sep = "/"))
  font_paths("font/whitney-2")
  font_add("Whitney", 
           regular = "whitneybook.otf", 
           bold = "whitneybold.otf",
           italic = "whitneybookitalic.otf", 
           bolditalic = "whitneysemibold.otf")
  
  showtext_auto()
  par(family = "Whitney")
  
}