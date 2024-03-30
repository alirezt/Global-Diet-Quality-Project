library(showtext)
font_paths("font/whitney-2")
font_add("Whitney", 
         regular = "whitneybook.otf", 
         bold = "whitneybold.otf",
         italic = "whitneybookitalic.otf", 
         bolditalic = "whitneysemibold.otf")

showtext_auto()
par(family = "Whitney")

