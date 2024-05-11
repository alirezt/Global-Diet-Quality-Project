dqqMap <- function(data, var, group, style, plt, flip = "no"){
  
  data <-  data %>%
    filter(Indicator == var & Subgroup == group)
  
  if (flip == "no"){
    if (var == "Dietary diversity score" | var == "NCD-Protect" | var == "NCD-Risk" | var == "GDR score"){
      dqq_world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
        left_join(data, by = join_by(iso_a3 == ISO3)) 
      
      plot(dqq_world[-which(dqq_world$sovereignt == "Antarctica"), ]["Mean_prevalence"],
           main = var,
           key.pos = 4, 
           axes = FALSE,
           pal = colorRampPalette(plt),
           breaks = style,
           #pal = colorRampPalette(c("#D1FBD4","#579C97", "#0E3F5C")),
           #pal = hcl.colors(11, "Dark Mint", rev = TRUE),
           #nbreaks = 10,
           #breaks = c(0, 20, 40, 60, 80, 100),
           key.width = lcm(1.3), key.length = 0.75)
    }
    
    else{
      dqq_world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
        left_join(data, by = join_by(iso_a3 == ISO3))
      
      plot(dqq_world[-which(dqq_world$sovereignt == "Antarctica"), ]["Mean_prevalence"],
           main = var,
           key.pos = 4, 
           axes = FALSE,
           pal = colorRampPalette(plt),
           breaks = style,
           key.width = lcm(1.3), key.length = 0.75)
    }
  }
  
  else if (flip == "yes"){
    if (var == "Dietary diversity score" | var == "NCD-Protect" | var == "NCD-Risk" | var == "GDR score"){
      dqq_world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
        left_join(data, by = join_by(iso_a3 == ISO3)) 
      
      plot(dqq_world[-which(dqq_world$sovereignt == "Antarctica"), ]["Mean_prevalence"],
           main = var,
           key.pos = 4, 
           axes = FALSE,
           pal = colorRampPalette(rev(plt)),
           breaks = style,
           key.width = lcm(1.3), key.length = 0.75)
    }
    
    else{
      dqq_world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
        left_join(data, by = join_by(iso_a3 == ISO3))
      
      plot(dqq_world[-which(dqq_world$sovereignt == "Antarctica"), ]["Mean_prevalence"],
           main = var,
           key.pos = 4, 
           axes = FALSE,
           pal = colorRampPalette(rev(plt)),
           breaks = style,
           key.width = lcm(1.3), key.length = 0.75)
    }
  }
  
}
