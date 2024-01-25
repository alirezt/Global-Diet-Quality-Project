#install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='alireza-taghdisian',
                          token='607F23B08A17CC0AE7B7CDF755048184',
                          secret='aUwlMT7RQV0g3cW9j6th8hUYZBJ+bcEzUa9zgfjo')

rsconnect::deployApp('shiny/dqqShiny_2', 
                     account = "alireza-taghdisian", 
                     server = 'shinyapps.io')
