#Script to get country shapefiles 
#15June23 the dashboard was down because the link to the shapefiles was no longer 
#valid. This changes the process so that it doesn't have to access it each time. 

library(sf)
library(giscoR)

world <- gisco_get_countries(year = "2016") 

save(world, file = "countryShapeFiles.RData")

