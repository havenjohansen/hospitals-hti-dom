# haven johansen
# spring 2026
# hispaniola healthcare site/elevation/coastline geospatial analysis

#### WORKSPACE ####

rm(list=ls())
graphics.off()

library(terra)
# library(raster)
library(sf)

# load country shapefiles
hti = geodata::gadm(country = "HTI", level = 0, path = ".") 
dom = geodata::gadm(country = "DOM", level = 0, path = ".")

# load elevation raster
elevation = rast("output_hh.tif")

# load healthcare site files
sites_hti = vect("Haiti") # nrow 2049
sites_dom = vect("Dominican Republic") # nrow 1081
# confirmed 4 vectors 1 raster

#### DATA CLEANING ####

# subset to remove places of worship, dentist, NA
sites_hti = terra::subset(sites_hti, sites_hti$amenity %in% c("hospital", "pharmacy", "clinic", "doctors")) # nrow 1988

# subset to remove dentist, NA
sites_dom = terra::subset(sites_dom, sites_dom$amenity %in% c("hospital", "pharmacy", "clinic", "doctors")) # nrow 945

# combine sites
sites_dom_hti = union(sites_hti, sites_dom)

#### COORDINATE SYSTEM (un-comment if you want to re-check) ####

# crs(hti)
# crs(dom)
# crs(elevation)
# crs(sites_dom_hti)
# all WGS 84

#### POINTS TO POLYGONS ####

# combine dom and hti polygons
dom_hti_test = combineGeoms(dom, hti)
dom_hti = aggregate(dom_hti_test)
# plots look good

# polygon to line
dom_hti_line = as.lines(dom_hti)
# plot looks good

#### ELEVATION ####

# create dataframe, extract elevation values for points and attach to attribute
df_dom_hti = terra::extract(
  x = elevation,
  y = sites_dom_hti,
  na.rm = TRUE,
)

sites_dom_hti$elevation = df_dom_hti[, 2]

plot(sites_dom_hti, 
     breaks = c(0, 10, 100, 1000), 
     "elevation")

plot(dom_hti_line, add = TRUE)
# there are some repeat entries, I think that sites can count as more than one thing
# I believe the bottom of the map is getting cut off in the health site data and that's carrying over, might be one or two points missing


#### DISTANCE TO COASTLINE ####

sites_sf = sf::st_as_sf(sites_dom_hti)
line_sf = sf::st_as_sf(dom_hti_line)

dist_test = st_distance(line_sf,
                        sites_sf)

distance_df = dist_test %>%
  t() %>% # transpose to set up columns vs. rows for data frame?
  as.data.frame

units(distance_df$.) = "km"  # set to kilometers 

# full data frame with raster IDs
sites_dom_hti$distance = distance_df[, 1]

plot(sites_dom_hti, 
     breaks = c(0, 10, 50, 100), 
     "distance")

plot(dom_hti_line, add = TRUE)
# seems that some of these are getting misclassified
# zeros are still concerning
