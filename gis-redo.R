#### HEADER ####
# haven johansen
# spring 2026
# attempting hispaniola healthcare site/elevation/coastline geospatial analysis

#### WORKSPACE ####

rm(list=ls())
graphics.off()

library(terra)
library(geodata)

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
sites_hti = subset(sites_hti, sites_hti$amenity %in% c("hospital", "pharmacy", "clinic", "doctors")) # nrow 1988
# confirmed successful 

# subset to remove dentist, NA
sites_dom = subset(sites_dom, sites_dom$amenity %in% c("hospital", "pharmacy", "clinic", "doctors")) # nrow 945
# confirmed successful

#### COORDINATE SYSTEM ####

crs(hti)
# WGS84
crs(dom)
# WGS84
crs(elevation)
# WGS84
crs(sites_hti)
# WGS84
crs(sites_dom)
# WGS84

#### POINTS TO POLYGONS ####

# combine dom and hti polygons
dom_hti_test = combineGeoms(dom, hti)
dom_hti = aggregate(dom_hti_test)
# plots look good

# polygon to line
dom_hti_line = as.lines(dom_hti)
# plot looks good

#### MAKE POINTS TALK TO EACH OTHER #### 

# create dataframe, extract and then attach to points layer as an attribute
hti_df <- terra::extract(
  x = elevation,
  y = sites_hti,
  na.rm = TRUE
)
# one column point IDs, one column associated values extracted from raster

# repeat for dom
dom_df = terra::extract(
  x = elevation,
  y = sites_dom,
  na.rm = TRUE
)

# add dataframe to attribute table
elevation_merge_hti = merge(sites_hti, hti_df, by.x=c('NAME_1', 'NAME_2'), by.y=c('District', 'Canton'))

# 
distance_test = distance(dom_hti_line, sites_dom)
# no

coord_dom = crds(sites_dom)

#
geosphere::dist2Line(coord_dom, dom_hti_line)


# create blank raster and call distance into raster
dom_hti_rast = rast(ext = ext(dom_hti_line), res = 0.009, crs = crs(dom_hti_line))
line_rast = rasterize(dom_hti_line, dom_hti_rast)
# calculate distance
dom_hti_dist = distance(line_rast)
# mask to original polygon
dom_hti_dist = mask(dom_hti_dist, dom_hti)

plot(dom_hti_dist)
plot(dom_hti, add = TRUE)
