# haven johansen
# spring 2026
# hispaniola healthcare site/elevation/coastline geospatial analysis

#### WORKSPACE ####

rm(list=ls())
graphics.off()

library(terra)

here::here("/Users/havenjohansen/Documents/github/hospitals-hti-dom")

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
sites_hti = terra::subset(sites_hti, sites_hti$amenity == c("hospital", "pharmacy", "clinic", "doctors")) # nrow 1988
# s4 warning need to check

# subset to remove dentist, NA
sites_dom = subset(sites_dom, sites_dom$amenity %in% c("hospital", "pharmacy", "clinic", "doctors")) # nrow 945
# confirmed successful

# combine sites
sites_dom_hti = union(sites_hti, sites_dom)
# note to check that the data cleaning turned out the same in arcgis

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

# create generic ID, create dataframe, extract elevation values for points and attach to attribute
sites_dom_hti$id_check = 1:nrow(sites_dom_hti)

df_dom_hti = terra::extract(
  x = elevation,
  y = sites_dom_hti,
  na.rm = TRUE,
)

sites_dom_hti$elevation = df_dom_hti[, 2]
# plot-wise this looks wrong. also there are some repeat entries

#### DISTANCE TO COASTLINE ####

# create blank raster and call line into raster
dom_hti_rast = rast(ext = ext(dom_hti_line), res = 0.009, crs = crs(dom_hti_line))
line_rast = rasterize(dom_hti_line, dom_hti_rast)
# calculate distance
dom_hti_dist = terra::distance(line_rast)
# mask to original polygon
dom_hti_dist = mask(dom_hti_dist, dom_hti)
# masking back to polygon instead of line here -- returns a blank plot if you mask to line

plot(dom_hti_dist)
plot(dom_hti_line, add = TRUE)

# create generic ID, create dataframe, extract distance values for points and attach to attribute
dom_hti_line$id_check = 1:nrow(dom_hti_line)

dist_df = terra::extract(
  x = dom_hti_dist,
  y = sites_dom_hti,
  na.rm = TRUE,
)

dom_hti_line$distance = dist_df[, 2]

plot(dist_df, col = map.pal("elevation", 3))
# units are meters. concerned that there are 0s


