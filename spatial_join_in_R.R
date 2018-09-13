library("sp")
library("rgdal")
library("dplyr")
library("tidyr")
library("raster")
library("data.table")
library("ggplot2")
library("broom")
# shape file, which contains information for polygon
NC2010Trt <- rgdal::readOGR(".", 
                            layer = "2010_Census_trct")
# extract the Coordinate Reference Systems
NC2010Trt.crs <- sp::CRS(sp::proj4string(NC2010Trt))
# convert a spatial object into data.frame
NC2010Trt.points = broom::tidy(NC2010Trt)

map_NC2010_trt <- ggplot() + 
  #geom_polygon() +
  geom_path(data=NC2010Trt.points, aes(long,lat,group=group), color="black") +
  coord_equal() 

map_NC2010_trt

# population Centroid
NC_cen_cty <- data.table::fread("CenPop2010_Mean_TR37.txt", 
                                colClasses = c(STATEFP = "character", 
                                               COUNTYFP = "character", 
                                               TRACTCE = "character"))

# list of weather station in NC
NC_station <- data.table::fread("./list_station_NC.csv") %>% 
  tidyr::drop_na(LON, LAT)

# list of weather station in GA, SC, TN and VA
NC_surrounding_station <- data.table::fread("./list_station_NC_surrounding.csv") %>% 
  tidyr::drop_na(LON, LAT)

map_NC2010_trt_point <- map_NC2010_trt +
  geom_point(data=NC_station, aes(LON, LAT), color="red") +
  geom_point(data=NC_surrounding_station, aes(LON, LAT), color="green")

map_NC2010_trt_point

xy_NC_cen_cty <- NC_cen_cty[, c("LONGITUDE", "LATITUDE")]
xy_NC_station <- NC_station[, c("LON", "LAT")]
xy_NC_surrounding_station <- NC_surrounding_station[, 
                                                    c("LON", "LAT")]

# using the same CRS with the 2010 NC Tracts
spdf_NC_cen_cty <- sp::SpatialPointsDataFrame(coords = xy_NC_cen_cty, 
                                              data = NC_cen_cty, proj4string = NC2010Trt.crs)
# project the spatial object
new_CRS <- "+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79 +x_0=609601.22 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
spdf_NC_cen_cty_pj <- sp::spTransform(spdf_NC_cen_cty, 
                                      sp::CRS(new_CRS))


# using the same CRS with the 2010 NC Tracts
spdf_NC_station <- sp::SpatialPointsDataFrame(coords = xy_NC_station, 
                                              data = NC_station, proj4string = NC2010Trt.crs)
# project the spatial object
spdf_NC_station_pj <- sp::spTransform(spdf_NC_station, 
                                      sp::CRS(new_CRS))

# using the same CRS with the 2010 NC Tracts
spdf_NC_surrounding_station <- sp::SpatialPointsDataFrame(coords = xy_NC_surrounding_station, 
                                                          data = NC_surrounding_station, 
                                                          proj4string = NC2010Trt.crs)
# project the spatial object
spdf_NC_surrounding_station_pj <- sp::spTransform(spdf_NC_surrounding_station, 
                                                  sp::CRS(new_CRS))

# select the columns we need
NC_station_clean <- NC_station %>% dplyr::select("CTRY", 
                                                 "LAT", 
                                                 "LON", 
                                                 "STATE", 
                                                 "STATION NAME", 
                                                 "USAF", 
                                                 "WBAN", 
                                                 "combine_USAF_WBAN") %>%
  dplyr::mutate(combine_USAF_WBAN = as.character(combine_USAF_WBAN)) # make sure it's not int

NC_surrounding_station_clean <- NC_surrounding_station %>% 
  dplyr::select("CTRY", "LAT", "LON", "STATE", "STATION NAME", 
                "USAF", "WBAN", "combine_USAF_WBAN") %>%
  dplyr::mutate(combine_USAF_WBAN = as.character(combine_USAF_WBAN)) # make sure it's not int

# union the stations in NC and the stations in GA, SC, TN and VA
merge_station_clean <- rbind(NC_station_clean, NC_surrounding_station_clean)

# get lon, lat
xy_merge_station_clean <- merge_station_clean[, c("LON", 
                                                  "LAT")]
# define spatial object, using the same CRS with the 2010 NC Tracts
spdf_merge_station_clean <- sp::SpatialPointsDataFrame(coords = xy_merge_station_clean, 
                                                       data = merge_station_clean, 
                                                       proj4string = NC2010Trt.crs)
# project the spatial object
spdf_merge_station_clean_pj <- sp::spTransform(spdf_merge_station_clean, 
                                               sp::CRS(new_CRS))

# calculate distance between points
distance_station_cen <- rgeos::gDistance(spdf_merge_station_clean_pj, 
                                         spdf_NC_cen_cty_pj, byid = T)

# get the index of station from the distance matrix
index_crt_station <- apply(distance_station_cen, 1, 
                           which.min)

# merge the result of cloest station index back to the target dataframe
merge_weather_station_crt <- cbind(NC_cen_cty, index_crt_station) %>%
  mutate(index_crt_station=as.character(index_crt_station))

# for joining purpose
merge_station_clean$row_number <- row.names(merge_station_clean)


census_trt_join_station <- left_join(merge_weather_station_crt, 
                                     merge_station_clean, 
                                     by = c("index_crt_station"= "row_number")
)

head(census_trt_join_station)

census_trt_join_station <- sp::over(spdf_NC_station, NC2010Trt)

head(census_trt_join_station)

