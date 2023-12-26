
### Geospatial workshop
## Dataset: NEON (https://data.neonscience.org/)



## Libraries that you would need
# install.packages(c("dplyr", "ggplot2", "raster", "rasterVis", "sf"))
library(tidyverse)
library(tidyr)
library(rasterVis)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tigris) # for US info (Census)
library(ggspatial) # For adding the scale bar and North arrow
library(maps)
library(ggspatial) # For adding scale bar and North arrow
library(cowplot)
library(raster)

## Google earth engine to find rasters

## Load the data 
Africa_TP_2019 <- raster::raster("Spatial_R/Africa_Transmission_Pot_2019.tif")
Africa_TP_2019
# res 0.1 is about 15KM?
#dev.off if getting the error, Error in plot.new() : figure margins too large
plot(Africa_TP_2019) #needs package raster to be loaded for it to show


Africa_TP_2019_df <- terra::as.data.frame(Africa_TP_2019, xy = TRUE)
Africa_TP_2019_df #NAs are area in the oceana

ggplot() +
  geom_raster(data = Africa_TP_2019_df , aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_viridis_c() ## Color-blind friendly

ggplot() +
  geom_raster(data = Africa_TP_2019_df , aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_viridis_c(option = "magma",direction = -1)

## Customise a palette: using: https://r-charts.com/color-palette-generator/ #copy and paste as a vector

custom_palette <- rev(c("#c3271b", "#c53020", "#c83926", "#ca402b", "#cc4730", "#ce4e36", "#d0543b", "#d25b41",
                        "#d46146", "#d6674c", "#d76d51", "#d97357", "#db785c", "#dc7e62", "#de8467", "#df896d", 
                        "#e08f73", "#e19579", "#e29a7f", "#e3a084", "#e4a58a", "#e5ab90", "#e5b096", "#e6b69c",
                        "#e6bba2", "#e7c1a8", "#e7c6ae", "#e7ccb5", "#e7d1bb", "#e7d7c1"))


ggplot() +
  geom_raster(data = Africa_TP_2019_df , aes(x = x, y = y, fill = Africa_Transmission_Pot_2019))+
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent")
  
## what if you want it to be categorical 
## Quick histogram 
ggplot()+
  geom_histogram(data=Africa_TP_2019_df, aes(Africa_Transmission_Pot_2019))

## Cutting the data in 3 
Africa_TP_2019_df <- Africa_TP_2019_df %>% 
  mutate(fct_TP = cut(Africa_Transmission_Pot_2019, breaks = 3))

 ggplot()+
  geom_bar(data = Africa_TP_2019_df, aes(fct_TP))

## You can custom the bins
custom_bin <- c(0,1,2,3)

Africa_TP_2019_df <- Africa_TP_2019_df %>% 
  mutate(fct_TP2 = cut(Africa_Transmission_Pot_2019, breaks = custom_bin))

### For the above, we are creating our own colors
## ## Customise a palette: using: https://r-charts.com/color-palette-generator/

my_colors <- c("#b1b7ff", "#d87298", "#ff2c30")

ggplot(data = Africa_TP_2019_df, aes(x=x, y=y, fill= fct_TP2))+
  geom_raster()+
  scale_fill_manual(values = my_colors, "TP")+
  theme(axis.title = element_blank())+
  coord_sf() #coord_quickmap()



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Multi-Band Raster
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

Africa_TP_2010.2019 <- raster::stack("Spatial_R/Africa_Transmission_Pot_2010-2019.tif")
Africa_TP_2010.2019
plot(Africa_TP_2010.2019)

Africa_TP_2010.2019_df <- terra::as.data.frame(Africa_TP_2010.2019, xy = TRUE)
Africa_TP_2010.2019_df

##For a stack, you need to work with the long format
Africa_TP_2010.2019_df_Long <- tidyr::pivot_longer(Africa_TP_2010.2019_df, cols = 3:13, names_to = "Layer")

ggplot() +
  geom_raster(Africa_TP_2010.2019_df_Long, mapping = aes(x, y, fill=value)) +
  scale_fill_viridis_c() +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent") +
  facet_wrap(~Layer)

#single band/layer
ggplot() +
  geom_raster(data = Africa_TP_2019_df , aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent") 

## Multiband
ggplot() +
  geom_raster(data=Africa_TP_2010.2019_df_Long, aes(x, y, fill=value)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent") +
  facet_wrap(~Layer)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Plotting - overlayed rasters 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

## Population Density 

Pop <- raster("Spatial_R/SEDAC_POP_2000-01-01_rgb_3600x1800.tiff")
Pop
plot(Pop)
Pop <- raster::reclassify(Pop, cbind(255, NA)) ## caused they assigned a value to the ocean 
plot(Pop)

library(paletteer) ## Packages for colours 
plot(Pop,col=rev(paletteer_c("grDevices::YlOrRd", 30)))

## Pop - extent is the world - your Transmission potential map is of the African region only 
# You need to crop it to the same extent and make sure it has the same projections 
Africa_TP_2019
Pop

### Make sure they have the same projections 
reprojected_Pop <- projectRaster(Pop,Africa_TP_2019)
plot(reprojected_Pop,col=rev(paletteer_c("grDevices::YlOrRd", 30)))

pop.tp <- reprojected_Pop*Africa_TP_2019
plot(pop.tp)

## ggplot does not interprete that you need only the part tht overlaps and thus you need a bit of cropping
pop_crop <- raster::crop(Pop, extent(Africa_TP_2019))

## sort of a bivariate map 
ggplot() +
  geom_raster(data = Africa_TP_2019_df, aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent") +
  geom_raster(data = as.data.frame(pop_crop, xy = TRUE), aes(x, y, alpha = SEDAC_POP_2000.01.01_rgb_3600x1800)) +
  scale_alpha_continuous(range = c(0, 1), name = "Pop",) +
  theme(legend.position = "right")  # Adjust the legend position


### If we have different projections 
#### Projections ######

DTM_HARV <- raster("Spatial_R/NEON_data_HARV_SJER_class/NEON_data_HARV_SJER_class/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")
DTM_hill_HARV <- raster("Spatial_R/NEON_data_HARV_SJER_class/NEON_data_HARV_SJER_class/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_DTMhill_WGS84.tif")
plot(DTM_hill_HARV)

DTM_HARV_df <- as.data.frame(DTM_HARV, xy=T)
DTM_hill_HARV_df <- as.data.frame(DTM_hill_HARV, xy=T)

ggplot()+
  geom_raster(data = DTM_HARV_df, aes(x=x, y=y, fill= HARV_dtmCrop))+
  geom_raster(data= DTM_hill_HARV_df, aes(x=x, y=y, alpha= HARV_DTMhill_WGS84)) + 
  scale_fill_gradientn(colors=terrain.colors(10),name = "Elevation")+
  coord_quickmap()
## The above will not work because the raster are not in the same projection (crs)

crs(DTM_HARV)
crs(DTM_hill_HARV)
## So we want them to be the same
DTM_hill_HARV_reproject <- raster::projectRaster(DTM_hill_HARV, crs= crs(DTM_HARV))
crs(DTM_hill_HARV_reproject)

## So we try again 
# we turn it into a dataframe again 

DTM_hill_HARV_reproject_df <- as.data.frame(DTM_hill_HARV_reproject, xy=T)

ggplot()+
  geom_raster(data = DTM_HARV_df, aes(x=x, y=y, fill= HARV_dtmCrop))+
  #geom_raster(data= DTM_hill_HARV_reproject_df, aes(x=x, y=y, alpha= HARV_DTMhill_WGS84)) + 
  scale_fill_gradientn(colors=terrain.colors(10),name = "Elevation")+
  coord_quickmap()

##### making calculations 
### 
## DSM - gives you the height of the landscape that was scanned during a flight (top of trees etc)
## DTM - gives the actual ground model 
## If you substract one from the other you get the canopy height 

DSM_HARV <- raster("Spatial_R/DSM_HARV_small.tiff")
plot(DSM_HARV)
DTM_HARV <- raster("Spatial_R/DTM_HARV_small.tiff")
plot(DTM_HARV)

## Canopy height model (you subtracting the rasters)
# You rasters need to overlaps - same extent - same CRS etc
CHM_HARV <- DSM_HARV - DTM_HARV 

CHM_HARV_df <- as.data.frame(CHM_HARV, xy=T)

ggplot()+
  geom_raster(data= CHM_HARV_df, aes(x=x,y=y, fill= layer))+
  scale_fill_gradientn(name="Canopy Height", colors=terrain.colors(10))+
  coord_quickmap()

ggplot()+
  geom_raster(data= CHM_HARV_df, aes(x=x,y=y, fill= layer))+
  scale_fill_gradient(name="Canopy Height", low="gold2", high="darkgreen")+
  coord_quickmap()

### Playing with the Resolution 
#
## Aggregate - making the resolution bigger - which is not necessarily good   
CHM_HARV_agg <- aggregate(CHM_HARV, fact = 10, fun= mean) # from 1mx1m to 10mx10m

## turn it into a df so you can plot it with ggplot
CHM_HARV_agg_df <- as.data.frame(CHM_HARV_agg, xy = T )

ggplot()+
  geom_raster(data= CHM_HARV_agg_df, aes(x=x,y=y, fill= layer))+
  scale_fill_gradient(name="Canopy Height", low="gold2", high="darkgreen")+
  coord_quickmap()


### If you want to have better resoltuion, i.e going from 10x10 to 1x1
## if now you want to disaggregate the pixel (i.e have more pixel) Better resolution
# but R would assign the same value from the one pixel to how many pixels its been divided to 
CHM_HARV_disag <- disaggregate(CHM_HARV_agg, fact= 10)

CHM_HARV_disag_df <- as.data.frame(CHM_HARV_disag, xy=T)

## You'll get the same map !! ie still a 10x10 resolution
ggplot()+
  geom_raster(data= CHM_HARV_disag_df, aes(x=x,y=y, fill= layer))+
  scale_fill_gradient(name="Canopy Height", low="gold2", high="darkgreen")+
  coord_quickmap()

## To correct that, there are different methods of dis-aggregating that 

CHM_HARV_disag2 <- disaggregate(CHM_HARV_agg, fact= 10, method = "bilinear")

CHM_HARV_disag_df2 <- as.data.frame(CHM_HARV_disag2, xy=T)

# Still quite not the same as the original - so thats something you have to bare in mind when changing the resolutions
ggplot()+
  geom_raster(data= CHM_HARV_disag_df2, aes(x=x,y=y, fill= layer))+
  scale_fill_gradient(name="Canopy Height", low="gold2", high="darkgreen")+
  coord_quickmap()

#################################### Vector data ###########################################
#Vector - are not pixels, they are lines/points/polygons
# Format - mostly shapefiles (extension .shp)
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### SHAPEFILE 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

#Examples of shapefiles 
# you can generally manage them like any dataframe 
# Point
point_HARV <- st_read("Spatial_R/NEON_data_HARV_SJER_class/NEON_data_HARV_SJER_class/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

# Line 
roads_HARV <- st_read("Spatial_R/NEON_data_HARV_SJER_class/NEON_data_HARV_SJER_class/NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
roads_HARV$TYPE
## I want to keep footpath
footpath_HARV <- roads_HARV %>% 
  filter(TYPE =="footpath")


## plot the lines 

road_colors <- c("blue","green", "navy", "purple")

ggplot()+
  geom_sf(data= roads_HARV, aes(color= TYPE))+
  scale_color_manual(values=road_colors)+
  labs(color= "Road Type")+
  ggtitle("NEON Harvard Site", subtitle = "Roads and trails")+
  coord_sf()

ggplot()+
  #geom_sf(data= aoi_HARV, fill = "grey", color = "black")+
  geom_sf(data = roads_HARV, aes(color=TYPE), size = 1)+
  geom_sf(data = point_HARV)+
  coord_sf()

## Examples ## 
####### Counties and extract only Florida
options(tigris_use_cache = TRUE)
us_counties <- tigris::counties(class = "sf") ## all counties in the US

Florida_map_counties <- us_counties %>% 
  filter(STATEFP == 12) # you can check this in the state object above

ggplot()+
  geom_sf(data = Florida_map_counties) +
  coord_sf()

# Zoom into an area
ggplot() +
  geom_sf(data = Florida_map_counties) +
  coord_sf(xlim = c(-83.5, -81.5), ylim = c(27.5,28.5), expand = FALSE) #If expand =T it usually gives you a white border around the map 

### BUT! These are not detailed enough coast lines, so I used a shapefile from the
# Florida Geographic Data Library
#rm(Florida_map_counties)

US_map <- st_read(dsn = "Spatial_R/Data_nice_map/Data_nice_map/dtl_cnty",
                  layer="dtl_cnty")

Florida_map <- US_map[US_map$STATE_NAME=="Florida",]
# make valid - this is a problem that occurs if some polygons intersect
library(lwgeom)
Florida_map <- st_make_valid(Florida_map) # takes a minute

ggplot() +
  geom_sf(data = Florida_map) +
  coord_sf() 


####################################################################################################
################## Example: Tampa Bay horseshoe crabs ###############
####################################################################################################

# Read in the data
collect_data <- read.csv("Spatial_R/Data_nice_map/Data_nice_map/new_tb_collect.csv")

coord_points <- data.frame(x=collect_data$Longitude, y=collect_data$Latitude)

## Converting the csv to a spatial object 
collect_shape <- st_as_sf(x = collect_data,
                          coords = c("Longitude", "Latitude"),
                          crs = st_crs(Florida_map))

TampaBay_map <- st_crop(x = Florida_map, y = st_bbox(collect_shape)) ## st_bbox - Return bounding of a simple feature 

# Quick map
ggplot() +
  geom_sf(data = TampaBay_map) +
  geom_sf(data = collect_shape) +
  coord_sf()

###### Making it all pretty
TampaBay_obs <- ggplot() +
  geom_sf(data = TampaBay_map, fill = "antiquewhite", alpha = 0.7, color = "black") +
  geom_sf(data = collect_shape, color = "blue", alpha = 0.3) +
  ggtitle("FIM data collection in Tampa Bay 1999 - 2016") +
  annotate(geom = "text", x = -82.75, y = 27.45, label = "Gulf of\nMexico", 
           fontface = "italic", color = "grey22", size = 3) + # if you use \n you can go to the next line
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank()) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.30, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(expand = FALSE) 
TampaBay_obs

library(maps)
library(mapdata) # to turn data from maps package into a dataframe you can use with
# ggplot

Florida_map_light <- map_data("county", "Florida")

Florida_inset <- ggplot() +
  geom_polygon(data = Florida_map_light, aes(x=long, y= lat, group = group),
               fill = "white", color = "black") +
  annotate("rect", xmin=st_bbox(collect_shape)$xmin, xmax=st_bbox(collect_shape)$xmax, 
           ymin=st_bbox(collect_shape)$ymin, ymax=st_bbox(collect_shape)$ymax, 
           color="black", fill=NA, linewidth = 1.0) +
  theme_void() +
  #coord_sf()
  coord_fixed(1.2)

# Now use cowplot to put them together
full_map <- plot_grid(Florida_inset, TampaBay_obs)
full_map
####################################################################################################

## World shp - from R 
spdf_africa <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
extent(spdf_africa)
st_crs(spdf_africa)
st_geometry_type(spdf_africa)
spdf_africa <- st_make_valid(spdf_africa)

spdf_africa <- spdf_africa %>% 
  filter(admin %in% c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
                      "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
                      "Democratic Republic of the Congo","Republic of Congo","Republic of the Congo","Ivory Coast","Cote D'Ivoire",
                      "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia",
                      "Gabon","Gambia","Ghana","Guinea","Guinea Bissau","Kenya","Lesotho","Liberia",
                      "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
                      "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
                      "Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
                      "Sudan","United Republic of Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"))


ggplot()+
  geom_sf(data=spdf_africa, aes(fill=admin), colour="darkblue",lwd=0.75)+
  coord_sf()

plot(spdf_africa)

## Adding Labels - If you want to write the countries names
## If you want to get the middle point of those polygons for geom_text 
centroids <- st_centroid(spdf_africa) ## to get mid points of polygons 
centroids$Lat <- st_coordinates(centroids)[, 2]
centroids$Long <- st_coordinates(centroids)[, 1]

ggplot()+
  geom_sf(data=spdf_africa, fill=NA, colour="darkblue",lwd=0.75)+
  geom_text(data = centroids, aes(label = adm0_a3_is, x = Long, y = Lat),
          nudge_y = 0.5, size = 2.5, fontface = "bold", family = "serif")+
  coord_sf()+
  xlab(" ") + ylab(" ")


## Assigning African regions
# Sample data frame of countries

North_A <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia","Sahrawi ADR")

West_A <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau",
              "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

Central_A <-  c("Burundi","Cameroon","Central African Republic","Chad","Democratic Republic of the Congo","Republic of Congo",
                "Equatorial Guinea","Sao Tome and Principe","Gabon")

East_A <- c("Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mauritius","Rwanda","Seychelles","Somalia","South Sudan",
            "Sudan","United Republic of Tanzania","Tanzania","Uganda")

Southern_A <- c("Angola","Botswana","Eswatini","Lesotho","Malawi","Mozambique","Namibia","South Africa","Zambia","Zimbabwe","Swaziland")

country_region_df <- data.frame(Region = rep(c("North_A", "West_A", "Central_A", "East_A", "Southern_A"), 
                                         c(length(North_A), length(West_A), length(Central_A), length(East_A), length(Southern_A))),
                               Country = c(North_A, West_A, Central_A, East_A, Southern_A))

spdf_africa <- spdf_africa %>% 
  left_join(country_region_df, by = c("sovereignt" = "Country"))

####################################################################################################
### Merging the geometries based on the African regions 
merged_Afri_regions <- spdf_africa %>% 
  group_by(Region) %>% 
  summarise(geometry = st_union(geometry), 
            Region = Region) %>%
  st_as_sf() %>% 
  unique()

merged_Afri_regions <-merged_Afri_regions[-6,] #remove NAs
plot(merged_Afri_regions) ## For a quick look

centroids_merged <- st_centroid(merged_Afri_regions) ## to get mid points of polygons 
centroids_merged$Lat <- st_coordinates(centroids_merged)[, 2]
centroids_merged$Long <- st_coordinates(centroids_merged)[, 1]


ggplot()+
  geom_sf(data=merged_Afri_regions, aes(fill=Region), colour="darkblue",lwd=0.5)+
  geom_text(data = centroids_merged, aes(label = Region, x = Long, y = Lat),
            nudge_y = 0.5, size = 2.5, fontface = "bold", family = "serif")
  coord_sf()


### Plotting both raster and Shapefiles
library(hrbrthemes)
ggplot() +
  geom_raster(data = Africa_TP_2019_df, aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent")+ 
  geom_sf(data=spdf_africa,fill=NA,size=4)+
  geom_text(data = centroids, aes(label = adm0_a3_is, x = Long, y = Lat),
         nudge_y = 0.1, size = 3.5, fontface = "bold", family = "serif", na.rm=TRUE)+
  # theme_ipsum(axis_title_size = 22, base_size = 20, subtitle_size = 19)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(1.3, 0.3),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10))+
  xlab(" ") + ylab(" ")

### Shapefile with African regions
ggplot() +
  geom_raster(data = Africa_TP_2019_df, aes(x = x, y = y, fill = Africa_Transmission_Pot_2019)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent")+ 
  geom_sf(data=merged_Afri_regions, fill=NA, colour="#31572c",lwd=0.5)+
  geom_text(data = centroids_merged, aes(label = Region, x = Long, y = Lat),
            nudge_y = 0.1, size = 3.5, fontface = "bold", family = "serif",col="#ecf39e",na.rm=TRUE)+
  theme_ipsum(axis_title_size = 22, base_size = 20, subtitle_size = 19)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(1.3, 0.3),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10))+
  xlab(" ") + ylab(" ")

############################################################################################################################
####Extracting data from raster using Shapefiles ######################################################################
############################################################################################################################

## First you need to make sure that both your raster and your shapefile has the same projection 

crs(spdf_africa) ## Shapefile projection
crs(Africa_TP_2019)

## Transforming to the same projection ! preferably shape to raster
spdf_africa_transformed <- sf::st_transform(spdf_africa,crs(Africa_TP_2019))

## Extracting ! 
Trans.pot.country <- raster::extract(Africa_TP_2019,spdf_africa_transformed,fun= mean, na.rm=TRUE)

Trans.pot.country.df <- cbind(spdf_africa_transformed,Trans.pot.country) 

##Plot
custom_palette2 <- c("#81ffdf", "#85f7da", "#8aefd5", "#8ee6d0", "#92decb", "#97d6c6", "#9bcec1", "#9fc5bc",
                     "#a4bdb6", "#a8b5b1", "#acadac", "#b1a4a7", "#b59ca2", "#b9949d", "#be8c98", "#c28393",
                     "#c77b8e", "#cb7389", "#cf6b84", "#d4627f", "#d85a7a", "#dc5275", "#e14a6f", "#e5416a", 
                     "#e93965", "#ee3160", "#f2295b", "#f62056", "#fb1851", "#ff104c")


ggplot()+
  geom_sf(data=spdf_africa_transformed, aes(fill=Trans.pot.country), colour="darkblue",lwd=0.5)+
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette2, na.value = "transparent")+ 
  coord_sf()


############################################################################################################################
#### Geom_Curves ######################################################################
############################################################################################################################


