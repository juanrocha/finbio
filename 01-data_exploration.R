library(tidyverse)
library(terra)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(patchwork)


dat <- read_csv("data/23-02-16-MTRX-Mining_data.csv")
dat <- st_as_sf(dat, coords = c("long","lat")) |> 
    st_set_crs(4326)

## world map
# world <- ggplot(map_data("world") , aes(x = long, y = lat)) +
#     geom_polygon(aes(group = group), color = "grey65",
#                  fill = "#f9f9f9", size = 0.2) +
#     coord_quickmap() + theme_void(base_size = 10)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# ggplot(dat)  + geom_sf()

## Biodiversity data: species richness from GeoBON
fl <- "~/Documents/Projects/DATA/Biodiversity/Historical local species richness (PREDICTS)/hill_comcom_id55_20221205_v1.nc"
bio <- raster::brick(fl)

## extracts biodiversity richness for each mine point
dat$spp_rich <- raster::extract(bio$X2010.01.01, dat)
dat <- dat |>  filter(vegetation != "#N/A")

a <- ggplot(world) +
    geom_sf() +
    geom_sf(data = dat, aes(color = primary), show.legend = FALSE) +
    labs(tag = "A") +
    theme_void(base_size = 6)

b <- dat |> 
    ggplot(aes(production, spp_rich)) +
    geom_point(aes(color = primary, shape = vegetation)) +
    scale_x_log10() +
    labs(tag = "B", x = 'Production', y = "Species richness") +
    theme_minimal(base_size = 6)

c <- dat |> 
    ggplot(aes(revenue, spp_rich)) +
    geom_point(aes(color = primary, shape = vegetation)) +
    scale_x_log10() +
    labs(tag = "C", x = 'Revenue', y = "Species richness") +
    theme_minimal(base_size = 6)

d <- ggplot(dat, aes(water, land)) +
    geom_point(aes(color = primary, shape = vegetation)) +
    scale_x_log10() +
    scale_y_log10() + labs(tag = "D") +
    theme_minimal(base_size = 6)

e <- ggplot(dat, aes(carbon, land)) +
    geom_point(aes(color = primary, shape = vegetation)) +
    scale_x_log10() +
    scale_y_log10() + labs(tag = "E") +
    theme_minimal(base_size = 6)

ggplot(dat, aes(spp_rich, carbon)) +
    geom_point(aes(color = primary, shape = vegetation)) +
    #scale_x_log10() +
    scale_y_log10()

a / (b|c) / (d|e) + plot_layout(guides = 'collect', heights = c(2,1,1))

ggsave(
    file = "data_exploration.png", path = "figures/", device = "png", 
    width = 6, height = 7, dpi = 400, bg = "white",
    plot = a / (b|c) / (d|e) + plot_layout(guides = 'collect', heights = c(2,1,1))
)


### raster comparison with Maus data
## This is the 0.5deg res file, also exist in 0.083 and 0.0083
maus <- rast("data/Maus-etal_2022_V2_allfiles/global_miningarea_v2_30arcminute.tif")
maus

bio05 <- aggregate(bio$X2010.01.01, fact = 2, fun = mean) |> 
    rast()
bio05 <- project(bio05, maus)

df_maus <- as.data.frame(maus, xy = TRUE) |> 
    left_join(as.data.frame(bio05, xy = TRUE)) |> 
    rename(mining_area = global_miningarea_v2_30arcminute, 
           spp_richness = X2010.01.01)

df_maus |> 
    mutate(mining = mining_area > 0) |> 
    #filter(mining_area > 0, !is.na(spp_richness)) |> 
    ggplot(aes(mining_area, spp_richness)) +
    geom_point(aes(color = as_factor(mining)), alpha = 0.5, size =0.5) +
    geom_hline(yintercept = 1, color = "orange", linetype = 2) +
    labs(x = "Mining area", y = "Species richness") +
    scale_x_log10() +
    theme_classic()


df_maus |> as_tibble() |> 
    mutate(mining = mining_area > 0) |> 
    filter(mining_area > 0, !is.na(spp_richness)) |> 
    ggplot(aes(mining, spp_richness)) +
    geom_boxplot(aes(fill = mining, color = mining), alpha = 0.25) +
    geom_hline(yintercept = 1, color = "orange", linetype = 2) +
    labs(x = "Mining area", y = "Species richness") +
    theme_classic(base_size = 6)

ggsave(
    file = "maus_mining_sp-richness.png", path = "figures/", device = "png", 
    width = 2.5, height = 2.5, dpi = 400, bg = "white",
    plot = last_plot()
)


## Does it have additional info on the polygon version

poly <- read_sf("data/Maus-etal_2022_V2_allfiles/global_mining_polygons_v2.gpkg")
poly
