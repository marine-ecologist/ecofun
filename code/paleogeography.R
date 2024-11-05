

library(rgplates)
library(chronosphere)

sf_use_s2(FALSE)


mid_miocene_coastlines <- reconstruct("coastlines", age=11.63, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)
late_miocene_coastlines <- reconstruct("coastlines", age=7.2, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)
early_pliocoene_coastlines <- reconstruct("coastlines", age=5.3, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)
mid_pliocoene_coastlines <- reconstruct("coastlines", age=3.6, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)
late_pliocoene_coastlines <- reconstruct("coastlines", age=2.58, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)
pliestocene_coastlines <- reconstruct("coastlines", age=0.126, model="MERDITH2021") |> st_make_valid() |>  st_union() |> st_transform(4326)

# get modern coastlines
world <- rnaturalearth:: ne_countries(scale = "large", returnclass = "sf")
world_map <- world %>%
  dplyr::group_by(continent) %>%
  dplyr::select(continent) %>%
 # st_buffer(dist=.001) %>%
  st_transform(4326)


# Download occurrences from PBDB
mid_miocene_fossils <- read.csv(paste0(
  "https://paleobiodb.org/data1.2/occs/list.csv?",
  "base_name=Scleractinia&interval=Miocene&show=coords")) |>
  st_as_sf(coords=c("lng", "lat")) |>
  st_set_crs("EPSG:4326")

Caribbean <- list(
  scale_x_continuous(limits = c(-100, -60)),
  scale_y_continuous(limits = c(5, 35))
)


Global <- list(
  scale_x_continuous(limits = c(-180, 180)),
  scale_y_continuous(limits = c(-60, 60))
)

ggplot() + theme_bw() + Caribbean +
  ggtitle("Miocene") +
  geom_sf(data=mid_miocene_coastlines, fill="#e0ffd1", linewidth = 0.05) +
  geom_sf(data=mid_miocene_fossils, aes(fill=min_ma), shape=21, size=3, alpha=0.1) +
  #geom_sf(data=world_map, fill=NA, colour="grey") +
  scale_fill_distiller(palette="Spectral") +
  theme(panel.background = element_rect(fill = '#c4ecf5'),
        panel.grid.minor = element_line(linewidth = 0.05),
        panel.grid.major = element_line(linewidth = 0.1))


ggplot() + theme_bw() + Global +
  ggtitle("Miocene") +
  geom_sf(data=mid_miocene_coastlines, fill="#e0ffd1", linewidth = 0.05) +
  geom_sf(data=mid_miocene_fossils, aes(fill=min_ma), shape=21, size=3, alpha=0.5) +
  #geom_sf(data=world_map, fill=NA, colour="grey") +
  scale_fill_distiller(palette="Spectral") +
  theme(panel.background = element_rect(fill = '#c4ecf5'),
        panel.grid.minor = element_line(linewidth = 0.05),
        panel.grid.major = element_line(linewidth = 0.1))




###


# Load necessary library
library(sf)

# Create a data frame with the data
iodp <- data.frame(
  site = c("138-848", "138-849", "138-850", "138-851", "320/321-U1335",
               "320/321-U1337", "320/321-U1338", "184-1143", "177-982",
               "177-985", "177-1085", "177-1090", "115-707", "115-710",
               "177-721", "121-758", "356-1463", "356-1464"),
  longitude = c(-110.48, -110.52, -110.51, -110.572, -126.284,
                -123.206, -117.97, 113.28, 15.87, 43.49,
                13.99, 15.31, 59.17, 60.98, 59.86, 90.36,
                117.62, 118.63),
  latitude = c(-2.994, 0.183, 1.297, 2.77, 5.312,
               3.834, 2.508, 9.36, 57.52, 4.21,
               -29.37, -31.46, 7.55, -7.55, 16.68,
               5.38, -18.97, -18.05)
) |> mutate(site = as.factor(site))

# Convert the data frame to an sf object with specified coordinates
iodp_sf <- st_as_sf(iodp, coords = c("longitude", "latitude"), crs = 4326)



# Assuming correct column name is found and used in aes()
ggplot() +
  theme_bw() +
  # ggtitle("Miocene") +  # Uncomment if you want a title
  geom_sf(data = world_map, fill = "#e0ffd1", color = "black", linewidth = 0.01) +
  geom_sf(data = iodp_sf, aes(fill = site), shape = 21, size = 3, alpha = 0.7) +  # Use the correct column name here
  # scale_fill_distiller(palette = "Spectral") +  # Only if you are using fill for a continuous variable
  theme(
    panel.background = element_rect(fill = '#c4ecf5'),
    panel.grid.minor = element_line(linewidth = 0.05),
    panel.grid.major = element_line(linewidth = 0.1)
  ) +
  coord_sf(lims_method = "geometry_bbox")
