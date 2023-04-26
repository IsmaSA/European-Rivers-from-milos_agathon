## R script from --->  Milos Popovic

library(readxl)
# libraries we need
libs <- c("httr", "tidyverse", "sf")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET RIVERS DATA
#---------

get_data <- function() {
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
  res <- httr::GET(
    url,
    write_disk("eu_rivers.zip"),
    progress()
  )
  unzip("eu_rivers.zip") # unzip
  filenames <- list.files("HydroRIVERS_v10_eu_shp",
                          pattern = "*.shp", full.names = T
  )
  
  return(filenames)
}

filenames <- get_data()

# 2. CREATE RIVER WIDTH
#---------

load_rivers <- function() {
  list_riv <- lapply(filenames, sf::st_read)
  eu_riv <- list_riv[[1]] |>
    sf::st_cast("MULTILINESTRING")
  
  return(eu_riv)
}

eu_riv <- load_rivers()

get_river_width <- function() {
  eu_riv_width <- eu_riv |>
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) |>
    sf::st_as_sf()
  
  return(eu_riv_width)
}

eu_riv_width <- get_river_width()

# 3. MAKE BOUNDING BOX
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, new_prj, bb) {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-10.5, 44.5, 44.5, -10.5, -10.5), #xmin xmax
      c(35.000, 35.000, 69.5, 69.5, 35.000) #ymin y max
    ))),
    crs = crsLONGLAT
  )
  
  new_prj <- sf::st_transform(bbox, crs = 4087)
  bb <- sf::st_bbox(new_prj)
  
  return(bb)
}

bbox <- get_bounding_box()

# 4. MAP
#---------
setwd("C:/Users/isma-/OneDrive/Escritorio/full databse/DATA")
df <- read_xlsx("Global_dataset.xlsx")
df<- df %>% filter(Alien =="Y")
df<-df[!duplicated(df$site_id), ]
#df<- df[,c(8,9)]
#df1<- df[c(1:50), ]

points_sf <- st_as_sf(df, coords = c("Longitude_X", "Latitude_Y")) 
st_crs(points_sf) <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
points_sf <- st_transform(points_sf, 4087)

#eu_riv_width1<- eu_riv_width %>% filter(width  > 0.6)

st_crs(points_sf)
st_crs(eu_riv_width1)


get_river_map <- function() {
  p <-
    ggplot() +
    geom_sf(
      data = eu_riv_width,
      aes(
        color = factor(ORD_FLOW), size = width,
        alpha = factor(ORD_FLOW)
      )
    ) +
    geom_sf(data=points_sf, colour = "red", fill = "black", alpha=.3)+
    coord_sf(
      crs = 4087,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    labs(
      y = "", subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "#08306b", "#08519c", "#2171b5",
        "#4292c6", "#6baed6", "#9ecae1",
        "#c6dbef", "#deebf7"
      )
    ) +
    scale_size(range = c(0, .3)) +
    scale_alpha_manual(values = c(
      "3" = 1, "4" = 1, "5" = .7, "6" = .6,
      "7" = .4, "8" = .3, "9" = .2, "10" = .1
    )) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 40, color = "#2171b5", hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 10, color = "grey60", hjust = 0.5, vjust = 10
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  return(p)
}

p1 <- get_river_map()

ggsave(
  filename = "european_rivers.png",
  width = 8.5, height = 7, dpi = 600,
  device = "png", bg = "white", p1
)



#############################################################################################################################################

library(rnaturalearth)
library(sf)
library(ggplot2)

get_river_map <- function() {
  
  # get Europe map
  europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent == "Europe")
  
  # plot the rivers on top of the Europe map
  p <- ggplot() +
    geom_sf(data = europe_map, fill = "gray99", color = "gray30") +
    geom_sf(
      data = eu_riv_width1,
      aes(
        color = factor(ORD_FLOW), size = width,
        alpha = factor(ORD_FLOW)
      )
    ) +
    
    geom_sf(data=points_sf,  fill = "purple", alpha=.8, shape = 22, size=0.8)+
    
    coord_sf(
      crs = 4087,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    labs(
      y = "", subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "#08306b", "#08519c", "#2171b5",
        "#4292c6", "#6baed6", "#9ecae1",
        "#c6dbef", "#deebf7"
      )
    ) +
    scale_size(range = c(0, .3)) +
    scale_alpha_manual(values = c(
      "3" = 1, "4" = 1, "5" = .7, "6" = .6,
      "7" = .4, "8" = .3, "9" = .2, "10" = .1
    )) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 40, color = "#2171b5", hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 10, color = "grey60", hjust = 0.5, vjust = 10
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  return(p)
}


