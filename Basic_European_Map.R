
library(maps)
library(rworldmap)
library(rworldxtra)

newmap <- getMap(resolution="high")
par(mar=c(0.5,0.5,0.5,0.5))
plot(newmap, col="tan1", border="black",bg="lightblue", 
     xlim=c(23,48), ylim=c(35,43))

setwd("C:/Users/isma-/OneDrive/Escritorio/Crustaceans0/crustaceans/qgis")
riversData <- rgdal::readOGR("tur_watcrsl_rvr_1m.shp")
plot(riversData, col= "steelblue2", add=T)

points(df1$Longitude1 , df1$Latitude1, 
       col="black", pch=23, cex=0.6, bg="red2")

scalebar(x = 24.5, y = 36.2, which = "both", unit = "km", cex = 0.8, 
         length = 20, col = "black", bg = "white", border = "black")
maps::map.scale(x = 24.5, y = 36.2, ratio = F, relwidth=0.25)



###########################################################

library(ggplot2)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe" | name == "Turkey" | 
           name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt"))

europe_map$color <- ifelse(europe_map$name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", "Russia", "Turkey"),
                           "grey80", "Wheat")

setwd("C:/Users/isotoalmena/Downloads")
riversData <- rgdal::readOGR("rivers_europe_37253.shp")
riversData$line_width <- scales::rescale(riversData$Strahler, to = c(0.1, 1))
riversData_sf <- st_as_sf(riversData)
riversData1 <-riversData_sf %>% filter(line_width > 0.25)
  
ggplot() +
  geom_sf(data = europe_map, aes(fill = color))+   scale_fill_identity()+ 
  geom_sf(data = riversData1, aes(size = line_width), color = "steelblue2") + 
  coord_sf() + theme_bw()+ xlim(-11,30) + ylim(36,60)+
  geom_point(data = df, aes(x = Longitude_X, y = Latitude_Y), 
             size = 1, shape = 21, color = "black", fill = "red", stroke = 0.2) +  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", 
                         which_north = "true", pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in"))



##################   Using tmap   ######################
## In this case i am using for continents


type = c("Damage", "Management")
t = type[1]

for (t in type) {
  
  all1 <- all %>% filter(Type_of_cost_merged == t)
  
  summary_data <- aggregate(billion ~ Official_country, all1, sum)
  
  {
    summary_data$Official_country[summary_data$Official_country== 'Viet Nam'] = 'Vietnam'
    summary_data$Official_country[summary_data$Official_country== 'Czechia'] = 'Czech Republic'
    summary_data$Official_country[summary_data$Official_country== 'Brunei Darussalam'] = 'Brunei'
    summary_data$Official_country[summary_data$Official_country== 'Syrian Arab Republic'] = 'Syria'
    #summary_data$Official_country[summary_data$Official_country== 'Greenland'] = 'Vietnam'
    summary_data$Official_country[summary_data$Official_country== 'Guinea-Bissau'] = 'Guinea Bissau'
    summary_data$Official_country[summary_data$Official_country== 'Iran, Islamic Republic of'] = 'Iran'
    summary_data$Official_country[summary_data$Official_country== 'Korea, Republic of'] = 'South Korea'
    summary_data$Official_country[summary_data$Official_country== 'Micronesia, Federated States of'] = 'Federated States of Micronesia'
    summary_data$Official_country[summary_data$Official_country== 'Russian Federation'] = 'Russia'
    summary_data$Official_country[summary_data$Official_country== 'Serbia'] = 'Republic of Serbia'
    summary_data$Official_country[summary_data$Official_country== 'Syrian Arab Republic'] = 'Syrian Arab Republic'
    summary_data$Official_country[summary_data$Official_country== 'Tanzania, United Republic of'] = 'United Republic of Tanzania'
    summary_data$Official_country[summary_data$Official_country== 'Timor-Leste'] = 'East Timor'
    summary_data$Official_country[summary_data$Official_country== 'United Kingdom of Great Britain and Northern Ireland'] = 'United Kingdom'
    
    
    summary_data$Official_country[summary_data$Official_country== 'Cape verde'] = 'Cape Verde'
    summary_data$Official_country[summary_data$Official_country== 'Vietnam'] = 'Viet Nam'
    summary_data$Official_country[summary_data$Official_country== 'United Kingdom'] = 'United Kingdom of Great Britain and Northern Ireland'
    summary_data$Official_country[summary_data$Official_country== 'Iran, Islamic Republic of'] = 'Iran'
    summary_data$Official_country[summary_data$Official_country== 'Russia'] = 'Russian Federation'
  }
  
  summary_data <- aggregate(billion ~ Official_country, summary_data, sum)
  
  {
    continent <- data.frame(
      Official_country = c(
        "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia", "Norway", "Oman",
        "Pakistan", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
        "Qatar", "Republic of the Congo", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
        "Samoa", "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia",
        "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan",
        "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga",
        "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates",
        "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela",
        "Vietnam", "Yemen", "Zambia", "Zimbabwe"
      ),
      Continent = c(
        "Europe", "Oceania", "North America", "Africa", "Africa", "Asia", "Europe", "Europe", "Asia",
        "Asia", "Asia", "North America", "Oceania", "South America", "South America", "Asia", "Europe", "Europe",
        "Asia", "Africa", "Europe", "Europe/Asia", "Africa", "North America", "North America", "North America",
        "Oceania", "Europe", "Asia", "Africa", "Europe", "Africa", "Africa", "Asia", "Europe",
        "Europe", "Oceania", "Africa", "Africa", "Asia", "Africa", "Europe", "Asia", "Africa",
        "South America", "Europe", "Europe", "Asia", "Asia", "Asia", "Africa", "Asia", "Africa", "Oceania",
        "North America", "Africa", "Asia", "Asia", "Oceania", "Africa", "Europe", "Asia",
        "Europe", "North America", "South America", "Asia", "Oceania", "Europe", "South America",
        "Asia", "Asia", "Africa", "Africa"
      )
    )
    additional_countries <- data.frame(
      Official_country = c(
        "Malaysia", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico",
        "Micronesia, Federated States of", "Moldova", "Mongolia", "Montenegro",
        "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Republic of Serbia",
        "Russian Federation", "Syrian Arab Republic", "Tanzania, United Republic of",
        "Timor-Leste", "United Kingdom of Great Britain and Northern Ireland",
        "United Republic of Tanzania", "Viet Nam"
      ),
      Continent = c(
        "Asia", "Africa", "Europe", "Africa", "Africa", "North America",
        "Oceania", "Europe", "Asia", "Europe",
        "Africa", "Africa", "Asia", "Africa", "Asia", "Europe",
        "Europe", "Asia", "Africa",
        "Asia", "Europe",
        "Africa", "Asia"
      )
    )
    countries_continents <- data.frame(
      Official_country = c(
        "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Argentina", "Armenia", "Australia", "Austria",
        "Azerbaijan", "Bahrain", "Bangladesh", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia",
        "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Brunei Darussalam", "Bulgaria", "Burkina Faso",
        "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", "Chad", "Chile",
        "China", "Colombia", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Czechia", "Democratic Republic of the Congo",
        "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador",
        "Equatorial Guinea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia",
        "Georgia", "Germany", "Ghana", "Greece", "Greenland", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana",
        "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq"
        # ... Add more countries as needed
      ),
      Continent = c(
        "Asia", "Europe", "Africa", "Europe", "Africa", "South America", "Asia", "Oceania", "Europe",
        "Asia", "Asia", "Asia", "Europe", "Europe", "North America", "Africa", "Asia", "South America",
        "Europe", "Africa", "South America", "Asia", "Asia", "Europe", "Africa",
        "Africa", "Asia", "Africa", "North America", "Africa", "Africa", "Africa", "South America",
        "Asia", "South America", "North America", "Europe", "North America", "Asia", "Europe", "Europe", "Africa",
        "Europe", "Africa", "North America", "North America", "Asia", "South America", "Africa", "North America",
        "Africa", "Europe", "Africa", "Africa", "Oceania", "Europe", "Europe", "Africa", "Africa",
        "Asia", "Europe", "Africa", "Europe", "North America", "North America", "Africa", "Africa", "South America",
        "North America", "North America", "Europe", "Europe", "Asia", "Asia", "Asia", "Asia"
        # ... Add continents for more countries as needed
      )
    )
    additional_countries2 <- data.frame(
      Official_country = c(
        "Iran", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan",
        "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea, Republic of", "Kuwait", "Kyrgyzstan",
        "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania",
        "Luxembourg", "Macedonia", "Madagascar", "Malawi"
      ),
      Continent = c(
        "Asia", "Europe", "Asia", "Europe", "Africa", "North America", "Asia",
        "Asia", "Asia", "Africa", "Oceania", "Asia", "Asia", "Asia",
        "Asia", "Europe", "Asia", "Africa", "Africa", "Africa", "Europe", "Europe",
        "Europe", "Europe", "Africa", "Africa"
      )
    )
    continent_classification <- rbind(continent, additional_countries,additional_countries2, countries_continents)
  }
  
  summary_data2<- left_join(summary_data, continent_classification, by ="Official_country")
  unique(summary_data2$Continent)
  
  summary_data2 <- summary_data2 %>%
    mutate(Continent = case_when(
        Official_country == "Guinea Bissau" & is.na(Continent)~ "Africa", 
        Official_country == "Federated States of Micronesia" & is.na(Continent) ~ "Oceania", 
        TRUE ~ Continent  ) )
        
        
  summary_data3 <- aggregate(billion ~ Continent, summary_data2, sum)
  
  world_continents <- aggregate(world["pop_est"], by = list(Continent = world$continent), FUN = sum)
  summary_data3_sf <- merge(world_continents, summary_data3, by = "Continent")
  unique(summary_data3_sf$billion)
  #   82.40793 151.91834  86.10591  80.91758  17.90964  46.28417 --- Damage
  # 3.846237 15.578686 15.660461 23.104743  3.151593  4.937124 --- Management
  if(t=="Damage" ){
    p5<-tm_shape(summary_data3_sf,
                 projection = "+proj=eck4") +
      tm_polygons("billion",
                  palette = "OrRd",
                  title = "Cost in US$ billion",
                  breaks = breaks,
                  # style="pretty"
      )+
      tm_layout(
        legend.bg.color = "white",
        inner.margins=c(.04, .01, .1, .01),
        bg.color="#AEDFE5",
        outer.bg.color="white",
        earth.boundary=c(-180, 180, -70, 90),
        earth.boundary.color="white",
        earth.boundary.lwd=.4,
        space.color="white",
        attr.outside=T,
        attr.color="grey20",
        frame = FALSE,
        legend.title.size = 1.5,
        legend.text.size = 1)
  } else{ 
    p6<-tm_shape(summary_data3_sf,
                 projection = "+proj=eck4") +
      tm_polygons("billion",
                  palette = "OrRd",
                  title = "Cost in US$ billion",
                  breaks = breaks,
                  # style="pretty"
      )+
      tm_layout(
        legend.bg.color = "white",
        inner.margins=c(.04, .01, .1, .01),
        bg.color="#AEDFE5",
        outer.bg.color="white",
        earth.boundary=c(-180, 180, -70, 90),
        earth.boundary.color="white",
        earth.boundary.lwd=.4,
        space.color="white",
        attr.outside=T,
        attr.color="grey20",
        frame = FALSE,
        legend.title.size = 1.5,
        legend.text.size = 1)
  }
  
  if(exists("p5") && exists("p6")) {
    tmap_arrange(p5, p6, ncol = 1)
  }  
}




setwd("C:/Users/Propietario/Desktop/ELZA")
list.files()
library(maps)
library(rworldmap)
library(rworldxtra)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

d = read_xlsx("GLOBAL NNS DATA FINAL.xlsx")
world <- ne_countries(type = 'countries', returnclass = "sf")

setdiff(unique(world$admin), unique(d$Location))

d$Location[d$Location =="Tanzania, United Republic of"] <- "United Republic of Tanzania"
d$Location[d$Location =="Russian Federation"] <- "Russia"
d$Location[d$Location =="Bahamas"] <- "The Bahamas"
d$Location[d$Location ==""] <- "French Southern and Antarctic Lands"
d$Location[d$Location =="Timor-Leste"] <- "East Timor"
d$Location[d$Location =="Venezuela, Bolivarian Republic of"] <- "Venezuela"
d$Location[d$Location =="Côte d'Ivoire"] <- "Ivory Coast"
d$Location[d$Location =="Congo"] <- "Republic of the Congo"
d$Location[d$Location =="Eswatini"] <- "eSwatini"
d$Location[d$Location =="Palestine, State of"] <- "Palestine"
d$Location[d$Location =="Lao People's Democratic Republic"] <- "Laos"
d$Location[d$Location =="Viet Nam"] <- "Vietnam"
d$Location[d$Location =="Korea, Democratic People's Republic of"] <- "North Korea"
d$Location[d$Location =="Korea, Republic of"] <- "South Korea"
d$Location[d$Location =="Iran, Islamic Republic of"] <- "Iran"
d$Location[d$Location =="Syrian Arab Republic"] <- "Syria"
d$Location[d$Location =="Moldova, Republic of"] <- "Moldova"
d$Location[d$Location =="United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
d$Location[d$Location =="Brunei Darussalam"] <- "Brunei"
d$Location[d$Location =="Czech Republic"] <- "Czechia"
d$Location[d$Location =="Cyprus"] <- "Cyprus"
d$Location[d$Location =="Macedonia"] <- "North Macedonia"
d$Location[d$Location =="Serbia"] <- "Republic of Serbia"
d$Location[d$Location =="Sudan"] <- "South Sudan"

unique(d$Location)
d %>% filter(str_detect(Location, "rance"))

setdiff(unique(d$Location), unique(world$admin) )

d$Location[d$Location =="Hawaiian Islands"] <- "United States of America"
d$Location[d$Location =="Galapagos"] <- "Ecuador"
d$Location[d$Location =="USA Coastal States"] <- "United States of America"
d$Location[d$Location =="Marshall Islands"] <- ""
d$Location[d$Location =="Seychelles"] <- ""
d$Location[d$Location =="Liechtenstein"] <- ""
d$Location[d$Location =="Antigua and Barbuda"] <- ""
d$Location[d$Location =="Barbados"] <- ""
d$Location[d$Location =="Cayman Islands"] <- ""
d$Location[d$Location =="Guadeloupe"] <- ""
d$Location[d$Location =="Guam"] <- "United States of America"
d$Location[d$Location =="Martinique"] <- ""
d$Location[d$Location =="Saint Kitts and Nevis"] <- ""
d$Location[d$Location =="Virgin Islands (U.S.)"] <- "United States of America"
d$Location[d$Location =="Comoros"] <- ""
d$Location[d$Location =="Réunion"] <- ""
d$Location[d$Location =="Cook Islands"] <- ""
d$Location[d$Location =="Micronesia, Federated States of"] <- ""
d$Location[d$Location =="Niue"] <- ""
d$Location[d$Location =="Palau"] <- ""
d$Location[d$Location =="Samoa"] <- ""
d$Location[d$Location =="Singapore"] <- ""
d$Location[d$Location =="Tonga"] <- ""
d$Location[d$Location =="American Samoa"] <- ""
d$Location[d$Location =="Dominica"] <- ""
d$Location[d$Location =="Grenada"] <- ""
d$Location[d$Location =="Northern Mariana Islands"] <- "United States of America"
d$Location[d$Location =="Saint Lucia"] <- ""
d$Location[d$Location =="Saint Vincent and the Grenadines"] <- ""
d$Location[d$Location =="Corsica"] <- "France"
d$Location[d$Location =="Madeira"] <- "Portugal"
d$Location[d$Location =="Balearic Islands"] <- "Spain"
d$Location[d$Location =="Canary Islands"] <- "Spain"
d$Location[d$Location =="Azores"] <- "Portugal"
d$Location[d$Location =="Sicily"] <- "Italy"
d$Location[d$Location =="Alaska"] <- "United States of America"
d$Location[d$Location =="Svalbard and Jan Mayen"] <- "Norway"
d$Location[d$Location =="California"] <- "United States of America"
d$Location[d$Location =="Hawaii"] <- "United States of America"
d$Location[d$Location =="Chesapeake Bay"] <- "United States of America"
d$Location[d$Location =="Isle of Man"] <- "United Kingdom"
d$Location[d$Location =="Åland Islands"] <- "Sweden"



d1<- d %>% group_by(Location) %>% summarise(Richness = n_distinct(Taxon)) 
str(d1)
str(world)

dat <- world %>%  left_join(d1, by = c("admin" = "Location"))
dat1<- dat[,c(10,169,170)]

tmap_tip()
tmap_show_palettes()

tm_shape(dat1, projection = "+proj=eck4") +
  tm_polygons("Richness",
              palette = "OrRd",
              title = "Number of non-native species",
              # style="pretty",
              breaks = breaks,
              style = "fixed",
  )+
  tm_layout(
    legend.bg.color = "white",
    legend.show = T,
    inner.margins=c(.04, .01, .1, .01),
    bg.color="#AEDFE5",
    outer.bg.color="white",
    earth.boundary=c(-180, 180, -80, 90),
    earth.boundary.color="white",
    earth.boundary.lwd=.4,
    space.color="white",
    attr.outside=T,
    attr.color="grey20",
    frame = FALSE,
    legend.title.size = 1.5,
    legend.text.size = 1)


min_richness <- min(dat1$Richness, na.rm = TRUE)
max_richness <- max(dat1$Richness, na.rm = TRUE)


num_intervals <- 8
breaks <- seq(min_richness, max_richness, length.out = num_intervals + 1)
breaks <- classIntervals(dat1$Richness, n = 7, style = "jenks")$brks

breaks <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000) # Adjust based on your data



