# Pakotnes ----
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(sf)) install.packages("sf")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata")


# Dati ----
Vietas <- read_excel("uzskaisu_dati.xlsx", sheet = "Vietas")
unique(Vietas$vieta)



# Transekšu centroīdi ----
head(Vietas) # strūktūra
class(Vietas$x_sakums) # pārbaudīt mainīga klasi

Vietas <- Vietas %>% # samainīt visus uz numeric
  mutate(across(c(x_sakums, y_sakums, x_beigas, y_beigas), as.numeric))

class(Vietas$x_sakums)


Vietas <- Vietas[,c(1:6)] %>%
  mutate(
    x_vidus = (Vietas$x_sakums + Vietas$x_beigas) / 2,
    y_vidus = (Vietas$y_sakums + Vietas$y_beigas) / 2
  )
head(Vietas) # strūktūra


vietu_centroidi <- Vietas %>%
  group_by(vieta) %>%
  summarise(
    x_centroid = mean(x_vidus),
    y_centroid = mean(y_vidus),
    .groups = "drop"
  )


print(vietu_centroidi)

vietas_centroidi <- st_as_sf(vietu_centroidi, coords = c("x_centroid", "y_centroid"), crs = 3059)  # LKS-92



latvija_sf <- ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(admin == "Latvia")

vietas_centroidi <- st_transform(vietas_centroidi, crs = st_crs(latvija_sf)) 


head(vietas_centroidi)
vietas_centroidi$tips <- ifelse(vietas_centroidi$vieta %in% c("Ģipka", "Apšupe"), "Apsaimniekoti", "Neapsaimniekoti")
unique(vietas_centroidi$vieta)




ggplot() +
  geom_sf(data = latvija_sf, fill = "white", color = "black") +
  geom_sf(data = vietas_centroidi, aes(color = vieta), size = 3) +
  labs(title = "Pētijuma vietu izvieotjums Latvijas teritorijā", legend( shape = )) +
  theme_minimal()


ggplot() +
  geom_sf(data = latvija_sf, fill = "white", color = "black") +
  geom_sf(data = vietas_centroidi, aes(color = vieta, shape = tips), size = 5) +  
  scale_color_manual(values = c("#D81B60", "#FFC107", "#004D40", "#1E88E5"), 
                     name = "Pētījuma vieta") +
  scale_shape_manual(values = c(16, 15),  # piemēram: aplis, trīsstūris, kvadrāts
                     name = "Mežu tips") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 23, face = "bold"), 
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )




