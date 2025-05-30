# Pakotnes ----
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(sf)) install.packages("sf")
if(!require(rnaturalearth)) install.packages("rnaturalearth")



# Dati ----
kartes_Vietas <- read_excel("Dati/originalie_dati.xlsx", sheet = "Vietas")
kartes_Vietas <-  kartes_Vietas[-101,]
dim(kartes_Vietas)
unique(kartes_Vietas$vieta)



# Transekšu centroīdi ----
head(kartes_Vietas) # strūktūra
class(kartes_Vietas$x_sakums) # pārbaudīt mainīga klasi

kartes_Vietas <- kartes_Vietas %>% # samainīt visus uz numeric
  mutate(across(c(x_sakums, y_sakums, x_beigas, y_beigas), as.numeric))

class(kartes_Vietas$x_sakums)


kartes_Vietas <- kartes_Vietas[,c(1:6)] %>%
  mutate(
    x_vidus = (kartes_Vietas$x_sakums + kartes_Vietas$x_beigas) / 2,
    y_vidus = (kartes_Vietas$y_sakums + kartes_Vietas$y_beigas) / 2
  )
head(kartes_Vietas) # strūktūra


vietu_centroidi <- kartes_Vietas %>%
  group_by(vieta) %>%
  summarise(
    x_centroid = mean(x_vidus),
    y_centroid = mean(y_vidus),
    .groups = "drop"
  )


print(vietu_centroidi)

vietu_centroidi <- st_as_sf(vietu_centroidi, coords = c("x_centroid", "y_centroid"), crs = 3059)  # LKS-92



latvija_sf <- ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(admin == "Latvia")

vietu_centroidi <- st_transform(vietu_centroidi, crs = st_crs(latvija_sf)) 


head(vietu_centroidi)
vietu_centroidi$tips <- ifelse(vietu_centroidi$vieta %in% c("Ģipka", "Apšupe"), "Saimnieciskie", "Aizsargātie")
unique(vietu_centroidi$vieta)

# Centroīdi wgs formātā
print(vietu_centroidi)

#Kopēja plans ------------------------------

ggplot() +
  geom_sf(data = latvija_sf, fill = "white", color = "black") +
  geom_sf(data = vietu_centroidi, aes(color = vieta, shape = tips), size = 5) +  
  scale_color_manual(values = c("#D81B60", "#FFC107", "#3cc140", "#1E88E5"), 
                     name = "Pētījuma vieta") +
  scale_shape_manual(values = c(16, 15),  # piemēram: aplis, trīsstūris, kvadrāts
                     name = "Apsaimniekošanas grupa") +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )

#Kopēja realitāte ------------------------------
vietu_centroidi2 <- vietu_centroidi[!vietu_centroidi$vieta == "Šlītere",]
vietu_centroidi2 <- vietu_centroidi2[,-3]

ggplot() +
  geom_sf(data = latvija_sf, fill = "white", color = "black") +
  geom_sf(data = vietu_centroidi2, aes(color = vieta,), size = 5) +  
  scale_color_manual(values = c("#D81B60", "#FFC107", "#3cc140", "#1E88E5"), 
                     name = "Pētījuma vieta") +
  scale_shape_manual(values = c(16, 15),  # piemēram: aplis, trīsstūris, kvadrāts
                     name = "Apsaimniekošanas grupa") +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )

# Katrai vietai ----------------------

unique(vietu_centroidi$vieta)
vieta_punkts <- vietu_centroidi %>% filter(vieta == "Ģipka")

vieta_karte <- ggplot() +
  geom_sf(data = latvija_sf, fill = "white", color = "black") +
  geom_sf(data = vieta_punkts, aes(color = vieta, shape = tips), size = 2) +
  scale_color_manual(values = c("Ģipka" = "#FFC107"), name = NULL) +
  scale_shape_manual(values = c(16, 15), name = NULL) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = "none"
  )

vieta_karte

ggsave("karte_caurs.png", 
       plot = vieta_karte,
       bg = "transparent",
       width = 428 / 600,
       height = 754 / 600,
       dpi = 600)




