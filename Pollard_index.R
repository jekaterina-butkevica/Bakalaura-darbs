if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")


# Dati ----
TDataset <- read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")
summary(TDataset)
TDataset <- TDataset[!is.na(TDataset$uzsk_ID),]  #Noņem tukšās rindas
dim(TDataset)

#Noņemt liekas kolonnas
TDataset_sugas <- TDataset[, -c(5:45,50:57)]

# Datumi
TDataset_sugas$datums <- as.Date(TDataset_sugas$datums, format = "%d.%m.%Y")





# Sugu fenoloģijas liknes -----

#Iegūstam visus iespējamos vieta–datums pārus
vieta_datums <- TDataset_sugas %>%
  distinct(vieta, datums)
vieta_datums <- vieta_datums[!is.na(vieta_datums$datums),]


# Direktorija 
dir.create("fenologija")


# Visas unikālās sugas
sugas <- unique(TDataset_sugas$latviskais)

# Cikls pa visām sugām
for (suga_x in sugas) {
  
  # Filtrēt tikai izvēlētas sugas novērojumus
  sugas_nov <- TDataset_sugas %>%
    filter(latviskais == suga_x) %>%
    group_by(vieta, datums) %>%
    summarise(skaits = n(), .groups = "drop")
  
  # Apvienot datus ar visiem vieta–datums pāriem (lai būtu arī 0 vērtības)
  suga_dati <- vieta_datums %>%
    left_join(sugas_nov, by = c("vieta", "datums")) %>%
    mutate(skaits = ifelse(is.na(skaits), 0, skaits))  # Aizpilda NA ar 0
  
  # Ja pēc tam vēl joprojām ir maz datu, izlaiž
  if (sum(suga_dati$skaits) < 2) next
  
  # Grafiks
  p <- ggplot(suga_dati, aes(x = datums, y = skaits, group = vieta, color = vieta)) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5) +
    geom_point(size = 4) +
    scale_x_date(
      breaks = sort(unique(suga_dati$datums)),
      date_labels = "%d.%m"
    ) +
    scale_y_continuous(
      limits = c(0, 12),
      breaks = seq(0, max(suga_dati$skaits), by = 1)
    ) +
    labs(
      title = paste("", suga_x),
      x = "Uzskaites datums",
      y = "Novēroto indivīdu skaits",
      color = "Vieta"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  # Saglabāšana
  faila_nosaukums <- gsub("[^a-zA-Z0-9_āčēģīķļņšūžĀČĒĢĪĶĻŅŠŪŽ]", "_", suga_x)
  faila_cels <- file.path("fenologija", paste0(faila_nosaukums, ".png"))
  
  ggsave(filename = faila_cels, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}















