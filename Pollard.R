# Pakotnes --------
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(Hmisc)) install.packages("Hmisc")
if(!require(EnvStats)) install.packages("EnvStats")

# Dati ----
TDataset <- read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")
summary(TDataset)
TDataset <- TDataset[!is.na(TDataset$uzsk_ID),]  #Noņem tukšās rindas
dim(TDataset)


#Noņemt liekas kolonnas
TDataset_indeksi <- TDataset[, -c(5:45,50:57)]

# Pievienot iztrūkstošo uzskaiti Ķemeros
kemeri_trans <- TDataset %>%
  filter(vieta == "Ķemeri") %>%
  distinct(trans_kods) %>%
  slice_head(n = 39) %>%
  pull(trans_kods)

# Jauna datu tabula ar 39 rindiņām
kemeri_3 <- tibble(
  vieta = "Ķemeri",
  uzskaite = 3,
  trans_kods = kemeri_trans
)
TDataset_pilnais <- bind_rows(TDataset, kemeri_3)

rm(kemeri_trans, kemeri_3)


#Noņemt liekas kolonnas
TDataset_pilnais <- TDataset_pilnais[, -c(5:45,50:57)]

# Datumi
TDataset_pilnais$datums <- as.Date(TDataset_pilnais$datums, format = "%d.%m.%Y")
unique(TDataset_pilnais$datums)

TDataset_pilnais <- TDataset_pilnais %>%
  mutate(datums = case_when(
    datums == as.Date("2024-06-25") ~ as.Date("2024-06-26"),
    datums == as.Date("2024-07-17") ~ as.Date("2024-07-18"),
    TRUE ~ datums  # visi pārējie datumi paliek nemainīti
  ))


# Abundance indeksa salīdzinājums starp vietam =================================

# Direktorija
dir.create("Pollard/indeksi", recursive = TRUE)


sugas <- unique(TDataset_indeksi$latviskais)


for (izveleta_suga in sugas) {
  
  # Datu filtrēšana un sagatavošana
  sugas_indeksi <- TDataset_pilnais %>%
    filter(latviskais == izveleta_suga) %>%
    group_by(vieta, trans_kods) %>%
    summarise(skaits = n(), .groups = "drop")
  
  # Ja nav datu, pāriet uz nākamo sugu
  if (nrow(sugas_indeksi) == 0) next
  
  # Aprēķina n uz vietu
  n_labels <- sugas_indeksi %>%
    group_by(vieta) %>%
    summarise(n = n(), y = max(skaits) + 1.5)
  
  # Zīmē grafiku
  index_plot <- ggplot(sugas_indeksi, aes(x = vieta, y = skaits, fill = vieta, color = vieta)) +
    geom_boxplot(outlier.shape = NA, width = 0.4, alpha = 0.4) +    
    geom_jitter(width = 0.1, height = 0, alpha = 0.7, size = 2) +
    stat_summary(fun = mean, geom = "point", shape = 21, size = 5, colour = "black") +
    geom_text(data = n_labels, aes(x = vieta, y = y, label = paste0("n = ", n)), color = "black", size = 4) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = seq(0, max(sugas_indeksi$skaits) + 2, by = 1)) +
    theme_minimal(base_size = 14) +
    labs(x = "Pētījuma vieta", y = "Pollarda pārpilnības indekss", title = izveleta_suga)
  
  # Saglabā attēlu
  ggsave(filename = file.path("Pollard/indeksi", paste0(gsub("[^a-zA-Z0-9_āčēģīķļņšūžĀČĒĢĪĶĻŅŠŪŽ]", "_", izveleta_suga), ".png")),
         plot = index_plot,
         width = 10,
         height = 6,
         dpi = 300,
         bg = "white")
}










# Sugu fenoloģijas liknes ======================================================

# Savā metosika Pollards uzsvēra, kā transekte var būt salīdzināma tikai
# pati ar sevi, nevis savā starpā. Tādēļ izvēlos fenoloģiju būvēt pēc transekšu
# indeksu summu (jeb novērojumu skaiti) kartrā uzsk. reizē


#Iegūstam visus iespējamos vieta–datums pārus
vieta_datums <- TDataset_pilnais %>%
  distinct(vieta, datums)
vieta_datums <- vieta_datums[!is.na(vieta_datums$datums),]


# Direktorija 
dir.create("Pollard/fenologija", recursive = TRUE)



# Visas unikālās sugas
sugas <- unique(TDataset_pilnais$latviskais)

# Cikls pa visām sugām
for (izveleta_suga in sugas) {
  
  # Filtrēt tikai izvēlētas sugas novērojumus
  sugas_nov <- TDataset_pilnais %>%
    filter(latviskais == izveleta_suga) %>%
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
      limits = c(0, 15),
      breaks = seq(0, max(suga_dati$skaits), by = 1)
    ) +
    labs(
      title = paste("", izveleta_suga),
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
  faila_nosaukums <- gsub("[^a-zA-Z0-9_āčēģīķļņšūžĀČĒĢĪĶĻŅŠŪŽ]", "_", izveleta_suga)
  faila_cels <- file.path("Pollard/fenologija", paste0(faila_nosaukums, ".png"))
  
  ggsave(filename = faila_cels, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}













