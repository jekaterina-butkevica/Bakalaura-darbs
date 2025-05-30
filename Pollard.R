# Pakotnes --------
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(EnvStats)) install.packages("EnvStats")
if(!require(rstatix)) install.packages("rstatix")
if(!require(tidyr)) install.packages("tidyr")

# Dati ----
Pollard_dataset <- read_excel("originalie_dati.xlsx", sheet = "Noverojumi")
summary(Pollard_dataset)
Pollard_dataset <- Pollard_dataset[!is.na(Pollard_dataset$uzsk_ID),]  #Noņem tukšās rindas
dim(Pollard_dataset)


#Noņemt liekas kolonnas

# Pievienot iztrūkstošo uzskaiti Ķemeros
kemeri_trans <- Pollard_dataset %>%
  filter(vieta == "Ķemeri") %>%
  distinct(trans_kods) %>%
  slice_head(n = 39) %>%
  pull(trans_kods)

# Jauna datu tabula ar 39 rindiņām
kemeri_3 <- tibble(
  vieta = "Ķemeri",
  uzsk_ID = 3,
  trans_kods = kemeri_trans
)
Pollard_dataset_pilnais <- bind_rows(Pollard_dataset, kemeri_3)




#Noņemt liekas kolonnas
data.frame(Numurs = seq_along(Pollard_dataset_pilnais), Kolonna = names(Pollard_dataset_pilnais))
Pollard_dataset_pilnais <- Pollard_dataset_pilnais[, -c(5:44,47:50,52:55)]

# Datumi
Pollard_dataset_pilnais$datums <- as.Date(Pollard_dataset_pilnais$datums, format = "%d.%m.%Y")
unique(Pollard_dataset_pilnais$datums)

Pollard_dataset_pilnais <- Pollard_dataset_pilnais %>%
  mutate(datums = case_when(
    datums == as.Date("2024-06-25") ~ as.Date("2024-06-26"),
    datums == as.Date("2024-07-17") ~ as.Date("2024-07-18"),
    TRUE ~ datums  # visi pārējie datumi paliek nemainīti
  ))





# Sugu fenoloģijas liknes ======================================================

# Savā metosika Pollards uzsvēra, kā transekte var būt salīdzināma tikai
# pati ar sevi, nevis savā starpā. Tādēļ izvēlos fenoloģiju būvēt pēc transekšu
# indeksu summu (jeb novērojumu skaiti) kartrā uzsk. reizē


#Iegūstam visus iespējamos vieta–datums pārus
vieta_datums <- Pollard_dataset_pilnais %>%
  distinct(vieta, datums)
vieta_datums <- vieta_datums[!is.na(vieta_datums$datums),]


# Direktorija 
dir.create("Pollard/fenologija", recursive = TRUE)



# Visas unikālās sugas
sugas <- unique(Pollard_dataset_pilnais$zinatniskais)

# Cikls pa visām sugām
for (izveleta_suga in sugas) {
  
  # Filtrēt tikai izvēlētas sugas novērojumus
  sugas_nov <- Pollard_dataset_pilnais %>%
    filter(zinatniskais == izveleta_suga) %>%
    group_by(vieta, datums) %>%
    summarise(skaits = n(), .groups = "drop")
  
  # Apvienot datus ar visiem vieta–datums pāriem (lai būtu arī 0 vērtības)
  suga_dati <- vieta_datums %>%
    left_join(sugas_nov, by = c("vieta", "datums")) %>%
    mutate(skaits = ifelse(is.na(skaits), 0, skaits))  # Aizpilda NA ar 0
  
  #Ja pēc tam vēl joprojām ir maz datu, izlaiž
  if (sum(suga_dati$skaits) < 2) next
  
  # Grafiks
  p <- ggplot(suga_dati, aes(x = datums, y = skaits, group = vieta, color = vieta)) +
    geom_line(size = 1.5) +
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



# Fenoloģijas grafiks kāpostu baltenim

fenol_pieris_brassicae <- Pollard_dataset_pilnais %>%
  filter(zinatniskais == "Pieris brassicae") %>%
  group_by(vieta, datums) %>%
  summarise(skaits = n(), .groups = "drop")

# Apvienot datus ar visiem vieta–datums pāriem (lai būtu arī 0 vērtības)
pieris_brassicae_dati <- vieta_datums %>%
  left_join(fenol_pieris_brassicae, by = c("vieta", "datums")) %>%
  mutate(skaits = ifelse(is.na(skaits), 0, skaits))  # Aizpilda NA ar 0


# Grafiks
ggplot(pieris_brassicae_dati, aes(x = datums, y = skaits, group = vieta, color = vieta)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_x_date(
    breaks = sort(unique(pieris_brassicae_dati$datums)),
    date_labels = "%d.%m"
  ) +
  scale_y_continuous(
    limits = c(0, 13),
    breaks = seq(0, max(pieris_brassicae_dati$skaits), by = 1)
  ) +
  labs(
    x = "Uzskaites datums",
    y = "Novēroto indivīdu skaits",
    color = "Pētījuma vieta"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.position = "right",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  ) + theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )












Pollard_dataset_pilnais_123 <- Pollard_dataset_pilnais[!Pollard_dataset_pilnais$josla == 4,] # Noņemt nov. rpus Pollarda telpas
Pollard_dataset_pilnais_123 <- Pollard_dataset_pilnais_123[!is.na(Pollard_dataset_pilnais_123$vieta),]

# Pollarda sastopamības indeksa salīdzinājums starp vietam =================================

# Direktorija
dir.create("Pollard/indeksi", recursive = TRUE)


sugas <- unique(Pollard_dataset_pilnais$zinatniskais)


for (izveleta_suga in sugas) {
  
  # Datu filtrēšana un sagatavošana
  sugas_indeksi <- Pollard_dataset_pilnais %>%
    filter(zinatniskais == izveleta_suga) %>%
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
    labs(x = "Pētījuma vieta", y = "Pollarda sastopamības indekss", title = izveleta_suga)
  
  # Saglabā attēlu
  ggsave(filename = file.path("Pollard/indeksi", paste0(gsub("[^a-zA-Z0-9_āčēģīķļņšūžĀČĒĢĪĶĻŅŠŪŽ]", "_", izveleta_suga), ".png")),
         plot = index_plot,
         width = 10,
         height = 6,
         dpi = 300,
         bg = "white")
}




kapostu_pindekss <- Pollard_dataset_pilnais %>%
  filter(zinatniskais == "Pieris brassicae") %>%
  group_by(vieta, trans_kods) %>%
  summarise(skaits = n(), .groups = "drop")


# Salīdzināt grupas 
kruskal.test(skaits ~ vieta, data = kapostu_pindekss)

dunn_test(skaits~vieta,data=kapostu_pindekss)








# Kopējie sugu skaiti pa vietām -------------------------------
sugas_kopsavilkums <- Pollard_dataset_pilnais_123 %>%
  group_by(zinatniskais, vieta) %>%
  summarise(skaits = n(), .groups = "drop") %>%
  pivot_wider(names_from = vieta, values_from = skaits, values_fill = 0)
sugas_kopsavilkums <- sugas_kopsavilkums[!is.na(sugas_kopsavilkums$zinatniskais),]

sugas_kopsavilkums[sugas_kopsavilkums$zinatniskais == "Pieris brassicae",]



# Pollarda indeksa grafiks Kāpostu baltenim
index_pieris_brassicae <- Pollard_dataset_pilnais %>%
  filter(zinatniskais == "Pieris brassicae") %>%
  group_by(vieta, trans_kods) %>%
  summarise(skaits = n(), .groups = "drop")


ggplot(index_pieris_brassicae, aes(x = vieta, y = skaits)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +    
  geom_jitter(width = 0.15, height = 0, size = 4, alpha = 0.8, color = "gray40") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, 
               fatten = 1.3, colour = "black", linewidth = 1) +
  stat_n_text() +

  scale_y_continuous(
    limits = c(0, NA),
    breaks = sort(unique(index_pieris_brassicae$skaits))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(size = 14),
    legend.position = "right",
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(
    x = "Pētījuma vieta",
    y = "Pollarda sastopamības indeksa vērtība",
    fill = "Pētījuma vieta",
    color = "Pētījuma vieta" 
  )




