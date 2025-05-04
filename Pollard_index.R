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




# Abundance indeksa salīdzinājums starp vietam -------------
unique(TDataset_pilnais$latviskais)

izveleta_suga <- "Kāpostu baltenis"

sugas_indeksi <- TDataset_pilnais %>%
  filter(latviskais == izveleta_suga) %>%        # tikai izvēlētā suga
  group_by(vieta, trans_kods) %>%       # grupē pēc vietas, transekta, uzskaites
  summarise(skaits = n(), .groups = "drop")      # saskaita indivīdus (rindas)

# Direktorija
dir.create("Pollard/indeksi", recursive = TRUE)



index_plot <- ggplot(sugas_indeksi, aes(x = vieta, y = skaits, fill = vieta, color = vieta)) +
  geom_violin(alpha = 0.5, linewidth = 1) +
  geom_jitter(width = 0.05, alpha = 1, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, colour = "black") +
  stat_n_text(y.pos = max(sugas_indeksi$skaits) + 1.5, size = 4, color = "black") +  # ← šeit tiek pielikts novērojumu skaits
  scale_y_continuous(breaks = seq(0, max(sugas_indeksi$skaits) + 2, by = 1)) +
  theme_minimal(base_size = 14) +
  labs(x = "Vieta", y = "Pollarda pārpilnības indekss", title = izveleta_suga)
index_plot


ggsave(filename = file.path("Pollard/indeksi", paste0(izveleta_suga, ".png")), 
       plot = index_plot, 
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")









# Sugu fenoloģijas liknes -----

# Savā metosika Pollards uzsvēra, kā transekte var būt salīdzināma tikai
# pati ar sevi, nevis savā starpā. Tādēļ izvēlos fenoloģiju būvēt pēc transekšu
# indeksu summu (jeb novērojumu skaiti) kartrā uzsk. reizē
.

#Iegūstam visus iespējamos vieta–datums pārus
vieta_datums <- TDataset_sugas %>%
  distinct(vieta, datums)
vieta_datums <- vieta_datums[!is.na(vieta_datums$datums),]


# Direktorija 
dir.create("Pollard/fenologija", recursive = TRUE)



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
  faila_cels <- file.path("Pollard/fenologija", paste0(faila_nosaukums, ".png"))
  
  ggsave(filename = faila_cels, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}













