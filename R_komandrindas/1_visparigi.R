# Pakotnes -----
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(forcats)) install.packages("forcats")
if(!require(dplyr)) install.packages("dplyr")


# Dati --------
dati <- read_excel("Dati/originalie_dati.xlsx", sheet = "Noverojumi")
summary(dati)
dati <- dati[!is.na(dati$zinatniskais),]  #Noņem tukšās rindas
dim(dati)

# Sugu saraksts un skaits
unique(dati$zinatniskais[!is.na(dati$zinatniskais)])
length(unique(dati$zinatniskais[!is.na(dati$zinatniskais)]))

# Katras sugas novērojumu skaits
Species_obs <- dati %>%
  filter(!is.na(zinatniskais)) %>%
  group_by(zinatniskais) %>%
  summarise( 
    suga_kompleks = first(suga_komplekss),
    observations = n(),
  )
Species_obs <- Species_obs[order(-Species_obs$observations),]




# Attēls: novērojumu sadalījums starp sugām
ggplot(Species_obs, aes(x = fct_reorder(zinatniskais, -observations), y = observations)) +
  geom_col() +
  theme_minimal(base_size = 16) +
  scale_y_continuous(limits = c(0,100))+
  geom_text(aes(label = paste("n =", observations)), 
            angle = 90, vjust = 0.1, hjust =- 0.05, size = 4) +  # Teksts uz stabiņa
  theme(
    axis.text.x = element_text(size = 14, angle = 90, vjust = 0,  hjust = 1,  face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Suga",
    y = "Novērojumu skaits"
  )





# Cik sugas no katras dzimtas
sugas_dzimtas <- dati %>%
  group_by(dzimta) %>%
  summarise(sugu_skaits = n_distinct(suga_komplekss)) 
print(sugas_dzimtas)



# Kāpostu balteņa novērojumu sadalījums starp vietām
sugu_noverojumi_vietas <- dati %>%
  group_by(zinatniskais, vieta) %>%
  summarise(novērojumi = n(), .groups = "drop")

sugu_noverojumi_vietas <- sugu_noverojumi_vietas %>%
  pivot_wider(names_from = vieta, values_from = novērojumi, values_fill = list(novērojumi = 0))
print(sugu_noverojumi_vietas[sugu_noverojumi_vietas$zinatniskais=="Pieris brassicae",])

