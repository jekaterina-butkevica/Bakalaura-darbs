# Pakotnes ------------------------------
if(!require(readxl)) install.packages("readxl")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(unmarked)) install.packages("unmarked")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(corrplot)) install.packages("corrplot")



# Dati -----------------------------------
orig_TDataset <- read_excel("Dati/originalie_dati.xlsx", sheet = "Noverojumi")
summary(orig_TDataset)
Dist_Dataset <- orig_TDataset[!is.na(orig_TDataset$uzsk_ID),]  #Noņem tukšās rindas

# Datu pārskats -----------------
dim(Dist_Dataset)
unique(orig_TDataset$suga_komplekss)



#novērojumu skaita sadalījums pa joslam. Kāpostu baltenis un Pieris spp.

ggplot(
  data = orig_TDataset[orig_TDataset$suga_komplekss == "Parasto balteņu sugu grupa Pieris brassicae/ P. rapae/ P. napi", ],
  aes(x = josla)
) +
  geom_bar(fill = "gray", color = "gray") +
  labs(
    x = "Attāluma josla",
    y = "Novērojumu skaits"
  ) +
  theme_minimal()



ggplot(
  data = orig_TDataset[orig_TDataset$suga_komplekss == "Kāpostu baltenis Pieris brassicae", ],
  aes(x = josla)
) +
  geom_bar(fill = "gray", color = "gray") +
  labs(
    x = "Attāluma josla",
    y = "Novērojumu skaits"
  ) +
  theme_minimal()


## Noņemt nenoteiktās sugas ----
unique(Dist_Dataset$suga_komplekss)
sum(!is.na(Dist_Dataset$suga_komplekss))


nenoteiktas_sugas <- c("Nenoteikts zilenītis", 
                       "Parasto balteņu sugu grupa Pieris brassicae/ P. rapae/ P. napi", 
                       "Nenoteikts raibenis",
                       "Nenoteikts pļavraibenis",
                       "Nenoteikts resngalvītis Hesperia/ Ochlodes/ Thymelicus",
                       "Nenoteikts raibenis/ samtenis"
                       )
Dist_Dataset$suga_komplekss[Dist_Dataset$suga_komplekss %in% nenoteiktas_sugas] <- NA
summary(Dist_Dataset)

## Datums -------------------------------
Dist_Dataset$datums <- as.Date(Dist_Dataset$datums, format= "%d.%m.%Y") 
unique(Dist_Dataset$datums)
Dist_Dataset$Jday <- yday(Dist_Dataset$datums)
summary(Dist_Dataset)

## Vērtību aprēķins  ------------------
# Vidējas vērtības
sakums_colnames <- names(Dist_Dataset)[grepl("_sak$", names(Dist_Dataset))]

for (sak_col in sakums_colnames) {
  base_name <- sub("_sak$", "", sak_col)
  beig_col <- paste0(base_name, "_beig")
  
  if (beig_col %in% names(Dist_Dataset)) {
    avg_col <- paste0(base_name, "_vid")
    Dist_Dataset[[avg_col]] <- rowMeans(cbind(Dist_Dataset[[sak_col]], Dist_Dataset[[beig_col]]), na.rm = TRUE)
  }
}
summary(Dist_Dataset)
dim(Dist_Dataset)


# Jaunās kolonnas 
Dist_Dataset$ziedi_sum <- Dist_Dataset$rl_ziedi_vid + 
  Dist_Dataset$z_ziedi_vid + 
  Dist_Dataset$v_ziedi_vid +
  Dist_Dataset$a_ziedi_vid # Ziedu summa

Dist_Dataset$augi_sum <- Dist_Dataset$rl_augi_vid + 
  Dist_Dataset$z_augi_vid + 
  Dist_Dataset$v_augi_vid +
  Dist_Dataset$a_augi_vid # Augu summa




## Grupu balansēšana -----------
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))

ggplot(Dist_Dataset, aes(plausana)) + 
  geom_histogram(stat="count") 
Dist_Dataset <- Dist_Dataset %>% # Samazināt pļaušanas grupu skaitu
  mutate(plausana = case_when(
    plausana %in% c("Jā", "Daļēji") ~ "Pļauts",
    plausana %in% c("Nē") ~ "Nepļauts",
    TRUE ~ plausana
  ))


ggplot(Dist_Dataset, aes(citi_bojajumi)) + 
  geom_histogram(stat="count") 
Dist_Dataset <- Dist_Dataset %>% # Samazināt citi_bojajumi grupu skaitu
  mutate(citi_bojajumi = case_when(
    citi_bojajumi %in% c("Jā", "Daļēji") ~ "Ir",
    citi_bojajumi %in% c("Nē") ~ "Nav",
    TRUE ~ citi_bojajumi
  ))


ggplot(Dist_Dataset, aes(kust_int)) + 
  geom_histogram(stat="count") 
Dist_Dataset <- Dist_Dataset %>% # Samazināt kustības intensitātes grupu skaitu
  mutate(kust_int = case_when(
    kust_int %in% c("Vidējā") ~ "Augstā",
    kust_int %in% c("Nav") ~ "Zemā",
    TRUE ~ kust_int
  ))


ggplot(Dist_Dataset, aes(mitr_apst)) + 
  geom_histogram(stat="count") #OK


ggplot(Dist_Dataset, aes(spares)) + 
  geom_histogram(stat="count") # Izņemt – parāk lielā atšķirība skaitā


ggplot(Dist_Dataset, aes(traucejumi)) + 
  geom_histogram(stat="count") #OK


ggplot(Dist_Dataset, aes(izmainas)) + 
  geom_histogram(stat="count") +
  facet_wrap(~vieta) # Izņemt – disbalanss starp vietām


Dist_Dataset$bojajumi <- ifelse(  # Kopējie bojājumi (plaušana un nobrauksana u.c.)
 Dist_Dataset$plausana %in% c("Pļauts") | Dist_Dataset$citi_bojajumi %in% c("Jā", "Daļēji"),
  "Ir",
  "Nav"
)

ggplot(Dist_Dataset, aes(bojajumi)) + 
  geom_histogram(stat="count") #OK


## Lieku kolonnu dzēšana --------------------------- 
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))


Dist_Dataset <- Dist_Dataset %>% 
  dplyr::select(-laiks_sak, -laiks_beig, -laiks_vid, -trans_ilg, -temp_sak, -temp_beig,
         -vej_atr_sak, -vej_atr_beig, -rel_mitr_sak, -rel_mitr_beig, -makon_sak,
         -makon_beig, -apg_sak, -apg_beig, -veg_augst_sak, -veg_augst_beig,
         -rl_ziedi_sak, -rl_ziedi_beig, -rl_augi_sak, -rl_augi_beig, -z_ziedi_sak, 
         -z_ziedi_beig, -z_augi_sak, -z_augi_beig, -v_ziedi_sak, -v_ziedi_beig,
         -v_augi_sak, -v_augi_beig, -a_ziedi_sak, -a_ziedi_beig, -a_augi_sak,
         -a_augi_beig, -izmainas, -spares, -kods, -dzimta, -apaksdzimta, -gints, 
         -komentars, -kom_traucejumi, -kom_bojajumi)
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))



## Korelāciju pārbaude ---------------------------
ggplot(Dist_Dataset, aes(temp_vid)) + 
  geom_histogram(binwidth = 1)
shapiro.test(Dist_Dataset$temp_vid) # Temperatūra nav normāli sadalīta


ggplot(Dist_Dataset, aes(vej_atr_vid)) + 
  geom_histogram(binwidth = 1)
shapiro.test(Dist_Dataset$vej_atr_vid) # Vēja ātrums nav normāli sadalīts


ggplot(Dist_Dataset, aes(rel_mitr_vid)) + 
  geom_histogram(binwidth = 1)
shapiro.test(Dist_Dataset$rel_mitr_vid) # Relatīvais mitrums nav normāli sadalīta


ggplot(Dist_Dataset, aes(makon_vid)) + 
  geom_histogram(binwidth = 1) # Makoņu segums mitrums nav normāli sadalīta


ggplot(Dist_Dataset, aes(apg_vid)) + 
  geom_histogram(binwidth = 1) # Transektes apgaismojums mitrums nav normāli sadalīta


ggplot(Dist_Dataset, aes(veg_augst_vid)) + 
  geom_histogram(binwidth = 1)
shapiro.test(Dist_Dataset$veg_augst_vid) # Veģetācijas augstums nav normāli sadalīta



skaitliskie_mainigie <- dplyr::select_if(Dist_Dataset, is.numeric)

korelaciju_matrica <- cor(scale(skaitliskie_mainigie), method = "spearman", use = "complete.obs")
library(corrplot)
corrplot(korelaciju_matrica, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black")


augstas_korelaciajs <- which(abs(korelaciju_matrica) >= 0.6 & abs(korelaciju_matrica) < 1, arr.ind = TRUE)
augsto_korelaciju_matrica <- data.frame(
  Var1 = rownames(korelaciju_matrica)[augstas_korelaciajs[, 1]],
  Var2 = colnames(korelaciju_matrica)[augstas_korelaciajs[, 2]],
  Correlation = korelaciju_matrica[augstas_korelaciajs]
)
augsto_korelaciju_matrica




# Datu pārskats -----------------
ggplot(Dist_Dataset[Dist_Dataset$zinatniskais == "Pieris brassicae",], aes(josla)) + 
  geom_histogram(stat="count")
# Ir labi

ggplot(Dist_Dataset[Dist_Dataset$zinatniskais == "Pieris brassicae",], aes(josla)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba, nrow = 2)

ggplot(Dist_Dataset[Dist_Dataset$zinatniskais == "Pieris brassicae",], aes(josla, fill = vieta)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba*vieta, nrow = 2)

# Datu apjoms nav pietiekami liels, lai modelētu uzvedības veidus atsevisķi.















#Y tabula  -------------
colnames(Dist_Dataset)

# Summējam katrā transektē veiktos novērojumus pa attāluma joslām
TNov_pa_trans_j <- data.frame(Dist_Dataset) %>%
  group_by(uzsk_ID, trans_kods, zinatniskais, josla) %>%
  summarise(Det=n())
TNov_pa_trans_j <- data.frame(TNov_pa_trans_j)

# Platais formāts (identifikācijas lietas, suga, novērojumu skaits pa joslām)
TNov_pa_trans_j_w <- reshape(TNov_pa_trans_j, 
                             idvar=c("uzsk_ID", "trans_kods", "zinatniskais"), 
                             timevar="josla", direction="wide")

# Sakārtot joslu kolonnas pareizā secībā
colnames(TNov_pa_trans_j_w)
TNov_pa_trans_j_w <- TNov_pa_trans_j_w %>% dplyr::select(uzsk_ID, trans_kods, zinatniskais, 
                                                  Det.1, Det.2, Det.3, Det.4)

names(TNov_pa_trans_j_w)[4:7] <- c("J50", "J150", "J250", "Jtalak")
TNov_pa_trans_j_w[is.na(TNov_pa_trans_j_w)] <- 0
head(TNov_pa_trans_j_w)


# Izvēlēties sugu
unique(TNov_pa_trans_j_w$zinatniskais)

Tsuga <- "Pieris brassicae"

# Atlasām mūs interesējošo sugu novērojumus
Tsugasdati <- TNov_pa_trans_j_w[TNov_pa_trans_j_w$zinatniskais==Tsuga,]
head(Tsugasdati)
dim(Tsugasdati)


# Savienojam transektes, kur suga tika novērota, ar pārējām
notikusas_uzskaites=Dist_Dataset %>% 
  dplyr::select(trans_kods,uzsk_ID,Jday) %>% 
  distinct() %>% 
  arrange(trans_kods,uzsk_ID)

reizem=expand.grid(trans_kods=levels(factor(Dist_Dataset$trans_kods)),
                   uzsk_ID=1:6)

Tpilnais <- merge(notikusas_uzskaites, Tsugasdati, by=c("trans_kods", "uzsk_ID"), 
                  all.x=TRUE, sort=TRUE)
Tpilnais=Tpilnais %>% 
  dplyr::select(trans_kods,uzsk_ID,J50,J150,J250,Jtalak)
Tpilnais[is.na(Tpilnais)] <- 0

Tpilnais=Tpilnais %>% 
  full_join(reizem,by=c("trans_kods","uzsk_ID")) %>% 
  arrange(trans_kods,uzsk_ID)

# radām tabulu, kurā katram punktam ir visas teorētiski iespējamās uzskaites 
transektes <- sort(unique(Tpilnais$trans_kods))
transektes_uzsk <- data.frame(expand.grid(trans_kods = transektes, uzsk_ID = 1:6))

transektes_uzsk <- transektes_uzsk[order(transektes_uzsk$trans_kods, transektes_uzsk$uzsk_ID),] 
head(transektes_uzsk)
dim(transektes_uzsk)

# savienojam visu punktu visas uzskaites (arī nenotikušās) ar notikušo datiem
Tpilnais <- merge(transektes_uzsk, Tpilnais, by=c("trans_kods", "uzsk_ID"), all.x=TRUE, sort=FALSE)



head(Tpilnais)
dim(Tpilnais)


#Pārbaudīt, vai uzskaišu reižu skaits ir vienāds.
ggplot(Tpilnais, aes(trans_kods,)) + geom_histogram(,stat="count")


# Tauriņu tabula satur izvēlētās sugas novērojumu datus katrā punktā, sadalītus pa attāluma joslām
taurini <- Tpilnais
head(taurini)
tail(taurini)
TY <- reshape(taurini, v.names=c("J50", "J150", "J250", "Jtalak"), idvar="trans_kods", timevar="uzsk_ID", direction="wide")
head(TY)
TY <- subset(TY, select=c(-trans_kods))
dim(TY) # 119 transektes x 24 (4 joslas * 6 uzskaitēs)
















# Tabula "Uzskaites"  -----------------------
# Veidojam atsevišķu tabulu ar visām transektēm un uzskaites reizēm, lai tā kalpotu kā skelets
transektes <- sort(unique(Dist_Dataset$trans_kods))
trans_uzsk <- data.frame(expand.grid(trans_kods = transektes, uzsk_ID = 1:6))
head(trans_uzsk) 
tail(trans_uzsk)

# No Dist_Dataset atlasām unikālus transektu aprakstus
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))
TUzskaites <- Dist_Dataset[,c(1:10, 15: 32)]
TUzskaites <- TUzskaites %>%
  distinct(trans_kods, uzsk_ID, .keep_all = TRUE)



## Iztrūkstoša uzskaite -----
# Ķemeris 3. uzsdkaite
kemeri_trans <- Dist_Dataset %>%
  filter(vieta == "Ķemeri") %>%
  distinct(trans_kods) %>%
  slice_head(n = 39) %>%
  pull(trans_kods)

kemeri_3 <- tibble(
  vieta = "Ķemeri",
  uzsk_ID = 3,
  trans_kods = kemeri_trans
)
TUzskaites <- bind_rows(TUzskaites, kemeri_3)

# Ķemeri 1. uzskaite
kemeri_trans
Kemeri_1 <- Dist_Dataset[Dist_Dataset$vieta == "Ķemeri",] %>% filter(uzsk_ID == 1)
trans_1 <- Kemeri_1 %>% distinct(trans_kods)
dim(trans_1)

# Meklējam, kuras trūkst
kemeri_trans <- data.frame(trans_kods = kemeri_trans)
iztrukstosie <- anti_join(kemeri_trans, Kemeri_1, by = "trans_kods")

Kemeri_1_tuksas_rindas <- iztrukstosie %>%
  mutate(
    uzsk_ID = 1,
    vieta = "Ķemeri"
  )

TUzskaites <- bind_rows(TUzskaites, Kemeri_1_tuksas_rindas)


table(TUzskaites$trans_kods) # viur jābūt 6
table(table(TUzskaites$trans_kods))

TUzskaites=TUzskaites %>% 
  arrange(trans_kods,uzsk_ID)










# Tabula "Vietas" ----------------------------
TVietas <- read_excel("originalie_dati.xlsx", sheet = "Vietas")

TVietas <- TVietas[!TVietas$vieta=="Šlītere",] #Izņemt Šlīteres transektes
unique(TVietas$vieta)

TVietas$trans_kods
TVietas <- TVietas[-101,] # izslēgtā transekte

# Kolonnu dzēšana
data.frame(Numurs = seq_along(TVietas), Kolonna = names(TVietas))
TVietas <- TVietas[ , -c(8:9)]

# Grupu balansēšana
ggplot(TVietas, aes(kust_int)) + 
  geom_histogram(stat="count") 

TVietas <- TVietas %>% # Samazināt grupu skaitu
  mutate(kust_int = case_when(
    kust_int %in% c("Nav") ~ "Zemā",
    kust_int %in% c("Vidējā") ~ "Augstā",
    TRUE ~ kust_int
  ))

# Maksimālais stāvu skaits transektē
maks_stavi <- Dist_Dataset %>%
  group_by(trans_kods) %>%
  summarise(maks_stavu_sk = max(stavu_sk, na.rm = TRUE))

TVietas <- TVietas %>%
  left_join(maks_stavi, by = "trans_kods")

# Maksimālais ziedu skaits transektē
maks_ziedi <- Dist_Dataset %>%
  group_by(trans_kods) %>%
  summarise(maks_ziedu_sk = max(ziedi_sum, na.rm = TRUE))

TVietas <- TVietas %>%
  left_join(maks_ziedi, by = "trans_kods") %>% 
  arrange(trans_kods)






























# Modelēšana (visas uzvedības) ----------

#Modeļa iestatījumi
R = 119 #transekšu skaits. 
T <- 6  # 6 atkārtotas uzskaites
garums <- 100
TK=100 #	An integer value specifying the upper bound used in the integration.
Tdist.breaks <- c(0, 0.5, 1.5, 2.5, 4)
numDistClasses <- length(Tdist.breaks) - 1  # = 4

Tmixture="P"
TunitsOut="ha"
Toutput="density"


# izvēlētajai sugai
TudfGDS<-unmarkedFrameGDS(y=TY, 
                          siteCovs=TVietas,
                          dist.breaks=Tdist.breaks,
                          numPrimary=T, 
                          yearlySiteCovs=TUzskaites, 
                          survey="line",
                          unitsIn="m",
                          tlength=rep(garums, R))

summary(TudfGDS)
head(yearlySiteCovs(TudfGDS),12)

summary(TVietas)


## Detekcijas funkcijas un miksturas izvēle----
m0haz <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture=Tmixture, K=TK, unitsOut=TunitsOut)
summary(m0haz)

m0hazNB <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m0hazNB)


m0hal <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, K=TK, unitsOut=TunitsOut)
summary(m0hal)

m0halNB <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="halfnorm", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m0halNB)


# Lābākais modelis ir ar Negatīvais bnomiālais sadalījums un HazardRate detekcijas funkciju
# AICc =  678.6092 

# Pirmā līkne – pelēkā augšējā konfidences robeža
plot(function(x) gxhaz(x,
                       shape = confint(backTransform(m0hazNB, type = "det"))[2],
                       scale = confint(backTransform(m0hazNB, type = "scale"))[1]),
     from = 0, to = 4, col = gray(0.7),
     xlab = "Attālums (m)", ylab = "Konstatēšanas varbūtība",
     ylim = c(0, 1),
     cex.lab = 1.5,      # Palielina asi nosaukumu lielumu
     cex.axis = 1.3)     # Palielina asi vērtību lielumu

# Otrā līkne – pelēkā apakšējā konfidences robeža
plot(function(x) gxhaz(x,
                       shape = confint(backTransform(m0hazNB, type = "det"))[1],
                       scale = confint(backTransform(m0hazNB, type = "scale"))[2]),
     from = 0, to = 4, col = gray(0.7), add = TRUE)

# Vidējā līkne – modeļa aplēse
plot(function(x) gxhaz(x,
                       shape = backTransform(m0hazNB, type = "det")@estimate,
                       scale = backTransform(m0hazNB, type = "scale")@estimate),
     from = 0, to = 4, add = TRUE)


# Iegūstam aplēstos parametrus konkrētām attālumām
dist_shape_est <- backTransform(m0hazNB, type = "det")@estimate
dist_scale_est <- backTransform(m0hazNB, type = "scale")@estimate

attalumi <- c(0.3, 1, 2.5)

varbutibas <- gxhaz(attalumi, shape = dist_shape_est, scale = dist_scale_est)
data.frame(Attālums_m = attalumi, Konstatēšanas_varbūtība = round(varbutibas, 3))


# Summāra varbūtība katrai joslai ---------

joslas_dist <- c(0,0.5,1.5,2.5)


# Saglabā funkciju ar konkrētiem parametriem
det_fun <- function(x) gxhaz(x, shape = dist_shape_est, scale = dist_scale_est)

# Aprēķina kopējo detekcijas varbūtību katrā joslā (integrālis)
joslu_varbutibas <- sapply(1:(length(joslas_dist) - 1), function(i) {
  integrate(det_fun, lower = joslas_dist[i], upper = joslas_dist[i + 1])$value
})

# Ja vajag proporcionālo daļu (piemēram, sadalījumam):
joslu_garumi <- diff(joslas_dist)
joslu_vid_varbutibas <- joslu_varbutibas / joslu_garumi  # vidējā varbūtība katrā joslā

# Rezultāts
data.frame(
  Josla = paste(joslas_dist[-length(joslas_dist)], joslas_dist[-1], sep = "–"),
  Vid_varbutiba = round(joslu_vid_varbutibas, 3),
  Integralis = round(joslu_varbutibas, 3)
)












#Var aplēst sakitu katrai transektei atsevīšķi balstoties uz viedējo konstatēšanas
# iespēju starp transektem (neņem vērā individuālas atširības)
m0hazNB <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", 
                     K=TK, unitsOut=TunitsOut)
summary(m0hazNB)


# Parametru iegūve no modeļa
sigma_hat <- backTransform(m0hazNB, type = "det")@estimate # Izņemt det parametru - sigmu
scale_hat <- backTransform(m0hazNB, type = "scale")@estimate

# Attalūmu joslas viduspunkti un konstatēšanas varbūtība pa joslam
midpoints <- (head(Tdist.breaks, -1) + tail(Tdist.breaks, -1)) / 2 #joslu robežas viduspunkti
p_joslas <- gxhaz(midpoints, sigma_hat, scale = scale_hat) # KOnst. varb katrai joslai (tā kā gxhn darbojas pēc attāluma)


# Pielāgota sakita aplēses
Noverojumi <- Tsugasdati
joslu_nosaukumi <- c("J50", "J150", "J250", "Jtalak")

for (i in seq_along(joslu_nosaukumi)) {
  josla <- joslu_nosaukumi[i]
  p <- p_joslas[i]
  Noverojumi[[paste0(josla, "_apl")]] <- Noverojumi[[josla]] / p
}
Noverojumi$kopa <- rowSums(Noverojumi[, c("J50", "J150", "J250", "Jtalak")], na.rm = TRUE)
Noverojumi$aplestais_skaits <- rowSums(Noverojumi[, paste0(joslu_nosaukumi, "_apl")], na.rm = TRUE)


# Cik reizes atšķīras?
Noverojumi$reizes <- Noverojumi$aplestais_skaits/   Noverojumi$kopa 


#pievienot vietu
Noverojumi$vieta <- ifelse(grepl("^K", Noverojumi$trans_kods), "Ķemeri", 
                           ifelse(grepl("^A", Noverojumi$trans_kods), "Apšupe", "Ģipka"))


Noverojumi_index <- Noverojumi %>%
  group_by(trans_kods) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))




Noverojumi_index_long <- pivot_longer(Noverojumi_index, cols = c(aplestais_skaits, kopa),
                          names_to = "tips", values_to = "skaits")
Noverojumi_index_long$vieta <- ifelse(grepl("^K", Noverojumi_index_long$trans_kods), "Ķemeri", 
                           ifelse(grepl("^A", Noverojumi_index_long$trans_kods), "Apšupe", "Ģipka"))

# Aprēķina skaitu katrai vieta + tips kombinācijai
n_df <- Noverojumi_index_long %>%
  group_by(vieta, tips) %>%
  summarise(n = n(), .groups = "drop")

# Pielāgo grafikam:
ggplot(Noverojumi_index_long, aes(x = vieta, y = skaits, fill = tips, color = tips)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.shape = NA, width = 0.5, alpha = 0.4) + 
  stat_summary(fun = median, position = position_dodge(width = 0.6), geom = "crossbar", width = 0.5, fatten = 1, colour = "black", linewidth = 1) +
  
  geom_jitter(position = position_dodge(width = 0.6), alpha = 0.7, size = 4) +
  geom_text(data = n_df, aes(x = vieta, y = 0, label = paste0("n = ", n), group = tips), 
            position = position_dodge(width = 0.6), vjust = 1.5, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(Noverojumi_index_long$skaits), by = 5)
  ) +
  scale_fill_discrete(
    name = "Kopskaita veids",
    labels = c("Aplēstais", "Novērotais")
  ) +
  scale_color_discrete(
    name = "Kopskaita veids",
    labels = c("Aplēstais", "Novērotais")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 14),
    legend.position = "right",
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(
    x = "Pētījuma vieta",
    y = "Kopskaita vērtība"
  )



#Aplēstie dati
#Grupu salīdzinājums
null_apleses_dati <- Noverojumi_index_long %>%
  filter(tips != "kopa")
ggplot(null_apleses_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)

# Salīdzināt grupas - neparametriskai, jo sakita dati
kruskal.test(skaits ~ vieta, data = null_apleses_dati)
# Visās grupās pieder vienai ģenerālkopai


#Novēroti dati
#Grupu salīdzinājums
null_noveroti_dati <- Noverojumi_index_long %>%
  filter(tips == "kopa")

ggplot(null_noveroti_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)

# Salīdzināt grupas - neparametriskais, jo skaita dati
kruskal.test(skaits ~ vieta, data = null_noveroti_dati)
dunn_test(skaits~vieta,data=null_noveroti_dati)






# Detekcijas procesa modeli --------------
summary(TUzskaites)
summary(m0hazNB)

m_det_haz <- gdistsamp(~1,
                       ~1, 
                       ~scale(stavu_sk) + 
                         I(scale(stavu_sk)^2) + 
                         scale(veg_augst_vid) + 
                         I(scale(veg_augst_vid)^2), 
                         TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_det_haz)
#AIC: 686.2351 




m_det2_haz <- gdistsamp(~1,
                       ~1, 
                       ~scale(stavu_sk) + 
                         I(scale(stavu_sk)^2) + 
                         scale(veg_augst_vid), 
                       TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_det2_haz)
#AIC: 701.0999 



m_det3_haz <- gdistsamp(~1,
                       ~1, 
                       ~scale(stavu_sk)  + 
                         scale(veg_augst_vid), 
                       TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_det3_haz)
#AIC: 682.248 










# Sezonalitātes modelis ------
summary(TudfGDS)
m_Jday_haz<- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(Jday)^2), # phi = availability
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_Jday_haz)
#AIC 668.5932 


m_Jday3_haz<- gdistsamp(~1, # lambda = abundance
                        ~1+I(scale(Jday)^2) :
                          scale(Jday), # phi = availability
                        ~1, # pi = detection
                        TudfGDS, 
                        keyfun ="hazard", 
                        output=Toutput, 
                        mixture="NB",
                        K=TK, 
                        unitsOut=TunitsOut)
summary(m_Jday3_haz)
#AIC: 679.3602 

m_Jday2_haz<- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(Jday)^2) +
                         scale(Jday), # phi = availability
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_Jday2_haz)
#AIC: 668.0225
# LABAKAIS
options(digits = 5)









newdata=expand.grid(Jday=seq(153,244,1))
a1=as.data.frame(predict(m_Jday2_haz,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.4) +
  geom_line(size= 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Diena no gada sākuma", y = "Pieejamība uzskaitei") +  # Asu nosaukumi
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),  # Asu nosaukumu lielums
    axis.text = element_text(size = 12)    # Asu vērtību lielums
  )


# Iegūstam pieejamības prognozes visiem novērojumiem
phi_pred2 <- predict(m_Jday2_haz, type = "phi")
# Vidējā pieejamība visā sezonā (visiem datiem)
mean(phi_pred2$Predicted, na.rm = TRUE)


# Vidēja vērtība katrām menesim


phi_menesi <- kraa1 %>%
  mutate(Menesis = case_when(
    Jday >= 153 & Jday <= 182 ~ "Jūnijs",
    Jday >= 183 & Jday <= 213 ~ "Jūlijs",
    Jday >= 214 & Jday <= 244 ~ "Augusts"
  )) %>%
  group_by(Menesis) %>%
  summarise(Pieejamiba = mean(Predicted, na.rm = TRUE))





# Meģinātie modeļi -----
m_Jday_ziedi_vejs_trauc_haz <- gdistsamp(~1,
                                         ~1+I(scale(Jday)^2)+ 
                                           scale(ziedi_sum) + 
                                           scale(vej_atr_vid)+ 
                                           traucejumi,
                                         ~1, 
                                         TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_ziedi_vejs_trauc_haz) #AIC: 644.1765 







m_Jday_ziedi_temp_apg_ziedi_vejs_trauc_haz <- gdistsamp(~1,
                                                        ~1+I(scale(Jday)^2)+ 
                                                           scale(temp_vid) +
                                                           scale(apg_vid) +
                                                           scale(ziedi_sum) + 
                                                           scale(vej_atr_vid)+ 
                                                           traucejumi,
                                                        ~1, 
                                                        TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_temp_apg_ziedi_vejs_trauc_haz) #AIC: 643.746  






m_Jday_ziedi_temp_apg_ziedi_vejs_trauc_haz2 <- gdistsamp(~1,
                                                        ~1+I(scale(Jday)^2)+ 
                                                          I(scale(temp_vid)^2) +
                                                          scale(apg_vid) +
                                                          scale(ziedi_sum) + 
                                                          scale(vej_atr_vid)+ 
                                                          traucejumi,
                                                        ~1, 
                                                        TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_temp_apg_ziedi_vejs_trauc_haz2) #AIC: 643.7234  







m_Jday_makoni_haz2 <- gdistsamp(~1,
                                ~1+I(scale(Jday)^2)+
                                  I(scale(temp_vid)^2) +
                                  scale(makon_vid)+
                                  scale(apg_vid) +
                                  scale(ziedi_sum) + 
                                  scale(vej_atr_vid)+ 
                                  traucejumi,
                                ~1, 
                                TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_makoni_haz2) #AIC: 645.72 



m_Jday_plausana_haz2 <- gdistsamp(~1,
                                ~1+I(scale(Jday)^2)+
                                  I(scale(temp_vid)^2) +
                                  plausana +
                                  scale(apg_vid) +
                                  scale(ziedi_sum) + 
                                  scale(vej_atr_vid)+ 
                                  traucejumi,
                                ~1, 
                                TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)



summary(m_Jday_plausana_haz2) #AIC:  645.5324




m_Jday_bojajumi_haz2 <- gdistsamp(~1,
                                  ~1+I(scale(Jday)^2)+
                                    I(scale(temp_vid)^2) +
                                    bojajumi +
                                    scale(apg_vid) +
                                    scale(ziedi_sum) + 
                                    scale(vej_atr_vid)+ 
                                    traucejumi,
                                  ~1, 
                                  TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)



summary(m_Jday_bojajumi_haz2) #AIC:  645.5324 






m_Jtest_haz2 <- gdistsamp(~1,
                          ~1+I(scale(Jday)^2) +
                            scale(Jday)+
                            scale(apg_vid) +
                            scale(ziedi_sum) + 
                            scale(vej_atr_vid)+ 
                            traucejumi,
                          ~1, 
                          TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jtest_haz2) #AIC: 644.4507 




m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2 <- gdistsamp(~1,
                                                    ~1+I(scale(Jday)^2) +
                                                       scale(apg_vid) +
                                                       scale(ziedi_sum) + 
                                                       scale(vej_atr_vid)+ 
                                                       traucejumi,
                                                    ~1, 
                                                    TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2) #AIC: 642.4741







 





# Labākais modelis ---------------
m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2 <- gdistsamp(~1,
                                                    ~1+I(scale(Jday)^2) +
                                                      scale(apg_vid) +
                                                      scale(ziedi_sum) + 
                                                      scale(vej_atr_vid)+ 
                                                      traucejumi,
                                                    ~1, 
                                                    TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2) #AIC: 642.4741












# 2. Iegūt detekcijas funkcijas parametru (kopīgs visām transektēm)
sigma_hat2 <- backTransform(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2, type = "det")@estimate
scale_hat2 <- backTransform(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2, type = "scale")@estimate

# 3. Aprēķināt attāluma joslu viduspunkta detekcijas varbūtības
midpoints <- (head(Tdist.breaks, -1) + tail(Tdist.breaks, -1)) / 2 #joslu robežas viduspunkti
p_joslas <- gxhaz(midpoints, sigma_hat, scale = scale_hat) # KOnst. varb katrai joslai (tā kā gxhn darbojas pēc attāluma)


## Pielāgota sakita aplēses ---------
taurini2 <- taurini
joslu_nosaukumi <- c("J50", "J150", "J250", "Jtalak")

for (i in seq_along(joslu_nosaukumi)) {
  josla <- joslu_nosaukumi[i]
  p <- p_joslas[i]
  taurini2[[paste0(josla, "_apl")]] <- taurini2[[josla]] / p
}

taurini2$kopa <- rowSums(taurini2[, c("J50", "J150", "J250", "Jtalak")], na.rm = TRUE)
taurini2$aplestais_skaits <- rowSums(taurini2[, paste0(joslu_nosaukumi, "_apl")], na.rm = TRUE)

# 5. Iegūt phi katrai transektei
phi_pred <- predict(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2, type = "phi")$Predicted  # Pieņem, ka katra rinda = transekte
taurini2$phi_hat <- phi_pred

# 6. Novērojumu skaits katrai transektei
taurini2$aplestais_skaits_phi <- taurini2$aplestais_skaits / taurini2$phi_hat     



#pievienot vietu
taurini2$vieta <- ifelse(grepl("^K", taurini2$trans_kods), "Ķemeri", 
                           ifelse(grepl("^A", taurini2$trans_kods), "Apšupe", "Ģipka"))


taurini2_index <- taurini2 %>%
  group_by(trans_kods) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))




taurini2_index_long <- pivot_longer(taurini2_index, cols = c(aplestais_skaits_phi, kopa),
                                      names_to = "tips", values_to = "skaits")
taurini2_index_long$vieta <- ifelse(grepl("^K", taurini2_index_long$trans_kods), "Ķemeri", 
                                      ifelse(grepl("^A", taurini2_index_long$trans_kods), "Apšupe", "Ģipka"))

taurini2_index_long <- taurini2_index_long[!taurini2_index_long$skaits == 0,]
# Aprēķina skaitu katrai vieta + tips kombinācijai
n_df2 <- taurini2_index_long %>%
  group_by(vieta, tips) %>%
  summarise(n = n(), .groups = "drop")

# Pielāgo grafikam:
ggplot(taurini2_index_long, aes(x = vieta, y = skaits, color = tips, fill = tips)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.shape = NA, width = 0.5, alpha = 0.4) +    
  geom_jitter(position = position_dodge(width = 0.6), alpha = 0.7, size = 4) +
  stat_summary(fun = median, position = position_dodge(width = 0.6), geom = "crossbar", width = 0.5, fatten = 1, colour = "black", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6), 
               shape = 21, size = 6, colour = "black") +
  geom_text(data = n_df2, aes(x = vieta, y = 0, label = paste0("n=", n), group = tips), 
            position = position_dodge(width = 0.6), vjust = 1.5, color = "black") +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, max(taurini2_index_long$skaits), by = 5)
  ) +
  scale_fill_discrete(
    name = "Skaits",
    labels = c("Aplēstais pēc phi un det", "Novērotais")
  ) +
  scale_color_discrete(
    name = "Skaits",
    labels = c("Aplēstais pēc phi un det", "Novērotais")
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Pētījuma vieta",
    y = "Pollarda sastopamības indekss"
  )




## Grupu salīdzinājums --------
phi_apleses_dati <- taurini2_index_long %>%
  filter(tips != "kopa")
ggplot(phi_apleses_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)

# Salīdzināt grupas 
kruskal.test(skaits ~ vieta, data = phi_apleses_dati)
# Visās grupās pieder vienai ģenerālkopai



## Fenoloģija ---------- 

reference_uzskaites <- read_excel("originalie_dati.xlsx", sheet = "Uzskaites")

phi_fenol <- left_join(
  taurini2, 
  reference_uzskaites %>% select(uzsk_ID, vieta, datums), 
  by = c("uzsk_ID", "vieta"),
  relationship = "many-to-many"
)


phi_fenol_grupets <- phi_fenol %>%
  filter(!is.na(aplestais_skaits_phi)) %>%  # Filtrējam rindas, kur "aplestais_skaits_phi" ir NA
  group_by(vieta, datums) %>%
  summarise(skaits = sum(aplestais_skaits_phi), .groups = "drop")

phi_fenol_grupets$datums <- as.Date(phi_fenol_grupets$datums, format = "%Y-%m-%d")


phi_fenol_grupets <- phi_fenol_grupets %>%
  mutate(datums = case_when(
    datums == as.Date("2024-06-25") ~ as.Date("2024-06-26"),
    datums == as.Date("2024-07-17") ~ as.Date("2024-07-18"),
    TRUE ~ datums  # visi pārējie datumi paliek nemainīti
  ))



# Grafiks
ggplot(phi_fenol_grupets, aes(x = datums, y = skaits, group = vieta, color = vieta)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_x_date(
    breaks = sort(unique(phi_fenol_grupets$datums)),
    date_labels = "%d.%m"
  ) +
  scale_y_continuous(
    limits = c(0, max(phi_fenol_grupets$skaits)),
    breaks = seq(0, max(phi_fenol_grupets$skaits, na.rm = TRUE), by = 20)
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
    panel.grid.majorr.y = element_blank(),
    panel.grid.major.x = element_blank(),
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













#1 Pollarda apstākļu standarta pārbaude 1 --------------
## DoY modelis -----
m_Pollardtest_haz<- gdistsamp(~1, # lambda = abundance
                          ~1+I(scale(Jday)^2)+scale(Jday)+scale(temp_vid)+I(scale(temp_vid)^2) + scale(apg_vid), # phi = availability
                          ~1, # pi = detection
                          TudfGDS, 
                          keyfun ="hazard", 
                          output=Toutput, 
                          mixture="NB",
                          K=TK, 
                          unitsOut=TunitsOut)

summary(m_Pollardtest_haz)
#AIC: AIC: 657.8566 




m_Pollardtest2_haz<- gdistsamp(~1, # lambda = abundance
                              ~1+I(scale(Jday)^2) + I(scale(temp_vid)^2) + scale(apg_vid), # phi = availability
                              ~1, # pi = detection
                              TudfGDS, 
                              keyfun ="hazard", 
                              output=Toutput, 
                              mixture="NB",
                              K=TK, 
                              unitsOut=TunitsOut)

summary(m_Pollardtest2_haz)


m_Pollardtest3_haz<- gdistsamp(~1, # lambda = abundance
                               ~1+I(scale(Jday)^2) + I(scale(temp_vid)^2) + scale(apg_vid) + I(scale(apg_vid)^2), # phi = availability
                               ~1, # pi = detection
                               TudfGDS, 
                               keyfun ="hazard", 
                               output=Toutput, 
                               mixture="NB",
                               K=TK, 
                               unitsOut=TunitsOut)

summary(m_Pollardtest3_haz)



summary(TudfGDS)
m_Pollard_haz<- gdistsamp(~1, # lambda = abundance
                          ~1+I(scale(Jday)^2)+scale(temp_vid)+I(scale(temp_vid)^2) + scale(apg_vid), # phi = availability
                          ~1, # pi = detection
                          TudfGDS, 
                          keyfun ="hazard", 
                          output=Toutput, 
                          mixture="NB",
                          K=TK, 
                          unitsOut=TunitsOut)

summary(m_Pollard_haz)
#AIC: 657.0908 # Labakais







newdata=expand.grid(Jday=seq(153,244,5),
                    apg_vid=seq(0,100,20),
                    temp_vid=seq(13,33,5))



a1=as.data.frame(predict(m_Pollard_haz,type="phi",newdata))
kraa1=cbind(newdata,a1)
head(kraa1)
ggplot(kraa1, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(size=1) +
  scale_x_continuous(
    name = "Diena kopš gada sākuma",
    sec.axis = dup_axis(name = "Temperatūra")  # <- te augšējais virsraksts
  ) + 
  scale_y_continuous(
    name = "Pieejamība uzskaitei",
    limits = c(0, 1),
    sec.axis = sec_axis(~., name = "Apgaismojums")
    ) +
  labs(x = "Diena no gada sākuma", y = "Pieejamība uzskaitei") +
  # Asu nosaukumi
  theme_minimal() +
  theme(
    axis.text.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title = element_text(size = 18),  # Asu nosaukumu lielums
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14) # Asu vērtību lielums
  ) +
  facet_grid(
    apg_vid ~ temp_vid,
    labeller = labeller(
      apg_vid = c("0" = "0%", "20" = "20%", "40" = "40%", "60" = "60%", "80" = "80%", "100" = "100%"),
      temp_vid = c("13" = "13°C", "18" = "18°C", "23" = "23°C", "28" = "28°C", "33" = "33°C")
    )
  )


# Q1: Vai ir atškirības standarta galos? Starp minimālam prasībam un maksimāli iespējamiem laikapstāklīe
# Q2: Vai pie vienada temp ir atsķirības starp dažādiem apg?
# G3: Vai Pie viena apgaismojuma ir atšķirības starp dažādam temp



## Q1 grupu salīdzinājums --------

min_apstakli <- subset(kraa1, temp_vid == 13 & apg_vid == 60)
lotilabi_apstakli <- subset(kraa1, temp_vid == 33 & apg_vid == 100)

# Apvienot un sagatavot salīdzināšanai
salidzin1 <- rbind(
  data.frame(grupa = "G1", Predicted = min_apstakli$Predicted, SE = min_apstakli$SE),
  data.frame(grupa = "G2", Predicted = lotilabi_apstakli$Predicted, SE = lotilabi_apstakli$SE)
)


### normalitate -----
library(qqplotr)
ggplot(salidzin1, aes(sample = Predicted)) + facet_wrap(~grupa) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teoretiskas kvantiles", y = "Paraugkopas kvantiles")
# ne ļoti - astes iet prom

ggplot(salidzin1, aes(grupa, Predicted)) +
  geom_boxplot()
# Nav ietekmējošo vertibu, skaidrs kā grupas atšķiras


tapply(salidzin1$Predicted, salidzin1$grupa, shapiro.test)
# Abas normāli sadalīti

### Dispersiju salīdzināsana ----
var.test(salidzin1$Predicted ~ salidzin1$grupa)

# Nav homogēnas dispresijas


### Salīdzināšana ----
t.test(salidzin1$Predicted ~ salidzin1$grupa, var.equal = FALSE)



## Q2 Salīdzinājums vienāda temp-----

temp23 <- subset(kraa1, temp_vid == 23)
temp23$apg_f <- factor(temp23$apg_vid)

### Normalitates parbaude -----
#labs grafiks
ggplot(temp23, aes(apg_f, Predicted)) + geom_boxplot() +
  scale_y_continuous(limits = c(0, 0.5)) +  # <-- iestati sev vajadzīgo intervālu
  theme_minimal()


tapply(temp23$Predicted, temp23$apg_f, shapiro.test)
# visi ir normāli sadalīti

### Dispersijas homoģenitātes parbaude ----
library(car)
leveneTest(y = temp23$Predicted, group = temp23$apg_f)
# Nav vienādas dispersijas

### Grupu salīdizināšana ----
oneway.test(Predicted ~ apg_f, data = temp23, var.equal = FALSE)

library(rstatix)
ght1 <- games_howell_test(Predicted ~ apg_f, data = temp23)
options(scipen = 99)



## Q3 Salīdzinājums -----
apg40 <- subset(kraa1, apg_vid == 40)
apg40$temp_f <- factor(apg40$temp_vid) # pārverst par faktoru


### Normalitātes parbaude ----
#labs grafiks
ggplot(apg40, aes(temp_f, Predicted)) + geom_boxplot()+
  scale_y_continuous(limits = c(0, 0.5)) + 
  theme_minimal()
  

tapply(apg40$Predicted, apg40$temp_f, shapiro.test)
# visi ir normāli sadalīti

### Dispersijjas homoģenitātes parbaude ----
library(car)
leveneTest(y = apg40$Predicted, group = apg40$temp_f)
# Nav vienādas dispersijas


### Grupu salīdzināšana ----
oneway.test(Predicted ~ temp_f, data = apg40, var.equal = FALSE)

library(rstatix)
ght2 <- games_howell_test(Predicted ~ temp_f, data = apg40)
options(scipen = 99)



# 2Pollarda standarta pārbaude ----
## DoY modelis -----
summary(TudfGDS)
m_Pollard2_haz<- gdistsamp(~1, # lambda = abundance
                          ~1+I(scale(Jday)^2)+I(scale(temp_vid)^2) + scale(vej_atr_vid), # phi = availability
                          ~1, # pi = detection
                          TudfGDS, 
                          keyfun ="hazard", 
                          output=Toutput, 
                          mixture="NB",
                          K=TK, 
                          unitsOut=TunitsOut)

summary(m_Pollard2_haz)
#AIC 651.3184 
### LABAIKAIS pēc VISPĀR

m_Pollard2test_haz<- gdistsamp(~1, # lambda = abundance
                           ~1+I(scale(Jday)^2)+scale(Jday)+scale(temp_vid)+I(scale(temp_vid)^2) + scale(vej_atr_vid), # phi = availability
                           ~1, # pi = detection
                           TudfGDS, 
                           keyfun ="hazard", 
                           output=Toutput, 
                           mixture="NB",
                           K=TK, 
                           unitsOut=TunitsOut)

summary(m_Pollard2test_haz)
#AIC 653.0187


m_Pollard3test_haz<- gdistsamp(~1, # lambda = abundance
                               ~1+I(scale(Jday)^2)+I(scale(temp_vid)^2)+scale(temp_vid) + scale(vej_atr_vid), # phi = availability
                               ~1, # pi = detection
                               TudfGDS, 
                               keyfun ="hazard", 
                               output=Toutput, 
                               mixture="NB",
                               K=TK, 
                               unitsOut=TunitsOut)

summary(m_Pollard3test_haz)
#AIC 651.3184 
### LABAIKAIS pēc AIC


m_Pollard4test_haz<- gdistsamp(~1, # lambda = abundance
                               ~1+I(scale(Jday)^2)+I(scale(temp_vid)^2)+scale(temp_vid) + I(scale(vej_atr_vid)^2) +scale(vej_atr_vid), # phi = availability
                               ~1, # pi = detection
                               TudfGDS, 
                               keyfun ="hazard", 
                               output=Toutput, 
                               mixture="NB",
                               K=TK, 
                               unitsOut=TunitsOut)

summary(m_Pollard4test_haz)
#AIC 653.0096 





newdata=expand.grid(Jday=seq(153,244,5),
                    vej_atr_vid=seq(0,5,1),
                    temp_vid=seq(13,35,5))


a2=as.data.frame(predict(m_Pollard2_haz,type="phi",newdata))
kraa2=cbind(newdata,a2)

ggplot(kraa2, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(size=1) +
  scale_x_continuous(
    name = "Diena kopš gada sākuma",
    sec.axis = dup_axis(name = "Temperatūra")  # <- te augšējais virsraksts
  ) + 
  scale_y_continuous(
    name = "Pieejamība uzskaitei",
    limits = c(0, 1),
    sec.axis = sec_axis(~., name = "Vēja ātums")
  ) +
  labs(x = "Diena no gada sākuma", y = "Pieejamība uzskaitei") +
  # Asu nosaukumi
  theme_minimal() +
  theme(
    axis.text.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title = element_text(size = 18),  # Asu nosaukumu lielums
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14) # Asu vērtību lielums
  ) +
  facet_grid(
    vej_atr_vid ~ temp_vid,
    labeller = labeller(
      vej_atr_vid = c("0" = "0 m/s", "1" = "1 m/s", "2" = "2 m/s", "3" = "3 m/s", "4" = "4 m/s", "5" = "5 m/s"),
      temp_vid = c("13" = "13°C", "18" = "18°C", "23" = "23°C", "28" = "28°C", "33" = "33°C")
    )
  )




# Q1: Vai ir atškirības piejamibā dažāda veja stiprumā?

### Normalitates parbaude -----
#labs grafiks
kraa2$vejs_f <- factor(kraa2$vej_atr_vid) # pārverst par faktoru


ggplot(kraa2, aes(vejs_f, Predicted)) + geom_boxplot() +
  scale_y_continuous(limits = c(0, 0.5)) +  # <-- iestati sev vajadzīgo intervālu
  theme_minimal()


tapply(kraa2$Predicted, kraa2$vejs_f, shapiro.test)
# neviens nav normālis sadalīts

###  Grupu salīdzinājums- ----
kruskal.test(Predicted ~ vejs_f, data = kraa2)
vejs_pari <- dunn_test(Predicted~vejs_f,data=kraa2)


















# Labākais modelis
m_Jday_ziedi_vejs_haz <- gdistsamp(~1,
                                         ~1+scale(Jday)+ scale(ziedi_sum) + scale(vej_atr_vid),
                                         ~1, 
                                         TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_ziedi_vejs_haz) #AIC: 653.7568   Ziedi un vejs ir būtiski, Jday kļust sliktāks




# 2. Iegūt detekcijas funkcijas parametru (kopīgs visām transektēm)
sigma_hat1 <- backTransform(m_Jday_ziedi_vejs_haz, type = "det")@estimate
scale_hat1 <- backTransform(m_Jday_ziedi_vejs_haz, type = "scale")@estimate

# 3. Aprēķināt attāluma joslu viduspunkta detekcijas varbūtības
midpoints <- (head(Tdist.breaks, -1) + tail(Tdist.breaks, -1)) / 2
p_joslas1 <- gxhaz(midpoints, sigma_hat1, scale = scale_hat1)

# 4. Aprēķināt vidējo detekcijas varbūtību visām joslām kopā (p_avg)
# Pieņemot, ka attālumu joslas ir vienāda platuma — vidējā var būt vidējais no `p_joslas`
p_avg1 <- mean(p_joslas1)

# 5. Iegūt phi katrai transektei
phi_pred <- predict(m_Jday_ziedi_vejs_haz, type = "phi")$Predicted  # Pieņem, ka katra rinda = transekte
Noverojumi$phi_hat <- phi_pred

# 6. Novērojumu skaits katrai transektei
Noverojumi$kopa <- rowSums(Noverojumi[, c("J50", "J150", "J250", "Jtalak")], na.rm = TRUE)

# 7. Aprēķināt koriģēto skaitu katrai transektei
Noverojumi$aplestais_skaits_phi <- Noverojumi$kopa / (Noverojumi$phi_hat * p_avg1)
















