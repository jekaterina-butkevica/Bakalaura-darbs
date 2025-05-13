# Pakotnes ------------------------------
if(!require(readxl)) install.packages("readxl")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(unmarked)) install.packages("unmarked")
if(!require(MASS)) install.packages("MASS")
if(!require(ggplot2)) install.packages("ggplot2")

# Dati -----------------------------------
orig_TDataset <- read_excel("originalie_dati.xlsx", sheet = "Noverojumi")
summary(orig_TDataset)
Dist_Dataset <- orig_TDataset[!is.na(orig_TDataset$uzsk_ID),]  #Noņem tukšās rindas
dim(Dist_Dataset)


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

## Laiks ---------------------------------
#head(Dist_Dataset$laiks_sak)
#Dist_Dataset$laiks_min_sak <-  as.numeric(format(Dist_Dataset$laiks_sak, "%H")) * 60 + as.numeric(format(Dist_Dataset$laiks_sak, "%M"))
#Dist_Dataset$laiks_min_beig <-  as.numeric(format(Dist_Dataset$laiks_beig, "%H")) * 60 + as.numeric(format(Dist_Dataset$laiks_beig, "%M"))


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
  select(-laiks_sak, -laiks_beig, -laiks_vid, -trans_ilg, -temp_sak, -temp_beig,
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
corrplot(korelaciju_matrica, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")


augstas_korelaciajs <- which(abs(korelaciju_matrica) >= 0.7 & abs(korelaciju_matrica) < 1, arr.ind = TRUE)
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
TNov_pa_trans_j_w <- TNov_pa_trans_j_w %>% select(uzsk_ID, trans_kods, zinatniskais, 
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
  geom_jitter(position = position_dodge(width = 0.6), alpha = 0.7, size = 4) +
  stat_summary(fun = median, position = position_dodge(width = 0.6), geom = "crossbar", width = 0.5, fatten = 1, colour = "black", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6), 
               shape = 21, size = 6, colour = "black") +
  geom_text(data = n_df, aes(x = vieta, y = 0, label = paste0("n=", n), group = tips), 
            position = position_dodge(width = 0.6), vjust = 1.5, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(Noverojumi_index_long$skaits), by = 5)
  ) +
  scale_fill_discrete(
    name = "Skaits",
    labels = c("Nulles modeļa aplēstais", "Novērotais")
  ) +
  scale_color_discrete(
    name = "Skaits",
    labels = c("Nulles modeļa aplēstais", "Novērotais")
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Pētījuma vieta",
    y = "Pollarda sastopamības indekss"
  )



# Grupu salīdzinājums
null_apleses_dati <- Noverojumi_index_long %>%
  filter(tips != "kopa")
ggplot(null_apleses_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)
tapply(null_apleses_dati$skaits, null_apleses_dati$vieta, shapiro.test)
# Ģipka nav normāli sadalīta
# Salīdzināt grupas 
kruskal.test(skaits ~ vieta, data = null_apleses_dati)
# Visās grupās pieder vienai ģenerālkopai


null_noveroti_dati <- Noverojumi_index_long %>%
  filter(tips == "kopa")

ggplot(null_noveroti_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)
tapply(null_noveroti_dati$skaits, null_noveroti_dati$vieta, shapiro.test)
# Neviens nav normāli sadalīts
# Salīdzināt grupas 
kruskal.test(skaits ~ vieta, data = null_noveroti_dati)
dunn_test(skaits~vieta,data=null_noveroti_dati)






# Detekcijas procesa modelis --------------
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


newdata=expand.grid(Jday=seq(153,244,5))
a1=as.data.frame(predict(m_Jday_haz,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.4) +
  geom_line(size= 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Diena no gada sākuma", y = "Pieejamība uzskaitei") +  # Asu nosaukumi
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),  # Asu nosaukumu lielums
    axis.text = element_text(size = 14)    # Asu vērtību lielums
  )



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



m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2 <- gdistsamp(~1,
                                                    ~1+I(scale(Jday)^2) +
                                                       scale(apg_vid) +
                                                       scale(ziedi_sum) + 
                                                       scale(vej_atr_vid)+ 
                                                       traucejumi,
                                                    ~1, 
                                                    TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2) #AIC: AIC: 642.4741



 





# Labākais modelis ---------------
m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2 <- gdistsamp(~1,
                                                    ~1+I(scale(Jday)^2) +
                                                      scale(apg_vid) +
                                                      scale(ziedi_sum) + 
                                                      scale(vej_atr_vid)+ 
                                                      traucejumi,
                                                    ~1, 
                                                    TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)


summary(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2) #AIC: AIC: 642.4741


# 2. Iegūt detekcijas funkcijas parametru (kopīgs visām transektēm)
sigma_hat2 <- backTransform(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2, type = "det")@estimate
scale_hat2 <- backTransform(m_Jday_ziedi_apg_ziedi_vejs_trauc_haz2, type = "scale")@estimate

# 3. Aprēķināt attāluma joslu viduspunkta detekcijas varbūtības
midpoints <- (head(Tdist.breaks, -1) + tail(Tdist.breaks, -1)) / 2 #joslu robežas viduspunkti
p_joslas <- gxhaz(midpoints, sigma_hat, scale = scale_hat) # KOnst. varb katrai joslai (tā kā gxhn darbojas pēc attāluma)


# Pielāgota sakita aplēses
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
  theme_minimal(base_size = 14) +
  labs(
    x = "Pētījuma vieta",
    y = "Pollarda sastopamības indekss"
  )




# Grupu salīdzinājums
phi_apleses_dati <- taurini2_index_long %>%
  filter(tips != "kopa")
ggplot(phi_apleses_dati, aes(skaits)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~vieta)
tapply(phi_apleses_dati$skaits, phi_apleses_dati$vieta, shapiro.test)
# Mekas nav normāli sadalīts
# Salīdzināt grupas 
kruskal.test(skaits ~ vieta, data = phi_apleses_dati)
# Visās grupās pieder vienai ģenerālkopai

















# Viens faktors

m_mitr_haz <- gdistsamp(~1,
                        ~1+scale(rel_mitr_vid),
                        ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_mitr_haz) # AIC: 667.1465 Mitrums nav būtisks


m_apg_haz <- gdistsamp(~1,
                       ~1+scale(apg_vid),
                       ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_apg_haz) # AIC: AIC: 678.3386  nav būtisks


m_apg2_haz <- gdistsamp(~1,
                       ~1+I(scale(apg_vid)^2),
                       ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_apg2_haz) # AIC: AIC: 678.3386  nav būtisks


m_vejs_haz <- gdistsamp(~1,
                        ~1+scale(vej_atr_vid),
                        ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_vejs_haz) # AIC: 660.0816 nav būtisks



m_Jday_haz <- gdistsamp(~1,
                        ~1+I(scale(Jday)^2),
                        ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_haz) #AIC: 668.5932 



# Daudz faktoru
summary(TUzskaites)

m_Jday_temp_vejs_haz <- gdistsamp(~1,
                        ~1+scale(Jday) + I(scale(temp_vid)^2) + I(scale(vej_atr_vid)^2),
                        ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_temp_vejs_haz) # AIC: 669.2091 nav būtisks




m_Jday_temp_vejs_kust_haz <- gdistsamp(~1,
                                  ~1+scale(Jday) + I(scale(temp_vid)^2) + I(scale(vej_atr_vid)^2)+ kust_int,
                                  ~1+veg_aug, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_temp_vejs_kust_haz) # AIC: 663.3629 traucējumi ir būtiski



m_Jday_ziedi_temp_vejs_trauc_haz <- gdistsamp(~1,
                                       ~1+scale(Jday)+ scale(ziedi_sum) + scale(temp_vid) + scale(vej_atr_vid)+ traucejumi,
                                       ~1+stavu_sk, 
                                       TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_ziedi_temp_vejs_trauc_haz) # AIC: 657.3271  Ziedi un vejs ir būtiski




m_Jday_ziedi_temp_vejs_haz <- gdistsamp(~1,
                                              ~1+scale(Jday)+ scale(ziedi_sum) + scale(temp_vid) + scale(vej_atr_vid),
                                              ~1+stavu_sk, 
                                              TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_ziedi_temp_vejs_haz) # AIC: 657.2529



m_Jday_ziedi_vejs_trauc_haz <- gdistsamp(~1,
                                         ~1+I(scale(Jday)^2)+ scale(ziedi_sum) + scale(vej_atr_vid)+ traucejumi,
                                         ~1+stavu_sk, 
                                         TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", K=TK, unitsOut=TunitsOut)
summary(m_Jday_ziedi_vejs_trauc_haz) # AIC: 655.4917   Ziedi un vejs ir būtiski









# Pollarda apstākļu standarta pārbaude 1 --------------
## DoY modelis -----
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
newdata=expand.grid(Jday=seq(153,244,5),
                    apg_vid=seq(0,100,20),
                    temp_vid=seq(13,35,5))

newdata=expand.grid(Jday=seq(153,244,5),
                    vej_atr_vid=seq(0,11,3),
                    temp_vid=seq(13,35,5),
                    vej_atr_vid=seq(0,11,3))


a1=as.data.frame(predict(m_Pollard_haz,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(size=1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Diena no gada sākuma", y = "Pieejamība uzskaitei") +  # Asu nosaukumi
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),  # Asu nosaukumu lielums
    axis.text = element_text(size = 14)    # Asu vērtību lielums
  ) +
  facet_grid(apg_vid~temp_vid)























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









# Pollarda apstākļu standarta pārbaude 1 --------------
## DoY modelis -----
summary(TudfGDS)
m_Jday_haz<- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(Jday)^2)+scale(temp_vid)+I(scale(temp_vid)^2)+scale(vej_atr_vid) + scale(apg_vid) + I(scale(apg_vid)^2), # phi = availability
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_Jday_haz) # Pieejamība nav būtiska


sh <- predict(m_doy_hal,type="lambda")/20 # skaits uz hektāru dalīt uz 20 = skaits uz transektes platību
sp <- predict(m_doy_hal,type="phi")/20


newdata=expand.grid(#vieta=c("Apšupe","Ģipka","Ķemeri")
  Jday=seq(160,250,10)
  ,vej_atr_vid=seq(0,5,1)
  ,temp_vid=seq(13,35,5)
  , traucejumi =c("Ir", "Nav")
)
a=as.data.frame(predict(m_doy_hal,type="phi",newdata))
kraa=cbind(newdata,a)
ggplot(kraa, aes(Jday, Predicted, fill = traucejumi, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5, colour = NA) +
  geom_line(aes(color = traucejumi)) +
  scale_fill_manual(values = c("red", "limegreen")) +
  scale_color_manual(values = c("darkred", "darkgreen")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  facet_grid(vej_atr_vid ~ temp_vid)





