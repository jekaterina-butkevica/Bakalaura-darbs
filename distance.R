# Pakotnes ------------------------------
if(!require(readxl)) install.packages("readxl")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(unmarked)) install.packages("unmarked")
if(!require(MASS)) install.packages("MASS")

# Dati -----------------------------------
orig_TDataset <- read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")
summary(orig_TDataset)
Dist_Dataset <- orig_TDataset[!is.na(orig_TDataset$uzsk_ID),]  #Noņem tukšās rindas
dim(Dist_Dataset)



## Datums -------------------------------
Dist_Dataset$datums <- as.Date(Dist_Dataset$datums, format= "%d.%m.%Y") 
unique(Dist_Dataset$datums)
Dist_Dataset$Jday <- yday(Dist_Dataset$datums)
summary(Dist_Dataset)

## Laiks ---------------------------------
head(Dist_Dataset$laiks_sak)
Dist_Dataset$laiks_min_sak <-  as.numeric(format(Dist_Dataset$laiks_sak, "%H")) * 60 + as.numeric(format(Dist_Dataset$laiks_sak, "%M"))
Dist_Dataset$laiks_min_beig <-  as.numeric(format(Dist_Dataset$laiks_beig, "%H")) * 60 + as.numeric(format(Dist_Dataset$laiks_beig, "%M"))


## Vērtību aprēķins un lieku kolonnu dzēšana ------------------

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



# Nevajadzīgo kolonnu dzēšana
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))
Dist_Dataset <- Dist_Dataset[ , -c(4:20, 22:37, 40, 41, 44:46, 48:52, 55:57, 59,60)]
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

Dist_Dataset$bojajumi <- ifelse(  # Kopējie bojājumi (plaušana un nobrauksana u.c.)
  Dist_Dataset$plausana %in% c("Jā", "Daļēji") | Dist_Dataset$citi_bojajumi %in% c("Jā", "Daļēji"),
  "Ir",
  "Nav"
)


## Grupu balansēšana ----------------------

ggplot(Dist_Dataset, aes(kust_int)) + 
  geom_histogram(stat="count") +
  facet_wrap(~vieta)
# Nē. Sanāk nesabalansēti.


ggplot(Dist_Dataset, aes(traucejumi)) + 
  geom_histogram(stat="count") +
  facet_wrap(~vieta)
# Šeit ir daudz izlidzinātāk, turklāt, šī pazīme norāda uz faktisko gadijumu, nevis
# uz traucējumu iespējamību, kā ar "kust_int". Un ļauj piemēram atšķirt vai faktiski
# traucejumi bija vai nē, jo piemēram grupa "mazā", lielā varbūtība, kā nekas nebija
# noticis. Tomēr mīnus ir tas, kā šajā pazīmē nevar atšķirt kas tieši bija par traucējumu.

data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))
Dist_Dataset <- Dist_Dataset[ , -8]


Dist_Dataset <- Dist_Dataset %>% # Samazināt pļaušanas grupu skaitu
  mutate(plausana = case_when(
    plausana %in% c("Jā", "Daļēji") ~ "Pļauts",
    plausana %in% c("Nē") ~ "Nepļauts",
    TRUE ~ plausana
  ))


Dist_Dataset <- Dist_Dataset %>% # Samazināt uzvedības grupu skaitu
  mutate(uzvediba = case_when(
    uzvediba %in% c("Barojas uz zieda", "Barojas uz auga") ~ "Barojas",
    uzvediba %in% c("Uzturas lokāli", "Pārojas") ~ "Uzturas lokāli",
    TRUE ~ uzvediba
  ))



# Datu pārskats -----------------
ggplot(Dist_Dataset, aes(josla)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba, nrow = 2)

ggplot(Dist_Dataset, aes(josla, fill = vieta)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba*vieta, nrow = 2)
# Ķemeros izteikti atšķirīgs sadalījums. Bet es zinu, ka tas tā ir tikai visiem tauriņiem kopā
# jo lielas sugas var konstatēt no lielāka attalumā.








# Tabula "Uzskaites"  -----------------------
# Veidojam atsevišķu tabulu ar visām transektēm un uzskaites reizēm, lai tā kalpotu kā skelets
transektes <- sort(unique(Dist_Dataset$trans_kods))
trans_uzsk <- data.frame(expand.grid(trans_kods = transektes, uzsk_ID = 1:6))
head(trans_uzsk) 
tail(trans_uzsk)

# No Dist_Dataset atlasām unikālus transektu aprakstus
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))

TUzskaites <- Dist_Dataset[,c(1:7, 11: 29)]
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













# Tabula "Vietas" ----------------------------
TVietas <- read_excel("uzskaisu_dati.xlsx", sheet = "Vietas")

TVietas <- TVietas[!TVietas$vieta=="Šlītere",] #Izņemt Šlīteres transektes
unique(TVietas$vieta)
TVietas <- TVietas[-101,] # izslēgtā transekte

# Kolonnu dzēšana
TVietas <- TVietas[ , -c(8:9)]

# Grupu balansēšana
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
  left_join(maks_ziedi, by = "trans_kods")



## Iztrūkstoša uzskaite -----
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
Dist_Dataset <- bind_rows(Dist_Dataset, kemeri_3)







#Y tabula visas uzvedibas kopā -------------
colnames(Dist_Dataset)

# Summējam katrā transektē veiktos novērojumus pa attāluma joslām
TNov_pa_trans_j <- data.frame(Dist_Dataset) %>%
  group_by(uzsk_ID, trans_kods, latviskais, josla) %>%
  summarise(Det=n())
TNov_pa_trans_j <- data.frame(TNov_pa_trans_j)

# Platais formāts (identifikācijas lietas, suga, novērojumu skaits pa joslām)
TNov_pa_trans_j_w <- reshape(TNov_pa_trans_j, 
                             idvar=c("uzsk_ID", "trans_kods", "latviskais"), 
                             timevar="josla", direction="wide")

# Sakārtot joslu kolonnas pareizā secībā
colnames(TNov_pa_trans_j_w)
TNov_pa_trans_j_w <- TNov_pa_trans_j_w %>% select(uzsk_ID, trans_kods, latviskais, 
                                                  Det.1, Det.2, Det.3, Det.4)

names(TNov_pa_trans_j_w)[4:7] <- c("J50", "J150", "J250", "Jtalak")
TNov_pa_trans_j_w[is.na(TNov_pa_trans_j_w)] <- 0
head(TNov_pa_trans_j_w)


# Izvēlēties sugu
unique(TNov_pa_trans_j_w$latviskais)

Tsuga <- "Kāpostu baltenis"

# Atlasām mūs interesējošo sugu novērojumus
Tsugasdati <- TNov_pa_trans_j_w[TNov_pa_trans_j_w$latviskais==Tsuga,]
head(Tsugasdati)
dim(Tsugasdati)


# Savienojam transektes, kur suga tika novērota, ar pārējām
Tpilnais <- merge(TUzskaites, Tsugasdati, by=c("trans_kods", "uzsk_ID"), 
                  all.x=TRUE, sort=TRUE)

Tpilnais[is.na(Tpilnais)] <- 0


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
taurini <- Tpilnais[,c(1,2,28:31)]
head(taurini)
TY <- reshape(taurini, v.names=c("J50", "J150", "J250", "Jtalak"), idvar="trans_kods", timevar="uzsk_ID", direction="wide")
head(TY)
TY <- subset(TY, select=c(-trans_kods))
dim(TY) # 119 transektes x 24 (4 joslas * 6 uzskaitēs)






# Modelēšana (visas uzvedības) ----------

#Modeļa iestatījumi
R = 119 #transekšu skaits. (Tikai Ģipka un Apšupe)
T <- 6  # 6 atkārtotas uzskaites
garums <- 100
TK=100 #	An integer value specifying the upper bound used in the integration.
Tdist.breaks <- c(0, 0.5, 1.5, 2.5, 7)
numDistClasses <- length(Tdist.breaks) - 1  # = 4

Tmixture="P"
TunitsOut="ha"
Toutput="density"

# Visām sugām
TVisiudfGDS<-unmarkedFrameGDS(y=TY, 
                              siteCovs=TVietas,
                              dist.breaks=Tdist.breaks,
                              numPrimary=T, 
                              yearlySiteCovs=TUzskaites, 
                              survey="line",
                              unitsIn="m",
                              tlength=rep(garums, R))

summary(TVisiudfGDS)
head(yearlySiteCovs(TVisiudfGDS),12)



## Nulles modelis ----
m0hal <- gdistsamp(~1, ~1, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                    K=TK, unitsOut=TunitsOut)
summary(m0hal)


backTransform(m0hal, type="lambda")
confint(backTransform(m0hal, type="lambda"))
backTransform(m0hal, type="phi")
backTransform(m0hal, type="det")

plot(function(x) gxhn(x, sigma = confint(backTransform(m0hal, type = "det"))[1,2]), 
     0, 4, col = gray(0.7),
     xlab = "Attālums (m)", ylab = "Detection probability",
     ylim = c(0, 1))
plot(function(x) gxhn(x, sigma=confint(backTransform(m0hal, type="det"))[1,1]), 0, 6, add=TRUE, col=gray(0.7))
plot(function(x) gxhn(x, sigma=backTransform(m0hal, type="det")@estimate), 0, 6, add=TRUE)



## Mēģinu iegūt aplesto indivīdu skaitu --------

Tsugasdati$kopa <- rowSums(Tsugasdati[, c("J50", "J150", "J250", "Jtalak")], na.rm = TRUE)



#Var aplēst sakitu katrai transektei atsevīšķi balstoties uz viedējo konstatēšanas
# iespēju starp transektem (neņem vērā individuālas atširības)


sigma_hat <- backTransform(m0hal, type = "det")@estimate # Izņemt det parametru - sigmu

midpoints <- (head(Tdist.breaks, -1) + tail(Tdist.breaks, -1)) / 2 #joslu robežas viduspunkti

p_joslas <- gxhn(midpoints, sigma_hat) # KOnst. varb katrai joslai (tā kā gxhn darbojas pēc attāluma)


joslas_platums <- diff(Tdist.breaks) # Svarot pēc joslas platības
p_avg <- sum(p_joslas * joslas_platums) / sum(joslas_platums)

# Sakita aplēses
Tsugasdati$aplestais_skaits <- Tsugasdati$kopa / p_avg

#pievienot vietu
Tsugasdati$vieta <- ifelse(grepl("^K", Tsugasdati$trans_kods), "Ķemeri", 
                           ifelse(grepl("^A", Tsugasdati$trans_kods), "Apšupe", "Ģipka"))


ggplot(Tsugasdati, aes(x = vieta, y = aplestais_skaits, fill = vieta, color = vieta)) +
  geom_boxplot(outlier.shape = NA, width = 0.4, alpha = 0.4) +    
  geom_jitter(width = 0.1, height = 0, alpha = 0.7, size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, colour = "black") +
  stat_n_text() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(Tsugasdati$aplestais_skaits) + 2, by = 1)) +
  theme_minimal(base_size = 14) +
  labs(x = "Pētījuma vieta", y = "Pollarda pārpilnības indekss", title = izveleta_suga)

 








##  Bootstrap prognoze --max()##  Bootstrap prognoze -----------
# Aplēsts skaits visai pētījuma teitorijai

# 1. Funkcija, kas trenē modeli un aprēķina vērtību (piemēram, abundance)
bootstrap_fun <- function(data) {
  # Izveido jaunu datu apakškopu ar nejaušu atkārtotu izvēli (resampling)
  i <- sample(1:nrow(data@y), replace = TRUE)
  umf_resampled <- data[i, ]
  
  # Pārtrenē modeli
  m_boot <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, 
                      data = umf_resampled, keyfun = "halfnorm", output = "abund")
  
  # Atgriež, piemēram, vidējo paredzēto abundanci
  return(sum(predict(m_boot, type = "lambda")$Predicted))
}

# 2. Izpildi vairākas bootstrap replikācijas
set.seed(123)
boot_results <- replicate(100, bootstrap_fun(TVisiudfGDS))  # 100 bootstrap replikācijas

# 3. Aprēķini ticamības intervālus
quantile(boot_results, probs = c(0.025, 0.975))  # 95% CI




##  Bootstrap prognoze pa vietam ---------------------
bootstrap_fun <- function(data) {
  i <- sample(1:nrow(data@y), replace = TRUE)
  umf_resampled <- data[i, ]
  
  m_boot <- gdistsamp(lambdaformula = ~vieta, phiformula = ~1, pformula = ~1,
                      data = umf_resampled, keyfun = "halfnorm", output = "abund")
  
  pred <- predict(m_boot, type = "lambda")$Predicted
  vietas <- data@siteCovs$vieta[i]  # atkārtotās vietas
  
  df <- data.frame(vieta = vietas, pred = pred)
  df_sum <- aggregate(pred ~ vieta, data = df, sum)
  
  return(df_sum)
}

set.seed(123)
boot_list <- replicate(100, bootstrap_fun(TVisiudfGDS), simplify = FALSE)

# Apvieno visus rezultātus vienā datu tabulā
boot_df <- bind_rows(boot_list, .id = "replicate")

# Aprēķini 95% intervālu katrai vietai
boot_df %>%
  group_by(vieta) %>%
  summarise(
    lower = quantile(pred, 0.025),
    upper = quantile(pred, 0.975),
    mean  = mean(pred)
  )







## Temperatūras modelis -----
m_temp_hal <- gdistsamp(~1, ~temp_vid -1, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                        K=TK, unitsOut=TunitsOut)
summary(m_temp_hal) # Pieejamība nav būtiska






## Veja modelis -----
m_vejs_hal <- gdistsamp(~1, ~vej_atr_vid -1, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                            K=TK, unitsOut=TunitsOut)
summary(m_vejs_hal) # Pieejamība ir būtiskā







## pļaušanas modelis ----
m_plausana_hal <- gdistsamp(~1, ~plausana -1, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                   K=TK, unitsOut=TunitsOut)
summary(m_plausana_hal) # Pieejamība nav būtiskā



# Lielais modelis
m_big_hal <- gdistsamp(~(vieta-1) +(kust_int-1) + maks_stavu_sk + maks_ziedu_sk, 
                       ~Jday + ziedi_sum + temp_vid + apg_vid + (traucejumi-1), 
                       ~veg_augst_vid + stavu_sk + temp_vid, 
                       TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                       K=TK, unitsOut=TunitsOut)
summary(m_big_hal) # Pieejamība ir būtiskā


