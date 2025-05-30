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
Dist_Dataset <- Dist_Dataset %>%  #IESPĒJAMS ŠO VAR AIZVIEOT AR CIKLU, KAD LIEKAIS BUS IZŅEMTS NO IEVĀDES DATIEM
  select(-laiks_sak, -laiks_beig, -laiks_vid, -trans_ilg, -temp_sak, -temp_beig,
         -vej_atr_sak, -vej_atr_beig, -rel_mitr_sak, -rel_mitr_beig, -makon_sak,
         -makon_beig, -apg_sak, -apg_beig, -veg_augst_sak, -veg_augst_beig,
         -rl_ziedi_sak, -rl_ziedi_beig, -rl_augi_sak, -rl_augi_beig, -z_ziedi_sak, 
         -z_ziedi_beig, -z_augi_sak, -z_augi_beig, -v_ziedi_sak, -v_ziedi_beig,
         -v_augi_sak, -v_augi_beig, -a_ziedi_sak, -a_ziedi_beig, -a_augi_sak,
         -a_augi_beig, -mitr_apst, -izmainas, -spares, -taurini, -kods, -dzimta,
         -apaksdzimta, -gints, -vid_sparnu_pletums, -komentars, -kom_traucejumi,
         -kom_bojajumi, -laiks_min_sak, -laiks_min_beig)
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
ggplot(Dist_Dataset[Dist_Dataset$zinatniskais == "Pieris brassicae",], aes(josla)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba, nrow = 2)

ggplot(Dist_Dataset[Dist_Dataset$zinatniskais == "Pieris brassicae",], aes(josla, fill = vieta)) + 
  geom_histogram(stat="count") +
  facet_wrap(~uzvediba*vieta, nrow = 2)
# Ķemeros izteikti atšķirīgs sadalījums. Bet es zinu, ka tas tā ir tikai visiem tauriņiem kopā
# jo lielas sugas var konstatēt no lielāka attalumā.






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
summary(TY)




# Tabula "Uzskaites"  -----------------------
# Veidojam atsevišķu tabulu ar visām transektēm un uzskaites reizēm, lai tā kalpotu kā skelets
transektes <- sort(unique(Dist_Dataset$trans_kods))
trans_uzsk <- data.frame(expand.grid(trans_kods = transektes, uzsk_ID = 1:6))
head(trans_uzsk) 
tail(trans_uzsk)

# No Dist_Dataset atlasām unikālus transektu aprakstus
data.frame(Numurs = seq_along(Dist_Dataset), Kolonna = names(Dist_Dataset))
TUzskaites <- Dist_Dataset[,c(1:8, 14: 32)]
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
  left_join(maks_ziedi, by = "trans_kods") %>% 
  arrange(trans_kods)



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


## Nulles modelis half norm ----
m0haz <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture=Tmixture, 
                   K=TK, unitsOut=TunitsOut)
summary(m0haz)
m0hazNB <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="hazard", output=Toutput, mixture="NB", 
                   K=TK, unitsOut=TunitsOut)
summary(m0hazNB)



m0hal <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                   K=TK, unitsOut=TunitsOut)
summary(m0hal)
m0halNB <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="halfnorm", output=Toutput, mixture="NB", 
                     K=TK, unitsOut=TunitsOut)
summary(m0halNB)


backTransform(m0hazNB, type="lambda")
confint(backTransform(m0hazNB, type="lambda"))
backTransform(m0hazNB, type="phi")
backTransform(m0hazNB, type="det")

plot(function(x) gxhn(x, sigma = confint(backTransform(m0hazNB, type = "det"))[1,2]), 
     0, 4, col = gray(0.7),
     xlab = "Attālums (m)", ylab = "Detection probability",
     ylim = c(0, 1))
plot(function(x) gxhn(x, sigma=confint(backTransform(m0hazNB, type="det"))[1,1]), 0, 4, add=TRUE, col=gray(0.7))
plot(function(x) gxhn(x, sigma=backTransform(m0hazNB, type="det")@estimate), 0, 4, add=TRUE)






## Mēģinu iegūt aplesto indivīdu skaitu --------

skaiti=unmarked::predict(m0hal,type="lambda") #vidējais skaits


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
Noverojumi$kopa <- rowSums(Noverojumi[, c("J50", "J150", "J250", "Jtalak")], na.rm = TRUE)

Noverojumi$aplestais_skaits <- Noverojumi$kopa / p_avg


Noverojumi <- Noverojumi %>%
  group_by(trans_kods) %>%
  summarise(
    aplestais_skaits= round(sum(aplestais_skaits)),
    .groups = "drop"
  )



#pievienot vietu 
Noverojumi$vieta <- ifelse(grepl("^K", Noverojumi$trans_kods), "Ķemeri", 
                           ifelse(grepl("^A", Noverojumi$trans_kods), "Apšupe", "Ģipka"))




ggplot(Noverojumi, aes(x = vieta, y = aplestais_skaits, fill = vieta, color = vieta)) +
  geom_boxplot(outlier.shape = NA, width = 0.4, alpha = 0.4) +    
  geom_jitter(width = 0.1, height = 0, alpha = 0.7, size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, colour = "black") +
  EnvStats::stat_n_text() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(Noverojumi$aplestais_skaits) + 2, by = 1)) +
  theme_minimal(base_size = 14) +
  labs(x = "Pētījuma vieta", y = "Pollarda pārpilnības indekss", title = Tsuga)

 
# Cik es saprotu, tad ar šo metodi neatšķiras novērojumu attiecība, salīdzinot
#ar origīnālu, jo visi tiek reizināti ar vienu skaitli. Bet šim var būt jēga 
# salīdzinot pa gadiem, jo starp tiem var mainities viedēja konstaatēšanas varbūtība





# Modelis ar vietam -----
summary(TudfGDS)
m_vieta_hal <- gdistsamp(~vieta, # lambda = abundance
                       ~1, # phi = availability
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_vieta_hal) # Pieejamība nav būtiska
skaiti=unmarked::predict(m_vieta_hal,type="lambda") 
skaiti


## DoY modelis -----
summary(TudfGDS)
m_doy_hal <- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(Jday)^2)+scale(temp_vid)+I(scale(temp_vid)^2)+scale(vej_atr_vid)+traucejumi, # phi = availability
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_doy_hal) # Pieejamība nav būtiska


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


m_doy1_hal <- gdistsamp(~1, 
                        ~scale(Jday)+I(scale(Jday)^2)+vieta, 
                        ~1, 
                        TudfGDS, keyfun ="hazard", output=Toutput, mixture=Tmixture, 
                       K=TK, unitsOut=TunitsOut)
summary(m_doy1_hal) # Pieejamība nav būtiska
newdata=expand.grid(vieta=c("Apšupe","Ģipka","Ķemeri"),
                    Jday=seq(100,250,10))
a1=as.data.frame(predict(m_doy1_hal,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1,aes(Jday,Predicted,ymin=lower,ymax=upper))+
  geom_ribbon(alpha=0.5)+
  geom_line()+
  facet_wrap(~vieta)








## Tikai dienaslaiks -----
summary(TudfGDS)
m_doy_hal <- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(laiks_min_vid)^2)+scale(laiks_min_vid)+scale(Jday),
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_doy_hal) # Pieejamība nav būtiska


sh <- predict(m_doy_hal,type="lambda")/20 # skaits uz hektāru dalīt uz 20 = skaits uz transektes platību


newdata=expand.grid(Jday=seq(160,250,10),
                    laiks_min_vid=seq(540,1080, 120)
                    )
a=as.data.frame(predict(m_doy_hal,type="phi",newdata))
kraa=cbind(newdata,a)
ggplot(kraa, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5, colour = NA) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  facet_wrap(~laiks_min_vid)


m_doy1_hal <- gdistsamp(~1, 
                        ~scale(Jday)+I(scale(Jday)^2)+vieta, 
                        ~1, 
                        TudfGDS, keyfun ="hazard", output=Toutput, mixture=Tmixture, 
                        K=TK, unitsOut=TunitsOut)
summary(m_doy1_hal) # Pieejamība nav būtiska
newdata=expand.grid(vieta=c("Apšupe","Ģipka","Ķemeri"),
                    Jday=seq(100,250,10))
a1=as.data.frame(predict(m_doy1_hal,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1,aes(Jday,Predicted,ymin=lower,ymax=upper))+
  geom_ribbon(alpha=0.5)+
  geom_line()+
  facet_wrap(~vieta)

## Tikai temperatūra -----


summary(TudfGDS)
m_doy_hal <- gdistsamp(~1, # lambda = abundance
                       ~1+I(scale(temp_vid)^2)+scale(temp_vid)+scale(Jday),
                       ~1, # pi = detection
                       TudfGDS, 
                       keyfun ="hazard", 
                       output=Toutput, 
                       mixture="NB",
                       K=TK, 
                       unitsOut=TunitsOut)
summary(m_doy_hal) # Pieejamība nav būtiska


sh <- predict(m_doy_hal,type="lambda")/20 # skaits uz hektāru dalīt uz 20 = skaits uz transektes platību


newdata=expand.grid(Jday=seq(160,250,10),
                    temp_vid=seq(13,35,5)
)
a=as.data.frame(predict(m_doy_hal,type="phi",newdata))
kraa=cbind(newdata,a)
ggplot(kraa, aes(Jday, Predicted, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.5, colour = NA) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  facet_wrap(~temp_vid)


m_doy1_hal <- gdistsamp(~1, 
                        ~scale(Jday)+I(scale(Jday)^2)+vieta, 
                        ~1, 
                        TudfGDS, keyfun ="hazard", output=Toutput, mixture=Tmixture, 
                        K=TK, unitsOut=TunitsOut)
summary(m_doy1_hal) # Pieejamība nav būtiska
newdata=expand.grid(vieta=c("Apšupe","Ģipka","Ķemeri"),
                    Jday=seq(100,250,10))
a1=as.data.frame(predict(m_doy1_hal,type="phi",newdata))
kraa1=cbind(newdata,a1)
ggplot(kraa1,aes(Jday,Predicted,ymin=lower,ymax=upper))+
  geom_ribbon(alpha=0.5)+
  geom_line()+
  facet_wrap(~vieta)

