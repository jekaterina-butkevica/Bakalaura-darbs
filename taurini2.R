# Pakotnes ----
if(!require(readxl)) install.packages("readxl")
if(!require(lubridate)) install.packages("lubridate")
if(!require(unmarked)) install.packages("unmarked")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(forcats)) install.packages("forcats")
if(!require(vegan)) install.packages("vegan")
if(!require(tidyr)) install.packages("tidyr")



# Dati --------
TDataset <- read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")
summary(TDataset)
TDataset <- TDataset[!is.na(TDataset$trans_kods),]  #Noņem tukšās rindas
summary(TDataset)


#Vidējo vērtību aprēķins
TDataset$ziedi_sum_sak <- (TDataset$rl_ziedi_sak + TDataset$zl_ziedi_sak + TDataset$vl_ziedi_sak + TDataset$al_ziedi_sak)
TDataset$ziedi_sum_beig <- (TDataset$rl_ziedi_beig + TDataset$zl_ziedi_beig + TDataset$vl_ziedi_beig + TDataset$al_ziedi_beig)
TDataset$augi_sum_sak <- (TDataset$rl_augi_sak + TDataset$zl_augi_sak + TDataset$vl_augi_sak + TDataset$al_augi_sak)
TDataset$augi_sum_beig <- (TDataset$rl_augi_beig + TDataset$zl_augi_beig + TDataset$vl_augi_beig + TDataset$al_augi_beig)


sakums_colnames <- names(TDataset)[grepl("_sak$", names(TDataset))]

for (sak_col in sakums_colnames) {
  base_name <- sub("_sak$", "", sak_col)
  beig_col <- paste0(base_name, "_beig")
  
  if (beig_col %in% names(TDataset)) {
    avg_col <- paste0(base_name, "_vid")
    TDataset[[avg_col]] <- rowMeans(cbind(TDataset[[sak_col]], TDataset[[beig_col]]), na.rm = TRUE)
  }
}
summary(TDataset)

# Datuma formatēšana
TDataset$datums <- as.Date(TDataset$datums, format= "%d.%m.%Y") 
head(TDataset$datums)
TDataset$Jday <- yday(TDataset$datums)
summary(TDataset)


#Vajadzīgo vietu atlase
unique(TDataset$vieta)
TDataset <- TDataset[!(TDataset$vieta == "Ķemeri"),] #Noņemt Ķemeru uzskaites
summary(TDataset)







## Datu pārskats ---------

# Novērojumu skaits dažādās distancēs
hist(TDataset$josla) #Viss ir labi

# Katras sugas novērojumu skaits
Species_obs = data.frame(TDataset[!is.na(TDataset$latviskais),]) %>%
  group_by(latviskais) %>%
  summarise(observations=n())
Species_obs <- Species_obs[order(-Species_obs$observations),]
head(Species_obs)

ggplot(Species_obs,aes(x=fct_reorder(latviskais, -observations), y=observations))+
  geom_col()+
  geom_hline(yintercept=40) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))


#Katrai sugai novērojumu skaits pa vietām
Species_obs_vieta <- TDataset %>%
  filter(!is.na(latviskais), !is.na(vieta)) %>%
  group_by(latviskais, vieta) %>%
  summarise(observations = n(), .groups = "drop")

ggplot(Species_obs_vieta, aes(x = fct_reorder(latviskais, -observations, .fun = sum), 
                        y = observations, 
                        fill = vieta)) +
  geom_col() +
  geom_hline(yintercept = 40) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Suga", y = "Novērojumu skaits", fill = "Vieta")





# Tabulas "Vietas" sagatavošana ----
TVietas <- read_excel("uzskaisu_dati.xlsx", sheet = "Vietas")
unique(TVietas$vieta)
TVietas <- TVietas[!TVietas$vieta=="Ķemeri",] #Izņemt Ķeneru transektes







# Kumulatīva likne ==============================================================
#kumulativa_likne=specaccum(bentoss_izejas[,3:55], method="exact", permutations=999, continued=TRUE, drop=FALSE, gamma="jack1", subset=TRUE, ci.type=polygon )















# Unmarked gdistsamp() ===========================================================


##Tabulas "Uzskaites" sagatavošana (katras uzskaites reizes specifiskā – mainīgā – informācija) -----

# Veidojam atsevišķu tabulu ar visām transektēm un uzskaites reizēm, lai tā kalpotu kā skelets
transektes <- sort(unique(TDataset$trans_kods))
trans_uzsk <- data.frame(expand.grid(trans_kods = transektes, uzsk_ID = 1:6))
head(trans_uzsk) 
tail(trans_uzsk)

# No TDataset atlasām unikālus transektu aprakstus
TUzskaites <- TDataset[,c(1:3,4:78)]
TUzskaites <- TUzskaites %>%
  distinct(trans_kods, uzsk_ID, .keep_all = TRUE)
table(TUzskaites$trans_kods) # viur jābūt 6





# Izkliedes attēli ==============================================================


dati <-  read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")

novērojumi <- dati %>%
  filter(!is.na(latviskais)) %>%
  group_by(uzsk_ID, trans_kods) %>%
  summarise(individu_skaits = n(), .groups = "drop")

visas_uzskaites <- dati %>%
  distinct(trans_kods, uzsk_ID)

visas_uzskaites <- visas_uzskaites %>%
  left_join(novērojumi, by = c("uzsk_ID", "trans_kods")) %>%
  mutate(individu_skaits = replace_na(individu_skaits, 0))


apvienotie_dati <- visas_uzskaites %>%
  left_join(TUzskaites[,c(1:3,38:44,62:78)], by = c("uzsk_ID", "trans_kods"))

apvienotie_dati <- apvienotie_dati[!is.na(apvienotie_dati$trans_kods),]







ggplot(apvienotie_dati, aes(x = temp_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()



ggplot(apvienotie_dati, aes(x = vej_atr_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = apg_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = veg_augst_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()

ggplot(apvienotie_dati, aes(x = augi_sum_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = ziedi_sum_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = vl_ziedi_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()

# Izkliedes attēli neņemot vēra tukšas transektes

ggplot(apvienotie_dati[apvienotie_dati$individu_skaits > 0,], aes(x = temp_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()

# Man liekas, ka tukšas transektes trauce novērot ietekmi. 
#Tauriņi bieži grupējas un man liekas, ir jāskatās tikai tos, kur bija 

ggplot(apvienotie_dati[apvienotie_dati$vieta == "Ģipka" & 
                         apvienotie_dati$kust_int != "Nav" & 
                         apvienotie_dati$individu_skaits > 0,], 
       aes(x = temp_vid, y = individu_skaits, color = kust_int)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()

ggplot(apvienotie_dati[apvienotie_dati$vieta == "Apšupe" &
                         apvienotie_dati$kust_int != "Nav" & 
                         apvienotie_dati$kust_int != "Vidējā" & 
                         apvienotie_dati$individu_skaits > 0,], 
       aes(x = temp_vid, y = individu_skaits, color = kust_int)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = vej_atr_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Indivīdu skaits", title = "Izkliedes attēls") +
  theme_minimal()


ggplot(apvienotie_dati, aes(x = apg_vid, y = individu_skaits, color = vieta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X ass", y = "Y ass", title = "Izkliedes attēls") +
  theme_minimal()




# Histogrammas ==============================================================


ggplot(TDataset[TDataset$taurini == "Ir" & !is.na(TDataset$uzvediba),], aes(x = uzvediba, fill = vieta)) +  # X ass ir uzvedības kategorija, krāsa vietai
  geom_bar(stat = "count", position = "dodge") +  # Stabiņi blakus katrai vietai
  facet_wrap(vieta~ datums) +  # Facetēšana pēc uzskaites ID
  labs(x = "Uzvedības kategorija", y = "Skaits", title = "Uzvedības sadalījums pa uzskaitēm un vietām") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Apgriež X ass tekstu vertikāli






## Tauriņiem kopā: Y tabulas sagatavošana -----
colnames(TDataset)
TVisiNov_pa_trans_j <- data.frame(TDataset) %>%
  group_by(Jday, uzsk_ID, trans_kods, taurini, josla) %>%
  summarise(Det=n())
TVisiNov_pa_trans_j <- data.frame(TVisiNov_pa_trans_j)
TVisiNov_pa_trans_j <- TVisiNov_pa_trans_j[!is.na(TVisiNov_pa_trans_j$josla),] # Noņemt rindas, kurās nav norādīta josla


# Platais formāts (identifikācijas lietas, suga, novērojumu skaits pa joslām)
TVisiNov_pa_trans_j_w <- reshape(TVisiNov_pa_trans_j, 
                             idvar=c("Jday", "uzsk_ID", "trans_kods", "taurini"), 
                             timevar="josla", direction="wide")

colnames(TVisiNov_pa_trans_j_w)
TVisiNov_pa_trans_j_w <- TVisiNov_pa_trans_j_w %>% select(Jday, uzsk_ID, trans_kods, taurini, 
                                                  Det.1, Det.2, Det.3, Det.4)

names(TVisiNov_pa_trans_j_w)[5:8] <- c("J50", "J150", "J250", "Jtalak")
TVisiNov_pa_trans_j_w[is.na(TVisiNov_pa_trans_j_w)] <- 0
head(TVisiNov_pa_trans_j_w)

TVisiNov_pa_trans_j_w

# Savienojam visu transekšu datus ar tiem, kuros tika novēroti tauriņi
TVisipilnais <- merge(TUzskaites, TVisiNov_pa_trans_j_w, by=c("trans_kods", "uzsk_ID"), 
                  all.x=TRUE, sort=TRUE)

# Datuma formātešana 
TVisipilnais$datums <- as.Date(TVisipilnais$datums, format= "%d.%m.%Y")
TVisipilnais$Jday <- yday(TVisipilnais$datums)

#Pārbaudīt, vai uzskaišu reižu skaits ir vienāds.
ggplot(TVisipilnais, aes(trans_kods,)) + geom_histogram(,stat="count")


# Tauriņu tabula satur novērojumu informāciju par dienastauriņu novērojumiem katrā punktā, sadalot to pa attālumu joslām
visitaurini <- TVisipilnais[,c(1,2,81:84)]
head(visitaurini)
TVisiY <- reshape(visitaurini, v.names=c("J50", "J150", "J250", "Jtalak"), idvar="trans_kods", timevar="uzsk_ID", direction="wide")
head(TVisiY)
TVisiY <- subset(TVisiY, select=c(-trans_kods))
dim(TVisiY) # 80 transektes x 24 (4 joslas * 6 uzskaitēs)
TVisiY[is.na(TVisiY)] <- 0







## Modelēšana: visi tauriņi kopā ----

#Modeļa iestatījumi
R = 80 #transekšu skaits. (Tikai Ģipka un Apšupe)
numPrimary <- 6  # 6 atkārtotas uzskaites
garums <- 100
TK=100 #	An integer value specifying the upper bound used in the integration.
Tdist.breaks <- c(0, 0.5, 1.5, 2.5, 10)
numDistClasses <- length(Tdist.breaks) - 1  # = 4

Tmixture="P"
TunitsOut="ha"
Toutput="density"

# Visām sugām
TVisiudfGDS<-unmarkedFrameGDS(y=TVisiY, 
                          siteCovs=TVietas,
                          dist.breaks=Tdist.breaks,
                          numPrimary=numPrimary, 
                          yearlySiteCovs=TUzskaites, 
                          survey="line",
                          unitsIn="m",
                          tlength=rep(garums, R))

summary(TVisiudfGDS)
head(yearlySiteCovs(TVisiudfGDS),12)




### Nulles modelis ----
tvisi0 <- gdistsamp(~1, ~1, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                K=TK, unitsOut=TunitsOut)
summary(tvisi0)

# Effective strip half-width
(sigma <- exp(0.158)) #no summary
(eshw <- integrate(gxhn, 0, 10, sigma=sigma)$value) # Sigma tiek noteikta no modeļa, un tā ir būtiska detekcijas funkcijas daļa.
# Detection probability
eshw / 10 # 10 is strip-width 

# No visiem indivīdiem, kas reāli atrodas 10 metru platumā no transekta līnijas, tu ar savām uzskaites metodēm efektīvi uztver tikai apmēram 14.7%.

backTransform(tvisi0, type="lambda")
confint(backTransform(tvisi0, type="lambda"))
backTransform(tvisi0, type="phi")
backTransform(tvisi0, type="det")

plot(function(x) gxhn(x, sigma = confint(backTransform(tvisi0, type = "det"))[1,2]), 
     0, 4, col = gray(0.7),
     xlab = "distance (m)", ylab = "Detection probability",
     ylim = c(0, 1))
plot(function(x) gxhn(x, sigma=confint(backTransform(tvisi0, type="det"))[1,1]), 0, 6, add=TRUE, col=gray(0.7))
plot(function(x) gxhn(x, sigma=backTransform(tvisi0, type="det")@estimate), 0, 6, add=TRUE)



# Empirical Bayes estimates of abundance at each site (lpp. 43 https://cran.r-project.org/web/packages/unmarked/unmarked.pdf )
par(mai=c(1,1,0,0))
re <- ranef(tvisi0)
plot(re, layout=c(10,8), xlim=c(-1, 20))








## Temperatūras un vietas modelis ---
tvisi3 <- gdistsamp(~1, ~temp_vid*vieta, ~1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                K=TK, unitsOut=TunitsOut)
summary(tvisi3)


newdata<-expand.grid(temp_vid = seq(13.7, 28.2, by = 1), 
                     vieta = c("Ģipka", "Apšupe"))
(E.phi<-predict(tvisi3, type='phi', newdata=newdata, appendData=TRUE))



ggplot(E.phi, aes(x = temp_vid, y = Predicted, color = as.factor(vieta))) +
  geom_ribbon(aes(ymin = Predicted - SE, ymax = Predicted + SE, fill = as.factor(vieta)), alpha = 0.3) +
  geom_line(size = 1) +
  theme_classic() +
  labs(title = expression("Prognozētās dienastauriņu pieejambas \nizmaiņas temperatūras un vietas ietekmē"),
       x = "Temperatūra (°C)", 
       y = "Piejamība uzskaitei",
       color = "Vieta",
       fill = "Vieta") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "right",
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))





## Vietas modelis ---
tvisi3 <- gdistsamp(~1, ~1, ~vieta-1, TVisiudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                    K=TK, unitsOut=TunitsOut)
summary(tvisi3)



##Pa sugām Y tabulas sagatavošana ---------
colnames(TDataset)

# Summējam katrā transektē veiktos novērojumus pa attāluma joslām
TNov_pa_trans_j <- data.frame(TDataset) %>%
  group_by(Jday, uzsk_ID, trans_kods, latviskais, josla) %>%
  summarise(Det=n())
TNov_pa_trans_j <- data.frame(TNov_pa_trans_j)
TNov_pa_trans_j <- TNov_pa_trans_j[!is.na(TNov_pa_trans_j$josla),]  # Noņemt rindas, kurās nav norādīta josla


# Platais formāts (identifikācijas lietas, suga, novērojumu skaits pa joslām)
TNov_pa_trans_j_w <- reshape(TNov_pa_trans_j, 
                             idvar=c("Jday", "uzsk_ID", "trans_kods", "latviskais"), 
                             timevar="josla", direction="wide")

# Sakārtot joslu kolonnas pareizā secībā
colnames(TNov_pa_trans_j_w)
TNov_pa_trans_j_w <- TNov_pa_trans_j_w %>% select(Jday, uzsk_ID, trans_kods, latviskais, 
                                                  Det.1, Det.2, Det.3, Det.4)

names(TNov_pa_trans_j_w)[5:8] <- c("J50", "J150", "J250", "Jtalak")
TNov_pa_trans_j_w[is.na(TNov_pa_trans_j_w)] <- 0
head(TNov_pa_trans_j_w)


##### Izvēlamies konkrētu sugu
print(Species_obs, n = nrow(Species_obs)) #Nokopēt vajadzīgo vērtību no saraksta

Tsuga <- "Rāceņu baltenis" 


# Atlasām mūs interesējošo sugu novērojumus
Tsugasdati <- TNov_pa_trans_j_w[TNov_pa_trans_j_w$latviskais==Tsuga,]
head(Tsugasdati)
dim(Tsugasdati)


# Savienojam transektes, kur suga tika novērota, ar pārējām
Tpilnais <- merge(TUzskaites, Tsugasdati, by=c("trans_kods", "uzsk_ID"), 
                  all.x=TRUE, sort=TRUE)

# Datuma formatēšana 
Tpilnais$datums <- as.Date(Tpilnais$datums, format= "%d.%m.%y")
Tpilnais$Jday <- yday(Tpilnais$datums)

#Pārbaudīt, vai uzskaišu reižu skaits ir vienāds.
ggplot(Tpilnais, aes(trans_kods,)) + geom_histogram(,stat="count")


# Tauriņu tabula satur izvēlētās sugas novērojumu datus katrā punktā, sadalītus pa attāluma joslām
taurini <- Tpilnais[,c(1,2,81:84)]
head(taurini)
TY <- reshape(taurini, v.names=c("J50", "J150", "J250", "Jtalak"), idvar="trans_kods", timevar="uzsk_ID", direction="wide")
head(TY)
TY <- subset(TY, select=c(-trans_kods))
dim(TY) # 80 transektes x 24 (4 joslas * 6 uzskaitēs)
TY[is.na(TY)] <- 0



## Modelēšana izvēlētajai sugai ----


#Modeļa iestatījumi
R = 80 #transekšu skaits (Tikai Ģipka un Apšupe)
numPrimary <- 6  # 6 atkārtotas uzskaites
garums <- 100
TK=100
Tdist.breaks <- c(0, 0.5, 1.5, 2.5, 10)
numDistClasses <- length(Tdist.breaks) - 1  # = 4

Tmixture="P"
TunitsOut="ha"
Toutput="density"

#Izvēletai sugai
TudfGDS<-unmarkedFrameGDS(y=TY, 
                          siteCovs=TVietas,
                          dist.breaks=Tdist.breaks,
                          numPrimary=numPrimary, 
                          yearlySiteCovs=TUzskaites, 
                          survey="line",
                          unitsIn="m",
                          tlength=rep(garums, R))

summary(TudfGDS)
head(yearlySiteCovs(TudfGDS),12)





### Nulles modelis ----
t0 <- gdistsamp(~1, ~1, ~1, TudfGDS, keyfun ="halfnorm", output=Toutput, mixture=Tmixture, 
                K=TK, unitsOut=TunitsOut)
summary(t0)

backTransform(t0, type="lambda")
confint(backTransform(t0, type="lambda"))
backTransform(t0, type="phi")
backTransform(t0, type="det")

plot(function(x) gxhn(x, sigma=confint(backTransform(t0, type="det"))[1,2]), 0, 10, col=gray(0.7), xlab="distance (m)", ylab="Detection probability")
plot(function(x) gxhn(x, sigma=confint(backTransform(t0, type="det"))[1,1]), 0, 10, add=TRUE, col=gray(0.7))
plot(function(x) gxhn(x, sigma=backTransform(t0, type="det")@estimate), 0, 10, add=TRUE)










# Distance ====================================================================

# Ainārs rekomendēja izmēģināt, bet man liekas, kā labāk paturēties pie 
#unmarked, jo man ir gan nenoteiktas sugas, gan uzskaites caur sezonu,
# distance liekas vairāk visparīga.














