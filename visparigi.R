# Pakotnes -----
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")




# Dati --------
TDataset <- read_excel("uzskaisu_dati.xlsx", sheet = "Noverojumi")
summary(TDataset)
TDataset <- TDataset[!is.na(TDataset$uzsk_ID),]  #Noņem tukšās rindas
dim(TDataset)

