# Materiāli bakalaura darba "Atkārtoto uzskaišu nozīme dienas tauriņu (Papilionoidea) imago monitoringā: piemērs mežmalās veiktām uzskaitēm" reproducēšanai

Šajā repozitorijā atrodami komandrindas un ievaddati, kas nepieciešami mana bakalaura darba analīžu procesa un rezultātu reproducēšanai.

## Repozitorija saturs
### [Ievaddati](./Dati/)
Satur petījuma analīzētos datus par dienastauriņiem novērojumiem: **`originalie_dati.xlsx`**

Datu fails satur sēkojošas lapas:
- **Noverojumi** - informācija TY tabulai  - visi pētījuma laikā veiktie novērojumi
- **Uzskaites** - informācija par uzskaišu skaitu un apsēkoto stransektu skaitu
- **Vietas** - nemainīga vietu informācija TVietas tabulai - identifikātori, transekšu koordinātes, kustības intensitāte
- **Refernce** - references lapa, lapas "Noverojumi" automātiskai aizpildei
- **Lauku_skaidrojumi** - Kolonnu nosaukumu skaidrojumi



### [Pētījuma dizains](.//Dizains)
Satur attēlus, kas informē par galvēnājiem pētījuma dizaina aspektiem: uzskaites veikšanas metodes shēma un transektes un parauglaukumu izvietojuma shēma.



### [R komandrindas](./R_komandrindas/)

- **`0_visparigi.R`**
Komandrindas rezultātu ievada sadaļas izveidei: reģistrēto sugu skaits, to sadalījums pa dzimtām, novērojumu sadalījuma attēla izveide.
  
- **`1_Pollarda_metode.R`**
Komandrindas sugu izvēlēi nepiciešamo attēlu un rezultātu sadaļas "3.1. Oriģinālā Pollarda metode" izveidei: fenoloģiju līkņu uzbūve visām sugām, Pollarda indeksu salīdzinājuma attēla izveide visām sugām, Kāpostu balteņa fenoloģijas attēla izveide, Kāpostu balteņa indeksu salīdzinājuma attēla izveide, Kāpostu balteņa Pollarda indeksu salīdzinājums starp vietām.
  
- **`3_distances_analize.R`**
Komandrindas distances analīzes veikšanai ar hierarhiskiem distances modeļiem: datu apstrāde, modeļu izveide un izvēle, pieejamības vērtību prognozēšana un ar to saistīto grupu salīdzinājuma veikšana, kā arī attēlu izveide.

- **`kartes.R`**
Komandrindas bakalaura darbā un aizstāvēšanas prezentācijā iekļauto Latvijas konturkartes izveidei pētījuma dizaina demonstrācijai.



### [Pollarda metode](./Pollard/)
Mape **[Sugu fenoloģijas līknes](./Pollards/fenologija/)**
Satur uz novērojumiem balstītas fenoloģijas līknes, visām pētījumā novērotām sugam, kam ir vairāk par 2 novērojumiem. Balstās uz novērojumeim no visām joslām.

Mape **[Pollarda indeksa salīdzinājums](./Pollards/indeksi/)**
Satur katrai pētījumā novērotai sugai veikto transekšu Pollarda indeksu vizuālo salīdzinājumu starp pētījuma vietām. Balstās uz novērojumiem Pollarda telpas ietvaros (1. līdz 3. josla).



### [Rezultāti](.//Rezultati)
Šī mape satur attēlus ar galvēnajiem pētījuma rezultātiem, kā arī attēlus, kas analīzēti diskusijas sadaļā.

- `sugu_noverojumi.png` - novērojumu skaita sadalijums starp pētījuma registrētām sugām
- `Pollard_index_Pieris_brassicae.png` - Kāpostu balteņa Polarda indeksa vērtību salīdzināums starp vietam
- `distances_funkcija.png` - kāpostu balteņa pieejamības izmaiņas ar attālumu
- `aplestais_noverotais_salidzinajums.png` - kāpostu balteņa  un aplēsta kopskaita vizuāls salīzinājums
- `pieejamiba_sezona.png` - prognoze kapostu balteņa pieejamības izmaiņām sezonas laikā
- `pollard_logs_temp_apg.png` - kāpostu balteņa pieejamības prognoze dažādām apgaismojuma un temperatūŗas kombinācijām
- `pollard_vejs.png` - kāpostu balteņa pieejamības prognoze dažādām vēja un temepratūras kombinācijām
- `Pieris_brassicase_fenologija.png` - uz novērojumiem balstīta kāpostu balteņa ffenoloģijas līkne
- `Pierisspp_pierisbrass_barplot.png` - novērojumu sadallījums starp joslām: salīdzinājums starp _Pieris_ ģints nenoteiktiem indivīdiem un kāpostu baltenim.



### [Kartes](.//Kartes)
Satur bakalaura darbā un aizstavēšanas prezentācijā izmantotās Latvijas konturkartes.


