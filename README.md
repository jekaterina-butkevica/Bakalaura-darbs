# Materiāli bakalaura darba "Atkārtoto uzskaišu nozīme dienas tauriņu (Papilionoidea) imago monitoringā: piemērs mežmalās veiktām uzskaitēm" reproducēšanai

Šajā repozitorijā atrodami komandrindas un ievaddati, kas nepieciešami mana bakalaura darba analīžu procesa un rezultātu reproducēšanai.

## Repozitorija saturs
### [Ievaddati](./Dati/)
Satur petījuma analīzētos datus par dienastauriņiem novērojumiem: **`originalie_dati.xlsx`**

Datu fails satur sēkojošas lapas:
- Noverojumi
- Uzskaites
- Vietas
- Refernce
- Lauku_skaidrojumi

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

Mape **[Pollarda līknesa_salīdzinājums](./Pollards/indeksi/)**
Satur katrai pētījumā novērotai sugai veikto transekšu Pollarda indeksu vizuālo salīdzinājumu starp pētījuma vietām. Balstās uz novērojumiem Pollarda telpas ietvaros (1. līdz 3. josla).



### [Kartes](.//Kartes)
Satur bakalaura darbā un aizstavēšana izmantotās Latvijas konturkartes.



### [Pētījuma dizains](.//Dizains)
Satur attēlus, kas informē par galvēnājiem pētījuma dizaina aspektiem: uzskaites veikšanas metodes shēma un transektes un parauglaukumu izvietojuma shēma.
