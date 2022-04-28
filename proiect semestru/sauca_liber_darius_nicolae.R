#Scrieti si explicati (ce fac) scripturi simple in care se utilizeaza
#structurile de control  if, for, while, cate 2 pentru fiecare. 

#subpunctul a

x <- c(12,7,34,9,14,22,17,16,42,15,11,22,24,7,44,19,2,76,62,18,13,15,40,23,80,60,45,12);
x
magazin_1 = x[1:7]
magazin_2 = x[8:14]
magazin_3 = x[15:21]
magazin_4 = x[22:28]

medie_vanzari <- data.frame (
  magazine = c("magazin 1", "magazin 2", "magazin 3", "magazin 4"),
  medie = c(mean(magazin_1),mean(magazin_2),mean(magazin_3),mean(magazin_4))
)

medie_vanzari;
rev_medie <- t(medie_vanzari); #transpusa
rev_medie


#instructiunea if 

if(max(magazin_1) > 10){print("numarul maxim de vanzari intr-o zi in magazinul 1 este mai mare de 10")} else 
{print("numarul maxim de vanzari intr-o zi in magazinul 1 este mai mic de 10")}
#verifica daca numarul maxim de vanzari intr-o zi in magazinul 1 este mai mare decat 10

numar <- as.integer(readline(prompt="introdu numar: ")); 
if(numar %% 2 == 0){cat("numarul ", numar , " este par\n")} else{cat("numarul ", numar, " este impar\n")}
#verifica daca un numar citit de la tastatura e par sau impar

#instructiunile for si while

for( i in 1:100){
  k<-0
  for(j in 1:i){
    if(i%%j==0)k<-k+1;
  }
  if(k == 2)print(j)
}#afisez numerele prime pana la 100


for(i in 1:nrow(medie_vanzari)) {       
  cat(medie_vanzari[i, 1], "a avut un numar mediu zilnic de vanzari de: ", medie_vanzari[ i, 2], "\n"); 
}
#afisez un mesaj cu magazinul si media zilnica de vanzari

#mai jos aceasi chestie doar ca cu while
i <- 1;
while(i <= nrow(medie_vanzari)) {       
  cat(medie_vanzari[i, 1], "a avut un numar mediu zilnic de vanzari de: ", medie_vanzari[ i, 2], "\n"); 
  i <- i+1
}

number <- 100
sum <- 0 
while(number >= 0 ) {
  sum = sum + number
  number = number - 2
}
sum
#suma numerelor pare mai mici decat 100

#######################################################################################################################

#subpunctul b

s<- 0 	#media
media <- function(x) {
  n <-  length(x) 
  for(i in 1:n) s <- s + x[i]
  medie<-s / n
  return(medie);
}


#abaterea standard 

#formula ii radical din (suma de la 1 la n din (valoarea nodului i - media)^2 / n-1
abatere_standard <- function(x){
  n <- length(x);
  suma = 0;
  for( i in 1:n) suma = suma + (x[i] - media(x))^2;
  rezultat = sqrt(suma/(n-1));
  return(rezultat);
}

a<-c(2, 4, 6, 7.5, 3)
print(paste("abaterea standard conform functiei mele este: ", abatere_standard(a)));
print(paste("abaterea standard conform functiei sd este: ", sd(a)))

#test student
student_test <-function(x,prob){
se <- sd(x) / sqrt(length(x))
alpha <- 1 - prob #qnorm ii normal distribution
limite<-c(mean(x) - se * qnorm(1-alpha / 2), mean(x) + se * qnorm(1-alpha / 2))
return(limite)
}
student_test(a,0.95)
test_medie<-t.test(a)
test_medie

#corelatie

#formula ii RAPORTUL DINTRE COVARIAŢIA SERIILOR ŞI PRODUSUL DEVIAŢIILOR LOR STANDARD
#la numarator suma de (Xi - media)*(yi - media)
#la numitor produsul dintre abaterea pentru x si pentru y

corelatie <- function(x,y){
  
  suma = 0;
  n = length(x);
  for(i in 1:n){
    suma = suma + (x[i]-mean(x))*(y[i]-mean(y))
  }
  rezultat = suma/(sd(x)*sd(y));
  return(rezultat/4)
}
a<-c(2, 4, 6, 7.5, 3)
b<-c(5, 6, 6.5, 8, 4.5)
cor(a,b)
corelatie(a,b)


#######################################################################################################################

#subpunctul c

x <- c(12,7,34,9,14,22,17,16,42,15,11,22,24,7,44,19,2,76,62,18,13,15,40,23,80,60,45,12);
x
luni = x[seq(1, length(x), 7)]
marti = x[seq(2, length(x), 7)]
miercuri = x[seq(3, length(x), 7)]
joi = x[seq(4, length(x), 7)]
vineri = x[seq(5, length(x), 7)]
sambata = x[seq(6, length(x), 7)]
duminica = x[seq(7, length(x), 7)]

media_pe_zi <- data.frame (
  ziua = c("luni", "marti", "miercuri", "joi", "vineri", "sambata", "duminica"),
  media = c(mean(luni),mean(marti),mean(miercuri),mean(joi), mean(vineri), mean(sambata), mean(duminica))
)

media_pe_zi
media_pe_zi_reversed <- t(media_pe_zi); #transpusa, asa cum este cerut in cerinta
media_pe_zi_reversed


matricea_x <- matrix(x, 7); #pe prima linie vanzarile de luni, a doua linie vanzarile de marti si as amai departe
matricea_x #coloanele reprezinta magazine asadar coloana 1 reprezinta primul mazagin

data_frame_x <- as.data.frame(matricea_x);
row.names(data_frame_x) <- c("luni", "marti", "miercuri", "joi", "vineri", "sambata", "duminica");
colnames(data_frame_x) <- c("magazin_1", "magazin_2", "magazin_3", "magazin_4")
data_frame_x = rev(data_frame_x);
data_frame_x
apply(data_frame_x, MARGIN=1, FUN=mean) #1 de la margin e pt linii, 2 e pt coloane

#d) este in pdf

#######################################################################################################################

#subpunctul e

#https://ec.europa.eu/eurostat/databrowser/view/tin00134/default/table?lang=en


format_wide<-read_xlsx('C:\\Users\\liber\\OneDrive\\Desktop\\proiecte r studio\\proiect de semestru\\proiect semestru\\tin00134_spreadsheet.xlsx')
View(format_wide)

format_long <- format_wide %>% 
  gather(key   = denumire,
         value = valori, -TIME, convert = TRUE)
View(format_long)

colnames(format_long)
colnames(format_long) <- c("tara", "an", "procent")

#format_long <-format_long %>% 
#  unite(col = variabila_year, tara, an, sep = "_")
#View(format_long)

format_long[format_long == ":"] <- NA
View(format_long)

format_long$procent<-as.integer(format_long$procent,na.omit = TRUE)
media_pe_an <- format_long %>% 
  filter(an == 2020) %>%
  summarize(
            media_pe_an = mean(procent, na.rm= TRUE)
            )
cat("Media pe anul 2020 este: ", sum(media_pe_an))

media_pe_ani <- format_long %>% 
  group_by(an)%>% 
  summarise(mean(procent, na.rm = TRUE))
colnames(media_pe_ani) <- c("an", "procent")
media_pe_ani

#######################################################################################################################

#subpunctul f

#https://en.wikipedia.org/wiki/List_of_countries_by_average_wage#cite_note-OECDaaw-3

library(rvest)
library(tidyr)
pagina<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_average_wage#cite_note-OECDaaw-3")	#url
class(pagina)
library(magrittr)
tabele<-pagina %>% html_nodes("table") 	#selecteaza noduri/elemente table din pagina web
length(tabele)
hpi<-html_table(tabele[[1]])  #primul tabel de pe pagina
hpi[1] <- lapply(hpi[1], gsub, pattern = "*", replacement = "", fixed = TRUE)
hpi[] <- lapply(hpi[], gsub, pattern = ",", replacement = "", fixed = TRUE)
View(hpi)

salarii_long <- hpi %>% 
  gather(key   = an,
         value = salariu, -Country, convert = TRUE)
View(salarii_long)

#salariul mediu in fiecare an
salarii_long$salariu<-as.integer(salarii_long$salariu,na.omit = TRUE)
salarii_pe_ani <- salarii_long %>% 
  group_by(an)%>% 
  summarise(mean(salariu, na.rm = TRUE))
colnames(salarii_pe_ani) <- c("an", "salariu")
salarii_pe_ani

salarii_long$salariu<-as.integer(salarii_long$salariu,na.omit = TRUE)
salariu_median_2017 <- salarii_long %>% 
  filter(an == 2017) %>%
  summarize(
    salariu_median_2017 = median(salariu, na.rm= TRUE)
  )
cat("salariul median dintre tarile selectate in anul 2017 a fost: ", sum(salariu_median_2017))

library(ggplot2)
library(dplyr)
ggplot(salarii_long,aes(x = an,y = salariu)) +
  geom_point() +   #regresie lineara (nu stiu daca are sens in contextul asta)
  geom_smooth(method='lm')

salarii_Norvegia <- salarii_long
salarii_Norvegia$Country<- as.character(salarii_long$Country)
salarii_Norvegia <- salarii_Norvegia %>% filter(str_detect(Country, "^Norway"))

salarii_Norvegia %>%
  ggplot(aes(x = an, y = salariu)) +
  geom_line() +
  geom_point()
  labs(
       y = "salariu",
       x = "an")
#grafic evolutie salariu in Norvegia

#######################################################################################################################
#######################################################################################################################

#problema 2

# avem ca set de date melodiile aflate in top 100 anual din 2010 si pana astazi
#obiectiv: sa identificam patternuri ale acestor piese, de aceea vom crea tabele de frecventa si grafice de corelatie
# variabile selectate
  #artist_type - categoriala
  #beats_per_minute - cantitativa continua numerica
  #duration - cantitativa continua numerica
  
songs_wide<-readr::read_csv('C:\\Users\\liber\\OneDrive\\Desktop\\proiecte r studio\\proiect de semestru\\proiect semestru\\Spotify 2010 - 2019 Top 100.csv')

#sectiunea de cleaning
songs_wide <- na.omit(songs_wide)
songs_wide$added <- as.Date(songs_wide$added) #convertim din caracter in tip data calendaristica
songs_wide$`top genre` <- as.factor(songs_wide$`top genre`) #le facem tip categorii
songs_wide$`artist type` <- as.factor(songs_wide$`artist type`)
#cateva renameuri
songs_wide <- songs_wide %>%	
  rename(beats_per_minute = "bpm") %>%
  rename(energy = "nrgy") %>%
  rename(dance = "dnce") %>%
  rename(duration = "dur") %>%
  rename(acoustic = "acous") %>%
  rename(speech = "spch") %>%
  rename(top_genre = "top genre") %>%
  rename(top_year = "top year") %>%
  rename(artist_type = "artist type") %>%
  rename(year_released = "year released")
songs_wide <- select(songs_wide, -val) #am sters coloana val pentru ca nu stiu ce semnifica
View(songs_wide)

#transformare in format long
songs_long <- songs_wide %>% 
  gather(key   = variabila,
         value = valoare, -title, convert = TRUE)
View(songs_long)


beats_per_minute_mediu <- songs_long %>% 
  filter(variabila == "beats_per_minute") %>%
  summarise(
    beats_per_minute_mediu <- mean(as.numeric(valoare), na.rm=TRUE)
  )

cat("valoarea medie beats_per_minute a tuturor pieselor este: ", sum(beats_per_minute_mediu))

#dorim sa vedem impartirea tuturor cantecelor in functie de tipul de artist(solo, duo, band, etc)
numar_songs_per_artist_type <- songs_wide %>%
  group_by(artist_type) %>%
  summarise(
    numar <- n()
  )
colnames(numar_songs_per_artist_type) <- c("tip artist", "numar cantece")
numar_songs_per_artist_type <- as.data.frame(numar_songs_per_artist_type)
numar_songs_per_artist_type
barplot(numar_songs_per_artist_type$`numar cantece`, names.arg=numar_songs_per_artist_type$`tip artist`, main = "numar cantece per tip de artist")

#dorim sa vedem numarul durata medie a unei piese in functie de tipul de artist
durata_songs_per_artist_type <- songs_wide %>%
  group_by(artist_type) %>%
  summarise(
    durata <- mean(duration)
  )
colnames(durata_songs_per_artist_type) <- c("tip artist", "durata medie")
durata_songs_per_artist_type <- as.data.frame(durata_songs_per_artist_type)
durata_songs_per_artist_type
barplot(durata_songs_per_artist_type$`durata medie`, names.arg=durata_songs_per_artist_type$`tip artist`, main = "numar cantece per tip de artist")
#observam ca nu exista diferente majore in durata cantecelor in fuctie de artist_type

#vom face plot la grafice avand 2 variabile(una pentru axa Ox cealalta pentru axa Oy) pentru a incerca sa gasim patternuri si corelatii

ggplot(songs_wide,aes(x = duration,y = beats_per_minute)) +
  geom_point() +
  geom_smooth(method='lm')
#beats_per minute si duration, nu pare sa existe corelatie

ggplot(songs_wide,aes(x = live,y = acoustic)) +
  geom_point() +
  geom_smooth(method='lm')
#pare sa existe o corelatie mai mare intre variabilele live si acoustic

ggplot(songs_wide,aes(x = dance,y = speech)) +
  geom_point() +
  geom_smooth(method='lm')

#vrem sa vedem cum a evoluat anual numarul de piese cantate de Bands/Groups din top 100
band_songs_per_year <- songs_wide %>%
  filter(artist_type == "Band/Group") %>%
  group_by(top_year) %>%
  summarise(
    numar <- n()
  )

colnames(band_songs_per_year) <- c("an", "nr piese")
band_songs_per_year <- as.data.frame(band_songs_per_year)
band_songs_per_year

band_songs_per_year %>%
  ggplot(aes(x = an, y = `nr piese`)) +
  geom_line() +
  geom_point()
labs(
  y = "nr piese",
  x = "an")
#observam o tendinta clara de scadere a numarului de cantece cantate de bands/group 


 
 




  
  
  
