#1) Creati un vector x ce contine 20 de valori.
#Aceste valori redau vanzarile dintr-un produs in n_days=5 zile (de luni pana vineri) 
#la n_markets=4 magazine;
#primele 5 valori sunt aferente primului magazin, 
#urmatoarele 5 sunt vanzarile la al 2-lea magazin..
#Scrieti un script ce afiseaza valoarea medie a vanzarilor 
#la fiecare din cele 5 magazine intr-un singur tabel
#(pe prima linie denumirea magazinului, iar a doua linie contine media).

#2) Scrieti si explicati (ce fac) scripturi simple în care se utilizeaza
#structurile de control  if, for, while, câte 2 pentru fiecare. 


#exercitiul 1

x <- c(12,7,34,9,14,22,17,16,42,15,11,22,24,7,44,19,2,76,62,18);
x
magazin_1 = x[1:5]
magazin_2 = x[6:10]
magazin_3 = x[11:15]
magazin_4 = x[16:20]

medie_vanzari <- data.frame (
  magazine = c("magazin 1", "magazin 2", "magazin 3", "magazin 4"),
  medie = c(mean(magazin_1),mean(magazin_2),mean(magazin_3),mean(magazin_4))
)

medie_vanzari;
rev_medie <- t(medie_vanzari); #transpusa
rev_medie

#exercitiul 2

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


