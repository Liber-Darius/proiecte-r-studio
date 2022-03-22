#1. Creati doua liste ce contin câteva elemente comune; 
#ambele liste contin elemente de tip caracter.
#Scrieti un cod pentru a returna toate elementele dintr-o lista 
#care nu sunt în cealalta lista 
#(ex. sugestie: functia match).
#Ex x<-c("Ana, "Vlad"), y<-c("Ana, "Vlad", "Ema", "Marc").
#2. Creati o functie care calculeaza matricial,
#prin metoda OLS, coeficientii de regresie (b) 
#din regresia liniara cu 2 variabile explicative;
#apelati apoi functia pentru un set de date. 


#exercitiul 1
x <- c("Anamaria", "Mihai", "Emma", "Marcus", "Luca", "Alexandru");
y <- c("Emanuel", "Cristian", "Maria", "Mihai", "Eusebiu", "Luca");

x[is.na(match(x,y))] #elementele care sunt in x si nu sunt in y
y[is.na(match(y,x))] #elementele care sunt in y si nu sunt in x
intersect(x,y) #elemente comune intre x si y

#exercitiul 2
#y = x*beta + epsilon
# ne intereseaza beta
# avem urmatoarea formula
# beta = (transpusa(x) * x)^(-1) * transpusa(x)*y

Y=matrix(c(23,15,19,22,20))
X1=c(1,0,3,-1,2)
X2=c(3,1,2,4,0)
X=matrix(c(rep(1, 4),X1,X2),nrow=5)

solve(t(X) %*% X) %*% t(X) %*% Y #asta este beta

coeficienti_beta <- function(Y,X){ 

  solve(t(X) %*% X) %*% t(X) %*% Y
  
}

coeficienti_beta(Y,X)






