#SYMULOWANE WYZARZANIE

#CZYSZCZENIE
rm(list=ls())

#WCZYTANIE DANYCH
data=read.table("dane3.csv",sep=";",header = TRUE)
data1=read.table("dane3.csv",sep=";",header = TRUE)

dane1<-as.data.frame(data[,2:length(data[1,])],row.names = data[,1])
dane<-as.data.frame(data[,2:length(data[1,])],row.names = data[,1])

#funkcja liczaca koszt calkowity dla danej kolejonsci wierszy czyli rozwiaznia
koszt_calkowity<-function(dane_baz){

  #sumy 1 wiersza
  
  for(i in 2:length(dane1[1,])){
    
    dane_baz[1,i]<-as.data.frame(dane_baz[1,i-1]+dane_baz[1,i])
  }
  #sumy 1 kolumny
  
  for(i in 2:length(dane1[,1])){
    
    dane_baz[i,1]<-as.data.frame(dane_baz[i-1,1]+dane_baz[i,1])
  }
  #pozostale sumy, wybor wiekszej z i,j-1 a i-1,j i dodanie do biezacej
  
  for(j in 2:length(dane1[1,])){
    for(i in 2:length(dane1[,1])){
      if(dane_baz[i,j-1]<dane_baz[i-1,j]){
        dane_baz[i,j]<-dane_baz[i-1,j]+dane_baz[i,j]
      }
      else{
        dane_baz[i,j]<-dane_baz[i,j-1]+dane_baz[i,j]
      }
    }
  }
return(dane_baz)
}
baz<-koszt_calkowity(dane1)

#losujemy liczby i oraz j 
#zamieniamy miejsami te wiersze
nowe_roz<-function(data){
  data_1<-data_2<-data

  x<-sample(1:length(dane1[,1]),1)
  y<-sample(1:length(dane1[,1]),1)
  
  if(x==y){
    x<-sample(1:length(dane1[,1]),1)
    y<-sample(1:length(dane1[,1]),1)
  }else{
    data_1[x,]<-data_2[y,]
    data_1[y,]<-data_2[x,]
  }
  return(data_1)
}

#liczby musza byc random

#dane_nowe<-nowe_roz(data)
#dane_nowe2<-nowe_roz(dane_nowe)

#roz_n<-dane_nowe[,1]
#tabela<-dane_nowe[,2:11]

#nowy_koszt<-koszt_calkowity(roz_n,tabela)

#szkic algorytmu glownego

algorytm<-function(dane,temp){
  #losowe rozwiaznie startowe

  ndane<-dane
  i<-dane
  kolejnosc_i<-dane[,1]
  #parametr temperatury
  t_max<-temp
  
  #koszt rozwiazania i
  f_i<-koszt_calkowity(i[,2:length(data[1,])])
  
  #propozycja nowego rozwiazania
  j<-nowe_roz(dane)
  kolejnosc_j<-j[,1]
  
  #koszt rozwiazania j
  f_j<-koszt_calkowity(j[,2:length(data[1,])])
  
  #porownanie kosztow i wybor nizszego
  if((f_j[length(f_j[,1]),length(f_j[1,])]-f_i[length(f_i[,1]),length(f_i[1,])])<0){
    dane<-f_j
    ndane[,1]<-kolejnosc_j
    ndane[,2:(length(f_j[1,])+1)]<-j[,2:(length(f_j[1,])+1)]
  }else if(runif(1)<exp((f_i[length(f_i[,1]),length(f_i[1,])]-f_j[length(f_j[,1]),length(f_j[1,])])/t_max)){
    dane<-f_j
    ndane[,1]<-kolejnosc_j
    ndane[,2:(length(f_j[1,])+1)]<-j[,2:(length(f_j[1,])+1)]
  }else{
    dane_i<-f_i
    ndane[,1]<-kolejnosc_i
    ndane[,2:(length(f_i[1,])+1)]<-i[,2:(length(f_i[1,])+1)]
  }
  return(ndane)
}

#--------------------------------------------------------------------------------------------------
#ZMIANA TEMPERATURY PoczATKOWEJ

wynik1<-as.data.frame(0)
wynik_min_pocz<-as.data.frame(0)
for(y in 1:10){
  for(x in 1:10){
    
    temp=1-y*0.01
    dane_1<-data
    
    for (i in 1:150){
      wynik<-algorytm(dane_1,temp)
      k_c<-koszt_calkowity(dane_1[,2:length(dane_1[1,])])
      dane_1<-wynik
      temp<-temp*0.9
      i=i+1
    }
    
    wynik1[1:200,x]<-as.data.frame(wynik[,1])
    k_w<-koszt_calkowity(wynik)
    wynik1[201,x]<-as.data.frame(k_w[200,20])
    
  }
  m<-min(wynik1[201,])
  for(x in 1:10){
    if(wynik1[201,x]==m){
      wynik_min_pocz[1:201,y]<-wynik1[,x]
      
    }
  }

}
write.csv(wynik_min_pocz,file = "Wynik1_2.csv")

#--------------------------------------------------------------------------------------------------
#ZMIANA SZYBKOSCI ZMIENIANIA TEMPERATURY
wynik1<-as.data.frame(0)
wynik_min_temp<-as.data.frame(0)
for(y in 1:10){
  for(x in 1:5){
    
    temp=0.95
    dane_1<-data
    
    for (i in 1:100){
      wynik<-algorytm(dane_1,temp)
      k_c<-koszt_calkowity(dane_1[,2:length(dane_1[1,])])
      dane_1<-wynik
      alpha<-(1-y*0.02)
      temp<-temp*alpha
      i=i+1
    }
    
    wynik1[1:200,x]<-as.data.frame(wynik[,1])
    k_w<-koszt_calkowity(wynik)
    wynik1[201,x]<-as.data.frame(k_w[200,20])
    
  }
  m<-min(wynik1[201,])
  for(x in 1:5){
    if(wynik1[201,x]==m){
      wynik_min_temp[1:201,y]<-wynik1[,x]
      
    }
  }
  y=y+1
  
}
write.csv(wynik_min_temp,file = "Wynik2_2.csv")

#--------------------------------------------------------------------------------------------------
#ZMIANA ilosci iteracji
wynik1<-as.data.frame(0)
wynik_min_iteracje<-as.data.frame(0)
for(y in 1:10){
  for(x in 1:5){
    
    temp=0.95
    dane_1<-data
    iter<-100*y/2
    for (i in 1:iter){
      wynik<-algorytm(dane_1,temp)
      k_c<-koszt_calkowity(dane_1[,2:length(dane_1[1,])])
      dane_1<-wynik
      temp<-temp*0.9
      i=i+1
    }
    
    wynik1[1:200,x]<-as.data.frame(wynik[,1])
    k_w<-koszt_calkowity(wynik)
    wynik1[201,x]<-as.data.frame(k_w[200,20])
    
  }
  m<-min(wynik1[201,])
  for(x in 1:5){
    if(wynik1[201,x]==m){
      wynik_min_iteracje[1:201,y]<-wynik1[,x]
      
    }
  }
  
}
write.csv(wynik_min_iteracje,file = "Wynik3_2.csv")









