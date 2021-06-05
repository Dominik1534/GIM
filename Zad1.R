#a)
#warto?? a
a<-12/exp(12)
a
#warto?? b
b<-a*3
b
#sprawdzenie co jest wi?ksze
max(a,b)
#b)
#wyszukanie dokumentacji funkcji sqrt()
help(sqrt)
# c)
# tworzenie wektora a z zakresu 80-90
c<- 80:90
c
#suma kwadratow liczb zawartych w wektorze
sum (c)

#D)
#Wszystkie funkcje zawieraj?ce max w swojej nazwie

apropos ("max")

# E) 
# ?cie?ka katalogu roboczego 
setwd("C:/Users/DK/Desktop/MGR/APU")
getwd()

# zmienna a zawieraj?ca ?a?cuch znak?w 
a<- "smartfon Motorola"

# zapis do pliku
write(a, file='Podpunkt_E.csv')

# usuni?cie zmiennej a oraz sprawdzenie poprawno?ci dzia?ania

remove(a)

a

# wczytanie pliku ze zmienn? a i sprawdzenie jej warto?ci 

a<-read.csv(file='Podpunkt_E.csv', sep=',')
a

# F)
# instalacja pakietu gridExtra
install.packages("gridExtra")

#za?adowanie pakietu gridExtra
library(gridExtra)

#dokumentacja pakietu
library(help = grid)
help(package = "gridExtra")

# pierwsze 10 wierszy zbioru Titanic
grid.table(Titanic[1:10])

# G)
#wektor zawieraj?cy ci?g liczb

a<-seq(400,320, -8)
a

#H)
#wektor a z liczbami od 60 do 40
a<-60:40
a
#wektor b z loiczbami od 40 do 50 
b<-40:50
b

#po?aczenie wektor?w b,a
d<-c(b,a)
d

#I)

#wektor nazwa z z nazwami 10 smartfon?w Motorola 

nazwa<-c("Motorola Moto E7 Plus ", "Motorola Moto g9 play", "Motorola Moto E6s", "Motorola Moto g9 power", "Motorola Moto G Pro ", 
         "Motorola Moto g100", "Motorola Moto g9 plus", "Motorola Moto G8 Power 4", "Motorola Moto E7i Power", "Motorola EDGE")


wyswietlacz<-c('HD+','Max Vision HD+','IPS','IPS','IPS','Full HD+','IPS','OLED','Max Vision HD+','OLED')

pamiecRAM<-c(4,4,2,4,4,
           8,4,4,2,8)

pamiecwbudowana<-c(64,64,32,128,128,128, 128, 64,32,128)

aparat_foto<- c(48,48,12,64,48,64,64,16,13,64)

cena<- c(499,699,399,749,1499,2499,899,599,449,2599)

liczba_opinii<- c(199,38,258,57,9,7,88,104,4,30)

#ramka danych

smartfony<-data.frame(nazwa,wyswietlacz, pamiecRAM, pamiecwbudowana, aparat_foto, cena, liczba_opinii)
smartfony

#?rednia ceny 
mean(smartfony$cena)


#J)
#dodanie nowego wpisu i ponowne wyliczenie ?redniej ceny

smartfony_nowy<-data.frame(nazwa="Motorola Moto G8",wyswietlacz="OLED", pamiecRAM=4, pamiecwbudowana=64,
                           aparat_foto=16, cena=669, liczba_opinii=60)
smartfony_nowy

smartfony <- rbind(smartfony, smartfony_nowy)
smartfony

mean(smartfony$cena)

#K)
#stworzenie nowej kolumny ocena_klienta
smartfony$ocena_klienta<-c('5','5','5','5','4','5','5','5','4','5','5')

smartfony

aggregate(smartfony$cena, list(smartfony$ocena_klienta),mean)

#L)
#dodanie 4 smartfon?w do ramki
smartfony_nowy<-data.frame(nazwa="Motorola Moto g5G plus 6",wyswietlacz="IPS", pamiecRAM=6, pamiecwbudowana=128,
                           aparat_foto=48, cena=1899, liczba_opinii=55,ocena_klienta = '5')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola One Zoom",wyswietlacz="OLED FHD+", pamiecRAM=4, pamiecwbudowana=128,
                           aparat_foto=48, cena=1099, liczba_opinii=61,ocena_klienta = '4')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola Moto g30",wyswietlacz="IPS", pamiecRAM=6, pamiecwbudowana=128,
                           aparat_foto=64, cena=999, liczba_opinii=22,ocena_klienta = '5')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola Moto g9 plus 4",wyswietlacz="IPS", pamiecRAM=4, pamiecwbudowana=128,
                           aparat_foto=64, cena=899, liczba_opinii=34,ocena_klienta = '4')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony

install.packages("plotrix")

library(plotrix)

dane <- aggregate(smartfony$liczba_opinii, list(smartfony$ocena_klienta), sum)

barp(dane[,2], names.arg = dane[,1], main = 'Liczebnosc reprezentantacji w kazdej z ocen klientow')

#M)Procentowy udzia? ka?dej oceny

percentage <- table(smartfony$ocena)/length(smartfony$ocena_klienta)
pie(percentage)


percentage <- table(smartfony$ocena)/length(smartfony$ocena_klienta)

fan.plot(percentage, labels = names(percentage), main = "Procentowy udzial oceny")

#N)
#procentowy udzia? monitor?w o konkretnym statusie opinii
n_column <- ifelse(smartfony$liczba_opinii>100,'wiecej 100 opinii',
                   ifelse(smartfony$liczba_opinii>=50, '50-100 opinii', 
                          ifelse(smartfony$liczba_opinii>0, 'mniej 50 opinii', 
                                 'nie ma')))
                                 
smartfony['status_opinii'] <- factor(n_column)

percentage <-table(smartfony$status_opinii)/length(smartfony$status_opinii)
pie(percentage)


#O)
#utworzenie zdania 
for (i in 1:length(smartfony$nazwa)){
  print(paste(smartfony$nazwa[i], 'ma oceny klientow', smartfony$ocena_klienta[i], 
              ',bo ma liczbe opinii', smartfony$liczba_opinii[i]))
}

#P)
#ramka danych w pliku .csv.?adowanie ramki danych z pliku .csv

write.csv(smartfony, 'smartfony.csv')
dane <- read.csv('smartfony.csv')

