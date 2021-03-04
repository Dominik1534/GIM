#a)
#wartoœæ a
a<-12/exp(12)
a
#wartoœæ b
b<-a*3
b
#sprawdzenie co jest wiêksze
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
#Wszystkie funkcje zawieraj¹ce max w swojej nazwie

apropos ("max")

# E) 
# œcie¿ka katalogu roboczego 
setwd("C:/Users/DK/Desktop/MGR/APU")
getwd()

# zmienna a zawieraj¹ca ³añcuch znaków 
a<- "smartfon Motorola"

# zapis do pliku
write(a, file='Podpunkt_E.csv')

# usuniêcie zmiennej a oraz sprawdzenie poprawnoœci dzia³ania

remove(a)

a

# wczytanie pliku ze zmienn¹ a i sprawdzenie jej wartoœci 

a<-read.csv(file='Podpunkt_E.csv', sep=',')
a

# F)
# instalacja pakietu gridExtra
install.packages("gridExtra")

#za³adowanie pakietu gridExtra
library(gridExtra)

#dokumentacja pakietu
library(help = grid)
help(package = "gridExtra")

# pierwsze 10 wierszy zbioru Titanic
grid.table(Titanic[1:10])

# G)
#wektor zawieraj¹cy ci¹g liczb

a<-seq(400,320, -8)
a

#H)
#wektor a z liczbami od 60 do 40
a<-60:40
a
#wektor b z loiczbami od 40 do 50 
b<-40:50
b

#po³aczenie wektorów b,a
d<-c(b,a)
d

#I)

#wektor nazwa z z nazwami 10 smartfonów Motorola 

nazwa<-c("Motorola 1000", "Motorola 2000", "Motorola 3000", "Motorola 4000", "Motorola 5000", 
         "Motorola 6000", "Motorola 7000", "Motorola 8000", "Motorola 9000", "Motorola 10000")


wyœwietlacz<-c('IPS','IPS','IPS','IPS','IPS','IPS','OLED','OLED','OLED','OLED')

pamiê_RAM<-c('2GB','2GB','4GB','4GB','6GB',
           '8GB','8GB','10GB','16GB','32GB')

pamiê_wbudowana<-c('16GB','32GB','64GB','64GB','128GB','256GB', '256GB', '256GB','512GB','512GB')

aparat_foto<- c('12Mpix','12Mpix','12Mpix','12Mpix','32Mpix','32Mpix','64Mpix','64Mpix','64Mpix','128Mpix')

cena<- c(1000,1500,1700,1800,2000,2200,2500,2700,2800,3200)

liczba_opinii<- c(80,90,50,40,30,40,40,10,20,35)

#ramka danych

smartfony<-data.frame(nazwa,wyœwietlacz, pamiê_RAM, pamiê_wbudowana, aparat_foto, cena, liczba_opinii)
smartfony

#œrednia ceny 
mean(smartfony$cena)


#J)
#dodanie nowego wpisu i ponowne wyliczenie œredniej ceny

smartfony_nowy<-data.frame(nazwa="Motorola 11000",wyœwietlacz="OLED", pamiê_RAM='32GB', pamiê_wbudowana='512GB',
                           aparat_foto='128Mpix', cena=3500, liczba_opinii=2)
smartfony_nowy

smartfony <- rbind(smartfony, smartfony_nowy)
smartfony

mean(smartfony$cena)

#K)
#stworzenie nowej kolumny ocena_klienta
smartfony$ocena_klienta<-c('0','0,5','1','1,5','2','2,5','3','3,5','4','4,5','5','4')

smartfony

aggregate(smartfony$cena, list(smartfony$ocena_klienta),mean)

#L)
#dodanie 4 smartfonów do ramki
smartfony_nowy<-data.frame(nazwa="Motorola 12000",wyœwietlacz="OLED", pamiê_RAM='32GB', pamiê_wbudowana='512GB',
                           aparat_foto='128Mpix', cena=3500, liczba_opinii=13,ocena_klienta = '4')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola 13000",wyœwietlacz="OLED", pamiê_RAM='32GB', pamiê_wbudowana='512GB',
                           aparat_foto='128Mpix', cena=3600, liczba_opinii=24,ocena_klienta = '4')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola 14000",wyœwietlacz="OLED", pamiê_RAM='64GB', pamiê_wbudowana='512GB',
                           aparat_foto='128Mpix', cena=3700, liczba_opinii=0,ocena_klienta = '5')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony_nowy<-data.frame(nazwa="Motorola 15000",wyœwietlacz="OLED", pamiê_RAM='64GB', pamiê_wbudowana='512GB',
                           aparat_foto='128Mpix', cena=3800, liczba_opinii=0,ocena_klienta = '4')
smartfony <- rbind(smartfony, smartfony_nowy)

smartfony

install.packages("plotrix")

library(plotrix)

dane <- aggregate(smartfony$liczba_opinii, list(smartfony$ocena_klienta), sum)

barp(dane[,2], names.arg = dane[,1], main = 'Liczebnoœæ reprezentantów ka¿dej z ocen klientów')

#M)Procentowy udzia³ ka¿dej oceny

percentage <- table(smartfony$ocena)/length(smartfony$ocena_klienta)
pie(percentage)


percentage <- table(smartfony$ocena)/length(smartfony$ocena_klienta)
fan.plot(percentage, labels = names(percentage), main = "Procentowy udzia³ oceny")

#N)
#procentowy udzia³ monitorów o konkretnym statusie opinii
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
  print(paste(smartfony$nazwa[i], 'ma ocenê klientów', smartfony$ocena_klienta[i], 
              ',bo ma liczbê opinii', smartfony$liczba_opinii[i]))
}

#P)
#ramka danych w pliku .csv.³adowanie ramki danych z pliku .csv

write.csv(smartfony, 'smartfony.csv')
dane <- read.csv('smartfony.csv')

