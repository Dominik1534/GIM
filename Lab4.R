install.packages("C50")
library(C50)

Nazwa<-(c('Apple iPhone 11 ','Apple iPhone 11 ','Apple iPhone 11 ','Apple iPhone 12 mini ','Apple iPhone 12 mini','Apple iPhone 12 mini'
          ,'Apple iPhone 12','Apple iPhone 12','Apple iPhone 12','Apple iPhone 12 Pro','Apple iPhone 12 Pro','Apple iPhone 12 Pro'))
Wyswietlacz<-c(6.1,6.1,6.1,5.4,5.4,5.4,6.1,6.1,6.1,6.1,6.1,6.1)
PamiecRam<-c(4,4,4,4,4,4,4,4,4,6,6,6)
PamiecWbudowana<-c(64,128,256,64,128,256,64,128,256,128,256,512)
Cena<-c(3199,3599,3899,2999,3399,3949,3999,4449,4599,5099,5469,6699)
AparatFoto<-c(36,36,36,36,36,36,36,36,36,48,48,48)
Komunikacja<-c(' WiFi 5G LTE NFC Bluetooth v4.0',' WiFi 5G LTE NFC Bluetooth v4.0',' WiFi 5G LTE NFC Bluetooth v4.0',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth'
               ,' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth',' WiFi 5G LTE NFC Bluetooth')

Opinia<-factor(c(5,5,4,5,5,4,5,5,4,5,5,5))





#ramka danych

smartfony<-data.frame(Nazwa,Wyswietlacz, PamiecRam, PamiecWbudowana, Cena,AparatFoto,Komunikacja,Opinia)

summary(smartfony)


built_tree<-C5.0(Opinia~.,data=smartfony)
summary(built_tree)
plot(built_tree)
predict(built_tree,newdata = smartfony,type = 'class')
