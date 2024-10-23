setwd("C:/Users/Ioana/OneDrive/Desktop")
library(moments)
library(corrplot)

proiect <- read.csv("C:/Users/Ioana/OneDrive/Desktop/proiect.csv")
View(proiect)

# Data frame preturi ---------------------------------------------------------


preturi<-data.frame(proiect)
View(preturi)


# statistici descriptive
summary(preturi)[,-1]
# Putem observa ca Johnson&Johnson are pretul minim si pretul maxim mai mari decat Pfizer, competitorul sau.

# sd
sd_JNJ<-sd(preturi$Pret_JNJ)  # 6.42 
sd_Pfizer<-sd(preturi$Pret_Pfizer) # 4.56
sd_Nasdaq<-sd(preturi$Pret_Nasdaq) #1122.55


# cv
cv_JNJ<- sd_JNJ/mean(preturi$Pret_JNJ) # 0.04
cv_Pfizer<-sd_Pfizer/mean(preturi$Pret_Pfizer) # 0.12
cv_Nasdaq<-sd_Nasdaq/mean(preturi$Pret_Nasdaq) # 0.08
# Deoarece toti 3 coeficientii sunt <1, inseamna ca media este reprezentativa

# skewness - asimetrie
skewness_JNJ<-skewness(preturi$Pret_JNJ)
skewness_Pfizer<-skewness(preturi$Pret_Pfizer)
skewness_Nasdaq<-skewness(preturi$Pret_Nasdaq)
# Coeficientii de asimetrie sunt: 
# 0.41 Johnson&Johnson => Avem o asimetrie la dreapta, inregistrandu-se multe preturi mari si posibil outlieri pe preturi mari.
# 0.14 Pfizer => Avem o usoara asimetrie la dreapta, putand fi considerata si aproape simetrie, nu exista o predominanta puternica a preturilor mici sau mari
# -0.26 => Avem o usoara asimetrie la stanga, deci, o tendinta usoara catre preturi mai mici in comparatie cu cele mai mari, dar asimetria nu este semnificativa. 


# kurtosis - aplatizare
kurtosis_JNJ<- kurtosis(preturi$Pret_JNJ)
kurtosis_Pfizer<- kurtosis(preturi$Pret_Pfizer)
kurtosis_Nasdaq<- kurtosis(preturi$Pret_Nasdaq)
# Toti cei trei coeficienti de aplatizare sunt <3, deci avem distributii usor platicurtice(preturile sunt mai imprastiate in jurul mediei)


# Matrice cu abatere standard, coeficienti de variatie, skewness si kurtosis
matrice_preturi<-matrix(c(sd_JNJ,sd_Pfizer,sd_Nasdaq,cv_JNJ,cv_Pfizer,cv_Nasdaq,skewness_JNJ,skewness_Pfizer
                          ,skewness_Nasdaq,kurtosis_JNJ,kurtosis_Pfizer,kurtosis_Nasdaq),nrow=3,ncol=4,
                        dimnames=list(c("Johnson&Johnson","Pfizer","Nasdaq"),c("Abatere standard","Coeficient de variatie","Skewness","Kurtosis")))
View(matrice_preturi)


# Histograme si Boxploturi preturi
par(mfrow=c(3,2))
hist(preturi$Pret_JNJ,col="red4",main="Pretul Johnson&Johnson",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_JNJ,col="red4",main="Pretul Johnson&Johnson",horizontal = TRUE)
hist(preturi$Pret_Pfizer,col="coral1",main="Pretul Pfizer",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_Pfizer,col="coral1",main="Pretul Pfizer",horizontal = TRUE)
hist(preturi$Pret_Nasdaq,col="darkviolet",main="Pretul Nasdaq",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_Nasdaq,col="darkviolet",main="Pretul Nasdaq",horizontal = TRUE)
#Interpretare:
#Distributia pretului actiunii J&J pare a fi asimetrica la dreapta, avand outlieri pe preturile mari.
#Distributia pretului actiunii Pfizer pare a fi asimetrica tot la dreapta, avand si ea outlieri pe preturile mari.
#Distributia indicelui Nasdaq prezinta o asimetrie la stanga si nu prezinta outlieri.

# matricea de corelatie

matrice_corelatie_preturi<-cor(preturi[2:4])
View(matrice_corelatie_preturi)
corrplot(matrice_corelatie_preturi,method ="number")
#Interpretare:
#Se observa o corelatie destul de slaba intre pretul actiunilor J&J si cel al Pfizer
#Putem spune ca intre J&J si Nasdaq nu exista o corelatie anume.
# Putem spune, de asemenea, ca cele doua actiuni nu au o influenta prea mare asupra indicelui Nasdaq.







# Data frame RENTABILITATI ------------------------------------------------

rentabilitate<-function(x){
  lungime<-length(x) # creez un vector de lungime x
  vect<-c() # declar variabila a ca fiind goala pentru a adauga ulterior valorile
  vect[1]<-0
  for(i in 2:lungime){ 
    vect[i]<- (x[i]/x[i-1])-1 
  }
  return(vect)
}
rentabilitati_preturi<-apply(preturi[2:4], MARGIN = 2, FUN=rentabilitate)
preturi<-data.frame(preturi,rentabilitati_preturi)
View(preturi)

# statistici descriptive
summary(preturi[,5:7])

# sd
sd_rent_JNJ<-sd(preturi$Pret_JNJ.1)  # 0.010
sd_rent_Pfizer<-sd(preturi$Pret_Pfizer.1) # 0.014
sd_rent_Nasdaq<-sd(preturi$Pret_Nasdaq.1) # 0.010

# cv
cv_rent_JNJ<- sd_rent_JNJ/mean(preturi$Pret_JNJ.1) # -30.6
cv_rent_Pfizer<- sd_rent_Pfizer/mean(preturi$Pret_Pfizer.1) # -7.15
cv_rent_Nasdaq<- sd_rent_Nasdaq/mean(preturi$Pret_Nasdaq.1) # 7.13


# skewness - asimetrie
skewness_rent_JNJ<-skewness(preturi$Pret_JNJ.1) # 0.49
skewness_rent_Pfizer<-skewness(preturi$Pret_Pfizer.1)# -0.26
skewness_rent_Nasdaq<-skewness(preturi$Pret_Nasdaq.1) # 0.01
# Coeficientii de asimetrie sunt: 
# 0.49 Johnson&Johnson => Avem o asimetrie la dreapta, inregistrandu-se multe preturi mari si posibil outlieri pe preturi mari.
# -0.26 Pfizer => Avem o usoara asimetrie la stanga, deci, o tendinta usoara catre preturi mai mici in comparatie cu cele mai mari, dar asimetria nu este semnificativa.
# 0.01  => Avem o usoara asimetrie la dreapta, putand fi considerata si aproape simetrie, nu exista o predominanta puternica a preturilor mici sau mari


# kurtosis - aplatizare
kurtosis_rent_JNJ<- kurtosis(preturi$Pret_JNJ.1) # 9.45
kurtosis_rent_Pfizer<- kurtosis(preturi$Pret_Pfizer.1) #5.87 
kurtosis_rent_Nasdaq<- kurtosis(preturi$Pret_Nasdaq.1) #2.72 
# Distributiile rentabilitatilor actiunilor JNJ si Pfizer au coeficientii de boltire > 3, insemnand ca acestea sunt leptocurtice
# Coeficientul rentabilitatii Nasdaq <3, deci avem distributie usor platicurtica(preturile sunt mai imprastiate in jurul mediei)


# Matrice cu abatere standard, coeficienti de variatie, skewness si kurtosis
matrice_rent_preturi<-matrix(c(sd_rent_JNJ,sd_rent_Pfizer,sd_rent_Nasdaq,cv_rent_JNJ,cv_rent_Pfizer,cv_rent_Nasdaq
                               ,skewness_rent_JNJ,skewness_rent_Pfizer,skewness_rent_Nasdaq,kurtosis_rent_JNJ,kurtosis_Pfizer,kurtosis_rent_Nasdaq),
                             nrow=3,ncol=4, dimnames=list(c("Johnson&Johnson","Pfizer","Nasdaq"),c("Abatere standard","Coeficient de variatie","Skewness","Kurtosis")))
View(matrice_rent_preturi)

# Histograme si Boxploturi rentabilitati
par(mfrow=c(3,2))
hist(preturi$Pret_JNJ.1,col="red4",main="Rentabilitate Johnson&Johnson",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_JNJ.1,col="red4",main="Rentabilitate Johnson&Johnson",horizontal = TRUE)
hist(preturi$Pret_Pfizer.1,col="coral1",main="Rentabilitate Pfizer",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_Pfizer.1,col="coral1",main="Rentabilitate Pfizer",horizontal = TRUE)
hist(preturi$Pret_Nasdaq.1,col="darkviolet",main="Rentabilitate Nasdaq",xlab ="freceventa",ylab="pretul")
boxplot(preturi$Pret_Nasdaq.1,col="darkviolet",main="Rentabilitate Nasdaq",horizontal = TRUE)
#Interpretare:
#Distributia rentabilitatii actiunii J&J pare a fi asimetrica la dreapta, avand outlieri pe preturile mari, dar si pe cele mici.
#Distributia rentabilitatii actiunii Pfizer pare a fi asimetrica tot la stanga, avand si ea outlieri pe preturile mari si mici.
#Distributia rentabilitatii indicelui Nasdaq prezinta aproape o simetrie, cu outlieri pe preturile mari.

# matricea de corelatie

matrice_rent_corelatie_preturi<-cor(preturi[5:7])
View(matrice_corelatie_preturi)
corrplot(matrice_rent_corelatie_preturi,method ="number")
#Interpretare:
# Nu observam corelatii foarte puternice intre cineva.







hist(preturi$Pret_JNJ.1 ,col="red4",border = "white",xlab ="Rentabilitati",main="Histograma
     rentabilitatilor J&J si Nasdaq" )
hist(preturi$Pret_Nasdaq.1,add=TRUE,col="black",border = "white")
legend("topright",legend=c("Rentabilitati J&J","Rentabilitati Nasdaq"),fill=c("red4","black"))


linie_min_JNJ<-which.min(preturi$Pret_JNJ)
preturi[linie_min_JNJ,]
linie_max_JNJ<-which.max(preturi$Pret_JNJ)
preturi[linie_max_JNJ,]
# Pretul maxim pentru J&J s-a inregistrat in ziua 06/01/2023, iar cel minim in ziua 27/10/2023

# 06/01/2023: 
# Johnson & Johnson a ajuns la o înțelegere provizorie pentru a rezolva o investigație a mai mult 
# de 40 de state cu privire la afirmațiile conform cărora compania a indus în eroare pacienții cu 
# privire la siguranța pudrei sale cu talc pentru copii și a altor produse pe bază de talc

# 27/10/2023:
# Alte 300 de cazuri noi au fost adăugate la MDL cu pulbere de talc în ultima lună, ridicând numărul
# total de cazuri în așteptare la 53.311.În cele 10 săptămâni de la ridicarea stării de faliment, 
# peste 6.000 de noi cazuri de talc au fost adăugate la MDL.

linie_min_Pfizer<-which.min(preturi$Pret_Pfizer)
preturi[linie_min_Pfizer,]
linie_max_Pfizer<-which.max(preturi$Pret_Pfizer)
preturi[linie_max_Pfizer,]
# Pretul maxim pentru Pfizer s-a inregistrat in ziua 03/01/2023, iar cel minim in ziua 14/12/2023

# 03/01/2023
# Pfizer anunta o modalitate de a evalua rapid capacitatea unui vaccin existent de a induce anticorpi care neutralizează
# o variantă de virus nou identificată. Apoi fac disponibile aceste date prin reviste științifice evaluate de colegi și 
# le folosesc ca unul din pașii pentru a determina dacă este necesară o actualizare a vaccinului.
# 14/12/2023
# Compania retrage unele dintre programele sale de cercetare și concediază lucrători după ce a supraestimat
# vânzările de produse pandemice

linie_min_Nasdaq<-which.min(preturi$Pret_Nasdaq)
preturi[linie_min_Nasdaq,]
linie_max_Nasdaq<-which.max(preturi$Pret_Nasdaq)
preturi[linie_max_Nasdaq,]
# Pretul maxim al indicelui s-a inregistrat in ziua 27/12/2023 ,iar cel minim in ziua 05/01/2023

pret_min_jnj<-min(preturi$Pret_JNJ)
pret_min_nasdaq<-min(preturi$Pret_Nasdaq)
pret_max_jnj<-max(preturi$Pret_JNJ)
pret_max_nasdaq<-max(preturi$Pret_Nasdaq)



par(mfrow=c(3,1))
plot(preturi$Pret_JNJ,type="l",main="Tendinta preturilor Johnson&Johnson",col="darkred")
plot(preturi$Pret_Pfizer,type="l",main="Tendinta preturilor Pfizer",col="darkblue")
plot(preturi$Pret_Nasdaq,type="l",main="Tendinta preturilor Indicelui", col="green")
# Prin comparatie, observam ca firmele au avut preturile maxime la inceputul anului(perioada
# mai apropiata de pandemie), iar spre sfarsitul anului putem vedea preturile minime. In cazul Pfizer 
# observam o tendinta clara de scadere, fata de Johnson&Johnson care are anumite fluctuatii pe parcursul
# anului. 
# Unul dintre motive poate fi si aria pe care se axeaza cele doua companii: Johnson&Johnson este un 
# healthcare conglomerat, iar Pfizer este usor mai specializat pe industria farmaceutica.
# Un alt motiv pentru care Johnson&Johnson a avut un succes mai mare poate fi si faptul ca pentru vaccinul
# sau era nevoie de o singura doza, spre deosebire de Pfizer la care era nevoie de 2 doze. 
# Putem observa din cele 3 grafice ca cele doua actiuni, Pfizer si Johnson&Johnson nu urmeaza trend-ul
# pietei. Graficul indicelui are o tendinta crescatoare spre sfarsitul anului, spre deosebire de 
# cele doua actiuni care au o tendinta usor descrescatoare spre sfarsitul anului.
# Media preturilor J&J pe anul 2023 a fost de 156 dolari, iar media celor Pfizer de aproximativ 35,5 dolari.


boxplot.stats(preturi$Pret_JNJ)
# Avem 2 outlieri pe preturile mari: 173.51 si 173.62
outlier1JNJ<-which(preturi$Pret_JNJ==173.51)
outlier1JNJ
preturi[2,1:2]
# Primul outlier s-a inregistrat pe data de 04/01/2023
outlier2JNJ<-which(preturi$Pret_JNJ==173.62)
outlier2JNJ
preturi[4,1:2]
# Al doilea outlier s-a inregistrat pe data de 06/01/2023



boxplot.stats(preturi$Pret_Pfizer)
# Avem 2 outlieri pe preturile mari: 48.31 si 47.99

outlier1Pfizer<-which(preturi$Pret_Pfizer==48.31)
outlier1Pfizer
preturi[1,c(1,3)]
# Primul outlier s-a inregistrat pe data de 03/01/2023

outlier2Pfizer<-which(preturi$Pret_Pfizer==47.98777)
outlier2Pfizer
preturi[4,c(1,3)]
# Al doilea outlier s-a inregistrat pe data de 06/01/2023


boxplot.stats(preturi$Pret_Nasdaq)
# Nu avem outlieri
# 06/01/2023: 
# Johnson & Johnson a ajuns la o înțelegere provizorie pentru a rezolva o investigație a mai mult 
# de 40 de state cu privire la afirmațiile conform cărora compania a indus în eroare pacienții cu 
# privire la siguranța pudrei sale cu talc pentru copii și a altor produse pe bază de talc

# 27/10/2023:
# Alte 300 de cazuri noi au fost adăugate la MDL cu pulbere de talc în ultima lună, ridicând numărul
# total de cazuri în așteptare la 53.311.În cele 10 săptămâni de la ridicarea stării de faliment, 
# peste 6.000 de noi cazuri de talc au fost adăugate la MDL.
# 03/01/2023
# Pfizer anunta o modalitate de a evalua rapid capacitatea unui vaccin existent de a induce anticorpi care neutralizează
# o variantă de virus nou identificată. Apoi fac disponibile aceste date prin reviste științifice evaluate de colegi și 
# le folosesc ca unul din pașii pentru a determina dacă este necesară o actualizare a vaccinului.
# 14/12/2023
# Compania retrage unele dintre programele sale de cercetare și concediază lucrători după ce a supraestimat
# vânzările de produse pandemice
# Dupa parerea mea, eu as alege sa investesc in Johnson&Johnson. 
# In primul rand, consider ca este o perioada buna pentru orice companie din healthcare. Pandemia peste care
# am trecut mai mult sau mai putin a reprezentat o perioada infloritoare pentru domeniul sanatatii. In urma
# acestei perioade, oamenilor li s-a sporit interesul si atentia vizavi de sanatate si sigur ar avea numai
# de castigat oamenii care investesc in acest sector, implicit si in Johnson&Johnson care reprezinta 
# unii dintre cei mai puternici pioni.
# J&J reprezinta un conglomerat de sanatate, operand pe mai multe sectoare, nu doar pe cel al vaccinurilor.
# Acesta produce produse farmaceutice, echipamente medicale, produse pentru consumatori, deci
# are un orizont mai larg decat Pfizer care se axeaza pe farmacie.
# In al doilea rand, J&J are o istorie cunoscuta despre inovatiile sale continue de-a lungul timpului, ca
# de exemplu inceperea vanzarilor acelor hipodermice la inceputul anilor 1900. Acestia au mai inventat
# si sapunul pentru bebelusi in anul 1916. Datorita istoriei sale indelungate, sunt sigura ca conglomeratul
# Johnson&Johnson va mai aduce multe inovatii pe piata.
# In concluzie, as investi in Johnson&Johnson datorita pozitiei sale pe piata, dar si datorita produselor
# si serviciilor pe care le-a facut, le face sau le va face.
# Al doilea outlier s-a inregistrat pe data de 06/01/2023
# => J&J a vorbit despre Kenvue Inc, o companie americană 
# de sănătate a consumatorilor. Fosta divizie Consumer Healthcare a Johnson & Johnson
# Al doilea outlier s-a inregistrat pe data de 06/01/2023
# => Centrul de Dezvoltare a Vaccinului de la Universitatea Saint Louis participă 
# la un studiu clinic pentru un vaccin experimental dezvoltat împotriva gripei, 
# dezvoltat de Pfizer, care finanțează această cercetare.