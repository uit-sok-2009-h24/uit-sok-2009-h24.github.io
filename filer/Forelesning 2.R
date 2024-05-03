#################################################################
######## Forelesning 2 Introduction to statistics ###############
#################################################################

##### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(mosaic))


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


##### Laster data #####
# Henter dataen fra nett
# Life expetancy datasettet
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/life_exp_raw.r?raw=true"))
# Student sporreundersokelsen
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )
# Hent selgere
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/seller_1.r?raw=true"))

##### Deskriptiv statestikk ##### 


##### Sannsynlighet #####

set.seed(1337)

# ruller en 6'er terning 10 gang
roll <- sample(1:6, 10, replace =TRUE) 
df <-  data.frame( roll)
ggplot(data = df ,aes(roll)) + geom_bar()
# Ikke helt pent fordelt

# ruller en 6'er terning 10'000 gang
roll <- sample(1:6, 10000, replace =TRUE)
df <-  data.frame( roll)
ggplot(data = df ,aes(roll)) + geom_bar()
# når vi ruller 10'000 så ser vi at det er mye gjevner


# Oppgi resultatene i frekvenser
ggplot(data = df ,aes(roll)) + geom_bar(aes( y = (..count..)/sum(..count..))) +
  scale_x_continuous("Øyner på terningen") +
  scale_y_continuous("Frekvens") +
  ylim(0,0.25)


##### forventnig av en terning og store talls lov #####

set.seed(1337)
# antall kast
n <- 20 

roll <- sample(1:6, n, replace =TRUE)
gjennomsnitt <- cumsum(roll)/seq(1,length(roll),by=1)
plot(gjennomsnitt, type= "l", ylim=c(1,6), main = "Gjennomsnitt av en terning kastet 20 gang")
abline(3.5,0,col="red")

# antall kast
n <- 1000 

roll <- sample(1:6, n, replace =TRUE)
gjennomsnitt <- cumsum(roll)/seq(1,length(roll),by=1)
plot(gjennomsnitt, type= "l", ylim=c(1,6), main = "Gjennomsnitt av en terning kastet mange gang")
abline(3.5,0,col="red")
# ser at når vi ruller en terning nok antall ganger blir gjennomsnittet veldig nært forventningsverdien




# Mynt hvor en side teller 1 og ander 10000
n <- 1000
roll <- sample(c(1,10000), n, replace =TRUE)
gjennomsnitt <- cumsum(roll)/seq(1,length(roll),by=1)
plot(gjennomsnitt, type= "l", ylim=c(1,10000), main = "Gjennomsnitt av en mynt som teller 1 eller 10000 kastet mange gang")


# Sette oppe en gruppe pa fire salgspersoner
selgere <- c("Anita", "Bernt","Camilla","David")

# setter seed slik at alle får likt utfall
set.seed(1) 

# Trekker en tilfellig fra gruppen
selgere %>% 
  sample(2, replace = TRUE)

selgere %>% 
  sample(2, replace = TRUE)


##### Binomisk Fordeling #####
library(mosaic)


# Kaster tre mynter med 50/50 sjanse for kron eller mynt. 
n = 3     # forsok
p =  0.5   # sannsynlighet

# Hva er sannsynligheten for å få 3 mynt?
xpbinom(2, size = n, prob = p, col="black")

# dbinom gir punkt sannsylighet
dbinom(1, size = n, prob = p)

# pbinom gir kumulativ sannsylighet
pbinom(1, size = n, prob = p)

# Hva er sannsynligheten for å få 3 mynt?
xpbinom(40, size = 100, prob = p, col="black")


# Sannsyligheten for at 10 eller mindre studenter stryker på eksamen
# Sannsyligheten for stryk 10%
# Antall studenter 60
xpbinom(10, size = 60, prob = 0.1, col="black")

# Poisson fordeling

# Eksempel forelesning
12/60*5

dpois(0, lambda=1) # punkt sannsynlighet

xppois(0, lambda=1) # Kumullativ sannsynlighet

# Forvetet passienter per time
xppois(5, lambda=12) # Kumullativ sannsynlighet

###### Normalforeleling #####
# Symmetrisk om gjennomsnitt
# 50% av fordelingen litter under gjennomsnittet og 50% over
# P(X<0) når my = 0 og sd =1
xpnorm(1, mean = 0, sd = 1) 


# Finn P(X < 0.5) for nomralfordlingennår my = 0 og sd =1
xpnorm(0.5, mean = 0, sd = 1) 

# Finn P(X > 0.5) for nomralfordlingennår my = 0 og sd =1
1-pnorm(0.5, mean = 0, sd = 1) 
xpnorm(0.5, mean = 0, sd = 1, lower.tail=FALSE) 

# Finn P(X < -0.5) for nomralfordlingennår my = 0 og sd =1
xpnorm(-0.5, mean = 0, sd = 1) 

# Finn P(-0.5 < X < 0.5) for nomralfordlingennår my = 0 og sd =1
xpnorm(c(-0.5,0.5), mean = 0, sd = 1) 
pnorm(0.5, mean = 0, sd = 1) - pnorm(-0.5, mean = 0, sd = 1)



# For studentene i spørre undersøkelsen
mean(spore_under$Kroppshoyde)
sd(spore_under$Kroppshoyde)

# Hvis vi pluker ut en tilfelig student fra denne gruppen. Hva er sannsyligheten for at hen er under 186?
xpnorm(186, mean = 175, sd = 11) 

# Hva er sannsyligheten for at studenten ligger mellom 164 og 186?
xpnorm(c(164,186), mean = 175, sd= 11)



##### Correlasjon #### 


# scatter plot over vekt og høyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt)) +
  # lager scaterplott
  geom_point() +
  # dette legger til en trendlinje
  geom_smooth(method=lm)

# Cor gir oss korrelasjonen mellom to variabler, hvor beveger de seg sammen eller motsatt av hverandre
cor(spore_under$Kroppshoyde, spore_under$Vekt)

# Lagt til farge for kjønn 
ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_point() 

# gjore punktene litt større
ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_point(size=2) 

# Vi kan får dottene til a bevege seg litt tilfeldig for a unnga at flere punkter ligger oppa hverandre 
ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_jitter(size=2)



