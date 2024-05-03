#################################################################
######## Forelesning 6 Introduction to Regression in R ##########
#################################################################

##### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(mosaic))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(ggfortify))


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Laster nødvendig dataset
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/churn.Rdata?raw=true"))
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/taiwan_real_estate.Rdata?raw=true"))
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/ad_conversion.Rdata?raw=true"))
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )


# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) 


# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_jitter() +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) 


# Utvider aksene så vi kan se skjæringspunktet
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) +
  xlim(0,200) +
  ylim(-60,120)



# Lage en linje som passer
Lineaer_model <-lm(Vekt ~ Kroppshoyde, data = spore_under)
Lineaer_model
summary(Lineaer_model)

# hva er forventet vekt til en student som er 172 cm?
0.757*172-57.367


# vi lager en funksjon av modellen
Fun <- makeFun(Lineaer_model)
# ser på estimert vekt for høyden 172
Fun(172)

# en annen måte
predict(Lineaer_model,172)


# Predikerer høyden for 172
Hoyder <- tibble(Kroppshoyde = 172)

# en av to måter
Predikerte <- Hoyder %>%
  mutate(Vekt = Fun(Kroppshoyde))

# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) +
  geom_point(data = Predikerte , color="red", size=5)


# Predikerer høyden for alle fra 160 til 190
Hoyder <- tibble(Kroppshoyde = 160:190)
# en av to måter
Predikerte <- Hoyder %>%
  mutate(Vekt = Fun(Kroppshoyde))

predict(Lineaer_model,Predikerte)

# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) +
  geom_point(data = Predikerte, color="red", size=3) 

# Viktigste regresjons output
summary(Lineaer_model)


# Hvis vi ønsker å hente ut variabler fra modellen 
Lineaer_model$coefficients
coefficients(Lineaer_model)

# Hent ut mindre men mer viktig info
tidy(Lineaer_model)

# Matematiske oppsettet
augment(Lineaer_model)

# Kan finne den predikete verdien for hver observasjon
fitted(Lineaer_model)

# Kan finne avviket for hver observasjon
residuals(Lineaer_model)
sum(residuals(Lineaer_model))

# la oss hent ut faktisk vekt, forventet og residual
Data <-  tibble(spore_under$Kroppshoyde,  spore_under$Vekt,  fitted(Lineaer_model),  residuals(Lineaer_model))
names(Data) <- c("høyde","Vekt","Forventet vekt","Residual")
Data

##### Oppgave #####
load(url("http://ansatte.uit.no/oystein.myrland/BED2011/data/Bruktbilpris.Rdata"))

# Får oversikriftene til alle kollonene
head(BruktbilprisData)
names(BruktbilprisData)



# Ikke lineære modeller

# Lineære sammenheng mellom avstand til metore og pris
ggplot(taiwan_real_estate, aes(dist_to_mrt_m, price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Avstand til metroen")+
  ylab("Pris på bolig")

# Kvadrat root av avstanden til metroen og pris
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Kvadrat rot av avstand til metroen")+
  ylab("Pris på bolig")


# Kvadrat rooten av både avstand til metro og pris. 
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), sqrt(price_twd_msq) )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Kvadrat rot av avstand til metroen")+
  ylab("Kvadrat rot av pris på bolig")


# Regresjons modell av lineære priser og avstand til metroen
reg_model_lin <- lm(
  n_clicks ~ n_impressions ,
  data = ad_conversion )
summary(reg_model_lin)

# Kvadratroten av begge
reg_model_rot <- lm(
  I(n_clicks ^ 0.5) ~ I(n_impressions ^ 0.5),
  data = ad_conversion )
summary(reg_model_rot)


# Passer modellen dataen?

autoplot(reg_model_lin, which = 1:3)

autoplot(reg_model_rot, which = 1:3)

autoplot(Lineaer_model, 
         which = 1:3,
         nrow=3,
         ncol=1)


#####  Outlier og leverage ##### 

# Finner den største outlineren
Lineaer_model %>% 
  augment() %>% 
  arrange(desc(.cooksd)) %>% 
  head()

# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) +
  geom_point(aes(x=190,y=115), color="red", size=5)



# Finner den med mess leverage
Lineaer_model %>% 
  augment() %>% 
  arrange(desc(.hat)) %>% 
  head()

# Scatter plott av vekt og kroppshøyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE) +
  geom_point(aes(x=198,y=82), color="red", size=5)



