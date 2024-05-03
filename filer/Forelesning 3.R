#################################################################
######## Forelesning 3 Sampling in R ############################
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
suppressPackageStartupMessages(library(fst))

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


##### Laster data #####
# Henter dataen fra nett
# Student sporreundersokelsen
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spotify_2000_2020.r?raw=true"))

View(spotify)

# From previous step
spotify_sample <- spotify_population %>% 
  slice_sample(n = 1000)

# Calculate the mean duration in mins from spotify_population
mean_dur_pop <- spotify_population %>% summarize(mean(duration_minutes))

# Calculate the mean duration in mins from spotify_sample
mean_dur_samp <- spotify_sample %>% summarize(mean(duration_minutes))

# See the results
mean_dur_pop
mean_dur_samp

# Her ser vi at det ikke er mye forskjell i lengde på gjennomsnitt i sanger når det er et 
# utvalg på 1000 samenliknet med populasjonen

rm(mean_dur_pop)
rm(mean_dur_samp)
rm(spotify_sample)

# Randomisering eller stratifisering? 

sporre_temp <- spore_under %>%
  slice_sample(n=10) %>%
  summarise(mean_hoyde = mean(Kroppshoyde), andel_menn = mean(Kjonn=="Mann"))

for(i in 1:1000){
  sporre_temp <- rbind(sporre_temp, spore_under %>% slice_sample(n=10) %>% summarise(mean_hoyde = mean(Kroppshoyde), andel_menn = mean(Kjonn=="Mann")))
}

spore_under %>% summarise(Gjennomsnitts_hoyde = mean(Kroppshoyde), Standardavvik_hoyde = sd(Kroppshoyde))
sporre_temp %>% summarise(Gjennomsnitts_hoyde= mean(mean_hoyde), Standardavvik_gennomsnitts_hoyde=sd(mean_hoyde))


ggplot(sporre_temp, aes(x=mean_hoyde, y=andel_menn)) + geom_point()
ggplot(sporre_temp, aes(x=mean_hoyde, y=andel_menn)) + 
  geom_jitter(width=0.005, height=0.005) +
  geom_vline(xintercept = 175) +
  scale_x_continuous("Gjennomsnitt høyde målt") +
  scale_y_continuous("Andel menn i utvalget") +
  ggtitle("Gjennomnitts høyden av 10 tilfeldige studenter replikert 1000 ganger og fordeling av kjønn")

ggplot(sporre_temp, aes(x=mean_hoyde))+
  geom_histogram()+
  xlim(limits=c(160,190)) +  
  scale_x_continuous("Gjennomsnitts Hoyde", limits = c(165,185)) +
  scale_y_continuous("Antall") +
  ggtitle("Gjennomsnitts høyden av 10 tilfeldige hentet tilfeldig fra datasett med 1000 replikasjoner") +
  geom_vline(xintercept = 175) +
  geom_vline(xintercept = 178) +
  geom_vline(xintercept = 172) 


sporre_temp <- spore_under %>% group_by(Kjonn) %>%
  slice_sample(n=5) %>% 
  ungroup() %>% 
  summarise(mean_hoyde = mean(Kroppshoyde), andel_menn = mean(Kjonn=="Mann"))



for(i in 1:1000){
  sporre_temp <- rbind(sporre_temp, spore_under %>% group_by(Kjonn) %>% slice_sample(n=5) %>% ungroup() %>% 
                         summarise(mean_hoyde = mean(Kroppshoyde), andel_menn = mean(Kjonn=="Mann")))
}

ggplot(sporre_temp, aes(x=mean_hoyde))+
  geom_histogram()+
  scale_x_continuous("Gjennomsnitts Hoyde",limits = c(165,185)) +
  scale_y_continuous("Antall") +
  ggtitle("Gjennomsnitts høyden av 5 men og 5 kvinner tilfeldige hentet tilfeldig fra datasett med 1000 replikasjoner") +
  geom_vline(xintercept = 175) +
  geom_vline(xintercept = 178) +
  geom_vline(xintercept = 172) 

spore_under %>% 
  summarise(Gjennomsnitts_hoyde = mean(Kroppshoyde), Standardavvik_hoyde = sd(Kroppshoyde))
sporre_temp %>% 
  summarise(Gjennomsnitts_hoyde= mean(mean_hoyde), Standardavvik_gennomsnitts_hoyde=sd(mean_hoyde))


##### Normal fordeling #####

###

# Hvor avgrenes nedreste 5%
quantile(sporre_temp$mean_hoyde, 0.05)
# hvor avgrenes øverste 5%
quantile(sporre_temp$mean_hoyde, 0.95)


# Hvis vi pluker ut en tilfelig student fra denne gruppen. Hva er sannsyligheten for at hen er under 186?
xpnorm(193, mean = 175, sd = 11) 

# men hvilken høyde er det 95 % av studentene ligger under? 
# da må vi bruke xqnorm
xqnorm(0.95, mean=175, sd=11)


# tilbake normal foreling
xqnorm(c(0.05, 0.95), mean = 175, sd=3.09)



##### Bootstrapping ##### 

dice <- expand_grid(
  dice1 =1:6,
  dice2 =1:6
) %>%
  mutate(sumdice = dice1+dice2)

ggplot(dice, aes(sumdice)) + geom_bar()






# deskriptiv statistikk av student høyde
summary(spore_under$Kroppshoyde)

# Histogra over høyde
ggplot(spore_under, aes(x=Kroppshoyde)) +
  geom_histogram(bins=10) +
  facet_wrap(~Kjonn)


# dessity plott over høyde
ggplot(spore_under, aes(x=Kroppshoyde)) +
  geom_density()

# density plot delt for kjønn
ggplot(spore_under, aes(x = Kroppshoyde, fill = Kjonn)) +
  geom_density(alpha = .3)


# box plot over høyde
ggplot(spore_under, aes(x=Kroppshoyde)) +
  geom_boxplot()

# box plot over høyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Kjonn)) +
  geom_boxplot()

# box plot over vekt og høyde
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt)) +
  geom_point() +
  geom_smooth(method=lm) + 
  scale_x_continuous("Høyde i cm") +
  scale_y_continuous("Vekt i kg")

ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt)) +
  geom_point() +
  geom_smooth(method=lm) + 
  scale_x_continuous("Høyde i cm") +
  scale_y_continuous("Vekt i kg") +
  coord_trans(x = "log10", y = "log10")


# 
cor(spore_under$Kroppshoyde, spore_under$Vekt)



