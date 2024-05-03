#################################################################
######## Forelesning 3 Hypothesis Testing in R ##################
#################################################################

##### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(mosaic))
suppressPackageStartupMessages(library(infer))
suppressPackageStartupMessages(library(HH))


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Hent inn late shipments dataen
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/late_shipments.Rdata?raw=true"))

# Henter inn sporre undersokelse
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )



##### Hyppotestesting eksempel med juksende søsken #####

# sannsyneligheten for å blir trukket 0 ganger 3 dager på rad
xpbinom(0, size=3 , prob=0.25, col = "Red")
# sannsyneligheten for å blir trukket 0 ganger 12 dager på rad
xpbinom(0, size=12 , prob=0.25, col = "Red")

# Sannsynlighetne for at Daniel tar oppvasken 22 ganger av 100
xpbinom(22, size=100 , prob=0.25, col = "Red")

# Sannsynlighetne for at Daniel tar oppvasken 14 ganger av 100
xpbinom(14, size=100 , prob=0.25, col = "Red")





#####  Hypotese testing av spørreundersøkelse #####
str(spore_under)

# vi ser på høyde
ggplot(spore_under, aes(x=Kroppshoyde)) +
  geom_density()

# Er studenter over 172 høy? 
set.seed(1337)
######
Hoyde <- replicate(
  # Gjør dette 1000 ganger
  n=1000,
  # Uttrykkes som kjøres
  expr = {
    # Fra spørre undersøkelsen
    spore_under %>%
      # Trekk med tilbakelegging like mange høyder som er der
      slice_sample(prop = 1, replace = TRUE) %>%
      # Regn gjennom gjennomsnittet
      summarize(Gjennomsnitt_hoyde = mean(Kroppshoyde) ) %>% 
      # Returner kun gjennomsnittet til vektoren Hoyde 
      pull(Gjennomsnitt_hoyde)
  }
)

Hoyde <- tibble(Hoyde)

# Lager et 95% konfidens intervall
conf_int_quantile <- Hoyde %>%
  summarize(lower = quantile(Hoyde, 0.025), 
            upper = quantile(Hoyde, 0.975))

# Ser på resultatet
conf_int_quantile

# Plotter alle gjennomsnittene 
Hoyde %>% ggplot(aes(x=Hoyde)) + 
  geom_histogram() 
  
# Legger til en linje for 172 
Hoyde %>% ggplot(aes(x=Hoyde)) + 
  geom_histogram() +
  geom_vline(xintercept = 172, color="Red", size=1.5)

# laveste 5% er 
Avgrenset <- Hoyde %>%
  summarize(lower = quantile(Hoyde, 0.05))

# Ser at det er under 5% sannsynlig at gjennomsnittet ligger under 173
Avgrenset


# finn gjennomsnitt og standardavviket til gjennomsnittet
Hoyde %>% 
  summarise(gj_hoyde = mean(Hoyde), sd_hoyde = sd(Hoyde))

# Regner ut P-verdi
p_verdi <- Hoyde %>%
  summarize(p_verdi = mean(Hoyde <= 172))

p_verdi

#### her forlater vi bootstraping og starter å bruke klassisk statistikk

# Regner ut fra orginal dataen gjennomsnitt av høyde, standardavvik, og standarderror
spore_under %>% 
  summarise(gj_hoyde = mean(Kroppshoyde),
            sd_hoyde = sd(Kroppshoyde),
            n(),
            se=sd(Kroppshoyde)/sqrt(n()))

#### Kan vi med sikkerhet si at studentene er høyere enn 172
# z verdien forteller oss hvor mange standardderror vi avviker fra gjennomsnittet
z_verdi = (175-172)/1.4

z_verdi
# Vår observasjon er 2.1429 antall standarderrir fra gjennomsnitt

# null hypotse er at studenten er 172 eller lavere, vi måler 175
xpnorm(175, mean=172, sd = 1.4)
# Kan også skrive inn z verdien direkte
xpnorm(z_verdi)

# Leger et konfidensintervall
xqnorm(c(0.025,0.975), mean=175, sd=1.4)


# sannsynligheten for å måle gjennomsnitt høyden av 59 studenter til 175 så er det lite sannsynlig at det sanne gjennomsnittet er 172 eller lavere.

# tosidet hypotesetest
t.test(spore_under$Kroppshoyde, mu=172, conf.level=0.95)

# En spesifisert test med confedens intervall og alternative hypotese
Test <- t.test(spore_under$Kroppshoyde,
               mu=172,
               conf.level=0.95 ,
               alternativ= "greater" )
Test
Test$statistic
# Vi kan plotte resultatene om vi ønsker 
NTplot(Test)


# Par testing 
# Sko eksempel 
SkoA <- c(172,173,109,121,137,098,145)/100
SkoB <- c(148,111,115,119,122,110,112)/100

Sko <- data.frame(SkoA,SkoB)
Sko <- Sko %>% 
  mutate(diff = SkoA - SkoB)

ggplot(Sko, aes(x=diff))+
  geom_histogram()

# Vi ser om forskjellen er forskjellig fra 0
t.test(Sko$diff, alternative = "two.side" ,
       mu=0 )

# Vi trenger ikke regne ut forskjellen, nå ser vi om de er forskjellige fra hverandre
t.test(Sko$SkoA, Sko$SkoB, paired = TRUE,
       alternative = "two.side" , mu=0 )

# Hvis vi ikke har med paired = TRUE og kjører gruppen mot hverandre så for vi forskjellige svare enn over
t.test(Sko$SkoA, Sko$SkoB,
       alternative = "two.side" , mu=0 )

# Vi trenger ikke regne ut forskjellen, nå ser vi om de er forskjellige fra hverandre
NTplot(t.test(Sko$SkoA, Sko$SkoB, paired = TRUE,
              alternative = "two.side" , mu=0 ) )


# Tester to grupper mot hverandre 

# Først la oss plotte gruppen mot hverandre
ggplot(spore_under, aes(x=Kroppshoyde, fill=Kjonn) ) +
  geom_density(alpha=0.3 )

Test <- t.test(Kroppshoyde ~ Kjonn,
               data = spore_under, mu =0)
Test
NTplot(Test)

# Boot strapping 

# Hva er gjennomsnittsforskjellen mellom kvinner og menn
spore_under %>%
  # Regn gjennom gjennomsnittet
  summarize(Gjennomsnitt_forskjell =
              sum(Kroppshoyde *(Kjonn == "Mann")) /sum(Kjonn == "Mann")  -
              sum(Kroppshoyde *(Kjonn == "Kvinne"))/sum(Kjonn == "Kvinne") ) 
# gjennomsnitts forskjell i høyde er 17 cm


set.seed(1337)
######
Hoyde_diff <- replicate(
  # Gjør dette 1000 ganger
  n=1000,
  # Uttrykkes som kjøres
  expr = {
    # Fra spørre undersøkelsen
    spore_under %>%
      # Trekk med tilbakelegging like mange høyder som er der
      slice_sample(prop = 1, replace = TRUE) %>%
      # Regn gjennom gjennomsnittet
      summarize(Gjennomsnitt_forskjell = 
                  sum(Kroppshoyde *(Kjonn == "Mann")) /sum(Kjonn == "Mann")  -
                  sum(Kroppshoyde *(Kjonn == "Kvinne"))/sum(Kjonn == "Kvinne") )  %>% 
      # Returner kun gjennomsnittet til vektoren Hoyde 
      pull(Gjennomsnitt_forskjell)
  }
)

# Gjøre dataen om dataframe
Hoyde_diff <- tibble(Hoyde_diff)

# Tegner et histogram for forskjellen 
ggplot(Hoyde_diff, aes(x=Hoyde_diff)) + 
  geom_histogram()

# Tegner et histogram for forskjellen 
ggplot(Hoyde_diff, aes(x=Hoyde_diff)) + 
  geom_histogram() +
  geom_vline(xintercept = 0, col = "Red")




# Anova


# Hent inn late shipments dataen
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/late_shipments.Rdata?raw=true"))

# Vi gjør det sammen for fright cost og transport type
late_shipments <- late_shipments %>% 
  filter(shipment_mode != "N/A")

# Er det forskjell mellom prise og shipping metode?
ggplot(late_shipments, aes(x=shipment_mode, y = freight_cost_usd)) + 
  geom_boxplot()

# Kjører en anova test, er det to grupper som er significant forskjellige? 
# til venstre er den numeriske variablene og til høyre den kategoriske
Regresjon <- lm(  freight_cost_usd ~ shipment_mode, data = late_shipments )
anova(Regresjon)
rm(Regresjon)

# Men denne testen sier KUN at det er forskjell mellom 2 grupper ikke hvilken. 

# Vi kjører en pair wise test for å se hvor 
pairwise.t.test(late_shipments$freight_cost_usd ,
                late_shipments$shipment_mode,
                p.adjust.method = "none")





##### Pair wise test mellom selvskaper #### 


# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot()

# la oss fjerne alle som har mindre en 
# noen grupper har for få observasjoner så da funker ikke denne koden
late_shipments <- late_shipments %>% group_by(vendor) %>% filter(n() >= 10)

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot() 

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot() +
  coord_flip()

late_shipments$vendor <- strtrim(late_shipments$vendor, 5)

# Kjører en anova test, er det to grupper som er significant forskjellige? 
# til venstre er den numeriske variablene og til høyre den kategoriske
Regresjon <- lm(  freight_cost_usd ~ vendor, data = late_shipments )
anova(Regresjon)
rm(Regresjon)


# kjøre en parvis test av alle gruppene

pairwise.t.test(late_shipments$freight_cost_usd,
                late_shipments$vendor , 
                alternative = "two.sided", 
                p.adjust.method= "none" )


# kjøre en parvis test av alle gruppene, men korrigerer for at vi har så mange grupper
# Ganger p-verdine med antal grupper
pairwise.t.test(late_shipments$freight_cost_usd, 
                late_shipments$vendor , 
                alternative = "two.sided", 
                p.adjust.method= "bonferroni" )


# kjøre en parvis test av alle gruppene, men korrigerer for at vi har så mange grupper
# Ganger laveste p-verdi men antall grupper, nest laveste p-verdi med grupper -1 osv
pairwise.t.test(late_shipments$freight_cost_usd, 
                late_shipments$vendor ,
                alternative = "two.sided", 
                p.adjust.method= "holm" )




#### Anova og parvis test for alle 

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor_inco_term, y = freight_cost_usd)) + 
  geom_boxplot()

# Er det forskjellige costnad mellom selvskapene? Sette type transport i forskjellige grupper
ggplot(late_shipments, aes(x=vendor_inco_term, y = freight_cost_usd)) + 
  geom_boxplot() +
  facet_wrap(~shipment_mode)

# Kjører en anova test, er det to grupper som er significant forskjellige? 
# til venstre er den numeriske variablene og til høyre den kategoriske
Regresjon <- lm(  freight_cost_usd ~ vendor_inco_term, data = late_shipments )
anova(Regresjon)
rm(Regresjon)

# kjøre en parvis test av alle gruppene
pairwise.t.test(late_shipments$pack_price,
                late_shipments$vendor_inco_term ,
                alternative = "two.sided", p.adjust.method= "none" )
# men vi har et problem

# noen grupper har for få observasjoner så da funker ikke denne koden
late_shipments <- late_shipments %>%
  group_by(vendor_inco_term) %>%
  filter(n() >= 10)

# Kjører igjen en parvis test, leg merke til at vi har 3 færre selvkaper og 3 færre observasjoner
pairwise.t.test(late_shipments$pack_price,
                late_shipments$vendor_inco_term ,
                alternative = "two.sided",
                p.adjust.method= "none" )


# Kjører igjen en parvis test, leg merke til at vi har 3 færre selvkaper og 3 færre observasjoner
pairwise.t.test(late_shipments$pack_price,
                late_shipments$vendor_inco_term ,
                alternative = "two.sided",
                p.adjust.method= "none" )




