#################################################################
######## Forelesning 5 Hypothesis Testing in R del 2 ############
#################################################################

##### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(infer))
suppressPackageStartupMessages(library(HH))

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Hent inn late shipments dataen
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/late_shipments.Rdata?raw=true"))

# Henter inn sporre undersokelse
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )

colnames(spore_under) <- make.unique(c("Nummer","Alder","Kjonn","Vekt","Kroppshoyde","Jobb","Jobb_timer","Studentforening","Sport","Sport_timer","Fest","Fest_n_byen","Fest_n_byen","Alkoholkonsum","Studieprogram", "Karakter","UiT","Sporsmal_forelesning","Forventet_karakter","Studietimer","Seminar_metode","Tidsfrist","Arbeidsmengde","Samarbeidsproblem" ) )

##### T-test #####

#### En gruppe mot spesifikk verdi 
# En spesifisert test med confedens intervall og alternative hypotese
# Er studentenes gjennomsnits høyde forskjellig fra 172?
Test <- t.test(spore_under$Kroppshoyde, # Data
               mu=172,                  # Null hypotese
               conf.level=0.95 ,        # Konfidens intterval
               alternativ= "greater" ) # alternativst hypotese: "two.sided", "less", "greater"
Test
Test$p.value
# Vi kan plotte resultatene om vi ønsker 
NTplot(Test)


#### To grupper mot hverandre
# En spesifisert test med confedens intervall og alternative hypotese
# Er det forskjell i høyde mellom menn og kvinner?
Test <- t.test(Kroppshoyde ~ Kjonn,      # collone ~ Kategori
               data = spore_under,       # Datafram
               conf.level=0.95 ,         # Konfidens intterval
               alternativ= "two.side" )  # alternativst hypotese: "two.sided", "less", "greater"
Test
Test$p.value
# Vi kan plotte resultatene om vi ønsker 
NTplot(Test)

rm(Test)

##### Anova #####


##### Sammen heng mellom kostand og shipping metode #####

# Vi gjør det sammen for fright cost og transport type
late_shipments <- late_shipments %>% 
  filter(shipment_mode != "N/A")

# Er det forskjell mellom prise og shipping metode?
ggplot(late_shipments, aes(x=shipment_mode, y = freight_cost_usd)) + 
  geom_boxplot() +
  xlab("Shipping metode") +
  ylab("Shipping kostnad") +
  ggtitle("Forskjelle mellom shipping metode og kostnad")


# Kjører en anova test, er det to grupper som er significant forskjellige? 
# til venstre er den numeriske variablene og til høyre den kategoriske
Regresjon <- lm(  freight_cost_usd ~ shipment_mode, data = late_shipments )
anova(Regresjon)
anova(Regresjon)[5]

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
# Veldig mange selskap!

# la oss filtrere vekk alle selvskap med mindre enn 10 observasjoner
# noen grupper har for få observasjoner så da funker ikke denne koden
late_shipments <- late_shipments %>% group_by(vendor) %>% filter(n() >= 10)

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot() 

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Shipping kostnad") +
  ylab("Selskap")

# Kutter ned alle selskap navn til de 5 første bokstavene 
late_shipments$vendor <- strtrim(late_shipments$vendor, 5)

# Er det forskjellige costnad mellom selvskapene?
ggplot(late_shipments, aes(x=vendor, y = freight_cost_usd)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Shipping kostnad") +
  ylab("Selskap") +
  ggtitle("Forskjelle mellom selskap  og kostnad")

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




# Tulle eksempel på p-hacking 
set.seed(8)
terning <-sample(1:6, size =100, replace = TRUE) 
Tull <-    tibble (terning)
Tull$kategori <- sample(letters[seq( from = 1, to = 25 )], size =100, replace = TRUE)

Reg <- lm(terning ~ kategori, data = Tull)
anova(Reg)

rm(Reg)

Tull <- Tull %>% group_by(kategori) %>% filter(n() >= 4)
pairwise.t.test(Tull$terning,
                Tull$kategori, 
                p.adjust.method= "none"  )

rm(Tull)
rm(terning)
##### Proposjons test #####


# Ensidg test
# Er det mindre en 30% av studetene som er medlem i en student forening? 
spore_under %>%
  summarise(mean(Studentforening=="Ja"))

prop_test(spore_under, 
          Studentforening ~ NULL,
          p=0.3,
          alternative = "less")

# Er det forskjell mellom kjønnene om hvem som deltar i student foreninger? 
ggplot(spore_under, aes(x = Studentforening, fill= Kjonn )) +
         geom_bar()

spore_under %>%
  group_by(Kjonn) %>%
  summarise(mean(Studentforening=="Ja"))

prop_test(spore_under,
    Studentforening ~ Kjonn,
    order = c("Kvinne", "Mann"),
            success = "Ja",
            alternative = "two.side",
            correct = FALSE
            )


#### Chi kvadrat test #### 

# ## Kji-kvadrattest
# ### Uavhengighetstest

# matrix( data inn i vektor form, antall rader, antall kolloner, byrow=TRUE gj?r at vi fyller inn en ras s? neste )
tabell <- matrix(c(456, 382,485,460,96,138), nrow=3, ncol=2, byrow=TRUE )
tabell
rownames(tabell) <- c("Sosialsitisk", "Borgelig", "Stemte ikke")
colnames(tabell) <- c("Menn", "Kvinner")
tabell


chisq.test(tabell)

chisq.test(tabell, correct=FALSE)

# lagrer kji-kvadrat testen
Xtest <- chisq.test(tabell, correct=FALSE)
Xtest
Xtest$expected 


Table <- table(late_shipments$late , late_shipments$shipment_mode)

chisq.test(Table)


# Bruker infer til å se om det er forskjell mellom shipment metdoe og om forsendingen er sen

table(late_shipments$late , late_shipments$shipment_mode)

Xtest <- late_shipments %>% 
  chisq_test(late ~ shipment_mode )
Xtest 

#### Ikke parametriske tester ####


SkoA <- c(172,173,109,121,137,098,145)/100
SkoB <- c(148,111,115,119,122,110,112)/100
Sko <- data.frame(SkoA,SkoB)

# Parret test
wilcox.test(SkoA, SkoB, paired = TRUE)

# Parret test ikke parret
wilcox.test(SkoA, SkoB)

# Kruskal-Wallis test
kruskal.test( late ~ shipment_mode,
              data = late_shipments)



