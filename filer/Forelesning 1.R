#################################################################
######## Forelesning 1 Exploratory Data Analysis in R ###########
#################################################################

##### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readr))

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

##### Hent spørreskjema fra nett ##### 
load(url("https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/spore_under.Rdata?raw=true") )

# La oss få en oversikt over datasettet
glimpse(spore_under)
str(spore_under)

# Hvis vi ønsker å se hvilken data en collone inneholder kan vi bruke formelen unique
# bruk dplyr og summarise 
spore_under %>% summarise(Forskjellige = unique(Alder))
# eller bare unique formelen med $ for å velge colonne
unique(spore_under$Alder)
# Vi ser at det alders kategorier i dette datasettet

# Vi kan gjøre det samme for kjønn 
unique(spore_under$Kjonn)
# Spørre undersøkelsen tilot Mann, Kvinne eller Ikke binær, men siste gruppe var ikke valgt.

# For å sammenlikne lager vi et barplot
# Vi bruker ggplot, skriver inn data kilden og aes
ggplot(spore_under, aes(x=Kjonn) ) +
  # Bruker geom_bar() for å gjøre det til et barplot
  geom_bar()
# Det er ganske god fordelig av kjønn i gruppen 

# Vi ser på fordeling i alder også
ggplot(spore_under, aes(x=Alder)) +
  geom_bar()
# Som forventet de fleste studentene er i gruppen 21-25



# Jobber studenten vedsiden av studiene

# Når vi ser på kategoriske variabler er det greit å bruke stolpe / søyle diagram
ggplot(spore_under, aes(x = `Jobber du ved siden av studiene?` )) + 
  geom_bar()

# Vi kan også bruke dot plott.... 
ggplot(spore_under, aes(x = `Jobber du ved siden av studiene?` )) + 
  geom_dotplot()

# Vi kan også se om noen studenter er med i studentforeninger
ggplot(spore_under, aes(x = `Er du med i noen studentforening?`  )) + 
  geom_bar()

# Er det da noe sammenheng mellom deltakeles på studentforeninger og jobb?
ggplot(spore_under, aes(x = `Er du med i noen studentforening?`, fill = `Jobber du ved siden av studiene?` )) + 
  geom_bar()

# Vi kan også sette stolpene ved siden av hverandre 
ggplot(spore_under, aes(x = `Er du med i noen studentforening?`, fill = `Jobber du ved siden av studiene?` )) + 
  geom_bar(position = "dodge")


# Hvilken karakter har studentene fått så langt
ggplot(spore_under, aes(x=`Hvilken gjennomsnittskarakter har du sa langt i studiet?` )) +
  geom_bar() 

# Hvor fornøyd er studenten? 
ggplot(spore_under, aes(x=`Hva synes du om UiT?` )) +
  geom_bar() 


# Lager et boxplot
ggplot(spore_under, aes(x= `Hva synes du om UiT?` ) ) +
  geom_boxplot()

ggplot(spore_under, aes(x= `Hva synes du om UiT?` , y = `Kjonn` ) ) +
  geom_boxplot()

# Her kan også et density plott være bra til å viste forskjellene
ggplot(spore_under, aes(x= `Hva synes du om UiT?` , fill = `Kjonn` ) ) +
  geom_density()

ggplot(spore_under, aes(x= `Hva synes du om UiT?` , fill = `Kjonn` ) ) +
  geom_density(alpha = .3)



# Sammenheng mellom karakterer og fornøydhet?
ggplot(spore_under, aes(x=`Hvilken gjennomsnittskarakter har du sa langt i studiet?` )) +
  geom_bar() +
  facet_wrap(~`Hva synes du om UiT?`)

ggplot(spore_under, aes(x=`Hva synes du om UiT?` )) +
  geom_bar() +
  facet_wrap(~`Hvilken gjennomsnittskarakter har du sa langt i studiet?`)


table(spore_under$`Hvilken gjennomsnittskarakter har du sa langt i studiet?`, spore_under$`Hva synes du om UiT?`)

# La oss se på hoyde, vekt og kjonn, to numeriske variabler og en kategorisk
mean(spore_under$Kroppshoyde)

spore_under %>% group_by(Kjonn) %>% summarise(mean(Kroppshoyde), mean(Vekt))

ggplot(spore_under, aes(x=Kroppshoyde) ) +
  geom_histogram()

# 
ggplot(spore_under, aes(x=Kroppshoyde) ) +
  geom_histogram()

ggplot(spore_under, aes(x=Kroppshoyde, fill=Kjonn) ) +
  geom_density(alpha=0.3 )

ggplot(spore_under, aes(x=Vekt, fill=Kjonn) ) +
  geom_density(alpha=0.3 )

# Er det sammenheng mellom hoyde og vekt
ggplot(spore_under, aes(x=Kroppshoyde, y=Vekt) ) +
  geom_point()


ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt))+ 
  geom_point() + 
  facet_wrap(~Kjonn) +
  geom_smooth(method=lm)

ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_point() 

ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_point(size=2) 

# Vi kan får dottene til a bevege seg litt tilfeldig for a unnga at flere punkter ligger oppa hverandre 
ggplot(spore_under, aes(x=Kroppshoyde,y=Vekt, colour=Kjonn))+ 
  geom_jitter(size=2)


# Create overlaid density plots for same data
ggplot(spore_under, aes(x = Kroppshoyde, fill = Kjonn)) +
  geom_density(alpha = .3)

# Create box plots of city
# Mitten er medianen. 
# Første og tredje kvartilen
ggplot(spore_under, aes(y = Vekt, x = Kjonn)) +
  geom_boxplot()


# 
ggplot(spore_under, aes(x = Vekt, fill = Kjonn)) +
  geom_density(alpha = .3)



##### Hoyde eksempel  #####
A <- c(167, 165, 163)
B <- c(186, 145, 164) 

sd(A)
sd(B)

var(A)

mean(abs((A - mean(A))))
mean(abs((B - mean(B))))

sd(spore_under$Kroppshoyde)
mean( abs( spore_under$Kroppshoyde - mean(spore_under$Kroppshoyde )  )  )


summary(spore_under$Kroppshoyde)
IQR(spore_under$Kroppshoyde)


##### 
ggplot(spore_under, aes(x = Kroppshoyde)) +
  geom_density()

