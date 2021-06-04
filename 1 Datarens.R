#Source datasæt
source("0 Data.R")

#Frasorter ufærdige besvarelse
dataset           <- dataset %>% filter(stato_4 == 1) 

#Frasorter dem, der ikke har svaret på første spørgsmål (dvs. dem der
#ikke er kandidatstuderende)
dataset           <- dataset %>% filter(alder > 0)

#Frasorter besvarelser angivet før udsendelsen (tests udfyldt af mig)
test              <- dataset %>%
                     filter(starttim >= as.Date('2021-04-22') & starttim <= as.Date('2021-04-27'))

#Frasorter to besvarelser, jeg har lavet på dagen
dataset           <- dataset %>% filter(responde != 797151748)
dataset           <- dataset %>% filter(responde != 797257240)

#Definer missing values for valgfags-spørgsmål
dataset$s_71[is.na(dataset$s_71)] <- "Har endnu ikke valgt valgfag"
dataset$s_72[is.na(dataset$s_72)] <- "Har endnu ikke valgt valgfag"
dataset$s_73[is.na(dataset$s_73)] <- "Har endnu ikke valgt valgfag"
dataset$s_75[is.na(dataset$s_75)] <- "Har endnu ikke valgt valgfag"
dataset$s_115[is.na(dataset$s_115)] <- "Har endnu ikke valgt valgfag"

#Frasorter variable med datoer osv., som jeg ikke skal bruge
dataset           <- dataset %>% select (-c(modified, 
                                  closetim, 
                                  starttim, 
                                  created, 
                                  importgr, 
                                  distribu, 
                                  email, 
                                  digit_1))

#Angiv NA som "MISSING" for at kunne bruge dem som passive kategorier
dataset[is.na(dataset)] <- "MISSING"

