#Datarens

source("0 Data.R")

#kun fulde besvarelser
dataset <- dataset %>% filter(stato_4 == 1) 

#skal have besvaret første spørgsmål
dataset <- dataset %>% filter(alder > 0)

#id variabel
dataset$ID <- 1:nrow(dataset)

dataset$ID
