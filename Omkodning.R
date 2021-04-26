setwd("/Users/trinenobel/Dropbox/My Mac (Trine’s MacBook Pro)/Downloads")

library(readxl)
library(dplyr)

dataset <- read_excel("complete_f.xlsx")

dataset <- dataset %>% filter(stato_4 == 1) 

dataset <- dataset %>% filter(alder > 0)

dataset$ID <- 1:nrow(dataset)

dataset$mor_udd <- recode(dataset$mor_udd, 
                          `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "MVU",
                          `Erhvervsuddannelse (fx snedker eller fris¯r)` = "KVU, EVU",
                          `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "PHD, LVU",
                          `Ph.d. eller forskeruddannelse` = "PHD, LVU",
                          `Grundskole` = "GYM/GRUND",
                          `Gymnasial uddannelse` = "GYM/GRUND",
                          `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "KVU, EVU",
                          `Andet` = "MISSING",
                          `Ved ikke` = "MISSING")

dataset$mor_udd[dataset$mor_udd=="MISSING"] <- NA 

dataset$far_udd <- recode(dataset$far_udd, 
                          `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "MVU",
                          `Erhvervsuddannelse (fx snedker eller fris¯r)` = "KVU, EVU",
                          `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "PHD, LVU",
                          `Ph.d. eller forskeruddannelse` = "PHD, LVU",
                          `Grundskole` = "GYM/GRUND",
                          `Gymnasial uddannelse` = "GYM/GRUND",
                          `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "KVU, EVU",
                          `Andet` = "MISSING",
                          `Ved ikke` = "MISSING")

dataset$far_udd[dataset$far_udd=="MISSING"] <- NA 

unique(dataset$far_udd)
unique(dataset$mor_udd)

dataset[dataset == "Ved ikke"] <- NA
dataset[dataset == "Andet"] <- NA

unique(dataset$adg_udd)

dataset$adg_udd <- recode(dataset$adg_udd, 
                          `Professionsbachelor fra professionsh¯jskole` = "Prof.bach",
                          `Professionsbachelor fra erhvervsakademi` = "Prof.bach",
                          `Bachelor fra et andet udenlandsk universitet` = "AndetUni",
                          `Bachelor fra et andet dansk universitet` = "AndetUni"
)

active <- dataset %>% arrange(ID) %>% select(mor_udd,
                                             far_udd,
                                             s_101, 
                                             s_113,
                                             adg_udd,
                                             s_116,
                                             s_7,
                                             s_9,
                                             s_18,
                                             s_43,
                                             s_52,
                                             s_59,
                                             s_13,
                                             s_121,
                                             s_110,
                                             s_96,
                                             Hvor vigtigt er hvert af de f¯lgende omrÂder i dit liv? - Uddannelse) 

sup <- dataset %>% arrange(ID) %>% select(udd_navn,
                                          koen) 

library(soc.ca)

result <- soc.mca(active, sup = NULL, 
                  identifier = NULL,
                  passive = getOption("passive", 
                                      default = "Missing"))

map.ind(result, point.size = 0.5, map.title = "Studerende på ISE")
map.ind(result, dim = c(1,2), point.size = 0.5, map.title = "Politisk holdninger i Danmark ESS")

map.active(result, label.size = 1.5)
map.active(result, label = FALSE)







