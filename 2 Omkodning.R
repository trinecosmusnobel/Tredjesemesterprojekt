#Source af datasæt og datarens
source("1 Datarens.R")

#Omkodning af mors uddannelse
dataset$mor_udd         <- recode(dataset$mor_udd, 
                                  `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "C - MVU",
                                  `Erhvervsuddannelse (fx snedker eller fris¯r)` = "B - KVU_EVU",
                                  `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "D - LVU",
                                  `Ph.d. eller forskeruddannelse` = "E - PHD",
                                  `Grundskole` = "A - GYM_GRUND",
                                  `Gymnasial uddannelse` = "A - GYM_GRUND",
                                  `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "B - KVU_EVU")

#Omkodning af fars uddannelse
dataset$far_udd         <- recode(dataset$far_udd, 
                                  `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "C - MVU",
                                  `Erhvervsuddannelse (fx snedker eller fris¯r)` = "B - KVU_EVU",
                                  `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "D - LVU",
                                  `Ph.d. eller forskeruddannelse` = "E - PHD",
                                  `Grundskole` = "A - GYM_GRUND",
                                  `Gymnasial uddannelse` = "A - GYM_GRUND",
                                  `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "B - KVU_EVU")

#Variabel for forældres højeste uddannelse pba. faktor
dataset                 <- dataset %>% 
                                  mutate(ForældresHøjesteUddannelse=pmax(mor_udd, far_udd))

#Omkodning til NA, fordi de skal bruges som supplementære variable
dataset$ForældresHøjesteUddannelse[dataset$ForældresHøjesteUddannelse == "Andet"] <- NA 
dataset$ForældresHøjesteUddannelse[dataset$ForældresHøjesteUddannelse == "Ved ikke"] <- NA 

#Omkodning af variable for sociale og faglige fællesskaber
dataset                 <- dataset %>% mutate("Fag.Pol.Fæl" = s_19_1 + s_19_2)
dataset                 <- dataset %>% mutate("Soc.Rus.Fæl" = s_19_3 + s_19_4 + s_19_5)

dataset$Fag.Pol.Fæl     <- recode(dataset$Fag.Pol.Fæl, 
                              "0" = "Nej",
                              "1" = "Ja",
                              "2" = "Ja",
                              "2" = "Ja"
)

dataset$Soc.Rus.Fæl     <- recode(dataset$Soc.Rus.Fæl, 
                              "0" = "Nej",
                              "1" = "Ja",
                              "2" = "Ja",
                              "3" = "Ja"
)

#Kompleksitetsreduktion af enighedsvariable
enigheds_variable       <- c("prof__1", "prof_kom", "uak_god_", "uak_fors",
                             "uak_f_1", "uak_komp", "udd_over", "kender_l",
                             "s_116", "s_118", "s_119", "s_120", "s_23",
                             "s_93", "s_124", "s_7", "s_8", "s_9",
                             "s_94", "s_71", "s_72", "s_73", "s_75", "s_115", 
                             "s_66", "s_67", "s_68", "s_69", "s_105", 
                             "s_111","s_11","s_13","s_38","s_112","s_121")

dataset                 <- dataset %>% 
                              mutate_at(vars(enigheds_variable), recode,`Meget enig` ='Enig',
                                        `Delvis enig` ='Enig',
                                        `Hverken enig eller uenig` ='Hverken_eller',
                                        `Delvis uenig` ='Uenig',
                                        `Meget uenig` ='Uenig', 
                                        `Har endnu ikke valgt valgfag` = "Har endnu ikke valgt valgfag")

#Omkodning af vigtighedsvariable
vigtighed               <- c("s_96",
                             "s_97",
                             "s_98",
                             "s_99",
                             "s_100",
                             "s_108")

dataset                 <- dataset %>% 
                               mutate_at(vars(vigtighed), recode,
                                         `Ikke sÊrlig vigtigt` ='Ikke_vigtigt',
                                         `Slet ikke vigtigt` ='Ikke_vigtigt',
                                         `Temmelig vigtigt` ='Temmelig_vigtigt',
                                         `Meget vigtigt` ='Meget_vigtigt')

#Omkodning af hvor-ofte variable
hvor_ofte                <- c("s_53","s_49","s_51","s_52","s_55","s_56","s_58","s_59")

dataset                  <- dataset %>% 
                                mutate_at(all_of(hvor_ofte), recode,`Aldrig eller nÊsten aldrig` ='Aldrig_sjældent',
                                          `SjÊldent` ='Aldrig_sjældent',
                                          `Sommetider` ='Sommetider',
                                          `Ofte` ='Ofte_altid',
                                          `Altid` ='Ofte_altid')

#Omkodning af køn
dataset$koen             <- recode(dataset$koen, 
                                   `ForetrÊkker selv at beskrive min k¯nsidentitet (uddyb gerne)` = "Andre kønsidentiteter",
                                   `Non-binÊr` = "Andre kønsidentiteter")

#Akademisk attitude - dataudvalg
akademisk_attitude       <- data.frame(
                            "Uddannelse skal overstås"              = dataset$udd_over, 
                            "Kommer til at bruge det man lærer"     = dataset$s_118, 
                            "Pensum generelt spændende"             = dataset$s_8,
                            "Udd. vigtig del af identitet"          = dataset$s_9,
                            "Gode karakterer vigtigt"               = dataset$s_94,
                            "God til at afkode und."                = dataset$s_93,
                            "Selvvurderet højere fagligt niveau"    = dataset$s_13
)

studiestrategi            <- data.frame(
                              "Stiller ikke spørgsmål"                = dataset$s_120, 
                              "Foretrækker praktisk underv."          = dataset$s_7,
                              "Valgfag - praksis"                     = dataset$s_72,
                              "Valgfag - arbejdsmarkedskomp."         = dataset$s_73,
                              "Valgfag - god karakter"                = dataset$s_75,
                              "Valg af udd. - prestige"               = dataset$s_66,
                              "Valg af udd. - ikke ledighed"          = dataset$s_68,
                              "Valg af udd. - god løn"                = dataset$s_69,
                              "Valg af udd. - bestemt type org."      = dataset$s_105,
                              "Møder velforberedt"                    = dataset$s_38
)

#Sociale relation - dataudvalg
sociale_relationer        <- data.frame(
                              "Sociale vigtigt i udd."                = dataset$s_116,
                              "Gruppe af medstuderende, tryg"         = dataset$s_119,
                              "Snakker m. fam. om din udd."           = dataset$s_52,
                              "Snakker m. andre om  din udd."         = dataset$s_59,
                              "Forældre forstår udd."                 = dataset$s_11
)

#Ekstrakurrikulær deltagelse - dataudvalg
ekstrakurrikulær          <- data.frame(
                              "Fag.Pol.Fæl"                           = dataset$Fag.Pol.Fæl,
                              "Soc.Rus.Fæl"                           = dataset$Soc.Rus.Fæl
)

#Sammensætning af variabeltemaer
active                    <- list(
                              "Akademisk attitude"                    = akademisk_attitude,
                              "Studiestrategi"                        = studiestrategi,
                              "Sociale relationer"                    = sociale_relationer,
                              "Ekstrakurrikulær deltagelse"           = ekstrakurrikulær
)


######SUPPLEMENTÆRE VARIABLE######

#Omkodning forældresuddannelse
dataset$ForældresHøjesteUddannelse_D <- recode(dataset$ForældresHøjesteUddannelse, 
                                               `A - GYM_GRUND` = "ikke PHD_LVU",
                                               `B - KVU_EVU` = "ikke PHD_LVU",
                                               `C - MVU` = "ikke PHD_LVU",
                                               `D - LVU` = "PHD_LVU",
                                               `E - PHD` = "PHD_LVU",
                                               Andet = "ikke PHD_LVU"
)

dataset$trives            <- dataset$s_103


dataset$trives            <- factor(dataset$trives,levels = c("Meget uenig",
                                                   "Delvis uenig",
                                                   "Hverken enig eller uenig",
                                                   "Delvis enig",
                                                   "Meget enig"))

dataset$ForældresHøjesteUddannelse <- factor(dataset$ForældresHøjesteUddannelse, levels = c("A - GYM_GRUND",
                                                                                            "B - KVU_EVU",
                                                                                            "C - MVU",
                                                                                            "D - LVU",
                                                                                            "E - PHD"))

dataset$ForældresHøjesteUddannelse    <- recode(dataset$ForældresHøjesteUddannelse, 
                                           "A - GYM_GRUND" = "Gymnasie/grundskole",
                                           "B - KVU_EVU" = "Kort videregående eller erhverv",
                                           "C - MVU" = "Mellemlang videregående",
                                           "D - LVU" = "Lang videregående",
                                           "E - PHD" = "Ph.D")






#Omkodning af adgangsgivende uddannelse
dataset$adg_udd           <- recode(dataset$adg_udd, 
                          `Professionsbachelor fra professionsh¯jskole` = "Professionsbachelor",
                          `Professionsbachelor fra erhvervsakademi` = "Erhvervsakademi",
                          `Bachelor fra et andet udenlandsk universitet` = "Udenlandsk universitet",
                          `Bachelor fra et andet dansk universitet` = "Dansk universitet",
                          `Bachelor fra Roskilde Universitet` = "RUC"
)

dataset                   <- dataset %>% rename(Bachelor = adg_udd)
dataset                   <- dataset %>% rename(Køn = koen)
dataset                   <- dataset %>% rename(Trives = trives)

#UDVALG AF SUPPLEMENTÆRE VARIABLE
sup                    <- dataset %>% select(Bachelor, 
                          ForældresHøjesteUddannelse, Køn,
                          Trives) %>% mutate_all(as_factor)





