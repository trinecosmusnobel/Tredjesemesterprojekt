#Omkodning

source("1 Datarens.R")

#mors uddannelse
dataset$mor_udd <- recode(dataset$mor_udd, 
                          `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "MVU",
                          `Erhvervsuddannelse (fx snedker eller fris¯r)` = "KVU, EVU",
                          `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "PHD, LVU",
                          `Ph.d. eller forskeruddannelse` = "PHD, LVU",
                          `Grundskole` = "GYM/GRUND",
                          `Gymnasial uddannelse` = "GYM/GRUND",
                          `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "KVU, EVU")

#fars uddannelse
dataset$far_udd <- recode(dataset$far_udd, 
                          `Mellemlang videregÂende uddannelse (fx sygeplejerske eller diplomingeni¯r)` = "MVU",
                          `Erhvervsuddannelse (fx snedker eller fris¯r)` = "KVU, EVU",
                          `Lang videregÂende uddannelse (fx kandidatuddannelse eller officer)` = "PHD, LVU",
                          `Ph.d. eller forskeruddannelse` = "PHD, LVU",
                          `Grundskole` = "GYM/GRUND",
                          `Gymnasial uddannelse` = "GYM/GRUND",
                          `Kort videregÂende uddannelse (fx laborant eller tandplejer)` = "KVU, EVU")

#adgangsgivende uddannelse
dataset$adg_udd <- recode(dataset$adg_udd, 
                          `Professionsbachelor fra professionsh¯jskole` = "Prof.bach",
                          `Professionsbachelor fra erhvervsakademi` = "Prof.bach",
                          `Bachelor fra et andet udenlandsk universitet` = "AndetUni",
                          `Bachelor fra et andet dansk universitet` = "AndetUni"
)

dataset$institut <- recode(dataset$udd_navn, 
  `Virksomhedsledelse (etfagskandidat)` = "SAM",
  `Politik og Forvaltning (etfagskandidat)` = "SAM",
  `International Public Administration and Politics (etfagskandidat)` = "SAM",
  `Socialvidenskab + Arbejdslivsstudier` = "SAM + IMT",
  `Business Studies + Global Studies` = "SAM + SAM",
  `Global Studies (etfagskandidat)` = "SAM",
  `Socialvidenskab +†PÊdagogik og Uddannelsesstudier` = "SAM + HUM",
  `Socialvidenskab +†Psykologi` = "SAM + HUM",
  `Socialvidenskab +†Sundhedsfremme og Sundhedsstrategier` = "SAM + IMT",
  `Politik†+ Kommunikation` = "SAM + HUM",
  `Socialvidenskab +†Plan, By og Proces` = "SAM + IMT",
  `Virksomhedsstudier + Kommunikation` = "SAM + HUM",
  `Global Studies + International Development Studies` = "SAM + SAM",
  `International Development Studies + Kultur- og Sprogm¯destudier` = "SAM + HUM",
  `International Development Studies + Business Studies` = "SAM + SAM",
  `Global Studies + Communication Studies` = "SAM + HUM",
  `Global Studies + Cultural Encounters` = "SAM + HUM",
  `International Development Studies + Communication Studies` = "SAM + HUM",
  `Socialvidenskab + Kultur- og Sprogm¯destudier` = "SAM + HUM",
  `Socialvidenskab +†Historie` = "SAM + HUM",
  `Virksomhedsstudier + Psykologi` = "SAM + HUM",
  `Virksomhedsstudier + Arbejdslivsstudier` = "SAM + IMT",
  `International Development Studies + Global Studies` = "SAM + SAM"
)

dataset$institut_andet <- recode(dataset$s_123, 
  `Cultural Encounters & International Development Studies (English)` = "SAM + HUM",
  `Geografi + Socialvidenskab` = "SAM + IMT",
  `sociale interventionsstudier` = "IKKE_SAM",
  `Filosofi og Videnskabsteori (1.fag) & Socialvidenskab (2.fag)` = "SAM + HUM",
  `Journalistik + socialvidenskab` = "SAM + HUM",
  `Kultur og sprogm¯destudier + socialvidenskab` = "SAM + HUM",
  `Politik + journalistik` = "SAM + HUM",
  `Business studies & Communication studies (english)` = "SAM + HUM",
  `Geografi + International Development Studies` = "SAM + IMT",
  `Performance Design og Virksomhedsstudier` = "SAM + HUM",
  `Virksomhedsstudier og dansk` = "SAM + HUM",
  `Cultural Encounters and International Development Studies` = "SAM + HUM",
  `Cultural Encounters and International Development` = "SAM + HUM",
  `Journalistik + Politik` = "SAM + HUM",
  `Journalistik og International Development Studies` = "SAM + HUM",
  `Communication & Business Studies` = "SAM + HUM",
  `Communication studies and Global studies` = "SAM + HUM",
)

dataset <- dataset %>% mutate(institut = coalesce(institut, institut_andet))

dataset$institut[dataset$institut == "IKKE_SAM"] <- NA

dataset %>% filter(institut != "IKKE_SAM"
)

dataset <- dataset %>% mutate("Fag.Pol.Fæl" = s_19_1 + s_19_2)
dataset <- dataset %>% mutate("Soc.Rus.Fæl" = s_19_3 + s_19_4 + s_19_5)

dataset$Fag.Pol.Fæl <- recode(dataset$Fag.Pol.Fæl, 
                              "0" = "Nej",
                              "1" = "Ja",
                              "2" = "Ja",
                              "2" = "Ja"
)

dataset$Soc.Rus.Fæl <- recode(dataset$Soc.Rus.Fæl, 
                              "0" = "Nej",
                              "1" = "Ja",
                              "2" = "Ja",
                              "3" = "Ja"
)

enigheds_variable <- c("prof__1",
                       "prof_kom",
                       "uak_god_",
                       "uak_fors",
                       "uak_f_1",
                       "uak_komp",
                       "udd_over",
                       "kender_l",
                       "s_116",
                       "s_118",
                       "s_119",
                       "s_120",
                       "s_23",
                       "s_93",
                       "s_103",
                       "s_124",
                       "s_7",
                       "s_8",
                       "s_9",
                       "s_94",
                       "s_71",
                       "s_72",
                       "s_73",
                       "s_75",
                       "s_115",
                       "s_66", 
                       "s_67", 
                       "s_68", 
                       "s_69", 
                       "s_105", 
                       "s_111",
                       "s_11",
                       "s_13",
                       "s_38",
                       "s_112",
                       "s_121")

dataset <- dataset %>% 
  mutate_at(vars(enigheds_variable), recode,`Meget enig` ='Enig',
            `Delvis enig` ='Enig',
            `Hverken enig eller uenig` ='Hverken/eller',
            `Delvis uenig` ='Uenig',
            `Meget uenig` ='Uenig')

vigtighed <- c("s_96", "s_97", "s_98", "s_99", "s_100", "s_108")

dataset <- dataset %>% 
  mutate_at(vars(vigtighed), recode,`Ikke sÊrlig vigtigt` ='Ikke_vigtigt',
            `Slet ikke vigtigt` ='Ikke_vigtigt',
            `Temmelig vigtigt` ='Temmelig_vigtigt',
            `Meget vigtigt` ='Meget_vigtigt')

dataset$politisk_fjøl <- recode(dataset$s_110, 
                        "Alternativet" = "Venstrefløj",
                        "Andet parti" = "Andre",
                        "Det Konservative Folkeparti" = "Højrefløj",
                        "Enhedslisten - De R¯d-Gr¯nne" = "Venstrefløj",
                        "Jeg har ikke stemmeret til folketingsvalg" = "Kan_ikke_stemme",
                        "Kandidat uden for partierne" = "Andre",
                        "Kristendemokraterne" = "Højrefløj",
                        "Liberal Alliance" = "Højrefløj",
                        "Radikale Venstre" = "Højrefløj",
                        "SF - Socialistisk Folkeparti" = "Venstrefløj",
                        "Socialdemokratiet" = "Venstrefløj",
                        "Ved ikke" = "Andre",
                        "Venstre, Danmarks Liberale Parti" = "Venstrefløj",
                        "Ville stemme blankt" = "Andre",
                        "ÿnsker ikke at svare" = "Andre"
)

dataset <- dataset %>% 
  rename(
    VF_Spændende = s_71,
    VF_Praksis = s_72,
    VF_Arb_Komp = s_73,
    VF_God_Karakter = s_73,
    VF_Kender = s_115,
    UddValg_Prestige = s_66,
    UddValg_Spændende = s_67,
    UddValg_IkkeLedig = s_68,
    UddValg_Løn = s_69,
    UddValg_Org = s_105,
    UddValg_Naturlig_For = s_111
  )


dataset[dataset == "Ved ikke"] <- NA
dataset[dataset == "Andet"] <- NA
dataset[dataset == "Hverken/eller"] <- NA










