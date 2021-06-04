source("2 Omkodning.R")

dataset[dataset == "MISSING"] <- NA
dataset[dataset == "Ved ikke"] <- NA

dataset$ Bachelor


nye_stud <- dataset %>% filter(Bachelor %in% c("Erhvervsakademi", 
                                              "Professionsbachelor", 
                                              "Udenlandsk universitet", 
                                              "Dansk universitet"))

nye_stud$Bachelor <- recode(nye_stud$Bachelor, 
                            "Professionsbachelor" = "Erhverv/prof",
                            "Erhvervsakademi" = "Erhverv/prof",
                            "Udenlandsk universitet" = "DK/uden. uni",
                            "Dansk universitet" = "DK/uden. uni"
)

nye_stud$god_start <- coalesce(nye_stud$prof_god, nye_stud$uak_god_)
nye_stud$komp <- coalesce(nye_stud$uak_komp, nye_stud$prof_kom)
nye_stud$forskel_studerende <- coalesce(nye_stud$uak_fors, nye_stud$prof_for)
nye_stud$forskel_undervisere <- coalesce(nye_stud$uak_f_1, nye_stud$prof__1)

table(nye_stud$god_start)
table(nye_stud$komp)
table(nye_stud$forskel_studerende)
table(nye_stud$forskel_undervisere)



nye_stud$god_start            <- factor(nye_stud$god_start, levels = c("Meget uenig",
                                                                       "Delvis uenig",
                                                                       "Hverken enig eller uenig",
                                                                       "Delvis eenig",
                                                                       "Meget enig"))

nye_stud$komp            <- factor(nye_stud$komp, levels = c("Meget uenig",
                                                                       "Delvis uenig",
                                                                       "Hverken enig eller uenig",
                                                                       "Delvis eenig",
                                                                       "Meget enig"))

nye_stud$forskel_studerende            <- factor(nye_stud$forskel_studerende, levels = c("Meget uenig",
                                                                       "Delvis uenig",
                                                                       "Hverken enig eller uenig",
                                                                       "Delvis eenig",
                                                                       "Meget enig"))

nye_stud$forskel_undervisere            <- factor(nye_stud$forskel_undervisere, levels = c("Meget uenig",
                                                                       "Delvis uenig",
                                                                       "Hverken enig eller uenig",
                                                                       "Delvis eenig",
                                                                       "Meget enig"))

table(nye_stud$forskel_undervisere)

xtabs(~Bachelor+komp,data=nye_stud)