#Analyse

source("2 Omkodning.R")

active <- dataset %>% arrange(ID) %>% select(
                                             adg_udd,
                                             Fag.Pol.Fæl,
                                             Soc.Rus.Fæl,
                                             mor_udd,
                                             far_udd,
                                             udd_over,
                                             VF_Praksis,
                                             VF_God_Karakter,
                                             VF_Kender,
                                             UddValg_Løn,
                                             s_121,
                                             s_13,
                                             s_43,
                                             s_116) 

sup <- dataset %>% arrange(ID) %>% select() 

result <- soc.mca(active, sup = NULL, 
                  identifier = NULL,
                  passive = getOption("passive", 
                                      default = "Missing"))

map.ind(result, point.size = 0.5, map.title = "Studerende på ISE")
map.ind(result, dim = c(1,2), point.size = 0.5, map.title = "Politisk holdninger i Danmark ESS")

map.active(result, label.size = 1.5)
  #map.active(result, label = FALSE)