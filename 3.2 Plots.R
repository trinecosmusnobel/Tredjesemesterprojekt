source("3.1 Analyse og tabeller.R")

######## Ekstrakt af positioner af hhv. individer og modaliteter
######## Når plots bygges manuelt og ikke med de indbyggede
######## plot-funktioner i soc.ca, er dette nødvendigt for at
######## specificere koordinater for punkter i ggplots. 
ind             <- extract_ind(result)
mod             <- extract_mod(result)

######## Her specificeres størrelsen på aksetekster
AKSETEKST = 10
AKSETEKST_2 = 12.5

######## Individsky-objekt med tekst på N/S/V/Ø-siderne. Dette
######## objekt er udgangspunkt for de øvrige plots. 
map             <- map.ca.base(up = "Høj grad af arbejdsmarked- og praksisorientering \n", 
                   down = " \n Lav grad af arbejdsmarked- og praksisorientering", 
                   right = "Høj grad af akademisk kapital \n", 
                   left = "Lav grad af akademisk kapital \n")

######## Her kommer koordinater for individer på som geom_points
individsky      <- map + geom_point(data = ind, aes(x = X, y = Y), 
                   shape = 21, 
                   fill = "lightgrey", 
                   size = 2)

######## Aesthetics for individskyen
ind_plot      <- individsky +  theme(axis.text = element_text(size = AKSETEKST), 
                   axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                   axis.ticks = element_blank(),
                   panel.border = element_rect(fill = NA, size = 0.4)) 

######## Gemmer individskyen
ggsave("ind_plot.png", 
       plot = ind_plot,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

######## Her med densitetskurve
map.density       <- ind_plot + geom_density_2d(data = ind, aes(x = X, y = Y), 
                             color = "black") 

######## Gemmer individskyen med densitetskurve
ggsave("map.density.png", 
       plot = map.density,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

######## MODALITETSSKYER
mod$Variable    <- as.factor(mod$Variable) #Omdannes til faktor, så den kan bruges i shape

######## Modaliteternes koordinater lægges over map-basen
modalitetssky   <- map + geom_point(data = mod, aes(x = X, y = Y, shape = Variable), 
                   fill = "black",
                   size = 2) + scale_shape_manual(values = 1:24)

######## Aesthetics for modalitetsskyen - her uden tekst
modalitetssky   <- modalitetssky +  theme(axis.text = element_text(size = AKSETEKST), 
                   axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                   axis.ticks = element_blank(),
                   panel.border = element_rect(fill = NA, size = 0.4), 
                   legend.position = "none")

modalitetssky_u <- modalitetssky #den her skal bruges senere - uden tekst

######## Tilføjer labels
modalitetssky   <- modalitetssky + geom_text_repel(data = mod, aes(x = X, y = Y, label = Modality), size = 2)

######## Gemmer modalitetssky
ggsave("modalitetssky.png", 
       plot = modalitetssky,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

######## MODALITETSSKY - over gennemsnitligt bidrag

######## Filtrerer modaliteterne - kun dem med over gnst. bidrag gemmes
mod_m           <- extract_mod(result) %>% filter(ctr.set)

######## Base for modalitetsskyen
modalitetssky_g <- map + geom_point(data = mod_m, aes(x = X, y = Y, shape = Variable), 
                                            fill = "lightgrey", 
                                            size = 2) + scale_shape_manual(values = 1:24)

######## Aesthetics på - uden tekst
modalitetssky_g <- modalitetssky_g +  theme(axis.text = element_text(size = AKSETEKST), 
                                        axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                                        axis.ticks = element_blank(),
                                        panel.border = element_rect(fill = NA, size = 0.4), 
                                        legend.position = "none")

######## Version med lille/transparent tekst, der skal bruges senere
modalitetssky_small_text <- modalitetssky_g + geom_text_repel(data = mod_m, aes(x = X, y = Y, label = Modality), size = 2, alpha = 0.5)

######## Modalitetsplot med normalt tekst
modalitetssky_g <- modalitetssky_g + geom_text_repel(data = mod_m, aes(x = X, y = Y, label = Modality), size = 2)

######## Gemmer til PNG
ggsave("modalitetssky_g.png", 
       plot = modalitetssky_g,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

####### MODALITETSSKY - passive modaliteter
md              <- extract_mod(result) #Her laves et nyt datasæt med de passive
md.sup          <- extract_sup(result) 
md$type         <- "Aktiv"
md.sup$type     <- "Passiv"
md              <- bind_rows(md, md.sup)
md$label        <- md$Modality

md_1            <- md %>% filter(type == "Passiv") #Vi skal kun bruge de passive her

md_1$Variable  <- as.factor(md_1$Variable) #Omdannes til faktor, så den kan bruges i shape

######## Base for plottet med supp. modaliteter
modalitetssky_s <- map + geom_point(data = 
                                                  md_1, aes(x = X, y = Y, 
                                                            shape = Variable), 
                                                size = 2) + scale_shape_manual(values = 1:10)

######## Aesthetics for plottet
modalitetssky_s <- modalitetssky_s +  theme(axis.text = element_text(size = AKSETEKST), 
                                            axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                                            axis.ticks = element_blank(),
                                            panel.border = element_rect(fill = NA, size = 0.4), 
                                            legend.position = "none")

######## Labels
modalitetssky_s <- modalitetssky_s + geom_text_repel(data = md_1, aes(x = X, y = Y, label = Modality), size = 3)

######## Gemmer til PNG
ggsave("modalitetssky_s.png", 
       plot = modalitetssky_s,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

####### PATH PLOT FOR TRIVES 
sup$Trives <- recode(sup$Trives, #Omkoder den så der kommer lidt luft til punktet fra label
                     "Meget uenig" = "Meget uenig \n",
                     "Delvis uenig" = "Delvis uenig: \n",
                     "Hverken enig eller uenig" = "Hverken enig eller uenig \n",
                     "Delvis enig" = "Delvis enig \n",
                     "Meget enig" = "Meget enig \n")

x.av            <- average.coord(result, #Koordinater for trives-modalitet
                      x = sup$Trives, 
                      dim = c(1,2))  

######## Plot som danner basis for trives
trives           <- add.count(x.av, 
                              modalitetssky_small_text
                   )

######## Koordinater på så man kan se hele forløbet for trives
trives <- trives + coord_cartesian(xlim = c(-1.75, 0.75))

######## Gemmmer PNG
ggsave("trives.png", 
       plot = trives,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

####### PATH PLOT FOR FORÆLDRE 
sup$ForældresHøjesteUddannelse <- recode(sup$ForældresHøjesteUddannelse, #Igen omkodes for at få lift luft
                              "Mellemlang videregående" = "Mellemlang videregående \n",
                              "Lang videregående" = "Lang videregående: \n",
                              "Kort videregående eller erhverv" = "Kort videregående eller erhverv \n",
                              "Ph.D" = "Ph.D \n",
                              "Gymnasie/grundskole" = "Gymnasie/grundskole \n")

x.av2            <- average.coord(result, #Koordinater for forløbet
                                  x = sup$ForældresHøjesteUddannelse, 
                                  dim = c(1,2))  

######## Plot som danner basis for forældre
map.p2           <- add.count(x.av2, 
                              modalitetssky_small_text
)

fræld.udd           <- map.p2

####### Gemmer PNG
ggsave("fræld.udd.png", 
       plot = fræld.udd,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

####### PATH PLOT FOR BACHELOR BAGGRUND 
####### Omkodes of rluft
sup$Bachelor <- recode(sup$Bachelor, 
                                         "RUC" = "RUC \n",
                                         "Erhvervsakademi" = "Erhvervsakademi \n",
                                         "Udenlandsk universitet" = "Udenlandsk universitet \n",
                                         "Dansk universitet" = "Dansk universitet \n",
                                         "Andet" = "Andet \n")

####### Faktor med levels
sup$Bachelor            <- factor(sup$Bachelor,levels = c("Andet \n",
                                                              "Erhvervsakademi \n",
                                                              "Professionsbachelor",
                                                              "Udenlandsk universitet \n",
                                                              "Dansk universitet \n",
                                                              "RUC \n"))
####### Plot der danner udgangspunkt
x.av3            <- average.coord(result, 
                                  x = sup$Bachelor, 
                                  dim = c(1,2))  

bachelor           <- add.count(x.av3, 
                                modalitetssky_small_text
)

####### Koordinater så det passer
bachelor_path <- bachelor + coord_cartesian(xlim = c(-0.75,0.25), ylim = c(-0.5,0.5))

####### Gemmer PNG
ggsave("bachelor_path.png", 
       plot = bachelor_path,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

######## INDIVIDSKY MED FARVER OG ELLIPSER pba. klyngeanalyse
coord <- result$coord.ind[, 1:3] #Koordinater til klyngeanalysen

cl    <- agnes(coord)            #AGNES for koordinaterne

plot(cl)                         #Plotter for overblik over klyngedannelse

cl.mem <- cutree(cl, k = 4)      #Deler det op i fire klynger pba. det forrige

table(cl.mem)                    #Antal i klynger i tabel

dataset$cl.mem <- cl.mem         #En variabel for hvilken klynge de tilhører

####### Plot med klynge
ind             <- extract_ind(result)

individsky      <- map + geom_point(data = ind, aes(X, Y, fill = as.factor(cl.mem)), 
                                           shape = 21,
                                           size = 1.5)

ind_plot      <- individsky +  theme(axis.text = element_text(size = AKSETEKST), 
                                     axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                                     axis.ticks = element_blank(),
                                     panel.border = element_rect(fill = NA, size = 0.4)) 

ind_plot <-   ind_plot + theme(legend.position = "none")

test2 <- ggplot(ind, aes(X, Y, color = as.factor(cl.mem))) + geom_point() + stat_ellipse()

####### Merger de to plots
ind_plot$data <- test2$data

ind_plot[["mapping"]] <- test2[["mapping"]] 

ind_plot <- ind_plot + stat_ellipse(aes(color = as.factor(cl.mem)))

ellipse <- ind_plot

####### Gemmer PNG
ggsave("ellipse.png", 
       plot = ellipse,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)

####### PATH PLOT FOR TRIVES  I SUP SKY

####### Igen omkodning for at få lidt luft
sup$Trives <- recode(sup$Trives, 
                     "Meget uenig" = "Meget uenig \n",
                     "Delvis uenig" = "Delvis uenig: \n",
                     "Hverken enig eller uenig" = "Hverken enig eller uenig \n",
                     "Delvis enig" = "Delvis enig \n",
                     "Meget enig" = "Meget enig \n")

####### Koordinater for trives
x.av            <- average.coord(result, 
                                 x = sup$Trives, 
                                 dim = c(1,2))  

####### Danner plottet
modalitetssky   <- modalitetssky_s +  theme(axis.text = element_text(size = AKSETEKST), 
                                                     axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                                                     axis.ticks = element_blank(),
                                                     panel.border = element_rect(fill = NA, size = 0.4), 
                                                     legend.position = "none")
####### Koordinater for trives
trives           <- add.count(x.av, 
                              modalitetssky
)

####### Koordinater for forældres højeste uddannelse
x.av2            <- average.coord(result, 
                                 x = sup$ForældresHøjesteUddannelse, 
                                 dim = c(1,2))  

####### Labels skal være blanke
trives[["layers"]][[7]][["aes_params"]][["label"]] <- ""

trives           <- add.count(x.av2, 
                              trives
)

trives[["layers"]][[10]][["aes_params"]][["label"]] <- ""

trives[["layers"]][[9]][["aes_params"]][["size"]] <- 0.6
trives[["layers"]][[6]][["aes_params"]][["size"]] <- 0.6


trives_sup <- trives

trives_sup[["layers"]][[4]][["geom"]][["default_aes"]][["colour"]] <- "lightgrey"

trives_sup

ggsave("trives_sup.png", 
       plot = trives_sup,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)
  
######## Path plot for forældres uddannelse i plot klyngeanalyse for modaliteter
coord <- extract_mod(res)

coord <- coord %>% filter(ctr.set == TRUE)

cl    <- agnes(coord[,2:3])

cl.mem <- cutree(cl, k = 4)
table(cl.mem)

plot(cl)

coord$cl.mem <- cl.mem

ind             <- coord

individsky      <- map

individsky      <- individsky + geom_point(data = ind, aes(X, Y),
                                           size = 2, alpha = 0.2)


ind_plot      <- individsky +  theme(axis.text = element_text(size = AKSETEKST), 
                                     axis.title = element_text(size = AKSETEKST_2, face = "bold"),
                                     axis.ticks = element_blank(),
                                     panel.border = element_rect(fill = NA, size = 0.4)) 

ind_plot <-   ind_plot + theme(legend.position = "none")

test2 <- ggplot(ind, aes(X, Y)) + geom_point(alpha = 0.2) + stat_ellipse()

ind_plot$data <- test2$data

ind_plot[["mapping"]] <- test2[["mapping"]] 

ind_plot <- ind_plot + stat_ellipse(aes(color = as.factor(cl.mem)), )

ellipse <- ind_plot


ellipse <- ellipse + geom_text_repel(data = ind, aes(x = X, y = Y, label = Modality, alpha = 0.6), size = 2)


map.p2           <- add.count(x.av2, 
                              ellipse
)

map.p2[["layers"]][[8]][["geom"]][["default_aes"]][["size"]] <- 2.5

ggsave("map.p2.png", 
       plot = map.p2,
       path = "Plots/", 
       width = 15, 
       height = 15,
       units = "cm", 
       dpi = 275)


