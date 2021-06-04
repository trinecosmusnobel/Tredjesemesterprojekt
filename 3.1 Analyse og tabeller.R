#Source de øvrige scripts
source("2 Omkodning.R")

#Dan MCA objekt, som danner udgangspunkt for analysen
result                           <- soc.mca(active, 
                                            sup, passive = c("Ved ikke", 
                                                             "Andet", 
                                                             "Har endnu ikke valgt valgfag", 
                                                             "Hverken_eller"))

result <- invert(result, dim = 1:2)

#Modaliteter, der bidrag over gennemsnittet til 1. og 2. dimensionen
dim_1_ctr                        <- contribution(result, 
                                                 dim = 1, 
                                                 all = FALSE, 
                                                 mode = "sort")

dim_2_ctr                        <- contribution(result, 
                                                 dim = 2, 
                                                 all = FALSE, 
                                                 mode = "sort")


#Teoretiske dimensioner bidrag til dimensionerne
modaliteter_teoretisk            <- headings(result)[,1:5] 

modaliteter_teoretisk

#tabel over supplementære variable og deres position på 1. og 2. dimensionerne
table_sup                        <- bind_cols(result$freq.sup, 
                                              result$names.sup, 
                                              result$coord.sup[,1], 
                                              result$coord.sup[,2])

colnames(table_sup)              <-  c("Frekvens", 
                                       "Modalitet", 
                                       "1. dim, koord.", 
                                       "2. dim, koord.")

table_sup


#screeplot
data_scree                       <- variance(result, 1:10) #variansen for 10 første akser

scree_data                       <- as.data.frame(t(data_scree)) 

scree_plot                       <- scree_data %>% #plot
                                    ggplot( aes(x=Dim, y=Adj.Var)) +
                                    geom_line() +
                                    geom_point() +
                                    scale_x_continuous(breaks = seq(1, 10, 1), limits = c(1,10)) +
                                    geom_hline(yintercept = 5.75, linetype = "dashed") +
                                    theme_clean() +
                                    xlab("Akse") +
                                    ylab("Modificeret eigen-værdi") + 
                                    scale_y_continuous(labels = function(x) paste0(x*1, "%"), breaks = seq(0,60,5), limits = c(0,45)) +
                                    geom_text(aes(8.3,6.9,label = "80 % af variansen forklaret"), size = 4) +
                                    theme(plot.background = element_rect(
                                    color = "white"
                                    ))

scree_plot

ggsave("scree_plot.png", 
       path = "Plots/", 
       width = 15, 
       units = "cm", 
       height = 12.5, 
       dpi = 300)

#mca og contributions for de enkelte teoretiske dimensioner
l.mca      <- map(active, ~soc.mca(.x, passive = c("Ved ikke", 
                                                   "Andet", 
                                                   "Har endnu ikke valgt valgfag", 
                                                   "Hverken_eller")))

#Modaliteter med mindre end 5 % i deres respektive kategorier
result_n <- data.frame(result[["freq.mod.all"]])
result_n <- tibble::rownames_to_column(result_n, "Kategori") 
result_n <- result_n %>% rename(n = result...freq.mod.all...)

result_n <- separate(result_n, Kategori, c("Variabel" , "Kategori") , ":")
result_n <- result_n %>% group_by(Variabel) %>% 
  mutate(Percent = 100*n/sum(n))
result_n <- result_n %>% filter(Percent < 5) %>% filter(Kategori != "MISSING") 
result_n

###BALANCE TEST
active_stab <- active

active_stab$Studiestrategi <- active_stab$Studiestrategi[1:9]

result_stab                           <- soc.mca(active_stab, 
                                            sup, passive = c("Ved ikke", 
                                                             "Andet", 
                                                             "Har endnu ikke valgt valgfag", 
                                                             "Hverken_eller"))

#Test for stabilitet
contribution.headings(result)
contribution.headings(result_stab)
