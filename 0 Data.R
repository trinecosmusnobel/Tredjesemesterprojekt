#Indlæsning af data & pakker
library(readxl)                           # Indlæsning af datasæt
library(dplyr)                            # Generelt tidyverse
library(tidyverse)                        # Generelt tidyverse
library(soc.ca)                           # MCA
library(knitr)                            # Rmarkdown
library(RColorBrewer)                     # Farvetemaer
library(ggthemes)                         # Ekstra ggplot-temaer
library(scales)                           # Skalatransformationer
library(lubridate)                        # Arbejde med datoer
library(ggrepel)                          # Til labels
library(cluster)                          # Klyngeanalyser
library("ggdendro")                       # Dendroplots til klynger
library(gtools)


#Indlæsning af datasæt
dataset <- read_excel("Survey_Complete.xlsx")

#Ekstra lag på ggplots for MCA
add.count <- function(x, p, label = TRUE, ...){
  p   <- p + geom_point(data = x, x = x$X, y = x$Y, ...) + geom_path(data = x, x = x$X, y = x$Y, size = 0.3)
  if (identical(label, TRUE)) p <- p + geom_text(data = x, x = x$X, y = x$Y, label = x$label, vjust = 0.2, ...)
}

#Ekstra funktioner til ggplots og MCA
extract_mod         <- function(result, dim = 1:2){
  coord.mod           <- result$coord.mod[, dim]
  rownames(coord.mod) <- result$names.mod
  coord.mod           <- coord.mod[,]
  colnames(coord.mod) <- c("X", "Y")
  
  md            <- coord.mod %>% data.frame() %>% rownames_to_column(var = "Modality")
  ctr           <- result$ctr.mod[, dim]
  md$ctr.x      <- ctr[, 1]
  md$ctr.y      <- ctr[, 2]
  md$ctr        <- rowSums(ctr) / 2
  md$ctr.set    <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md$Frequency  <- result$freq.mod
  md$Variable   <- result$variable
  md
}

extract_sup         <- function(result, dim = 1:2){
  coord.sup           <- result$coord.sup[, dim]
  rownames(coord.sup) <- result$names.sup
  coord.sup           <- coord.sup[,]
  colnames(coord.sup) <- c("X", "Y")
  
  md                  <- coord.sup %>% data.frame() %>% rownames_to_column(var = "Modality")
  md$Frequency        <- result$freq.sup
  md$Variable         <- result$variable.sup
  md
}

extract_ind         <- function(result, dim = 1:2){
  coord.ind           <- result$coord.ind[, dim]
  rownames(coord.ind) <- result$names.ind
  coord.ind           <- coord.ind[,]
  colnames(coord.ind) <- c("X", "Y")
  
  md           <- coord.ind %>% data.frame() %>% rownames_to_column(var = "Individual")
  ctr          <- result$ctr.ind[, dim]
  md$ctr.x     <- ctr[, 1]
  md$ctr.y     <- ctr[, 2]
  md$ctr       <- rowSums(ctr) / 2
  md$ctr.set   <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md
}

map.ca.base <- function(up = "UP", down = "DOWN", right = "RIGHT", left = "LEFT", ...){
  
  breaks.major <- seq(-100, 100, by = 0.25)
  labels       <- breaks.major
  labels[c(FALSE, TRUE)] <- ""
  
  p      <- ggplot(...) + geom_vline(xintercept = 0, size = 0.2) + geom_hline(yintercept = 0, size = 0.2)
  p      <- p + scale_x_continuous(sec.axis = sec_axis(~.*1, name = up, breaks = breaks.major, labels = labels),  
                                   name = down, breaks = breaks.major, labels = labels)
  
  
  p      <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = right, breaks = breaks.major, labels = labels),  
                                   name = left, breaks = breaks.major, labels = labels)  
  
  p      <- p + theme(axis.title.y.left = element_text(size = 2), axis.title.y.right =  element_text(size = 2))
  p      <- p + theme(axis.title.x.top = element_text(size = 2), axis.title.x.bottom =  element_text(size = 2))
  
  
  theme_ca_base <- function (base_size = 5, base_family = "serif", ticks = TRUE) 
  {
    ret <- theme_bw(base_family = base_family, base_size = base_size) + 
      theme(legend.background = element_blank(), legend.key = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_blank(), plot.background = element_blank(), 
            axis.line = element_blank(), panel.grid = element_blank())
    if (!ticks) {
      ret <- ret + theme(axis.ticks = element_blank())
    }
    ret
  }
  
  p      <- p + theme_ca_base()
  
  
  p      <- p + scale_size_continuous(range = c(0.1, 2))
  p      <- p + theme(legend.position = "bottom")
  p
}


