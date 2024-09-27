# Script EMD
# Initialisation

# Préalables =====

library(tidyverse)  # inclut : ggplot2, tibble, tidyr, readr, purr, stringr, forcats, dplyr
# NB : NE PAS INSTALLER AVEC INSTALL.PACKAGES sous Linux, chercher un paquet système
# (ça fonctionne mais ça prend 1 heure)
library(readr)      # lecture fichier sous forme de tibble
library(spatstat)   # pour weighted.median (matrixStats est broken avec R 4.2)

library(sf)         # gestion des couches géographiques
library(mapsf)      # carto du Riate
library(potential)  # carto de potentiel du Riate
library(cartography)# ancêtre de mapsf

library(ggspatial)  # pour faire des cartes avec ggplot
library(cowplot)    # pour combiner des ggplots

library(ade4)       # analyses factorielles
library(crayon)     # cat en couleur

library(magick)     # ImageMagick, pour tourner les figures de 90°

# Seuil pour calculer (s'il y a moins d'enquêté⋅es, renvoyer NA)
seuilSignifiant = 20
# Pas de temps pour les analyses récursives en minutes
pas = 20

garder = c("initMémoire", "seuilSignifiant", "pas", "pasDeTemps", "garder")  

rm(list = ls()[!ls() %in% garder], pos = globalenv())

initMémoire = function(f_base = FALSE, BasesCharger = NULL)
{
  # Chargement des fonctions
  source("OUTILS.R", print.eval=T)
  source("LIBELLES.R", print.eval=T)
  source("ANALYSE INDIV.R", print.eval=T)
  source("ANALYSE ESPACE.R", print.eval=T)
  
  if (f_base) {
    source("BASES.R", print.eval=T)
  }
  
  if (!is.null(BasesCharger))
  {
    for (base in BasesCharger)
    {
      if (file.exists(paste0("Data/", base, ".rds")))
      {
        cat("\nChargement", base)
        load(file = paste0("Data/", base, ".rds"), envir = globalenv())
      } else {
        cat(crayon::yellow("Base", base, "introuvable dans Data"))
      }
    }
  }
  
  # Réglage du thème de tous les ggplots
  theme_set(theme_bw(base_size = 9))
} 

gc()