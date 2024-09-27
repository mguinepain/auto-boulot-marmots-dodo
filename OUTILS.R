# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                   E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                     #
#                                                                                                 #
#                               SCRIPTS DE TRAVAIL M. GUINEPAIN                                   #
#                                           FEV. 2021                                             #
#                                                                                                 #  
#                           N°0 - BOITE A OUTILS DE FONCTIONS UTILES                              #
#                                                                                                 #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #


# Depuis mars 2021, ce script a pour vocation à être chargé dans les autres scripts
# (avec la fonction "source") pour y
# importer toutes les fonctions usuelles en bloc et libérer de la place.

# Dépendances chargées dans START.R

# Cohérence du travail ==============================================================================

# Pour afficher quelque-chose dans la console ET dans un journal d'exécution.
rapport = function(..., prim = F, extrait = F, info = F)
{
  load("Data/idVersion.rds")
  chemin = paste0("Sorties/Journaux/Journal n°", idVersion, ".txt")
  
  dateJour = paste0("\n", "\n========== ", substr(Sys.time(), 1,10), " ",
                    paste(rep("=", 82), collapse=""), "\n")
  
  if (file.exists(chemin)) {
    journal = read_file(chemin)
    if (length(grep(dateJour, journal)) == 0)
    { ajouterDate = T } else {ajouterDate = F}
    remove(journal)
  }
  else {
    ajouterDate = T
  }
  
  messageInit = paste(...)
  
  # Les chaînes de caractères passées à rapport sont collées
  message = paste(...)
  
  lignesMessages = unlist(strsplit(message, "\n"))
  for (i in 1:length(lignesMessages))
  {
    # Si la ligne est trop longue, il faut la couper
    if (nchar(lignesMessages[i])>100)
    {
      messageReste = lignesMessages[i]
      messageSort  = ""
      ligne = ""
      
      while(nchar(messageReste) > 100)
      {
        messages = unlist(strsplit(messageReste, split = " "))
        
        for(j in 1:length(messages))
        {
          ligne = paste(messages[1:j], collapse=" ")
          if (nchar(ligne) > 100)
          {
            ligne = paste(messages[1:j-1], collapse=" ")
            break()
          }
        }
        messageSort = c(messageSort, ligne)
        messageReste = substr(messageReste, nchar(ligne)+2, nchar(messageReste))
      }
      lignesMessages[i] = paste(c(messageSort[2:length(messageSort)], messageReste), collapse = "\n")
    }
  }
  message = paste(lignesMessages, collapse="\n")
  
  
  # On s'arrange pour mettre de l'indentation sur les retours à la ligne
  if (extrait)
  { message = gsub(pattern = "\n", replacement = "\n           | ", x = message)
  message = paste0("\n           | ", message) }
  if (!extrait)
  { message = gsub(pattern = "\n", replacement = "\n           ", x=message) }
  
  # On ajoute un préfixe d'heure pour chaque entrée du journal (sauf si "info")
  heure = paste0("[", substr(as.character(Sys.time()), 12, 19), "]")
  if (extrait) {heure = ""}
  if (info) {heure = "          "}
  
  load("Data/idVersion.rds")
  
  # On écrit dans le journal à l'aide de la fonction Sink
  sink(chemin, append = T)
  if(ajouterDate) {cat(dateJour, "\n")}
  if(prim) {cat("\n") ; cat("\n")}
  cat(heure, message)
  cat("\n")
  if (extrait) {cat("\n")}
  sink()
  
  # On répercute l'entrée sur la console
  if(prim) {cat("\n")}
  cat(messageInit) ; cat("\n")
  if(extrait) {cat("\n")}
}

# Mois en cours aujourd'hui
moisEnCours = function()
{
    mois = substr(Sys.Date(),6,7) %>%
        plyr::revalue(c("01" = "janvier", "02" = "février", "03" = "mars", "04" = "avril",
                        "05" = "mai", "06" = "juin", "07" = "juillet", "08" = "août",
                        "09" = "septembre", "10" = "octobre", "11" = "novembre", "12" = "décembre"),
                      warn_missing = F)
    return(paste0(mois, " ", substr(Sys.Date(),1,4)))
}


# Fonction pour sourcer les documents
src_fig = function(base = NULL, bu = T, emp = T, carto = F, date = moisEnCours(),
                   auteurs = "Maxime Guinepain")
{
    
    listeEMDs = c("AGL2012", "AGR2012", "AJA2017", "ALB2011", "ALE2018", "AMI2010", "ANC2017",
                  "ANM2016", "ARR2014", "BAY2010", "BEA2010", "BEB2017", "BES2018", "BEZ2014",
                  "BOR2009", "BOU2019", "BRE2018", "BRH2009", "CAL2011", "CAR2015", "CAY2010",
                  "CHE2016", "CHS2014", "CLF2012", "CRE2017", "DIJ2016", "DIN2010", "DOU2012",
                  "DUN2015", "EVR2018", "GAP2018", "GRE2010", "HAV2018", "IDF2010", "LAR2011",
                  "LAV2011", "LCS2012", "LIL2016", "LOI2015", "LON2014", "LYO2015", "MAR2009",
                  "MET2017", "MON2014", "MTQ2014", "NAN2013", "NIC2009", "NIM2015", "NIO2016",
                  "PFO2012", "POI2018", "QMP2013", "REN2018", "REU2016", "ROA2012", "ROC2016",
                  "ROU2017", "ROY2015", "RSY2013", "SAI2016", "SDO2011", "SQY2010", "STB2012",
                  "STE2010", "STL2011", "THI2012", "TLS2013", "TOU2019", "VAR2012", "VLC2014",
                  "VLN2011", "VLN2019")
    
    lib = "Données : "
    
    if (!is.null(base))
    {
        if (!"uid_ENQ" %in% colnames(base)) {
            
            if ("uid_PER" %in% colnames(base)) { base$uid_ENQ = substr(base$uid_PER, 1, 7) }
            else { stop("Pas de colonne uid_ENQ à évaluer pour sourcer les données") }
        }
        
        if (length(unique(base$uid_ENQ)) > 1)
        {
            if (any(listeEMDs %in% base$uid_ENQ))
            {
                intervalleMin = min(as.integer(substr(base$uid_ENQ, 4,7)))
                intervalleMax = max(as.integer(substr(base$uid_ENQ, 4,7)))
                if (intervalleMin != intervalleMax) {
                    if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
                    lib = paste0(lib, "Enquêtes Cerema, ", intervalleMin, "-", intervalleMax)
                } else {
                    if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
                    lib = paste0(lib, "Enquêtes Cerema, ", intervalleMin)
                }
            }
            if ("EMP2019" %in% base$uid_ENQ)
            {
                if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
                lib = paste0(lib, "EMP, 2019")
            }
          lib = paste0(lib, ".\n")
        } else {
            typeEnq = z_Nomenclature[z_Nomenclature$uid_ENQ == unique(base$uid_ENQ),]$Methodo
            libEnq =  z_Nomenclature[z_Nomenclature$uid_ENQ == unique(base$uid_ENQ),]$Libelle_Simple
            annEnq =  z_Nomenclature[z_Nomenclature$uid_ENQ == unique(base$uid_ENQ),]$Annee
            
            if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
            lib = paste0(lib, paste0(typeEnq, " ", libEnq, ", ", annEnq, ".\n"))
        }
    
      if ("densite" %in% colnames(base))
      {
        lib = paste0(lib, " ;\nrecensement carroyé (Insee, 2015)")
      }
      
    } else {
        if (bu)
        {
            if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
            lib = paste0(lib, "Enquêtes Cerema, 2009-2019")
        }
        if (emp)
        {
            if (nchar(lib) > 10) { lib = paste0(lib, " ; ") }
            lib = paste0(lib, "EMP, 2019")
        }
        if (emp | bu)
        {
          lib = paste0(lib, ".\n")
        }
        if(!emp & !bu )
        {
          lib = ""
        }
    }
    
    if (carto)
    {
        lib = paste0(lib, "Cartographie : IGN Route500, geonames.org, Mobiliscope.\n")
    }
    
    if (!is.null(auteurs) & !is.null(date)) {
        lib = paste0(lib, auteurs, ", ", date, ".")
    }
    
    return(lib)
}

# Pour constituer l'échantillon de la base (travailleur⋅ses uniquement,
# sans les 5% de distances les plus élevées)
init_PER_ff = function(PER)
{
  PER_ff = PER %>%
    filter(DuTvl > 0 & Dis < 166000 &
             PCS8 %in% c("01", "02", "03", "04", "05", "06") & Activ %in% c("10", "11") &
             Age < 70) |>
    mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T))
  rapport("Nombre d'individus dans PER_ff : ", nrow(PER_ff))
  return(PER_ff)
}

# Bugs ====

# Pour vérifier qu'il n'y a pas de doublon dans les principales composantes chargées de la base.
chk_Bases = function(supprimer = F, sauver=F, vTotale=F, afficher=F)
{
    if (vTotale)
    {
        rapport("Vérification de toutes les bases depuis leurs fichiers d'origine.")
        load("Data/MEN.rds")
        load("Data/PER.rds")
        load("Data/DEPintoPER.rds")
        load("Data/DEP.rds")
        load("Data/TRJ.rds")
    }
    else
    {
        rapport("Vérification de l'intégrité des bases chargées.")
    }
    
    mémoire = ls(envir = globalenv())
    checkOk = T
    
    if ("MEN" %in% mémoire) {
        uids = unique(MEN$uid_MEN)
        if (length(uids) != length(MEN$uid_MEN))
        {
            rapport("ATTENTION :", length(MEN$uid_MEN) - length(uids), "doublons détectés dans la base MEN.", info=T)
            checkOk = F
            
            if (!supprimer & afficher)
            {
                lUids = as.data.frame(table(MEN$uid_MEN)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                View(filter(MEN, uid_MEN %in% lUids))
            }
            
            if (supprimer) {
                lUids = as.data.frame(table(MEN$uid_MEN)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                MEN = filter(MEN, !uid_MEN %in% lUids)
                rapport("Suppressions effectuées.", info=T)
                if (sauver) {save(MEN, file="Data/MEN.rds") ; rapport("Fichiers mis à jour.", info=T)}}
        }
    }
    if ("PER" %in% mémoire) {
        uids = unique(PER$uid_PER)
        if (length(uids) != length(PER$uid_PER))
        {
            rapport("ATTENTION :", length(PER$uid_PER) - length(uids), "doublons détectés dans la base PER.", info=T)
            checkOk = F
            
            if (!supprimer & afficher)
            {
                lUids = as.data.frame(table(PER$uid_PER)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                View(filter(PER, uid_PER %in% lUids))
            }
            
            if (supprimer) {
                lUids = as.data.frame(table(PER$uid_PER)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                PER = filter(PER, !uid_PER %in% lUids)
                rapport("Suppressions effectuées.", info=T)
                if (sauver) {save(PER, file="Data/PER.rds") ; rapport("Fichiers mis à jour.", info=T)}}
        }
    }
    if ("DEPintoPER" %in% mémoire) {
        uids = unique(DEPintoPER$uid_PER)
        if (length(uids) != length(DEPintoPER$uid_PER))
        {
            rapport("ATTENTION :", length(DEPintoPER$uid_PER) - length(uids), "doublons détectés dans la base DEPintoPER", info=T)
            checkOk = F
            
            if (supprimer) {
                lUids = as.data.frame(table(DEPintoPER$uid_PER)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                rapport("Purge : un seul élément parmi les doublons sera conservé.")
                
                DEPintoPER = distinct(DEPintoPER)
                
                uids = unique(DEPintoPER$uid_PER)
                if (length(uids) != length(DEPintoPER$uid_PER))
                {
                    rapport("Impossible de résoudre ce problème. Suppression des entités surnuméraires.")
                    rapport("Cette manoeuvre va causer une perte de données.", info = T)
                    
                    listeDbl = DEPintoPER %>% group_by(uid_PER) %>% summarize(n = n()) %>% filter(n>1)
                    rapport(length(unique(listeDbl$uid_PER)), "individus vont être supprimés.", info=T)
                    
                    DEPintoPER = filter(DEPintoPER, !uid_PER %in% listeDbl$uid_PER)
                    
                    uids = unique(DEPintoPER$uid_PER)
                    if (length(uids) != length(DEPintoPER$uid_PER))
                    {
                        rapport("Erreur fatale. Insoluble.")
                        stop("Erreur. Impossible de réduire les doublons de DEPintoPER. Arrêt du programme.")
                    }
                }
                
                if (sauver) {save(DEPintoPER, file="Data/DEPintoPER.rds") ; rapport("Fichiers mis à jour.", info=T)}}
        }
    }
    if ("DEP" %in% mémoire) {
        uids = unique(DEP$uid_DEP)
        if (length(uids) != length(DEP$uid_DEP))
        {
            rapport("ATTENTION :", length(DEP$uid_DEP) - length(uids), "doublons détectés dans la base DEP.", info=T)
            checkOk = F
            if (supprimer) {
                lUids = as.data.frame(table(DEP$uid_DEP)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                DEP = filter(DEP, !uid_DEP %in% lUids)
                rapport("Suppressions effectuées.", info=T)
                if (sauver) {save(DEP, file="Data/DEP.rds") ; rapport("Fichiers mis à jour.", info=T)}}
        }
    }
    
    if ("TRJ" %in% mémoire) {
        uids = unique(TRJ$uid_TRJ)
        if (length(uids) != length(TRJ$uid_TRJ))
        {
            rapport("ATTENTION :", length(TRJ$uid_TRJ) - length(uids), "doublons détectés dans la base TRJ.", info=T)
            checkOk = F
            if (supprimer) {
                lUids = as.data.frame(table(TRJ$uid_TRJ)) %>% filter(Freq>1)
                lUids = as.character(lUids$Var1)
                TRJ = filter(TRJ, !uid_TRJ %in% lUids)
                rapport("Suppressions effectuées.", info=T)
                if (sauver) {save(TRJ, file="Data/TRJ.rds") ; rapport("Fichiers mis à jour.", info=T)}}
        }
    }
    
    if (checkOk) {rapport("Les bases ne contenaient aucune erreur.", info=T)}
    
    if (!checkOk & !supprimer) { stop("Les bases sont corrompues.") }
    
    
    if (vTotale) {rapport("La mémoire est vidée.", info=T)
        remove(MEN, PER, DEPintoPER, DEP, TRJ)}
}

colonnesDupliquees = function(tab)
{
  # Pour trouver rapidement la colonne en double de l'enfer !!
  
  tabFq = table(colnames(tab)) |>
    as.data.frame() |>
    filter(Freq>2)
  
  if (nrow(tabFq) > 0) {
    cat("Colonnes en double :", paste(tabFq$Var1, collapse = ", "), "\n")
  } else {
    cat("Pas de colonne en double !\n")
  }
  
  return(tabFq$Var1)
}

# Manipulation de données ===========================================================================

# FreqToPart passe un tableau croisé (2 ou 3 dimensions) en pourcentages à l'aide d'un tableau (1 ou 2 dims)
# décrivant les sous-totaux (chaque pourcentage fait 100 pour chaque case du tableau parent)
# Le mode "Résidus NAs" ne renvoie que les écarts entre les totaux calculés et les NAs contenus dans Tabgen
tab_FreqToPart = function(tab, tabgen = NULL, compterNA = F, compenserNA = F, ajouterTotal = F)
{
    
    if(compterNA == T & compenserNA == T)
    {
        stop ("Fonction FreqToPart: impossible de compter les NAs ET de les compenser")
    }
    if(compenserNA == T)
    {
        tabgen = NULL
    }
    if(is.null(tabgen) & compenserNA == F)
    {
        stop( paste(c("Sans tableau comptant les totaux, les comptes seront incomplets.",
                      "Activer Compenser les NAs pour utiliser FreqToPart quand-même."), collapse="\n"))
    }
    
    tab = as.data.frame(tab)
    
    if (ncol(tab) == 3) {colnames(tab) = c("Var1", "Var2", "Freq")}
    if (ncol(tab) == 4) {colnames(tab) = c("Var1", "Var2", "Var3", "Freq")}
    if (!is.data.frame(tab)) {stop("tab doit être un dataframe")}
    tab$Freq = as.numeric(tab$Freq)
    
    if (!is.null(tabgen)){
        if (is.table(tabgen)) { tabgen = as.data.frame(tabgen)}
        if (!is.data.frame(tabgen)) {stop("tabgen doit être un dataframe")}
        tabgen$Freq = as.numeric(tabgen$Freq)
        
        if (ncol(tab) == 3) {colnames(tabgen) = c("Var1", "Total")}
        if (ncol(tab) == 4) {colnames(tabgen) = c("Var1", "Var2", "Total")}
    }
    
    t = tab
    
    if (is.null(tabgen)) {tabgen = t %>% group_by(Var1) %>% summarize(Total = sum(Freq))}
    
    if (ncol(tab) == 3) {
        t = left_join(t, tabgen, by=c("Var1" = "Var1")) %>%
            mutate(Part = Freq / Total * 100)}
    
    if (ncol(tab) == 4) {
        t = left_join(t, tabgen, by=c("Var1" = "Var1", "Var2" = "Var2")) %>%
            mutate(Part = Freq / Total * 100)}
    
    
    if (ncol(tab) == 3) {t = t %>% select(Var1, Var2, Part)}
    if (ncol(tab) == 4) {t = t %>% select(Var1, Var2, Var3, Part)}
    
    if (compterNA == T) {
        if (ncol(tab) == 3) {
            NAs = t %>% group_by(Var1) %>% summarize(P = sum(Part)) %>% mutate(iP = 100 - P)
            NAs$Var2 = "NA" ; NAs$Part = NAs$iP ; NAs = select(NAs, Var1, Var2, Part)
        }
        if (ncol(tab) == 4) {
            NAs = t %>% group_by(Var1, Var2) %>% summarize(P = sum(Part)) %>% mutate(iP = 100 - P)
            NAs$Var3 = "NA" ; NAs$Part = NAs$iP ; NAs = select(NAs, Var1, Var2, Var3, Part)
        }
        t = rbind(t, NAs)
    }
    
    # Possibilité de sortir les effectifs totaux, toutes valeurs de la variable 1 comprises
    if (ajouterTotal == T) {
        if (ncol(tab) == 3) {
            Total = tab %>% group_by(Var2) %>% summarize(Freq = sum(Freq))
            effTotal = sum(Total$Freq)
            Total = mutate(Total, Part = Freq / effTotal * 100, Var1 = "Total") %>%
                select(Var1, Var2, Part)
            t = rbind(t, Total)
        }
    }
    
    t = t %>% mutate(Var2 = ifelse(is.na(Var2), "NA", as.character(Var2)), Var2 = as.factor(Var2))
    t = filter(t, !is.na(Var1))
    return(t)
}

# La fonction ci-dessous retour la part de Var2 qui sont des NAs pour chaque Var1, quand on croise deux variables.
# Elle est utile pour savoir quelle est la part de NAs quand on les a compensés dans FreqToPart, ce qui les y fait disparaître.
tab_FreqToPart.NAs = function(tab, tabgen) 
{
    tab = as.data.frame(tab)
    
    if (ncol(tab) == 3) {colnames(tab) = c("Var1", "Var2", "Freq")}
    if (ncol(tab) == 4) {colnames(tab) = c("Var1", "Var2", "Var3", "Freq")}
    if (!is.data.frame(tab)) {stop("tab doit être un dataframe")}
    tab$Freq = as.numeric(tab$Freq)
    
    if (is.table(tabgen)) { tabgen = as.data.frame(tabgen)}
    if (!is.data.frame(tabgen)) {stop("tabgen doit être un dataframe")}
    tabgen$Freq = as.numeric(tabgen$Freq)
    
    if (ncol(tab) == 3) {colnames(tabgen) = c("Var1", "Total")}
    if (ncol(tab) == 4) {colnames(tabgen) = c("Var1", "Var2", "Total")}
    
    t = tab
    
    tabgen2 = t %>% group_by(Var1) %>% summarize(Total2 = sum(Freq))
    
    tNAs = left_join(tabgen, tabgen2, by=c("Var1" = "Var1")) %>%
        mutate(ecart = Total - Total2, ecartRelatif = ecart / Total * 100)
    
    return(tNAs$ecartRelatif)
}


# Trie rapidement un tableau croisé par sa 1e, 2e ou 3e colonne, et réordonne les facteurs
# en fonction (afin d'apparaître trié dans ggplot)
tab_Tri = function(t, i = 1, rev = F, parCol = NULL){  #Val = NULL, colVal = NULL
    
    # je ne comprends pas pourquoi j'ai fait ça ↓
    # if (!is.null(parCol) & is.null(Val) | !is.null(parCol) & is.null(colVal))
    # {stop("Il faut spécifier : la colonne contenant les étiquettes à contrôler, la valeur par laquelle trier, la colonne contenant les valeurs.")}
    
    clé = t[[i]]
    
    num = is.numeric(clé)
    
    if (is.null(parCol)) {
        
        clé = na.omit(clé)
        
        if (is.factor(clé)) {clé = as.character(clé)}
        # peut-être que la clé est un pseudonumérique : s'il est possible de la convertir en nombre, on le fait
        clé2 = try(as.numeric(clé), T) ; if (!T %in% is.na(clé2)) {clé = clé2}
        
        t = t[order(clé, decreasing = rev),]
        
        if (!num) {to = unique(t[[i]]) ; t[[i]] = factor(t[[i]], to)}
        
        # On reconvertit la colonne en numérique si elle l'était à l'origine
        if(num) {t[[i]] = as.numeric(as.character(t[[i]]))}
        
    } else
    { # Plus complexe : tri sur une valeur d'une autre colonne
        
        # On récupère l'ordre des valeurs sur la colonne par laquelle on trie
        t = t[order(t[[parCol]], decreasing = rev),]
        # On transforme la colonne à trier en facteur, dont les niveaux sont réordonnés
        t[[i]] = as.factor(as.character(t[[i]]))
        t[[i]] = factor(t[[i]], unique(t[[i]]))
        
    }
    
    return(t)
}

# Géométrie d'une table EMD (à partir des codes, transformer en sf)
init_geomMen = function(base, shp_ZF, shp_COM, centroides=F)
{
    rapport("Génération de la géométrie pour une table EMD")
    
    if ("sf" %in% class(base))
    {
        rapport("Géométrie préexistante détectée et écrasée", info=T)
        base = st_drop_geometry(base)
    }
    
    # On s'assure que les géométries sont correctes et on les rend compatibles
    shp_ZF = st_cast(st_as_sf(shp_ZF, crs=2154), "MULTIPOLYGON")
    shp_ZF = st_transform(shp_ZF, crs=3857)
    
    # On convertit les ZF et les communes en points. Un point = un ménage.
    if (!centroides)
    {
        pts.shp = st_point_on_surface(shp_ZF)
        pts.com = st_point_on_surface(shp_COM)
    }
    else
    {
        pts.shp = st_centroid(shp_ZF)
        pts.com = st_centroid(shp_COM)
    }
    
    
    # On associe des coordonnées aux ménages selon leur ZF et leur Com.
    base = base %>%
        left_join(select(pts.shp, CODE_ZF, geometry), by = c("ZF" = "CODE_ZF")) %>%
        left_join(select(pts.com, insee  , geometry), by = c("Com"="insee"))
    
    # S'il y a une géométrie ZF, on la prend ; sinon, on prend la géométrie commune.
    base = mutate(base, geometry = ifelse(as.character(geometry.x) == "list()", geometry.y, geometry.x))
    
    # On se débarrasse des deux colonnes
    base = select(base, -geometry.y) %>% select(-geometry.x)
    
    # On force à reconnaître une géométrie à la base
    base = st_as_sf(base, crs=3857)
    
    # On vérifie le résultat
    nEntSansGeom = nrow(filter(mutate(base, test = as.character(geometry)), test == "list()"))
    
    if (nEntSansGeom > 0) {
        rapport("Nombre d'entités sans géométrie :", nEntSansGeom, "soit",
                nEntSansGeom/nrow(base)*100 %>% round(2), "%")
    } else {
        rapport("Aucune entité sans géométrie à l'issue du traitement. :)", info=T)
    }
    
    return(base)
}

# Problème avec des codes en 9
patch_codeZF = function(base)
{
    base[which(nchar(base$ZF) == 9),]$ZF = paste(base[which(nchar(base$ZF) == 9),]$uid_ENQ,
                                                 base[which(nchar(base$ZF) == 9),]$ZF)
    return(base)
}

# Manipulations de la géométrie =====================================================================

# Charge les ZT et les ZTS reconstituées dans une même base shp_ZT :
chargerZTetZTS = function() {
  rapport("Chargement de la base géographique ZT avec les ZT reconstituées...")
  load("Data/shp_ZT.rds")
  load("Data/shp_ZTS.rds")
  
  shp_ZTS = shp_ZTS %>% rename(ZT = ZTS)
  
  shp_ZT  = select(shp_ZT,  uid_ENQ, CODE_SEC) %>% mutate(ZT = paste0(uid_ENQ, CODE_SEC)) %>% select(uid_ENQ, ZT) %>%
    st_transform(crs = 3857)
  shp_ZTS = select(shp_ZTS, uid_ENQ, ZT)
  
  shp_ZT = rbind(shp_ZT, shp_ZTS)
  
  return(shp_ZT)
}

# Associe un code_zf avec une zf :
calculer_pointZF = function(base, shp_ZF, PGT, champZF = "ZF", champUid)
{
  colnames(base)[colnames(base) == champZF] = "champZF"
  colnames(base)[colnames(base) == champUid] = "champUid"
  
  PGT = mutate(PGT, ZF = paste(ENQ, ZF)) %>% st_transform(crs = 2154)
  
  base_PGT = left_join(select(base, champUid, champZF),
                       select(PGT, ZF, geometry), by=c("champZF" = "ZF"))
  base_ZF  = left_join(select(base, champUid, champZF),
                       select(shp_ZF, CODE_ZF, geometry), by=c("champZF" = "CODE_ZF")) %>%
    st_as_sf() %>%
    st_point_on_surface()
  
  base = left_join(base, left_join(select(base_PGT, champUid, geometry),
                                   select(base_ZF, champUid, geometry),
                                   by = c("champUid" = "champUid"),
                                   suffix = c(".pgt", ".zf")), by=c("champUid" = "champUid"))
  
  base$geometry = ifelse(st_is_empty(base$geometry.pgt), base$geometry.zf, base$geometry.pgt)
  base = select(base, -geometry.zf, -geometry.pgt)
  base = st_as_sf(base)
  
  colnames(base)[colnames(base) == "champZF"] = champZF
  colnames(base)[colnames(base) == "champUid"] = champUid
  
  return(base)
}

calculer_lieuTravail = function(base, ACT, typo=F, PER = NULL, DEP = NULL)
{
  if (typo)
  {
    # Nécessaire de charger les fonctions de bases de données
    source("BASES.R", print.eval=T)
  }
  
  rapport("Calcul du lieu de travail principal de l'échantillon d'enquêté⋅es fourni")
  
  # On filtre ACT pour ne porter que sur la base traitée
  ACT = filter(ACT, uid_PER %in% base$uid_PER)
  
  # On regarde sur quel lieu de travail la personne a passé le plus de temps dans la journée
  ACT.Trav = filter(ACT, Tache %in% as.character(c(100:109))) %>% group_by(uid_PER, l) %>%
    summarize(du = sum(du), .groups="drop_last") %>%
    group_by(uid_PER) %>%
    summarize(ZF_travMax = unique(l)[which.max(du)]) %>%
    ztzf(colZF = "ZF_travMax", colZT = "ZT_travMax")
  
  # On attribue ces données à la table fournie
  base = left_join(base, ACT.Trav, by=c("uid_PER" = "uid_PER"))
  
  if (typo)
  {
    if(is.null(PER) | is.null(DEP)) { stop("Fournir les bases PER et DEP à calculer_lieuTravail") }
    
    # On va ajouter une colonne "communes" et les ZoneRang, ZonePosi et ZoneDens
    # correspondant⋅es
    
    # correspondance zf → com
    zf2com = charger_tableZF2Com(PER = PER, DEP = DEP)
    
    # correspondance com → typologies
    load("Data/typoCom.rds")
    zf2com = left_join(zf2com, st_drop_geometry(typoCom), by= c("Com" = "COM"))
    
    # jointure avec la base
    base = left_join(base, zf2com, by=c("ZF_travMax" = "ZF"), suffix=c("", "_travMax"))
  }
  
  return(base)
}

densitesZversPER = function(PER)
{
  rapport("Import des densités par secteur dans la base personnelle")
  
  # On charge les tableaux qui, en principe, comportent la densité
  load("Data/shp_ZF.rds")
  load("Data/shp_COM.rds")
  
  # On joint la densité au lieu de résidence…
  # Si disponible au niveau ZF, on l'utilise
  PER_zt_zf = PER %>%
    left_join(select(shp_ZF, CODE_ZF, densite), by=c("ZF" = "CODE_ZF")) %>%
    filter(!is.na(densite))
  # Sinon, on utilise la densité communale
  PER_zt_com = PER %>%
    left_join(select(shp_COM, insee, densite), by=c("Com" = "insee")) %>%
    filter(!ZF %in% PER_zt_zf$ZF)
  # On joint le tout
  PER_zt = rbind(PER_zt_zf, PER_zt_com) %>% 
    mutate(etiqLog = classesDensites(densite)) %>%
    filter(Dis>0)
  
  # Maintenant, on fait la même chose depuis le lieu de travail
  PER_zt_zf = PER %>%
    left_join(select(shp_ZF, CODE_ZF, densite), by=c("ZF_travMax" = "CODE_ZF")) %>%
    filter(!is.na(densite))
  PER_zt_com = PER %>%
    left_join(select(shp_COM, insee, densite), by=c("Com_travMax" = "insee")) %>%
    filter(!ZF %in% PER_zt_zf$ZF)
  PER_zt_trav = rbind(PER_zt_zf, PER_zt_com) %>%
    mutate(etiqLog = classesDensites(densite)) %>%
    filter(Dis>0)
  
  # On renomme de part et d'autre pour joindre ensuite ensemble
  PER_zt = rename(PER_zt, dsDom = densite, dsDomEtq = etiqLog)
  PER_zt_trav = rename(PER_zt_trav, dsTvl = densite, dsTvlEtq = etiqLog)
  
  # On joint ensemble
  PER = left_join(PER, select(PER_zt, uid_PER, dsDom, dsDomEtq), by="uid_PER")
  PER = left_join(PER, select(PER_zt_trav, uid_PER, dsTvl, dsTvlEtq), by="uid_PER")
  
  # Prêt
  return(PER)
}



ztzf = function(table, colZF = "ZF", colZT = "ZT", suppr99 = T)
{
  colnames(table)[colnames(table) == colZF] = "ZF"
  
  # On entre une table avec la colonne "ZF", ça sort avec la colonne ZT
  
  if (!"DEP" %in% ls()) { load("Data/DEP.rds") }
  
  # On crée la table correzZTZF
  corresZTZF  = DEP %>% mutate(O_ZT = paste(uid_ENQ, O_ZT)) %>% group_by(ZF = O_ZF) %>% summarize(ZT = first(O_ZT))
  corresZTZF2 = DEP %>% mutate(D_ZT = paste(uid_ENQ, D_ZT)) %>% group_by(ZF = D_ZF) %>% summarize(ZT = first(D_ZT))
  corresZTZF = rbind(corresZTZF, corresZTZF2) %>% group_by(ZF) %>% summarize(ZT = first(ZT)) ; remove(corresZTZF2)
  corresZTZF = corresZTZF %>% mutate(ZT = paste0(substr(ZT, 1, 7), substr(ZT, 9, 12)))
  
  # Optionnellement, on supprime le secteur indéfini
  if (suppr99)
  {
    corresZTZF = corresZTZF %>% mutate(ZT = ifelse(substr(ZT, 8,12) == "9999", NA, ZT))
  }
  
  # On joint
  table = left_join(table, corresZTZF, by=c("ZF" = "ZF"))
  
  colnames(table)[colnames(table) == "ZF"] = colZF
  colnames(table)[colnames(table) == "ZT"] = colZT
  
  return(table)
}


purgeSfc = function(t){ # Permet de s'assurer qu'aucune colonne Sfc ne vient faire tout buguer
  lCol = sapply(t, function(x) {"sfc" %in% class(x)})
  lCol = lCol[lCol == T]
  lCol = names(lCol)
  
  t = t %>% select(-all_of(lCol))
  return(t)
}

# Diagrammes =====

# Pour à ajouter à un ggplot pour neutraliser les axes
ggRetirerAxeX = theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
ggRetirerAxeY = theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Pour faire des jolis graphes en barre sans s'embêter à paramétrer GGplot, la fonction ci-dessous est conçue pour être tout-terrain
# - importer un graphe au format étendu (si tableau croisé brut, indiquer dejaMelt = F pour la transformation)
# - les étiquettes servent à identifier les valeurs à retrouver sur l'axe Y, laisser vide pour déduire du tableau
# - réglage titre, sous-titre, étiquettes des axes X et Y, étiquette de la légende
# - couleurs : indiquer une palette complète, une seule couleur (barres unicolores), deux couleurs (dégradé) ou rien (par défaut)
# - yminmax : indiquer le min et le max sur l'axe Y, les valeurs qui sortent de cet intervalle sont supprimées par ggplot
# - stack : indique s'il faut "empiler" les valeurs correspondants aux facteurs de la 2e dimension du tableau, ou répartir en facets
# - stackdim : avec un tableau croisé en 3 dimensions, empile les 2 premières et crée des facets avec la troisième
# - log : passe l'axe Y en échelle pseudologarithmique (nécessite un package spécifique)
# - val.xval, val.dim permettent de renommer les valeurs des 1e et 3e dimensions à l'aide d'un vecteur texte
# - print : en le désactivant, la fonction ne trace pas le graphique mais le fournit en sortie (on peut donc écrire g = viz_Barres())
# - verbose active les sorties textes de débogage (la fonction va être très bavarde sur la console)
# - legende.pos permet de repositionner la légende directement : accepte "right", "left", "bottom" ou "top" (natif ggplot)
viz_Barres = function(grph,
                      valeurs = NULL, var1 = NULL, var2 = NULL, var3 = NULL,
                      etiquettes = NULL, titre = "Graphique", soustitre = "", xlab = "", ylab = "", legende = "", couleurs = "def",
                      yminmax = NULL, dejaMelt = T, reverse = F,
                      unicol = F, stack=F, dodge=F, stackdim=F, facetdim=F, log=F,
                      etiquettes.renom = NULL, lignes=NULL, legendelignes = NULL,
                      val.xval = NULL, val.dim = NULL, print = T, verbose = F, legende.pos="right",
                      empecherMultipage=F, comparer = F, legendeComparer = NULL, etiqueterBarres = F,
                      vCramer = F)
{
    total = F
    library(reshape2)
    # library(DescTools)
    
    limFlip = 6 # à baisser pour faire se retourner avec moins de catégos (à l'origine était de 11)
    
    if (dodge) {limFlip = 0}
    
    if(length(couleurs) == 1) {if (couleurs == "") {couleurs = "def"}} # finalement plus aisé à manipuler
    
    if (stackdim == T) {stack = T ; warning("Stack activé pour pouvoir utiliser Stackdim")}
    
    if (reverse == T) {
        grph = Rev(grph, 1)
    }
    
    if (is.null(valeurs)){ # Ancien fonctionnement
        if (dejaMelt == F) {
            if (is.null(etiquettes)) {
                stop("Si le tableau n'est pas déjà sous format melt, la liste des étiquettes est requise")
            }
            colnames(grph) = c("xval", etiquettes)
            if (length(etiquettes)>1) {
                grphMelt = melt(grph, id.vars = "xval", measure.vars=etiquettes[1:length(etiquettes)])
            }
            else {
                grphMelt = melt(grph, id.vars = "xval", measure.vars=etiquettes)
            }
            grphMelt$variable = as.factor(grphMelt$variable)
        }
        else {
            grphMelt = grph
            if (unicol == F) {
                if (stackdim == T & facetdim == F){
                    listeCol = c("dim", "variable", "xval", "value")}
                if (facetdim == T & stackdim == F){
                    listeCol = c("xval", "variable", "dim", "value")}
                if (stackdim == F & facetdim == F) {
                    listeCol = c("xval", "variable", "value")}
            }
            else {
                listeCol = c("variable", "value")
            }
            if (comparer == T) {listeCol = c(listeCol, "compare")}
            if (verbose == T) {cat(paste0("\nVizBarres >> Champs : ", paste(listeCol, collapse=", "), " "))}
            colnames(grphMelt) = listeCol
            grphMelt$variable = as.factor(grphMelt$variable)
            if (is.null(etiquettes)){
                etiquettes = levels(grphMelt$variable)
            }
            levels(grphMelt$variable) = etiquettes
        }
    }
    else # Nouveau fonctionnement, plus clair (mars 2022)
    {
        if(!is.null(valeurs) & !is.null(var1) & is.null(var2) & is.null(var3))
        {
            if (length(valeurs) == length(var1))
            {
                grphMelt = data.frame(variable = grph[[var1]], value = grph[[valeurs]])
                unicol = T
            } else {stop("Arguments fournis à viz_Barres invalides")}
        }
        if(!is.null(valeurs) & !is.null(var1) & !is.null(var2) & is.null(var3))
        {
            if (length(valeurs) == length(var1) & length(var1) == length(var2))
            {
                grphMelt = data.frame(xval = grph[[var2]], variable = grph[[var1]], value = grph[[valeurs]])
            } else {stop("Arguments fournis à viz_Barres invalides")}
        }
        if(!is.null(valeurs) & !is.null(var1) & !is.null(var2) & !is.null(var3))
        {
            if (length(valeurs) == length(var1) & length(var1) == length(var2) & length(var2) == length(var3))
            {
                if (stackdim == T & facetdim == F) {
                    grphMelt = data.frame(dim = grph[[var3]], variable = grph[[var1]],
                                          xval = grph[[var2]], value = grph[[valeurs]])
                }
                if (stackdim == F & facetdim == T) {
                    grphMelt = data.frame(xval = grph[[var3]], variable = grph[[var1]],
                                          dim = grph[[var2]], value = grph[[valeurs]])
                }
                if (stackdim == F & facetdim == F) {
                    stop("viz_Barres: Préciser stackdim == T ou facetdim == T")
                }
            } else {stop("Arguments fournis à viz_Barres invalides")}
        }
        grphMelt$variable = as.factor(grphMelt$variable)
        if (is.null(etiquettes)){
            etiquettes = levels(grphMelt$variable)
        }
        levels(grphMelt$variable) = etiquettes
    }
    
    
    
    if (!is.null(etiquettes.renom)) {
        if(verbose) {cat(">> Remplacement des étiquettes :",
                         paste(levels(grphMelt$variable), collapse=", "),
                         "par :",
                         paste(etiquettes.renom, collapse=", "))}
        levels(grphMelt$variable) = etiquettes.renom
    }
    grphMelt$value = as.double(grphMelt$value)
    
    if (!unicol) {
        if (nrow(filter(grphMelt, xval == "Total"))>0) # Détection automatique d'un Total en variable 2
        {
            grphTotal = select(filter(grphMelt, xval == "Total"), variable, value)
            colnames(grphTotal) = c("variable", "total")
            grphMelt = filter(grphMelt, xval != "Total") %>% # on passe le total en une nouvelle colonne
                left_join(grphTotal, by=c("variable" = "variable"))
            soustitre = paste(c(soustitre,
                                paste0("Les rectangles gris représentent la répartition totale de la variable ", legende)), collapse="\n")
        }
    }
    
    if (!is.null(yminmax) & unicol == F){
        # Retirer les lignes dont un des caractères sort de la zone
        retirer = filter(grphMelt, value > yminmax[2])$xval
        # grphMelt = filter(grphMelt, !xval %in% retirer)
        # temporaire : remplacer par le maximum
        grphMelt = mutate(grphMelt, value = ifelse(value > yminmax[2], yminmax[2], value))
        # if(length(retirer) > 0) {
        #   soustitre = paste0(ifelse(soustitre == "", "", paste0(soustitre, "\n")),
        #                      "Les éléments suivants n'ont pas été dessinés (valeurs excédant la zone de tracé) : ",
        #                      paste(retirer, collapse=", "))
        # }
    }
    
    if (!is.null(val.xval)) {
        if (verbose==T) {
            cat("\nVizBarres >> ") ; cat(paste(levels(grphMelt$xval), collapse = ", ")) ; cat(" à remplacer par ") ;
            cat(paste(c(val.xval), collapse = ", "))}
        if (length(levels(grphMelt$xval)) != length(val.xval)){cat("!!! ERREUR") ; print(grphMelt)}
        levels(grphMelt$xval) = val.xval}
    if (!is.null(val.dim )) {levels(grphMelt$dim ) = val.dim }
    
    if (etiqueterBarres == T)
    {
        limiteinf = 0.5*(max(grphMelt$value) - min(grphMelt$value))
        grphMelt$hjust = ifelse(grphMelt$value < limiteinf, 0, 1)
    }
    
    if (!unicol) {
        g = ggplot(data = grphMelt, aes(x = xval, y = value)) + theme_bw()
    }
    else {
        g = ggplot(data = grphMelt, aes(x = variable, y = value)) + theme_bw() 
    }
    
    if(length(couleurs) > 1) {
        if(length(couleurs) == 2 & length(levels(grphMelt$variable)) != 2) {
            cliste = scales::seq_gradient_pal(couleurs[1], couleurs[2],"Lab")(seq(0,1,length.out=length(levels(grphMelt$variable))))
            g = g + scale_fill_manual(name = legende, values = cliste) + theme(legend.position=legende.pos)
        }
        else {
            g = g + scale_fill_manual(name = legende, values = couleurs) + theme(legend.position=legende.pos)
        }
    }
    else {
        if(couleurs != "def") {
            g = g + theme(legend.position="none")
        }
        else{
            g = g + scale_fill_hue(name = legende)  + theme(legend.position=legende.pos)
        }
    }
    g = g + labs(title = titre, caption = soustitre)
    if (log == F) {
        g = g + scale_y_continuous(name = ylab, limits = yminmax, labels=scales::comma)
    } else {
        g = g + scale_y_continuous(name = ylab, trans = pseudolog10_trans)
    }
    
    if (stack == F & unicol == F) {
        if (dodge == F & facetdim == T) {g = g + facet_grid(dim ~ xval)}
        if (dodge == F & facetdim == F)    {g = g + facet_wrap(~ xval)}
        
        if (dodge == F)
        {
            if (length(couleurs) > 1) {
                g = g + geom_col(aes(x = variable, y=value, fill = variable)) +
                    theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
            }
            else {
                if(couleurs != "def") {
                    g = g + geom_col(aes(x = variable, y=value),fill = couleurs) + theme(axis.text.x = element_text(angle = 45, hjust=1))
                }
                else {
                    g = g + geom_col(aes(x = variable, y=value, fill = variable)) +
                        theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
                }
            }
            if(total == T)
            { g = g + geom_bar(aes(x= variable, y= total), stat="identity", fill=NA, col="grey80") }
            if (length(unique(grphMelt$xval))>limFlip + 4) {
                g = g + coord_flip() # flip si n.cat > 12
            }
            if (etiqueterBarres == T)
            {
                g = g + geom_text(aes(x= variable, y= value, label=paste0(round(value,1), " "),
                                      hjust = hjust), angle=90, size=3.5)
            }
        } else {
            if (length(couleurs) > 1) {
                g = g + geom_col(aes(x = variable, y=value, fill = variable, alpha = xval),
                                 position = "dodge") +
                    theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
            }
            else {
                if(couleurs != "def") {
                    g = g + geom_col(aes(x = variable, y=value, alpha = xval), fill = couleurs,
                                     position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust=1))
                }
                else {
                    g = g + geom_col(aes(x = variable, y=value, fill = variable, alpha = xval),
                                     position = "dodge") +
                        theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
                }
            }
            if(total == T)
            { g = g + geom_bar(aes(x= variable, y= total, alpha = xval), stat="identity", fill=NA, col="grey80") }
            if (length(unique(grphMelt$xval))>limFlip + 4) {
                g = g + coord_flip() # flip si n.cat > 12
            }
            if (etiqueterBarres == T)
            {
                warning("Désactiver l'argument Dodge pour tracer les étiquettes")
            }
            g = g + scale_alpha_manual(name="", values= rev(seq(.4, 1, .6/length(unique(grphMelt$xval)))))
        }
    }
    if (stack == T & unicol == F) {
        g = g + geom_col(aes(x = xval, y=value, fill = variable), position = position_stack(reverse = !reverse))
        if(unicol == F) {
            if (length(unique(grphMelt$xval))>limFlip) {
                g = g + coord_flip() # flip si n.cat > 12
            }
            g = g + scale_x_discrete(name = xlab)
        }
        if(stackdim == T) {
            g = g + facet_wrap(~ dim)
        }
    }
    if (unicol == T) {
        
        if (length(unique(grphMelt$variable))>limFlip) {
            g = g + coord_flip() # flip si n.cat > 12
        }
        
        if(length(couleurs) == 1) {
            if (couleurs != "def") {
                g = g + geom_bar(aes(x = variable, y=value), stat="identity", fill = couleurs) +
                    scale_x_discrete(name = xlab) +
                    theme(axis.text.x = element_text(angle = 45, hjust=1))
            }
            if (couleurs == "def") {
                g = g + geom_col(aes(x = variable, y=value, fill = variable), position = position_stack(reverse = !reverse)) +
                    theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
            }
        }
        else {
            g = g + geom_col(aes(x = variable, y=value, fill = variable), position = position_stack(reverse = !reverse)) +
                theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
        }
        
    }
    
    if (!is.null(lignes)){
        if (is.null(legendelignes)){
            g = g + geom_hline(yintercept = lignes, color = "slateblue")
        } else {
            g = g + geom_hline(aes(yintercept = lignes[1], color = "slateblue")) +
                scale_color_identity(breaks = c("slateblue"), labels = c(legendelignes), guide="legend", name = "")
            for (i in 2:(length(lignes))){
                g = g + geom_hline(yintercept = lignes[i], color = "slateblue") # show.legend = ifelse(i == 1, T, F)
            }
            #g = g + scale_color_identity(breaks = "slateblue", labels = legendelignes)
            #guide = guide_legend(override.aes = list(color = c(rep("slateblue", length(lignes))))))
        }
    }
    if (comparer == T)
    {
        g = g + geom_bar(aes(x= variable, y= compare, linetype = "solid"), stat="identity", fill=NA, col="grey65")
        if (!is.null(legendeComparer))
        {
            g = g + scale_linetype_identity(breaks = c("solid"), labels = c(legendeComparer), guide="legend", name = "")
        }
    }
    if (print==T)
    {print(g)} # traçage
    else { return(g) } # sinon, la fonction renvoie le contenu du graphisme
}



viz_DispersionSelonVariable = function(table, champContinu, champDiscret, champPoids, legendeContinu, legendeDiscret, titre="",
                                       facteurDiv = 1, etiquettes = NULL, compterZeros=T, tracerZeros=F, inverserZeros=F, legZeros="",
                                       champContinu2 = NULL, legendeContinu2, facteurDiv2 = 1,
                                       verbose=F, view=F, zerosCommeBarres = F, capt = "",
                                       champCateg = NULL, layoutVertical = T, layoutReajust = 0.1,
                                       compterPropCateg = F, plancher = F,
                                       méthodeCalcul = 2)
{
    table = purgeSfc(table) %>% filter(!is.na(pull(table, var=champPoids)))
    
    g1 = NULL ; g2 = NULL ; g3 = NULL ; g4 = NULL ; g5 = NULL #initialisation des variables
    
    champs2 = ifelse(is.null(champContinu2), F, T)
    champContinu1 = champContinu ; facteurDiv1 = facteurDiv ; legendeContinu1 = legendeContinu
    
    if (is.null(zerosCommeBarres)) { zerosCommeBarres = champs2 } # Si 2 champs, par défaut les zéros seront des barres
    # Mais si l'utilisateur.rice spécifie, quel que soit le champ 2, son instruction prend le pas
    
    if (verbose == T){
        cat("DISPERSION.SELON.VARIABLE =====\n")
        cat("Nature des variables en entrée :\n") ; str(table)}
    
    library(matrixStats) ; library(cowplot)  #  ; library(DescTools)
    listeVal = sort(unique(as.character(table[[champDiscret]])))
    
    passes = ifelse(champs2 == F, 1, 2)
    
    if (méthodeCalcul == 2)
    {
        # Pour faciliter l'affaire, on renomme les colonnes
        colnames(table)[colnames(table) == champContinu1] = "chpContinu1"
        colnames(table)[colnames(table) == champContinu2] = "chpContinu2"
        colnames(table)[colnames(table) == champDiscret ] = "chpDiscret"
        colnames(table)[colnames(table) == champPoids   ] = "chpPoids"
        colnames(table)[colnames(table) == champCateg   ] = "chpCateg"
    }
    
    
    if (méthodeCalcul == 1) {    b = ui_ProgInit(length(listeVal) * passes)}
    
    for (h in 1:passes){
        
        if(h == 1){champContinu = champContinu1; facteurDiv = facteurDiv1; legendeContinu = legendeContinu1}
        if(h == 2){champContinu = champContinu2; facteurDiv = facteurDiv2; legendeContinu = legendeContinu2}
        
        if (is.null(champCateg)) { nCol = 11 } else { nCol = 12 }
        
        if (méthodeCalcul == 1)
        {
            matrice = rep("", nCol)
            for(i in 1:length(listeVal))
            {
                ui_Prog(b, (length(listeVal) * (h - 1)) + i)
                
                if (is.null(champCateg))
                {
                    
                    if (compterZeros == F) {
                        sousTable = filter(table, pull(table, var=champContinu) > 0)
                    } else {sousTable = table}
                    
                    sousSousTable = filter(sousTable, pull(sousTable, var=champDiscret) == listeVal[i]) 
                    
                    qua = quantile(as.numeric(sousSousTable[[champContinu]]),
                                               
                                               na.rm=T, probs=c(.1, .25, .5, .75, .9))
                    moy = weighted.mean(sousSousTable[[champContinu]],
                                       w= sousSousTable[[champPoids]], na.rm=T)
                    
                    testP = filter(table, pull(table, var=champDiscret) == listeVal[i])
                    testZ = filter(testP, pull(testP, var=champContinu) == 0)
                    testNA= filter(testP, is.na(pull(testP, var=champContinu)))
                    
                    partZ =  sum(testZ$CoeffEnq,  na.rm=T) / sum(testP$CoeffEnq, na.rm=T)  * 100
                    partNA = sum(testNA$CoeffEnq, na.rm=T) / sum(testP$CoeffEnq, na.rm=T)  * 100
                    
                    N = nrow(sousSousTable)
                    Pop = sum(sousSousTable[[champPoids]], na.rm=T)
                    
                    matrice = rbind(matrice, c(listeVal[i], qua, moy, partZ, partNA, N, Pop))}
                
                else {
                    
                    if (!is.factor(table[[champCateg]])) { stop("Le champ Catégorie doit être un facteur.") }
                    
                    Categs = c(levels(table[[champCateg]]))
                    
                    for (k in 1:length(Categs))
                    {
                        
                        if (compterZeros == F) {
                            sousTable = filter(table, pull(table, var=champContinu) > 0)
                        } else {sousTable = table}
                        
                        sousSousTable = filter(sousTable, pull(sousTable, var=champDiscret) == listeVal[i])
                        sousSousTable = filter(sousSousTable, pull(sousSousTable, var=champCateg) == Categs[k])
                        
                        # if (verbose == T & i == 1 & k == 1)
                        # {
                        #     cat("\nInfos pour la catég. 1, valeur discrète 1 :\n")
                        #     str(sousSousTable[[champContinu]])
                        # }
                        
                        qua =   quantile(as.numeric(sousSousTable[[champContinu]]),
                                                    #weights= as.numeric(sousSousTable[[champPoids]]),
                                                    na.rm=T, probs=c(.1, .25, .5, .75, .9))
                        moy = weighted.mean(sousSousTable[[champContinu]],
                                           w= sousSousTable[[champPoids]], na.rm=T)
                        
                        testP = filter(table, pull(table, var=champDiscret) == listeVal[i])
                        testP = filter(testP, pull(testP, var=champCateg)   == Categs[k])
                        testZ = filter(testP, pull(testP, var=champContinu) == 0)
                        testNA= filter(testP, is.na(pull(testP, var=champContinu)))
                        
                        partZ =  sum(testZ$CoeffEnq,  na.rm=T) / sum(testP$CoeffEnq, na.rm=T)  * 100
                        partNA = sum(testNA$CoeffEnq, na.rm=T) / sum(testP$CoeffEnq, na.rm=T)  * 100
                        
                        N = nrow(sousSousTable)
                        Pop = sum(sousSousTable[[champPoids]], na.rm=T)
                        
                        matrice = rbind(matrice, c(listeVal[i], qua, moy, partZ, partNA, N, Pop, Categs[k]))
                        
                    }
                }
            }
        }
        if (méthodeCalcul == 2)
        {
            if (h == 1) { table = rename(table, chpContinu = chpContinu1) }
            if (h == 2) { table = rename(table, chpContinu1 = chpContinu, chpContinu = chpContinu2)}
            
            if (! "chpCateg" %in% colnames(table))
            {
                # D'abord, on calcule la part des 0
                m1 = table %>% mutate(zero = ifelse(chpContinu == 0, 1, 0)) %>%
                    group_by(zero, chpDiscret) %>% summarize(p = sum(chpPoids, na.rm=T), .groups="drop") %>%
                    pivot_wider(names_from = zero, names_prefix = "zero", values_from=p) %>% rowwise() %>%
                    mutate(Pop = sum(zero0, zero1, zeroNA), pZero = zero1 / Pop * 100, pNA = zeroNA / Pop * 100)
                
                m2 = table %>% dplyr::count(chpDiscret)
                
                # Ensuite, on calcule les moyennes et quartiles
                if (compterZeros) { sT = table } else { sT = filter(table, chpContinu != 0) }
                
                m3 = sT %>% mutate(chpContinu = as.numeric(chpContinu), chpPoids = as.numeric(chpPoids)) %>%
                    group_by(chpDiscret) %>%
                    summarize(q10 = quantile(chpContinu,  na.rm=T, probs=.1),
                              q25 = quantile(chpContinu,  na.rm=T, probs=.25),
                              Med = quantile(chpContinu,  na.rm=T, probs=.5),
                              q75 = quantile(chpContinu,  na.rm=T, probs=.75),
                              q90 = quantile(chpContinu,  na.rm=T, probs=.9),
                              Moy = weighted.mean(chpContinu, chpPoids, na.rm=T))
                
                matrice = left_join(m1, m2, by=c("chpDiscret" = "chpDiscret")) %>%
                    left_join(    m3, by=c("chpDiscret" = "chpDiscret")) %>%
                    select(chpDiscret, q10, q25, Med, q75, q90, Moy, pZero, pNA, n, Pop)
            }
            if ("chpCateg" %in% colnames(table))
            {
                # D'abord, on calcule la part des 0
                m1 = table %>% mutate(zero = ifelse(chpContinu == 0, 1, 0)) %>%
                    group_by(zero, chpCateg, chpDiscret) %>% summarize(p = sum(chpPoids, na.rm=T), .groups="drop") %>%
                    pivot_wider(names_from = zero, names_prefix = "zero", values_from=p) %>% rowwise() %>%
                    mutate(Pop = sum(zero0, zero1, zeroNA), pZero = zero1 / Pop * 100, pNA = zeroNA / Pop * 100)
                
                m2 = table %>% dplyr::count(chpCateg, chpDiscret)
                
                # Ensuite, on calcule les moyennes et quartiles
                if (compterZeros) { sT = table } else { sT = filter(table, chpContinu != 0) }
                
                m3 = sT %>% mutate(chpContinu = as.numeric(chpContinu), chpPoids = as.numeric(chpPoids)) %>%
                    group_by(chpCateg, chpDiscret) %>%
                    summarize(q10 = quantile(chpContinu, na.rm=T, probs=.1),   # DescTools débranché, out of the way
                              q25 = quantile(chpContinu, na.rm=T, probs=.25),
                              Med = quantile(chpContinu, na.rm=T, probs=.5),
                              q75 = quantile(chpContinu, na.rm=T, probs=.75),
                              q90 = quantile(chpContinu, na.rm=T, probs=.9),
                              Moy = weighted.mean(chpContinu, chpPoids, na.rm=T), .groups="drop")
                
                matrice = left_join(m1, m2, by=c("chpDiscret" = "chpDiscret", "chpCateg" = "chpCateg")) %>%
                    left_join(    m3, by=c("chpDiscret" = "chpDiscret", "chpCateg" = "chpCateg")) %>%
                    select(chpDiscret, q10, q25, Med, q75, q90, Moy, pZero, pNA, n, Pop, chpCateg)
            }
        }
        
        # if (h == passes) {ui_Prog(b, length(listeVal))} # je mets la barre à 100%
        
        if(méthodeCalcul == 1)
        {
            matrice = as.data.frame(matrice[2:nrow(matrice),])
        }
        
        stats = c("q10", "q25", "Med", "q75", "q90", "Moy", "Part_0", "Part_NA", "N", "Pop")
        
        if (!is.null(champCateg)) {stats = c(stats, "Categ")}
        
        colnames(matrice) = c("Mode", stats)
        
        if(méthodeCalcul == 1) {
            matrice[,2] = as.numeric(matrice[,2]) / facteurDiv
            matrice[,3] = as.numeric(matrice[,3]) / facteurDiv
            matrice[,4] = as.numeric(matrice[,4]) / facteurDiv
            matrice[,5] = as.numeric(matrice[,5]) / facteurDiv
            matrice[,6] = as.numeric(matrice[,6]) / facteurDiv
            matrice[,7] = as.numeric(matrice[,7]) / facteurDiv
            matrice[,8] = as.numeric(matrice[,8])
            matrice[,9] = as.numeric(matrice[,9])
            matrice[,10]= as.numeric(matrice[,10])
            matrice[,11]= as.numeric(matrice[,11])
            if (!is.null(champCateg)) { matrice[,12] = as.factor(matrice[,12]) }
        }
        
        if(méthodeCalcul == 2) {
            matrice = matrice %>%
                mutate(across(starts_with(c("q", "Med", "Moy")), ~ . / facteurDiv))
        }
        
        matrice$Mode = ifelse(is.na(matrice$Mode), "NA", as.character(matrice$Mode)) %>% as.factor()
        matrice = filter(matrice, !is.na(Med))
        
        if (compterPropCateg == T) {
            if(méthodeCalcul == 1){
                poptot = sum(table[[champPoids]], na.rm=T)
                indexDiscret = which(colnames(table) == champDiscret)
                indexPoids   = which(colnames(table) == champPoids)
                
                tabDiscr = table %>% select(indexDiscret, indexPoids)
                colnames(tabDiscr) = c("champ", "p")
                tabDiscr = tabDiscr %>% group_by(champ) %>% summarize(p = sum(p) / poptot)
                
                if (!is.null(champCateg)) {
                    indexCateg = which(colnames(table) == champCateg)
                    
                    tabCateg = table %>% select(indexCateg, indexPoids)
                    colnames(tabCateg) = c("champ", "p")
                    tabCateg = tabCateg %>% group_by(champ) %>% summarize(p = sum(p) / poptot)
                    
                    matrice[,12] = as.character(matrice[,12])
                }
                
                if (verbose == T) { cat("\nTable des totaux\n") ; print (tabDiscr)
                    if(!is.null(champCateg)) {print (tabCateg)}}
                
                for (lmat in 1:nrow(matrice))
                {
                    eDiscr = as.character(matrice[lmat,1])
                    pDiscr = tabDiscr[as.character(tabDiscr$champ) == eDiscr,]$p[1]
                    matrice[lmat,1] = paste0(matrice[lmat,1], "\n(", round(pDiscr*100,1), " %)")
                    
                    if (!is.null(champCateg)) {
                        eCateg = as.character(matrice[lmat,12])
                        pCateg = tabCateg[as.character(tabCateg$champ) == eCateg,]$p[1]
                        matrice[lmat,12] = paste0(matrice[lmat,12], " (", round(pCateg*100,1), " %)")
                        if (is.na(matrice[lmat,12])) {
                            stop (paste0("Erreur lors du calcul de la part des catégories. Index ", lmat,
                                         ", eCateg = ", eCateg, ", pCateg = ", pCateg, "."))}
                    }
                }
                
                if (!is.null(champCateg)) {matrice[,12] = as.factor(matrice[,12])}
            }
            if (méthodeCalcul == 2)
            {
                pM = matrice %>% select(Mode, Pop) %>%
                    group_by(Mode) %>% summarize(pMode = sum(Pop)) %>%
                    group_by(tout = 1) %>% mutate(partMode = pMode / sum(pMode) * 100) %>%
                    ungroup() %>% select(Mode, partMode)
                
                matrice = left_join(matrice, pM, by=c("Mode" = "Mode")) %>%
                    mutate(Mode = paste0(Mode, " (", round(partMode, 1), " %)")) %>%
                    mutate(Mode = as.factor(Mode)) %>% select(-partMode)
            }
            legendeDiscret = paste(legendeDiscret, "(part pop. tot. en %)")
        }
        
        for (lmat in 1:nrow(matrice))
        {
            if(matrice[lmat,10] < 6) {
                # S'il y a moins de 5 enquêté.es sur la ligne, je passe tout en NA
                matrice[lmat,2:9] = NA
            }
        }
        
        if (verbose==T)
        {
            Str(matrice)
            if (!view) { print(matrice) }
            if (view) { View(matrice) }
        }
        
        if (!is.null(etiquettes) )
        {
            matrice$Mode = plyr::revalue(matrice$Mode, etiquettes, warn_missing = F)
        }
        
        Leg = ifelse(compterZeros, "", "Parmi les déplacements mesurés")
        
        legendeContinu = ifelse(compterZeros, legendeContinu, paste0(legendeContinu, ", hors valeurs nulles"))
        
        g =ggplot(matrice) +
            geom_linerange(aes(y=Mode,  xmin=q10, xmax=q90, col="80% enquêté.es")) +
            geom_linerange(aes(y=Mode,  xmin=q25, xmax=q75, col="Médiane / 50% enquêté.es")) +
            geom_point(aes(y=Mode, x=Med, col="Médiane / 50% enquêté.es")) +
            geom_text(aes(y=Mode, x=Med, label=round(Med,1)), vjust=-0.5, hjust=1, colour="firebrick", size=3) +
            geom_point(aes(y=Mode, x=Moy, col="Moyenne")) + 
            geom_text(aes(y=Mode, x=Moy, label=round(Moy,1)), vjust=-0.5, hjust=0,  colour="slateblue", size=3) +
            scale_y_discrete(limits = rev) +
            ggtitle(Leg)
        
        if (is.null(champCateg)) {}
        else { g = g +
            facet_grid(. ~ Categ)
        }
        
        
        g = g + xlab(legendeContinu) + ylab(legendeDiscret) +
            scale_colour_manual(values=c("Médiane / 50% enquêté.es"="firebrick", "Moyenne"="slateblue",
                                         "80% enquêté.es"="orange"), name=Leg) +
            theme(legend.position="none", legend.box = "horizontal")
        
        
        if (h == 1) {g1 = g}
        if (h == 2) {g3 = g}
    }
    
    
    # On va tracer une légende propre pour expliquer la dispersion, plutôt que des symboles traiditionnels
    
    protoLeg = t(matrix(c(.1, .25, .5, .75, .9, .66), ncol = 1, nrow = 6)) %>%
        as.data.frame()
    colnames(protoLeg) = c("q10", "q25", "Med", "q75", "q90", "Moy")
    
    legGen = ggplot(protoLeg) + theme_nothing() +
        geom_linerange(aes(y=1,  xmin=q10, xmax=q90), col="orange") +
        geom_linerange(aes(y=1,  xmin=q25, xmax=q75), col="firebrick") +
        geom_point(aes(y=1, x=Med), col="firebrick") +
        geom_text(aes(y=1, x=Med), vjust=-1, hjust=1, label="Médiane", colour="firebrick", size=3) +
        geom_point(aes(y=1, x=Moy), col="slateblue") + 
        geom_text(aes(y=1, x=Moy), vjust=-1, hjust=0, label="Moyenne", colour="slateblue", size=3) +
        geom_text(aes(y=1, x=.1), vjust=1.7, hjust=0, label="1er décile", size=2) +
        geom_text(aes(y=1, x=.25), vjust=1.7, hjust=0, label="1er quartile", size=2) +
        geom_text(aes(y=1, x=.75), vjust=1.7, hjust=1, label="3e quartile", size=2) +
        geom_text(aes(y=1, x=.9), vjust=1.7, hjust=1, label="9e décile", size=2)
    
    
    if (tracerZeros==T)
    {
        if (zerosCommeBarres == T){
            matrice = as.data.frame(matrice)
            if(inverserZeros == T) { matrice$Part_0 = 100 - matrice$Part_0 }
            legAxeX2 = legZeros
            matrice$hjust = ifelse(matrice$Part_0 < 50, 0, 1)
            
            g2 = ggplot(matrice)
            
            g2 = g2 + geom_bar(aes(y=Mode, x=Part_0), stat="identity", fill="gray65") +
                theme_bw() + ggtitle(legAxeX2) +
                theme(legend.position="bottom") + 
                theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
                scale_x_continuous(name = "", limits = c(0,100), labels=scales::comma) +
                scale_y_discrete(limits = rev) +
                geom_text(aes(x= Part_0, y= Mode, label=paste0(" ", round(Part_0,1), "% "), hjust = hjust), size=3.5)
            
            if (!is.null(champCateg))
            {
                g2 = g2 + facet_grid(. ~ Categ)
            }
        }
        else {
            
            if(inverserZeros == T) { matrice$Part_0 = 100 - matrice$Part_0 }
            coulmax = ifelse(inverserZeros, "slateblue", "firebrick")
            
            if (is.null(champCateg)) { matrice$Categ = " "}
            
            g5 = ggplot(matrice) + theme_bw() +
                theme(legend.position = "bottom") +
                geom_count(aes(x = Categ, y = Mode, size = Pop, col = Part_0)) +
                scale_y_discrete(limits = rev) +
                scale_size(name = "Nombre\nde personnes", range = c(1,12)) +
                scale_colour_gradient(low = "grey", high = coulmax, name = legZeros) +
                ylab(legendeDiscret) + xlab("") +
                ggtitle("Population") +
                theme(legend.text = element_text(size = 6), legend.title = element_text(size = 8))
            
            if (is.null(champCateg))
            {
                g5 = g5 +                     
                    theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
            }
        }
    }
    
    
    if (!is.null(g2) & champs2 == F)
    {
        fig = plot_grid(g1, g2, ncol = 2, nrow = 1, rel_widths = c(1,.5), align = "h", axis="bt")
    }
    
    if (!is.null(g2) & champs2== T)
    {
        if(verbose) {cat("Graphique. Zéros comme barres, 2 champs présents.\n")}
        
        if (layoutVertical) {
            fig = plot_grid(g1, g2, g3 + ggtitle(""), legGen,
                            ncol = 2, nrow = 2, rel_widths = c(1,.5))
        } else {
            gAxeX = ggplot(matrice, aes(x = 0, y = Mode)) + geom_blank() +
                theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
            
            g1 = g1 +                     
                theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
            
            g3 = g3 +                     
                theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
            
            fig = plot_grid(gAxeX, g1, g3 + ggtitle(""), g2,
                            ncol = 4, nrow = 1, rel_widths = c(.2,1,1,1), align = "h", axis="bt")
        }
    }
    
    if (!is.null(g5))
    {
        if(verbose){cat("Zéros comme cercles. Suppression des étiquettes du ou des graphiques principaux.\n")}
        
        # Si on affiche les ronds rouges, plus besoin des étiquettes Y
        g1 = g1 + theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
        if (!is.null(g3)) {
            g3 = g3 + theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
        }
    }
    
    if (is.null(g2) & champs2== F)
    {
        if (!is.null(g5)) {
            
            fig = plot_grid(g5 + theme(legend.position = "none"), g1, ncol = 2, nrow = 1, rel_widths = c(.4, 1), align = "h",
                            axis="bt")}
        else {
            fig = g1
        }
    }
    
    if (is.null(g2) & champs2== T)
    {
        if (layoutVertical == T){
            
            if (!is.null(g5))
            {
                fig = plot_grid(g5 + theme(legend.position = "none"), g1, NULL, g3 + ggtitle(NULL),
                                ncol = 2, nrow = 2, align = "h", rel_widths = c(.4, 1), axis = "bt")
            } else {
                fig = plot_grid(g1, g3 + ggtitle(""),
                                ncol = 1, nrow = 2, align = "v")
            }
        }
        else {
            
            if(!is.null(g5))
            {
                fig = plot_grid(g5 + theme(legend.position = "none"), g1, g3 + ggtitle(NULL),
                                ncol = 3, nrow = 1, align = "h", rel_widths = c(.3, 1, 1), axis="bt")
            }
            else {
                fig = plot_grid(g1, g3 + ggtitle(""),
                                ncol = 2, nrow = 1, align = "h", axis="bt")
            }
        }
    }
    
    # On ajoute la légende en-dessous
    
    if(!is.null(g5)) {
        legG5 = get_legend(g5) #+ theme(legend.direction = "horizontal")
        legGen = plot_grid(legG5, legGen, nrow = 1, ncol = 2, rel_widths = c(.6,.4))
    }
    else {
        legGen = plot_grid(NULL, legGen, nrow = 1, ncol = 2, rel_widths = c(.6, .4))
    }
    
    if(champs2 & !is.null(g2) & layoutVertical) {
        # si 2 champs en pleine page, la légende est dessinée directement dans le layout : ne rien faire
    }
    else {
        fig = plot_grid(fig, legGen, ncol=1, nrow=2, rel_heights =  c(.9, .1))
    }
    
    if (nchar(titre) > 0) {
        titreFig = ggdraw() + draw_label(label = titre, fontface = "bold", x = 0.5, hjust = .5)
        
        fig = plot_grid(titreFig, fig, ncol = 1, nrow = 2, rel_heights = c(.1, .9))
    }
    
    if (nchar(capt) > 0) {
        
        if(verbose){cat("Valeur de Capt :", capt, ", longueur de Capt :", nchar(capt))}
        
        footer = ggdraw() + draw_label(label = capt, x = 1, fontface = "italic", hjust = 1, size = 9) +
            theme(plot.margin = margin(0, 8, 0, 0))
        
        fig = plot_grid(fig, footer, ncol = 1, nrow = 2, rel_heights = c(1, .05))
    }
    
    return(fig)
}

# Ajoute un titre sur une figure de type ggplot
viz_Titre = function(fig, titre, rel_heights = c(.1, .9), align = "c")
{
    if (align == "c")
    {
        titreFig = ggdraw() + draw_label(label = titre, fontface = "bold", x = 0.5, hjust = .5)
    }
    
    if (align == "g")
    {
        titreFig = ggdraw() + draw_label(label = titre, fontface = "bold", x = 0.02, hjust = 0)
    }
    
    fig = plot_grid(titreFig, fig, ncol = 1, nrow = 2, rel_heights = rel_heights)
    return(fig)
}

# Ajoute un footer sur une figure de type ggplot
viz_Pied = function(fig, pied, rel_heights = c(.9, .1))
{
    piedPage = ggdraw() + draw_label(label = pied, size = 8, x = .99, hjust = 1)
    fig = plot_grid(fig, piedPage, ncol = 1, nrow = 2, rel_heights = rel_heights)
    return(fig)
}

# Fonction permettant de tracer un tableau en R classique
# source https://elliotnoma.wordpress.com/2015/07/18/how-to-plot-a-table-of-values-in-r/
# Pour les couleurs conditionnelles colors <- matrix(sapply(d, function(x) ifelse(x < 0, "orange","green")),ncol=ncol(d))
viz_Table = function(d, colors = "white", marginColor = "grey", main="", text.cex=1.0, colAuto = T,
                     coeff.col=1, add=F, premLigne=T, premCol=T) 
{
    d = as.table(d)
    
    if (add==F) {plot.new()}
    
    if (!"matrix" %in% class(colors))
    {
        colors = matrix(sapply(d, function(x) ifelse(T, colors,NA)), ncol=ncol(d))
    }
    
    if (length(coeff.col) == 1) { coeff.col = rep(coeff.col, ncol(d)+1) }
    if (colAuto) { coeff.col = c(max(nchar(rownames(d))), apply(d, 2, function(x) {max(nchar(x))}))}
    
    plHauteur = ifelse(premLigne, 1, 0)
    pcLargeur = ifelse(premCol, coeff.col[1], 0)
    
    plot(c(0,sum(coeff.col[2:length(coeff.col)]+pcLargeur)),c(0,nrow(d)+0),
         type="n", xaxt = "n", yaxt = "n", xlab="",ylab="",main=main, bty="n")
    
    if (premLigne) {
        xDeb = 0
        for (c in 1:ncol(d)) {
            xFin = xDeb + coeff.col[c]
            rect(xDeb, nrow(d), xFin, nrow(d) + 1, col=marginColor)
            text(xDeb + (xFin-xDeb) * .5,nrow(d) +.5,colnames(d)[c], cex=text.cex)
            xDeb = xFin
        }
    }
    
    if (premCol){
        for (r in 1:nrow(d)) {
            rect(0, r-1, coeff.col[1], r, col=marginColor)
            text(coeff.col[1]/2, r-.5,rownames(d)[nrow(d) - r + 1], cex=text.cex)
        }
    }
    
    for (r in 1:nrow(d)) {
        xDeb = pcLargeur
        for (c in 2:(ncol(d)+1)) {
            xFin = xDeb + coeff.col[c]
            rect(xDeb, r-1, xFin, r, col=colors[nrow(d) - r + 1,c-1])
            text(xDeb + (xFin-xDeb) * .5,r-.5,d[nrow(d) - r + 1,c-1], cex=text.cex)
            xDeb = xFin
        }
    }
}

viz_enTete = function(titre = "", num = NA, soustitre = "M. Guinepain / EMD Cerema")
{
    .pardefaut = par ; par(mar= c(0,0,0,0), mfrow = c(1,1))
    plot(c(0,1), c(0,1), type="n", axes=F)
    text(.5,.5, titre, cex=2)
    if(!is.na(num)) {
        text(.5,.2, paste0(soustitre, "\nN°",num), cex=1)}
    else{ text(.5,.2, soustitre, cex=1.4)}
    par(.pardefaut)
}

tracerCarreEdt = function(df, ligne)
{
    hDeb = as.numeric(heureHHMMtoM(df["hDeb"])/60)
    hFin = as.numeric(heureHHMMtoM(df["hFin"])/60)
    Tache = df["Tache"]
    rect(hDeb, 0, hFin, 1, col = palMotifs(Tache), border=NA)
    if (hFin - hDeb > 1)
    {
        text(hFin - ((hFin-hDeb)/2), .5, etqMotifLieu(substr(df["Tache"],1,2)), cex=.6)
        if (hDeb > 4) {text(hDeb, 1.05, heureHHMM(df["hDeb"]), angle=90, cex=.5, adj=0, srt=45)}
        if (hFin <28) {text(hFin, 1.05, heureHHMM(df["hFin"]), angle=90, cex=.5, adj=0, srt=45)}
        
    }
}

viz_EDT = function(TSK_PER, main="")
{
    plot(c(4,28), c(0,1.5), type="n", xaxt="n", yaxt = "n", xlab="",ylab="",main=main, bty="n")
    
    apply(TSK_PER, 1, tracerCarreEdt)
    
}

# Échelle créée par Luc pour visualiser les sous- et sur-représentations (ou probabilités) en %
# de façon équilibrée à gauche et à droite
# D'après une idée de Luc Guibard !!
transf_echelle_sur100 = function(x)
{
  if (class(x) == "logical")
  {
    if (is.na(x[1])) {
      return(rep(NA, length(x)))
    }
  }
  
  if (!"numeric" %in% class(x) & !"integer" %in% class(x))
  { stop(paste("fonction sur100 appelée avec une valeur non numérique :",x,"\nClasse détectée : ", class(x), "\n")) }
  x = ifelse(x > 0, (x/100+1) - 1, ((1/(x/100+1)) * -1) + 1)
  return (x)
}

transf_echelle_sur100_lab = function(x, deuxLignes=F)
{
  etiqPlus = ifelse(deuxLignes, "×\nplus", "× plus")
  etiqMoins= ifelse(deuxLignes, "×\nmoins","× moins")
  
  if (!"numeric" %in% class(x) & !"integer" %in% class(x))
  { stop(paste("fonction sur100 label appelée avec une valeur non numérique :",x)) }
  y = ifelse(x>0, paste0(round(transf_echelle_sur100(x), 1)+1, etiqPlus),
             paste0(round(abs(transf_echelle_sur100(x)), 1)+1, etiqMoins))
  y = ifelse(x==0, "autant", y)
  y = gsub(pattern = ".", ",", y, fixed = T)
  return(y)
}

transf_echelle_sur100_labcourt = function(x)
{
  if (!"numeric" %in% class(x) & !"integer" %in% class(x))
  { stop(paste("fonction sur100 label appelée avec une valeur non numérique :",x)) }
  y = ifelse(x>0, paste0("× ", round(transf_echelle_sur100(x),1)+1),
             paste0("÷ ", round(abs(transf_echelle_sur100(x)),1)+1))
  y = ifelse(round(x)==0, "", y)
  y = gsub(pattern = ".", ",", y, fixed = T)
  return(y)
}

transf_echelle_sur100_inverse = function(x)
{
  if (!"numeric" %in% class(x) & !"integer" %in% class(x))
  { stop(paste("fonction sur100 inverse appelée avec une valeur non numérique :",x)) }
  x = ifelse(x > 0, (x * 100), ((1/((x - 1) * -1))-1) * 100) 
  return (x)
}

trans_sur100 = scales::trans_new(
  name = "sur100",
  transform = transf_echelle_sur100,
  inverse = transf_echelle_sur100_inverse,
  domain = c(-100, Inf)
)

# Programme =========================================================================================

version = function(incrémenter = F)
{
    load("Data/idVersion.rds")
    if (incrémenter) {
        idVersion = idVersion + 1 ; save(idVersion, file="Data/idVersion.rds")
    }
    cat("Version actuelle :", idVersion,"\n")
}

texteToBool = function(entrée)
{
    if (!is.character(entrée)) {stop("Erreur : la variable à traiter n'est pas une chaîne de car.")}
    
    sortie = NA
    if (entrée == "y" | entrée == "Y" | entrée == "o" | entrée == "O") { sortie = T }
    if (entrée == "n" | entrée == "N") { sortie = F }
    
    return(sortie)
}

# Barre de progression maison.
# - initialisée à l'aide d'ui_ProgInit qui démarre le chronomètre
# - passer l'objet "barre" retourné par la fonction à celle qui permet de la mettre à jour, ui_Prog
# - affiche une estimation de la durée restante, qui s'actualise à chaque pas, par règle de trois
ui_ProgInit = function(total)
{
  barre = c(total, as.character(Sys.time()))
  cat(sprintf('\r[%-50s] %d%%', paste(rep('/', 0 / 2), collapse = ''), floor(0)))
  return(barre)
}

ui_Prog = function(barre, i)
{
  total = as.numeric(barre[1])
  tps    = difftime(Sys.time(), strptime(barre[2], format="%Y-%m-%d %H:%M:%S"), units = "secs") %>% as.numeric()
  # estimation du temps restant par proportionnalité
  estimt = ((tps * total / i) - tps) / 60
  
  symbole = NULL
  if (i/4 - trunc(i/4) == 0    ) { symbole = "―" } 
  if (i/4 - trunc(i/4) == 0.25 ) { symbole = "\\" } 
  if (i/4 - trunc(i/4) == 0.5  ) { symbole = "|" }
  if (i/4 - trunc(i/4) == 0.75 ) { symbole = "/" }
  if (i                == total) { symbole = "√" }
  
  if(i > total) {stop(paste0("Erreur ! Valeur i de la barre de progression de ", i, ", ce qui excède le total"))}
  if(is.null(symbole)) {stop(paste0("Erreur indéterminée sur la barre de progression. Valeur i = ", i, "."))}
  
  if(i == 0)
  {
    cat(sprintf(paste0('\r[%-50s] %s%% ', symbole, ' ...'), paste(rep('/', (i/total*100) / 2), collapse = ''), "  0"))
  }
  
  if(i != 0 & i != total)
  {
    cat(sprintf(paste0('\r[%-50s] %s%% ', symbole, ' reste %.1f mn.   '), paste(rep('/', (i/total*100) / 2), collapse = ''),
                formatC(floor((i/total*100)), width = 3, flag = " "), estimt))
  }
  
  if(i == total)
  {
    cat(sprintf(paste0('\r[%-50s] %s%% ', symbole, ' temps écoulé : %.1f mn.\n'), paste(rep('/', (i/total*100) / 2), collapse = ''),
                "100", round(tps / 60, 1)))
  }
}

ml = function(...) # pour faire des étiquettes multilignes facilement dans le code
{
  return(paste(c(...), collapse="\n"))
}

nv = function(noms, valeurs) {
  
  vals = valeurs
  names(vals) = noms
  
  return(vals)
}

# Mise en page ============

imprPdf = function(chemin = "temp.pdf", titre = "", format = "a4", paysage=F, num=NA)
{
    if (!is.null(chemin)) {
        
        rapport(">>PDF>>", chemin, info=T)
        
        if (format == "a5" & paysage == F) {w= 5.875 ; h= 8.25 }
        if (format == "a5" & paysage == T) {w= 8.25  ; h= 5.875}
        if (format == "a4" & paysage == F) {w= 8.25  ; h= 11.75}
        if (format == "a4" & paysage == T) {w= 11.75 ; h= 8.25 }
        if (format == "a3" & paysage == F) {w= 11.75 ; h= 16.5 }
        if (format == "a3" & paysage == T) {w= 16    ; h= 11.75}
        
        pdf(file = chemin, height = h, width = w)
        
        if (titre != ""){
            viz_enTete(titre)
        }
    }
}

# Équivalent de dev.off(), sans sortie intempestive sur le terminal
off = function(optionDesactivationSiNull = "")
{
    if (!is.null(optionDesactivationSiNull)) {
        o = dev.off()
    }
}

viz_Pdf = imprPdf # rétrocompatibilité

pdf_Titre = function(texte = "Titre", niv = 1)
{
    def = par()
    
    par(mar = c(1,1,1,1))
    plot.new()
    
    if (niv == 1){taille = 2.5  ; stl = 2}
    if (niv == 2){taille = 2  ; stl = 2}
    if (niv == 3){taille = 1.5; stl = 1} 
    
    
    text(x=.5, y=.66, adj=.5, labels=texte, cex=taille, font=stl)
    
    par(def)
}

viz_Texte = function(texte = NULL, chemin = "print.txt",  lignesParPage = 60, largLigne = 78, police = "mono", wrap=T,
                     titre = NULL) {
    def = par()
    
    if (!is.null(texte))
    {
        if (class(texte) == "character"){
            texteImpr = strsplit(texte, split = "\n")
        }
        else{
            if (class(texte)[1] %in% c("lm", "multinom"))
            {
                sink(chemin)
                print(summary(texte))
                sink()
                texteImpr = readLines(chemin)
            }
            else
            {
                sink(chemin)
                print(texte)
                sink()
                texteImpr = readLines(chemin)
            }
        }
    }
    else {
        texteImpr = readLines(chemin)
    }
    
    if (wrap == T){
        texteImpr = strwrap(texteImpr, width = largLigne, simplify=F)}
    else {
        for(i in 1:length(texteImpr))
        {
            ajs = trunc(nchar(texteImpr[i]) / largLigne)
            if (ajs > 0) {
                for (j in 1:ajs) {
                    texteImpr[i] = paste0(substr(texteImpr[i], 1, j * largLigne), "\n",
                                          substr(texteImpr[i], j * largLigne + 1, nchar(texteImpr[i])))
                }
            }
        }
        texteImpr = strsplit(paste(texteImpr, collapse="\n"), split = "\n")      
    }
    
    
    if (!is.null(titre)) {texteImpr = c("", "", "", texteImpr)}
    
    nLignes = length(unlist(texteImpr))
    nPages = trunc(nLignes / lignesParPage) + 1
    
    if (wrap == F) {texteImpr = unlist(texteImpr)}
    
    ligne1 = 1
    for(page in 1:nPages)
    {
        par(mar = c(1,1,1,1))
        plot.new()
        if (page == 1) {text(x=.5, y=1, adj=.5, labels=titre, cex=1.5)}
        max = ifelse(ligne1+(lignesParPage-1) <= nLignes, ligne1+(lignesParPage-1), nLignes)
        textePage = paste(texteImpr[ligne1:max], collapse="\n")
        textePage = as.expression(paste0(textePage, paste(rep("\n", times=(ligne1+(lignesParPage-1) - max)), collapse="")))
        text(x=0, y=0, adj=0, labels=textePage, family=police, cex=.9)
        ligne1 = ligne1 + lignesParPage
    }
    
    par(def)
}

# Fonction de Romain :
#' @name pageZ
#' @description format size of plots in a A4 page.
#' @param name name for saving plot.
#' @param output format of the output file. Can be "pdf","svg" or "png".
#' @param format size of the plot in the page. Can be "portrait", "landscape", "portrait_half" or "portrait_third".
#' @param mfrow splitting plot region for several plots. c(number of rows, number of columns). If mfrow is not by default, adapt mg.
#' @param mg size of the margins to draw axis and axis labels. c(0,0,0,0) in case of none.
#' @examples
#'
#' pageZ()
#'
#' plot(rnorm(100), rnorm(100))
#'
#' dev.off()
#'

sortie = function(nom = "sortie", format = "png", taille = "def", portrait = F,
                  h = 11, l = 17, chemin = NULL, pointsize=8, rotation=F)
{
    if (is.null(chemin)) { chemin = paste0("Sorties/", nom, ".", format) }
    
    if (taille == "page")  { h = 17 ; l = 24 }
    if (taille == "def")   { h = 12 ; l = 17 }
    if (taille == "carré") { h = 17 ; l = 17 }
    if (taille == "a4")    { h = 21 ; l = 29.7 }
    if (taille == "mini")  { h = 8  ; l = 11 }
    if (taille == "man")   {}
    
    if (portrait == T) { t = l ; l = h ; h = t }
    
    if (!is.null(taille) & !taille %in% c("page", "def", "a4", "carré", "mini", "man"))
    { stop("Format de sortie non reconnu :", taille) }
    
    if (format == "pdf")
    {
        h = 0.3937008 * h ; l = 0.3937008 * l
        pdf(file = chemin, height = h, width = l, pointsize=pointsize)
        
        rapport(">PDF> Figure :", nom, info = T)
    }
    
    if (format == "svg")
    {
        h = 0.3937008 * h ; l = 0.3937008 * l
        svg(file = chemin, height = h, width = l, pointsize=pointsize)
        
        rapport(">SVG> Figure :", nom, info = T)
    }
    
    if (format == "png")
    {
        png(file = chemin, width=l, height=h, units="cm", res=600, pointsize=pointsize)
        
        rapport(">PNG> Figure :", nom, info = T)
    }
}

# Stats simples ====

# Centre-réduit avec des pondérations
# (la 1e partie de la fonction opère sur une simple liste de type tab$colonne,
#  la 2e partie opère sur un tableau doté de plusieurs colonnes, par colonne)
# !!! Faute de fonction "weightedSd", ce n'est pas pondéré ici, et ça craint
scale_w = function(x, w)
{
    if(! "data.frame" %in% class(x))
    {
        x = (x - weighted.mean(x, w= w, na.rm=T)) / sd(x, na.rm=T)
    }
    else
    {
        for (c in 1:ncol(x))
        {
            x[,c] = (x[[c]] - weighted.mean(x[[c]], w= w, na.rm=T)) / sd(x[[c]], na.rm=T)
        }
    }
    
    return(x)
}

mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
    # https://stackoverflow.com/questions/64678599/most-frequent-value-in-a-given-column
    # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441
}

weighted.sd = function(x, w, na.rm=F) {
  if (na.rm)
  {
    w = w[!is.na(x)]
    x = x[!is.na(x)]
  }
  
  moyenne = weighted.mean(x, w)
  wsd = weighted.mean((x - moyenne)^2, w)
  
  # d'après Michael Lachmann
  # https://stackoverflow.com/questions/10049402/calculating-weighted-mean-and-standard-deviation
  
  wsd = sqrt(wsd)
  
  return(wsd)
}

retirerDoubles = function(chaine)
{
    chaine = chaine[!is.na(chaine)]
    res = chaine[chaine[-1] != chaine[-length(chaine)]]
    
    if (length(res) == 0 & length(chaine) == 1){res = chaine}
    if (length(res) == 0 & length(unique(chaine)) == 1){res = chaine[1]}
    if (length(res) == 0 & length(chaine) == 0){res = NA}
    
    if (!anyNA(res) & last(res) != last(chaine)) { res = c(res, last(chaine))}
    
    return(res)
}

testBrks = function(distrib, n, style = "quantile")
{
  # 5 classes par quantiles
  brks = classInt::classIntervals(distrib, n, style=style)$brks
  
  # vizu
  g= ggplot(as_tibble(distrib), aes(x = value)) +
    geom_vline(xintercept = c(brks), col = "lightslateblue") +
    geom_density() + theme_bw() +
    labs(title = "Distribution de l'indice de moyenne des écarts à la moyenne (en points)") +
    xlab("moyenne absolue des écarts à la moyenne (en points)") + ylab("part des secteurs")
  
  return(list(brks,g))
}



paliers = function(x, palier, floor = F)
{
  if (!floor)
  {
    x = round(x / palier) * palier
  }
  if (floor)
  {
    x = floor(x / palier) * palier
  }
  
  return(x)
}

discretisation = function(x, methode = "net", verb = F, couper1pc = F, nbClassesCible = 8, passerLog = F)
{
  if (couper1pc)
  {
    x = ifelse(x > quantile(x, probs=c(.01), na.rm=T) & x < quantile(x, probs = c(.99), na.rm=T), x, NA)
  }
  
  if (methode == "quartiles")
  {
    intervalles = round(quantile(x, c(0, .25, .5, .75, 1), na.rm=T))
  }
  
  if (methode == "déciles")
  {
    intervalles = quantile(x, c(0:10)/10, na.rm=T) |> round()
    intervalles = intervalles[!is.na(intervalles)]
  }
  
  if (methode == "net")
  {
    if (verb) {cat("\ndiscrétisation, méthode des ruptures nettes")}
    seuilsPaliers = c(.01, .05, .1, .25, .5, 1, 2, 5, 10, 20, 25, 50,
                      100, 500, 1000, 5000, 10000, 50000, 100000)
    
    nbClasses = length(unique(x))
    i = 0
    
    while(nbClasses > nbClassesCible)
    {
      i = i + 1
      nbClasses = length(unique(paliers(na.omit(x), seuilsPaliers[i], floor = T)))
      if (verb) {cat("\navec un seuil de", seuilsPaliers[i], "nb de classe de", nbClasses)}
      if (i + 1 > length(seuilsPaliers)) { break }
    }
    
    intervalles = unique(paliers(na.omit(x), seuilsPaliers[i], floor = T))
    intervalles = c(intervalles, intervalles[length(intervalles)] + seuilsPaliers[i])
  }
  
  y = x
  
  if (verb) {cat("\nintervalles choisis :", paste(sort(intervalles), collapse = " → ")) }
  
  intervalles = sort(intervalles)
  
  if(passerLog) { intervalles = round(exp(intervalles)) }
  
  if (verb) {cat("\nau logarithme :", paste(sort(intervalles), collapse = " → ")) }
  
  for (i in c(2:length(intervalles)))
  {
    if (i < length(intervalles))
    {
      y = ifelse(x >= intervalles[i - 1] & x < intervalles[i],
                 paste0("[", intervalles[i - 1], ":", intervalles[i], "["), y)
    } else {
      y = ifelse(x >= intervalles[i - 1] & x <= intervalles[i],
                 paste0("[", intervalles[i - 1], ":", intervalles[i], "]"), y)
    }
  }
  
  if (verb) {cat("\n", length(unique(y)), "étiquettes créées")}
  
  niveaux = c(paste0("[",
                     intervalles[1:(length(intervalles) - 1) - 1], ":",
                     intervalles[2:(length(intervalles) - 1)], "["),
              paste0("[", intervalles[length(intervalles) - 1], ":",
                     intervalles[length(intervalles)], "]"))
  
  y = factor(y, levels = niveaux)
  
  if (verb) { print(table(y)) }
  
  return(y)
}



# Conversions ====

heureHHMMtoM = function(val)
{
    v = as.numeric(substr(val,1,2))*60 + as.numeric(substr(val,3,4))
    return(v)
}

heureMtoHHMM = function(val)
{
    heures = trunc(val / 60)     
    minutes = val - (heures*60)
    
    heures = as.character(heures)
    minutes= as.character(minutes)
    
    heures = ifelse(nchar(heures) == 1, paste0("0", heures), heures)
    minutes= ifelse(nchar(minutes)== 1, paste0("0", minutes),minutes)
    
    return(paste0(heures, minutes))
}

diffHeures_interne = function(heures)
{
    heure1 = heures[1]
    heure2 = heures[2]
  
    h1_h = as.numeric(substr(heure1, 1,2)) ; h1_m = as.numeric(substr(heure1, 3,4))
    h2_h = as.numeric(substr(heure2, 1,2)) ; h2_m = as.numeric(substr(heure2, 3,4))
    
    if (is.na(h1_m) | is.na(h2_m)) {return(NA)}
    if (h1_m>59){return(NA)} ; if(h2_m>59){return(NA)}
    
    diffmin = h2_m - h1_m ; retenue = 0
    if (diffmin < 0) {retenue = -1 ; diffmin = diffmin + 60}
    
    diffheu = h2_h - h1_h + retenue
    
    diff = diffheu * 60 + diffmin
    return(diff)
}

diffHeures = function(heures1, heures2)
{
  vals = apply(FUN = diffHeures_interne, X = cbind(heures1, heures2), MARGIN = 1)
  return(unlist(vals))
}

km = function(km)
{
    km = formatC(round(km/1000), width=2, flag="0")
    return(km)
}

hr = function(hr)
{
    hr = formatC(round(hr/60, 1), width=4, digits=1, flag="0", format="f")
    return(hr)
}

heureHHMM = function(h)
{
    sortie = ""
    for (i in 1:length(h))
    {
        hNum = as.numeric(substr(h[i],1,2))
        if (hNum > 23) {hNum = hNum - 24}
        sortie[i] = paste0(hNum, "h", substr(h[i],3,4))
    }
    return(sortie)
}

heureMinToHr = function(m, secondes = F)
{
    heures = trunc(m / 60)

    if (secondes)
    {
        minutes = trunc(m - heures * 60)
        secondes = round((m - heures*60 - minutes) * 60)
        
        if (secondes == 60) { minutes = minutes + 1 ; secondes = 0 }
        if (minutes >= 60)  { heures = heures + 1 ; minutes = 0}
        
        sortie = paste0(heures, "h",
                        ifelse(nchar(as.character(minutes)) == 1, paste0("0", minutes), minutes),
                        ifelse(secondes>0,
                               paste0("m ",
                                      ifelse(nchar(as.character(secondes)) == 1,
                                             paste0("0", secondes), secondes),
                                      "s"),""))
    } else {
        minutes = round(m - heures * 60)
        
        heures  = ifelse(minutes >= 60, heures + 1, heures)
        minutes = ifelse(minutes >= 60, 0, minutes) 
        
        sortie = paste0(heures, "h",
                        ifelse(nchar(as.character(minutes))==1, paste0("0", minutes), minutes))
    }

    return(sortie)
}

comEnDep = function(base)
{
  base = base |>
    mutate(dep = case_when(substr(Com, 1, 2) == "97" ~ substr(Com, 1, 3),
                          substr(Com, 1, 2) != "97" ~ substr(Com, 1, 2)))
  return(base)
}