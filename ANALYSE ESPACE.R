# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                   E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                     #
#                                                                                                 #
#                               SCRIPTS DE TRAVAIL M. GUINEPAIN                                   #
#                                           SEPT 2022                                             #
#                                                                                                 #  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# Fonctions de cartographie ggplot ====

cartoSchematiser = function(shp)
{
    shp = st_simplify(shp, preserveTopology = T, dTolerance = 300) %>% st_buffer(dist = 100)
    return(shp)
}

cartoEtendue = function(shp, enq, proj = 2154, df = T, unExemplairePar = NULL, filtreZT = NULL) {
    
    if (!"uid_ENQ" %in% colnames(shp)) { stop("La fonction \"étendue\" nécessite un champ uid_ENQ") }
    
    if (!is.null(unExemplairePar))
    { 
        shp = group_by(shp, uid_ENQ, across(matches(unExemplairePar)), .drop=T) %>%
              slice(1) }
    
    
    etendue = shp %>%
        filter(uid_ENQ %in% enq)
    
    if (!is.null(filtreZT))
    { etendue = etendue %>% filter(ZT %in% filtreZT) }
    
    etendue = etendue %>%
        st_buffer(dist = 100) %>%
        group_by(uid_ENQ) %>% summarise() %>%
        st_transform(crs = proj)
    
    if (df) {return(etendue)}
    if (!df) {return(etendue$geometry)}
}

cartoHydro = function(g, etendue, carte = fdCarte, proj = 2154)
{
    boite = st_bbox(etendue)
    diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
        list() %>% st_multilinestring() %>% st_length()
    
    eau = carte$shpEau %>%
        st_transform(crs = proj) %>%
        st_simplify(dTolerance = 20) %>%
        st_crop(y = st_bbox(st_buffer(etendue, dist = .5*diagonale)))
    g = g + geom_sf(data = eau, fill = "#e6f0fa", color = NA)
    return(g)
} #avant c'était lightblue

cartoAxes = function(g, etendue, carte = fdCarte, proj = 2154, det = T)
{
    ra = carte$shpRail %>%
        st_transform(crs = proj) %>%
        st_intersection(etendue) %>%
        st_simplify(dTolerance = 10000) %>%
        mutate(type = "voies ferrées") %>%
        select(type, geometry)
    if (det)
    {
      r2 = carte$shpRoutes2 %>%
        st_transform(crs = proj) %>%
        st_intersection(etendue) %>%
        st_simplify(dTolerance = 10000) %>%
        mutate(type = "routes secondaires") %>%
        select(type, geometry)
    }
    r1 = carte$shpRoutes1 %>%
        st_transform(crs = proj) %>%
        st_intersection(etendue) %>%
        st_simplify(dTolerance = 10000) %>%
        mutate(type = "routes majeures") %>%
        select(type, geometry)
    
    if (det)
    {
      axes = rbind(ra, r2, r1)
      couleurs = c("maroon", "gray40", "rosybrown4")
    } else {
      axes = rbind(ra, r1)
      couleurs = c("maroon", "rosybrown4")
    }
    
    g = g +
        geom_sf(data = axes, aes(color = type), alpha=1, linewidth = .2, key_glyph = "path") +
        scale_color_manual(values = couleurs,
                           name = "Axes de transport")
    
    return(g)
}

cartoLib = function(g, etendue, detail = 5, carte = fdCarte, proj = 2154, overrideEtendue = F, tailleTexte = 8) {
    
    if (overrideEtendue)
    {
        # override manuel de etendue à cause de cette andouille de R SANS TYPES FIXES
        etendue = st_sfc(etendue, crs = proj)
    }
  
  etendue = st_transform(etendue, crs = proj)
    
    # Etiquetage avec un carroyage : but = afficher le lieu de peuplement le plus big de
    # chaque carreau
    grilleNoms = st_make_grid(x = etendue, n = c(detail,detail), square=F)
    grilleNoms = tibble(id = c(1:length(grilleNoms)), geometry = grilleNoms) %>% st_as_sf()
    noms = filter(carte$geonames, substr(feature_code,1,2) == "PP" & population > 0 | name == "Paris") %>%
        st_transform(crs = proj) %>%
        st_intersection(y = grilleNoms) %>%
        group_by(id) %>%
        mutate(jeSuisMax = ifelse(max(population) == population, T, F)) %>%
        filter(jeSuisMax == T) %>%
        st_intersection(y = etendue)
    
    g = g + geom_sf_label(data = noms, aes(label = name),
                          alpha = .4, color = "gray30", label.size=0,
                          size=tailleTexte/4, fontface="bold",
                          check_overlap = T)
    
    return(g)
}

cartoFinish = function(g, etendue)
{
    g = g + 
        # theme_bw(base_size=7) +
        coord_sf(xlim = c(as.double(st_bbox(etendue)$xmin), as.double(st_bbox(etendue)$xmax)),
                 ylim = c(as.double(st_bbox(etendue)$ymin), as.double(st_bbox(etendue)$ymax))) +
        ggspatial::annotation_scale(location="bl", bar_cols = c("gray60", "gray95"),
                                    line_col = "gray70", height = unit(.1, "cm")) +
        ggRetirerAxeX + ggRetirerAxeY
    
    return(g)
}

ggCarteZT = function(uid, shp, var, descr_leg, titre = NULL, sousTitre = NULL,
                     degrMin = "blue", degrMax = "red", degrMid = "grey90", lims = NULL)
{
    colnames(shp)[colnames(shp) == var] = "var"
    
    if (!"uid_ENQ" %in% colnames(shp))
    {
      shp$uid_ENQ = substr(shp$ZT, 1, 7)
    }

    if (is.null(lims))
    {
        lims = c(min(shp$var, na.rm=T), max(shp$var, na.rm=T))
    }
    
    # Pour les outremers, on va prendre du Mercator plutôt que du Lambert
    if (uid %in% c("MTQ2014", "REU2016", "CAY2011"))
    {
        shp = st_transform(shp, crs = 3857)
    }
    
    proj = ifelse(uid %in% c("MTQ2014", "REU2016", "CAY2011"), 3857, 2154)
    etendue = cartoEtendue(shp, enq=uid, proj = proj, df = F, unExemplairePar = "ZT")
    
    b = filter(shp, uid_ENQ == uid)
    g = ggplot(data = b) +
        geom_sf(aes(fill = var), color = "white", size=.1) +
        labs(title = titre,
             subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
             caption = src_fig(base = filter(shp, uid_ENQ == uid)))
    
    if (lims[1] >= 0){
        g = g + scale_fill_gradient (low = degrMid, high = degrMax, name = descr_leg, limits = lims)
    } else {
        g = g + scale_fill_gradient2(low = degrMin, mid = degrMid, high = degrMax, name = descr_leg,
                                     limits = lims)
    }
    
    g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
    g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T, proj = proj)
    g = cartoFinish(g, etendue = etendue)
    return(g)
}

nomVilleDuCoin = function(etendue, carte = fdCarte)
{
    # Cherche dans la base des libelles la plus grosse ville correspondant à l'étendue
    gn = fdCarte$geonames %>% filter(feature_class == "P") %>% st_transform(crs = st_crs(etendue))
    villesDuCoin = st_intersection(gn, etendue)
    villesDuCoin = tab_Tri(villesDuCoin, rev = T, parCol = "population")
    rapport(paste0("Détermination de la ville la plus importante de l'emprise : ",
            villesDuCoin[1,]$name, ", population de ", villesDuCoin[1,]$population, " hab."))
    return(villesDuCoin[1,]$name)
}

ggCarteCat = function(uid, shp, var, descr_leg, titre = NULL, sousTitre = NULL,
                     couleurs = NULL, detailLabs = 5, filtreZT=NULL,
                     champPop = NULL, champPart = NULL, legPop = NULL, legPart = NULL,
                     retirerLegVar = F)
{
    colnames(shp)[colnames(shp) == var] = "var"
    
    # Pour les outremers, on va prendre du Mercator plutôt que du Lambert
    if (uid %in% c("MTQ2014", "REU2016", "CAY2011"))
    {
        shp = st_transform(shp, crs = 3857)
    }
    
    proj = ifelse(uid %in% c("MTQ2014", "REU2016", "CAY2011"), 3857, 2154)
    etendue = cartoEtendue(shp, enq=uid, proj = proj, df = F, unExemplairePar = "ZT", filtreZT = filtreZT)
    
    b = filter(shp, uid_ENQ == uid)
    
    if (!is.null(filtreZT))
    {
        b = filter(b, ZT %in% filtreZT)
    }
    
    g = ggplot(data = b) +
        geom_sf(aes(fill = var), color = "white", size=.1)
    
    if (is.null(filtreZT)) {
        g = g + labs(title = titre,
                     subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
                     caption = src_fig(base = filter(shp, uid_ENQ == uid), auteurs = NULL, date = NULL))
    } else {
        g = g + labs(title = titre,
                     subtitle = ifelse(is.null(sousTitre), paste("Zoom sur", nomVilleDuCoin(etendue)), sousTitre),
                     caption = src_fig(base = filter(shp, uid_ENQ == uid), auteurs = NULL, date = NULL))
    }

    if (!is.null(couleurs))
    {
        couleurs = couleurs[levels(b$var) %in% b$var]
        g = g + scale_fill_manual(values = couleurs, name = descr_leg)
    } else {
        g = g + scale_fill_hue(name = descr_leg)
    }
    
    g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
    
    if (!is.null(champPop) & !is.null(champPart))
    {
        colnames(b)[colnames(b) == champPop] = "pop" #nb : ça va buguer si déjà col nommée ainsi
        colnames(b)[colnames(b) == champPart] = "part"
        
        b = filter(b, !is.na(part))
        
        g = g + geom_sf(data = st_point_on_surface(b),
                           aes(size = pop, colour = part), shape = 21) +
            scale_size(name = legPop, range = c(.5,3)) + scale_colour_brewer(palette = "PuRd",
                                                                             name = legPart, na.translate = F)
    } else {
        # Si échelle rapprochée, tracer les axes routiers
        boite = st_bbox(etendue)
        diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
            list() %>% st_multilinestring() %>% st_length()
        if (diagonale < 100000) { g = cartoAxes(g, etendue = etendue, proj = proj) }
    }
    
    g = cartoLib(g, etendue = etendue, detail = detailLabs, overrideEtendue = T, proj = proj)
    g = cartoFinish(g, etendue = etendue)
    
    if(retirerLegVar) { g = g + guides(fill = "none") }
    
    return(g)
}

# Fonctions d'analyse spatiale =====================================================================

reduireMaillage = function(init, uid1 = "uid", dest, valGarder = NULL)
{
  colnames(init)[colnames(init) == uid1] = "uid"
  
  # But : répartir le contenu du maillage 1 dans le maillage 2.
  # Difficulté supplémentaire : prendre en compte les cas où le maillage 2 ne couvre pas tout le
  # maillage 1 (exemple : pour transformer maillage en maillage des zones habitées).
  
  if (is.null(valGarder))
  {
    intersection1 = st_intersection(init, dest) %>%
      group_by(uid) %>% summarise(across(where(~is.numeric(.x) | is.factor(.x) | is.character(.x)), first))
  } else {
    intersection1 = st_intersection(init, dest) %>%
      group_by(uid) %>% summarise(across(valGarder, first))
  }
  
  colnames(intersection1)[colnames(intersection1) == "uid"] = uid1
  
  return(intersection1)
}

changerMaillage = function(init, dest, uid1 = NULL, uid2 = "uid")
{
  
  if (!is.null(uid1)) {colnames(init)[colnames(init) == uid1] = "uid1"}
  
  colnames(dest)[colnames(dest) == uid2] = "uid"
  
  if (st_crs(init) != st_crs(dest))
  { dest = st_transform(crs = as.numeric(substr(st_crs(init)$input, 6, 9))) }
  
  init$surfaceA = st_area(init)
  
  intersec = st_intersection(init, dest)
  
  intersec$surfaceB = st_area(intersec)
  
  listeChamps = names(intersec)[sapply(intersec, is.numeric)]
  listeChamps = listeChamps[!listeChamps %in% c("uid1", "uid", "surfaceA", "surfaceB")]
  intersec = mutate(intersec, across(listeChamps, ~.* (surfaceB / surfaceA)))
  intersec = select(intersec, -surfaceA, -surfaceB)
  
  if (is.null(uid2)) {
    intersec = intersec %>% group_by(uid) %>% summarise(across(where(is.numeric), sum, na.rm=T))}
  if (!is.null(uid1)) {
    intersec = intersec %>% group_by(uid1, uid) %>% summarise(across(where(is.numeric), sum, na.rm=T))}
  
  colnames(intersec)[colnames(intersec) == "uid"] = uid2
  colnames(intersec)[colnames(intersec) == "uid1"] = uid1
  
  intersec = st_as_sf(intersec)
  
  return(intersec)
}

indexDuncan = function(populationA, populationTotale)
{
  if (!is.numeric(populationA)) {stop("L'indice de Duncan nécessite une variable numérique pour la population A")}
  if (!is.numeric(populationTotale)) {stop("L'indice de Duncan nécessite une variable numérique pour la population B")}
  if (length(populationA) != length(populationTotale)) {stop("Les listes population A et totale ne font pas la même longueur (indice Duncan")}
  if (is.null(populationTotale)) { stop("L'argument Population totale a été omis")}
  
  populationB = populationTotale - populationA
  if(any(populationB < 0)) {stop("La population A excède la population totale, ce qui empêche le calcul de l'indice de Duncan.")}
  
  # Formule copiée depuis Wikipedia
  total = sum(populationTotale)
  totalA = sum(populationA)
  totalB = sum(populationB)
  
  i = .5 * sum(abs((populationA / totalA) - (populationB / totalB)))
  
  return(i)
}

etendreBbox = function(bbox, facteur = .5)
{
  xmin = bbox$xmin - facteur*bbox$xmin
  xmax = bbox$xmax + facteur*bbox$xmax
  ymin = bbox$ymin - facteur*bbox$ymin
  ymax = bbox$ymax + facteur*bbox$ymax
  
  bbox = c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax)
  return(bbox)
}

tableDepVersReg = function()
{
  table = read_delim("Sources/IRIS population.csv", delim = ";", show_col_types = F) |>
    select(IRIS, REG, DEP) |>
    group_by(DEP) |> summarise(REG = first(REG)) |>
    rename(Dep = DEP, Reg = REG)
  table = rbind(table, data.frame(Dep = "20", Reg = "94"))
  return(table)
}

comEnDep = function(com) # Le substr 1,2 ne suffit pas car il y a les outremers
{
  dep = ifelse(substr(com, 1, 2) == "97", substr(com, 1, 3), substr(com, 1, 2))
  return(dep)
}

maillageDepVersReg = function(table, champDep = "Dep")
{
  # On devrait normalement pouvoir injecter un nom de variable en dplyr avec !!
  # Mais bien sûr, ça ne fonctionne pas. Plus précisément, ça ne fonctionne que
  # si la variable est en 2e position, donc on ne peut pas renommmer dans
  # l'autre sens. Fuck
  # (J'ai tout essayé : !!, {{}}, inject()...) # Juillet 2024
  
  colnames(table)[colnames(table) == champDep] = "Dep"
  
  ref = tableDepVersReg()
  
  if ("Reg" %in% colnames(table)) { table$Reg = NULL }
  
  table = left_join(table, ref, by="Dep")
  table = table |> group_by(Reg) |> summarise(across(where(is.numeric), ~sum(., na.rm=T)),
                                              across(where(is.factor ), ~mode(.)))
  
  colnames(table)[colnames(table) == "Dep"] = champDep
  return(table)
}

corse2A2Bvers20 = function(table, champCom = "Com")
{
  if ("idCom2A2B" %in% colnames(table))
  {
    table$idCom2A2B = NULL
    warning("Le champ idCom2A2B présent dans la table a été écrasé.")
  }
  colnames(table)[colnames(table) == champCom] = "idCom2A2B"
  
  table = table |>
    mutate(idCom2A2B = ifelse(substr(idCom2A2B,1,2) == "2A",
                              paste0("20", substr(idCom2A2B,3,5)), idCom2A2B)) %>%
    mutate(idCom2A2B = ifelse(substr(idCom2A2B,1,2) == "2B",
                              paste0("20", as.character(as.integer(substr(idCom2A2B,3,5))+500)), idCom2A2B))
  
  colnames(table)[colnames(table) == "idCom2A2B"] = champCom
  
  return(table)
}



# Fonctions cartographiques ========================================================================

load_geonames = function()
{
  colstand = c("geonameid", "name", "asciiname", "alternatenames",
               "latitude", "longitude", "feature_class", "feature_code",
               "country_code", "cc2", "admin1_code", "admin2_code", "admin3_code",
               "admin4_code", "population", "elevation", "dem", "timezone", "mod_date")
  
  # EDIT AVRIL 2022 : pour une raison inconnue, le chargement des Geonames tend à échouer chroniquement avec
  # l'erreur "Ressource temporairement indisponible". Cela empêche la cartographie.
  # Pour limiter l'impact de cette erreur, les appels sont encapsulés dans des TryCatch qui renvoient un tableau
  # vide, mais bien formaté, en cas d'erreur. Dans le pire des cas, LoadGeonames renverra donc un tableau vide à la
  # fonction de cartographie qui pourra quand même s'exécuter correctement.
  
  geonames_empty = function()
  {
    setNames(rep("", length(colstand)), colstand) %>%
      bind_rows() %>%
      .[0,] %>%
      st_as_sf(coords=c("longitude", "latitude"), crs="wgs84") %>%
      return()
  }
  
  geonames = tryCatch(
    {
      read_tsv("Sources/Fond Carte/geonames_fr.txt", col_types = cols(), # fait taire la fonction
               col_names = colstand) %>%
        st_as_sf(coords=c("longitude", "latitude"), crs="wgs84") %>%
        return()
    },
    error=function(cond){
      rapport("Impossible de charger les libellés (France métropolitaine).")
      return(geonames_empty())
    }
  )
  
  
  geonames_re = tryCatch(
    {
      read_tsv("Sources/Fond Carte/geonames_re.txt", col_types = cols(), 
               col_names = colstand) %>%
        st_as_sf(coords=c("longitude", "latitude"), crs="wgs84") %>%
        return()
    },
    error=function(cond){
      rapport("Impossible de charger les libellés (Réunion).")
      return(geonames_empty())
    }
  )
  
  geonames_gf = tryCatch(
    {
      read_tsv("Sources/Fond Carte/geonames_gf.txt", col_types = cols(), 
               col_names = colstand) %>%
        st_as_sf(coords=c("longitude", "latitude"), crs="wgs84") %>%
        return()
    },
    error=function(cond){
      rapport("Impossible de charger les libellés (Guyane française).")
      return(geonames_empty())
    }
  )
  
  geonames_mq = tryCatch(
    {
      read_tsv("Sources/Fond Carte/geonames_mq.txt", col_types = cols(), 
               col_names = colstand) %>%
        st_as_sf(coords=c("longitude", "latitude"), crs="wgs84") %>%
        return()
    },
    error = function(cond){
      rapport("Impossible de charger les libellés (Martinique).")
      return(geonames_empty())
    }
  )
  
  geonames = rbind(geonames, geonames_re, geonames_gf, geonames_mq) %>%
    st_transform(crs = 3857)
  
  return(geonames)
}


# CarteMonde sort une planche unique qui représente toutes les enquêtes (en France) et dans des encarts, les outremers,
# avec leur propre échelle. Ce rendu n'est pas très pratique pour le format EMD.
# Il faut fournir une base sf Carte, avec en colonne 1 l'identifiant des enquêtes, en 2 la variable à cartographier.
# Brks permet d'imposer une discrétisation. Sinon, utiliser nClasses (en jenks).
# Mettre Typo = F pour signaler qu'on cartographie un facteur. Accepte Stocks et Prop pour cartographier un stock + un écart.
# (issu du travail sur l'EMD Nantes, printemps 2020).

map_CarteMonde = function(Carte, Titre, TxtLegende, Unit = "", nClasses, Palette,
                          Brks = NA, Stocks = F, Prop = F, Typo = F, TxtLegende2 = "", Unit2 = "")
{
  
  # Carte doit être un sf
  if(!"sf" %in% class(Carte)) {Stop("Il faut des données cartographiques à cartographier")}
  
  # load("Data/shp_enq.rds")
  # load("Data/shp_monde.rds") ; load("Data/shp_monde84.rds")
  
  # Renommage de la base en entrée
  if (ncol(Carte) == 2){ colnames(Carte) = c("enq","val") }
  if (ncol(Carte) == 3){ colnames(Carte) = c("enq","val","val2") }
  
  # Repérage des enquêtes d'outremer, pour adapter la carte et prévoir des encarts spécifiques
  outremer = c("MTQ2014", "CAY2010", "REU2016")
  Carte$outremer = Carte$enq %in% outremer
  
  # Est-ce qu'on cartographie une évolution des stocks ?
  if (Stocks == T & Prop == T) {
    Carte = Carte %>% mutate(val2_sens = ifelse(val2>0, 1, ifelse(val2 == 0, 0, -1)),
                             val2_abso = abs(val2))
    colSens = c("#A1D5FA","#E1E1E2","#FE8A5D")
  }
  
  # Paramétrage des marges, si l'on détecte les enquêtes de l'outremer
  nOutremer = nrow(filter(Carte, outremer==T))
  if (nOutremer == 0) { layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE), widths=c(2.5,1), heights=c(0.2,1))}
  if (nOutremer == 1) { layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(2.5,1), heights=c(0.2,1))}
  if (nOutremer == 2) { layout(matrix(c(1,2,3,4,3,5), 3, 2, byrow = TRUE), widths=c(2.5,1), heights=c(0.4,1,1))}
  if (nOutremer == 3) { layout(matrix(c(1,2,3,4,3,5,3,6), 4, 2, byrow = TRUE), widths=c(2.5,1), heights=c(0.6,1,1,1))}
  
  # Préparation du sous-titre qui donne la médiane (sauf dans le cas des typologies)
  if (Typo==F){
    var_med = paste0("Valeur médiane : ",round(median(Carte$val),1)," ",Unit)
    if (is.na(median(Carte$val))) { var_med = paste0("Valeur médiane (sans les valeurs manquantes) : ",round(median(Carte$val,na.rm=T),1)," ",Unit) }
  } else {
    var_med = paste0("Groupe majoritaire : ", Carte$val[as.integer(which.max(table(Carte$val)))])
  }
  
  # Écriture du titre
  par(mar= c(0,0,0,0))
  plot(c(1,10), c(1,2), type="n", axes=F)
  text(5.5,1.7, Titre, cex=2)
  text(5.5,1.2, paste0("Exploitation de la Base Unique des EMDs du Cerema (2009-2019)\n",var_med), cex=1)
  
  # Si les classes ne sont pas spécifiées, on utilise Jenks
  if(Typo==F){
    if(length(Brks)==1)
    {
      distrib = classInt::classIntervals(c(Carte$val), nClasses, style="jenks")$brks
      
      # Parfois Jenks sort une classification avec deux bornes inf identiques : ça fait tout buguer
      # Il faut donc tricher
      if(distrib[1] == distrib[2]){
        distrib = classInt::classIntervals(c(Carte$val), nClasses + 1, style="jenks")$brks
        distrib = distrib[2:length(distrib)]
      }
    } else {
      distrib = Brks
    }
  }
  
  # On trace un barplot dans l'angle haut droit pour observer la répartition globale des valeurs
  par(mar= c(2,2,1,1))
  if(Typo==F){hist(Carte$val, breaks=distrib, col=Palette, main="")} else {
    barplot(table(as.factor(as.character(Carte$val))), col=Palette, main="")}
  
  # Préparation du texte des légendes
  txt_legende = paste0(TxtLegende, ifelse(nchar(Unit)>0, paste0(" (", Unit,")"),""))
  txt_legende2 = paste0(TxtLegende2, ifelse(nchar(Unit2)>0, paste0(" (", Unit2,")"),""))
  
  # Préparation du traçage principal (maintenant Carte est directement un objet spatial)
  par(mar= c(1,1,2,1))
  # Carte = left_join(shp.enq, Carte, by=c("uid_ENQ" = "enq")) %>% filter(!uid_ENQ %in% outremer) %>% filter(!is.na(val))
  
  # Pour séparer les enquêtes différentes (nécessite un filtrage par type de géométrie pour éviter les erreurs)
  # CarteLim = Carte
  # CarteLim$Type = st_geometry_type(CarteLim)
  # CarteLim = filter(CarteLim, Type %in% c("POLYGON", "MULTIPOLYGON")) %>% st_cast(CarteLim, to = "MULTIPOLYGON")
  # CarteLim = getBorders(CarteLim)
  
  # Pour avoir les noms en clair des aires d'enquête
  Carte = left_join(Carte, z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ"))
  
  # Cadrage sur les données, fond gris des pays
  plot(filter(Carte, !is.na(val) & outremer==F)$geometry, border=NA)
  # plot(monde$geometry, col="grey80", border=NA, add=T)
  # plot(filter(monde, ISO_A2 == "FR")$geometry, col="grey90", border=NA, add=T)
  if(Typo == F){
    choroLayer(x= Carte, var="val", legend.title.txt = txt_legende, breaks=distrib, border=NA,
               col= Palette, legend.pos="bottomleftextra", legend.values.rnd = 2, add=T)} else {
                 typoLayer(x= Carte, var="val", legend.title.txt = txt_legende, border=NA,
                           col= Palette, legend.pos="bottomleftextra", add=T)}
  # plot(carteLim$geometry, col="grey50", add=T)
  labelLayer(x=Carte, txt="Libelle_Simple", overlap = F, col = "black", show.lines = FALSE, halo = TRUE,cex = .4)
  if (Stocks == T){
    propSymbolsChoroLayer(x= Carte, var="val2_abso", var2="val2_sens", legend.var.pos = "topleft", legend.var2.pos = NA, inches=.25,
                          legend.var.title.txt = txt_legende2,
                          breaks=c(-1,-.1,.1,1), border="grey70", col= colSens, add=T) }
  
  # Cadre propre au package Cartography
  layoutLayer(title = "Hexagone",
              source = "Cerema, 2009-2019",
              author = "Maxime Guinepain",
              scale = 100, north = T,
              frame = T, tabtitle = T, col = "darkslateblue", coltitle = "white")
  
  # Conversion en WGS84 et traçage des outremers
  CarteO = st_transform(filter(Carte, outremer==T), "wgs84")
  
  if (nOutremer>0){
    for(i in 1:nOutremer)
    {
      plot(filter(CarteO[i,], !is.na(val))$geometry, border=NA)
      # plot(monde84$geometry, col="grey80", border=NA, add=T)
      if(Typo==F){
        choroLayer(x= CarteO[i,], var="val", legend.title.txt = "", breaks=distrib, border=NA,
                   col= Palette, legend.pos=NA, add=T)}
      else{ 
        typoLayer(x= CarteO[i,], var="val", legend.title.txt = txt_legende, border=NA,
                  col= Palette, legend.pos=NA, add=T)}
      if (Stocks==T) {
        propSymbolsChoroLayer(x= CarteO[i,], var="val2_abso", var2="val2_sens", legend.var.pos = NA, legend.var2.pos = NA, inches=.25,
                              breaks=c(-1,-.1,.1,1), border="grey70", col= colSens, legend.var.style="c", add=T) }
      layoutLayer(title = CarteO[i,]$Libelle_Simple,
                  scale = 0.0001, north = T,
                  frame = T, tabtitle = T, col = "grey20", coltitle = "white")
    }
    
  }
}

# etiquette = lEnq[i] # libelle = lLib[i]

map_Enquete = function(shp, subshp, subshpz, fdCarte, AAV_montrer=NULL,
                       geonames, libelle, txt_legende, txt_legende2, palette, distrib, prop, stockSup,
                       completerGeonames = F, titreGen = "",
                       osm, simplifier, couleurCadre = "grey20", supprCarton=F, ajPolygone = NULL)
{
  shpTerre   = fdCarte$shpTerre
  shpEau     = fdCarte$shpEau
  shpRoutes1 = fdCarte$shpRoutes1
  shpRoutes2 = fdCarte$shpRoutes2
  shpRoutes3 = fdCarte$shpRoutes3
  shpRail    = fdCarte$shpRail
  shpAAV     = fdCarte$shpAAV
  geonames   = fdCarte$geonames
  
  if (!is.null(ajPolygone)) { if (!"sf" %in% class(ajPolygone)) { stop ("Polygone à ajouter invalide !") } }
  
  boite = st_bbox(subshpz)
  rectangle = matrix(c(boite$xmin, boite$ymin, boite$xmin, boite$ymax, boite$xmax,
                       boite$ymax, boite$xmax, boite$ymin, boite$xmin, boite$ymin), ncol=2, byrow=T) %>%
    list() %>% st_polygon()
  
  diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
    list() %>% st_multilinestring() %>% st_length() / 1000 # la diagonale en km
  
  # Elle va permettre de déterminer le niveau de zoom (de façon très approx...)
  nivZoom = ""
  nivZoom = ifelse(diagonale <  5,                     14, nivZoom)
  nivZoom = ifelse(diagonale >= 5   & diagonale < 10,  13, nivZoom)
  nivZoom = ifelse(diagonale >= 10  & diagonale < 50,  12, nivZoom)
  nivZoom = ifelse(diagonale >= 50  & diagonale < 100, 11, nivZoom)
  nivZoom = ifelse(diagonale >= 100 & diagonale < 500, 10, nivZoom)
  nivZoom = ifelse(diagonale >= 500 & diagonale < 1000, 9, nivZoom)
  nivZoom = ifelse(diagonale >  1000,                NULL, nivZoom)
  
  echelle = 0 # pas trouvé de façon de faire style
  echelle = ifelse(diagonale <  5,                     .1, echelle)
  echelle = ifelse(diagonale >= 5   & diagonale < 10,   1, echelle)
  echelle = ifelse(diagonale >= 10  & diagonale < 50,   1, echelle)
  echelle = ifelse(diagonale >= 50  & diagonale < 100,  5, echelle)
  echelle = ifelse(diagonale >= 100 & diagonale < 500, 10, echelle)
  echelle = ifelse(diagonale >= 500 & diagonale < 1000,50, echelle)
  echelle = ifelse(diagonale >  1000,                 100, echelle)
  
  # Simplification de la géométrie (but = réduire taille du pdf)
  # de l'ordre de 1% de la diagonale
  subshp = st_simplify(subshp, preserveTopology = T, dTolerance = diagonale/100)
  
  # Le fond de carte est également simplifié à cette échelle
  # + on coupe au-delà d'un périmètre étendu les terres et l'eau (des fois qu'il essaye de les ploter hors de la page)
  # + on coupe les routes au-delà de 2% de l'étendue de subshp
  # + un calque intermédiaire (anneau de 5% à 8% pour avoir des pointillés)
  # edit août 2021 : le premier anneau est réglé à 0, le second à 2%
  # edit mars 2022 : on va utiliser st_crop sur le cadre de la carte
  # 
  # perimetreL = st_convex_hull(subshp) %>% st_buffer(dist = diagonale*2000) %>% mutate(t=1) %>% group_by(t) %>% summarize()
  # 
  # perimetre = st_convex_hull(subshp) %>% st_buffer(dist = 0) %>% mutate(t=1) %>% group_by(t) %>% summarize()
  # perimetreP = st_convex_hull(subshp) %>% st_buffer(dist = diagonale*20) %>%
  #     st_difference(perimetre) %>% mutate(t=1) %>% group_by(t) %>% summarize()
  
  if (!osm) {
    l.shpTerre   = shpTerre %>%   st_simplify(preserveTopology = T, dTolerance=diagonale/50)
    l.shpEau     = shpEau %>%     st_simplify(preserveTopology = T, dTolerance=diagonale/50)
    l.shpRoutes1 = shpRoutes1 %>% st_simplify(preserveTopology = T, dTolerance=diagonale/20)
    if(!simplifier){
      l.shpRoutes2 = shpRoutes2 %>% st_simplify(preserveTopology = T, dTolerance=diagonale/20)
      l.shpRoutes3 = shpRoutes3 %>% st_simplify(preserveTopology = T, dTolerance=diagonale/20)}
    
    l.shpRail    = shpRail    %>% st_simplify(preserveTopology = T, dTolerance=diagonale/20)}
  
  # pl.shpRoutes1 = shpRoutes1 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)
  # if(simplifier==F){
  #     pl.shpRoutes2 = shpRoutes2 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)
  #     pl.shpRoutes3 = shpRoutes3 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)}
  # pl.shpRail    = shpRail    %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)
  
  # On affiche les noms de lieux avec un seuil de population qui varie selon l'échelle :
  
  #plot(subshp$geometry, border=NA)
  
  if(!completerGeonames | !supprCarton)
  {
    plot(rectangle, col=NA, border=NA, main=titreGen)
  } else {
    plot(rectangle, col=NA, border=NA)
  }
  
  # on coupe ce qui dépasse !
  boxcrop = st_bbox(st_sfc(st_point(c(par("usr")[1], par("usr")[3])),
                           st_point(c(par("usr")[2], par("usr")[4])),
                           crs = st_crs(shp))) %>%
    st_as_sfc() %>% st_as_sf()
  if(!osm) {
    l.shpTerre   = st_intersection (l.shpTerre,   boxcrop)
    subshp       = st_intersection (subshp,       boxcrop)
    l.shpEau     = st_intersection (l.shpEau,     boxcrop)
    l.shpRoutes1 = st_intersection (l.shpRoutes1, boxcrop)
    if (!simplifier){
      l.shpRoutes2 = st_intersection (l.shpRoutes2, boxcrop)
      l.shpRoutes3 = st_intersection (l.shpRoutes3, boxcrop)}
    l.shpRail    = st_intersection (l.shpRail,    boxcrop)
    
    l.geonames = filter(geonames, feature_class == "P") %>%
      st_intersection (boxcrop)
    
    filtre = 2.5
    l.geonames.f = filter(l.geonames, population > diagonale ^ filtre)
    
    while(nrow(l.geonames.f) < 8 & filtre > .1)
    {
      filtre = filtre - .1
      l.geonames.f = filter(l.geonames, population > diagonale ^ filtre)
      cat("\nfiltre geonames :", filtre, "/ nombre d'entités :", nrow(l.geonames.f))
    }
    
    l.geonames = l.geonames.f
    
    nomCentre = l.geonames[which.max(l.geonames$population),]$name
    
    if (completerGeonames & supprCarton) { titreGen = paste0(titreGen, "\n", "Zoom sur ", nomCentre) }
    
    plot(rectangle, col=NA, border=NA, main = titreGen, add=T)
    
    
    if(!is.null(AAV_montrer))
    {
      aav = filter(shpAAV, AAV2020 == AAV_montrer)
    }
  }
  
  # if(nrow(l.geonames) == 0)
  # {l.geonames = filter(geonames, feature_class == "P" & population > diagonale^1) %>%
  #     st_intersection (boxcrop)}
  # 
  # if(nrow(l.geonames) == 0)
  # {l.geonames = filter(geonames, feature_class == "P" & population > diagonale^.5) %>%
  #     st_intersection (boxcrop)}
  
  if (osm) {
    osmTiles = getTiles(subshp, cachedir=T, crop=F, zoom=nivZoom, type="CartoDB.VoyagerNoLabels")
    tilesLayer(osmTiles, add=T)
  } else {
    #plot(rectangle, border=NA, col="lightblue1")
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue1")
    plot(l.shpTerre$geometry, border = NA, col = "grey95", add=T)
    
  }
  
  if (stockSup & !osm)
  {
    plot(subshp$geometry, col="grey87", border="grey24", lwd=.8, lty=3, add=T)
    plot(l.shpEau$geometry, border = NA, col = "lightblue1", add=T)
    if (!simplifier)
    {
      if(diagonale*1000 < 100) {plot(l.shpRoutes3$geometry, col = "#F68EA2", lwd=1, add=T)}
      plot(l.shpRoutes2$geometry, col = "#F68EA2", lwd=1, add=T)
      plot(l.shpRoutes1$geometry, col = "#EC6880", lwd=1.2, add=T)
      plot(l.shpRail$geometry, col= "#b3908a", ldw=1, add=T)
    }
    if (!is.null(AAV_montrer))
    {
      plot(aav$geometry, border = "darkslateblue", lwd=2, col=NA, add=T)
    }
    if (!is.null(ajPolygone))
    {
      plot(st_transform(ajPolygone, crs=3857)$geometry, border = "slateblue", lwd=2, col=NA, add=T)
    }
  }
  
  if (is.numeric(shp$val) & prop & !stockSup) {
    choroLayer(x= subshp, var="val", breaks=distrib, border=NA,
               colNA  = "white",
               col= palette, legend.pos="none", legend.values.rnd = 2, add=T)
    plot(subshp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
  }
  if (!is.numeric(shp$val) & prop & !stockSup) {
    
    # on ne retient que les niveaux présents, on les met dans le bon ordre
    nouvlevels = levels(shp$val)[levels(shp$val) %in% unique(subshp$val)]
    subshp$val = factor(subshp$val, nouvlevels)
    
    # une sous-palette avec uniquement les couleurs de l'échantillon
    subpal = palette[levels(shp$val) %in% levels(subshp$val)]
    
    typoLayer(x= subshp, var="val", border=NA, colNA  = "white",
              col= subpal, legend.pos="none", legend.values.order = nouvlevels, add=T)
    plot(subshp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
  }
  if (is.numeric(shp$val) & prop & stockSup) {
    propSymbolsChoroLayer(x= subshp, var="val2", var2="val",
                          breaks=distrib, colNA  = "white", legend.var2.values.rnd = 2,
                          col= palette, legend.var.pos="none", legend.var2.pos = "none", add=T)
  }
  if (!is.numeric(shp$val) & prop & stockSup) {
    nouvlevels = levels(shp$val)[levels(shp$val) %in% unique(subshp$val)]
    subshp$val = factor(subshp$val, nouvlevels)
    subpal = palette[levels(shp$val) %in% levels(subshp$val)]
    propSymbolsTypoLayer(x= subshp, var="val2", var2="val",
                         colNA  = "white", col= subpal,
                         legend.var.pos="none", legend.var2.pos = "none",
                         legend.var2.values.order = nouvlevels, add=T)
  }
  
  if(!osm)
  {
    if (!stockSup) # si c'est pas des chorosymboles, on met devant ; sinon, derrière
    {
      plot(l.shpEau$geometry, border = NA, col = "lightblue1", add=T)
      if (!is.null(AAV_montrer))
      {
        plot(aav$geometry, border = "darkslateblue", lwd=2, col=NA, add=T)
      }
      if (!is.null(ajPolygone))
      {
        plot(st_transform(ajPolygone, crs=3857)$geometry, border = "slateblue", lwd=2, col=NA, add=T)
      }
    }
    
    if(!simplifier & !stockSup){
      if(diagonale*1000 < 100) {plot(l.shpRoutes3$geometry, col = "#F68EA2", lwd=1, add=T)}
      plot(l.shpRoutes2$geometry, col = "#F68EA2", lwd=1, add=T)
    }
    
    if (!stockSup)
    {
      plot(l.shpRoutes1$geometry, col = "#EC6880", lwd=1.2, add=T)
      plot(l.shpRail$geometry, col= "#b3908a", ldw=1, add=T)
      
      if (nrow(l.geonames) > 0){labelLayer(x= l.geonames, txt="name", cex=.7, overlap=F, halo=T)}
    }
    
    if (is.numeric(shp$val) & prop == T) {
      legendChoro(pos = "bottomleftextra", title.txt = txt_legende, breaks = distrib,
                  col = palette, values.rnd = 2, 
                  nodata.col = "white", nodata.txt = "Pas de données")
    }
    if (!is.numeric(shp$val) & prop == T) {
      legendTypo(pos = "bottomleftextra", title.txt = txt_legende, 
                 col = subpal, categ = nouvlevels,
                 nodata.col = "white",nodata.txt = "Pas de données")
    }
    if (stockSup)
    {
      legendCirclesSymbols(pos = "topleft", title.txt = txt_legende2,
                           var = c(min(shp$val2, na.rm=T), max(shp$val2, na.rm=T)),
                           inches = .3) # valeur par défaut de la fonction de traçage
    }
    
    
    if (completerGeonames) { libelle = paste0(libelle, nomCentre)}
    
    if (!supprCarton)
    {
      layoutLayer(title = libelle, sources="Maxime Guinepain",
                  author="Données B.U. du Cerema (2009-2019) / Insee / IGN Route500 et BDTopo / Natural Earth",
                  scale = echelle, north = T,
                  frame = T, tabtitle = T, col = couleurCadre, coltitle = "white")
    } else {
      layoutLayer(sources="Maxime Guinepain",
                  author="Données B.U. du Cerema (2009-2019) / Insee / IGN Route500 et BDTopo / Natural Earth",
                  scale = echelle, north = T, 
                  frame = T, col = couleurCadre)
    }
    
  }
}



map_initCarteParEnquete = function(simplifier = F, proj = 3857)
{
  rapport("Génération du cache de cartographie pour toutes les enquêtes")
  # chargement du fond de carte
  cat("\rInitialisation... Chargement Fond de carte Terres")
  shpTerre   = st_read("Sources/Fond Carte/Vecto_Terres.shp" , quiet=T) %>% st_transform(crs=proj) 
  shpEau     = st_read("Sources/Fond Carte/Vecto_Eau.shp"    , quiet=T) %>% st_transform(crs=proj)
  cat("\rInitialisation... Chargement Fond de carte Routes")
  shpRail    = st_read("Sources/Fond Carte/Vecto_Rail.shp"   , quiet=T) %>% st_transform(crs=proj) 
  shpRoutes1 = st_read("Sources/Fond Carte/Vecto_Routes1.shp", quiet=T) %>% st_transform(crs=proj)
  if(simplifier == F){
    cat("\rInitialisation... Chargement Fond de carte Routes (niv. 2)")
    shpRoutes2 = st_read("Sources/Fond Carte/Vecto_Routes2.shp", quiet=T) %>% st_transform(crs=proj)
    cat("\rInitialisation... Chargement Fond de carte Routes (niv. 3)")
    shpRoutes3 = st_read("Sources/Fond Carte/Vecto_Routes3.shp", quiet=T) %>% st_transform(crs=proj)}
  else{shpRoutes2 = NULL ; shpRoutes3 = NULL}
  cat("\rInitialisation... Chargement Fond de carte Noms               ")
  geonames = load_geonames() %>% st_transform(crs = proj)
  
  cat("\rInitialisation... Chargement Fond de carte AAVs               ")
  aav = read_delim("Sources/Mailles/Zonage Attraction.csv", delim = ";")
  load("Data/shp_COM.rds")
  shpAAV = left_join(shp_COM, aav, by=c("insee" = "CODGEO")) ; remove(shp_COM)
  shpAAV = shpAAV %>%
    group_by(AAV2020) %>%
    summarize(lib = first(LIBAAV2020)) %>%
    st_transform(crs=proj)
  
  track = setClass("jeuShp", slots = c(x="numeric", y="numeric"))
  
  shps = list("shpTerre"   = shpTerre,
              "shpEau"     = shpEau,
              "shpRail"    = shpRail,
              "shpRoutes1" = shpRoutes1,
              "shpRoutes2" = shpRoutes2,
              "shpRoutes3" = shpRoutes3,
              "shpAAV"     = shpAAV,
              "geonames"   = geonames)
  
  return(shps)
}

# CarteParEnquete trace un "atlas" avec une page représentant chaque enquête, à sa propre échelle.
# shp contient la géométrie et les valeurs à cartographier. Il faut spécifier avec pdf le chemin vers le fichier de sortie.
# On peut choisir une discrétisation automatique (nClasses et mClasses pour la méthode : "jenks" ou "quantile"), ou manuelle avec brks.
# La fonction a besoin que z_Nomenclature soit présente dans l'espace de travail ou qu'on la lui fournisse.

map_CarteParEnquete = function(shp, colUidEnq = "uid_ENQ",
                               colVal, colVal2 = NULL, colVal3 = NULL, colVal4 = NULL,
                               pdf = NULL, format="a4", paysage=F, titre, legende,
                               unite = "", legende2 = "", nClasses = 0, mClasses = "jenks", # accepte quantile
                               cartes4 = F, 
                               palette= NULL, brks= NA, prop= T, stockSup = F, nomenclature = z_Nomenclature, etiquettes = NULL,
                               osm= F, verb= F, autoriserZooms= T, simplifier=F,
                               fdCarte = NULL, montrerAAV = NULL, supprCarton = F, ajPolygone = NULL)
{
  library(sf) ; library(cartography) ; library(classInt)
  
  # si rien n'est précisé, portrait A4
  # si on laisse "pdf" en vide, la sortie se fait directement dans le device
  w = 8.25 ; h = 11.75
  if (format == "a3") { w = 11.75 ; h = 16 }
  if (paysage==T) { x = w ; w = h ; h = x }
  
  if (!is.null(pdf)){
    pdf(pdf, width=w, height=h)
  }
  
  .pardefaut = par()
  par(mar = c(1,1,3.5,1))
  
  if (cartes4)
  {
    par(mfrow = c(2,2))
    simplifier = T #de toute façon
  }
  
  cat("Initialisation...")
  colnames(shp)[colnames(shp) == colUidEnq] = "uid_ENQ"
  colnames(shp)[colnames(shp) == colVal]    = "val"
  if (stockSup) {
    if (is.null(colVal2)) { stop("Spécifier une 2e variable pour les stocks") }
    colnames(shp)[colnames(shp) == colVal2] = "val2"
  }
  if (cartes4)
  {
    if (is.null(colVal2) | is.null(colVal3) | is.null(colVal4)) { stop("Spécifier variables 2:4") }
    colnames(shp)[colnames(shp) == colVal2] = "val2"
    colnames(shp)[colnames(shp) == colVal3] = "val3"
    colnames(shp)[colnames(shp) == colVal4] = "val4"
  }
  
  lEnq = unique(shp$uid_ENQ)
  nEnq = length(unique(shp$uid_ENQ))
  
  shp = st_transform(shp, crs=3857) # conversion pour toutes les couches en 1
  
  if (osm == F)
  {
    # Si les données vectorielles n'ont pas encore été chargées, on les charge
    if (is.null(fdCarte)) { load("Data/fdCarte.rds") }
  }
  else
  {
    fdCarte = list("shpTerre" = NULL,
                   "shpEau" = NULL,
                   "shpRail" = NULL,
                   "shpRoutes1" = NULL,
                   "shpRoutes2" = NULL,
                   "shpRoutes3" = NULL)
  }
  
  
  if (nClasses == 0) {nClasses = length(brks) - 1}
  
  txt_legende = paste0(legende, ifelse(nchar(unite)>0, paste0(" (", unite,")"),""))
  
  shp = left_join(shp, z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ"))
  
  lLib = unique(shp$Libelle_Long)
  
  #cat("\rInitialisation... simplification de la géométrie...")
  
  #shp = st_buffer(st_as_sf(shp), dist = 0) %>% st_make_valid()
  #shp = st_simplify(st_as_sf(shp), dTolerance=facteurSpl)
  
  cat("\rInitialisation... OK                                        ")
  
  if(is.numeric(shp$val)){
    if(length(brks)==1)
    {
      distrib = classInt::classIntervals(c(shp$val), nClasses, style=mClasses)$brks
      
      # Parfois Jenks sort une classification avec deux bornes inf identiques : ça fait tout buguer
      # Il faut donc tricher
      if(distrib[1] == distrib[2]){
        distrib = classInt::classIntervals(c(shp$val), nClasses + 1, style=mClasses)$brks
        distrib = distrib[2:length(distrib)]
      }
      
    } else {
      distrib = brks
    }
  }
  
  if (cartes4)
  {
    if(is.numeric(shp$val2)){
      if(length(brks)==1)
      {
        distrib = classInt::classIntervals(c(shp$val2), nClasses, style=mClasses)$brks
        if(distrib[1] == distrib[2]){
          distrib = classInt::classIntervals(c(shp$val2), nClasses + 1, style=mClasses)$brks
          distrib = distrib[2:length(distrib)]
        }
        
      } else {
        distrib = brks
      }
    }
    if(is.numeric(shp$val3)){
      if(length(brks)==1)
      {
        distrib = classInt::classIntervals(c(shp$val3), nClasses, style=mClasses)$brks
        if(distrib[1] == distrib[2]){
          distrib = classInt::classIntervals(c(shp$val3), nClasses + 1, style=mClasses)$brks
          distrib = distrib[2:length(distrib)]
        }
        
      } else {
        distrib = brks
      }
    }
    if(is.numeric(shp$val4)){
      if(length(brks)==1)
      {
        distrib = classInt::classIntervals(c(shp$val4), nClasses, style=mClasses)$brks
        if(distrib[1] == distrib[2]){
          distrib = classInt::classIntervals(c(shp$val4), nClasses + 1, style=mClasses)$brks
          distrib = distrib[2:length(distrib)]
        }
        
      } else {
        distrib = brks
      }
    }
  }
  
  if(is.character(shp$val)){
    shp$val = as.factor(shp$val)
  }
  
  if (is.null(palette))
  {
    if (is.numeric(shp$val)) { palette = carto.pal("wine.pal", nClasses) }
    if (is.factor(shp$val))  { palette = carto.pal("multi.pal", length(levels(shp$val))) }
    cat("palette automatique", palette)
  }
  
  if(cartes4)
  {
    if(is.character(shp$val2)) {shp$val2 = as.factor(shp$val2)}
    if(is.character(shp$val3)) {shp$val3 = as.factor(shp$val3)}
    if(is.character(shp$val4)) {shp$val4 = as.factor(shp$val4)}
  }
  
  if(is.factor(shp$val)){
    if (!is.null(etiquettes)){
      levels(shp$val) = etiquettes
    }
    # par commodité, on ne remappera pas les autres cartes en cas de 4 cartes
  }
  
  tempsdeb = Sys.time() ; estim = 0 ; estimt = 0
  
  if (nEnq>1){
    cat(paste0("\nImpression de ", as.character(nEnq)," cartes"))
    if (autoriserZooms == T) { cat(" + zooms éventuels")}
    if (cartes4) {cat("en 4 encadrés")}
    cat("\n")
  }
  
  cat(sprintf('\r[%-50s] %d%%', paste(rep('=', 0 / 2), collapse = ''), floor(0)))
  for(i in 1:nEnq)
  {
    subshp = filter(shp, uid_ENQ == lEnq[i])
    
    if (length(unique(subshp$val)) > 1)
    {
      cat(paste(" >> Enq."), lEnq[i], rep(" ", 18))
      
      # edit mars 2022 : pour limiter la redondance, le processus de carto a été décalé
      # dans une fonction autonome
      
      if (cartes4) {pal1 = palette[[1]]}   # un problème dans ifelse me force à faire ça ici
      else         {pal1 = palette}        # R est vraiment codé AVEC LES PIEDS
      
      map_Enquete(shp = shp, subshp = subshp, subshpz = subshp,
                  fdCarte = fdCarte,
                  txt_legende = txt_legende[1],
                  txt_legende2 = legende2[1],
                  palette = pal1,
                  distrib = distrib, prop = prop,
                  stockSup = stockSup,
                  libelle = lLib[i],
                  osm = osm,
                  simplifier = simplifier,
                  titreGen = titre,
                  AAV_montrer = montrerAAV,
                  supprCarton = supprCarton,
                  ajPolygone = ajPolygone)
      
      if(cartes4)
      {
        map_Enquete(shp    = rename(select(   shp, uid_ENQ, val2), val = val2),
                    subshp = rename(select(subshp, uid_ENQ, val2), val = val2),
                    subshpz = subshp, fdCarte = fdCarte,
                    txt_legende = txt_legende[2], txt_legende2 = legende2[2],
                    palette = palette[[2]], distrib = distrib, prop = prop,
                    stockSup = stockSup,
                    libelle = lLib[i], titreGen = titre,
                    osm = osm, simplifier = simplifier,
                    AAV_montrer = montrerAAV,
                    supprCarton = T,
                    ajPolygone = ajPolygone)
        map_Enquete(shp    = rename(select(   shp, uid_ENQ, val3), val = val3),
                    subshp = rename(select(subshp, uid_ENQ, val3), val = val3),
                    subshpz = subshp, fdCarte = fdCarte,
                    txt_legende = txt_legende[3], txt_legende2 = legende2[3],
                    palette = palette[[3]], distrib = distrib, prop = prop,
                    stockSup = stockSup,
                    libelle = lLib[i], titreGen = titre,
                    osm = osm, simplifier = simplifier,
                    AAV_montrer = montrerAAV,
                    supprCarton = T,
                    ajPolygone = ajPolygone)
        map_Enquete(shp    = rename(select(   shp, uid_ENQ, val4), val = val4),
                    subshp = rename(select(subshp, uid_ENQ, val4), val = val4),
                    subshpz = subshp, fdCarte = fdCarte,
                    txt_legende = txt_legende[4], txt_legende2 = legende2[4],
                    palette = palette[[4]], distrib = distrib, prop = prop,
                    stockSup = stockSup,
                    libelle = lLib[i], titreGen = titre,
                    osm = osm, simplifier = simplifier,
                    AAV_montrer = montrerAAV,
                    supprCarton = T,
                    ajPolygone = ajPolygone)
      }
      
      if (autoriserZooms == T) {
        # Détection d'un cluster d'unités trop petites pour être lues distinctement :
        # celles dont la superficie est inférieure à 2 pourmilles de toute la zone
        petitesZones = subshp %>% mutate(superficie = st_area(.)) %>%
          filter(superficie < (sum(superficie) / 500)) %>%
          filter(as.numeric(superficie) > 0)
        
        boite = st_bbox(subshp)
        diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
          list() %>% st_multilinestring() %>% st_length() / 1000
        
        # Forment-elles des unités cohérentes ?
        clustersPZ = petitesZones %>% st_centroid() %>% st_buffer(dist = diagonale*1000 / 50) 
        
        if (nrow(clustersPZ) > 0)
        {
          # Formation de clusters,
          # ref https://gis.stackexchange.com/questions/323038/dissolve-only-overlapping-polygons-in-r-using-sf
          parts = st_cast(st_union(clustersPZ),"POLYGON")
          clust = unlist(st_intersects(clustersPZ, parts))
          diss = cbind(clustersPZ, clust) %>%
            group_by(clust) %>%
            summarize(N = n())
          
          # Liste des clusters contenant plus de 10 éléments
          clustersPZ = filter(diss, N > 10)
        }
        
        if(nrow(clustersPZ) > 0)
        {
          # Cartographie des clusters concernés
          
          for(j in 1:nrow(clustersPZ))
          {
            z.boite = st_bbox(clustersPZ[j,])
            
            if (cartes4) {pal1 = palette[[1]]}
            else         {pal1 = palette}
            
            map_Enquete(shp = shp, subshp = subshp, subshpz = clustersPZ[j,],
                        fdCarte = fdCarte,
                        txt_legende = txt_legende[1], txt_legende2 = legende2[1],
                        palette = pal1, titreGen = titre,
                        distrib = distrib, prop = prop,
                        stockSup = stockSup, 
                        libelle = paste0(lLib[i], " : Zoom sur "), completerGeonames = T,
                        osm = osm, simplifier = simplifier, couleurCadre = "slateblue",
                        AAV_montrer = montrerAAV,
                        supprCarton=supprCarton,
                        ajPolygone = ajPolygone)
            if(cartes4)
            {
              map_Enquete(shp    = rename(select(   shp, uid_ENQ, val2), val = val2),
                          subshp = rename(select(subshp, uid_ENQ, val2), val = val2),
                          subshpz = clustersPZ[j,],fdCarte = fdCarte,
                          txt_legende = txt_legende[2], txt_legende2 = legende2[2],
                          palette = palette[[2]], distrib = distrib, prop = prop,
                          stockSup = stockSup,  titreGen=titre,
                          libelle = paste0(lLib[i], " : Zoom sur "), completerGeonames = T,
                          osm = osm, simplifier = simplifier,
                          AAV_montrer = montrerAAV,
                          supprCarton=T,
                          ajPolygone = ajPolygone)
              map_Enquete(shp    = rename(select(   shp, uid_ENQ, val3), val = val3),
                          subshp = rename(select(subshp, uid_ENQ, val3), val = val3),
                          subshpz = clustersPZ[j,], fdCarte = fdCarte,
                          txt_legende = txt_legende[3], txt_legende2 = legende2[3],
                          palette = palette[[3]], distrib = distrib, prop = prop,
                          stockSup = stockSup,  titreGen=titre,
                          libelle = paste0(lLib[i], " : Zoom sur "), completerGeonames = T,
                          osm = osm, simplifier = simplifier,
                          AAV_montrer = montrerAAV,
                          supprCarton=T,
                          ajPolygone = ajPolygone)
              map_Enquete(shp    = rename(select(   shp, uid_ENQ, val4), val = val4),
                          subshp = rename(select(subshp, uid_ENQ, val4), val = val4),
                          subshpz = clustersPZ[j,], fdCarte = fdCarte,
                          txt_legende = txt_legende[4], txt_legende2 = legende2[4],
                          palette = palette[[4]], distrib = distrib, prop = prop,
                          stockSup = stockSup,  titreGen=titre,
                          libelle = paste0(lLib[i], " : Zoom sur "), completerGeonames = T,
                          osm = osm, simplifier = simplifier,
                          AAV_montrer = montrerAAV,
                          supprCarton=T,
                          ajPolygone = ajPolygone)
            }
          }
        }
      }
    }
    tps    = difftime(Sys.time(), tempsdeb, units = "secs") # estimation du temps restant par proportionnalité
    estimt = ((tps * nEnq / i) - tps) / 60
    
    # code pour afficher une barre de prog' piqué sur internet
    cat(sprintf('\r[%-50s] %d%% .. reste %.1f mn.', paste(rep('=', (i/nEnq*100) / 2), collapse = ''),
                floor((i/nEnq*100)), estimt))
  }
  cat("\n")
  
  if (!is.null(pdf))
  {
    dev.off()
  }
  
  par(.pardefaut)
}


map_CarteParEnqVal = function(shp, plagesval, pdf, format="a4", paysage=F, titre, legende,
                              unit = rep("", length(plagesval)), nClasses = rep(0, length(plagesval)),
                              mClasses = rep("jenks", length(plagesval)),
                              palette= NULL, brks= NA, nomenclature = z_Nomenclature,
                              etiquettes = NULL,  osm= F, verb= F, autoriserZooms= T)
{
  library(sf) ; library(cartography) ; library(classInt)
  
  # si rien n'est précisé, portrait A4
  w = 8.25 ; h = 11.75
  
  if (paysage==T) { x = w ; w = h ; h = x }
  
  pdf(pdf, width=w, height=h)
  
  .pardefaut = par()
  par(mar = c(1,1,2,1))
  
  cat("Initialisation...")
  colnames(shp)[1] = "uid_ENQ"
  
  lEnq = unique(shp$uid_ENQ)
  nEnq = length(unique(shp$uid_ENQ))
  
  shp = st_transform(shp, crs=3857) # conversion pour toutes les couches en 1
  
  if (osm == F)
  {
    # chargement du fond de carte
    cat("\rInitialisation... Chargement Fond de carte Terres")
    shpTerre   = st_read("Sources/Fond Carte/Vecto_Terres.shp" , quiet=T) %>% st_transform(crs=3857) 
    shpEau     = st_read("Sources/Fond Carte/Vecto_Eau.shp"    , quiet=T)
    cat("\rInitialisation... Chargement Fond de carte Routes")
    shpRoutes1 = st_read("Sources/Fond Carte/Vecto_Routes1.shp", quiet=T) 
    cat("\rInitialisation... Chargement Fond de carte Noms  ")
    geonames = load_geonames()
  }
  
  
  if (nClasses == 0) {nClasses = length(brks) - 1}
  if (is.null(palette)) {palette = carto.pal("wine.pal", nClasses)}
  
  txt_legende = paste0(legende, ifelse(nchar(unit)>0, paste0(" (", unit,")"),""))
  
  shp = left_join(shp, z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ"))
  
  lLib = unique(shp$Libelle_Long)
  
  #cat("\rInitialisation... simplification de la géométrie...")
  
  #shp = st_buffer(st_as_sf(shp), dist = 0) %>% st_make_valid()
  #shp = st_simplify(st_as_sf(shp), dTolerance=facteurSpl)
  
  cat("\rInitialisation... OK                                        ")
  
  for(indexVal in 1:length(plagesval)) # Ajustement des différentes variables de la plage
  {
    if(is.numeric(shp[[1,]])){
      if(length(brks)==1)
      {
        distrib = classInt::classIntervals(c(shp$val), nClasses, style=mClasses)$brks
        
        # Parfois Jenks sort une classification avec deux bornes inf identiques : ça fait tout buguer
        # Il faut donc tricher
        if(distrib[1] == distrib[2]){
          distrib = classInt::classIntervals(c(shp$val), nClasses + 1, style=mClasses)$brks
          distrib = distrib[2:length(distrib)]
        }
        
      } else {
        distrib = brks
      }
    }
    
    if(is.character(shp$val)){
      shp$val = as.factor(shp$val)
    }
    
    if(is.factor(shp$val)){
      if (!is.null(etiquettes)){
        levels(shp$val) = etiquettes
      }
    }
    
  }
  
  
  
  
  tempsdeb = Sys.time() ; estim = 0 ; estimt = 0
  
  if (nEnq>1){
    cat(paste0("\nImpression de ", as.character(nEnq)," planches"))
    if (autoriserZooms == T) { cat(" + zooms éventuels")}
    cat("\n")
  }
  
  cat(sprintf('\r[%-50s] %d%%', paste(rep('=', 0 / 2), collapse = ''), floor(0)))
  for(i in 1:nEnq)
  {
    subshp = filter(shp, uid_ENQ == lEnq[i])
    
    cat(paste(" >> Enq."), lEnq[i], rep(" ",10))
    
    boite = st_bbox(subshp)
    rectangle = matrix(c(boite$xmin, boite$ymin, boite$xmin, boite$ymax, boite$xmax,
                         boite$ymax, boite$xmax, boite$ymin, boite$xmin, boite$ymin), ncol=2, byrow=T) %>%
      list() %>% st_polygon()
    
    diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
      list() %>% st_multilinestring() %>% st_length() / 1000 # la diagonale en km
    
    # Elle va permettre de déterminer le niveau de zoom (de façon très approx...)
    nivZoom = ""
    nivZoom = ifelse(diagonale <  5,                     14, nivZoom)
    nivZoom = ifelse(diagonale >= 5   & diagonale < 10,  13, nivZoom)
    nivZoom = ifelse(diagonale >= 10  & diagonale < 50,  12, nivZoom)
    nivZoom = ifelse(diagonale >= 50  & diagonale < 100, 11, nivZoom)
    nivZoom = ifelse(diagonale >= 100 & diagonale < 500, 10, nivZoom)
    nivZoom = ifelse(diagonale >= 500 & diagonale < 1000, 9, nivZoom)
    nivZoom = ifelse(diagonale >  1000,                NULL, nivZoom)
    
    echelle = 0 # pas trouvé de façon de faire style
    echelle = ifelse(diagonale <  5,                     .1, echelle)
    echelle = ifelse(diagonale >= 5   & diagonale < 10,   1, echelle)
    echelle = ifelse(diagonale >= 10  & diagonale < 50,   1, echelle)
    echelle = ifelse(diagonale >= 50  & diagonale < 100,  5, echelle)
    echelle = ifelse(diagonale >= 100 & diagonale < 500, 10, echelle)
    echelle = ifelse(diagonale >= 500 & diagonale < 1000,50, echelle)
    echelle = ifelse(diagonale >  1000,                 100, echelle)
    
    # Simplification de la géométrie (but = réduire taille du pdf)
    # de l'ordre de 1% de la diagonale
    subshp = st_simplify(subshp, preserveTopology = T, dTolerance = diagonale/100)
    
    # Le fond de carte est également simplifié à cette échelle
    # + on coupe au-delà d'un périmètre étendu les terres et l'eau (des fois qu'il essaye de les ploter hors de la page)
    # + on coupe les routes au-delà de 5% de l'étendue de subshp
    # + un calque intermédiaire (anneau de 5% à 8% pour avoir des pointillés)
    perimetreL = st_convex_hull(subshp) %>% st_buffer(dist = diagonale*2000) %>% mutate(t=1) %>% group_by(t) %>% summarize()
    
    perimetre = st_convex_hull(subshp) %>% st_buffer(dist = 0) %>% mutate(t=1) %>% group_by(t) %>% summarize()
    perimetreP = st_convex_hull(subshp) %>% st_buffer(dist = diagonale*20) %>%
      st_difference(perimetre) %>% mutate(t=1) %>% group_by(t) %>% summarize()
    
    l.shpTerre   = shpTerre %>% st_intersection(perimetreL) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/100)
    l.shpEau     = shpEau %>% st_intersection(perimetreL) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/100)
    l.shpRoutes1 = shpRoutes1 %>% st_intersection(perimetre) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)
    
    pl.shpRoutes1 = shpRoutes1 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=diagonale/25)
    
    # On affiche les noms de lieux avec un seuil de population qui varie selon l'échelle :
    
    l.geonames = filter(geonames, feature_class == "P" & population > diagonale^1.9) %>%
      st_intersection(perimetre)
    
    if(nrow(l.geonames) == 0)
    {l.geonames = filter(geonames, feature_class == "P" & population > diagonale^1.5) %>%
      st_intersection(perimetre)}
    
    for(indexVal in 1:length(plagesval))
    {
      cartoSubShp = subShp ; libVal = colnames(cartoSubShp)[indexVal]
      colnames(cartoSubShp)[indexVal] = "val"
      
      #plot(subshp$geometry, border=NA)
      plot(rectangle, col=NA, border=NA)
      
      if (osm==T) {
        osm = getTiles(cartoSubShp, cachedir=T, crop=F, zoom=nivZoom, type="CartoDB.VoyagerNoLabels")
        tilesLayer(osm, add=T)
      } else {
        #plot(rectangle, border=NA, col="lightblue1")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue1")
        plot(l.shpTerre$geometry, border = NA, col = "grey95", add=T)
        
      }
      
      if (is.numeric(shp$val) & prop == T) {
        choroLayer(x= cartoSubShp, var="val", legend.title.txt = txt_legende, breaks=distrib, border=NA,
                   colNA  = "white",
                   col= palette, legend.pos="none", legend.values.rnd = 2, add=T)
        plot(cartoSubShp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
      }
      if (!is.numeric(shp$val) & prop == T) {
        
        # on ne retient que les niveaux présents, on les met dans le bon ordre
        nouvlevels = levels(shp$val)[levels(shp$val) %in% unique(cartoSubShp$val)]
        cartoSubShp$val = factor(cartoSubShp$val, nouvlevels)
        
        # une sous-palette avec uniquement les couleurs de l'échantillon
        subpal = palette[levels(shp$val) %in% levels(cartoSubShp$val)]
        
        typoLayer(x= cartoSubShp, var="val", legend.title.txt = txt_legende, border=NA, colNA  = "white",
                  col= subpal, legend.pos="none", legend.values.order = nouvlevels, add=T)
        plot(cartoSubShp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
      }
      if(osm==F)
      {
        plot(l.shpEau$geometry, border = NA, col = "lightblue1", add=T)
        plot(l.shpRoutes1$geometry, col = "#EC6880", lwd=1.5, add=T)
        plot(pl.shpRoutes1$geometry, col = "#EC6880", lwd=1.5, lty=2, add=T)
        
        if (nrow(l.geonames) > 0){labelLayer(x= l.geonames, txt="name", cex=.7, overlap=F, halo=T)}
        
      }
      
      if (is.numeric(shp$val) & prop == T) {
        legendChoro(pos = "bottomleftextra", title.txt = txt_legende, breaks = distrib,
                    col = palette, values.rnd = 2, 
                    nodata.col = "white", nodata.txt = "Pas de données")
      }
      if (!is.numeric(shp$val) & prop == T) {
        legendTypo(pos = "bottomleftextra", title.txt = txt_legende, 
                   col = subpal, categ = nouvlevels, 
                   nodata.col = "white",nodata.txt = "Pas de données")
      }
      
      layoutLayer(title = paste0(lLib[i], " : ", libVal), sources="Maxime Guinepain",
                  author="Données B.U. du Cerema (2009-2019) / Insee / IGN Route500 et BDTopo / Natural Earth",
                  scale = echelle, north = T,
                  frame = T, tabtitle = T, col = "grey20", coltitle = "white")
      
      if (autoriserZooms == T) {
        # Détection d'un cluster d'unités trop petites pour être lues distinctement :
        # celles dont la superficie est inférieure à 2 pourmilles de toute la zone
        petitesZones = cartoSubShp %>% mutate(superficie = st_area(.)) %>%
          filter(superficie < (sum(superficie) / 500)) %>%
          filter(as.numeric(superficie) > 0)
        
        # Forment-elles des unités cohérentes ?
        clustersPZ = petitesZones %>% st_centroid() %>% st_buffer(dist = diagonale*1000 / 50) 
        
        if (nrow(clustersPZ) > 0)
        {
          # Formation de clusters, ref https://gis.stackexchange.com/questions/323038/dissolve-only-overlapping-polygons-in-r-using-sf
          parts = st_cast(st_union(clustersPZ),"POLYGON")
          clust = unlist(st_intersects(clustersPZ, parts))
          diss = cbind(clustersPZ, clust) %>%
            group_by(clust) %>%
            summarize(N = n())
          
          # Liste des clusters contenant plus de 10 éléments
          clustersPZ = filter(diss, N > 10)
        }
        
        if(nrow(clustersPZ) > 0)
        {
          # Cartographie des clusters concernés
          
          for(j in 1:nrow(clustersPZ))
          {
            z.boite = st_bbox(clustersPZ[j,])
            z.rectangle = matrix(c(z.boite$xmin, z.boite$ymin, z.boite$xmin, z.boite$ymax, z.boite$xmax,
                                   z.boite$ymax, z.boite$xmax, z.boite$ymin, z.boite$xmin, z.boite$ymin), ncol=2, byrow=T) %>%
              list() %>% st_polygon()
            
            z.diagonale = matrix(c(z.boite$xmin, z.boite$ymin, z.boite$xmax, z.boite$ymax), ncol=2, byrow=T) %>%
              list() %>% st_multilinestring() %>% st_length() / 1000 # la z.diagonale en km
            
            # Il arrive que l'encart zoom soit aussi grand que la carte elle-même (petites entités disséminées,
            # ou ensemble des entités trop petites) => il vaut mieux oublier l'encart de zoom, sans quoi ça
            # va être un peu complexe
            
            if (z.diagonale < diagonale/2) {
              
              z.nivZoom = ""
              z.nivZoom = ifelse(z.diagonale <  5,                     14, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >= 5   & z.diagonale < 10,  13, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >= 10  & z.diagonale < 50,  12, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >= 50  & z.diagonale < 100, 11, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >= 100 & z.diagonale < 500, 10, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >= 500 & z.diagonale < 1000, 9, z.nivZoom)
              z.nivZoom = ifelse(z.diagonale >  1000,                NULL, z.nivZoom)
              
              z.echelle = 0 
              z.echelle = ifelse(z.diagonale <  5,                     .1, z.echelle)
              z.echelle = ifelse(z.diagonale >= 5   & z.diagonale < 10,   1, z.echelle)
              z.echelle = ifelse(z.diagonale >= 10  & z.diagonale < 50,   1, z.echelle)
              z.echelle = ifelse(z.diagonale >= 50  & z.diagonale < 100,  5, z.echelle)
              z.echelle = ifelse(z.diagonale >= 100 & z.diagonale < 500, 10, z.echelle)
              z.echelle = ifelse(z.diagonale >= 500 & z.diagonale < 1000,50, z.echelle)
              z.echelle = ifelse(z.diagonale >  1000,                 100, z.echelle)
              
              # On recharge cartoSubShp pour éviter que ce soit trop simplifié (échelle plus rapprochée)
              cartoSubShp = filter(shp, uid_ENQ == lEnq[i])
              cartoSubShp = st_simplify(cartoSubShp, preserveTopology = T, dTolerance = z.diagonale/100)
              
              perimetreL = st_convex_hull(cartoSubShp) %>% st_buffer(dist = z.diagonale*2000) %>% mutate(t=1) %>% group_by(t) %>% summarize()
              
              perimetre = st_convex_hull(clustersPZ[j,]) %>% st_buffer(dist = z.diagonale*1000) %>%
                mutate(t=1) %>% group_by(t) %>% summarize()
              perimetreP = st_convex_hull(clustersPZ[j,]) %>% st_buffer(dist = z.diagonale*1030) %>%
                st_difference(perimetre) %>% mutate(t=1) %>% group_by(t) %>% summarize()
              
              l.shpTerre   = shpTerre %>% st_intersection(perimetreL) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/100)
              l.shpEau     = shpEau %>% st_intersection(perimetreL) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/100)
              
              l.shpRoutes1 = shpRoutes1 %>% st_intersection(perimetre) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              l.shpRoutes2 = shpRoutes2 %>% st_intersection(perimetre) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              l.shpRoutes3 = shpRoutes3 %>% st_intersection(perimetre) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              l.shpRail    = shpRail    %>% st_intersection(perimetre) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              
              pl.shpRoutes1 = shpRoutes1 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              pl.shpRoutes2 = shpRoutes2 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              pl.shpRoutes3 = shpRoutes3 %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              pl.shpRail    = shpRail    %>% st_intersection(perimetreP) %>% st_simplify(preserveTopology = T, dTolerance=z.diagonale/25)
              
              l.geonames = filter(geonames, feature_class == "P" & population > z.diagonale^1.9) %>%
                st_intersection(st_convex_hull(clustersPZ[j,]))
              if(nrow(l.geonames) == 0)
              {l.geonames = filter(geonames, feature_class == "P" & population > z.diagonale^1.5) %>%
                st_intersection(st_convex_hull(clustersPZ[j,]))}
              
              # On va quand-même limiter à 25 étiquettes pour pas que ce soit la jungle à petite échelle
              if(nrow(l.geonames)>25) {
                l.geonames = l.geonames[order(l.geonames$population, decreasing = T)[1:20],]
              }
              
              plot(z.rectangle, col=NA, border=NA)
              
              if (osm==T) {
                osm = getTiles(cartoSubShp, cachedir=T, crop=F, zoom=z.nivZoom, type="CartoDB.VoyagerNoLabels")
                tilesLayer(osm, add=T)
              } else {
                #plot(z.rectangle, border=NA, col="lightblue1")
                rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue1")
                plot(l.shpTerre$geometry, border = NA, col = "grey95", add=T)
                
              }
              
              if (is.numeric(shp$val) & prop == T) {
                choroLayer(x= cartoSubShp, var="val", legend.title.txt = txt_legende, breaks=distrib, border=NA,
                           colNA  = "white",
                           col= palette, legend.pos="none", legend.values.rnd = 2, add=T)
                plot(cartoSubShp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
              }
              if (!is.numeric(shp$val) & prop == T) {
                nouvlevels = levels(shp$val)[levels(shp$val) %in% unique(cartoSubShp$val)]
                cartoSubShp$val = factor(cartoSubShp$val, nouvlevels)
                
                subpal = palette[levels(shp$val) %in% levels(cartoSubShp$val)]
                
                typoLayer(x= cartoSubShp, var="val", legend.title.txt = txt_legende, border=NA, colNA  = "white",
                          col= subpal, legend.pos="none", legend.values.order = nouvlevels, add=T)
                plot(cartoSubShp$geometry, col=NA, border="white", lwd=.8, lty=3, add=T)
              }
              if(osm==F)
              {
                plot(l.shpEau$geometry, border = NA, col = "lightblue1", add=T)
                if(diagonale*1000 < 100) {plot(l.shpRoutes3$geometry, col = "#F68EA2", lwd=1, add=T)}
                if(diagonale*1000 < 100) {plot(pl.shpRoutes3$geometry, col = "#F68EA2", lwd=1, lty=2, add=T)}
                
                plot(l.shpRoutes2$geometry, col = "#F68EA2", lwd=1.5, add=T)
                plot(pl.shpRoutes2$geometry, col = "#F68EA2", lwd=1.5, lty=2, add=T)
                plot(l.shpRoutes1$geometry, col = "#EC6880", lwd=1.5, add=T)
                plot(pl.shpRoutes1$geometry, col = "#EC6880", lwd=1.5, lty=2, add=T)
                plot(l.shpRail$geometry, col="#b3908a", lwd=1.5, add=T)
                plot(pl.shpRail$geometry, col="#b3908a", lwd=1.5, lty=2, add=T)
                
                if (nrow(l.geonames) > 0){labelLayer(x= l.geonames, txt="name", cex=.7, overlap=F, halo=T)}
                
              }
              
              if (is.numeric(shp$val) & prop == T) {
                legendChoro(pos = "bottomleftextra", title.txt = txt_legende, breaks = distrib,
                            col = palette, values.rnd = 2, 
                            nodata.col = "white", nodata.txt = "Pas de données")
              }
              if (!is.numeric(shp$val) & prop == T) {
                legendTypo(pos = "bottomleftextra", title.txt = txt_legende, 
                           col = subpal, categ = nouvlevels, 
                           nodata.col = "white",nodata.txt = "Pas de données")
              }
              
              
              layoutLayer(title = paste0(lLib[i], " : Zoom sur ", l.geonames[which.max(l.geonames$population),]$name),
                          sources="Maxime Guinepain", 
                          author="Données B.U. du Cerema (2009-2019) / Insee / IGN Route500 et BDTopo / Natural Earth",
                          scale = z.echelle, north = T,
                          frame = T, tabtitle = T, col = "slateblue", coltitle = "white")
            }
          }
        }
      }
    }
    tps    = difftime(Sys.time(), tempsdeb, units = "secs") # estimation du temps restant par proportionnalité
    estimt = ((tps * nEnq / i) - tps) / 60
    
    # code pour afficher une barre de prog' piqué sur internet
    cat(sprintf('\r[%-50s] %d%% .. reste %.1f mn.', paste(rep('=', (i/nEnq*100) / 2), collapse = ''),
                floor((i/nEnq*100)), estimt))
  }
  cat("\n")
  
  dev.off()
  par(.pardefaut)
}


map_vizJour = function(id, verb=F)
{
  rapport("Appel du module VizJour autonome, pour", length(id),
          ifelse(length(id)==1, "entrée", "entrées"))
  
  # On charge les bases nécessaires, si nécessaire
  cat("Chargement des données nécessaires…\n")
  if (! "PER"        %in% ls()) {load("Data/PER.rds")}
  if (! "DEP"        %in% ls()) {load("Data/DEP.rds")}
  if (! "shp_ZF"     %in% ls()) {load("Data/shp_ZF.rds")}
  if (! "shp_PGT"    %in% ls()) {load("Data/shp_PGT.rds")}
  if (! "ACT"        %in% ls()) {load("Data/ACT.rds")}
  if (! "shp_ZT"     %in% ls()) {load("Data/shp_ZT.rds")}
  # if (! "DEP.G"      %in% ls()) {load("Data/DEPG.rds")} plus nécessaire
  
  # Géométrie dans DEP → hors d'usage
  # DEP = left_join(DEP, select(DEP.G, uid_DEP, value), by=c("uid_DEP" = "uid_DEP")) ;
  # remove(DEP.G)
  
  listeEnq = unique(substr(id, 1, 7))
  
  # Fusion des maillages, vers maillage de points unique
  cat("Association des secteurs avec leur géométrie…\n")
  PGT =  filter(PGT, ENQ %in% listeEnq)
  PGT$ZF = paste(PGT$ENQ, PGT$ZF)
  shp_ZF = filter(shp_ZF, uid_ENQ %in% listeEnq) %>%
    st_transform(crs = 3857) %>%
    mutate(NOM = ifelse(is.na(NOM_ZF) | NOM_ZF == "NA", NA, NOM_ZF))
  shp_ZT = filter(shp_ZT, uid_ENQ %in% listeEnq) %>%
    st_transform(crs = 3857) %>% st_buffer(dist = 100) %>% st_simplify(dTolerance=20)
  
  Z1 = select(shp_ZF, CODE_ZF, NOM) %>% rename(ZF = CODE_ZF) %>% st_point_on_surface()
  Z2 = select(PGT, ZF, LIB) %>% rename(NOM = LIB)
  Points = rbind(Z1, Z2)
  PER$PCS_str = ifelse(is.na(PER$PCS42S),
                       as.character(PER$PCS8),
                       as.character(PER$PCS42S))
  
  # Fonds de carte, si nécessaire
  if (is.null(fdCarte)) { fdCarte = map_initCarteParEnquete() }
  
  b = ui_ProgInit(length(id))
  for (i in 1:length(id)) {
    
    # On ne garde que les déplacements et les activités de l'individu
    # On en trace la géométrie on the go
    DEPf = filter(DEP, uid_PER == id[i]) %>%
      left_join(Points, by = c("O_ZF" = "ZF")) %>%
      left_join(Points, by = c("D_ZF" = "ZF"), suffix=c("", ".D")) %>%
      pivot_longer(cols = c("geometry", "geometry.D")) %>%
      st_as_sf() %>%
      group_by(uid_DEP) %>%
      summarize(value = st_union(value, do_union = F),
                uid_PER = first(uid_PER), ModeP = first(ModeP)) %>%
      filter(st_geometry_type(.) == "MULTIPOINT") %>%
      st_cast("LINESTRING")
    
    # Si la séquence est incomplète, abandonner la cartographie
    if (nrow(DEPf) < nrow(filter(DEP, uid_PER == id[i]))) {DEPf = filter(DEPf, F)}
    
    ACTf = filter(ACT, uid_PER == id[i])
    
    if (verb){cat(" ", id[i], ":", nrow(DEPf), "déplacements valides,", nrow(ACTf), "activités")}
    
    if(nrow(DEPf) > 1 & nrow(ACTf) > 1)
    {
      # Réétiquetage des modes pour la légende
      DEPf$ModePe = etqMode(DEPf$ModeP)
      
      # Préparation des géométries ponctuelles et zonales des activités
      Pointsf = filter(Points, ZF %in% ACTf$l)
      Zonesf  = filter(shp_ZF, CODE_ZF %in% ACTf$l)
      
      if(nrow(Pointsf)<1) { break }
      
      # Réétiquetage avec l'heure et le lieu
      ACTf = left_join(ACTf, st_drop_geometry(Pointsf), by=c("l" = "ZF"))
      # ACTf = mutate(ACTf, Lib = etqMotifLieu(substr(Tache,1,2)),
      #                     Lib = paste0(Lib, " (", as.numeric(substr(hDeb,1,2)), "h",
      #                                                        substr(hDeb,3,4), ")"))
      
      # Pondération des points en fonction du temps d'activité
      ACTfsum = group_by(ACTf, l) %>% summarize(du = sum(du)) %>% mutate(du = du/60)
      Pointsf = left_join(Pointsf, ACTfsum, by=c("ZF" = "l")) %>%
        mutate(NOM = ifelse(substr(NOM, 1,2) == "NA", paste0("Secteur ", substr(NOM, 4, nchar(NOM))), NOM))
      
      # Agrégation des points par type d'activité
      pointsLib = ACTf %>% group_by(l, Tache) %>%
        summarize(Lib = paste0(etqMotifLieu(substr(Tache,1,2)), " (", paste(heureHHMM(hDeb), collapse=", "), ")")[1],
                  .groups="keep")
      
      # Initialisation de la carte
      mf_theme(mar = c(.5,.5,3.5,.5),  line=2.8, bg="grey85", tab=F)
      mf_init(x = DEPf, expandBB = rep(.25,4))
      
      # Préparation de la bbox pour couper les cadres illustratifs (= retirer la géométrie qui dépasse)
      rng = par("usr")
      cadre = st_bbox(c(xmin = rng[1], xmax = rng[2], ymax = rng[3], ymin = rng[4]), crs = st_crs(3857))
      rectangle = matrix(c(cadre$xmin, cadre$ymin, cadre$xmin, cadre$ymax, cadre$xmax,
                           cadre$ymax, cadre$xmax, cadre$ymin, cadre$xmin, cadre$ymin), ncol=2, byrow=T) %>%
        list() %>% st_polygon()
      
      # Calcul de la diagonale pour filtrer les toponymes
      diagonale = matrix(c(cadre$xmin, cadre$ymin, cadre$xmax, cadre$ymax), ncol=2, byrow=T) %>%
        list() %>% st_multilinestring() %>% st_length() / 1000 # la diagonale en km
      
      geonamesf = filter(fdCarte$geonames, feature_class == "P" & population > diagonale^2.5) %>%
        st_crop(cadre)
      
      # Traçage de la carte
      plot(rectangle, col="lightblue1", border=NA, add=T)
      plot(st_crop(fdCarte$shpTerre, cadre)$geometry, col = "grey90", border=NA, add=T)
      plot(Zonesf$geometry, col = "lightgrey", border="grey", add=T)
      plot(st_crop(fdCarte$shpEau, cadre)$geometry, border = NA, col = "lightblue1", add=T)
      plot(st_crop(fdCarte$shpRoutes3, cadre)$geometry, col = "#F68EA2", lwd=1, add=T)
      plot(st_crop(fdCarte$shpRoutes2, cadre)$geometry, col = "#F68EA2", lwd=1.5, add=T)
      plot(st_crop(fdCarte$shpRoutes1, cadre)$geometry, col = "#EC6880", lwd=1.5, add=T)
      plot(st_crop(fdCarte$shpRail, cadre)$geometry, col="#b3908a", lwd=1.5, add=T)
      mf_map(x = DEPf, type="typo", var="ModePe", leg_pos = "left",
             pal = as.character(palModes(sort(unique(DEPf$ModePe)))),
             leg_title = "Mode principal", alpha=.5, lwd=5, add=T)
      if (nrow(Pointsf)>1) {
        mf_map(x = Pointsf, type="prop", var="du", col="lightsteelblue", border="steelblue",
               leg_title = "Temps passé (heures)", add=T, leg_frame = T, leg_pos="topright")
      }
      if (nrow(geonamesf)>0) {
        mf_label(x = geonamesf, var = "name", cex=.7)
      }
      mf_label(x = left_join(Pointsf, pointsLib, by=c("ZF" = "l")),
               var = "Lib", cex=.8, halo=T, r=.3, lines=T, overlap=F)
      
      # # Etiquetage des distances (lignes)
      # DEPf_pt = st_centroid(DEPf) %>% mutate(DisStr = paste(round(Dis/1000,1), "km"))
      # mf_label(x = DEPf_pt, var="DisStr", halo=T, r=.05, font=3, cex=.6, lines=T)
      
      # Insert de localisation
      z_Nomenclature = read.csv("Sources/Dictionnaires/Nomenclature.csv", sep=";", encoding="latin1") %>%
        mutate(Libelle_Long = paste0(Libelle_Simple, " (", Annee, ")"))
      polyEnq = filter(shp_ZT, uid_ENQ == substr(id[i], 1,7)) %>% group_by(tout = T) %>% summarize()
      mf_inset_on(x= polyEnq, pos="topleft", cex=.15)
      mf_map(polyEnq, col="white", add=F)
      mf_map(rectangle, col=NA, border="red", lwd=2, add=T)
      pointEnq = st_centroid(polyEnq) %>%
        mutate(LIB = z_Nomenclature[z_Nomenclature$uid_ENQ == substr(id[i],1,7),]$Libelle_Long)
      mf_label(pointEnq, var = "LIB", cex=.5)
      box()
      mf_inset_off()
      
      # Insert d'informations sur l'individu
      liDis = c("Distance totale estimée",    paste(round(PER[PER$uid_PER == id[i],]$Dis/1000,1), "km"))
      liTps = c("Temps passé en déplacement", paste(      PER[PER$uid_PER == id[i],]$Tps, "mn"))
      liNbd = c("Nombre de déplacements",                 PER[PER$uid_PER == id[i],]$N)
      tabPrint = rbind(liDis, liTps, liNbd)
      if(!anyNA(tabPrint)){
        mf_inset_on(fig = c(0, .25, 0.03, .18))
        viz_Table(tabPrint, premLigne = F, premCol = F, text.cex=.6)
        mf_inset_off()
      }
      
      # Insert décrivant l'emploi du temps
      mf_inset_on(fig = c(.25, 1, .03, .12))
      viz_EDT(ACT[ACT$uid_PER == id[i],])
      mf_inset_off()
      
      # Calque informatif
      mf_title(paste0("Journée de l'individu ", id[i], " (",
                      etqPro(PER[PER$uid_PER==id[i],]$Genre,
                             PER[PER$uid_PER==id[i],]$PCS_str,
                             PER[PER$uid_PER==id[i],]$Activ),
                      ", ",  PER[PER$uid_PER==id[i],]$Age, " ans,\n",
                      etqMenage(PER[PER$uid_PER==id[i],]$uid_PER), " ",
                      etqLogement(introLigne = T, uid_MEN = substr(id[i],1,22)), ")"))
      mf_credits(txt = src_fig())
      mf_scale()
      
      ui_Prog(b, i)
    }
    else
    { if(verb){cat("→ erreur de carto")}}
    
  }
}



viz_France = function(base, champAbs = NULL, champRel = NULL,
                      echelleAbs = NULL, echelleRel = NULL, methode = "dep")
{
  # On renomme le champ à représenter
  if (!is.null(champAbs))
  {
    colnames(base)[colnames(base) == champAbs] = "champAbs"
  }
  if (!is.null(champRel))
  {
    colnames(base)[colnames(base) == champRel] = "champRel"
  }
  
  # On récupère la population en 2014 si nécessaire
  # coms_pop = read_delim("Sources/base-cc-evol-struct-pop-2020.CSV", delim = ";", locale = locale(encoding = "Windows-1252"))
  # coms_pop = select(coms_pop, CODGEO, P14_POP)
  # coms = left_join(coms, coms_pop, by = c("insee" = "CODGEO")) |> st_transform(crs = 2154)
  
  if (methode == "dep")
  {
    # On agrège pour faire des départements (simplifiés)
    # Opération très longue, à sauter si possible
    if (file.exists("Data/deps.rds"))
    {
      load("Data/deps.rds")
    } else {
      coms = read_sf("Sources/Mailles/communes-20210101.shp")
      coms$dep = substr(coms$insee, 1, 2)
      coms$dep = ifelse(coms$dep == "97", substr(coms$insee, 1, 3), coms$dep)
      
      deps = coms |> st_simplify(preserveTopology = T, dTolerance = 100) |>
        st_buffer(dist = 100) |>
        group_by(dep) |> summarise() |> st_transform(crs = 3857)
      
      save(deps, file = "Data/deps.rds")
      
      remove(coms)
    }
    
    # On joint à la base fournie
    deps = left_join(deps, base, by = "dep")
    
    base = deps ; remove(deps)
  }
  
  if (methode == "aav")
  {
    aavs = read_sf("Sources/Fond Carte/zMetro.shp")
    aavs = rbind(aavs, read_sf("Sources/Fond Carte/zDOM.shp"))
    aavs = st_transform(aavs, crs = 3857)
    aavs = aavs %>% filter(AAV20 != "000") %>% group_by(AAV20) %>%
      summarise(LIBAAV2 = first(LIBAAV2), DEP = mode(DEP)) %>%
      rename(LIBAAV2020 = LIBAAV2, AAV2020 = AAV20)
    
    base = left_join(aavs, base, by = "AAV2020")
    print(head(base))
    
    # Les outremers sont dans l'océan... j'aime pas... mieux vaut les retirer
    base = filter(base, !DEP %in% c(971:976))
  }
  
  if (!is.null(champAbs))
  {
    # On crée des centroïdes pour le champ absolu (symboles prop)
    base_points = st_centroid(base)
    base_points = filter(base_points, !is.na(rapport))
  }
  
  # On initialise le graphique
  g = ggplot(data = base)
  
  # Un fond continental
  gB = read_sf("Sources/Fond Carte/geoBoundariesCGAZ_ADM0.shp")
  
  # S'il n'y a pas de variable relative, on met un fond gris ; sinon,
  # on met un fond avec un fill sur la variable relative
  if (is.null(champRel))
  {
    g = g +
      geom_sf(data = gB, fill = "seashell", colour = "transparent") +
      geom_sf(fill = "grey95", colour = "grey80")
  }
  else
  {
    if (is.null(champAbs))
    {
      g = g +
        geom_sf(data = gB, fill = "seashell", colour = "transparent") +
        geom_sf(aes(fill = champRel), colour = "grey80")
    } else {
      print(head(base_points))
      g = g +
        geom_sf(data = gB, fill = "seashell", colour = "transparent") +
        geom_sf(fill = "ghostwhite", colour="grey80") +
        geom_sf(data = base_points, aes(colour = champRel, size = champAbs))
    }
  }
  
  # S'il y a une variable absolue, on ajoute des symboles props
  if (!is.null(champAbs) & is.null(champRel))
  {
    g = g + geom_sf(data = base_points, aes(size = champAbs),
                    alpha = .8, fill = "grey30")
  }
  
  # On centre le carton principal sur la France hexa
  g = g + coord_sf(xlim = c(-550000,1100000), ylim = c(5000000,6600000))
  
  # On retire les axes
  g = g + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
  
  
  # Échelles pour les champs
  if (!is.null(echelleAbs))
  {
    g = g + echelleAbs
  }
  if (!is.null(echelleRel))
  {
    g = g + echelleRel
  }

  if (methode == "dep")
  {
    # On crée des encarts sur les 3 outremers
    # → Bbox de la Guyane
    bbox973 = st_bbox(filter(base, dep == "973"))
    
    # → Centre des bboxes de Martinique et Réunion
    ctr972 = st_centroid(filter(base, dep == "972")) |> st_bbox()
    ctr974 = st_centroid(filter(base, dep == "974")) |> st_bbox()
    
    # → Intervalles entre le centre bbox Guyane et ses bords
    xinterv = as.double(bbox973$xmax - bbox973$xmin)
    yinterv = as.double(bbox973$ymax - bbox973$ymin)
    
    # → Calcul de bboxes de même échelle pour Martinique et Réunion
    pts972 = data.frame(
      lon = c(ctr972$xmax - (xinterv / 2), ctr972$xmax + (xinterv / 2), ctr972$xmax + (xinterv / 2), ctr972$xmax - (xinterv / 2)) |> as.double(),
      lat = c(ctr972$ymax + (yinterv / 2), ctr972$ymax + (yinterv / 2), ctr972$ymax - (yinterv / 2), ctr972$ymax - (yinterv / 2)) |> as.double()
    )
    box972 = st_as_sf(pts972, coords = c("lon", "lat"), crs = st_crs(bbox973)) |>
      summarise(geometry = st_combine(geometry)) |> st_cast("POLYGON") 
    bbox972 = st_bbox(box972)
    
    pts974 = data.frame(
      lon = c(ctr974$xmax - (xinterv / 2), ctr974$xmax + (xinterv / 2), ctr974$xmax + (xinterv / 2), ctr974$xmax - (xinterv / 2)) |> as.double(),
      lat = c(ctr974$ymax + (yinterv / 2), ctr974$ymax + (yinterv / 2), ctr974$ymax - (yinterv / 2), ctr974$ymax - (yinterv / 2)) |> as.double()
    )
    box974 = st_as_sf(pts974, coords = c("lon", "lat"), crs = st_crs(bbox973)) |>
      summarise(geometry = st_combine(geometry)) |> st_cast("POLYGON")
    bbox974 = st_bbox(box974)
    
    # → Encarts pour les 3 outremers
    g972 = g + coord_sf(xlim = c(bbox972$xmin, bbox972$xmax), ylim = c(bbox972$ymin, bbox972$ymax))
    g973 = g + coord_sf(xlim = c(bbox973$xmin, bbox973$xmax), ylim = c(bbox973$ymin, bbox973$ymax))
    g974 = g + coord_sf(xlim = c(bbox974$xmin, bbox974$xmax), ylim = c(bbox974$ymin, bbox974$ymax))
    
    # → Pas d'axes pour ces encarts
    g972 = g972 + theme(axis.text = element_blank(), axis.ticks = element_blank())
    g973 = g973 + theme(axis.text = element_blank(), axis.ticks = element_blank())
    g974 = g974 + theme(axis.text = element_blank(), axis.ticks = element_blank())
    
    # → Création d'un carton de côté
    gOM = cowplot::plot_grid(g972 + labs(subtitle = "Martinique") +
                               theme(line = element_blank(),
                                     legend.position = "none", plot.subtitle = element_text(hjust=.5)),
                             g973 + labs(subtitle = "Guyane") +
                               theme(line = element_blank(), 
                                     legend.position = "none", plot.subtitle = element_text(hjust=.5)),
                             g974 + labs(subtitle = "Réunion") +
                               theme(line = element_blank(),
                                     legend.position = "none", plot.subtitle = element_text(hjust=.5)),
                             nrow = 3, rel_heights=c(1,1,1))
  }
  
  # Échelle
  g = g + ggspatial::annotation_scale(location="bl", bar_cols = c("gray60", "gray95"),
                                      line_col = "gray70", height = unit(.1, "cm"))
  
  if (methode == "aav")
  {
    g = cartoLib(g, etendue = aavs, proj = st_crs(base))
  }
  
  if (methode == "dep")
  {
    # On colle l'encart outremers et l'encart hexa
    gg = cowplot::plot_grid(gOM, g, nrow = 1, rel_widths = c(1,5))
  } else {
    gg = g
  }

  siEmp = methode == "dep"
  
  # Pied de page
  gg = viz_Pied(gg, src_fig(bu = T, emp = siEmp, carto = T, gB= T, date = moisEnCours()))
  
  return(gg)
}

# Divers ====

categZTselonDEP = function(DEP, PER)
{
    # On récup les coeffs
    DEPs = DEP %>% left_join(select(PER, uid_PER, CoeffEnq), by = c("uid_PER" = "uid_PER")) %>%
        mutate(D_ZT = paste0(uid_ENQ, ":", D_ZT), O_ZT = paste0(uid_ENQ, ":", O_ZT))
    
    paires = as.data.frame("") ; cat("nombre de paires restantes : ...")
    while(nrow(paires) == nrow(DEPparZTmax))
    {
    DEPparZTmax = DEPs %>%
        group_by(O_ZT, D_ZT) %>%
        summarize(n = sum(CoeffEnq), .groups="drop_last") %>%
        filter(O_ZT != D_ZT) %>%
        summarize(D_ZT = D_ZT[which.max(n)])
    
    DEPparZTmax = left_join(DEPparZTmax, DEPparZTmax, by = c("D_ZT" = "O_ZT"), suffix = c("", ".mir"))
    paires = filter(DEPparZTmax, O_ZT == D_ZT.mir)
    cat("\rnombre de paires restantes :", nrow(paires) - nrow(DEPparZTmax))
    paires$index = rep(c(0,1), times=nrow(paires)/2)
    paires = paires %>% mutate(rplc = ifelse(index == 0, paste0(O_ZT, "+", D_ZT),
                                                         paste0(D_ZT, "+", O_ZT))) %>%
        rename(sch = O_ZT) %>%
        select(sch, rplc)
    
    DEPs = left_join(DEPs, paires, by=c("O_ZT" = "sch")) %>% mutate(O_ZT = rplc) %>% select(-rplc) %>%
           left_join      (paires, by=c("D_ZT" = "sch")) %>% mutate(D_ZT = rplc) %>% select(-rplc)
    }
}

centroidesAAV = function()
{
    AAV = read.csv("Sources/Mailles/Zonage Attraction.csv", sep=";", encoding="latin1")
    
    rapport("Création des centroïdes des AAV", info = T)
    # Etape 1 : création des centroïdes
    AAV_Centres = filter(AAV, CATEAAV2020 == "11") %>%
        left_join(mutate(shp_COM, insee = ifelse(insee == "75101", "75056", insee)),
                  by=c("CODGEO" = "insee")) %>%
        st_as_sf() %>%
        st_centroid() %>%
        select(AAV2020, geometry) %>%
        st_transform(crs = 2154)
    
    rapport("Attribution des centroïdes aux AAV", info = T)
    # Etape 2 : attribution d'un centroïde selon la commune
    ZFs = PER %>%
        group_by(ZF) %>% summarise(Com = mode(Com)) %>%
        mutate(Com = ifelse(substr(Com, 1, 2) == "75", "75056", Com)) %>% # pour Paris
        left_join(AAV, by=c("Com" = "CODGEO")) %>%
        left_join(AAV_Centres, by="AAV2020") %>%
        left_join(st_centroid(select(shp_ZF, CODE_ZF)), by=c("ZF" = "CODE_ZF"), suffix=c(".centre",".ego")) %>%
        filter(!st_is_empty(geometry.ego))
    
    rapport("Calcul des distances AAV-ZFs")
    # Etape 3 : calcul de la distance
    ZFs = mutate(ZFs, dis = st_distance(x = geometry.centre, y = geometry.ego, by_element = T)) %>%
        mutate(dis = as.double(dis))
    
    return(ZFs)
}


# Quartier moyen, carroyage avancé =================================================================

initGrille = function(shp_ZF, filtreEnq = NULL, tailleMaille = 2000)
{
    rapport("Création d'un carroyage orthogonal de", tailleMaille/1000, "km de côté")
    
    grille = filter(shp_ZF, uid_ENQ %in% filtreEnq) %>%
        st_transform(crs = 2154) %>%
        st_make_grid(cellsize = c(tailleMaille,tailleMaille), square = F) %>%
        st_transform(crs = 3857)
    
    baseGrille = matrix(ncol=1, nrow=length(grille), data=c(1:length(grille))) %>%
        as_tibble() %>%
        cbind(grille) %>%
        st_as_sf() %>%
        rename(idCar = V1)
    
    return(baseGrille)
}


transfoCarroyage = function(base, shp_ZF, shp_PGT = NULL, baseGrille, zonesHab, filtreEnq = NULL,
                            tailleMaille = 2000, bloquerCoeffEnq = F)
{
    rapport("Appel de la fonction transfo-carroyage.")
    
    # Vérifications
    if (!"ZF" %in% colnames(base))
    { stop("La fonction transfoCarroyage recquiert une variable ZF") }
    if (!"uid_PER" %in% colnames(base))
    { stop("La fonction transfoCarroyage recquiert une variable uid_PER") }
    if ("sf" %in% class(base))
    { base = st_drop_geometry(base) }
    if (is.null(filtreEnq))
    { filtreEnq = unique(base$uid_ENQ) }
    
    nrowInit = nrow(base)
    
    rapport("Préparation de la répartition sur zones bâties", info=T)
    
    zonesHab = st_transform(zonesHab, crs=3857)
    ZF = shp_ZF %>% st_transform(crs = 3857) %>% reduireMaillage(uid1 = "CODE_ZF", dest=zonesHabitées)
    
    if (bloquerCoeffEnq & "CoeffEnq" %in% colnames(base))
    { base$CoeffEnq = as.character(base$CoeffEnq) }
    
    rapport("Réattribution et répartition de la base dans le nouveau maillage", info=T)
    listeVarNum = colnames(base)[unlist(lapply(base, is.numeric))]
    rapport("Variables recalculées :", paste(listeVarNum, collapse = ", "), info = T)
    
    baseZO = base %>%
        filter(substr(uid_PER, 1,7) %in% filtreEnq) %>%
        left_join(ZF, by=c("ZF" = "CODE_ZF")) %>%
        filter(!st_is_empty(geometry)) %>%
        st_as_sf() %>%
        changerMaillage(dest = baseGrille, uid1 = "uid_PER", uid2 = "idCar") %>%
        st_drop_geometry() %>%
        mutate(across(where(is.numeric), ~as.numeric(.)))
    
    basePT = base %>%
        filter(substr(uid_PER, 1,7) %in% filtreEnq) %>%
        left_join(shp_PGT, by=c("ZF" = "CODE_ZF")) %>%
        filter(!st_is_empty(geometry)) %>%
        st_as_sf() %>%
        st_intersection(baseGrille) %>%
        st_drop_geometry()
    
    base = rbind(baseZO, basePT)
    
    if (bloquerCoeffEnq & "CoeffEnq" %in% colnames(base))
    { base$CoeffEnq = as.numeric(base$CoeffEnq) }
    
    rapport(round(nrow(base)/nrowInit * 100, 2), "% des lignes n'ont pu être traitées.", info = T)
    
    save(baseGrille, "Data/grilleTemp.rds")
    rapport("Nouveau maillage sauvegardé dans grilleTemp.rds, l'uid est idCar", info = T)
    
    return(base)
}

spc_AireMoyenne = function(MEN, PER)
{
    rapport("Calcul d'un indice de dissemblance à partir de données carroyées")
    
    if ("sf" %in% class(MEN))
    {
        MEN = st_drop_geometry(MEN)
    }
    
    
    
    
    
    zonesHabitées = read_sf("Sources/CLC/CLC00_RPDL_RGF.shp") %>%
        st_transform(crs = 3857) %>%
        filter(CODE_00 %in% c("111", "112", "121", "123", "124", "131"))
    
    ZF = shp_ZF %>% st_transform(crs = 3857) %>%
        reduireMaillage(uid1 = "CODE_ZF", dest=zonesHabitées)
    
    grille = filter(shp_ZF, uid_ENQ == "LOI2015") %>%
        st_transform(crs = 2154) %>%
        st_make_grid(cellsize = c(2000,2000), square = F) %>%
        st_transform(crs = 3857)
    
    baseGrille = matrix(ncol=1, nrow=length(grille), data=c(1:length(grille))) %>%
        as_tibble() %>%
        cbind(grille) %>%
        st_as_sf()
    
    load("Data/shp_PGT.rds")
    PGT = mutate(PGT, CODE_ZF = paste(ENQ, CODE_ZF))
    
    ACT.44.CAR = ACT2 %>%
        filter(substr(uid_PER, 1,7) == "LOI2015") %>%
        left_join(select(PER.PCS, -CoeffEnq), by=c("uid_PER" = "uid_PER")) %>%
        left_join(ZF, by=c("l" = "CODE_ZF")) %>%
        filter(!st_is_empty(geometry)) %>%
        filter(PCS8 %in% c("03", "04", "05", "06")) %>%
        mutate(du = du * CoeffEnq) %>%
        mutate(pcs_Tot = du) %>%
        pivot_wider(names_from = PCS8, names_prefix = "pcs_", values_from = du) %>%
        st_as_sf() %>%
        changerMaillage(dest = baseGrille, uid1 = "uid_PER", uid2 = "V1") %>%
        st_centroid() %>%
        select(uid_PER, pcs_Tot:pcs_06, V1, geometry)
    
    ACT.44.PGT = ACT2 %>%
        filter(substr(uid_PER, 1,7) == "LOI2015") %>%
        left_join(select(PER.PCS, -CoeffEnq), by=c("uid_PER" = "uid_PER")) %>%
        left_join(PGT, by=c("l" = "CODE_ZF")) %>%
        filter(!st_is_empty(geometry)) %>%
        filter(PCS8 %in% c("03", "04", "05", "06")) %>%
        mutate(du = du * CoeffEnq) %>%
        mutate(pcs_Tot = du) %>%
        pivot_wider(names_from = PCS8, names_prefix = "pcs_", values_from = du) %>%
        st_as_sf() %>%
        st_intersection(baseGrille) %>%
        select(uid_PER, pcs_Tot:pcs_06, V1, geometry)
    
    ACT.44.CAR = rbind(ACT.44.CAR, ACT.44.PGT) %>%
        rename(idCar = V1)
    
    ACT.44.CAR = ACT.44.CAR %>%
        group_by(idCar) %>%
        summarize(n = length(unique(uid_PER)), across(starts_with("pcs_"), sum, na.rm=T)) %>%
        select(idCar, n, pcs_03, pcs_04, pcs_05, pcs_06, pcs_Tot)
    
    ACT.44.CAR.COMP = group_by(ACT.44.CAR, tout = T) %>%
        summarize(across(starts_with("pcs_"), sum, na.rm=T)) %>%
        rename(tot = pcs_Tot) %>%
        mutate(across(starts_with("pcs_"), ~./tot)) %>%
        st_drop_geometry()
    
    ACT.44.CAR = ACT.44.CAR %>%
        rename(tot = pcs_Tot) %>%
        mutate(across(starts_with("pcs_"), ~./tot)) %>%
        mutate(tout = T) %>%
        left_join(ACT.44.CAR.COMP, by=c("tout" = "tout"), suffix = c("", ".tot")) %>%
        select(-tout) %>%
        mutate(across(where(is.numeric), as.double)) %>%
        mutate(indice = (abs(pcs_03/pcs_03.tot - 1) +
                             abs(pcs_04/pcs_04.tot - 1) +
                             abs(pcs_05/pcs_05.tot - 1) +
                             abs(pcs_06/pcs_06.tot - 1)) / 4) %>%
        filter(n>20)
    
    baseGrille = baseGrille %>% rename(idCar = V1) %>%
        left_join(st_drop_geometry(ACT.44.CAR), by=c("idCar" = "idCar")) %>%
        mutate(uid_ENQ = "LOI2015")
    
    brks = testBrks(baseGrille$indice, n=5)
    
    brks[[2]]
    
    rapport("Carte de l'indice de dissimilarité au lieu d'emploi", info=T)
    
    map_CarteParEnquete(
        shp = filter(select(baseGrille, uid_ENQ, indice, geometry), !is.na(indice)),
        pdf = "Sorties/5- TERRITOIRES/Indice Dissim Carroyage.pdf", paysage = T,
        titre = "Écart par rapport au profil moyen d'emploi par maillage arbitraire en Loire Atlantique",
        legende = "Écart par rapport au\nprofil moyen d'emploi\n(maillage arbitraire)\nen % moyennés",
        palette = c("darkolivegreen1", "gold", "orange", "darkorange2", "brown2"),
        brks = brks[[1]]
    )
    
    # On ne voit pas grand chose sur Nantes, il faudrait zoomer ?
    # Rayon de 15 km autour de Nantes.
    
    ZF.Nantes = ZF.44 %>% group_by(NOM_COM) %>% summarize() %>% filter(NOM_COM == "Nantes") %>%
        st_centroid() %>% st_transform(crs = 2154) %>% st_buffer(dist = 15000) %>% st_transform(crs = 3857)
    
    baseGrille.Nantes = baseGrille[which(st_intersects(baseGrille, ZF.Nantes, sparse=F)[,1]),]
    
    map_CarteParEnquete(
        shp = filter(select(baseGrille.Nantes, uid_ENQ, indice, geometry), !is.na(indice)),
        pdf = "Sorties/5- TERRITOIRES/Indice Dissim Carroyage (Nantes).pdf", paysage = T,
        titre = "Écart par rapport au profil moyen d'emploi par maillage arbitraire à Nantes",
        legende = "Écart par rapport au\nprofil moyen d'emploi\n(maillage arbitraire)\nen % moyennés",
        palette = c("darkolivegreen1", "gold", "orange", "darkorange2", "brown2"),
        brks = brks[[1]]
    )
}



# Carroyage automatisé =============================================================================

# La fonction suivante doit permettre de re-répartir n'importe quelle base en carreaux plutôt que
# selon le maillage traditionnel des EMDs.
# Elle identifie bien le nombre d'enquêté⋅es dans chaque carreau (à partir des uid_PER) pour
# éliminer les mailles trop petites pour être pertinentes.
# Lorsque l'enquêté⋅e se trouve dans une maille qui traverse plusieurs carreaux, son coeff_enq est
# réparti entre les zones bâties des différents carreaux.



# Chargement initial

# load("Data/ACT2.rds")
# load("Data/shp_ZT.rds")
# load("Data/shp_PGT.rds")
# PGT = mutate(PGT, CODE_ZF = paste(ENQ, CODE_ZF))
# 
# zonesHabitées = read_sf("Sources/CLC/CLC00_RPDL_RGF.shp") %>%
#     st_transform(crs = 3857) %>%
#     filter(CODE_00 %in% c("111", "112", "121", "123", "124", "131"))
# 
# baseGrille = initGrille(shp_ZF, "LOI2015", tailleMaille = 2000)
# 
# ACT2.CAR = ACT2 %>% rename(ZF = l) %>% mutate(idAct = as.character(idAct)) %>%
#     mutate(du = as.character(du)) %>%
#     transfoCarroyage(shp_ZF = shp_ZF, shp_PGT = PGT, zonesHab = zonesHabitées,
#                      baseGrille = baseGrille,
#                      filtreEnq = "LOI2015") %>%
#     mutate(du = as.numeric(du)) # on répartit les poids, on garde les durées (calculs moyens)


# Fonction Potentiel modifiée (pour faire des moyennes)

#' @title Compute the Potential Model using Parallelization with Weighted Means
#' @description This function computes the potential model with a cutoff 
#' distance and parallel
#' computation for weighted means.
#' @param x an sf object (POINT), the set of known observations to estimate
#' the potentials from.
#' @param y an sf object (POINT), the set of unknown units for which the
#' function computes the estimates.
#' @param var names of the variables in \code{x} from which potentials are
#' computed. Quantitative variables with no negative values.
#' @param fun spatial interaction function. Options are "p"
#' (pareto, power law) or "e" (exponential).
#' For pareto the interaction is defined as: (1 + alpha * mDistance) ^ (-beta).
#' For "exponential" the interaction is defined as:
#' exp(- alpha * mDistance ^ beta).
#' The alpha parameter is computed from parameters given by the user
#' (\code{beta} and \code{span}).
#' @param span distance where the density of probability of the spatial
#' interaction function equals 0.5.
#' @param beta impedance factor for the spatial interaction function.
#' @param limit maximum distance used to retrieve \code{x} points, in map units.
#' @param ncl number of clusters. \code{ncl} is set to
#' \code{parallel::detectCores() - 1} by default.
#' @param size \code{mcpotential} splits \code{y} in smaller chunks and
#' dispatches the computation in \code{ncl} cores, \code{size} indicates the
#' size of each chunks.
#' @return If only one variable is computed a vector is returned, if more than
#' one variable is computed a matrix is returned.
#' @export
#' @importFrom sf st_buffer st_centroid st_geometry st_intersects

mcpotential_wm <- function(x, y, var, w = NULL, fun,
                           span, beta,
                           limit = 3 * span,
                           ncl, size = 500) {
    
   # test_point(x, "x")
   # test_point(y, "y")  
    
    # si aucun poids, la colonne w = 1
    if (!is.null(w)) { x$w = 1 ; w = "w" }
    
    # launch multiple cores
    if (missing(ncl)) {
        ncl <- parallel::detectCores(all.tests = FALSE, logical = FALSE) - 1
        if (is.na(ncl)) {
            ncl <- 1
        }
        if (ncl == 0) {
            ncl <- 1
        }
    }
    ##
    cl <- parallel::makeCluster(ncl, setup_strategy = "sequential")
    doParallel::registerDoParallel(cl)
    
    # data simplification
    xsfc <- st_geometry(x)
    kgeom <- matrix(unlist(xsfc), ncol = 2, byrow = TRUE)
    
    v <- as.matrix(x = x[, var, drop = TRUE])
    weights <- as.matrix(x = x[, w, drop = TRUE])
    
    ysfc <- st_geometry(y)
    
    # sequence to split unknown pts
    ny <- nrow(y)
    sequence <- unique(c(seq(1, ny, size), ny + 1))
    lseq <- length(sequence) - 1
    
    # split unknown pts and put it on a list
    ml <- list()
    for  (i in 1:lseq) {
        ml[[i]] <- ysfc[(sequence[i]):(sequence[i + 1] - 1)]
    }
    
    # dispatch
    pot <- foreach::`%dopar%`(
        foreach::foreach(
            ysfc = ml,
            .packages = "sf",
            .combine = c,
            .inorder = FALSE
        ),
        {
            # FUNS
            eucledian_simple <- function(from, to) {
                sqrt((from[1] - to[1])^2 + (from[2] - to[2])^2)
            }
            if (fun == "e") {
                alpha <- log(2) / span^beta
                fric <- function(alpha, matdist, beta) {
                    exp(-alpha * matdist^beta)
                }
            }
            if (fun == "p") {
                alpha <- (2^(1 / beta) - 1) / span
                fric <- function(alpha, matdist, beta) {
                    (1 + alpha * matdist)^(-beta)
                }
            }
            
            # Buffer limit
            gbuf <- st_buffer(ysfc, limit)
            inter <- st_intersects(gbuf, xsfc, prepared = TRUE)
            
            # data transformation
            ugeom <- matrix(unlist(ysfc), ncol = 2, byrow = TRUE)
            
            # go through each y
            # ici, j'ai remplacé `sum` par weighted means
            # au lieu d'ajouter les stocks multipliés par leur potentiel, je fais une moyenne
            # des valeurs pondérées par le résultat de la fonction : plus un point est éloigné,
            # moins il a d'incidence sur la moyenne mesurée, en fonction de la fonction choisie
            # Difficulté = prendre en compte aussi la pondération de l'enquête, que j'ajoute
            # dans une seconde matrice
            l <- vector("list", nrow(ugeom))
            for (i in seq_along(l)) {
                kindex <- unlist(inter[i])
                kn <- kgeom[kindex, , drop = FALSE]
                un <- ugeom[i, ]
                matdist <- apply(kn, 1, eucledian_simple, un)
                un <- apply(
                    X = v[kindex, , drop = FALSE],
                    MARGIN = 2,
                    FUN = function(x, w) {
                        matrixStats::weightedMean(x, w * fric(alpha, matdist, beta), na.rm = TRUE)
                    },
                    w = weights[kindex, , drop = FALSE]
                )
                l[[i]] <- un
            }
            unlist(l)
        }
    )
    # stop parralel
    parallel::stopCluster(cl)
    if (length(var) == 1) {
        pot <- as.numeric(pot)
    } else {
        pot <- matrix(pot,
                      ncol = length(var), byrow = TRUE,
                      dimnames = list(NULL, var)
        )
    }
    
    return(pot)
}


# Cartes de potentiel =====

library(potential)

# Idée : appliquer à chaque point une formule qui
# - en entrée : prend la localisation du point et un argument, le range
# - en sortie : donne la part des points situés dans l'intervalle à tester
intervallePointsRangeDis = function(x, range, logCible = .1, seuil = seuilSignifiant)
{
    buffers = st_buffer(x, dist = range) %>% select(uid_PER, Dis)
    intersec = st_intersection(rename(select(x, Dis, geometry, uid_PER),
                                      DisY = Dis, uid_PER.x = uid_PER), buffers) %>%
        filter(uid_PER.x != uid_PER) %>%
        mutate(dansIntervalle = ifelse(log(DisY) >= log(Dis) - logCible &
                                           log(DisY) <= log(Dis) + logCible,
                                       1, 0)) %>%
        group_by(uid_PER) %>%
        summarise(pDansIntervalle = ifelse(n() > seuil, sum(dansIntervalle, na.rm=T) / n(), NA))
    
    # Au cas où il y aurait des na's
    x = left_join(x, st_drop_geometry(intersec), by = "uid_PER")
    return(x$pDansIntervalle)
}

intervallePointsRangeDis2 = function(x, range, logCible = .1, seuil = seuilSignifiant)
{
    buffers = st_buffer(x, dist = range) %>% select(uid_PER, Dis)
    intersec = st_intersection(rename(select(x, Dis, geometry, uid_PER),
                                      DisY = Dis, uid_PER.x = uid_PER), buffers) %>%
        filter(uid_PER.x != uid_PER) %>%
        mutate(diffLog = abs(log(Dis) - log(DisY))) %>%
        st_drop_geometry() %>%
        group_by(uid_PER) %>%
        summarise(n = n(), diffLogAvg = ifelse(n() > seuil, mean(diffLog, na.rm=T), NA))
    
    x = left_join(x, intersec, by = "uid_PER")
    return(x$diffLogAvg)
}

intervallePointsRangeDis3 = function(x, range, logCible = .1, seuil = seuilSignifiant)
{
    buffers = st_buffer(x, dist = range) %>% select(uid_PER, Dis)
    intersec = st_intersection(rename(select(x, Dis, geometry, uid_PER),
                                      DisY = Dis, uid_PER.x = uid_PER), buffers) %>%
        filter(uid_PER.x != uid_PER) %>%
        mutate(diffLog = abs(log(Dis) - log(DisY))) %>%
        st_drop_geometry() %>%
        group_by(uid_PER) %>%
        summarise(n = n(), diffLogAvg = ifelse(n() > seuil,
                                               as.double(quantile(diffLog, probs=.75, na.rm=T)) -
                                                   as.double(quantile(diffLog, probs=.25, na.rm=T)), NA))
    
    x = left_join(x, intersec, by = "uid_PER")
    return(x$diffLogAvg)
}

intervallePointsRangeDis3 = function(x, range, seuil = seuilSignifiant)
{
    buffers = st_buffer(x, dist = range) %>% select(uid_PER, Dis)
    intersec = st_intersection(rename(select(x, Dis, geometry, uid_PER),
                                      DisY = Dis, uid_PER.x = uid_PER), buffers) %>%
        filter(uid_PER.x != uid_PER) %>%
        mutate(diffLog = abs(log(Dis) - log(DisY))) %>%
        st_drop_geometry() %>%
        group_by(uid_PER) %>%
        summarise(n = n(), diffLogAvg = ifelse(n() > seuil, median(diffLog, na.rm=T), NA))
    
    x = left_join(x, intersec, by = "uid_PER")
    return(x$diffLogAvg)
}

intervallePointsRangeTps = function(x, range, logCible = .1)
{
    buffers = st_buffer(x, dist = range) %>% select(uid_PER, Tps)
    intersec = st_intersection(rename(select(x, Tps, geometry), TpsY = Tps), buffers) %>%
        mutate(dansIntervalle = ifelse(log(TpsY) >= log(Tps) - logCible &
                                           log(TpsY) <= log(Tps) + logCible,
                                       1, 0)) %>%
        group_by(uid_PER) %>%
        summarise(pDansIntervalle = sum(dansIntervalle) / n())
    
    # Au cas où il y aurait des na's
    x = left_join(x, st_drop_geometry(intersec), by = "uid_PER")
    return(x$pDansIntervalle)
}

cartePotentiel = function(PER, shp_ZT, enq, proj = 2154,
                          var, w, fun, span, beta, limit,
                          res_grille = 1000, brks= NA, facteurDiv=1,
                          palette = "TealGrn",
                          plotSemis = F, plotSemisNom = "Semis.svg",
                          plotFonction = F, plotFonctionNom = "Fonction.svg",
                          titre, leg, credits = src_fig(),
                          carte = fdCarte, rail = T, route1 = T, route2 = T)
{
    sortie = ifelse(length(enq) == 1, enq, paste0(length(enq), " enquêtes"))
    rapport("Carte de potentiel :", sortie)
    
    colnames(PER)[colnames(PER) == var] = "var"
    
    # Sous-calque PER
    PER_pot = PER %>%
        filter(uid_ENQ %in% enq) %>%
        filter(typoJo == "TRAV") %>%
        filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
        init_geomMen(shp_ZF, shp_COM) %>%
        st_transform(crs = proj) %>%
        mutate(var = var/facteurDiv)
    
    # Etendue(s) enquête(s)
    etendue = shp_ZT %>%
        filter(uid_ENQ %in% enq) %>%
        st_buffer(dist = 100) %>%
        group_by(uid_ENQ) %>% summarise() %>%
        st_transform(crs = proj)
    
    # Grille
    grille = create_grid(x = etendue, res = res_grille)
    
    # Contrôle du semis
    if (plotSemis)
    {
        ggplot(PER_pot) + geom_sf(data = etendue, fill = "lightblue") + geom_sf() +
            geom_sf(data=grille, color = "midnightblue", shape = 4, size = 1, alpha=.5) + theme_bw() 
    }
    
    # Filtrage nécessaire
    # Sans les poids, car on veut aussi virer le bruit amplifié par eux
    seuil = quantile(PER_pot$var, probs=.99, na.rm=T)
    PER_pot = PER_pot %>% filter(var < seuil)
    
    rapport("Calcul du potentiel", info=T)
    # Potentiel
    grille$potentiel = mcpotential_wm(x = PER_pot, y = grille,
                                      var = "var", w = w,
                                      fun = fun, span = span, beta = beta, limit = limit,
                                      ncl = 2)
    
    print(summary(grille$potentiel))
    
    # Discrétisation
    if (is.na(brks))
    {
        bks <- seq(min(grille$potentiel, na.rm=T), max(grille$potentiel, na.rm=T), length.out=11)
    } else {bks = brks}
    
    # Calcul des isozones
    grille_eq = equipotential(grille, var = "potentiel", breaks = bks, mask = etendue)
    
    rapport("Traçage de la carte", info=T)
    # Carte
    mf_map(x = grille_eq, var = "min", type = "choro",
           breaks = bks,
           pal = hcl.colors(10, palette, rev = T),
           border = "grey70",
           leg_val_rnd=1,
           leg_pos = "left",
           lwd = .2,
           leg_title = leg)
    
    # Calques lignes
    if (rail)
    {
        ra = carte$shpRail %>%
            st_transform(crs = proj) %>%
            st_intersection(etendue) %>%
            st_simplify(dTolerance = 10000)
        mf_map(x = ra, col = "rosybrown", lty=2, add=T)
    }
    if (route2)
    {
        r2 = carte$shpRoutes2 %>%
            st_transform(crs = proj) %>%
            st_intersection(etendue) %>%
            st_simplify(dTolerance = 10000)
        mf_map(x = r2, col = "grey60", add=T)
    }
    if (route1)
    {
        r1 = carte$shpRoutes1 %>%
            st_transform(crs = proj) %>%
            st_intersection(etendue) %>%
            st_simplify(dTolerance = 10000)
        mf_map(x = r1, col = "grey40", add=T)
    }
    
    mf_layout(title = titre,
              credits = credits)
}


ggCartePotentiel = function(PER, enq, proj = 2154,
                            var, w, fun, span, beta, limit,
                            res_grille = 1000, brks= NA, facteurDiv=1,
                            coulBas, coulHaut,
                            plotSemis = F, plotSemisNom = "Semis.svg",
                            plotFonction = F, plotFonctionNom = "Fonction.svg",
                            titre, leg, credits = NULL,
                            carte = fdCarte, axes = T, detailNoms = 5,
                            forcerDecoupage = F)
{
    if(!"shp_ZT" %in% ls())
    {
        load("Data/shp_ZT.rds")
    }
    
    if(!"shp_ZF" %in% ls())
    {
        load("Data/shp_ZF.rds")
    }
    
    if(!"shp_COM" %in% ls())
    {
        load("Data/shp_COM.rds")
    }
    
    sortie = ifelse(length(enq) == 1, enq, paste0(length(enq), " enquêtes"))
    rapport("Carte de potentiel :", sortie)
    
    fonction = paste0(ifelse(fun == "e", "fonction exponentielle ", ""),
                      ifelse(fun == "p", "fonction pareto ", ""),
                      "(span = ", span, ", beta = ", beta, ")")
    
    colnames(PER)[colnames(PER) == var] = "var"
    
    # Sous-calque PER
    PER_pot = PER %>%
        filter(uid_ENQ %in% enq) %>%
        filter(typoJo == "TRAV") %>%
        filter(PCS8 %in% c("01", "02", "03", "04", "05", "06")) %>%
        init_geomMen(shp_ZF, shp_COM, centroides = T) %>%
        st_transform(crs = proj) %>%
        mutate(var = var/facteurDiv)
    
    if (is.null(credits)) { credits = src_fig(PER_pot) }
    
    # Etendue(s) enquête(s)
    etendue = cartoEtendue(shp_ZT, enq, proj)

    if (forcerDecoupage)
    { PER_pot = st_intersection(PER_pot, select(etendue, geometry)) }
    
    # Grille
    grille = create_grid(x = etendue, res = res_grille)
    
    # Contrôle du semis
    if (plotSemis)
    {
        ggplot(PER_pot) + geom_sf(data = etendue, fill = "lightblue") + geom_sf() +
            geom_sf(data=grille, color = "midnightblue", shape = 4, size = 1, alpha=.5) + theme_bw() 
    }
    
    # Filtrage nécessaire
    # Sans les poids, car on veut aussi virer le bruit amplifié par eux
    # seuil = quantile(PER_pot$var, probs=.99, na.rm=T)
    # PER_pot = PER_pot %>% filter(var < seuil)
    # → n'est plus nécessaire depuis que je cartographie la médiane
    
    rapport("Calcul du potentiel", info=T)
    # Potentiel
    grille$potentiel = mcpotential_wm(x = PER_pot, y = grille,
                                      var = "var", w = w,
                                      fun = fun, span = span, beta = beta, limit = limit,
                                      ncl = 2)
    
    print(summary(grille$potentiel))
    
    # Discrétisation
    if (is.na(brks))
    {
        bks <- seq(min(grille$potentiel, na.rm=T), max(grille$potentiel, na.rm=T), length.out=11)
        brks = bks
    } else {bks = brks}
    
    # Calcul des isozones
    grille_eq = equipotential(grille, var = "potentiel", breaks = bks, mask = etendue)
    
    rapport("Traçage de la carte", info=T)
    # Carte
    g = ggplot(data = grille_eq) +
        geom_sf(aes(fill = min), color = "grey70", size=.1) +
        scale_fill_gradient(low = coulBas, high = coulHaut, name = leg,
                            breaks = brks, limits = c(brks[1], brks[length(brks)]),
                            guide = "legend")
    
    # Calques lignes
    if (axes)
    {
        g = cartoAxes(g = g, etendue =  etendue, carte = carte, proj = 2154)
        g = cartoLib(g = g, etendue = etendue, detail = detailNoms, carte = carte, proj = 2154)
    }
    
    g = g + labs(title = titre, subtitle = fonction, caption = credits)
    g = cartoFinish(g = g, etendue = etendue)
    
    return(g)
}

