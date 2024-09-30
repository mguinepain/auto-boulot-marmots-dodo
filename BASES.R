# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                   E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                     #
#                                                                                                 #
#                               SCRIPTS DE TRAVAIL M. GUINEPAIN                                   #
#                                           JAN. 2021                                             #
#                                                                                                 #  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# Dépendances ====

decodeUTF8 = function(chaine){
    chaine = gsub("Ã¨", "è", chaine)
    chaine = gsub("Ã©", "é", chaine)
    chaine = gsub("Ã§", "ç", chaine)
    chaine = gsub("Ã´", "ô", chaine)
    chaine = gsub("Ã«", "ë", chaine)
    chaine = gsub("Ãª", "ê", chaine)
    chaine = gsub("Ã¢", "â", chaine)
    return(chaine)
}

decodeDeduc = function(chaine){
    chaine = gsub("routiï¿½re", "routière", chaine)
    chaine = gsub("Hï¿½pital", "Hôpital", chaine)
    chaine = gsub("Samoï¿½ns", "Samoëns", chaine)
    return(chaine)
}

correcCase = function(chaine){
    chaine = gsub(" De ", " de ", chaine)
    chaine = gsub(" Des ", " des ", chaine)
    chaine = gsub(" Du ", " du ", chaine)
    chaine = gsub(" De La ", " de la ", chaine)
    chaine = gsub(" L'a", " l'A", chaine)
    chaine = gsub(" L'e", " l'E", chaine)
    chaine = gsub(" L'i", " l'I", chaine)
    chaine = gsub(" L'o", " l'O", chaine)
    chaine = gsub(" L'u", " l'U", chaine)
    chaine = gsub(" D'a", " d'A", chaine)
    chaine = gsub(" D'e", " d'E", chaine)
    chaine = gsub(" D'i", " d'I", chaine)
    chaine = gsub(" D'o", " d'O", chaine)
    chaine = gsub(" D'u", " d'U", chaine)
    chaine = gsub("Zi ", "ZI ", chaine)
    chaine = gsub("Za ", "ZA ", chaine)
    chaine = gsub("sncf", "SNCF", chaine)
    chaine = gsub("P+r", "P+R", chaine)
    chaine = gsub(" u ", " U ", chaine)
}

charger_tableZFdeComUniques = function(shp_ZF, PGT, DEP)
{
    # Renvoie une table d'équivalence qui permet de réattribuer certaines ZF manquantes
    
    # On crée une liste unifiée des codes des PGT et des ZF dont on a la géométrie
    shp.ui = select(st_drop_geometry(shp_ZF), CODE_ZF)
    pgt.ui = select(st_drop_geometry(PGT), ENQ, ZF)
    pgt.ui$uid_ZF = paste(pgt.ui$ENQ, pgt.ui$ZF)
    pgt.ui = select(pgt.ui, uid_ZF)
    listeZF = c(shp.ui$CODE_ZF, pgt.ui$uid_ZF)
    remove(shp.ui, pgt.ui)
    
    # On a besoin d'une base référençant : commune, nb de ZF, 1e ZF
    DEP.com = group_by(st_drop_geometry(shp_ZF), CODE_COM) %>% dplyr::summarize(N = n(), ZF = first(CODE_ZF))
    
    # On fait une base des ZF problématiques
    DEP.pb = select(DEP, uid_ENQ, D_ZF, D_Com) %>%
        mutate(Correspondance = ifelse(D_ZF %in% listeZF, 1, 0)) %>% filter(Correspondance == 0)
    
    # Lesquelles de ces ZF sont associées à une commune pour laquelle n'existe qu'une possibilité ?
    DEP.pb = left_join(DEP.pb, DEP.com, by=c("D_Com" = "CODE_COM")) %>% filter(N == 1)
    
    # On convertit ça en table de remplacement avec des lignes uniques
    DEP.pb = group_by(DEP.pb, D_ZF) %>% dplyr::summarize(ZF.y = first(ZF))
    
    return(DEP.pb)
}

charger_tableZF2Com = function(PER, DEP, mute=F)
{
    rapport("Calcul d'une table de correspondance ZF/Communes", info=mute)
    corzf1 = PER %>% select(uid_PER, ZF, Com) %>% rename(Com = Com) %>%
        mutate(ZF = paste(substr(uid_PER, 1, 7), ZF))
    corzf2 = PER %>% select(uid_PER, Travail_ZF, Travail_Com) %>%
        rename(ZF = Travail_ZF, Com = Travail_Com) %>%
        mutate(ZF = paste(substr(uid_PER, 1, 7), ZF))
    corzf3 = DEP %>% select(uid_PER, O_ZF, O_Com) %>% rename(ZF = O_ZF, Com = O_Com)
    corzf = rbind(corzf1, corzf2, corzf3) %>%
        group_by(ZF, Com) %>% summarize(n = n(), .groups="drop_last")
    testzf = group_by(corzf, ZF) %>% summarize(n = n(), .groups="drop_last") %>%
        mutate(unique = ifelse(n>1, F, T))
    corzf  = left_join(corzf, select(testzf, ZF, unique), by=c("ZF" = "ZF"))
    rapport("Part de correspondances uniques par ZF :",
            nrow(filter(corzf, unique==T))/nrow(corzf)*100, "%", info=T,mute=mute)
    
    nZonesNonTrouvees = nrow(filter(DEP, !O_ZF %in% corzf$ZF))
    rapport("Nombre de déplacements sans correspondance à échelle fine :",
            nZonesNonTrouvees/nrow(filter(DEP))*100, "%", info=T,mute=mute)
    nZonesNonTrouvees = nrow(filter(filter(DEP, O_ZT != "9999"), !O_ZF %in% corzf$ZF))
    rapport("en ne comptant pas le secteur 9999 :",
            nZonesNonTrouvees/nrow(filter(DEP, O_ZT != "9999"))*100, "%", info=T,mute=mute)
    
    # Réponse : approximation par réponse majoritaire à partir de la ZF.
    # On va ordonner corzf en décroissant :
    corzf2 = tab_Tri(corzf, i=3, rev = T)
    # Puis on groupe par ZF, on retient la commune avec "first" :
    corzf2 = group_by(corzf2, ZF) %>% summarize(Com = first(Com), n = sum(n))
    # On calcule combien de personnes ont été mal placées, par jointure :
    corzf2 = left_join(corzf2, select(corzf, ZF, Com, n), by=c("ZF" = "ZF", "Com" = "Com")) %>%
        mutate(nErr = n.x - n.y) %>% select(ZF, Com, n.x, nErr) %>% rename(n = n.x)
    rapport("A l'issue de l'opération,", sum(corzf2$nErr), "entités ont été mal placées,",
            "soit", round(sum(corzf2$nErr)/sum(corzf2$n)*100, 4), "% des activités", info=T,mute=mute)
    
    corzf = select(corzf2, ZF, Com, n, nErr)
    rapport(nrow(filter(corzf2, nErr>0)), "zones fines ambiguës présentent des erreurs, notamment :", info=T,mute=mute)
    
    return(corzf)
}

charger_listeZF = function(shp_ZF, PGT)
{
    # Initialisation de "ListeZF" qui comprend les entités géométriques existantes
    shp.ui = select(st_drop_geometry(shp_ZF), CODE_ZF)
    pgt.ui = select(st_drop_geometry(PGT), ENQ, ZF)
    pgt.ui$uid_ZF = paste(pgt.ui$ENQ, pgt.ui$ZF)
    pgt.ui = select(pgt.ui, uid_ZF)
    listeZF = c(shp.ui$CODE_ZF, pgt.ui$uid_ZF)
    remove(shp.ui, pgt.ui)
    return(listeZF)
}

tab_Renum = function(tab, # au format ACT
                     champTri, # chaîne de caractères
                     champNum)
{
    cat("\nRenumérotation... (itérations de plus en plus courtes)\n")
    colnames(tab)[colnames(tab) == champTri] = "cléTri"
    colnames(tab)[colnames(tab) == champNum] = "id"
    
    tab = tab_Tri(tab, "cléTri")
    
    # Sachant que la base est à présent triée dans le bon ordre
    n = 1
    clés = tab %>% group_by(uid_PER) %>% summarize(cléTri = first(cléTri), N = n())
    
    nMax = max(clés$N)
    ACT.restantes = tab
    
    while (nrow(clés) > 0) {
        
        cat("\rItération", n, "/", nMax, "(reste à traiter :", nrow(ACT.restantes), "entités)         ")
        
        # Toutes les clés correspondant à la tâche de rang n sont renumérotées n
        tab = tab %>% mutate(id = ifelse(cléTri %in% clés$cléTri, n, id))
        
        # On enlève ces tâches de la liste à traiter
        ACT.restantes = filter(ACT.restantes, !cléTri %in% clés$cléTri)
        
        # On rassemble les clés des tâches de rang n+1
        n = n + 1
        clés = ACT.restantes %>% group_by(uid_PER) %>% summarize(cléTri = first(cléTri))
    }
    
    colnames(tab)[colnames(tab) == "id"] = champNum
    colnames(tab)[colnames(tab) == "cléTri"] = champTri
    
    return(tab)
}

is.num_char = function(champ)
{
    if (is.numeric(champ) | is.character(champ)) { return(T)} else { return(F) }
}

# Initialisation manuelle des bases composites ====

init_PGT = function()
{
    rapport("Chargement de la base des PGT...")
    gt_BESAN = st_read("Sources/PGT/ED_Besancon_2018_GT.TAB",                 quiet = T)
    gt_REUNI = st_read("Sources/PGT/EDGT974_2015_2016_GT.TAB",                quiet = T)
    gt_DIJON = st_read("Sources/PGT/EDGT_DIJON_2016_GT.TAB",                  quiet = T)
    gt_DUNKE = st_read("Sources/PGT/EDGT_Dunkerque_2015_GT.TAB",              quiet = T)
    gt_ANNEM = st_read("Sources/PGT/EDGTFVG2016_GT.TAB",                      quiet = T)
    gt_ROYAN = st_read("Sources/PGT/EDGT Grand rovaltain_GT.MIF",             quiet = T)
    gt_ANNEC = st_read("Sources/PGT/EDGT_Reste74_2017_GT.TAB",                quiet = T)
    gt_METZ  = st_read("Sources/PGT/EDGT_SCOTAM_2017_GT.TAB",                 quiet = T)
    gt_HAVRE = st_read("Sources/PGT/ED LeHavre2018_GT.TAB",                   quiet = T)
    gt_BREST = st_read("Sources/PGT/ED_Pays_de_Brest_GT.TAB",                 quiet = T)
    gt_CARCA = st_read("Sources/PGT/EDVM_Carcassonne_2015_GT.TAB",            quiet = T)
    gt_CREIL = st_read("Sources/PGT/EDVM_Creil_2017_GT.TAB",                  quiet = T)
    gt_POITI = st_read("Sources/PGT/EDVM_Grand_Poitiers_2018_GT.TAB",         quiet = T)
    gt_NIORT = st_read("Sources/PGT/EDVM_Niort_2016_GT.TAB",                  quiet = T)
    gt_COTEN = st_read("Sources/PGT/EDVM_Pays_du_Cotentin_2016_GT_renum.TAB", quiet = T)
    gt_ALENC = st_read("Sources/PGT/EDVM_Pole_Metro_Caen_2017_GT.TAB",        quiet = T) # Alençon, pas Caen
    gt_STBRI = st_read("Sources/PGT/EDVM_st_brieuc_gt.MIF",                   quiet = T)
    gt_TOURS = st_read("Sources/PGT/EMC2_37_GT.TAB",                          quiet = T)
    gt_MARTI = st_read("Sources/PGT/EMD Martinique 2014_GT.TAB",              quiet = T)
    gt_CALVA = st_read("Sources/PGT/Dep14_GT_Aucame_2011.shp",                quiet = T)
    gt_DOUAI = st_read("Sources/PGT/PGT_Douai_2154.shp",                      quiet = T) 
    gt_LOIRA = read.csv("Sources/PGT/EMD_Nantes_2015_GT.csv", sep=";", dec=",")
    gt_ROUEN = st_read("Sources/PGT/EMD_ROUEN_2017_GT.TAB", quiet = T) %>% st_centroid()

    # Annecy
    gt_ANNEC = gt_ANNEC %>%
        mutate(ENQ  = "ANC2017",
               ZF   = paste0("000", Zf),
               LIB  = Nom,
               COM  = Insee,
               TYPE = Typegd) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    gt_ANNEC$LIB = decodeDeduc(gt_ANNEC$LIB)
    gt_ANNEC$LIB = gsub("ï¿½", "é", gt_ANNEC$LIB)
    
    # Annemasse
    gt_ANNEM = gt_ANNEM %>%
        mutate(ENQ  = "ANM2016",
               ZF   = paste0("000", NUM_ZF),
               LIB  = nom_ZF,
               COM  = INSEE,
               TYPE = CATEGORIE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Besançon
    gt_BESAN = gt_BESAN %>%
        mutate(ENQ  = "BES2018",
               ZF   = paste0("000", NUM_GT),
               LIB  = Libelle,
               COM  = NA,
               TYPE = NA)%>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    gt_BESAN$LIB = decodeUTF8(gt_BESAN$LIB)
    
    # Brest
    gt_BREST = gt_BREST %>%
        mutate(ENQ  = "BRE2018",
               ZF   = paste0("000", ZF),
               LIB  = REM,
               COM  = INSEE,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Alençon (en fait)
    gt_ALENC = gt_ALENC %>%
        mutate(ENQ  = "ALE2018",
               ZF   = paste0("000", ZF),
               LIB  = REM,
               COM  = INSEE,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    gt_ALENC$LIB = decodeUTF8(gt_ALENC$LIB)
    
    # Calvados
    gt_CALVA = gt_CALVA %>%
        mutate(ENQ = "CAL2011",
               ZF  = paste0("000", substr(DFIN,1,3), substr(DFIN,5,7)),
               LIB = LIB_DFIN,
               COM = NA,
               TYPE= TYPE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    
    # Carcassonne
    gt_CARCA = gt_CARCA %>%
        mutate(ENQ  = "CAR2015",
               ZF   = paste0("000", gsub(" ","", NUM_GENE2013)),
               LIB  = NOM_GENE,
               COM  = NA,
               TYPE = nature_generateur) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Cotentin
    gt_COTEN = gt_COTEN %>%
        mutate(ENQ  = "CHE2016",
               ZF   = formatC(Code_pgt, width=9, format="d", flag="0"), # reformater avec les zéros devant !!
               LIB  = Lib_pgt,
               COM  = Commune,
               TYPE = Type) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Creil
    gt_CREIL = gt_CREIL %>%
        mutate(ENQ  = "CRE2017",
               ZF   = paste0("000", Num_zf),
               LIB  = Nom,
               COM  = Cog,
               TYPE = NA)%>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    gt_CREIL$LIB = decodeUTF8(gt_CREIL$LIB)
    
    # Dijon
    gt_DIJON = gt_DIJON %>%
        mutate(ENQ  = "DIJ2016",
               ZF   = paste0("000", Num_zone_fine),
               LIB  = NOM,
               COM  = CODCOMM,
               TYPE = CATEGORIE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Douai
    gt_DOUAI = gt_DOUAI %>%
        mutate(ENQ  = "DOU2012",
               ZF   = formatC(NumGenerat, width=9, format="d", flag="0"),
               LIB  = Generateur,
               COM  = NA,
               TYPE = Type)%>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    gt_DOUAI$ZF = gsub(" ", "0", gt_DOUAI$ZF) # je ne m'explique pas qu'il faille faire ça
    
    # Dunkerque
    gt_DUNKE = gt_DUNKE %>%
        mutate(ENQ  = "DUN2015",
               ZF   = paste0("000", ZONE_FINE_GT),
               LIB  = Nom,
               COM  = Insee,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Le Havre
    gt_HAVRE = gt_HAVRE %>%
        mutate(ENQ  = "HAV2018",
               ZF   = paste0("000", ZF),
               LIB  = REM,
               COM  = INSEE,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Loire-Atlantique
    gt_LOIRA = gt_LOIRA %>%
        st_as_sf(coords = c("XL93", "YL93"), crs=2154) %>%
        mutate(ENQ  = "LOI2015",
               ZF   = formatC(cod_PGT_2, width=9, format="d", flag="0"),
               LIB  = Nom,
               COM  = Insee,
               TYPE = Libelle_theme_EDGT) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Martinique
    gt_MARTI = gt_MARTI %>%
        mutate(ENQ  = "MTQ2014",
               ZF   = paste0("000", ZF_GT_def),
               LIB  = NOM,
               COM  = NA,
               TYPE = TYPE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Metz
    gt_METZ  = gt_METZ  %>%
        mutate(ENQ  = "MET2017",
               ZF   = paste0("000", Code_gt),
               LIB  = Nom_gt,
               COM  = NA,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Niort
    gt_NIORT = gt_NIORT %>%
        mutate(ENQ  = "NIO2016",
               ZF   = paste0("0", ZoneFine), # préfixe 0
               LIB  = paste(TYPE, NOM_IRIS, ADRESSE), # du grand bricolage
               COM  = DEPCOM,
               TYPE = TYPE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Poitiers
    gt_POITI = gt_POITI %>%
        mutate(ENQ  = "POI2018",
               ZF   = paste0("000", ZF),
               LIB  = paste0("PGT N°", NUM_GT, " (", NOM_DTIR, ")"),
               COM  = INSEE,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # La Réunion
    gt_REUNI = gt_REUNI %>%
        mutate(ENQ  = "REU2016",
               ZF   = paste0("000", gsub(" ", "", NUM_GENE_2015)),
               LIB  = NOM,
               COM  = NA,
               TYPE = NOM_TYPE) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Rouen
    gt_ROUEN = gt_ROUEN %>%
        mutate(ENQ  = "ROU2017",
               ZF   = paste0("000", X_2017_zf),
               LIB  = Libelle_zf,
               COM  = Cod_com,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Royan
    gt_ROYAN = gt_ROYAN %>%
        mutate(ENQ  = "ROY2015",
               ZF   = paste0("000", substr(ZoneFine,1,3), substr(ZoneFine,5,7)),
               LIB  = Nom_ZoneFine,
               COM  = NA,
               TYPE = Type_ZoneFine) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Saint-Brieuc
    gt_STBRI = gt_STBRI %>%
        mutate(ENQ  = "STB2012",
               ZF   = GCIdentifier, # Ne correspond à rien : impossible de deviner les codes. Couche inutilisable
               LIB  = Nom,
               COM  = DEPCOM,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    # Tours
    gt_TOURS = gt_TOURS %>%
        mutate(ENQ  = "TOU2019",
               ZF   = Zf,
               LIB  = paste0(Rem, " (", Com, ")"),
               COM  = Insee,
               TYPE = NA) %>%
        select(ENQ, ZF, LIB, COM, TYPE, geometry)
    
    st_crs(gt_ROYAN) = 2154
    st_crs(gt_STBRI) = "wgs84"
    
    rapport("Adaptation des CRS des fichiers sources et fusion...")
    
    gt_ANNEC = st_transform(gt_ANNEC, crs = 3857)
    gt_ANNEM = st_transform(gt_ANNEM, crs = 3857)
    gt_BESAN = st_transform(gt_BESAN, crs = 3857)
    gt_BREST = st_transform(gt_BREST, crs = 3857)
    gt_ALENC = st_transform(gt_ALENC, crs = 3857)
    gt_CARCA = st_transform(gt_CARCA, crs = 3857)
    gt_COTEN = st_transform(gt_COTEN, crs = 3857)
    gt_CREIL = st_transform(gt_CREIL, crs = 3857)
    gt_DIJON = st_transform(gt_DIJON, crs = 3857)
    gt_DOUAI = st_transform(gt_DOUAI, crs = 3857)
    gt_DUNKE = st_transform(gt_DUNKE, crs = 3857)
    gt_HAVRE = st_transform(gt_HAVRE, crs = 3857)
    gt_LOIRA = st_transform(gt_LOIRA, crs = 3857)
    gt_MARTI = st_transform(gt_MARTI, crs = 3857)
    gt_METZ  = st_transform(gt_METZ , crs = 3857)
    gt_NIORT = st_transform(gt_NIORT, crs = 3857)
    gt_POITI = st_transform(gt_POITI, crs = 3857)
    gt_REUNI = st_transform(gt_REUNI, crs = 3857)
    gt_ROUEN = st_transform(gt_ROUEN, crs = 3857)
    gt_ROYAN = st_transform(gt_ROYAN, crs = 3857)
    gt_STBRI = st_transform(gt_STBRI, crs = 3857)
    gt_TOURS = st_transform(gt_TOURS, crs = 3857)
    gt_CALVA = st_transform(gt_CALVA, crs = 3857)
    
    # Fusion
    PGT = rbind(gt_ANNEC, gt_ANNEM, gt_BESAN, gt_BREST, gt_ALENC, gt_CARCA, gt_COTEN, gt_CREIL, gt_DIJON,
                gt_DOUAI, gt_DUNKE, gt_HAVRE, gt_LOIRA, gt_MARTI, gt_METZ , gt_NIORT, gt_POITI, gt_REUNI,
                gt_ROUEN, gt_ROYAN, gt_STBRI, gt_TOURS, gt_CALVA)
    
    # Un point à Niort n'est pas identifié
    PGT = filter(PGT, nchar(ZF) == 9)
    
    rapport("Association aux données communales...")
    
    shp_COM = st_read("Sources/Mailles/communes-20210101.shp", quiet=T)
    shp_COM = st_transform(shp_COM, crs = 3857)
    
    # Récupération du code commune (uniquement, d'où le select)
    # méthode nearest_feature pour inclure les points côtiers qui ne se trouvent pas exactement sur le terr de la commune
    PGT = st_join(PGT, select(shp_COM, insee, geometry), st_nearest_feature)
    
    if (nrow(filter(PGT, is.na(insee))) != 0) {
        rapport ("Impossible d'associer certains PGT à une commune :",
                 nrow(filter(PGT, is.na(insee))), "erreur(s)")
    }

    # On va garder ce code communal comme code principal
    colnames(PGT) = c("ENQ", "ZF", "LIB", "COM.orig", "TYPE", "COM", "geometry")
    
    rapport("Association aux ZF...")
    # Fichier d'Aurélie
    shp_ZF = st_read("Sources/ZF_49ED.shp", quiet = T) %>% st_transform(crs = 3857)
    # Standardisation des codes ZF à 9 chiffres
    shp_ZF = shp_ZF %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "NIORT", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "TOURS", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "VALENCIENNES" & ANNEE == "2019", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "IDF", paste0("00",CODE_ZF), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(nchar(CODE_ZF) == 6, paste0("000",CODE_ZF), CODE_ZF))
    
    # Jointure imparfaite, problèmes au niveau des littoraux
    PGT = st_join(PGT, select(shp_ZF, CODE_ZF, geometry), st_intersects)
    
    if (nrow(filter(PGT, is.na(CODE_ZF))) != 0) {
        rapport ("Impossible d'associer certains PGT à des codes de Zone Fine :",
                 nrow(filter(PGT, is.na(CODE_ZF))), "erreur(s)", info=T)
    }

    # Certains libellés sont en majuscules, d'autre pas, on va tout remettre à zéro
    PGT$LIB = tolower(PGT$LIB)
    PGT$LIB = tools::toTitleCase(PGT$LIB)
    PGT$LIB = correcCase(PGT$LIB)
    
    # À Metz, le PGT 00003301 n'est pas dans la base. Il correspond à un centre commercial.
    # Il y a justement un PGT mal codé dans le SHP qui correspond à un centre commercial (code déjà attribué à sa ZF).
    # Suggestion : corriger le shp PGT et supposer que ce point est celui incriminé
    PGT = mutate(PGT, ZF = ifelse(ENQ == "MET2017" & ZF == "000003003", "000003301", ZF))
    
    return(PGT)
}

init_COM = function(fsource = "Sources/Mailles/communes-20210101.shp",
                    corrigerParis = T, corrigerCorse = T)
{
    shp_COM = st_read(fsource, quiet = T)
    shp_COM = st_transform(shp_COM, crs=3857)
    
    if(corrigerParis)
    {
        shp_COM.arr = st_read("Sources/Mailles/arrondissements.shp", quiet = T) %>% st_transform(crs = 3857)
        shp_COM.arr$c_arinsee = as.character(shp_COM.arr$c_arinsee)
        colnames(shp_COM.arr) = c("nul1","nul2","insee","nom","nul3","nul4","nul5","nul6", "geometry")
        shp_COM.arr$wikipedia = NA ; shp_COM.arr$surf_ha = NA
        shp_COM.arr = select(shp_COM.arr, insee, nom, wikipedia, surf_ha, geometry)
        shp_COM = filter(shp_COM, insee != "75056")
        shp_COM = rbind(shp_COM, shp_COM.arr) ; remove(shp_COM.arr)
    }
    
    if(corrigerCorse)
    {
        shp_COM = mutate(shp_COM,
                         insee = ifelse(substr(insee,1,2) == "2A", paste0("20", substr(insee,3,5)), insee)) %>%
            mutate(insee = ifelse(substr(insee,1,2) == "2B",
                                  paste0("20", as.character(as.integer(substr(insee,3,5))+500)), insee))
    }
    
    return(shp_COM)
}

init_ZTS = function(MEN, shp_ZT, shp_COM, sauvZTS = T)
{
    rapport("Calque Zones de tirage pour les communes sans géométrie...")
    
    if ("sf" %in% class(MEN)) { MEN = st_drop_geometry(MEN) }
    
    # On crée des identifiants de secteurs uniques
    shp_ZT$CODE_SEC = paste0(shp_ZT$uid_ENQ, shp_ZT$CODE_SEC)
    
    # Ménages sans géométrie de secteur de tirage
    listeZT = shp_ZT$CODE_SEC
    MEN.SG = filter(MEN, !ZT %in% listeZT)
    
    # On crée une table de correspondance entre communes et zones de tirage simplifiées
    ComZT = MEN.SG %>% mutate(ZTS = paste0(uid_ENQ, ZT)) %>% group_by(Com) %>%
        summarize(uid_ENQ = first(uid_ENQ), ZTS = first(ZTS))
    
    # On réattribue une ZTS à tous les ménages
    MEN.SG = MEN.SG %>% left_join(ComZT, by=c("Com" = "Com"))
    
    # On créée une table géométrique pour ces nouvelles ZTs
    shp_ZTS = shp_COM %>% right_join(ComZT, by=c("insee" = "Com")) %>%
        group_by(ZTS) %>%
        summarize(ID_SEC = first(ZTS), ANNEE = substr(first(uid_ENQ), 4,7),
                  CODE_INSEE_VC = first(insee), uid_ENQ = first(uid_ENQ))
    
    # Problème : ces cartes comportent des "trous" dans les communes rurales sans enquêté.es
    # On crée des convex-hull pour chaque enquête
    hulls = shp_ZTS %>% group_by(uid_ENQ) %>% summarize() %>% st_convex_hull()
    
    # On regarde quels pointsonsurface se retrouvent dans les hulls des enquêtes
    pointsCom = shp_COM %>% st_point_on_surface() %>% st_join(hulls)
    
    # On ne garde que les points qui n'ont pas d'association
    pointsCom = filter(pointsCom, !insee %in% ComZT$Com)
    
    # On récupère les polygones
    pointsCom = left_join(st_drop_geometry(pointsCom), shp_COM, by=c("insee" = "insee")) %>% st_as_sf()
    
    # On leur attribue le secteur "le plus proche" et on fusionne, au sein de chaque enquête
    # du coup on retranche les polygones de chaque enquête pour les remplacer par des polygones "sans trous"
    listeEnq = unique(shp_ZTS$uid_ENQ)
    rapport("Résolution des communes isolées...", info=T)
    p = ui_ProgInit(length(listeEnq))
    for (i in 1:length(listeEnq))
    {
        subPointsCom = filter(pointsCom, uid_ENQ == listeEnq[i]) ; subShp_ZTS = filter(shp_ZTS, uid_ENQ == listeEnq[i])
        subPointsCom = subPointsCom %>% mutate(index = st_nearest_feature(subPointsCom, subShp_ZTS))
        subShp_ZTS$index = 1:nrow(subShp_ZTS)
        subPointsCom = mutate(subPointsCom, ZTS = NA, ID_SEC = NA, ANNEE = substr(uid_ENQ, 4,7)) %>%
            rename(CODE_INSEE_VC = insee) %>%
            select(ZTS, ID_SEC, ANNEE, CODE_INSEE_VC, uid_ENQ, geometry, index)
        subShp_ZTS = rbind(subShp_ZTS, subPointsCom) %>%
            group_by(index) %>% summarize(ZTS = first(na.omit(ZTS)), ID_SEC = first(na.omit(ID_SEC)), ANNEE = first(ANNEE),
                                          CODE_INSEE_VC = first(CODE_INSEE_VC), uid_ENQ = first(uid_ENQ))
        
        shp_ZTS = filter(shp_ZTS, uid_ENQ != listeEnq[i])
        shp_ZTS = rbind(shp_ZTS, select(subShp_ZTS, ZTS:uid_ENQ))
        ui_Prog(p, i)
    }
    
    # On attribue maintenant une ZT "graphique" à tout le monde
    MEN = MEN %>%
        left_join(ComZT, by=c("Com" = "Com", "uid_ENQ" = "uid_ENQ")) %>%
        mutate(ZTS = ifelse(is.na(ZTS), paste0(uid_ENQ, ZT), ZTS))
    
    if (sauvZTS) {
        # On sauve shp_ZTS
        rapport("Sauvegarde de la couche shp_ZTS dans Data")
        save(shp_ZTS, file="Data/shp_ZTS.rds")
    }
    
    return(MEN)
}

init_DEP2PER = function(PER, DEP)
{
    rapport("Préparation de l'agrégation de la base DEP")
    
    DEPintoPER = DEP %>%
        mutate(Dis_MAR = ifelse(ModeP %in% mode_MAR, Dis, 0),
               Dis_VEL = ifelse(ModeP %in% mode_VEL, Dis, 0),
               Dis_DRM = ifelse(ModeP %in% mode_DRM, Dis, 0),
               Dis_VOI = ifelse(ModeP %in% mode_VOI, Dis, 0),
               Dis_BUS = ifelse(ModeP %in% mode_BUS, Dis, 0),
               Dis_TRN = ifelse(ModeP %in% mode_TRN, Dis, 0),
               
               Dis_ALL_Dom = ifelse(D_Motif %in% motif_Dom, Dis_V, 0), Dis_RET_Dom = ifelse(O_Motif %in% motif_Dom, Dis_V, 0),
               Dis_ALL_Tvl = ifelse(D_Motif %in% motif_Tvl, Dis_V, 0), Dis_RET_Tvl = ifelse(O_Motif %in% motif_Tvl, Dis_V, 0),
               Dis_ALL_Etu = ifelse(D_Motif %in% motif_Etu, Dis_V, 0), Dis_RET_Etu = ifelse(O_Motif %in% motif_Etu, Dis_V, 0),
               Dis_ALL_Com = ifelse(D_Motif %in% motif_Com, Dis_V, 0), Dis_RET_Com = ifelse(O_Motif %in% motif_Com, Dis_V, 0),
               Dis_ALL_San = ifelse(D_Motif %in% motif_San, Dis_V, 0), Dis_RET_San = ifelse(O_Motif %in% motif_San, Dis_V, 0),
               Dis_ALL_Svc = ifelse(D_Motif %in% motif_Svc, Dis_V, 0), Dis_RET_Svc = ifelse(O_Motif %in% motif_Svc, Dis_V, 0),
               Dis_ALL_Lsr = ifelse(D_Motif %in% motif_Lsr, Dis_V, 0), Dis_RET_Lsr = ifelse(O_Motif %in% motif_Lsr, Dis_V, 0),
               Dis_ALL_Vst = ifelse(D_Motif %in% motif_Vst, Dis_V, 0), Dis_RET_Vst = ifelse(O_Motif %in% motif_Vst, Dis_V, 0),
               Dis_ALL_Acc = ifelse(D_Motif %in% motif_Acc, Dis_V, 0), Dis_RET_Acc = ifelse(O_Motif %in% motif_Acc, Dis_V, 0),
               
               Tps_MAR = ifelse(ModeP %in% mode_MAR, Duree, 0),
               Tps_VEL = ifelse(ModeP %in% mode_VEL, Duree, 0),
               Tps_DRM = ifelse(ModeP %in% mode_DRM, Duree, 0),
               Tps_VOI = ifelse(ModeP %in% mode_VOI, Duree, 0),
               Tps_BUS = ifelse(ModeP %in% mode_BUS, Duree, 0),
               Tps_TRN = ifelse(ModeP %in% mode_TRN, Duree, 0),
               
               Tps_ALL_Dom = ifelse(D_Motif %in% motif_Dom, Duree, 0), Tps_RET_Dom = ifelse(O_Motif %in% motif_Dom, Duree, 0),
               Tps_ALL_Tvl = ifelse(D_Motif %in% motif_Tvl, Duree, 0), Tps_RET_Tvl = ifelse(O_Motif %in% motif_Tvl, Duree, 0),
               Tps_ALL_Etu = ifelse(D_Motif %in% motif_Etu, Duree, 0), Tps_RET_Etu = ifelse(O_Motif %in% motif_Etu, Duree, 0),
               Tps_ALL_Com = ifelse(D_Motif %in% motif_Com, Duree, 0), Tps_RET_Com = ifelse(O_Motif %in% motif_Com, Duree, 0),
               Tps_ALL_San = ifelse(D_Motif %in% motif_San, Duree, 0), Tps_RET_San = ifelse(O_Motif %in% motif_San, Duree, 0),
               Tps_ALL_Svc = ifelse(D_Motif %in% motif_Svc, Duree, 0), Tps_RET_Svc = ifelse(O_Motif %in% motif_Svc, Duree, 0),
               Tps_ALL_Lsr = ifelse(D_Motif %in% motif_Lsr, Duree, 0), Tps_RET_Lsr = ifelse(O_Motif %in% motif_Lsr, Duree, 0),
               Tps_ALL_Vst = ifelse(D_Motif %in% motif_Vst, Duree, 0), Tps_RET_Vst = ifelse(O_Motif %in% motif_Vst, Duree, 0),
               Tps_ALL_Acc = ifelse(D_Motif %in% motif_Acc, Duree, 0), Tps_RET_Acc = ifelse(O_Motif %in% motif_Acc, Duree, 0))
    
    
    rapport("Agrégation de la base DEP à l'échelle des individus")
    
    DEPintoPER = DEPintoPER %>%
        group_by(uid_PER) %>%
        summarize(
            mDis = median(Dis),
            mDis_MAR = median(Dis_MAR), mDis_VEL = median(Dis_VEL), mDis_DRM = median(Dis_DRM),
            mDis_VOI = median(Dis_VOI), mDis_BUS = median(Dis_BUS), mDis_TRN = median(Dis_TRN),
            
            mDis.V = median(Dis_V),
            
            Dis.M = sum(Dis_MAR, Dis_VEL, Dis_DRM, Dis_VOI, Dis_BUS, Dis_TRN),
            Dis = sum(Dis), Tps = sum(Duree), N = n(),
            
            Dis.V = sum(Dis_V),
            
            Dis_MAR = sum(Dis_MAR), pDis_MAR = sum(Dis_MAR) / sum(Dis.M) * 100,
            Dis_VEL = sum(Dis_VEL), pDis_VEL = sum(Dis_VEL) / sum(Dis.M) * 100,
            Dis_DRM = sum(Dis_DRM), pDis_DRM = sum(Dis_DRM) / sum(Dis.M) * 100,
            Dis_VOI = sum(Dis_VOI), pDis_VOI = sum(Dis_VOI) / sum(Dis.M) * 100,
            Dis_BUS = sum(Dis_BUS), pDis_BUS = sum(Dis_BUS) / sum(Dis.M) * 100,
            Dis_TRN = sum(Dis_TRN), pDis_TRN = sum(Dis_TRN) / sum(Dis.M) * 100,
            
            Dis_Tvl = (sum(Dis_ALL_Tvl) + sum(Dis_RET_Tvl)) / 2,
            Dis_Etu = (sum(Dis_ALL_Etu) + sum(Dis_RET_Etu)) / 2,
            Dis_Com = (sum(Dis_ALL_Com) + sum(Dis_RET_Com)) / 2,
            Dis_San = (sum(Dis_ALL_San) + sum(Dis_RET_San)) / 2,
            Dis_Svc = (sum(Dis_ALL_Svc) + sum(Dis_RET_Svc)) / 2,
            Dis_Lsr = (sum(Dis_ALL_Lsr) + sum(Dis_RET_Lsr)) / 2,
            Dis_Vst = (sum(Dis_ALL_Vst) + sum(Dis_RET_Vst)) / 2,
            Dis_Acc = (sum(Dis_ALL_Acc) + sum(Dis_RET_Acc)) / 2,
            
            mDis_Tvl = median(c(Dis_ALL_Tvl, Dis_RET_Tvl)),
            mDis_Etu = median(c(Dis_ALL_Etu, Dis_RET_Etu)),
            mDis_Com = median(c(Dis_ALL_Com, Dis_RET_Com)),
            mDis_San = median(c(Dis_ALL_San, Dis_RET_San)),
            mDis_Svc = median(c(Dis_ALL_Svc, Dis_RET_Svc)),
            mDis_Lsr = median(c(Dis_ALL_Lsr, Dis_RET_Lsr)),
            mDis_Vst = median(c(Dis_ALL_Vst, Dis_RET_Vst)),
            mDis_Acc = median(c(Dis_ALL_Acc, Dis_RET_Acc)),
            
            mTps_MAR = median(Tps_MAR), mTps_VEL = median(Tps_VEL), mTps_DRM = median(Tps_DRM),
            mTps_VOI = median(Tps_VOI), mTps_BUS = median(Tps_BUS), mTps_TRN = median(Tps_TRN),
            
            Tps_MAR = sum(Tps_MAR),
            Tps_VEL = sum(Tps_VEL),
            Tps_DRM = sum(Tps_DRM),
            Tps_VOI = sum(Tps_VOI),
            Tps_BUS = sum(Tps_BUS),
            Tps_TRN = sum(Tps_TRN),
            
            Tps_Tvl = (sum(Tps_ALL_Tvl) + sum(Tps_RET_Tvl)) / 2,
            Tps_Etu = (sum(Tps_ALL_Etu) + sum(Tps_RET_Etu)) / 2,
            Tps_Com = (sum(Tps_ALL_Com) + sum(Tps_RET_Com)) / 2,
            Tps_San = (sum(Tps_ALL_San) + sum(Tps_RET_San)) / 2,
            Tps_Svc = (sum(Tps_ALL_Svc) + sum(Tps_RET_Svc)) / 2,
            Tps_Lsr = (sum(Tps_ALL_Lsr) + sum(Tps_RET_Lsr)) / 2,
            Tps_Vst = (sum(Tps_ALL_Vst) + sum(Tps_RET_Vst)) / 2,
            Tps_Acc = (sum(Tps_ALL_Acc) + sum(Tps_RET_Acc)) / 2,
            
            mTps = median(Duree),
            
            mTps_Tvl = median(c(Tps_ALL_Tvl, Tps_RET_Tvl)),
            mTps_Etu = median(c(Tps_ALL_Etu, Tps_RET_Etu)),
            mTps_Com = median(c(Tps_ALL_Com, Tps_RET_Com)),
            mTps_San = median(c(Tps_ALL_San, Tps_RET_San)),
            mTps_Svc = median(c(Tps_ALL_Svc, Tps_RET_Svc)),
            mTps_Lsr = median(c(Tps_ALL_Lsr, Tps_RET_Lsr)),
            mTps_Vst = median(c(Tps_ALL_Vst, Tps_RET_Vst)),
            mTps_Acc = median(c(Tps_ALL_Acc, Tps_RET_Acc)))
    

    PER = left_join(PER, DEPintoPER, by=c("uid_PER" = "uid_PER"))
    
    # Certaines personnes ne se sont pas déplacées. Elles n'ont donc aucun déplacement dans DEP, d'où des valeurs NA.
    # Mais il semble légitime de faire compter leur non-déplacement (0 minutes, 0 km) dans les moyennes de stock
    # (qu'elles vont donc faire chuter). Contrairement aux parts modales ou aux Distances médianes,
    # l'univers des variables de stock sera "Ensemble des enquêté.es ayant déclaré leurs déplacements",
    # et non seulement "Ensemble des enquêté.es ayant déclaré leurs déplacements et s'étant déplacés".
    
    colChange = c("Dis", "Tps", "N",
                  "Dis_MAR", "Dis_VEL", "Dis_DRM", "Dis_VOI", "Dis_BUS", "Dis_TRN",
                  "Tps_MAR", "Tps_VEL", "Tps_DRM", "Tps_VOI", "Tps_BUS", "Tps_TRN",
                  "Dis_Tvl", "Dis_Etu", "Dis_Com", "Dis_San", "Dis_Svc", "Dis_Lsr", "Dis_Vst", "Dis_Acc",
                  "Tps_Tvl", "Tps_Etu", "Tps_Com", "Tps_San", "Tps_Svc", "Tps_Lsr", "Tps_Vst", "Tps_Acc")
    
    PER = PER %>% mutate_at(colChange, ~ifelse(VeilleDepl == "2", 0, .))
    
    return(PER)
}

init_ACT = function(DEP, PER)
{
    # Tableau de base : 1e activité de la journée
    ACT = DEP %>% filter(as.numeric(O_Hr) > 399) %>% 
        group_by(uid_PER) %>% tab_Tri("idDep") %>%
        dplyr::summarize(idAct = 1,
                         Tache  = first(O_Motif),
                         Mode   = NA,
                         hFin   = first(O_Hr),
                         lDeb   = first(O_ZF),
                         lFin   = first(O_ZF)) %>%
        mutate(hDeb = "0400") %>% select(uid_PER, idAct, Tache, Mode, hDeb, hDeb, hFin, hFin, lDeb, lFin)
    
    # Méthode récursive pour ajouter ensuite les autres tâches : préparation
    DEP.T = DEP %>% filter(as.numeric(O_Hr) > 399) %>%
        mutate(idDep = as.numeric(idDep)) %>%
        tab_Renum(champTri = "uid_DEP", champNum = "idDep")
    DEP.T$idD = as.integer(as.character(DEP.T$idDep))
    i = 0 ; j = 1 ; m = max(DEP.T$idDep)
    rapport("Complétion de la base ACT à partir de la base DEP...")
    cat("(Chaque itération sera plus rapide que la précédente)") ;  cat("\nItération 0 /", m)
    listeChevauchements = ""
    
    # Boucle, tant qu'il reste des déplacements à inverser
    while(nrow(DEP.T) > 0)
    {
        i = i + 1 ; j = j + 2
        cat("\rItération", i, "/", m)
        ACT.l0 = DEP.T %>% group_by(uid_PER) %>%
            dplyr::summarize(idAct = j-1,
                             Tache = "99",
                             Mode  = first(ModeP),
                             hDeb  = first(O_Hr),
                             hFin  = first(D_Hr),
                             lDeb  = first(O_ZF),
                             lFin  = first(D_ZF))
        ACT.l1 = DEP.T %>% group_by(uid_PER) %>%
            dplyr::summarize(idAct = j,
                             Mode  = NA,
                             Tache = first(D_Motif),
                             hDeb  = first(D_Hr),
                             lDeb  = first(D_ZF))
        DEP.T = DEP.T %>% filter(idD>i)
        ACT.l2 = DEP.T %>% group_by(uid_PER) %>%
            dplyr::summarize(hFin = first(O_Hr),
                             lFin = first(O_ZF))
        ACT.l1 = left_join(ACT.l1, ACT.l2, by=c("uid_PER" = "uid_PER")) %>%
            select(uid_PER, idAct, Tache, Mode, hDeb, hFin, lDeb, lFin)
        listeChevauchements = c(listeChevauchements,
                                filter(ACT.l1, heureHHMMtoM(hFin) < heureHHMMtoM(hDeb))$uid_PER)
        ACT = rbind(ACT, ACT.l0, ACT.l1)
    }
    cat(" succès !\n")
    
    # On retire les tuilages
    rapport(length(listeChevauchements), "cas de tuilage de déplacements trouvés et supprimés de la base.",
            info = T)
    DEP = filter(DEP, !uid_PER %in% listeChevauchements)
    ACT = filter(ACT, !uid_PER %in% listeChevauchements)
    
    # Edit juin 2021
    # Pour appliquer ce qui suit, il faut filtrer de rares cas où l'heure de fin est un NA par erreur
    # Pour le savoir, il faut qu'on voie si le déplacement est la dernière ligne
    ACT.max = ACT %>% group_by(uid_PER) %>% summarize(idMax = max(idAct))
    listeNA.erreurs = ACT %>% left_join(ACT.max, by=c("uid_PER" = "uid_PER")) %>%
        filter(is.na(hFin) & idAct != idMax)
    remove(ACT.max)
    rapport(nrow(listeNA.erreurs), "valeurs manquantes dans les heures de fin de tâche,",
            length(unique(listeNA.erreurs$uid_PER)), "enquêté·es retiré·es", info = T)
    ACT = filter(ACT, !uid_PER %in% unique(listeNA.erreurs$uid_PER))
    
    # Les heures de fin en NA signifient (désormais) qu'il n'y a plus de déplacements ensuite :
    # on reste donc sur cette même "tâche" jusqu'à 4h du lendemain,
    # donc on leur attribue la valeur 28
    ACT = ACT %>% mutate(lFin   = ifelse(is.na(hFin),              lDeb , lFin),
                         hFin   = ifelse(is.na(hFin),             "2800", hFin),
                         hFin   = ifelse(as.numeric(hFin) > 2800, "2800", hFin))
    
    # Nettoyage
    remove(DEP.T, i, j, ACT.l0, ACT.l1, ACT.l2)
    
    # Calcul de la durée de chaque activité à partir de l'heure de début et de l'heure de fin
    ACT = ACT %>% mutate(du =
                             (as.numeric(substr(hFin,1,2))*60 - as.numeric(substr(hDeb,1,2))*60) -
                             as.numeric(substr(hDeb,3,4))    + as.numeric(substr(hFin,3,4)))
    
    # Attribution des types d'activité à partir des motifs des déplacements précédents et suivants
    # Problème : les "tournées" et "promenades" comptent comme des activités de longueur 0
    #            il faudrait prendre comme activité les déplacements qui les encadrent
    # Pour comparer une ligne avec la suivante et la précédente il va falloir tricher :
    # on va trier le tableau, on va le coller à un fragment de lui-même avec une ligne de
    # décalage vers le haut et même chose une ligne vers le bas
    rapport("Réattribution des motifs de déplacement...")
    ACT.sd = ACT[order(ACT$uid_PER,ACT$idAct),]
    ACT.sd = ACT.sd %>% mutate(prTache = Tache, prDu = du, suTache = Tache, suDu = du)
    ACT.pr = select(ACT.sd, uid_PER, prTache, prDu)
    ACT.su = select(ACT.sd, uid_PER, suTache, suDu)
    colnames(ACT.pr) = c("prUid_PER", "prTache", "prDu")
    colnames(ACT.su) = c("suUid_PER", "suTache", "suDu")
    ACT.pr = rbind(c("","",0), ACT.pr[1:(nrow(ACT.pr)-1),])
    ACT.su = rbind(ACT.su[2:nrow(ACT.su),], c("","",0))
    ACT.sd = select(ACT.sd, uid_PER:du)
    ACT.pr$prTache = as.character(ACT.pr$prTache)
    ACT.su$suTache = as.character(ACT.su$suTache)
    ACT.sd = cbind(ACT.sd, ACT.pr, ACT.su) %>%
        mutate(prTache = ifelse(uid_PER != prUid_PER, NA, prTache),
               prDu    = ifelse(uid_PER != prUid_PER, NA, prDu   ),
               suTache = ifelse(uid_PER != suUid_PER, NA, suTache),
               suDu    = ifelse(uid_PER != suUid_PER, NA, suDu   )) %>%
        select(uid_PER:du, prTache, prDu, suTache, suDu)
    remove(ACT.pr, ACT.su)
    
    # On va donc remplacer "déplacement" par l'activité de longueur zéro pour les cas suivants :
    z_Zero = c("52", "81", "82", "61", "62", "63", "64", "71", "72", "73", "74")
    # Tournées, promenades et accompagnements, où le déplacement est considéré comme une tâche
    # à part entière (ce qui explique qu'elle soit répétée immédiatement après ; n'est pas suivie
    # d'un moment d'accomplissement statique).
    # "Contaminons" les lignes de déplacement par celles à 0 :
    ACT.sd$Tache = as.character(ACT.sd$Tache)
    ACT.sd = ACT.sd %>%
        mutate(Tache = ifelse(suDu == 0 & suTache %in% z_Zero, suTache, Tache),
               Tache = ifelse(prDu == 0 & prTache %in% z_Zero, prTache, Tache))
    ACT = ACT.sd %>% select(uid_PER:du)
    remove(ACT.sd)
    
    # On filtre les activités qui démarrent "après" 28h (déplacements plus longs, etc.), résidus d'un échec de
    # la prise en charge de ce qu'il se passe au-delà de 4h du jour même
    ACT = filter(ACT, !heureHHMMtoM(hDeb) > 1680)
    
    # On vérifie qu'il n'y a pas d'activités de longueur négative
    erreurs = filter(ACT, du < 0)
    if (nrow(erreurs) > 0)
    {
        View(filter(ACT, uid_PER %in% erreurs$uid_PER))
        DEP = tab_Tri(DEP, "uid_PER")
        View(filter(DEP, uid_PER %in% erreurs$uid_PER))
        rapport("Erreur : certaines activités sont de longueur inférieure à zéro.")
        stop("Certaines activités sont de longueur inférieure à zéro.")
    }
    
    # Précisions dans l'attribution des tâches
    # Pour les traitements ultérieurs (à base géographique), on a besoin de préciser certaines activités.
    # On propose de traiter les déplacements différemment selon le mode principal (le suivi détaillé est
    # trop compliqué pour l'instant).
    # On propose également de différencier les activités professionnelles selon la profession de l'enquêté.e.
    rapport("Modification des codes d'activité")
    Mode_Adhérant   = c("01", "93", "94", "96", "97")
    Mode_Individuel = as.character(c(10:19, 20:22, 37, 61, 62, 71, 81, 82, 95))
    Mode_Co_Bus     = c("31", "38")
    Mode_Co_Tram    = c("32", "34")
    Mode_Co_Métro   = c("33")
    Mode_Co_InterU  = c("41")
    Mode_Co_Train   = c("52", "53", "54")
    Mode_Co_Extra   = c("39", "42", "43", "51", "91", "92")
    Travail = as.character(c(10:14), 81)
    Dépl    = as.character(c(99, 60, 70, 52))
    ACT = left_join(ACT, select(PER, uid_PER, PCS8), by=c("uid_PER" = "uid_PER"))
    ACT = mutate(ACT,
                 PCS8 = substr(as.character(PCS8), 2, 2),
                 Tache = case_when(# 1) on simplifie les tâches d'accompagnement et de travail
                     Tache %in% c("61", "62", "63", "64") ~ "60",
                     Tache %in% c("71", "72", "73", "74") ~ "70",
                     Tache %in% c("11", "12", "13", "14") ~ "10",
                     Tache == "26"                        ~ "22",
                     Tache == "27"                        ~ "23",
                     Tache == "28"                        ~ "24",
                     Tache == "29"                        ~ "25",
                     TRUE ~ Tache)) %>%
        mutate(  Tache = case_when(# 2) on recode les tâches des déplacements en fonction du mode
            Tache %in% Dépl & Mode %in% Mode_Adhérant ~ paste0(Tache, "1"),
            Tache %in% Dépl & Mode %in% Mode_Individuel~paste0(Tache, "2"),
            Tache %in% Dépl & Mode %in% Mode_Co_Bus   ~ paste0(Tache, "3"),
            Tache %in% Dépl & Mode %in% Mode_Co_Tram  ~ paste0(Tache, "4"),
            Tache %in% Dépl & Mode %in% Mode_Co_Métro ~ paste0(Tache, "5"),
            Tache %in% Dépl & Mode %in% Mode_Co_InterU~ paste0(Tache, "6"),
            Tache %in% Dépl & Mode %in% Mode_Co_Train ~ paste0(Tache, "7"),
            Tache %in% Dépl & Mode %in% Mode_Co_Extra ~ paste0(Tache, "8"),
            Tache %in% Dépl & is.na(Mode)             ~ paste0(Tache, "9"),
            # 3) on recode les tâches travail en fonction de la PCS individuelle,
            #    si elle est disponible
            Tache %in% Travail & !is.na(PCS8) &
                PCS8 %in% as.character(c(1:6)) ~ paste0(Tache, PCS8),
            Tache %in% Travail &  is.na(PCS8)  ~ paste0(Tache, "9"),
            Tache %in% Travail & !is.na(PCS8) &
                PCS8 %in% c("7", "8")          ~ paste0(Tache, "9"),
            # 4) tout ce qui reste (2 caractères donc) : on rajoute un 0
            nchar(Tache) == 2 ~ paste0(Tache, "0")
        )) %>%
        mutate (Tache = case_when( Tache == "100" ~ "109",
                                   T              ~ Tache)) %>%
        mutate (Tache = as.factor(Tache)) %>%
        select(uid_PER, idAct, Tache, hDeb, hFin, lDeb, lFin, du)

    # Pour les traitements à base géographique, les activités qui commencent à un endroit et se finissent à un
    # autre ne vont pas fonctionner. La solution adoptée est de :
    # - "casser" en deux les activités mobiles "adhérentes", comme les balades à pied (une au début, une à la fin)
    # - attribuer les activités pratiquées en transports en commun à un secteur fictif correspondant au type de TCO
    # - considérer que les personnes qui sont dans leur voiture/TGV/jet privé sont "retirées" du territoire
    # De nombreuses tâches qui ne sont pas censées être des déplacements se retrouvent, elles aussi, à avoir
    # un lieu de début et de fin différent...
    ACT = ACT %>% mutate(casser = lDeb != lFin) %>%
        mutate(casser = ifelse(is.na(casser), F, casser))
    
    # En l'état, les déplacements et les activités sont des entités de même nature. Mais cela signifie qu'on
    # ne sait pas où sont les gens à chaque moment : certaines activités (les déplacements donc) commencent à
    # un endroit et se finissent à un autre. Pour savoir où est la personne, il faut un ID-géo unique.
    # Plusieurs solutions sont adoptées :
    # - les transports : dans certains cas (modes doux et lents), on peut considérer que la personne en déplacement
    #   "adhère" au territoire, et donc qu'elle se situe dans les lieux mentionnés. Solution envisagée = diviser
    #   le déplacement en deux activités de longueur égale, l'une située dans le lieu de départ, et l'autre dans
    #   celui d'arrivée. Une solution très approximative (la personne peut être passée par d'autres ZT...), mais
    #   qui permet d'étudier la présence dans l'espace public.
    #   Solution à adopter pour les modes relatifs à la marche à pied. Contrairement à ce que j'ai entendu
    #   ailleurs, cependant, ne pas prendre le vélo (la division par deux ne ferait vraiment pas trop sens)
    # - pour les transports non adhérents, il est impossible de savoir "où" sont les gens durant le trajet.
    #   les approximations seraient vraiment abusives (inclure les secteus situés sur une ligne entre le départ
    #   et l'arrivée, en considérant un temps de transit constant, à partir de TRJ ?)
    #   pour l'instant, on peut soit les exclure de l'étude (lieu --> NA pendant le temps de trajet)
    #   soit leur attribuer un "lieu" fictif associant mode de transport et rayon d'enquête (lieu = "train IDF")
    # - les lieux de travail peuvent être précisés selon la PCS8 de l'enquêté.e (ex: "lieu de travail type prof.
    #   intermédiaire", "lieu de travail type agriculture", etc.)
    # - ce niveau de détail mobilise une rétrocorrection de ACT qui a été appliquée en en-tête de ce fichier
    
    # On "casse" les activités ayant un lieu de début et de fin en 2 activités distinctes :
    rapport("Division des activités bi-localisées")
    Moitiés = ACT %>% filter(casser) %>%
        mutate(du1  = du,
               du   = ceiling(du / 2),
               hDeb = heureMtoHHMM(heureHHMMtoM(hDeb) + (du1 - du)),
               l    = lFin) %>%
        select(uid_PER, idAct, Tache, hDeb, hFin, lDeb, lFin, du, l)
    ACT2 = ACT %>%
        mutate(du1 = du,
               du  = ifelse(casser, floor(du/2), du), # le floor est complémentaire du ceiling de la 2e moitié
               hFin= ifelse(casser, heureMtoHHMM(heureHHMMtoM(hFin) - (du1 - du)), hFin),
               l   = lDeb) %>%
        select(uid_PER, idAct, Tache, hDeb, hFin, lDeb, lFin, du, l) %>%
        rbind(Moitiés) %>%
        select(uid_PER, idAct, Tache, hDeb, hFin, du, l) %>%
        mutate(cléTri = paste0(uid_PER, formatC(heureHHMMtoM(hDeb), width = 4, flag="0"),
                               formatC(heureHHMMtoM(hFin), width = 4, flag="0"))) %>%
        mutate(cléTri = as.character(cléTri)) %>%
        tab_Tri("cléTri")
    
    # !!!! AOÛT 2022 : La prise en compte des activités de longueur 0 génère des pbs de numérotation.
    # Pour que ça fonctionne, je prends aussi l'heure de fin en compte pour la renumérotation.
    ACT2 = tab_Renum(tab = ACT2, champTri = "cléTri", champNum = "idAct") %>%
        select(uid_PER:l)
    
    # L'inclusion d'activités de longueur 0 (adoptée en avril 2022 pour pouvoir identifier la destination
    # d'une promenade ou d'un déplacement considéré comme une activité) provoque des erreurs. Le patch
    # suivant doit les corriger.
    test = ACT2 %>% group_by(uid_PER) %>% summarize(s = sum(du)) %>% filter(s != 1440)
    if (nrow(test) > 0)
    {
        rapport("Erreurs détectées lors de la division des activités bilocalisées.")
        corrigeable = nrow(filter(test, s == 2880)) / nrow(test) * 100
        rapport(corrigeable, "% de ces erreurs peuvent être corrigées (doublons parfaits).", info=T)
        # On groupe les activités ayant le même uid_PER et le même identifiant, on garde la première valeur
        ACTErr = filter(ACT2, uid_PER %in% filter(test, s==2880)$uid_PER) %>%
            group_by(uid_PER, idAct) %>% summarize_all(~ first(.x))
        # On vire ces activités de la base ACT2
        ACT2 = filter(ACT2, !uid_PER %in% filter(test, s==2880)$uid_PER)
        # On les réinsère
        ACT2 = rbind(ACT2, ACTErr)
        # Le tour est joué
        test = ACT2 %>% group_by(uid_PER) %>% summarize(s = sum(du)) %>% filter(s != 1440)
        if (nrow(test) > 0)
        {
            View(filter(ACT2, uid_PER %in% filter(test, s!=1440)$uid_PER))
            rapport("La correction des erreurs n'a pas permis de résoudre le problème. Le script doit s'arrêter.", info=T)
            stop("Le cassage des activités bilocalisées provoque des dédoublements.")
        }
        else
        {
            rapport("Les erreurs ont été corrigées avec succès. Mais il faudrait en vérifier la provenance.", info=T)
        }
    } else { cat(" succès !") }
    
    # Pour finir, on retire les modes non-adhérents des secteurs (afin de pouvoir ne pas les considérer
    # comme étant dans le quartier) et on attribue un secteur fictif aux personnes des t-co, ce qui
    # permet de faire comme s'ils étaient "ensemble" pour les analyses de coprésence (mieux que rien)
    # C'est cela qui pose problème pour les activités qui sont des déplacements (ex: un accompagnement
    # en voiture est une activité en soi, mais elle "disparaît de la carte" ; solution = une activité
    # de longueur 0 qui est aussi un accompagnement mais qui, elle, est localisée ; permet de retracer
    # le trajet suivi par la personne, tout en n'estimant pas à tort qu'elle fréquente le secteur tout
    # du long).
    ACT2 = ACT2 %>%
        mutate(l = case_when(Tache %in% c("992", "602", "702", "522") ~ "NA",
                             Tache %in% c("993", "603", "703", "523") ~ paste(substr(uid_PER,1,7), "BUS"),
                             Tache %in% c("994", "604", "704", "524") ~ paste(substr(uid_PER,1,7), "TRAM"),
                             Tache %in% c("995", "605", "705", "525") ~ paste(substr(uid_PER,1,7), "METRO"),
                             Tache %in% c("996", "606", "706", "526") ~ paste(substr(uid_PER,1,7), "CAR INTER"),
                             Tache %in% c("997", "607", "707", "527") ~ paste(substr(uid_PER,1,7), "TRAIN"),
                             Tache %in% c("998", "608", "708", "528") ~ "NA",
                             Tache %in% c("999", "609", "709", "529") ~ "NA",
                             T ~ l)) %>%
        mutate(l = ifelse(l == "NA", NA, l)) # les lieux retirés sont des NA (case_when n'en veut pas)
    
    # PROBLÈME : il reste 340 doublons dans ACT2 à cause d'activités de longueur nulle qui se succèdent.
    # Tab_Renum ne peut pas les résoudre. Il n'en faut aucun. Workaround : supprimer les doublons.
    # Solution inélégante : 1) attribuer un numéro pair ou impair à chaque ligne
    #                       2) faire la liste des doublons
    #                       3) supprimer toutes les lignes faisant partie de la liste et ayant un numéro pair
    while (nrow(filter(as.data.frame(table(mutate(ACT2, uid_ACT = paste0(uid_PER, ".", idAct))$uid_ACT)), Freq>1))>0)
    {
        nDbl = nrow(filter(as.data.frame(table(mutate(ACT2, uid_ACT = paste0(uid_PER, ".", idAct))$uid_ACT)), Freq>1))
        rapport("!!! Présence de", nDbl, "doublons dans ACT2 (liés à des activités de longueur nulle).")
        rapport("Purge des doublons d'ACT2.", info=T)
        listeNums = rep.int(c(1, 0), ceiling(nrow(ACT2)/2))
        if (length(listeNums) != nrow(ACT2)) { listeNums = listeNums[1:length(listeNums) - 1] }
        ACT2$pair = listeNums
        listeDbls = filter(as.data.frame(table(mutate(ACT2, uid_ACT = paste0(uid_PER, ".",
                                                                             formatC(idAct, width = 4,
                                                                                     flag="0")))$uid_ACT)),
                           Freq>1)$Var1
        ACT2$uid_ACT = paste0(ACT2$uid_PER, ".", formatC(ACT2$idAct, width=4, flag="0"))
        ACT2$doublon = ACT2$uid_ACT %in% listeDbls
        ACT2 = ACT2 %>% filter(doublon == F | (doublon == T & pair == 1))
        ACT2 = select(ACT2, -uid_ACT, -doublon, -pair) ; remove(listeNums, listeDbls)
        # À recommencer jusqu'à s'être débarrassé de tous ces foutus doublons !!!!
    }
    
    # Rester chez soi constitue une activité !
    ACT2 = right_join(ACT2, filter(select(PER, uid_PER, CoeffEnq, uid_ENQ, ZF), CoeffEnq > 0), by=c("uid_PER" = "uid_PER")) %>%
        mutate(du    = ifelse(is.na(idAct), 1440, du),
               l     = ifelse(is.na(idAct), paste(uid_ENQ, ZF), l),
               hDeb  = ifelse(is.na(idAct), "0400", hDeb),
               hFin  = ifelse(is.na(idAct), "2800", hFin),
               Tache = ifelse(is.na(idAct), "010", as.character(Tache)),
               Tache = as.factor(Tache),
               idAct = ifelse(is.na(idAct), 1, idAct)) %>%
        select(uid_PER:l, CoeffEnq) %>%
        mutate(uid_MEN = substr(uid_PER, 1,22))
    
    # C'est enfin fini
    return(ACT2)
}

init_ACT2PER = function(PER, ACT)
{
    ACTintoPER = ACT %>%
        mutate(duDom = ifelse(substr(Tache,1,1) == "0", du, 0),
               duTvl = ifelse(substr(Tache,1,1) == "1" | substr(Tache,1,2) == "81", du, 0),
               duEtu = ifelse(substr(Tache,1,1) == "2", du, 0),
               duCom = ifelse(substr(Tache,1,1) == "3" | substr(Tache,1,2) == "82", du, 0),
               duSvc = ifelse(substr(Tache,1,1) == "4", du, 0),
               duLsr = ifelse(substr(Tache,1,1) == "5", du, 0),
               duTax = ifelse(substr(Tache,1,1) %in% c("6","7"), du, 0),
               duDep = ifelse(substr(Tache,1,2) == "99", du, 0),
               duNA  = ifelse(substr(Tache,1,2) == "91", du, 0),
               # les 30 et 0 qui suivent remplacent temporairement des non-réponses
               tvlDeb= ifelse(substr(Tache,1,1) %in% c("1","2"),
                              as.numeric(substr(hDeb,1,2)) + (as.numeric(substr(hDeb,3,4))/60), 30),
               tvlFin= ifelse(substr(Tache,1,1) %in% c("1","2"),
                              as.numeric(substr(hFin,1,2)) + (as.numeric(substr(hFin,3,4))/60), 0 ),
               comDeb= ifelse(substr(Tache,1,1) == "3",
                              as.numeric(substr(hDeb,1,2)) + (as.numeric(substr(hDeb,3,4))/60), 30),
               comFin= ifelse(substr(Tache,1,1) == "3",
                              as.numeric(substr(hFin,1,2)) + (as.numeric(substr(hFin,3,4))/60), 0 ),
               lsrDeb= ifelse(substr(Tache,1,1) == "5",
                              as.numeric(substr(hDeb,1,2)) + (as.numeric(substr(hDeb,3,4))/60), 30),
               lsrFin= ifelse(substr(Tache,1,1) == "5",
                              as.numeric(substr(hFin,1,2)) + (as.numeric(substr(hFin,3,4))/60), 0 )) %>%
        group_by(uid_PER) %>% dplyr::summarize(DuDom = sum(duDom), DuTvl = sum(duTvl),
                                               DuEtu = sum(duEtu), DuCom = sum(duCom),
                                               DuSvc = sum(duSvc), DuLsr = sum(duLsr),
                                               DuTax = sum(duTax), DuDep = sum(duDep),
                                               DuNA  = sum(duNA),
                                               JoDeb = first(hFin),   JoFin = last(hDeb), 
                                               JoTvDeb = min(tvlDeb), JoTvFin = max(tvlFin),
                                               JoCmDeb = min(comDeb), JoCmFin = max(comFin),
                                               JoLrDeb = min(lsrDeb), JoLrFin = max(lsrFin)) %>%
        mutate(JoTvDeb = ifelse(JoTvDeb == 30, NA, JoTvDeb),
               JoTvFin = ifelse(JoTvFin == 0,  NA, JoTvFin),
               JoCmDeb = ifelse(JoCmDeb == 30, NA, JoCmDeb),
               JoCmFin = ifelse(JoCmFin == 0,  NA, JoCmFin),
               JoLrDeb = ifelse(JoLrDeb == 30, NA, JoLrDeb),
               JoLrFin = ifelse(JoLrFin == 0,  NA, JoLrFin)) %>%
        right_join(select(PER, uid_PER, CoeffEnq), by=c("uid_PER" = "uid_PER")) %>%
        filter(CoeffEnq > 0) %>% # on vire les gens pas enquêtés
        mutate(DuDom = ifelse(is.na(DuNA), 1440, DuDom), # on récupère les gens qui n'ont pas bougé... et on suppose
               # qu'ils sont restés à domicile, ce qui est une inexactitude
               # majeure, mais très pratique (stricto sensu, devrait être NA)
               DuTvl = ifelse(is.na(DuNA),    0, DuTvl), DuEtu = ifelse(is.na(DuNA), 0, DuEtu),
               DuCom = ifelse(is.na(DuNA),    0, DuCom),
               DuSvc = ifelse(is.na(DuNA),    0, DuSvc), DuLsr = ifelse(is.na(DuNA), 0, DuLsr),
               DuTax = ifelse(is.na(DuNA),    0, DuTax), DuDep = ifelse(is.na(DuNA), 0, DuDep),
               DuNA  = ifelse(is.na(DuNA),    0, DuNA))
    
    # On vérifie qu'il n'y a pas de problèmes (emplois du temps absurdes pour un individu)
    listepb = filter(ACTintoPER, DuDom + DuTvl + DuEtu + DuCom + DuSvc + DuLsr + DuTax + DuDep + DuNA != 1440) %>%
        mutate(s = DuDom + DuTvl + DuEtu + DuCom + DuSvc + DuLsr + DuTax + DuDep + DuNA)
    if (nrow(listepb) > 0) {
        rapport("Nombre d'emplois du temps non conformes :", nrow(listepb),
                "soit", round(nrow(listepb)/nrow(ACTintoPER)*100,2), "% du total", info = T)
    } else { rapport("Aucun emploi du temps non conforme. Bravo ! :)", info = T) }
    
    ACTintoPER = select(ACTintoPER, -CoeffEnq)
    
    PER = left_join(PER, ACTintoPER, by=c("uid_PER" = "uid_PER"))
    return(PER)
}

init_HDP = function(ACT, PER)
{
    rapport("Constitution du tableau des heures de pointe")
    
    # Circulation pendant les heures de pointe ?
    # Il faut qu'on détermine ce qui correspond aux heures de pointe.
    ACT$uid_ENQ = substr(ACT$uid_PER, 1, 7)
    listeH = seq(248, 1620, 15) # On calcule le tableau avec 8 mn de décalage pr éviter vals rondes.
    hdp = data.frame(heure = NA, uid_ENQ = NA, n_Voit = NA, n_Coll = NA)
    b = ui_ProgInit(max(listeH)-240)
    for (h in listeH)
    {
        hdpP = filter(ACT, h >= heureHHMMtoM(hDeb) & h < heureHHMMtoM(hFin)) %>%
            mutate(n_Voit = ifelse(Tache == "992", CoeffEnq, 0),
                   n_Coll = ifelse(Tache %in% c("993","994","995","996","997","998"), CoeffEnq, 0)) %>%
            group_by(uid_ENQ) %>% summarize(n_Voit = sum(n_Voit), n_Coll = sum(n_Coll)) %>%
            mutate(heure = h)
        hdp = rbind(hdp, hdpP)
        ui_Prog(b, h-240)
    }
    
    hdp = left_join(hdp, summarize(group_by(PER, uid_ENQ), Pop = sum(CoeffEnq, na.rm=T)), by=c("uid_ENQ" = "uid_ENQ"))
    hdp = left_join(hdp, summarize(group_by(hdp, uid_ENQ), med_Voit = median(n_Voit), med_Coll = median(n_Coll),
                                   mad_Voit = mad(n_Voit, constant=1),    mad_Coll = mad(n_Coll, constant=1)),
                    by = c("uid_ENQ" = "uid_ENQ"))
    
    hdp = mutate(hdp, HDP_Voit = ifelse(n_Voit > med_Voit + mad_Voit, T, F),
                      HDP_Coll = ifelse(n_Coll > med_Coll + mad_Coll, T, F))
    
    return(hdp)
}

init_VEH = function(MEN)
{
    rapport("Préparation de la base Véhicules...")
    
    Veh1 = select(MEN, uid_MEN, Veh1_Typ:Veh1_Pty, PCSMT, PCSMLT, MenTypo, ZoneDens, EnqAnnee, Coeff)
    Veh2 = select(MEN, uid_MEN, Veh2_Typ:Veh2_Pty, PCSMT, PCSMLT, MenTypo, ZoneDens, EnqAnnee, Coeff)
    Veh3 = select(MEN, uid_MEN, Veh3_Typ:Veh3_Pty, PCSMT, PCSMLT, MenTypo, ZoneDens, EnqAnnee, Coeff)
    Veh4 = select(MEN, uid_MEN, Veh4_Typ:Veh4_Pty, PCSMT, PCSMLT, MenTypo, ZoneDens, EnqAnnee, Coeff)
    
    lCol = c("uid_MEN", "Type", "Energie", "Annee", "Puissance", "Propr", "ProprAutre", "Parking", "ParkingType",
             "PCSMT", "PCSMLT", "MenTypo", "ZoneDens", "EnqAnnee", "Coeff")
    
    colnames(Veh1) = lCol ; colnames(Veh2) = lCol ; colnames(Veh3) = lCol ; colnames(Veh4) = lCol
    
    Veh1 = mutate(Veh1, uid_VEH = paste0(uid_MEN, "-V1"))
    Veh2 = mutate(Veh2, uid_VEH = paste0(uid_MEN, "-V2"))
    Veh3 = mutate(Veh3, uid_VEH = paste0(uid_MEN, "-V3"))
    Veh4 = mutate(Veh4, uid_VEH = paste0(uid_MEN, "-V4"))
    
    VEH = rbind(Veh1, Veh2, Veh3, Veh4) %>% select(uid_VEH, Type:Coeff) %>% filter(!is.na(Type))
    remove(Veh1, Veh2, Veh3, Veh4)
    
    VEH$Type    = plyr::revalue(VEH$Type, c("1" = "Tourisme", "2" = "Camping-car", "3" = "Utilitaire",
                                            "4" = "Sans permis"))
    VEH$Energie = plyr::revalue(VEH$Energie, c("1" = "S/plomb", "2" = "Super", "3" = "Diesel", "4" = "Gaz",
                                               "5" = "Électrique", "6" = "Hybride", "7" = NA, "9" = NA))
    VEH$Propr   = plyr::revalue(VEH$Propr, c("1" = "oui", "2" = "non", "9" = NA))
    VEH$ProprAutre = plyr::revalue(VEH$ProprAutre, c("1" ="empl., totale", "2" = "empl., limitée", "3" = "autre"))
    VEH$Parking = plyr::revalue(VEH$Parking, c("1" = "garage", "2" = "rue", "3" = "parking ouvert", "4" = "parking sout.", "9" = NA))
    VEH$ParkingType = plyr::revalue(VEH$ParkingType, c("1" = "interdit", "2" = "gratuit", "3" = "payant", "4" = "payé", "9" = NA))
    
    VEH$PCSMT = etqPCSM(VEH$PCSMT)
    VEH$PCSMLT = etqPCSM(VEH$PCSMLT)
    VEH$MenTypo = etqMenTypo(VEH$MenTypo)
    VEH$ZoneDens = etqZoneDens(VEH$ZoneDens, supprTrFaible = T)
    
    VEH$EnqAnnee = as.integer(VEH$EnqAnnee)
    VEH$Annee = ifelse(VEH$Annee == 9999, NA, VEH$Annee)
    VEH$Age = ifelse(!is.na(VEH$Annee), VEH$EnqAnnee - VEH$Annee, NA)

    VEH = select(VEH, uid_VEH, Type, Energie, Annee, Age, Puissance, Propr:ZoneDens, Coeff)
    
    return(VEH)
}

load_activites_memoryProof = function(PER, DEP, ACT, pasDeTemps = 60)
{
  gc()
  # Segmenter la génération de la table d'activités pour pouvoir lancer le calcul
  # sur un ordinateur doté d'une mémoire limitée
  
  # On va utiliser dsDom now
  if (!"dsDom" %in% colnames(PER))
  {
    PER = densitesZversPER(PER)
  }
  
  PER = select(PER, uid_ENQ, uid_PER, ZT, ZF, Com, dsDom, dsTvl, CoeffRecEnq)
  
  ACT$uid_ENQ = substr(ACT$uid_MEN, 1, 7)
  
  activites = data.frame(heure = NA, uid_PER = NA, ZT.ACT = NA, ZF.ACT = NA,
                         Tache = NA, ZT = NA, dsDom = NA, dsTvl = NA, CoeffRecEnq = NA)
  
  if (file.exists("Data/activites.csv")) { file.remove("Data/activites.csv") }
  write_csv(activites, file = "Data/activites.csv")
  
  
  for(Enq in unique(PER$uid_ENQ))
  {
    cat("\nTable d'activités → Enquête", etqEnq(Enq), "→ ")

    activites_enq = load_activites(PER = filter(PER, uid_ENQ == Enq),
                                   DEP = filter(DEP, uid_ENQ == Enq),
                                   ACT = filter(ACT, uid_ENQ == Enq),
                                   pasDeTemps = pasDeTemps)

    write_csv(activites_enq, file = "Data/activites.csv", append = T)
    
    remove(activites_enq)
    gc()
  }
  
  remove(PER, DEP, ACT)
  
  cat("\nChargement de la base finale")
  
  activites = read_csv("Data/activites.csv")
  
  cat("\nFait !")
  
  return(activites)
}

load_activites = function(PER, DEP, ACT, pasDeTemps = 60)
{
   if (!"dsDom" %in% colnames(PER))
   {
     PER = densitesZversPER(PER)
   }
  
    corresZTZF  = DEP %>% mutate(O_ZT = paste(uid_ENQ, O_ZT)) %>% group_by(ZF = O_ZF) %>% summarize(ZT = first(O_ZT))
    corresZTZF2 = DEP %>% mutate(D_ZT = paste(uid_ENQ, D_ZT)) %>% group_by(ZF = D_ZF) %>% summarize(ZT = first(D_ZT))
    corresZTZF = rbind(corresZTZF, corresZTZF2) %>% group_by(ZF) %>% summarize(ZT = first(ZT)) ; remove(corresZTZF2)
    corresZTZF = corresZTZF %>% mutate(ZT = paste0(substr(ZT, 1, 7), substr(ZT, 9, 12)))
    
    corresZTZF = corresZTZF %>% mutate(ZT = ifelse(substr(ZT, 8,12) == "9999", NA, ZT))
    
    ACT2 = ACT %>% left_join(corresZTZF, by=c("l" = "ZF"))
    
    rapport("Calcul de la table des activités par intervalles de", pasDeTemps, "minutes")
    
    if (nchar(PER$ZT)[1] == 4)
    {
        PER$ZT = paste0(PER$uid_ENQ, PER$ZT)
    }
    
    # On recharge les ZT avec les secteurs 9999 pour identifier quand on ne sait pas
    ACT2$ZT = NULL ; ACT2$ZT.ACT = NULL
    
    ACT2 = ACT2 %>% left_join(corresZTZF, by=c("l" = "ZF")) %>%
        rename(ZT.ACT = ZT, ZF.ACT = l)
    
    listeH = seq(240, 1620, pasDeTemps) # pas de temps entre 4h et 3h du lendemain
    
    ACT.E = left_join(ACT2, select(PER, uid_PER, ZT, dsDom, dsTvl, CoeffRecEnq),
                      by = c("uid_PER" = "uid_PER"))
    activites = data.frame(heure = NA, uid_PER = NA, ZT.ACT = NA, ZF.ACT = NA,
                           Tache = NA, ZT = NA, dsDom = NA, dsTvl = NA, CoeffRecEnq = NA)
    
    b = ui_ProgInit(max(listeH)-180)
    
    for (h in listeH)
    {
        activitesEnCours = filter(ACT.E, h >= heureHHMMtoM(hDeb) & h < heureHHMMtoM(hFin)) %>%
            mutate(heure = h) %>% select(heure, uid_PER, ZT.ACT, ZF.ACT, Tache, ZT, dsDom, dsTvl, CoeffRecEnq)
        activites = rbind(activites, activitesEnCours)
        ui_Prog(b, h-180)
    }
    
    remove(activitesEnCours)
    remove(ACT2, ACT.E, DEP) # problèmes de mémoire
    
    return(activites)
}


# Initialisation des bases et des champs ====

init_base = function(nomDico, nomFic)
{
    dico = read_delim(paste0("Sources/Dictionnaires/", nomDico), ";", trim_ws = T, escape_double = F)
    base = read_fwf(paste0("Sources/", nomFic), col_positions = fwf_widths(dico$Taille, col_names = dico$Libelle),
                   col_types=paste(unlist(dico$Type),collapse=""))
    return(base)
}

init_base_csv = function(nomDico, nomFic, filtrerStd=F, encodingBase = "UTF-8", encodingDico = "UTF-8" )
{
    dico = read_delim(paste0("Sources/Dictionnaires/", nomDico), ";", trim_ws = T,
                      escape_double = F,
                      locale = locale(encoding = encodingDico))
    base = read_delim(paste0("Sources/", nomFic), delim = ";", skip=1, 
                      col_names = dico$Libelle,
                      col_types = paste(unlist(dico$Type),collapse=""),
                      locale = locale(encoding = encodingBase))
    
    if (filtrerStd)
    {
        base = select(base, filter(dico, ConformeCerema=="oui")$Libelle)
    }
    
    return(base)
}

init_CodeLieu = function()
{
    CodeLieu = read.csv("Sources/Dictionnaires/Correspondance INSEE-VC.csv", sep = ";", fileEncoding = "utf8",
                        colClasses = c("character","character","character","character","NULL"))  %>%
        mutate(filtre.Geom = case_when(ENQUETE == "" ~ "N", ENQUETE != "" ~ "O"))
    CodeLieu$ENQUETE = NULL
    colnames(CodeLieu) = c("CodeInsee","EnqLieu","uid_ENQ", "filtre.Geom")
    return(CodeLieu)
}

init_uid_ENQ = function(base, cor_Paris2011 = F)
{
    CodeLieu = init_CodeLieu()
    
    # Les bases SHP nécessitent un traitement différent
    if ("EnqCode" %in% colnames(base)) {
        base = left_join(base, CodeLieu, by = c("EnqCode" = "CodeInsee"))
    }
    if ("ENQUETE" %in% colnames(base)) {
        Ass.Shp = read.csv("Sources/Dictionnaires/Correspondance INSEE-VC.csv", sep = ";",
                           colClasses = c("character","NULL","NULL","character","NULL"))
        base = left_join(base, Ass.Shp, by = c("ENQUETE" = "ENQUETE")) %>%
            left_join(select(CodeLieu, CodeInsee, uid_ENQ), by = c("CODE_INSEE_VC" = "CodeInsee")) %>%
            rename(EnqAnnee = ANNEE)
    }
    
    if (cor_Paris2011) {
        base = mutate(base, EnqAnnee = ifelse(EnqCode == "75056", "2010", EnqAnnee))
    }
    
    base$uid_ENQ = paste0(base$uid_ENQ, base$EnqAnnee)
    base$EnqCode = NULL
    
    return(base)
}

init_codeZF = function(base)
{
    if ("ZF" %in% colnames(base))
    { base$ZF = paste(base$uid_ENQ, toupper(base$ZF)) }
    if ("D_ZF" %in% colnames(base))
    { base$D_ZF = paste(base$uid_ENQ, toupper(base$D_ZF)) }
    if ("O_ZF" %in% colnames(base))
    { base$O_ZF = paste(base$uid_ENQ, toupper(base$O_ZF)) }
    if ("CODE_ZF" %in% colnames(base))
    { base$CODE_ZF = paste(base$uid_ENQ, base$CODE_ZF) }
    if ("ZT" %in% colnames(base))
    { base$ZT = paste0(base$uid_ENQ, base$ZT) }
    if ("Travail_ZF" %in% colnames(base))
    { base$Travail_ZF = paste(base$uid_ENQ, base$Travail_ZF) }
    if ("Travail_ZT" %in% colnames(base))
    { base$Travail_ZT = paste0(base$uid_ENQ, base$Travail_ZT) }
    return(base)
}

init_uid = function(base)
{
    # Composition : Code ENQ > Code Méth. > Code ZF > Code Ménage > Code Individu > Code Depl > Code Traj
      base$uid_MEN = paste0(base$uid_ENQ, base$EnqMeth, base$ZF, base$Ech)
    
    if("idPer" %in% colnames(base))
    { base$uid_PER = paste0(base$uid_ENQ, base$EnqMeth, base$ZF, base$Ech, base$idPer) }
      
    if("idPer" %in% colnames(base) & "idDep" %in% colnames(base))
    { base$uid_DEP = paste0(base$uid_ENQ, base$EnqMeth, base$ZF, base$Ech, base$idPer, base$idDep) }
      
    if("idPer" %in% colnames(base) & "idDep" %in% colnames(base) & "idTrj" %in% colnames(base))
    { base$uid_TRJ = paste0(base$uid_ENQ, base$EnqMeth, base$ZF, base$Ech, base$idPer, base$idDep, base$idTrj) }
      
      return(base)
}

init_filtres = function(PER, DEP, shp_ZF, PGT, afficherTableFiltreGeomAvancé = F)
{
    rapport("Création des filtres...")
    # Une base de filtres doit permettre de nettoyer la base rapidement pour certains usages exigeant d'exclure
    # certains types d'individus.
    
    # On sait que les enquêtes où il n'y a aucune PCS détaillées n'ont aucun champ PCS dét où il n'y a pas de NAs :
    # On va donc faire un summarize avec un champ virtuel = 1 quand la PCS+ est NA
    # si 100%, alors la PCS+ n'est pas dispo pour cette enquête
    PER.ctl = select(PER, uid_ENQ, uid_PER, PCS42S)
    PER.ctl$PCS_Indispo = ifelse(is.na(PER.ctl$PCS42S), 1, 0)
    PER.ctl = group_by(PER.ctl, uid_ENQ) %>% dplyr::summarize(N = n(), NNA = sum(PCS_Indispo)) %>%
        mutate(tauxIndispo = NNA/N) %>%
        mutate(filtre.PCS = ifelse(tauxIndispo == 1, F,T))
    PER.ctl = select(PER.ctl, uid_ENQ, filtre.PCS)
    filtres = PER.ctl
    remove(PER.ctl)
    
    # Création d'un filtre de géométrie disponible
    # Les enquêtes pour lesquelles il y a de la géométrie sont dans shp
    shp.test = st_drop_geometry(shp_ZF) %>% group_by(uid_ENQ) %>% dplyr::summarize(N = n())
    
    # On joint le résultat du test à la base filtre
    filtres = left_join(filtres, shp.test, by=c("uid_ENQ" = "uid_ENQ"))
    filtres$filtre.Shp = ifelse(is.na(filtres$N), F, T)
    filtres = select(filtres, uid_ENQ, filtre.PCS, filtre.Shp)
    
    listeZF = charger_listeZF(shp_ZF, PGT)
    
    # Création du filtre de géométrie avancée
    DEP.test = select(DEP, uid_ENQ, D_ZF, D_ZT) %>%
        mutate(Correspondance = ifelse(D_ZF %in% listeZF, 1, 0)) ; DEP.testb = DEP.test
    ZF_Echec = filter(DEP.test, Correspondance == 0) %>% group_by(D_ZF) %>% dplyr::summarize(N = n())
    DEP.test = group_by(DEP.test, uid_ENQ) %>% dplyr::summarize(N = n(), NOK = sum(Correspondance)) %>%
        mutate(Taux_Corresp = NOK / N) %>% select(uid_ENQ, Taux_Corresp)
    DEP.test$n.ZFPB = 0
    DEP.test$l.ZFPB = ""
    DEP.test$t.ZFPB = ""
    listeEnq=sort(unique(PER$uid_ENQ))
    for(i in 1:length(listeEnq)){
        subsetDEP.test = filter(DEP.testb, uid_ENQ == listeEnq[i] & Correspondance == 0)
        listeZFPb = unique(subsetDEP.test$D_ZF)
        listeTirg = unique(subsetDEP.test$D_ZT)
        if (DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$Taux_Corresp == 0) {
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$n.ZFPB = length(listeZFPb)
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$l.ZFPB = "[tous les secteurs]"
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$t.ZFPB = "[toutes les zones]"
        }
        else{
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$n.ZFPB = length(listeZFPb)
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$l.ZFPB =
                gsub(paste0(listeEnq[i], " "), "", paste(unlist(listeZFPb), collapse = " "))
            DEP.test[which(DEP.test$uid_ENQ == listeEnq[i]),]$t.ZFPB = paste(unlist(listeTirg), collapse = " ")
        }
    }
    
    if(afficherTableFiltreGeomAvancé) {View (DEP.test)}
    
    # On peut facilement déterminer quelles bases sont disponibles avec le full détail des ZF maintenant :
    # il suffit de ne retenir que celles dont
    # - 100% des données sont attribuées OU
    # - les seuls secteurs non attribués sont codés 9999 (autrement dit, hors secteur)
    DEP.test = mutate(DEP.test, filtre.ZF = ifelse(Taux_Corresp == 1 | t.ZFPB == "9999", T, F))
    filtres = left_join(filtres, select(DEP.test, uid_ENQ, filtre.ZF), by=c("uid_ENQ" = "uid_ENQ"))
    
    return(filtres)
}

init_PCSM = function(MEN, PER)
{
    # Process en deux canaux séparés :
    # - Calcul des PCSM précises  là où c'est possible
    # - Calcul de  PCSM "approximatives" là où ce n'est pas possible, sur le modèle de celles de Nantes
    #   dans deux champs différents
    # + 2 niveaux de détail : le rang de 1 à 7 sur l'échelle de Thomas Amossé, et les classes détaillées 1A-7A
    
    # par précaution, pour éviter des problèmes à la jointure
    MEN$PCSM = NULL ; MEN$PCSML = NULL ; MEN$PCSMD = NULL ; MEN$PCSMLD = NULL ; MEN$PCSMT = NULL ; MEN$PCSMLT = NULL
    
    rapport("Préparation des PCS Ménage")
    
    # Extrait de PER qui ne contient que les infos pertinentes
    PERex = select(PER, uid_PER, uid_MEN, PCS42S, Lien)
    
    # Pour faire la différence, lors du couplage, entre un célibataire et une personne associée à une autre n'ayant pas répondu,
    # il va falloir artificiellement remplacer les NAs par des valeurs textuelles (car NA = pas de conjoint.e)
    # Le 00 semble approprié
    # Ainsi : si personne n°1 a répondu et pas personne n°2 → on ne peut pas calculer la PCS ménage
    #         si personne n°1 a répondu et personne n°2 n'existe pas → on sait qu'elle est célibataire
    PERex = mutate(PERex, PCS42S = ifelse(is.na(PCS42S), "00", as.character(PCS42S)))
    
    
    
    # Abréviations
    gr2 = c("10", "21", "22")       # "Petits indépendants" avec exploitant.es agricoles
    gr3 = c("23", "31", "32", "36") # "Cadres" avec chef.fes d'entreprises 10+ personnes
    gr4 = c("41", "46", "47", "48") # Prof Int
    gr5 = c("51", "54", "55", "56") # Empl
    gr6 = c("61", "66", "69")       # Ouvr
    gr8 = as.character(c(80:89))    # Inactif.ves et étudiant.es
    
    # Association des 2 membres du ménage
    # Lien == "1" : personne de référence et Lien == "2" : conjoint⋅e
    # Passage d'un tableau par individu à un tableau par ménage avec deux champs, PCS n°1 et n°2
    PERex = PERex %>%
        mutate(PCS.P = ifelse(Lien == "1", PCS42S, NA),
               PCS.C = ifelse(Lien == "2", PCS42S, NA)) %>%
        group_by(uid_MEN) %>%
        summarize(PCS.P = first(na.omit(PCS.P)), PCS.C = first(na.omit(PCS.C))) %>%
        mutate(PCS.C = ifelse(is.na(PCS.C), "S", as.character(PCS.C)))
    # Le S identifie ici les cas où la personne est célibataire
    # Dans les autres cas (non réponse), la PCS Ménage sera NA
    
    # Croisements conditionnels exacts (d'après le document du CNIS)
    # Il faut évidemment à chaque fois les tester dans les 2 sens (e.g. ouvr. et emp., emp. et ouvr.)
    PERex = PERex %>%
        mutate(PCSMD = case_when(
            # groupe I
            PCS.P %in%   gr3       & PCS.C %in%   gr3            ~ "1A",
            PCS.P %in%   gr3       & PCS.C %in%   gr4            ~ "1B",
            PCS.C %in%   gr3       & PCS.P %in%   gr4            ~ "1B",
            # groupe II
            PCS.P %in%   gr3       & PCS.C %in% c(gr5, gr6)      ~ "2A",
            PCS.C %in%   gr3       & PCS.P %in% c(gr5, gr6)      ~ "2A",
            PCS.P %in%   gr3       & PCS.C %in% c(gr8, "S")      ~ "2B",
            PCS.C %in%   gr3       & PCS.P %in%   gr8            ~ "2B",
            PCS.P %in% c(gr3, gr4) & PCS.C %in%   gr2            ~ "2C",
            PCS.C %in% c(gr3, gr4) & PCS.P %in%   gr2            ~ "2C",
            PCS.P %in%   gr4       & PCS.C %in%   gr4            ~ "2D",
            # groupe III
            PCS.P %in%   gr4       & PCS.C %in% c(gr5, gr6)      ~ "3A",
            PCS.C %in%   gr4       & PCS.P %in% c(gr5, gr6)      ~ "3A",
            PCS.P %in%   gr4       & PCS.C %in% c(gr8, "S")      ~ "3B",
            PCS.C %in%   gr4       & PCS.P %in%   gr8            ~ "3B",
            PCS.P %in%   gr5       & PCS.C %in%   gr5            ~ "3C",
            # groupe IV
            PCS.P %in%   gr2       & PCS.C %in% c(gr2, gr8, "S") ~ "4A",
            PCS.C %in%   gr2       & PCS.P %in% c(gr2, gr8)      ~ "4A",
            PCS.P %in%   gr2       & PCS.C %in% c(gr5, gr6)      ~ "4B",
            PCS.C %in%   gr2       & PCS.P %in% c(gr5, gr6)      ~ "4B",
            # groupe V
            PCS.P %in%   gr5       & PCS.C %in%   gr6            ~ "5A",
            PCS.C %in%   gr5       & PCS.P %in%   gr6            ~ "5A",
            PCS.P %in%   gr6       & PCS.C %in%   gr6            ~ "5B",
            PCS.C %in%   gr6       & PCS.P %in%   gr6            ~ "5B",
            # groupe VI
            PCS.P %in%   gr5       & PCS.C %in% c(gr8, "S")      ~ "6A",
            PCS.C %in%   gr5       & PCS.P %in%   gr8            ~ "6A",
            PCS.P %in%   gr6       & PCS.C %in% c(gr8, "S")      ~ "6B",
            PCS.C %in%   gr6       & PCS.P %in%   gr8            ~ "6B",
            # groupe VII
            PCS.P %in%   gr8       & PCS.C %in% c(gr8, "S")      ~ "7A")) %>%
        # Pour calculer au niveau simple (sans les sous-groupes) il suffit de prendre le 1er caractère
        mutate(PCSM = substr(PCSMD,1,1))
    
    # Part de NA's
    rapport("Échec d'attribution des PCSM :",
            nrow(filter(PERex, is.na(PERex$PCSMD))) / nrow(PERex) * 100,
            "%", info=T)
    
    # Les PCSM simplifiées peuvent être calculées à partir de la PCS8.
    # Plutôt que d'exclure les indépendant.es, je vais marquer d'un 3e caractère les catégos qui vont comporter
    # des ménages qui ne devraient pas s'y trouver.
    
    # Comme pour le cas précédent, les NAs sont remplacés par des 0 pour pouvoir compter les célibataires
    PERex2 = mutate(PER, PCS8 = ifelse(is.na(PCS8), "00", as.character(PCS8))) %>%
        select(uid_PER, uid_MEN, PCS8, Lien)
    
    # Il y a un 0 inutile devant les PCS8
    PERex2 = PERex2 %>% mutate(PCS8 = substr(PCS8,2,2))
    
    # Association des conjoint.es
    PERex2 = PERex2 %>%
        mutate(PCS.P = ifelse(Lien == "1", PCS8, NA),
               PCS.C = ifelse(Lien == "2", PCS8, NA)) %>%
        group_by(uid_MEN) %>%
        summarize(PCS.P = first(na.omit(PCS.P)), PCS.C = first(na.omit(PCS.C))) %>%
        mutate(PCS.C = ifelse(is.na(PCS.C), "S", as.character(PCS.C)))
    
    # Possibilités ; le champ est baptisé PCSML pour "large" (taille de l'échantillon)
    PERex2 = PERex2 %>%
        mutate(PCSMLD = case_when(
            PCS.P  ==    "3"          & PCS.C  ==    "3"                           ~ "1A" ,
            PCS.P  ==    "3"          & PCS.C  ==    "4"                           ~ "1B" ,
            PCS.C  ==    "3"          & PCS.P  ==    "4"                           ~ "1B" ,
            PCS.P  ==    "3"          & PCS.C %in% c("3", "4")                     ~ "2A" ,
            PCS.C  ==    "3"          & PCS.P %in% c("3", "4")                     ~ "2A" ,
            PCS.P  ==    "3"          & PCS.C %in% c("7", "8", "9", "S")           ~ "2B" ,
            PCS.C  ==    "3"          & PCS.P %in% c("7", "8", "9")                ~ "2B" ,
            PCS.P %in% c("3", "4")    & PCS.C %in% c("1", "2")                     ~ "2C?",
            PCS.C %in% c("3", "4")    & PCS.P %in% c("1", "2")                     ~ "2C?",
            PCS.P  ==    "4"          & PCS.C  ==    "4"                           ~ "2D" ,
            PCS.P  ==    "4"          & PCS.C %in% c("5", "6")                     ~ "3A" ,
            PCS.C  ==    "4"          & PCS.P %in% c("5", "6")                     ~ "3A" ,
            PCS.P  ==    "4"          & PCS.C %in% c("7", "8", "9", "S")           ~ "3B" ,
            PCS.C  ==    "4"          & PCS.P %in% c("7", "8", "9", "S")           ~ "3B" ,
            PCS.P  ==    "5"          & PCS.C  ==    "5"                           ~ "3C" ,
            PCS.P %in% c("1", "2")    & PCS.C %in% c("1", "2", "7", "8", "9", "S") ~ "4A?",
            PCS.C %in% c("1", "2")    & PCS.P %in% c("1", "2", "7", "8", "9")      ~ "4A?",
            PCS.P %in% c("1", "2")    & PCS.C %in% c("5", "6")                     ~ "4B?",
            PCS.C %in% c("1", "2")    & PCS.P %in% c("5", "6")                     ~ "4B?",
            PCS.P  ==    "5"          & PCS.C  ==    "6"                           ~ "5A" ,
            PCS.C  ==    "5"          & PCS.P  ==    "6"                           ~ "5A" ,
            PCS.P  ==    "6"          & PCS.C  ==    "6"                           ~ "5B" ,
            PCS.P  ==    "5"          & PCS.C %in% c("7", "8", "9", "S")           ~ "6A" ,
            PCS.C  ==    "5"          & PCS.P %in% c("7", "8", "9")                ~ "6A" ,
            PCS.P  ==    "6"          & PCS.C %in% c("7", "8", "9", "S")           ~ "6B" ,
            PCS.C  ==    "6"          & PCS.P %in% c("7", "8", "9")                ~ "6B" ,
            PCS.P %in% c("7","8","9") & PCS.C %in% c("7", "8", "9", "S")           ~ "7A")) %>%
        # Dans la version non détaillée, on n'a plus cette indication '?'
        mutate(PCSML = substr(PCSMLD,1,1))
    
    # Association à la base MEN
    MEN = left_join(MEN, select(PERex, uid_MEN, PCSM, PCSMD), by=c("uid_MEN" = "uid_MEN")) %>%
        left_join(select(PERex2,uid_MEN, PCSML,PCSMLD),by=c("uid_MEN" = "uid_MEN"))
    
    # Ces valeurs sont des facteurs, pas des chaînes
    MEN$PCSM = as.factor(MEN$PCSM) ; MEN$PCSMD = as.factor(MEN$PCSMD)
    MEN$PCSML= as.factor(MEN$PCSML); MEN$PCSMLD= as.factor(MEN$PCSMLD)
    
    # Là où les deux données sont disponibles, quel est le taux de correspondance
    # entre les PCSM "larges" et les PCSM "exactes" ?
    PER = left_join(PER, select(MEN, uid_MEN, PCSM, PCSML, PCSMD, PCSMLD), by=c("uid_MEN" = "uid_MEN"))
    PER.test = filter(PER, !is.na(PCSM) & !is.na(PCSML))
    PER.test = mutate(PER.test, CorrSim = PCSM  == PCSML,
                      CorrDet = PCSMD == substr(PCSMLD,1,2)) # on retire le ? pour comparer
    
    # Correspondance des PCSSimples
    rapport("Part d'erreurs constatées dans les PCSM simples, niveau 1 :",
            round(nrow(filter(PER.test, CorrSim == F)) / nrow(PER.test) * 100,2), # niveau 1
            "%", info = T)
    rapport("Part d'erreurs constatées dans les PCSM simples, niveau 2 :",
            round(nrow(filter(PER.test, CorrDet == F)) / nrow(PER.test) * 100,2), # niveau 2
            "%", info = T)
    # Résultat : en fait, 1.3% d'erreurs
    # les grands patrons ne sont pas nombreux...
    
    CategErreurs = unique(filter(PER.test, CorrDet == F)$PCSMLD)
    
    # Les erreurs concernent uniquement des indépendant.es mal classé.es, comme prévu :
    rapport("Catégories concernées (où erreurs) :",
            paste(unique(filter(PER.test, CorrDet == F)$PCSMLD), collapse=", "), info=T)
    rapport("Catégories faisant l'objet d'erreurs :",
            paste(unique(filter(PER.test, CorrDet == F)$PCSMD), collapse=", "), info=T)
    
    # Cela signifie que toutes les autres catégories peuvent être considérées comme exactes
    # (les 1. et 2. étant cependant incomplètes)
    # Mais ça va fausser les graphiques en formant un creux artificiel.

    
    nSansPCSM = nrow(filter(PER, is.na(PCSM) & !is.na(PCSML)))
    
    rapport(nrow(filter(PER, is.na(PCSM) & !is.na(PCSML))), "personnes ont une PCSM large mais pas de PCSM détaillée", info=T)

    # Parti pris contestable : là où on dispose de la PCS détaillée, on peut corriger les erreurs
    # du champ PCSML en utilisant le champ PCSM. Cela limite le nombre d'erreurs, mais cela implique
    # que le champ PCSML n'est pas toujours calculé de la même façon (là où c'est possible c'est une
    # PCS ménage exacte, ailleurs elle comporte des erreurs)
    
    PER$PCSM = ifelse(is.na(PER$PCSM) & !is.na(PER$PCSML) & !PER$PCSMLD %in% CategErreurs, PER$PCSML, PER$PCSM)
    PER$PCSMD = ifelse(is.na(PER$PCSMD) & !is.na(PER$PCSMLD) & !PER$PCSMLD %in% CategErreurs, PER$PCSMLD, PER$PCSMD)
    
    rapSansPCSM = round(100 - (nrow(filter(PER, is.na(PCSM) & !is.na(PCSML))) / nSansPCSM * 100), 3)
    
    rapport(rapSansPCSM, "% des cas ont pu être corrigés.", info=T)
    
    # Edit Août 2021 : on va ajouter une typologie comprenant, à part, les PCS 3C, qui constituent un objet
    # d'étude intéressant. On en profite pour reconfigurer les tours des autres groupes.
    
    # Liste des personnes de référence
    PerRef = PER %>% filter(Lien == "1") %>% rename(Activ.Ref = Activ) %>% select(uid_MEN, Activ.Ref)
    # Liste des conjoint⋅es
    PerCjt = PER %>% filter(Lien == "2") %>% rename(Activ.Cjt = Activ) %>% select(uid_MEN, Activ.Cjt)
    
    # On recode pour distinguer 3AB de 3C, et on se sert des deux listes précédentes pour 7e vs 7i
    # À partir de PCSML :
    MEN = MEN %>%
        left_join(PerRef, by = c("uid_MEN" = "uid_MEN")) %>%
        left_join(PerCjt, by = c("uid_MEN" = "uid_MEN")) %>%
        mutate(PCSMLT = case_when(PCSML  == "1"  ~ "1",
                                 PCSML  == "2"  ~ "2",
                                 PCSMLD == "3A" ~ "3AB",
                                 PCSMLD == "3B" ~ "3AB",
                                 PCSMLD == "3C" ~ "3C",
                                 PCSML  == "4"  ~ "4",
                                 PCSML  == "5"  ~ "5",
                                 PCSML  == "6"  ~ "6",
                                 PCSML  == "7" &  Activ.Ref %in% c("12", "21", "22") & is.na(Activ.Cjt) ~ "7e",
                                 PCSML  == "7" & !Activ.Ref %in% c("12", "21", "22") & is.na(Activ.Cjt) ~ "7i",
                                 PCSML  == "7" & !is.na(Activ.Cjt) & 
                                     (Activ.Ref %in% c("12", "21", "22") | Activ.Cjt %in% c("12", "21", "22")) ~ "7e",
                                 PCSML  == "7" & !is.na(Activ.Cjt) & 
                                     !Activ.Ref %in% c("12", "21", "22") & !Activ.Cjt %in% c("12", "21", "22") ~ "7i")) %>%
        select(-c(Activ.Ref, Activ.Cjt))
    
    # À partir de PCSM (version sans erreurs) :
    MEN = MEN %>%
        left_join(PerRef, by = c("uid_MEN" = "uid_MEN")) %>%
        left_join(PerCjt, by = c("uid_MEN" = "uid_MEN")) %>%
        mutate(PCSMT =  case_when(PCSM  == "1"  ~ "1",
                                  PCSM  == "2"  ~ "2",
                                  PCSMD == "3A" ~ "3AB",
                                  PCSMD == "3B" ~ "3AB",
                                  PCSMD == "3C" ~ "3C",
                                  PCSM  == "4"  ~ "4",
                                  PCSM  == "5"  ~ "5",
                                  PCSM  == "6"  ~ "6",
                                  PCSM  == "7" &  Activ.Ref %in% c("12", "21", "22") & is.na(Activ.Cjt) ~ "7e",
                                  PCSM  == "7" & !Activ.Ref %in% c("12", "21", "22") & is.na(Activ.Cjt) ~ "7i",
                                  PCSM  == "7" & !is.na(Activ.Cjt) & 
                                      (Activ.Ref %in% c("12", "21", "22") | Activ.Cjt %in% c("12", "21", "22")) ~ "7e",
                                  PCSM  == "7" & !is.na(Activ.Cjt) & 
                                      !Activ.Ref %in% c("12", "21", "22") & !Activ.Cjt %in% c("12", "21", "22") ~ "7i")) %>%
        select(-c(Activ.Ref, Activ.Cjt))
    
    rapport("Parmi les", nrow(filter(MEN, PCSML == "7")), "ménages de PCSML 7,",
            nrow(filter(MEN, PCSMT == "7e")), "ont été classés en 7e et",
            nrow(filter(MEN, PCSMT == "7i")), "ont été classés en 7i ;",
            nrow(filter(MEN, PCSML == "7" & is.na(PCSMT))), "cas de NA anormaux.", info=T)
    
    MEN$PCSMT= as.factor(MEN$PCSMT); MEN$PCSMLT= as.factor(MEN$PCSMLT)
    
    # On retourne MEN avec les champs supplémentaires : PCSM, PCSMD, PCSML, PCSMLD, PCSMT, PCSMLT.
    return(MEN)
}

init_typoMen = function(MEN, PER)
{
    rapport("Typologies de ménages...")
    MEN$MenTypo = NULL ; MEN$MenCouple = NULL ; MEN$MenEnfants = NULL ; MEN$MenBebe = NULL
    
    # Par convention, je vais estimer que les enfants adultes sont des membres de la famille comme les autres.
    # Je précise le cas des enfants de moins de 16 ans (âge complètement arbitraire...) qui sont potentiellement
    # beaucoup moins autonomes, ce qui va avoir un impact sur mes ménages.
    
    npMEN = PER %>%
        mutate(b_Ref  = ifelse(Lien == "1"                                  ,1,0),
               b_Conj = ifelse(Lien == "2"                                  ,1,0),
               b_Enft = ifelse(Age < 16                                     ,1,0),
               b_Bebe = ifelse(Age < 4                                      ,1,0),
               b_Colc = ifelse(Lien == "4"                                  ,1,0),
               b_Faml = ifelse(Lien == "5" | Lien == "3" & Age >= 16,1,0),
               b_Autr = ifelse(Lien == "6" | Lien == "7"                ,1,0)) %>%
        group_by(uid_MEN) %>% dplyr::summarize(n_Ref = sum(b_Ref), n_Conj = sum(b_Conj),
                                               n_Enft = sum(b_Enft), n_Bebe = sum(b_Bebe),
                                               n_Colc = sum(b_Colc), n_Faml = sum(b_Faml),
                                               n_Autr = sum(b_Autr),
                                               nMEN = n(), uid_ENQ = first(uid_ENQ))
    npMEN = left_join(npMEN, select(MEN, uid_MEN, Coeff), by=c("uid_MEN" = "uid_MEN"))
    
    # table(npMEN$n_Conj) ; table(npMEN$n_Enft) ; table(npMEN$n_Faml)
    
    npMEN = npMEN %>% mutate(MenTypo = case_when(n_Conj == 0 & n_Enft == 0 & n_Colc == 0 & n_Faml + n_Autr == 0 ~ "10",
                                                 n_Conj == 0 & n_Enft >  0 & n_Colc == 0 & n_Faml + n_Autr == 0 ~ "11",
                                                 n_Conj == 0 & n_Enft == 0 & n_Colc == 0 & n_Faml + n_Autr >  0 ~ "12",
                                                 n_Conj == 0 & n_Enft >  0 & n_Colc == 0 & n_Faml + n_Autr >  0 ~ "13",
                                                 n_Conj == 1 & n_Enft == 0 & n_Colc == 0 & n_Faml + n_Autr == 0 ~ "20",
                                                 n_Conj == 1 & n_Enft >  0 & n_Colc == 0 & n_Faml + n_Autr == 0 ~ "21",
                                                 n_Conj == 1 & n_Enft == 0 & n_Colc == 0 & n_Faml + n_Autr >  0 ~ "22",
                                                 n_Conj == 1 & n_Enft >  0 & n_Colc == 0 & n_Faml + n_Autr >  0 ~ "23",
                                                 n_Colc >  0                                                    ~ "30",
                                                 T                                                              ~ "99"),
                             MenCouple = ifelse(n_Conj>0, T, F),
                             MenEnfants= ifelse(n_Enft>0, T, F),
                             MenBebe = ifelse(n_Bebe>0, T, F))

    # On renvoie aux ménages
    MEN = left_join(MEN, select(npMEN, uid_MEN, MenTypo, MenCouple, MenEnfants, MenBebe), by=c("uid_MEN" = "uid_MEN"))
    
    return(MEN)
}

init_nivEtu = function(MEN, PER)
{
    rapport("Calcul de la variable 'Bourdieu'")
    # edit 12 mars 2021 : a été modifié, car prendre en compte le nombre de personnes provoquait trop 
    # d'irrégularités morphologiques liés aux couples de célibataires
    
    PER2 = PER %>% mutate(
        etu1 = ifelse(NivEtu %in% c("00", "10", "20") & Lien != "3", 1, 0),
        etu2 = ifelse(NivEtu %in% c("30", "31", "32") & Lien != "3", 1, 0),
        etu3 = ifelse(NivEtu %in% c("40", "41")       & Lien != "3", 1, 0),
        etu4 = ifelse(NivEtu %in% c("45", "50")       & Lien != "3", 1, 0),
        etuN = ifelse(is.na(NivEtu),                                     1, 0)) %>%
        group_by(uid_MEN) %>% summarize(etu1 = sum(etu1), etu2 = sum(etu2), etu3 = sum(etu3), etu4 = sum(etu4),
                                        nNAs = sum(etuN)) %>%
        mutate(n = etu1 + etu2 + etu3 + etu4) %>%
        mutate(MenNivEtu = case_when(etu4 >= 1 ~ "4",
                                     etu3 >= 1 ~ "3",
                                     etu2 >= 1 ~ "2",
                                     etu1 >= 1 ~ "1",
                                     T         ~ "9"))
    MEN = left_join(MEN, PER2, by=c("uid_MEN" = "uid_MEN"))
    return(MEN)
}

init_nEnfants = function(MEN, PER)
{
    # Nombre d'enfants
    PER2 = PER %>% mutate(enfant = ifelse(Lien == "3" | Age <16, 1, 0)) %>%
        group_by(uid_MEN) %>% summarize(EnfantsN = sum(enfant))
    MEN = left_join(MEN, PER2, by=c("uid_MEN" = "uid_MEN"))
    # barplot(wtd.table(MEN$EnfantsN, weights=MEN$Coeff), main="Nombre d'enfants par ménage")
    
    # En facteur, en compressant les familles nombreuses
    MEN$EnfantsN = ifelse(MEN$EnfantsN > 3, "4+", as.character(MEN$EnfantsN)) %>% as.factor()

    return(MEN)
}



init_z_com = function(shp_COM)
{
    rapport("Lecture du zonage en aires d'attraction")
    rapport("Chargement des fichiers extérieurs : Zonage attraction, Typologie de densité (insee).", info=T)
    
    # Zonage en aires d'attraction
    z_AAV = read.csv("Sources/Mailles/Zonage Attraction.csv", sep=";", encoding="latin1")
    l_AAV = read.csv("Sources/Mailles/Aires Attraction.csv", sep=";", encoding="latin1")
    l_AAV$LIBAAV2020 = NULL # pour éviter l'overlap avec la jointure
    z_AAV = z_AAV %>% left_join(l_AAV, by=c("AAV2020" = "AAV2020")) ; remove(l_AAV)
    z_AAV$TypeAAV = paste0(z_AAV$TAAV2017, z_AAV$CATEAAV2020) %>% as.factor()
    
    # Il va nous falloir les arrondissements de Paris (à rajouter manuellement, c'est sale !)
    codgeo = as.character(c(75101:75120))
    libgeo = paste0("Paris ", c(1:20), rep("e Arrondissement", 20))
    aav2020 = rep("001", 20) ; libaav2020 = rep("Paris", 20) ; cateaav2020 = rep("11", 20) ;
    dep = rep("75", 20) ; reg = rep("11", 20) ; taav2017 = rep("5", 20) ; tdaav2017 = rep("50", 20)
    nbcom = rep("1929", 20) ; typeaav = rep("511", 20)
    paris = data.frame(CODGEO = codgeo, LIBGEO = libgeo, AAV2020 = aav2020, LIBAAV2020 = libaav2020,
                       CATEAAV2020 = cateaav2020, DEP = dep, REG = reg, TAAV2017 = taav2017, TDAAV2017 = tdaav2017,
                       NB_COM = nbcom, TypeAAV = typeaav)
    z_AAV = rbind(z_AAV, paris) %>% filter(CODGEO != "75056")
    
    # Il faut modifier z_AAV pour ne pas avoir de soucis avec la Corse et les arrondissements
    z_AAV = mutate(z_AAV, CODGEO = ifelse(substr(CODGEO,1,2) == "2A", paste0("20", substr(CODGEO,3,5)), CODGEO)) %>%
        mutate(CODGEO = ifelse(substr(CODGEO,1,2) == "2B", paste0("20", as.character(as.integer(substr(CODGEO,3,5))+500)), CODGEO))
    arrLyon = as.character(c(69381:69389)) ; arrMars = as.character(c(13201:13216))
    z_AAV = z_AAV %>% mutate(CODGEO = ifelse(CODGEO %in% arrLyon, "69123", CODGEO),
                             CODGEO = ifelse(CODGEO %in% arrMars, "13055", CODGEO))
    
    # On récupère le fichier de la typologie de densité
    GCD = read.csv("Sources/grille_densite_2020_agrege.csv", sep=";", encoding="latin1") %>% # fichier remanié (en-têtes)
        mutate(COM = ifelse(substr(COM,1,2) == "2A", paste0("20", substr(COM,3,5)), COM)) %>%
        mutate(COM = ifelse(substr(COM,1,2) == "2B",
                            paste0("20", as.character(as.integer(substr(COM,3,5))+500)), COM)) %>%
        select(COM:DENS)
    
    # Comme pour la typo AAV, il nous faut créer les arrondissements parisiens...
    com = as.character(c(75101:75120))
    comlib = paste0("Paris ", c(1:20), rep("e Arrondissement", 20))
    dens = rep("1", 20)
    paris = data.frame(COM = com, COMLIB = comlib, DENS = dens)
    GCD = rbind(GCD, paris) %>% filter(COM != "75056")
    
    # Création de z_com à partir de z_iris qui contient la population
    z_iris = read.csv("Sources/IRIS population.csv", sep=";") %>%
        mutate(COM = ifelse(COM %in% arrLyon, "69123", COM),
               COM = ifelse(COM %in% arrMars, "13055", COM)) %>%
        mutate(COM = ifelse(substr(COM,1,2) == "2A", paste0("20", substr(COM,3,5)), COM)) %>%
        mutate(COM = ifelse(substr(COM,1,2) == "2B",
                            paste0("20", as.character(as.integer(substr(COM,3,5))+500)), COM)) %>%
        group_by(COM) %>% summarize_if(is.numeric, sum)
    z_iris = z_iris %>% right_join(shp_COM, by=c("COM" = "insee"))
    z_com = st_as_sf(z_iris) ; remove(z_iris)
    z_com$densite = as.numeric(z_com$P15_POP / (st_area(z_com) / 10^6))
    z_com$surface = as.numeric(st_area(z_com) / 10^6)
    z_com = z_com %>% left_join(z_AAV, by=c("COM" = "CODGEO"))
    rapport("Communes sans AAV :", paste(filter(z_com, is.na(AAV2020))$COM, collapse=", "), info=T)
    
    # Jointure avec z_com
    z_com = left_join(z_com, select(GCD, COM, DENS), by=c("COM" = "COM"))
    
    # 3 indicateurs différents :
    # - l'aire urbaine en 6 postes : de Paris à aucune (voir ensuite s'il est judicieux de distinguer 50k-200k et 200k-700k)
    # - le statut en 5 postes : centre (commune centre ou voisines), centre 2dr, couronne, hors couronne
    # - les 4 classes de densité de l'Insee
    
    return(z_com)
}


init_typoCom = function(MEN, shp_COM)
{
    if (!"sf" %in% class(MEN)) { stop("Pas de données géométriques dans MEN !") }
    
    z_com = init_z_com(shp_COM)
    
    # On crée un subset avec ces trois champs
    typoCom = select(z_com, COM, AAV2020, TAAV2017, CATEAAV2020, DENS) %>%
        rename(ZoneRang = TAAV2017, ZonePosi = CATEAAV2020, ZoneDens = DENS, AAV = AAV2020) %>%
        mutate(ZoneRang = as.factor(as.character(ZoneRang)), ZonePosi = as.factor(as.character(ZonePosi)), ZoneDens = as.factor(as.character(ZoneDens)),
               AAV = as.factor(AAV))
    
    # On vérifie qu'on n'a égaré personne
    rapport("Communes non classées :", paste(filter(typoCom, is.na(ZoneRang))$COM, collapse=", "), info=T)

    # On agrège à la base Ménage, avec deux petites conversions liées à des communes nouvelles
    # On prend soin de conserver les valeurs des champs si elles sont déjà là : c'est le cas pour
    # les EMDs (qui fournissent ces variables sans fournir le code commune qui permettrait de les trouver
    comNouv = tibble(COM = c("21507", "21213"),
                     AAV = c("028", "028"),
                     ZoneRang = c("3", "3"),
                     ZonePosi = c("20", "20"),
                     ZoneDens = c("3", "2"),
                     geometry = c(NULL, NULL))
    typoCom = rbind(typoCom)
    
    MEN = left_join(MEN, st_drop_geometry(typoCom), by=c("Com" = "COM"), suffix = c(".init", ".corr")) %>%
        mutate(ZoneRang = ifelse(is.na(ZoneRang.init), as.character(ZoneRang.corr), as.character(ZoneRang.init)),
               ZonePosi = ifelse(is.na(ZonePosi.init), as.character(ZonePosi.corr), as.character(ZonePosi.init)),
               ZoneDens = ifelse(is.na(ZoneDens.init), as.character(ZoneDens.corr), as.character(ZoneDens.init))) %>%
        mutate(ZoneRang = factor(ZoneRang, levels = c("0", "1", "2", "3", "4", "5")),
               ZonePosi = factor(ZonePosi, levels = c("11", "12", "13", "20", "30")),
               ZoneDens = factor(ZoneDens, levels = c("1", "2", "3", "4")))
    
    rapport("Communes non classées dans MEN :", paste(unique(filter(MEN, is.na(ZoneRang))$Com), collapse=", "), info=T)
    
    save(typoCom, file="Data/typoCom.rds")
    rapport("Table typoCom sauvée pour usage ultérieur", info=T)
    
    return(MEN)
}

init_patternsAct = function (PER, ACT, tableZF2Com)
{
    if (!file.exists("Data/typoCom.rds"))
    { stop("Il est nécessaire de charger typoCom avant d'utiliser cette fn : init_typoCom") }
    
    rapport("Préparation de la caractérisation des ZT pour caractériser les patterns.")
    
    # Dans un premier temps, on identifie la typo ego-centrée
    ACT2 = ACT %>%
        mutate(EspaceAct=case_when(substr(Tache, 1,1) == "0"                  ~ "DOMICILE",
                                   Tache == "540"                             ~ "VISITE",
                                   substr(Tache, 1,1) == "1" | Tache == "810" ~ "TRAVAIL", 
                                   Tache == "220" | Tache == "260"            ~ "ECOLE PRIM.",
                                   Tache == "230" | Tache == "270"            ~ "ECOLE COLL.",
                                   Tache == "240" | Tache == "280"            ~ "ECOLE LYC.",
                                   Tache == "250" | Tache == "290"            ~ "UNIVERSITE",
                                   Tache == "960" | Tache == "970"            ~ "ETUDES",
                                   Tache == "2x0"                             ~ "ETUDES",
                                   Tache == "330" | Tache == "340"            ~ "COMMERCE",
                                   Tache == "320"                             ~ "CTR. COMM.",
                                   Tache == "410"                             ~ "SANTE",
                                   Tache == "420" | Tache == "430"            ~ "SERVICES",
                                   Tache == "510"                             ~ "LOISIRS",
                                   Tache %in% c("521", "601", "701")          ~ "ESPACE PUBLIC", # 991 retiré ici
                                   Tache == "530"                             ~ "RESTAURANT",
                                   T                                          ~ "NA")) %>%
        mutate(EspaceAct = ifelse(EspaceAct == "NA", NA, EspaceAct)) %>%
        left_join(select(PER, uid_PER, ZF, Travail_ZF), by=c("uid_PER" = "uid_PER")) %>%
        rename(Ego.Res = ZF, Ego.Tvl = Travail_ZF) %>%
        mutate(Ego.Res = paste(substr(uid_PER, 1, 7), Ego.Res),
               Ego.Tvl = paste(substr(uid_PER, 1, 7), Ego.Tvl)) %>%
        mutate(TypoEgo = case_when(EspaceAct == "DOMICILE" ~ "DOMICILE",
                                   l == Ego.Res            ~ "VOISINAGE",
                                   l == Ego.Tvl            ~ "ACTIVITE",
                                   T                       ~ "AUTRE")) %>%
        mutate(TypoEgo = ifelse(is.na(l), NA, TypoEgo)) %>%
        select(uid_PER:EspaceAct, TypoEgo)
    
    # On joint avec la tabe de correspondance ZF/communes
    ACT2 = ACT2 %>% left_join(select(tableZF2Com, ZF, Com), by = c("l" = "ZF"))
    
    load("Data/typoCom.rds")
    
    # Combien de communes dans ACT manquantes ?
    ACT2 = left_join(ACT2, typoCom, by=c("Com" = "COM")) %>%
        rename(AAV = AAV, Rang = ZoneRang, Pos = ZonePosi, Dens = ZoneDens)
    rapport(round(nrow(filter(ACT2, !is.na(Com) & is.na(AAV)))/nrow(filter(ACT2, !is.na(Com)))*100,4),
            "% des communes n'ont pu être localisées dans le fichier des AAV de l'Insee", info=T)
    # Vérifier qu'il s'agit bien d'erreurs isolées et liées à des communes étrangères
    rapport("Communes sans association :", info=T)
    t = as.data.frame(table(filter(ACT2, !is.na(Com) & is.na(AAV))$Com))
    t = t[order(t$Freq, decreasing = T),]
    t = filter(t, Freq/nrow(ACT2)*100 > .01)
    rapport(paste0(t$Var1, " (", t$Freq, " cas)\n", collapse = ""), "etc. (<.01% des activités)", extrait=T)
    
    # TODO: Réattribuer aires de l'EMP
    
    # Simplification des typologies
    ACT2 = mutate(ACT2, TypoCtr = case_when(Pos %in% c("11", "12") & Rang %in% c("4", "5") ~ "CENTRE_1",
                                            Pos %in% c("11", "12", "13")                   ~ "CENTRE_2",
                                            Dens %in% c("1", "2")                          ~ "PERIPH",
                                            Dens %in% c("3", "4")                          ~ "RURAL")) %>%
        select(uid_PER:Com, EspaceAct, TypoEgo, TypoCtr)
    ACT2$EspaceAct = as.factor(ACT2$EspaceAct)
    ACT2$TypoAct = plyr::revalue(ACT2$EspaceAct, c("CTR. COMM."  = "COMMERCE",
                                                   "ECOLE COLL." = "ETUDES",
                                                   "ECOLE LYC."  = "ETUDES",
                                                   "ECOLE PRIM." = "ETUDES",
                                                   "RESTAURANT"  = "COMMERCE",
                                                   "SANTE"       = "SERVICES",
                                                   "UNIVERSITE"  = "ETUDES"))
    
    # Calcul des trajectoires
    schemas = ACT2 %>% group_by(uid_PER) %>%
        summarize(schCtr = paste(retirerDoubles(TypoCtr), collapse = " → "),
                  schAct = paste(retirerDoubles(as.character(TypoAct)), collapse = " → "),
                  schEgo = paste(retirerDoubles(TypoEgo), collapse = " → "))
    schCtr = schemas %>% group_by(schCtr) %>% summarize(n = n()) %>% tab_Tri(2, rev = T)
    schAct = schemas %>% group_by(schAct) %>% summarize(n = n()) %>% tab_Tri(2, rev = T)
    schEgo = schemas %>% group_by(schEgo) %>% summarize(n = n()) %>% tab_Tri(2, rev = T)
    
    # Jonction du résultat avec PER
    PER = PER %>% left_join(schemas, by=c("uid_PER" = "uid_PER"))
    
    # ZoneTypoCentre aide à faire des graphiques
    PER = mutate(PER, ZoneTypoCtr =
                            case_when(ZonePosi  %in% c("11", "12") & ZoneRang %in% c("4", "5") ~ "CENTRE_1",
                                      ZonePosi  %in% c("11", "12", "13")                          ~ "CENTRE_2",
                                      ZoneDens %in% c("1", "2")   & ZoneRang %in% c("4", "5") ~ "PERIPH_1",
                                      ZoneDens %in% c("1", "2")                                  ~ "PERIPH_2",
                                      ZoneDens %in% c("3", "4")                                  ~ "RURAL"))
    
    return(PER)
}

init_typoDep = function(PER, hdp, ACT, TRJ, DEP)
{
    rapport("Calcul des variables typologiques d'après les déplacements agrégés")
    
    # Journée de travail, d'étude, ou chômée ?
    PER$typoJo = NA
    PER$typoJo = ifelse(grepl("ETUDES", PER$schAct) & PER$Activ %in% c(21, 22), "ETUD", PER$typoJo)
    PER$typoJo = ifelse(grepl("TRAVAIL",PER$schAct)                           , "TRAV", PER$typoJo)
    PER$typoJo = ifelse(!is.na(PER$schAct) & is.na(PER$typoJo)                , "CHOM", PER$typoJo)
    PER$typoJo = as.factor(PER$typoJo)
    
    # Type de déplacement ou de boucle ?
    PER = PER %>%
        mutate(bclLongueur = sapply(strsplit(schAct, " → "), length)) %>%
        mutate(bclComplexe = case_when(bclLongueur == 3                  ~ "SIMPLE",
                                       bclLongueur > 3 & bclLongueur < 6 ~ "COMPOSÉE",
                                       bclLongueur > 5                   ~ "COMPLEXE")) %>%
        mutate(bclComplexe = as.factor(bclComplexe))
    PER$bclComplexe = factor(PER$bclComplexe, levels = c("SIMPLE", "COMPOSÉE", "COMPLEXE"))
    
    # Ordre de fréquentation des commerces, loisirs, espaces publics ?
    PER = PER %>%
        mutate(indexTvl1 =                   sapply(strsplit(schAct, " → "), match, x = "TRAVAIL"),
               indexEtu1 =                   sapply(strsplit(schAct, " → "), match, x = "ETUDES"),
               indexCom1 =                   sapply(strsplit(schAct, " → "), match, x = "COMMERCE"),
               indexLsr1 =                   sapply(strsplit(schAct, " → "), match, x = "LOISIRS"),
               indexPub1 =                   sapply(strsplit(schAct, " → "), match, x = "ESPACE PUBLIC"),
               indexTvl2 = bclLongueur + 1 - sapply(sapply(strsplit(schAct, " → "), rev), match, x = "TRAVAIL"),
               indexEtu2 = bclLongueur + 1 - sapply(sapply(strsplit(schAct, " → "), rev), match, x = "ETUDES"),
               indexCom2 = bclLongueur + 1 - sapply(sapply(strsplit(schAct, " → "), rev), match, x = "COMMERCE"),
               indexLsr2 = bclLongueur + 1 - sapply(sapply(strsplit(schAct, " → "), rev), match, x = "LOISIRS"),
               indexPub2 = bclLongueur + 1 - sapply(sapply(strsplit(schAct, " → "), rev), match, x = "ESPACE PUBLIC")) %>%
        mutate(posiCom = case_when(typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 < indexTvl1 & indexCom2 < indexTvl1 ~ "AVANT",
                                   typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 > indexTvl2 & indexCom2 > indexTvl2 ~ "APRES",
                                   typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 > indexTvl1 & indexCom2 < indexTvl2 ~ "PENDANT",
                                   typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 < indexTvl1 & indexCom2 > indexTvl2 ~ "AUTOUR",
                                   typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 < indexTvl1 & indexCom2 < indexTvl2 ~ "AVANT/PDT",
                                   typoJo == "TRAV" & !is.na(indexCom1) & indexCom1 > indexTvl1 & indexCom2 > indexTvl2 ~ "APRES/PDT",
                                   #typoJo == "TRAV" &  is.na(indexCom1) ~ "PAS DE COM.",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 < indexEtu1 & indexCom2 < indexEtu1 ~ "AVANT",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 > indexEtu2 & indexCom2 > indexEtu2 ~ "APRES",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 > indexEtu1 & indexCom2 < indexEtu2 ~ "PENDANT",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 < indexEtu1 & indexCom2 > indexEtu2 ~ "AUTOUR",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 < indexEtu1 & indexCom2 < indexEtu2 ~ "AVANT/PDT",
                                   typoJo == "ETUD" & !is.na(indexCom1) & indexCom1 > indexEtu1 & indexCom2 > indexEtu2 ~ "APRES/PDT"#,
                                   #typoJo == "ETUD" &  is.na(indexCom1) ~ "PAS DE COM."
        ),
        posiLsr = case_when(typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 < indexTvl1 & indexLsr2 < indexTvl1 ~ "AVANT",
                            typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 > indexTvl2 & indexLsr2 > indexTvl2 ~ "APRES",
                            typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 > indexTvl1 & indexLsr2 < indexTvl2 ~ "PENDANT",
                            typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 < indexTvl1 & indexLsr2 > indexTvl2 ~ "AUTOUR",
                            typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 < indexTvl1 & indexLsr2 < indexTvl2 ~ "AVANT/PDT",
                            typoJo == "TRAV" & !is.na(indexLsr1) & indexLsr1 > indexTvl1 & indexLsr2 > indexTvl2 ~ "APRES/PDT",
                            #typoJo == "TRAV" &  is.na(indexLsr1) ~ "NEANT",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 < indexEtu1 & indexLsr2 < indexEtu1 ~ "AVANT",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 > indexEtu2 & indexLsr2 > indexEtu2 ~ "APRES",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 > indexEtu1 & indexLsr2 < indexEtu2 ~ "PENDANT",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 < indexEtu1 & indexLsr2 > indexEtu2 ~ "AUTOUR",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 < indexEtu1 & indexLsr2 < indexEtu2 ~ "AVANT/PDT",
                            typoJo == "ETUD" & !is.na(indexLsr1) & indexLsr1 > indexEtu1 & indexLsr2 > indexEtu2 ~ "APRES/PDT"#,
                            #typoJo == "ETUD" &  is.na(indexLsr1) ~ "NEANT"
        ),
        posiPub = case_when(typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 < indexTvl1 & indexPub2 < indexTvl1 ~ "AVANT",
                            typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 > indexTvl2 & indexPub2 > indexTvl2 ~ "APRES",
                            typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 > indexTvl1 & indexPub2 < indexTvl2 ~ "PENDANT",
                            typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 < indexTvl1 & indexPub2 > indexTvl2 ~ "AUTOUR",
                            typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 < indexTvl1 & indexPub2 < indexTvl2 ~ "AVANT/PDT",
                            typoJo == "TRAV" & !is.na(indexPub1) & indexPub1 > indexTvl1 & indexPub2 > indexTvl2 ~ "APRES/PDT",
                            #typoJo == "TRAV" &  is.na(indexPub1) ~ "NEANT",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 < indexEtu1 & indexPub2 < indexEtu1 ~ "AVANT",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 > indexEtu2 & indexPub2 > indexEtu2 ~ "APRES",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 > indexEtu1 & indexPub2 < indexEtu2 ~ "PENDANT",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 < indexEtu1 & indexPub2 > indexEtu2 ~ "AUTOUR",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 < indexEtu1 & indexPub2 < indexEtu2 ~ "AVANT/PDT",
                            typoJo == "ETUD" & !is.na(indexPub1) & indexPub1 > indexEtu1 & indexPub2 > indexEtu2 ~ "APRES/PDT"#,
                            #typoJo == "ETUD" &  is.na(indexPub1) ~ "NEANT"
        )) %>%
        select(-indexTvl1, -indexTvl2, -indexEtu1, -indexEtu2, -indexCom1, -indexCom2, -indexLsr1, -indexLsr2, -indexPub1, -indexPub2)
    levPosi = c("AVANT", "AVANT/PDT", "PENDANT", "APRES/PDT", "APRES", "AUTOUR")
    PER$posiCom     =    factor(PER$posiCom, levels=levPosi)
    PER$posiLsr     =    factor(PER$posiLsr, levels=levPosi)
    PER$posiPub     =    factor(PER$posiPub, levels=levPosi)
    
    # Type de trajectoire par rapport au centre de l'agglomération ?
    p_PasDePendule = c("CENTRE_1", "CENTRE_2", "PERIPH", "RURAL") # différencier si domicile / voisinage ou autre
    p_Centre       = c("PERIPH → CENTRE_1 → PERIPH", "PERIPH → CENTRE_2 → PERIPH",
                       "RURAL → CENTRE_1 → RURAL", "RURAL → CENTRE_2 → RURAL",
                       "RURAL → PERIPH → RURAL")
    p_CentreDouble = c("PERIPH → CENTRE_1 → PERIPH → CENTRE_1 → PERIPH", "PERIPH → CENTRE_2 → PERIPH → CENTRE_2 → PERIPH",
                       "RURAL → CENTRE_1 → RURAL → CENTRE_1 → RURAL", "RURAL → CENTRE_2 → RURAL → CENTRE_2 → RURAL",
                       "RURAL → PERIPH → RURAL → PERIPH → RURAL")
    p_Periph       = c("CENTRE_1 → PERIPH → CENTRE_1", "CENTRE_1 → RURAL → CENTRE_1",
                       "CENTRE_2 → PERIPH → CENTRE_2", "CENTRE_2 → RURAL → CENTRE_2",
                       "PERIPH → RURAL → PERIPH")
    p_PeriphDouble = c("CENTRE_1 → PERIPH → CENTRE_1 → PERIPH → CENTRE_1", "CENTRE_1 → RURAL → CENTRE_1 → RURAL → CENTRE_1",
                       "CENTRE_2 → PERIPH → CENTRE_2 → PERIPH → CENTRE_2", "CENTRE_2 → RURAL → CENTRE_2 → RURAL → CENTRE_2",
                       "PERIPH → RURAL → PERIPH → RURAL → PERIPH")
    PER = PER %>%
        mutate(pendule = case_when(schCtr %in% p_Centre       ~ "VERS CENTRE",
                                   schCtr %in% p_CentreDouble ~ "VERS CENTRE",
                                   schCtr %in% p_Periph       ~ "VERS PERIPH",
                                   schCtr %in% p_PeriphDouble ~ "VERS PERIPH",
                                   schCtr %in% p_PasDePendule ~ "PAS DE PENDULE",
                                   !is.na(schCtr)             ~ "COMPLEXE"))
    PER$pendule     =    factor(PER$pendule, levels=c("PAS DE PENDULE", "VERS CENTRE", "VERS PERIPH", "COMPLEXE"))
    
    # Recours aux modes doux, à la voiture, au vélo ?
    
    # Edit du 28 octobre 2022 : on va procéder depuis TRJ pour que ce soit plus précis
    # (meilleure prise en compte du multimodal).
    
    # Il faut un uid pour que dplyr puisse manipuler TRJ :
    TRJ_Modes = TRJ %>%
        mutate(uid_TRJ = paste0(uid_DEP, ":", idTrj), nTj = 1)
    
    # Il y a des doublons. Faut les purger :
    TRJ_dbl = TRJ_Modes %>%
            group_by(uid_TRJ) %>%
            summarize(n = n(), uid_PER = first(uid_PER)) %>%
            filter(n>1)
    TRJ_Modes = TRJ_Modes %>% filter(!uid_TRJ %in% TRJ_dbl$uid_TRJ)
    
    # Correspondance :
    corresModes = c("01" = "marche",
                    "10" = "voiture",
                    "11" = "vélo", "12" = "vélo", "17" = "vélo", "18" = "vélo",
                    "10x" = "vélo", "11x" = "vélo",
                    "13" = "drm", "14" = "drm", "15" = "drm", "16" = "drm", "19" = "drm", "20" = "drm",
                    "21" = "voiture", "22" = "voiture",
                    "31" = "bus", "32" = "tram", "33" = "métro", "34" = "bus", "37" = "bus",
                    "38" = "bus", "39" = "bus",
                    "41" = "car", "42" = "car", "43" = "car",
                    "51" = "tgv", "52" = "train", "53" = "train", "54" = "train",
                    "61" = "taxi", "62" = "taxi",
                    "71" = "bus",
                    "81" = "utilitaire", "82" = "utilitaire",
                    "91" = "bateau", "92" = "avion",
                    "93" = "trottoir", "94" = "trottoir", "96" = "trottoir", "97" = "trottoir",
                    "95" = "utilitaire")
    
    # On simplifie la nomenclature :
    TRJ_Modes$Mode = plyr::revalue(TRJ_Modes$Mode, corresModes)
    
    # On crée une table qui présente simplement le recours modal :
    TRJ_Modes = TRJ_Modes %>%
        pivot_wider(names_from = Mode, values_from = nTj, names_prefix = "Mode_") %>%
        group_by(uid_PER) %>%
        summarise(modes_marche  = ifelse(sum(O_Mch, na.rm=T) + sum(D_Mch, na.rm=T) >0, "oui", "non"),
                  modes_voiture = ifelse(1 %in% Mode_voiture, "oui", "non"),
                  modes_vélo    = ifelse(1 %in% Mode_vélo, "oui", "non"),
                  modes_drm     = ifelse(1 %in% Mode_drm, "oui", "non"),
                  modes_tc_route= ifelse(1 %in% Mode_bus | 1 %in% Mode_car, "oui", "non"),
                  modes_tc_light= ifelse(1 %in% Mode_tram | 1 %in% Mode_métro, "oui", "non"),
                  modes_tc_rail = ifelse(1 %in% Mode_tgv | 1 %in% Mode_train, "oui", "non"),
                  modes_trott   = ifelse(1 %in% Mode_trottoir, "oui", "non"),
                  modes_avion   = ifelse(1 %in% Mode_avion, "oui", "non"),
                  modes_bateau  = ifelse(1 %in% Mode_bateau, "oui", "non"),
                  
                  modes_motor   = ifelse(1 %in% Mode_voiture | 1 %in% Mode_drm | 1 %in% Mode_utilitaire |
                                             1 %in% Mode_taxi, "oui", "non"),
                  
                  modes_tgv     = ifelse(1 %in% Mode_tgv, "oui", "non"),
                  modes_métro   = ifelse(1 %in% Mode_métro, "oui", "non"))
    
    # Si un déplacement a été fait entièrement à pied, il n'est que dans DEP :
    # Vaut aussi pour les tables (nombreuses) où il n'y a pas de TRJ
    DEP_Modes = DEP %>% group_by(uid_PER)
    
    DEP_Modes$ModeP = plyr::revalue(DEP_Modes$ModeP, corresModes)
    DEP_Modes = DEP_Modes %>%
        mutate(nTj = 1) %>%
        pivot_wider(names_from = ModeP, values_from = nTj, names_prefix = "Mode_") %>%
        group_by(uid_PER) %>%
        summarise(modesd_marche  = ifelse(1 %in% Mode_marche, "oui", "non"),
                  modesd_voiture = ifelse(1 %in% Mode_voiture, "oui", "non"),
                  modesd_vélo    = ifelse(1 %in% Mode_vélo, "oui", "non"),
                  modesd_drm     = ifelse(1 %in% Mode_drm, "oui", "non"),
                  modesd_tc_route= ifelse(1 %in% Mode_bus | 1 %in% Mode_car, "oui", "non"),
                  modesd_tc_light= ifelse(1 %in% Mode_tram | 1 %in% Mode_métro, "oui", "non"),
                  modesd_tc_rail = ifelse(1 %in% Mode_tgv | 1 %in% Mode_train, "oui", "non"),
                  modesd_trott   = ifelse(1 %in% Mode_trottoir, "oui", "non"),
                  modesd_avion   = ifelse(1 %in% Mode_avion, "oui", "non"),
                  modesd_bateau  = ifelse(1 %in% Mode_bateau, "oui", "non"),
                  
                  modesd_motor   = ifelse(1 %in% Mode_voiture | 1 %in% Mode_drm | 1 %in% Mode_utilitaire |
                                             1 %in% Mode_taxi, "oui", "non"),
                  
                  modesd_tgv     = ifelse(1 %in% Mode_tgv, "oui", "non"),
                  modesd_métro   = ifelse(1 %in% Mode_métro, "oui", "non"))
    
    # Il y a des lignes vides pour les gens qui n'ont fait que des déplacements à pied (?)
    PER_Modes = select(PER, uid_PER) %>%
        left_join(TRJ_Modes, by="uid_PER") %>%
        left_join(DEP_Modes, by="uid_PER") %>%
        mutate(modes_marche = ifelse(modes_marche == "oui" | modesd_marche  == "oui", "oui", "non"),
               modes_voiture= ifelse(modes_voiture== "oui" | modesd_voiture == "oui", "oui", "non"),
               modes_vélo   = ifelse(modes_vélo   == "oui" | modesd_vélo    == "oui", "oui", "non"),
               modes_drm    = ifelse(modes_drm    == "oui" | modesd_drm     == "oui", "oui", "non"),
               modes_tc_route=ifelse(modes_tc_route=="oui" | modesd_tc_route== "oui", "oui", "non"),
               modes_tc_light=ifelse(modes_tc_light=="oui" | modesd_tc_light== "oui", "oui", "non"),
               modes_tc_rail= ifelse(modes_tc_rail== "oui" | modesd_tc_rail == "oui", "oui", "non"),
               modes_trott  = ifelse(modes_trott  == "oui" | modesd_trott   == "oui", "oui", "non"),
               modes_avion  = ifelse(modes_avion  == "oui" | modesd_avion   == "oui", "oui", "non"),
               modes_bateau = ifelse(modes_bateau == "oui" | modesd_bateau  == "oui", "oui", "non"),
               modes_motor  = ifelse(modes_motor  == "oui" | modesd_motor   == "oui", "oui", "non"),
               modes_tgv    = ifelse(modes_tgv    == "oui" | modesd_tgv     == "oui", "oui", "non"),
               modes_métro  = ifelse(modes_métro  == "oui" | modesd_métro   == "oui", "oui", "non"))
    
    # On détecte les lignes où il n'y a de la marche mais rien d'autre :
    # PER_Modes = PER_Modes %>% mutate(across(starts_with("modes_"),
    #                           ~ifelse(!is.na(marche) & marche == "oui" & is.na(modes_voiture), "non", .)))
    # PER_Modes = rename(PER_Modes, modes_marche = marche)
    
    # On veut des facteurs
    PER_Modes = PER_Modes %>%
        mutate(across(starts_with("modes_"), ~factor(., levels = c("non", "oui"))))
    
    PER = left_join(PER, PER_Modes, by="uid_PER")
    
    # PER = mutate(PER,   Voiture =  case_when(pDis_VOI == 100               ~ "VOITURE",
    #                                          pDis_VOI < 100 & pDis_VOI > 0 ~ "MULTI",
    #                                          Dis_VOI == 0                  ~ "SANS VOITURE"),
    #                     ModeDoux = case_when(pDis_MAR == 0 & pDis_VEL == 0 ~ "AUCUN",
    #                                          pDis_MAR >  0 & pDis_VEL == 0 ~ "MARCHE",
    #                                          pDis_VEL >  0                 ~ "VELO"),
    #                     TranspCo = case_when(pDis_BUS == 0 & pDis_TRN == 0 ~ "AUCUN",
    #                                          pDis_BUS >  0 & pDis_TRN == 0 ~ "SANS TRAIN",
    #                                          pDis_TRN >  0                 ~ "DONT TRAIN"))

    # On recalcule les indices à l'ancienne :
    PER = PER %>%
        mutate(autreQueVoiture = ifelse(modes_avion == "oui" | modes_vélo == "oui" |
                                        modes_tc_route == "oui" | modes_tc_light == "oui" |
                                        modes_tc_rail == "oui" | modes_bateau == "oui", "oui", "non")) %>%
        mutate(Voiture = case_when(modes_voiture == "oui" & autreQueVoiture == "non" ~ "voiture",
                                   modes_voiture == "oui" & autreQueVoiture == "oui" ~ "multi",
                                   modes_voiture == "non" ~ "sans voiture"),
               marche   = ifelse(modes_marche == "oui" | modes_trott == "oui", "oui", "non"),
               ModeDoux = case_when(modes_vélo == "oui" ~ "vélo",
                                    modes_vélo == "non" & marche == "oui" ~ "marche",
                                    modes_vélo == "non" & marche == "non" ~ "aucun"),
               modes_tc = ifelse(modes_tc_light == "oui" | modes_tc_route == "oui", "oui", "non"),
               TranspCo = case_when(modes_tc == "non" & modes_tc_rail == "non" ~ "aucun",
                                    modes_tc == "oui" & modes_tc_rail == "non" ~ "tc",
                                    modes_tc_rail == "oui" ~ "tc train"))
    
    PER$TriMode = NULL
    PER$typoMode = NULL
    Modes = PER %>% filter(!is.na(Voiture)) %>% pivot_longer(cols = c("Voiture", "ModeDoux", "TranspCo")) %>%
        group_by(uid_PER) %>% summarize(TriMode = paste(value, collapse = " + "))
    
    PER = left_join(PER, Modes, by=c("uid_PER" = "uid_PER")) ; remove(Modes)
    
    # Variable unique décrivant les différentes combinaisons entre les trois précédentes
    PER = mutate(PER, typoModes = case_when(TriMode == "voiture + aucun + aucun"         ~ "voiture",
                                            TriMode == "sans voiture + marche + aucun"   ~ "marche",
                                            TriMode == "multi + marche + aucun"          ~ "voiture+doux",
                                            TriMode == "sans voiture + aucun + tc"       ~ "tc",
                                            TriMode == "sans voiture + marche + tc"      ~ "tc",
                                            TriMode == "multi + aucun + tc"              ~ "voiture+tc",
                                            TriMode == "sans voiture + aucun + tc train" ~ "train",
                                            TriMode == "sans voiture + marche + tc train"~ "train",
                                            TriMode == "multi + marche + tc"             ~ "voiture+tc",
                                            TriMode == "sans voiture + vélo + aucun"     ~ "vélo",
                                            TriMode == "multi + aucun + tc train"        ~ "voiture+train",
                                            TriMode == "multi + vélo + aucun"            ~ "voiture+doux",
                                            TriMode == "multi + marche + tc train"       ~ "train",
                                            TriMode == "sans voiture + vélo + tc"        ~ "vélo",
                                            TriMode == "multi + vélo + tc"               ~ "voiture+tc",
                                            TriMode == "multi + vélo + tc train"         ~ "voiture+train",
                                            TriMode == "sans voiture + vélo + tc train"  ~ "train"))
    PER$typoModes   =    factor(PER$typoModes, levels=c("marche", "vélo", "voiture+doux", "voiture",
                                                                      "voiture+tc", "tc", "voiture+train", "train"))
    
    # Calcul pour savoir si l'individu se trouve en heure de pointe
    # Cette étape semble faire échouer le calcul par dépassement de la capacité de mémoire de Québec
    # Comme ce champ n'est pas très utilisé, je le mets HS
    
    # hdp_pivot = HDP %>% #pivot_longer(cols = c("pVoit", "pColl"), names_to = "Mode", values_to = "nPER") %>%
    #     pivot_longer(cols = c("n_Voit", "n_Coll", "med_Voit", "med_Coll", "mad_Voit", "mad_Coll"),
    #                  names_to = c(".value", "Mode"), names_pattern = "(.+)_(.+)") %>%
    #     mutate(MedianesVal = med + mad)
    # listeH = seq(248, 1620, 15)
    # PERetHDP = data.frame(heure = NA, uid_PER = NA, hdpMat = NA, hdpSoir = NA)
    # ACT$uid_ENQ = substr(ACT$uid_PER, 1,7)
    # b = ui_ProgInit(max(listeH)-min(listeH))
    # for (h in listeH)
    # {
    #     PERetHDPp = filter(ACT, h >= heureHHMMtoM(hDeb) & h < heureHHMMtoM(hFin)) %>%
    #         left_join(filter(hdp, heure == h), by = c("uid_ENQ" = "uid_ENQ")) %>%
    #         mutate(hdp = case_when(Tache == "992" & HDP_Voit & h <840                           ~ 1,
    #                                Tache == "602" & HDP_Voit & h <840                           ~ 1,
    #                                Tache == "702" & HDP_Voit & h <840                           ~ 1,
    #                                Tache == "522" & HDP_Voit & h <840                           ~ 1,
    #                                Tache %in% c("993","994","995","996","997","998") & HDP_Coll ~ 1,
    #                                Tache %in% c("603","604","605","606","607","608") & HDP_Coll ~ 1,
    #                                Tache %in% c("703","704","705","706","707","708") & HDP_Coll ~ 1,
    #                                Tache %in% c("523","524","525","526","527","528") & HDP_Coll ~ 1,
    #                                T                                                            ~ 0)) %>%
    #         mutate(heure = h) %>%
    #         mutate(hdpMat = ifelse(heure < 840, hdp, 0), hdpSoir = ifelse(heure >=840, hdp, 0)) %>%
    #         select(heure, uid_PER, hdpMat, hdpSoir)
    #     PERetHDP = rbind(PERetHDP, PERetHDPp)
    #     ui_Prog(b, h-min(listeH))
    # }
    # PERetHDP = group_by(PERetHDP, uid_PER) %>% summarize(hdpMat = max(hdpMat), hdpSoir = max(hdpSoir)) %>%
    #     mutate(hdp = case_when(hdpMat == 1 & hdpSoir == 1 ~ "MATIN/SOIR",
    #                            hdpMat == 1 & hdpSoir == 0 ~ "MATIN",
    #                            hdpMat == 0 & hdpSoir == 1 ~ "SOIR",
    #                            hdpMat == 0 & hdpSoir == 0 ~ "AUCUN")) %>%
    #     select(uid_PER, hdp)
    # PER = left_join(PER, PERetHDP, by=c("uid_PER" = "uid_PER")) ; remove(hdp_pivot, PERetHDP)
    # PER$hdp = factor(PER$hdp, levels=c("AUCUN", "MATIN", "MATIN/SOIR", "SOIR"))
    
    quantDis = Hmisc::wtd.quantile(filter(PER, Dis !=0)$Dis,
                                   filter(PER, Dis !=0)$CoeffEnq, prob=c(0, .2, .4, .6, .8, 1))
    
    PER = mutate(PER, 
                 clDis = case_when(Dis >= quantDis[1] & Dis < quantDis[2] ~
                                       paste0(km(quantDis[1]), " A ", km(quantDis[2]), " KM"),
                                   Dis >= quantDis[2] & Dis < quantDis[3] ~
                                       paste0(km(quantDis[2]), " A ", km(quantDis[3]), " KM"),
                                   Dis >= quantDis[3] & Dis < quantDis[4] ~ 
                                       paste0(km(quantDis[3]), " A ", km(quantDis[4]), " KM"),
                                   Dis >= quantDis[4] & Dis < quantDis[5] ~
                                       paste0(km(quantDis[4]), " A ", km(quantDis[5]), " KM"),
                                   Dis >= quantDis[5] & Dis <=quantDis[6] ~
                                       paste0("PLUS DE ", km(quantDis[5]), " KM")))
    PER$clDis = as.factor(PER$clDis)
    
    return(PER)
}

init_geomAct = function(PER, ACT, shp_ZF, PGT)
{
    # Id unique pour les PGT
    PGT = PGT %>% mutate(CODE_ZF = paste(ENQ, ZF), geometry) %>% select(CODE_ZF, geometry)
    
    # Transformons nos secteurs fins en points :
    pts_ZF = shp_ZF %>% st_point_on_surface() %>% st_transform(crs = 3857)
    pts_ZF = rbind(select(pts_ZF, CODE_ZF, geometry), PGT)
    ACT = left_join(ACT, pts_ZF, by=c("l" = "CODE_ZF"))

    # On peut enlever tous les points qui correspondent à du transport
    ACT2G = filter(ACT, !(substr(Tache,1,2) %in% c("99", "60", "70", "52") & du > 0))
    # Après ça, qui a des trous dans sa journée ?
    nEmpty = ACT %>% mutate(empty = ifelse(st_is_empty(geometry), 1, 0)) %>% group_by(uid_PER) %>%
        summarize(nEmp = sum(empty), Coeff = first(CoeffEnq))
    rapport(round(sum(filter(nEmpty, nEmp>1)$Coeff, na.rm=T)/
                      sum(summarize(group_by(ACT2G, uid_PER), CoeffEnq = first(CoeffEnq))$CoeffEnq, na.rm=T)*100, 1),
            "% des enquêté·es restant·es n'ont pas un maillage de points complet",
            info = T)
    ACT2G = filter(ACT2G, uid_PER %in% filter(nEmpty, nEmp==0)$uid_PER)
    rapport("Il reste", length(unique(ACT2G$uid_PER)), "enquêté·es après suppression des valeurs manquantes.", info=T)
    
    # Identification des points représentant le domicile
    rapport("Identification des domiciles.")
    ACT2G.Dom = filter(ACT2G, Tache %in% c("010", "020")) %>% group_by(uid_PER, l) %>%
        summarize(du = sum(du), .groups="drop_last") %>% group_by(uid_PER) %>% summarize(which.max(du))
    rapport("La sélection du 1er lieu comme lieu de domicile pose problème pour", table(ACT2G.Dom[[2]]>1)[[2]],
            "enquêté·es (temps passé dans un autre lieu de domicile par la suite supérieur à celui passé",
            "dans le premier)", info=T)
    ACT2G.Dom = filter(ACT2G, Tache %in% c("010", "020")) %>% group_by(uid_PER) %>%
        dplyr::summarize(l = first(l)) %>%
        left_join(pts_ZF, by=c("l" = "CODE_ZF"))
    ACT2G.Dom = rename(ACT2G.Dom, lDom = l, gDom = geometry)
    ACT2G = left_join(ACT2G, as.data.frame(ACT2G.Dom), by=c("uid_PER" = "uid_PER"))
    ACT2G = st_as_sf(ACT2G)
    
    # Convex hull par personne.
    rapport("Calcul du polygone convexe autour des lieux fréquentés dans la journée")
    ACT2G.CvH = ACT2G %>% group_by(uid_PER) %>% summarize(geometry = st_combine(geometry)) %>%
        st_convex_hull()
    ACT2G.CvH$surfCvxHull = as.numeric(st_area(ACT2G.CvH) / 1000000)
    
    # On associe le centroïde pour faire des mesures d'excentricité
    rapport("Centroïde des nuages de points")
    ACT2G.CvC = ACT2G.CvH %>% st_centroid() %>% rename(gCtd = geometry)
    ACT2G = left_join(ACT2G, as.data.frame(ACT2G.CvC), by=c("uid_PER" = "uid_PER"))
    
    # Distance moyenne des points au domicile, y.c. avec pondérations, et distance moyenne au centroïde
    rapport("Calcul de la distance moyenne des points fréquentés dans la journée avec centroïde et domicile")
    ACT2G = ACT2G %>% mutate(disDom = st_distance(geometry, gDom, by_element = T),
                             disCtd = st_distance(geometry, gCtd, by_element = T))
    # Dist moyenne au domicile (en incluant le domicile, donc 0m)
    ACT2G.Gr1 = st_drop_geometry(ACT2G) %>% group_by(uid_PER) %>%
        summarize(disDomP = weighted.mean(as.numeric(disDom), du)/1000)
    # Dist moyenne des points par rapport au domicile
    ACT2G.Gr2 = st_drop_geometry(ACT2G) %>% filter(!Tache %in% c("010", "020")) %>%
        group_by(uid_PER, l) %>%
        summarize(disDom = first(disDom), .groups="drop_last") %>% # d'abord, on élimine les doublons
        summarize(disDom = mean(as.numeric(disDom))/1000)
    # Dist moyenne des points par rapport au centroïde
    ACT2G.Gr3 = st_drop_geometry(ACT2G) %>%
        group_by(uid_PER, l) %>%
        summarize(disCtd = first(disCtd), .groups="drop_last") %>% # d'abord, on élimine les doublons
        summarize(disCtd = mean(as.numeric(disCtd))/1000)
    
    # Distance entre le centroïde et le domicile (pour en évaluer l'excentricité)
    rapport("Calcul de la distance entre le domicile et le centroïde des nuages de points")
    tblDisDomCtd = PER %>% select(uid_PER) %>%
        left_join(select(ACT2G.Dom, uid_PER, gDom), by=c("uid_PER" = "uid_PER")) %>%
        left_join(select(ACT2G.CvC, uid_PER, gCtd), by=c("uid_PER" = "uid_PER")) %>%
        filter(!st_is_empty(gDom) & !st_is_empty(gCtd))
    tblDisDomCtd$disDomCtd = st_distance(x = tblDisDomCtd$gDom, y = tblDisDomCtd$gCtd, by_element = T)
    tblDisDomCtd$disDomCtd = as.numeric(tblDisDomCtd$disDomCtd) / 1000
    
    # Jointure à PER
    PER = PER %>%
        left_join(ACT2G.Gr1, by=c("uid_PER" = "uid_PER")) %>%
        left_join(ACT2G.Gr2, by=c("uid_PER" = "uid_PER")) %>%
        left_join(ACT2G.Gr3, by=c("uid_PER" = "uid_PER")) %>%
        left_join(select(tblDisDomCtd, uid_PER, disDomCtd), by=c("uid_PER" = "uid_PER")) %>%
        left_join(select(st_drop_geometry(ACT2G.CvH), uid_PER, surfCvxHull), by=c("uid_PER" = "uid_PER"))
    
    return(PER)
}

attribuerVehicule = function(PER, TRJ)
{
    rapport("Attribution des codes uniques des véhicules à chaque enquêté⋅e")
    partMax = TRJ %>%
        mutate(disVeh1 = ifelse(IdVeh == "01", Dis, 0),
               disVeh2 = ifelse(IdVeh == "02", Dis, 0),
               disVeh3 = ifelse(IdVeh == "03", Dis, 0),
               disVeh4 = ifelse(IdVeh == "04", Dis, 0)) %>%
        group_by(uid_PER) %>%
        summarize(pDisVeh1 = sum(disVeh1, na.rm=T) / sum(Dis, na.rm=T),
                  pDisVeh2 = sum(disVeh2, na.rm=T) / sum(Dis, na.rm=T),
                  pDisVeh3 = sum(disVeh3, na.rm=T) / sum(Dis, na.rm=T),
                  pDisVeh4 = sum(disVeh4, na.rm=T) / sum(Dis, na.rm=T)) %>%
        filter(!is.na(pDisVeh1)) %>%  # les personnes qui n'ont pas pris la voiture sont NaN 
        ungroup() %>% rowwise() %>%
        mutate(VehP = which.max(c(pDisVeh1, pDisVeh2, pDisVeh3, pDisVeh4))) %>%
        mutate(uid_VEH = paste0(substr(uid_PER, 1, 22), "-V", VehP))
    
    PER = left_join(PER, select(partMax, uid_PER, uid_VEH), by=c("uid_PER" = "uid_PER"))
    return(PER)
}

attribuerVehiculeEMP = function(PER, VEH_EMP)
{
    rapport("Attribution des véhicules aux conducteur·rices de l'EMP")
    VEH_EMP = mutate(VEH_EMP,
                     uid_MEN = substr(uid_VEH, 1, 22),
                     uid_PER = paste0(uid_MEN, ifelse(nchar(as.character(Conducteurice) == 1),
                                             paste0("0", as.character(Conducteurice)),
                                             as.character(Conducteurice))))
    VEH_EMP$uid_MEN = NULL
    
    # On ne peut pas faire un left_join directement ! < erreur janvier 2023 >
    # Cela crée des doublons dans la base PER à chaque fois qu'il y a plusieurs véhicules pour 1 pers.
    # Il faut donc faire un choix : quand il y en aura plusieurs, on privilégiera le véhicule ayant
    # le kmg le plus important sur les 12 mois précédents.
    
    VEH_EMP = tab_Tri(t=VEH_EMP, i="uid_VEH", parCol = "Km_12m", rev=T)
    
    VEH_EMP_Prim = VEH_EMP %>% group_by(uid_PER) %>%
        summarise(across(everything(), ~as.character(first(.))))
    
    PER = left_join(PER, select(VEH_EMP_Prim, uid_PER, uid_VEH), by="uid_PER")
    return(PER)
}

# Courroies de transmission ====

courroie_PCSM = function(PER, MEN)
{
    PER = left_join(PER, select(MEN, uid_MEN, PCSMT, PCSMLT, PCSM, PCSMD, PCSML, PCSMLD),
                    by = c("uid_MEN" = "uid_MEN"))
    return(PER)
}

courroie_typoCom = function(PER, MEN)
{
    PER = left_join(PER, select(MEN, uid_MEN, ZoneRang, ZonePosi, ZoneDens),
                    by = c("uid_MEN" = "uid_MEN"))
    return(PER)
}

calcul_coeffs_nationaux_PER = function(MEN, PER, shp_ZT, shp_COM)
{
  rapport("Calcul du champ de coefficient conforme à l'échelle nationale")
  
  # On vire ce qu'il y a déjà, si nécessaire (si utilisation non linéaire)
  PER$CoeffRec = NULL
  PER$CoeffRecEnq = NULL
  PER$CoeffRecSansEMP = NULL
  PER$CoeffRecEnqSansEMP = NULL
  
  # Fichier Population du recensement de 2020, avec les données de 2014.
  structurePop = read_delim("Sources/base-cc-evol-struct-pop-2020.CSV",
                            delim = ";")
  
  # La Corse toujours
  structurePop = structurePop |>
    corse2A2Bvers20(champCom = "CODGEO")
  
  # Jointure pour get la grille communale de densité.
  densite = read_delim("Sources/grille_densite_2020_agrege.csv", delim = ";") %>%
    select(COM, DENS) |> corse2A2Bvers20(champCom = "COM")
  
  # Ça marche pas pour les arrondissements de Paris, Lyon et Marseille.
  arrondissements = tibble(COM = c(as.character(c(13201:13216)),
                                   as.character(c(69381:69389)),
                                   as.character(c(75101:75120))),
                           DENS = rep(1, times = 45))
  densite = rbind(densite, arrondissements)
  structurePop = left_join(structurePop, densite, by=c("CODGEO" = "COM"))
  remove(densite, arrondissements)
  
  # Jointure pour get le rang d'AAV.
  aavs = read_delim("Sources/Mailles/Aires Attraction.csv", delim = ";") %>%
    select(AAV2020, TAAV2017)
  aavs_com = read_delim("Sources/Mailles/Zonage Attraction.csv", delim = ";") %>%
    select(CODGEO, AAV2020) %>%
    corse2A2Bvers20(champCom = "CODGEO") |>
    left_join(aavs, by="AAV2020") %>%
    select(-AAV2020)
  
  # On l'applique aux arrondissements
  arrondissements = tibble(CODGEO = c(as.character(c(13201:13216)),
                                      as.character(c(69381:69389)),
                                      as.character(c(75101:75120))),
                           TAAV2017 = c(rep(4, times = 25), rep(5, times = 20)))
  aavs_com = rbind(aavs_com, arrondissements)
  
  structurePop = left_join(structurePop, aavs_com, by="CODGEO")
  remove(aavs_com, arrondissements)
  
  # Test Juillet 2024
  # sPopTemp = structurePop %>% # filter(CODGEO %in% PER$Com) |>
  #   select(C14_POP1524_CS1:C14_POP1524_CS8, C14_POP2554_CS1:C14_POP2554_CS8,
  #          C14_POP55P_CS1:C14_POP55P_CS8) %>%
  #   pivot_longer(cols = starts_with("C14"), values_to = "n", names_to = "CATEG") %>%
  #   mutate(PCS_insee = substr(CATEG, nchar(CATEG), nchar(CATEG))) %>%
  #   mutate(age_insee = substr(CATEG, 8,9),
  #          age_insee = plyr::revalue(age_insee, c("15" = "1524",
  #                                                 "25" = "2554",
  #                                                 "55" = "55P"))) %>% select(-CATEG) |>
  #   group_by(age_insee, PCS_insee) |> summarise(n = sum(n, na.rm=T)) |>
  #   ungroup() |> mutate(p = n / sum(n))
  # comp = PER %>%
  #   mutate(age_insee = case_when(Age > 14 & Age < 25 ~ "1524",
  #                                Age > 24 & Age < 55 ~ "2554",
  #                                Age > 54            ~ "55P"),
  #          PCS_insee = case_when(Activ == "32" ~ "7",
  #                                Activ == "33" ~ "8",
  #                                PCS8  == "07" ~ "8",
  #                                !PCS8 %in% c("00", "09") ~ substr(PCS8, 2, 2))) |>
  #   filter(!is.na(age_insee), !is.na(PCS_insee)) |>
  #   group_by(age_insee, PCS_insee) |> summarise(nPER = n()) |>
  #   ungroup() |> mutate(pPER = nPER / sum(nPER)) |>
  #   left_join(sPopTemp, by = c("age_insee", "PCS_insee")) |>
  #   mutate(surrep = pPER / p)
  
  # Ready pour faire deux tableaux :
  tabPop = structurePop %>%
    mutate(CODGEO = comEnDep(CODGEO)) |> # agrégation par département
 #   group_by(DENS, CODGEO) %>% summarise(across(where(is.numeric), sum, na.rm=T)) %>%
    left_join(tableDepVersReg(), by = c("CODGEO" = "Dep")) |>
    group_by(DENS, Reg) |> summarise(across(where(is.numeric), sum, na.rm=T)) |>
    select(DENS, Reg, C14_POP1524_CS1:C14_POP1524_CS8, C14_POP2554_CS1:C14_POP2554_CS8,
           C14_POP55P_CS1:C14_POP55P_CS8) %>%
    pivot_longer(cols = starts_with("C14"), values_to = "n", names_to = "CATEG") %>%
    mutate(PCS_insee = substr(CATEG, nchar(CATEG), nchar(CATEG))) %>%
    mutate(age_insee = substr(CATEG, 8,9),
           age_insee = plyr::revalue(age_insee, c("15" = "1524",
                                                  "25" = "2554",
                                                  "55" = "55P"))) %>% select(-CATEG) %>%
    rename(ZoneDens = DENS) |>
    mutate(ZoneDens = as.factor(ZoneDens))
  
  remove(structurePop)
  
  # Impossible malheureusement d'agréger la population
  # Échoue systématiquement
  
  # # Constitution d'un fichier de référence
  # maillage = MEN |> group_by(ZT) |> summarise(ZTS = first(ZTS))
  # maillage = left_join(maillage, select(shp_ZT, ZT, geometry), by="ZT") |>
  #   left_join(select(shp_ZTS, ZTS, geometry), by="ZTS")
  # maillage$geometry = ifelse(st_is_empty(maillage$geometry.x), maillage$geometry.y, maillage$geometry.x)
  # maillage = select(maillage, -geometry.y, -geometry.x)
  # maillage = st_as_sf(maillage)
  
  # # Il faut diviser les ZTS qui se recoupent sur le territoire d'une commune
  # division = maillage |> group_by(ZTS) |> summarise(coeffDiv = n())
  # maillage = maillage |> left_join(st_drop_geometry(division), by="ZTS")
  # remove(division)
  
  # # On récupère à présent les données du recensement via le maillage communal
  # tabPop = st_as_sf(tabPop)
  # maillage = st_set_crs(maillage, 3857)
  # 
  # tabPop = st_simplify(tabPop, dTolerance = 500, preserveTopology = T)
  
  # L'interesction est ABSOLUMENT IMPOSSIBLE. Elle remplit systématiquement la mémoire
  # et fait planter l'ordinateur. On est bloqué...
  # À moins d'être intelligent et de présélectionner les mailles...
  # i = 1
  # 
  # tabPopLocal = st_intersection(tabPop, st_buffer(st_centroid(maillage[i,]), dist = 40000))
  # st_intersection(x = maillage[i,], y = tabPop)
  
  # popZTS = changerMaillage(init = tabPop, dest = maillage, uid1 = "CODGEO", uid2 = "ZT")
  
  PER = PER %>%
    mutate(age_insee = case_when(Age > 14 & Age < 25 ~ "1524",
                                 Age > 24 & Age < 55 ~ "2554",
                                 Age > 54            ~ "55P"),
           PCS_insee = case_when(Activ == "32" ~ "7",
                                 Activ == "33" ~ "8",
                                 PCS8  == "07" ~ "8",
                                 !PCS8 %in% c("00", "09") ~ substr(PCS8, 2, 2)))
  
  # Ajout Juillet 2024 : sans EMP
  tabPopPER = PER %>%
    corse2A2Bvers20() |>
    mutate(Dep = comEnDep(Com)) |>
    left_join(tableDepVersReg(), by="Dep") |>
    filter(Age > 14) %>%
    filter(!is.na(ZoneDens), !is.na(Reg), !is.na(PCS_insee), !is.na(Age)) %>%
    group_by(ZoneDens, Reg, age_insee, PCS_insee) %>%
    summarise(nPER = n(), nPER_enq = sum(ifelse(is.na(CoeffEnq) | CoeffEnq == 0, 0, 1)))
  tabPopPER_sansEMP = PER %>%
    corse2A2Bvers20() |>
    mutate(Dep = comEnDep(Com)) |>
    left_join(tableDepVersReg(), by="Dep") |>
    filter(uid_ENQ != "EMP2019") |>
    filter(Age > 14) %>%
    filter(!is.na(ZoneDens), !is.na(Reg), !is.na(PCS_insee), !is.na(Age)) %>%
    group_by(ZoneDens, Reg, age_insee, PCS_insee) %>%
    summarise(nPER = n(), nPER_enq = sum(ifelse(is.na(CoeffEnq) | CoeffEnq == 0, 0, 1)))
  
  # On joint et on calcule les ratios pour chaque intersection :
  tabPopPER = tabPopPER %>%
    left_join(tabPop, by=c("ZoneDens", "Reg", "age_insee", "PCS_insee")) %>%
    mutate(CoeffRec = n / nPER, CoeffRecEnq = n / nPER_enq)
  tabPopPER_sansEMP = tabPopPER_sansEMP %>%
    left_join(tabPop, by=c("ZoneDens", "Reg", "age_insee", "PCS_insee")) %>%
    mutate(CoeffRecSansEMP = n / nPER, CoeffRecEnqSansEMP = n / nPER_enq)
  
  PER$SansEMP = PER$uid_ENQ != "EMP2019"
  tabPopPER_sansEMP$SansEMP = T
  
  # Besoin d'un champ Reg dans PER
  PER = PER |>
    mutate(Com2 = Com) |>
    corse2A2Bvers20(champCom = "Com2") |>
    mutate(Dep = comEnDep(Com)) |>
    left_join(tableDepVersReg(), by="Dep")
  PER$Com2 = NULL
  
  # On récupère le nouveau poids dans PER :
  PER = left_join(PER, select(tabPopPER, ZoneDens, Reg, age_insee, PCS_insee, CoeffRec),
                  by = c("ZoneDens", "Reg", "age_insee", "PCS_insee"))
  PER = left_join(PER, select(tabPopPER_sansEMP, ZoneDens, Reg, age_insee, PCS_insee, SansEMP, CoeffRecSansEMP),
                  by = c("ZoneDens", "Reg", "age_insee", "PCS_insee", "SansEMP"))
  
  # Pour le CoeffEnq :
  PER = mutate(PER, CoeffEnqOk = !is.na(CoeffEnq) & CoeffEnq != 0)
  tabPopPER$CoeffEnqOk = T
  tabPopPER_sansEMP$CoeffEnqOk = T
  PER = left_join(PER, select(tabPopPER, ZoneDens, Reg, age_insee, PCS_insee, CoeffEnqOk, CoeffRecEnq),
                  by = c("ZoneDens", "Reg", "age_insee", "PCS_insee", "CoeffEnqOk"))
  PER = left_join(PER, select(tabPopPER_sansEMP, ZoneDens, Reg, age_insee, PCS_insee, CoeffEnqOk, SansEMP, CoeffRecEnqSansEMP),
                  by = c("ZoneDens", "Reg", "age_insee", "PCS_insee", "CoeffEnqOk", "SansEMP"))
  
  PER = select(PER, -CoeffEnqOk, -Reg)
  
  rapport("Population selon le recensement :", round(sum(tabPop$n)), info = T)
  rapport("Population redressée :", round(sum(PER$CoeffRec, na.rm=T)), info = T)
  rapport("Population redressée (enquêté⋅es) :", round(sum(PER$CoeffRecEnq, na.rm=T)), info = T)
  rapport("Population redressée (enquêté⋅es, sans EMP) :", round(sum(PER$CoeffRecEnqSansEMP, na.rm=T)), info = T)
  
  return(PER)
}


# Recodages élémentaires ====

recode_EnqMeth = function(base)
{
    base$EnqMeth = plyr::revalue(base$EnqMeth, c("1"="F", "2"="T"))
    return(base)
}

recode_Enq = function(base)
{
    base$Enq = plyr::revalue(base$Enq,
                            c("1" = "EMD", "2" = "EDGT", "3" = "EDVM", "4" = "Autre", "5" = "EMC2"))
    return(base)
}

recode_LogOcc = function(base)
{
    base$LogOcc = plyr::revalue(base$LogOcc, c("1" = "10", "2" = "21", "3" = "20", "4" = "30", "5" = "40",
                                             "6" = "99", "7" = "99", "8" = "22", "9" = NA))
    return(base)
}

recode_Genre = function(base)
{
    base$Genre = plyr::revalue(base$Genre, c("1"="H","2"="F"))
    return(base)
}

recode_NivEtu = function(base)
{
    base$NivEtu = plyr::revalue(base$NivEtu, c("00" = "90", # En cours
                                               "01" = "10", # Primaire
                                               "02" = "20", # Collège
                                               "03" = "30", # Lycée
                                               "04" = "40", # Bac
                                               "05" = "45", # Sup (L1-L2)
                                               "06" = "50", # Sup (Licence +)
                                               "07" = "31", # Apprentissage
                                               "08" = "41", # Apprentissage Sup
                                               "09" = "00", # Pas d'études
                                               "93" = "32", # Lycée (GT ou Pro)
                                               "97" = "33", # Apprentissage (Sup ou non)
                                               "90" =  NA ))# Non connu
    return(base)
}

recode_Activ = function(base)
{
    base$Activ = plyr::revalue(base$Activ,   c("1" = "10", # Emploi plein temps
                                               "2" = "11", # Emploi temps partiel
                                               "3" = "12", # Alternance ou stage
                                               "4" = "22", # Etudes
                                               "5" = "21", # Scolarité
                                               "6" = "31", # Sans emploi
                                               "7" = "32", # Retraite
                                               "8" = "33", # Au foyer (= ne cherche pas d'emploi ?)
                                               "9" =  NA ))
    return(base)
}

# Patches manuels sur les bases ====

patch_shpCodeZf = function(base)
{
    base = base %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "NIORT", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "TOURS", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "VALENCIENNES" & EnqAnnee == "2019", paste0(CODE_ZF, "000"), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(ENQUETE == "IDF", paste0("00",CODE_ZF), CODE_ZF)) %>%
        mutate(CODE_ZF = ifelse(nchar(CODE_ZF) == 6, paste0("000",CODE_ZF), CODE_ZF))
    return(base)
}

patch_shpCodeZt = function(base)
{
    base = base %>% mutate(CODE_SEC = ifelse(ENQUETE != "IDF", paste0("0", CODE_SEC), CODE_SEC)) %>%
                    mutate(CODE_SEC = ifelse(nchar(CODE_SEC) == 3, paste0("0", CODE_SEC), CODE_SEC))
    return(base)
}

patch_AurelieCodesZT = function(base)
{
    # Certains secteurs de tirage ne fonctionnent pas. Il faut les remplacer par leurs équivalents
    
    # Toulouse 
    listeO = c("101", "102", "103", "104", "105", "106", "108", "112", "121", "132", "210", "211", "223",
               "224", "313", "315", "316", "317", "328", "329", "330", "434", "446", "447")
    listeO = paste0("0", listeO)
    listeD = c("001", "002", "003", "004", "005", "006", "008", "012", "021", "032", "010", "011", "023",
               "024", "013", "015", "016", "017", "028", "029", "030", "034", "046", "047")
    listeD = paste0("0", listeD)
    base$ZT = ifelse(base$uid_ENQ == "TLS2013", plyr::mapvalues(base$ZT, listeO, listeD), base$ZT)
    base$ZF = ifelse(base$uid_ENQ == "TLS2013", paste0("00", base$ZT, substr(base$ZF, 7, 9)), base$ZF)
    
    # Nancy
    listeO = c("202", "203", "204", "210", "216", "217", "218", "219", "221", "223")
    listeO = paste0("0", listeO)
    listeD = c("102", "103", "104", "110", "116", "117", "118", "119", "121", "123")
    listeD = paste0("0", listeD)
    base$ZT = ifelse(base$uid_ENQ == "NAN2013", plyr::mapvalues(base$ZT, listeO, listeD), base$ZT)
    base$ZF = ifelse(base$uid_ENQ == "NAN2013", paste0("00", base$ZT, substr(base$ZF, 7, 9)), base$ZF)

    # Bayonne
    base$Com = ifelse(base$uid_ENQ == "BAY2010" & base$ZF == "000101005", "40187", base$Com)
    
    # Toulouse, base DEP uniquement
    if ("O_ZF" %in% colnames(base) & "D_ZF" %in% colnames(base))
    {
        listeO = c("0210", "0211", "0224", "0313") ; listeD = c("0010", "0011", "0024", "0013")
        base$D_ZT = ifelse(base$uid_ENQ == "TLS2013", plyr::mapvalues(base$D_ZT, listeO, listeD), base$D_ZT)
        base$O_ZT = ifelse(base$uid_ENQ == "TLS2013", plyr::mapvalues(base$O_ZT, listeO, listeD), base$O_ZT)
        base$D_ZF = ifelse(base$uid_ENQ == "TLS2013", paste0(substr(base$D_ZF, 1, 10),
                                                           plyr::mapvalues(substr(base$D_ZF, 11, 14), listeO, listeD),
                                                           substr(base$D_ZF, 15, 17)), base$D_ZF)
        base$O_ZF = ifelse(base$uid_ENQ == "TLS2013", paste0(substr(base$O_ZF, 1, 10),
                                                           plyr::mapvalues(substr(base$O_ZF, 11, 14), listeO, listeD),
                                                           substr(base$O_ZF, 15, 17)), base$O_ZF)
    }
    
    return(base)
}

patch_GeomFev2021 = function(base, shp_ZF, PGT)
{
    # Quelques erreurs découvertes à la livraison des bases et corrigées au cas par cas
    
    listeZF = charger_listeZF(shp_ZF, PGT)
    
    # Découverte intéressante à Brest : les PGT non renseignés se trouvent toujours dans la ZF
    # arrondie à la centaine, on peut donc deviner la ZF de rattachement facilement
    if ("D_ZF" %in% colnames(base)) {
        base = mutate(base, D_ZF = ifelse(uid_ENQ == "BRE2018" & !D_ZF %in% listeZF & D_ZT != "9999",
                                    paste0(substr(D_ZF,1,15),"00"), D_ZF))}
    if ("O_ZF" %in% colnames(base)) {
        base = mutate(base, O_ZF = ifelse(uid_ENQ == "BRE2018" & !O_ZF %in% listeZF & O_ZT != "9999",
                                    paste0(substr(O_ZF,1,15),"00"), O_ZF))}
    if ("ZF" %in% colnames(base)) {
        base = mutate(base, ZF   = ifelse(uid_ENQ == "BRE2018" & !ZF   %in% listeZF & ZT != "9999",
                                    paste0(substr(ZF,  1,7 ),"00"), ZF  ))}
    
    # À Dunkerque, les PGTs qui ont un 9 en avant-avant dernière position posent problème, pour 8 cas uniquement...
    # Suggestion : passer leur secteur de tirage en 9999, comme s'ils étaient indisponibles
    if ("D_ZF" %in% colnames(base)) {
        base = mutate(base, D_ZT = ifelse(uid_ENQ == "DUN2015" & substr(D_ZF, 15,15) == "9", "9999", D_ZT))}
    if ("O_ZF" %in% colnames(base)) {
        base = mutate(base, O_ZT = ifelse(uid_ENQ == "DUN2015" & substr(O_ZF, 15,15) == "9", "9999", O_ZT))}
    
    # En Loire-Atlantique, il y a une erreur de codage : deux ZF sont codées comme tirage 233 alors qu'elles sont
    # non localisées (le tirage devrait être 9999).
    # Suggestion : toutes les ZF dont le 12e caractère est 9 devraient être en tirage 9999, au moins en Loire-Atl
    if ("D_ZF" %in% colnames(base)) {
        base = mutate(base, D_ZT = ifelse(uid_ENQ == "LOI2015" & substr(D_ZF, 12,12) == "9", "9999", D_ZT))}
    if ("O_ZF" %in% colnames(base)) {
        base = mutate(base, O_ZT = ifelse(uid_ENQ == "LOI2015" & substr(O_ZF, 12,12) == "9", "9999", O_ZT))}
    
    # À Bayonne, la réattribution par commune a triomphé de toutes les erreurs. Il suffit d'adapter le secteur
    # d'après le code : on va réattribuer tous les secteurs 2... d'après le nom de leur ZF
    if ("D_ZF" %in% colnames(base)) {
        base = mutate(base, D_ZT = ifelse(substr(D_ZT,2,2) == "2" & uid_ENQ=="BAY2010",
                                    substr(D_ZF, 11, 14), D_ZT))}
    if ("O_ZF" %in% colnames(base)) {
        base = mutate(base, O_ZT = ifelse(substr(O_ZT,2,2) == "2" & uid_ENQ=="BAY2010",
                                    substr(O_ZF, 11, 14), O_ZT))}
    
    return(base)
}




patch_ZFenComUniques = function(base, tableZFdeComUniques)
{
    rapport("Réattribution des codes ZF par déduction dans les petites communes")
    
    if ("O_ZF" %in% colnames(base))
    {
        colnames(tableZFdeComUniques) = c("ZF", "O_ZF.RPL")
        base = left_join(base, tableZFdeComUniques, by=c("O_ZF" = "ZF")) %>%
               mutate(O_ZF = ifelse(is.na(O_ZF.RPL), O_ZF, O_ZF.RPL))
    }
    if ("D_ZF" %in% colnames(base))
    {
        colnames(tableZFdeComUniques) = c("ZF", "D_ZF.RPL")
        base = left_join(base, tableZFdeComUniques, by=c("D_ZF" = "ZF")) %>%
                mutate(D_ZF = ifelse(is.na(D_ZF.RPL), D_ZF, D_ZF.RPL))
    }
    if ("ZF" %in% colnames(base))
    {
        colnames(tableZFdeComUniques) = c("ZF", "ZF.RPL")
        base = mutate(base, ZFunique = ZF) %>%
               left_join(tableZFdeComUniques, by=c("ZFunique" = "ZF")) %>%
               mutate(ZF = ifelse(is.na(ZF.RPL), ZF, substr(ZF.RPL,9,17)))
    }
    
    base$D_ZF.RPL = NULL ; base$O_ZF.RPL = NULL ; base$ZF.RPL = NULL ; base$ZFUnique = NULL
    
    return(base)
}

patch_compoMenagesFev2021 = function(PER)
{
    rapport("Application du patch manuel de correction des erreurs de composition de ménage")
    
    # Revaluation de Lien pour rendre la correction plus facile
    PER$Lien = plyr::revalue(PER$Lien, c("1"="Réf","2"="Conjoint.e","3"="Enfant","4"="Co/Locataire","5"="Famille",
                                         "6"="Autre","7"="Autre ou Famille"))
    
    # Les corrections qui suivent sont réalisées individuellement afin de constituer une liste des problèmes constatés dans
    # la base. Cf. constitution des UID pour son application hors contexte de cet ensemble de scripts.
    # Elles ont été réalisées au moyen de la liste des cas aberrants issues de la section suivante. Les corrections
    # appliquées, dépendant du contexte apparent (traitement au cas par cas), sont décrites pour chaque section.
    
    # Enfants marqués comme conjoint.es surnuméraires, ou comme PRéf 
    # Douai
    PER[PER$uid_PER == "DOU2012F0000090080002404",]$Lien = "Enfant"
    PER[PER$uid_PER == "DOU2012F0000110060002701",]$Lien = "Réf"
    PER[PER$uid_PER == "DOU2012F0000110070004906",]$Lien = "Enfant"
    PER[PER$uid_PER == "DOU2012F0000160020003103",]$Lien = "Enfant"
    PER[PER$uid_PER == "DOU2012F0000290010007103",]$Lien = "Enfant"
    PER[PER$uid_PER == "DOU2012F0000310010003104",]$Lien = "Autre ou Famille" # ménage peu conventionnel
    PER[PER$uid_PER == "DOU2012F0000310020003504",]$Lien = "Enfant"
    # Thionville
    PER[PER$uid_PER == "THI2012T0000010030077004",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000050010027003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000070020046703",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000080030006703",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000100050036002",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000150010077403",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000150010077404",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000180020003003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000200080036003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000210050028003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000220030052503",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000070010023002",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000200010037202",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000100120020003",]$Lien = "Enfant"
    # Valenciennes 2011
    PER[PER$uid_PER == "VLN2011F0000050130005505",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000070020011504",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000070020011505",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000070050017402",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000070050017403",]$Lien = "Enfant"
    PER[PER$uid_PER == "VLN2011F0000370050006802",]$Lien = "Enfant"
    # Bordeaux
    PER[PER$uid_PER == "BOR2009F0000050060010903",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000190010017102",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000190010017103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000410010011402",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000410010011403",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000470040015103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000490070001103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000560010007403",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000560010007404",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000680020012404",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000050010018203",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000360050011703",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000370040000505",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000390080013805",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000390080013806",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000540010018503",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000540010018504",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000560010004104",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000270010012004",]$Lien = "Enfant"
    PER[PER$uid_PER == "BOR2009F0000420010001003",]$Lien = "Enfant"
    # Strasbourg / Bas-Rhin
    PER[PER$uid_PER == "BRH2009F0000080320008903",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000110010009102",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000110010009103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000150040006103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000190010008503",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000290020011003",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000430230010403",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000440260011003",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000470040010703",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000490100002203",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000540310001603",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000570390000604",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000620140005304",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000670320008503",]$Lien = "Autre ou Famille" # âgé
    PER[PER$uid_PER == "BRH2009F0000460040007103",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000460040007104",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000500330054003",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000530320056203",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000540530009603",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000570390000603",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000590190055402",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000610040000903",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000680160003503",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000120060010702",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000210060003302",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000350080010202",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000580370051402",]$Lien = "Enfant"
    PER[PER$uid_PER == "BRH2009F0000030230006404",]$Lien = "Enfant"
    # Toulouse
    PER[PER$uid_PER == "TOU2019T1150160002030903",]$Lien = "Enfant"
    PER[PER$uid_PER == "TOU2019T2410050002089103",]$Lien = "Enfant"
    # Marseille
    PER[PER$uid_PER == "MAR2009T0001310070001402",]$Lien = "Enfant"
    PER[PER$uid_PER == "MAR2009T0001310070001401",]$Lien = "Réf"
    # Angoulême
    PER[PER$uid_PER == "AGL2012T0000020040065802",]$Lien = "Enfant"
    # Saint-Louis
    PER[PER$uid_PER == "STL2011T0000003030008502",]$Lien = "Enfant"
    # Quimper
    PER[PER$uid_PER == "QMP2013T0000180050060802",]$Lien = "Enfant"
    
    # ~ Erreurs manifestes : pas de PRéf
    # Solution : l'adulte en activité (si possible), de la PCS la plus favorisée, ou le plus diplômé, ou le plus âgé
    # (si égaux.ales sur les 2 premières conditions ; privilégie l'âge sur le diplôme pour les couples >50 ans) est
    # passé en personne de référence, sachant que les enquêteurs de l'EMD tendent à toujours sélectionner l'H du couple
    # central. Quand sont égaux en tous points, privilégie l'ID 1
    # Nécessite également d'identifier un couple central (en général en position 1 et 2 de l'ID personne)
    # Valenciennes 2011
    PER[PER$uid_PER == "VLN2011F0000120080012701",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000020080000101",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000060170001002",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000080180015301",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000130040001501",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000190030004101",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000190030011301",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000210020011401",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000240180000701",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000340010005501",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000370030009301",]$Lien = "Réf"
    PER[PER$uid_PER == "VLN2011F0000400030012101",]$Lien = "Réf"
    # Paris / IDF
    PER[PER$uid_PER == "IDF2010F00555295F0011102",]$Lien = "Réf"
    PER[PER$uid_PER == "IDF2010F00528666B0287101",]$Lien = "Réf"
    PER[PER$uid_PER == "IDF2010F00551923D0305101",]$Lien = "Réf"
    # Bordeaux
    PER[PER$uid_PER == "BOR2009F0000010010002301",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000020010008801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000040050021201",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000310070003901",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000460060017201",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000490050002101",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000810040005901",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000030080017502",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000030080017501",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000340020096901",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000390080006201",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000470030004801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000680020010901",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000700030012801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000700040005701",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000710010004401",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000710010016801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000740010003001",]$Lien = "Réf"
    # Strasbourg / Bas Rhin
    PER[PER$uid_PER == "BRH2009F0000050040009901",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000050080009401",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000100030006302",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000230040008201",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000300030010801",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000450040001102",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000450060009401",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000450130009502",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000450140005102",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000450140007102",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000460150003401",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000510110001802",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000540290004701",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000600430003701",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000620040059001",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000620070052201",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000620180017202",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000620230056201",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000020160003301",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000110070004301",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000120040010401",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000320010013701",]$Lien = "Réf" # coloc étudiante
    PER[PER$uid_PER == "BRH2009F0000330070004301",]$Lien = "Réf" # enfant à la place de PRéf
    PER[PER$uid_PER == "BRH2009F0000340090006601",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000340090012101",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000370130004501",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000410260003701",]$Lien = "Réf" # enfant à la place de Réf
    PER[PER$uid_PER == "BRH2009F0000480030003901",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000490150003001",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000510330001701",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000530810007501",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000560070002202",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000590190001601",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000590190001602",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000630070014502",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000670290004401",]$Lien = "Réf"
    # Clermont-Ferrand
    PER[PER$uid_PER == "CLF2012T0002150070031701",]$Lien = "Réf"
    # Thionville
    PER[PER$uid_PER == "THI2012T0000200090014203",]$Lien = "Réf"
    # Le Creusot
    PER[PER$uid_PER == "LCS2012T0001060030006301",]$Lien = "Réf"
    PER[PER$uid_PER == "LCS2012T0001060040006401",]$Lien = "Réf"
    # Douai
    PER[PER$uid_PER == "DOU2012F0000110060002701",]$Lien = "Réf"
    # Quimper
    PER[PER$uid_PER == "QMP2013T0000020020005102",]$Lien = "Réf"
    PER[PER$uid_PER == "QMP2013T0000030050121301",]$Lien = "Réf" # autre ou famille : n'est pas son enfant ?
    PER[PER$uid_PER == "QMP2013T0000030050121302",]$Lien = "Autre ou Famille"
    # Roche sur Yon
    PER[PER$uid_PER == "RSY2013T0000030030008901",]$Lien = "Réf"
    
    # ~ Erreurs manifestes : 2 personnes de référence
    # Solution adoptée : un des deux adultes est identifié comme "conjoint.e" plutôt que comme "référent.e"
    # selon les mêmes règles d'attribution que pour la section précédente (activité > PCS > diplôme et/ou âge > ID)
    # Douai
    PER[PER$uid_PER == "DOU2012F0000080020006902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "DOU2012F0000170120007802",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "DOU2012F0000080020006902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "DOU2012F0000170120007802",]$Lien = "Conjoint.e"
    # Valenciennes
    PER[PER$uid_PER == "VLN2011F0000010030012902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000320150002702",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000060170001001",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000080180015302",]$Lien = "Conjoint.e" # ? aussi jeune que l'enfant
    PER[PER$uid_PER == "VLN2011F0000190030004102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000190030011302",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000370030009302",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "VLN2011F0000400030012102",]$Lien = "Conjoint.e"
    # Bordeaux
    PER[PER$uid_PER == "BOR2009F0000010020021402",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000030020000802",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000030050009202",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000050010002602",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000050030023702",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000050080025102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000070010007402",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000070030004102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000100040002802",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000140020017202",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000150020011202",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000160010007602",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000200010002302",]$Lien = "Conjoint.e" # identique au 01 : pptés écrasées ?
    PER[PER$uid_PER == "BOR2009F0000240030012602",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000250010013101",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000260100018501",]$Lien = "Autre ou Famille" # gros écart d'âge 69/39
    PER[PER$uid_PER == "BOR2009F0000330050011301",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000350040009802",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000350100011702",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000360050010502",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000370030004002",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000410020017801",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000420010011902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000430070005002",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000460010006002",]$Lien = "Autre ou Famille" # écart d'âge 60+/37
    PER[PER$uid_PER == "BOR2009F0000520050000902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000660050008001",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000700020017701",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000730030008902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000760020015102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000760030011102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000810020005102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000810020005102",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BOR2009F0000810020012601",]$Lien = "Conjoint.e"
    # Strasbourg / Bas-Rhin
    PER[PER$uid_PER == "BRH2009F0000060100000202",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000070120003602",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000030020008302",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000090140007801",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000090140009701",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000130090011202",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000210110011001",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000230050007402",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000390010008001",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000440500010901",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000500260050601",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000580410005002",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000610790002401",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000620100001902",]$Lien = "Conjoint.e"
    PER[PER$uid_PER == "BRH2009F0000650030002401",]$Lien = "Conjoint.e"
    
    # ~ Erreurs manifestes : Personnes seules, mais pas de référence : des disparu.es ?
    # Solution : rien ne permet d'être sûr que ces personnes se trouvaient dans un ménage comptant plusieurs personnes,
    # bien qu'elles n'apparaissent pas dans la base. Ces personnes sont donc classées comme référent.es
    # Bordeaux
    PER[PER$uid_PER == "BOR2009F0000010030009201",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000020040022801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000020050012301",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000020050014401",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000030020007001",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000090070002101",]$Lien = "Réf" #forte suspicion : "foyer"
    PER[PER$uid_PER == "BOR2009F0000110030026301",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000120090019501",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000270050011801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000280040007301",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000340010014401",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000600040009601",]$Lien = "Réf" # foyer ?
    PER[PER$uid_PER == "BOR2009F0000660060004101",]$Lien = "Réf" # foyer ?
    PER[PER$uid_PER == "BOR2009F0000700040017801",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000700040019901",]$Lien = "Réf"
    PER[PER$uid_PER == "BOR2009F0000710010012901",]$Lien = "Réf"
    # Strasbourg / Bas-Rhin
    PER[PER$uid_PER == "BRH2009F0000140050003001",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000150060007902",]$Lien = "Réf" # suspicion : code 02
    PER[PER$uid_PER == "BRH2009F0000210110004801",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000350060005701",]$Lien = "Réf" # enfant seul
    PER[PER$uid_PER == "BRH2009F0000440070002001",]$Lien = "Réf"
    PER[PER$uid_PER == "BRH2009F0000500040000801",]$Lien = "Réf"
    # Le Havre
    PER[PER$uid_PER == "HAV2018F0000010020000501",]$Lien = "Réf"
    # Saint-Brieuc
    PER[PER$uid_PER == "STB2012T0000220010075401",]$Lien = "Réf"
    # Tours
    PER[PER$uid_PER == "TOU2019T2510080002084601",]$Lien = "Réf"
    
    # ~ Erreurs probables : des PRéf choisies parmi des enfants sans emploi
    # Des enfants du ménage sont marqués comme référents alors qu'ils ne travaillent pas. En revanche, ils sont dans
    # un ménage où sont présent.es des parents actifs qui travaillent (souvent tous deux marqué.es comme conjoint.es,
    # ce qui génère des erreurs). Solution : un.e des conjoint.es est marqué.e comme référent.e, les enfants comme
    # "enfants" (rien ne prouve le lien de parenté, mais ça correspond à la situation la plus commune)
    # Thionville  
    PER[PER$uid_PER == "THI2012T0000080090024702",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000080090024704",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000090020003702",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000090020003703",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000100030025002",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000100030025003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000100040005101",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000100040005103",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000110050015102",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000110050015104",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000120080046501",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000120080046504",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000150010026701",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000150010026703",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000160020039601",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000160020039603",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000170060021601",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000170060021604",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000190010027801",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000190010027803",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000200080044002",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000200080044003",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000210020039601",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000210020039603",]$Lien = "Enfant"
    PER[PER$uid_PER == "THI2012T0000210050007501",]$Lien = "Réf"
    PER[PER$uid_PER == "THI2012T0000210050007503",]$Lien = "Enfant"
    # Strasbourg / Bas-Rhin
    PER[PER$uid_PER == "BRH2009F0000580280003401",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000580280003402",]$Lien = "Réf"
    # Toulouse
    PER[PER$uid_PER == "TOU2019T1350130002031701",]$Lien = "Autre ou Famille" #hard to guess
    
    # ~ Saisies incommodes : des couples identifiés comme conjoint.es, mais la Réf est leur fille/fils
    # Même cas que précédemment, mais cette fois ci le fait d'avoir marqué l'enfant comme référent semble légitime
    # (eux ne travaillent plus, leurs enfants sont actif.ves). Solution : l'enfant est considéré comme le "coeur de
    # ménage" qui servira à calculer la PCSM. Les parents conjoint.es sont passés en "famille" (ou Autre ou Fam quand
    # cela semble moins évident, ménages complexes, risque de calquer la norme)
    # Thionville
    PER[PER$uid_PER == "THI2012T0000040030132301",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000040030132302",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000080030032301",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000160010031001",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000160010031002",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000080030032302",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000200090014201",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000200090014202",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000200090014204",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000220080006101",]$Lien = "Famille"
    PER[PER$uid_PER == "THI2012T0000220080006102",]$Lien = "Famille"
    # Valenciennes
    PER[PER$uid_PER == "VLN2011F0000040090005803",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "VLN2011F0000040090005804",]$Lien = "Autre ou Famille"
    # Strasbourg / Bas-Rhin
    PER[PER$uid_PER == "BRH2009F0000030120009402",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000030120009403",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000450010005701",]$Lien = "Famille"
    PER[PER$uid_PER == "BRH2009F0000450010005702",]$Lien = "Famille"
    PER[PER$uid_PER == "BRH2009F0000570420005603",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000570420005604",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000420270008501",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000420270008502",]$Lien = "Autre ou Famille"
    PER[PER$uid_PER == "BRH2009F0000460010001901",]$Lien = "Famille"
    PER[PER$uid_PER == "BRH2009F0000460010001902",]$Lien = "Famille"
    PER[PER$uid_PER == "BRH2009F0000460010001903",]$Lien = "Réf"
    
    # ~ Cas particuliers non-arbitrables
    # ménage MON2014F00002700100081 : deux "conjointes" de 44 ans, cas isolé dans l'enquête (peu d'autres erreurs)
    # rien ne permet de savoir qui est la "vraie" conjoint.e, ni même s'il ne s'agit pas tout simplement d'un couple
    # à trois ! Solution : la 2e épouse est passée en "membre de la famille", pour pouvoir calculer la PCSM
    PER[PER$uid_PER == "MON2014F0000270010008103",]$Lien = "Famille"
    
    # ~ Ménages dont il manque visiblement des membres (!!!)
    # Les personnes correspondant à ces ménages font apparaître des "trous" dans la composition du ménage : manque
    # un.e référent.e, non continuité dans les ID, il manque visiblement des individus
    # On ne peut rien faire d'autre que les supprimer
    # Besançon
    listeBesMqt = c("BES2018F00010100200126", "BES2018F00010200100124", "BES2018F00010200100136",
                    "BES2018F00010400300004", "BES2018F00010600300072", "BES2018F00010900100116", # ?
                    "BES2018F00011300100094", "BES2018F00011600200132", "BES2018F00011700100036",
                    "BES2018F00011900300100", "BES2018F00012200900076", "BES2018F00012400800110")
    # Le Havre
    listeHavMqt = c("HAV2018F00000400100026", "HAV2018F00000500100026", "HAV2018F00000600100132",
                    "HAV2018F00000600600037", "HAV2018F00000600800013", "HAV2018F00000800100015",
                    "HAV2018F00001200300056", "HAV2018F00001700200056", "HAV2018F00001700400054",
                    "HAV2018F00002000400043", "HAV2018F00001000500015")
    PER = filter(PER, !uid_MEN %in% listeBesMqt)
    PER = filter(PER, !uid_MEN %in% listeHavMqt)
    
    # D'autres erreurs sont peut-être présent.es mais difficiles à débusquer + elles n'affectent pas le calcul
    # de la PCSM, sinon par omission d'information ou information faussée.
    # Rétablissement des codes initiaux de Lien        
    PER$Lien = plyr::revalue(PER$Lien, c("Réf"="1",
                                         "Conjoint.e"="2",
                                         "Enfant"="3",
                                         "Co/Locataire"="4",
                                         "Famille"="5",
                                         "Autre"="6",
                                         "Autre ou Famille"="7"), warn_missing = F)
    return(PER)
}

patch_facteurs = function(base)
{
    # Assure que les variables suivantes (présentes dans MEN ou PER) sont bien des facteurs
    # et ordonnés dans le bon ordre
    
    if ("PCSM"      %in% colnames(base)) { base$PCSM      = as.factor(base$PCSM)      }
    if ("PCSML"     %in% colnames(base)) { base$PCSML     = as.factor(base$PCSML)     }
    if ("MenTypo"   %in% colnames(base)) { base$MenTypo   = as.factor(base$MenTypo)   }
    if ("MenNivEtu" %in% colnames(base)) { base$MenNivEtu = as.factor(base$MenNivEtu) }
    if ("MenCouple" %in% colnames(base)) { base$MenCouple = as.factor(base$MenCouple) }
    if ("MenEnfants"%in% colnames(base)) { base$MenEnfants= as.factor(base$MenEnfants)}

    if ("LogType" %in% colnames(base))
    {
        base$LogType = factor(base$LogType, levels = c("1", "2", "3", "4", "5"))
    }
    if ("NivEtu" %in% colnames(base))
    {
        base$NivEtu = factor(base$NivEtu, levels=c("00", "10", "20", "30", "31", "32", "33",
                                                   "40", "41", "45", "50", "90"))    
    }
    if ("Activ" %in% colnames(base))
    {
        base$Activ = factor(base$Activ, levels=c("10", "11", "12", "21", "22", "31", "32", "33"))
    }

    if ("PCS8" %in% colnames(base))
    {
        if (!is.factor(base$PCS8)){
            base$PCS8= ifelse(base$PCS8 == "00", NA, as.character(base$PCS8)) %>% as.factor()
            base$PCS8= factor(base$PCS8, levels=c("01", "02", "03", "04", "05", "06", "07", "08", "09"))
        }
    }
    
    if ("Lien" %in% colnames(base))
    {
        base$Lien= factor(base$Lien, levels=c("1", "2", "3", "4", "5", "6", "7"))
    }
    
    if ("PCSMT" %in% colnames(base))
    {
        base$PCSMT = factor(base$PCSMT, levels= c("1", "2", "3AB", "3C", "4", "5", "6", "7e", "7i"))
    }

    return(base)
}

patch_deplZero = function(DEP)
{
    nInit = nrow(filter(DEP, Duree == 0))
    DEP = mutate(DEP, Duree = ifelse(Duree == 0, diffHeures(O_Hr, D_Hr), Duree))
    nFin = nrow(filter(DEP, Duree==0))
    rapport(nInit-nFin, "cas de durées de déplacement nulles corrigées", info=T)
    return(DEP)
}

patch_doublonsZF = function(shp_ZF)
{
    # Il s'avère que certaines ZF proches les unes des autres ont le même code dans la géom d'Aurélie
    lZ = as.data.frame(table(shp_ZF$CODE_ZF)) %>% filter(Freq>1)
    lZ = lZ$Var1
    
    if (length(lZ > 0)) { rapport(length(lZ), "doublons de CODE_ZF détectés dans ZF") } else { return(shp_ZF) }
    
    # Fusionnons-les, mais à part du reste, pour éviter que ça prenne trop de temps
    shp_ZF_fus = filter(shp_ZF, CODE_ZF %in% lZ)
    shp_ZF = filter(shp_ZF, !CODE_ZF %in% lZ)
    shp_ZF_fus = shp_ZF_fus %>% group_by(CODE_ZF) %>% summarise(across(where(is.num_char), first))
    shp_ZF = rbind(shp_ZF, shp_ZF_fus)
    
    rapport("Problème corrigé", info=T)
    
    return(shp_ZF)
}


# Intercompatibilité =====

convert_EMPtoEMD_MEN = function(MEN_EMP, MEN_EMP_equi)
{
    MEN_EMP = MEN_EMP %>%
        left_join(select(MEN_EMP_equi, -Coeff), by="uid_MEN") %>%
        mutate(Enq = "EMP", EnqRec = "1", EnqAnnee = "2009",
               ZF = NA, Ech = NA) %>%
        mutate(Com = paste0(Dep, "000"), ZT = paste0("DP", Dep)) %>%
        mutate(LogType = NA,
               LogOcc = case_when(LogOcc == "1"                                             ~ "10",
                                  LogOcc == "2"                                             ~ "10",
                                  LogOcc == "3"                                             ~ "99",
                                  LogOcc == "4" & LogPropri %in% c("2", "3")                ~ "21",
                                  LogOcc == "4" & LogPropri %in% c("1", "4", "5", "6", "7") ~ "20",
                                  LogOcc == "5"                                             ~ "30"),
               LogTel = NA, LogAnnuaire = NA,
               LogInternet = NA,
               Veh1_Typ = NA, Veh1_Eng = NA, Veh1_Ann = NA, Veh1_Psc = NA, Veh1_Pro = NA, Veh1_Dsp = NA, Veh1_Pli = NA, Veh1_Pty = NA,
               Veh2_Typ = NA, Veh2_Eng = NA, Veh2_Ann = NA, Veh2_Psc = NA, Veh2_Pro = NA, Veh2_Dsp = NA, Veh2_Pli = NA, Veh2_Pty = NA,
               Veh3_Typ = NA, Veh3_Eng = NA, Veh3_Ann = NA, Veh3_Psc = NA, Veh3_Pro = NA, Veh3_Dsp = NA, Veh3_Pli = NA, Veh3_Pty = NA,
               Veh4_Typ = NA, Veh4_Eng = NA, Veh4_Ann = NA, Veh4_Psc = NA, Veh4_Pro = NA, Veh4_Dsp = NA, Veh4_Pli = NA, Veh4_Pty = NA,
               DrmN = DrmN_moto + DrmN_cyclo,
               Drm1_Typ = NA, Drm1_Psc = NA, Drm1_Eng = NA, Drm1_Ann = NA, Drm1_Pli = NA, Drm1_Pty = NA,
               Drm2_Typ = NA, Drm2_Psc = NA, Drm2_Eng = NA, Drm2_Ann = NA, Drm2_Pli = NA, Drm2_Pty = NA,
               Drm3_Typ = NA, Drm3_Psc = NA, Drm3_Eng = NA, Drm3_Ann = NA, Drm3_Pli = NA, Drm3_Pty = NA,
               Drm4_Typ = NA, Drm4_Psc = NA, Drm4_Eng = NA, Drm4_Ann = NA, Drm4_Pli = NA, Drm4_Pty = NA,
               VelN = VelN_adu + VelN_enf, VelElN = NA, VelParc = NA,
               EnqMeth = "F", EnqLieu = "EMP", filtre.Geom = F,
               LogCrit_A = NA, LogCrit_B = NA, LogComPrec = NA, LogOccPrec = NA,
               # les catégories de commune sont celles définies par l'Insee, rien à changer
               ZoneRang = ZoneRang, ZonePosi = ZonePosi, ZoneDens = ZoneDens)
    
    return(MEN_EMP)
}

convert_EMPtoEMD_PER = function(PER_EMP, MEN_EMP, PERK_EMP, supprWeekEnd = T)
{
    PER_EMP = PER_EMP %>%
        mutate(uid_MEN = paste0("EMP2019", "##", uid_MEN),
               uid_PER = paste0("EMP2019", "##", uid_PER),
               uid_ENQ = "EMP2019",
               EnqAnnee = "2019",
               ZF = NA)
    
    PERK_EMP = PERK_EMP %>%
        mutate(uid_PER = paste0("EMP2019##", uid_PER)) %>%
        select(-id_PER, -uid_MEN)
    
    PER_EMP = left_join(PER_EMP, PERK_EMP, by="uid_PER")
    
    PER_EMP = left_join(PER_EMP, select(MEN_EMP, uid_MEN, Dep), by="uid_MEN") %>%
        mutate(Com = paste0(Dep, "000"), ZT = paste0("DP", Dep))
    
    
    # Problème : dans les EMP, une personne interrogée par ménage.
    #            dans les EMD, il y a une ligne par personne, même quand non interrogée.
    # On va artificiellement créer les autres membres du ménage à l'aide des "liens", ce qui
    # va démultiplier le nombre de personnes, mais qu'importe. L'uid_PER ne sera pas celui des EMP,
    # mais un identifiant unique que je crée moi-même.
    # Pour chaque ménage, on aura une ligne par personne dans PER.
    
    PER_EMP = pivot_longer(PER_EMP, cols=Lien01:Lien10, names_to = "Lien_Qui", values_to = "Lien_Quoi") %>%
        filter(!is.na(Lien_Quoi)) %>% # Une ligne par personne du ménage
        left_join(select(MEN_EMP, uid_MEN, PerRef, PerCj), by="uid_MEN") %>%
        mutate(Lien_Qui = substr(Lien_Qui,5,6),
               PerRef = as.character(PerRef), PerCj = as.character(PerCj),
               PerRef = ifelse(nchar(PerRef)<2, paste0("0", PerRef), PerRef),
               PerCj  = ifelse(nchar(PerCj )<2, paste0("0", PerCj ), PerCj ))
    
    # Etape 1 : qui est la PR ? Qui est son ou sa conjointe ?
    PER_EMP = mutate(PER_EMP, LienEMD = case_when(PerRef == Lien_Qui ~ "1"))
    #                                               PerCj  == Lien_Qui ~ "2"))
    
    # Etape 2 : Ego peut connaître son lien avec la PR (il suffit de l'inverser)
    PER_EMP = left_join(PER_EMP, select(filter(PER_EMP, LienEMD == "1"), uid_MEN, Lien_Qui, Lien_Quoi),
                        by=c("uid_MEN" = "uid_MEN", "PerRef" = "Lien_Qui"),
                        suffix=c("", "_PerRef"))
    
    # Etape 3 : chacun peut connaître son lien avec Ego, puis le lien de Ego à la PR, et en déduire sa position
    PER_EMP = left_join(PER_EMP, select(filter(PER_EMP, Lien_Quoi == "00"), uid_MEN, Lien_Quoi_PerRef),
                        by=c("uid_MEN" = "uid_MEN"),
                        suffix=c("", "_viaEgo"))

    # Je reconnais les différents cas : frère/soeur de l'enfant de la PR, parent de la PR, etc.
    # Je pars du principe que les EMD ne font pas de différence si les enfants sont les enfants
    # d'un des deux conjoints (ex, les enfants du conjoint sont-ils comptés comme "autres" quand
    # les enfants de la PR sont comptés comme tels ?)
    # D'après l'Ined, 9% des familles sont recomposées, or 1,1% des familles dans les EMD comportent
    # des personnes mineures considérées comme d'autres membres de la famille (et ce peuvent être 
    # des petits-enfants).
    
    PER_EMP = mutate(PER_EMP, LienEMD = case_when(PerRef == Lien_Qui ~ "1",
                                                  PerCj  == Lien_Qui ~ "2",
                                                  Lien_Quoi == "00" & Lien_Quoi_PerRef == "01" ~ "2",
                                                  Lien_Quoi == "00" & Lien_Quoi_PerRef == "02" ~ "3",
                                                  Lien_Quoi == "00" & Lien_Quoi_PerRef %in%
                                                      c("03", "10", "21", "22", "31", "32", "40", "50") ~ "5",
                                                  Lien_Quoi == "00" & Lien_Quoi_PerRef == "60" ~ "4",
                                                  Lien_Quoi == "00" & Lien_Quoi_PerRef == "90" ~ "6",
                                                  Lien_Quoi == "10" & Lien_Quoi_PerRef_viaEgo == "02" ~ "3",
                                                  Lien_Quoi == "02" & Lien_Quoi_PerRef_viaEgo %in% c("00", "01") ~ "3",
                                                  Lien_Quoi == "03" & Lien_Quoi_PerRef_viaEgo %in% c("00", "01", "03") ~ "5",
                                                  Lien_Quoi == "10" & Lien_Quoi_PerRef_viaEgo == "00" ~ "5",
                                                  Lien_Quoi == "31" & Lien_Quoi_PerRef_viaEgo %in% c("00", "01") ~ "5",
                                                  Lien_Quoi == "32" & Lien_Quoi_PerRef_viaEgo %in% c("00", "01") ~ "5",
                                                  Lien_Quoi == "21" & Lien_Quoi_PerRef_viaEgo == "02" ~ "5",
                                                  Lien_Quoi == "50" & Lien_Quoi_PerRef_viaEgo %in% c("00", "01", "02") ~ "5",
                                                  Lien_Quoi %in%c("03","10") & Lien_Quoi_PerRef_viaEgo == "31" ~ "5",
                                                  Lien_Quoi == "01" & Lien_Quoi_PerRef_viaEgo == "40" ~ "5",
                                                  Lien_Quoi %in%c("02","10") & Lien_Quoi_PerRef_viaEgo == "21" ~ "5",
                                                  Lien_Quoi == "22" & Lien_Quoi_PerRef_viaEgo %in% c("00","01","03","32") ~ "5",
                                                  Lien_Quoi == "01" & Lien_Quoi_PerRef_viaEgo %in% c("02","03","31","32") ~ "5",
                                                  Lien_Quoi == "02" & Lien_Quoi_PerRef_viaEgo %in% c("02", "10", "21", "22", "31", "32", "40", "50") ~ "5",
                                                  Lien_Quoi == "10" & Lien_Quoi_PerRef_viaEgo %in% c("10", "50") ~ "5",
                                                  Lien_Quoi == "60" ~ "6",
                                                  Lien_Quoi == "50" ~ "5",
                                                  Lien_Quoi == "90" ~ "6"),
                     Ego = case_when(Lien_Quoi == "00" ~ 1, Lien_Quoi != "00" ~ 0))
    
    # Si on a des infos sur la personne de référence ou le/la conjoint·e et que l'individu est
    # l'une d'elles, on applique ces infos.
    # Sinon, on supprime les infos qui ne concernent que l'individu Ego.
    
    PER_EMP = left_join(PER_EMP, select(MEN_EMP, uid_MEN, starts_with("PerRef"), starts_with("PerCj")), by="uid_MEN")
    
    # Anciens champs, présents dans les EMD :
    PER_EMP = mutate(PER_EMP,
                     Genre = case_when(Ego == 1 ~ Genre, LienEMD == "1" ~ PerRef_Genre,
                                       LienEMD == "2" ~ PerCj_Genre),
                     Age   = case_when(Ego == 1 ~ Age,   LienEMD == "1" ~ PerRef_Age,
                                       LienEMD == "2" ~ PerCj_Age),
                     Tel = NA, Mail = NA, Permis = NA,
                     NivEtu = case_when(Ego == 1 ~ NivEtu,
                                        LienEMD == "1" ~ PerRef_NivEtu,
                                        LienEMD == "2" ~ PerCj_NivEtu),
                     Activ = case_when(Ego == 1 ~ Activ,
                                       LienEMD == "1" ~ PerRef_Activ,
                                       LienEMD == "2" ~ PerCj_Activ),
                     ActivType = case_when(Ego == 1 ~ ActivType),
                     ActivTemps = case_when(Ego == 1 ~ ActivTemps),
                     PCS42 = case_when(Ego == 1 ~ PCS42,
                                       LienEMD == "1" ~ PerRef_PCS42,
                                       LienEMD == "2" ~ PerCj_PCS42),
                     AboTC = NA, TravDom = NA)
    
    # Nouveaux champs propres aux EMP : Nat, LieuNsc, Handi
    PER_EMP = mutate(PER_EMP,
                     LieuNsc = case_when(Ego == 1       ~ as.character(ImmigrPays),
                                         LienEMD == "1" ~ PerRef_LieuNsc,
                                         LienEMD == "2" ~ PerCj_LieuNsc),
                     Nat = case_when(Ego == 1 ~ Nat,
                                     LienEMD == "1" ~ PerRef_Nat,
                                     LienEMD == "2" ~ PerCj_Nat),
                     Handi = case_when(Ego == 1 ~ Handi))
    
    print(table(PER_EMP$Activ, PER_EMP$ActivTemps))
    
    # Recodage format EMP → Format EMD
    PER_EMP = mutate(PER_EMP,
                     Genre = plyr::revalue(Genre, c("1" = "H", "2" = "F")),
                     NivEtu = plyr::revalue(NivEtu, c("7" = "00",
                                                      "6" = "20",
                                                      "5" = "32",
                                                      "4" = "40", 
                                                      "3" = "46",
                                                      "1" = "50")),
                     Activ = case_when(Activ == "1" & ActivTemps == "1" ~ "10",
                                       Activ == "1" & ActivTemps == "2" ~ "11",
                                       Activ == "1" & is.na(ActivTemps) ~ "19",
                                       Activ == "2"                     ~ "12",
                                       Activ == "3" & NivEtu %in% c("00", "10", "32") ~ "21",
                                       Activ == "3" & NivEtu %in% c("40", "46", "50") ~ "22",
                                       Activ == "4"                     ~ "31",
                                       Activ == "5"                     ~ "32",
                                       Activ == "6"                     ~ "33"),
                     Activ = factor(as.character(Activ), levels=c("10", "11", "12", "19", "21", "22", "31", "32", "33")),
                     PCS8 = NA,
                     PCS8 = case_when(PCS42 %in% c("11", "12", "13", "71") ~ "01",
                                      PCS42 %in% c("21", "22", "23", "72") ~ "02",
                                      PCS42 %in% c("31", "33", "34", "35", "37", "38", "74") ~ "03",
                                      PCS42 %in% c("42", "43", "44", "45", "46", "47", "48", "75") ~ "04",
                                      PCS42 %in% c("52", "53", "54", "55", "56", "77") ~ "05",
                                      PCS42 %in% c("62", "63", "64", "65", "67", "68", "69", "78") ~ "06",
                                      PCS42 == "84" ~ "07",
                                      PCS42 == "81" ~ "08",
                                      PCS42 %in% c("83", "85", "86") ~ "09"),
                     PCS42S = case_when(PCS42 == "00" ~ "00",
                                        PCS42 %in% c("11", "12", "13") ~ "10",
                                        PCS42 == "21" ~ "21",
                                        PCS42 == "22" ~ "22",
                                        PCS42 == "23" ~ "23",
                                        PCS42 == "31" ~ "31",
                                        PCS42 %in% c("33", "34", "35") ~ "32",
                                        PCS42 %in% c("37", "38") ~ "36",
                                        PCS42 %in% c("42", "43", "44", "45") ~ "41",
                                        PCS42 == "46" ~ "46",
                                        PCS42 == "47" ~ "47",
                                        PCS42 == "48" ~ "48",
                                        PCS42 %in% c("52", "53") ~ "51",
                                        PCS42 == "54" ~ "54",
                                        PCS42 == "55" ~ "55",
                                        PCS42 == "56" ~ "56",
                                        PCS42 %in% c("62", "63", "64", "65") ~ "61",
                                        PCS42 %in% c("67", "68") ~ "66",
                                        PCS42 == "69" ~ "69",
                                        PCS42 == "81" ~ "81",
                                        PCS42 == "83" ~ "82",
                                         # impossible de distinguer collégien·nes/scolaires
                                        PCS42 == "84" & NivEtu == "20" ~ "85",
                                        PCS42 == "84" & NivEtu == "32" ~ "89",
                                        PCS42 == "84" & NivEtu == "40" ~ "80",
                                        PCS42 == "84" & NivEtu == "46" ~ "87",
                                        PCS42 == "84" & NivEtu == "50" ~ "88"),
                     NivEtu = ifelse(PCS8 == "07", 90, NivEtu),
                     NivEtu = factor(NivEtu, levels = sort(unique(NivEtu))),
                     PCS8 =   factor(PCS8,   levels = sort(unique(PCS8))),
                     PCS42S = factor(PCS42S, levels = sort(unique(PCS42S))))
    
    # On peut oublier le "Lien" initial
    PER_EMP$Lien = PER_EMP$LienEMD
    
    # Télétravail
    PER_EMP = mutate(PER_EMP, Travail_Tele = plyr::revalue(Teletrav,
                                                           c("1" = "3", "2" = "3", "3" = "2", "4" = "2", "5" = "1")))
    
    # Distance au lieu de travail principal ... on suppose que c'est bien à vol d'oiseau dans les EMD
    PER_EMP = mutate(PER_EMP, Travail_Dis = Travail_DisVO * 1000)
    
    # Immobilité le jour d'enquête ?
    PER_EMP = PER_EMP %>% mutate(VeilleDepl = case_when(Delai == 1 & Immobilite_j1 == "1" ~ "2",
                                                        Delai == 1 & Immobilite_j1 == "0" ~ "1",
                                                        Delai == 2 & Immobilite_j2 == "1" ~ "2",
                                                        Delai == 2 & Immobilite_j2 == "0" ~ "1",
                                                        Delai == 3 & Immobilite_j3 == "1" ~ "2",
                                                        Delai == 3 & Immobilite_j3 == "0" ~ "1",
                                                        Delai == 4 & Immobilite_j4 == "1" ~ "2",
                                                        Delai == 4 & Immobilite_j4 == "0" ~ "1",
                                                        Delai == 5 & Immobilite_j5 == "1" ~ "2",
                                                        Delai == 5 & Immobilite_j5 == "0" ~ "1",
                                                        Delai == 6 & Immobilite_j6 == "1" ~ "2",
                                                        Delai == 6 & Immobilite_j6 == "0" ~ "1",
                                                        Delai == 7 & Immobilite_j7 == "1" ~ "2",
                                                        Delai == 7 & Immobilite_j7 == "0" ~ "1"))
    
    # Les autres variables restent en blanc
    PER_EMP = mutate(PER_EMP,
                     Activ2 = NA,
                     PerEnq = NA,
                     Travail_ZF = NA,
                     Travail_Com = NA,
                     Travail_ZT = NA,
                     Travail_Voit = NA,
                     Travail_Parc = NA,
                     Travail_ParcVel = NA,
                     Fqc_Mch = NA,
                     Fqc_Vel = NA,
                     Fqc_Drm = NA,
                     Fqc_Vco = NA,
                     Fqc_Vpa = NA,
                     Fqc_Tco = NA,
                     VeilleTrav = NA)
    
    # Estimation de la date
    PER_EMP = left_join(PER_EMP, select(MEN_EMP, uid_MEN, enqEMP_vague), by="uid_MEN")
    
    PER_EMP = mutate(PER_EMP, EnqDate_A = case_when(enqEmp_vague %in% c("1", "2", "3", "4") ~ "2018",
                                                    enqEmp_vague %in% c("5", "6") ~ "2019"),
                     EnqDate_M = plyr::revalue(EnqDate_M, c("janvier" = "01", "février" = "02",
                                                            "mars" = "03", "avril" = "04",
                                                            "mai" = "05", "juin" = "06",
                                                            "juillet" = "07", "août" = "08",
                                                            "septembre" = "09", "octobre" = "10",
                                                            "novembre" = "11", "décembre" = "12")),
                     EnqDate_J = NA,
                     EnqDate_JS = as.factor(substr(EnqDate_JS, 1,3)),
                     EnqDate_JS = factor(EnqDate_JS, levels=c("lun", "mar", "mer", "jeu", "ven",
                                                              "sam", "dim")))
    
    if (supprWeekEnd)
    {
        # Il y a en fait plein de jours du weekend !!!
        PER_EMP = filter(PER_EMP, !EnqDate_JS %in% c("sam","dim"))
    }

    # Refactorisation d'AboTC au format EMD
    PER_EMP = mutate(PER_EMP, AboTC = plyr::revalue(as.factor(AboTC), c("1" = "6", "2" = "4", "3" = NA)))
    
    # Les coeffs sont, on va le supposer, les mêmes que pour MEN_EMP
    PER_EMP = left_join(PER_EMP, select(MEN_EMP, uid_MEN, Coeff), by="uid_MEN") %>%
        mutate(CoeffEnq = ifelse(Ego == 1, Coeff, NA))
    
    # On refait un uid_PER vraiment unique
    PER_EMP$uid_PER = paste0(PER_EMP$uid_MEN, PER_EMP$Lien_Qui)
    
    # Pas de Coeff dispo pour chaque membre du ménage ; à défaut, prendre le coeff du ménage...
    # PER_EMP = left_join(PER_EMP, select(MEN_EMP, uid_MEN, Coeff), by="uid_MEN")
    
    # Les champs surnuméraires seront évacués dans MAIN.R (tout champ non présent dans PER)
    
    # Note: problème avec le ménage 7400000412000 (2 conjointes → une mère et sa fille)
    # Je ne vois pas comment le résoudre
    # PER_EMP[PER_EMP$uid_PER == ""]
    
    return(PER_EMP)
}

convert_EMPtoEMD_DEP = function(DEP_EMP)
{
    DEP_EMP = DEP_EMP %>%
        mutate(uid_MEN = paste0("EMP2019", "##", uid_MEN),
               uid_PER = paste0("EMP2019", "##", uid_PER),
               uid_ENQ = "EMP2019",
               id_DEP = ifelse(nchar(as.character(id_DEP) == 1), paste0("0", as.character(id_DEP)),
                               as.character(id_DEP)),
               uid_DEP = paste0(uid_PER, id_DEP))
    
    # Transformation des codes Motif EMP en codes Motif EMD
    # Attention : pas sûr du plan 1.5 = 21
    numsEMP = c("1.1", "1.2", "1.3", "1.4", "1.5", "2.1", "2.2", "3.1", "4.1", "4.12",
                "5.1", "5.2", "6.1", "6.2", "6.3", "6.4", "7.1", "7.2", "7.3", "7.4" ,
                "7.5", "7.6", "7.7", "7.8", "8.1", "8.2", "8.3", "8.4", "9.1", "9.2" ,
                "9.3", "9.4", "9.5", "9999")
    numsEMD = c("01" , "02" , "02" , "2x" , "21" , "32" , "33" , "41" , "44" , "42"  ,
                "54" , "54" , "71" , "61" , "74" , "64" , "51" , "51" , "53" , "51"  ,
                "51" , "51" , "52" , "52" , "02" , "02" , "02" , "91" , "11" , "14"  ,
                "14" , "81" , "14" , "91")
    
    DEP_EMP = DEP_EMP %>%
        mutate(O_Motif = plyr::revalue(O_Motif, set_names(nm = numsEMP, x = numsEMD)),
               D_Motif = plyr::revalue(D_Motif, set_names(nm = numsEMP, x = numsEMD)),
               O_MotAc = NA, D_MotAc = NA)
        
        

    
    # Transformation des heures au format HHMM
    DEP_EMP = DEP_EMP %>%
              # On ajoute un 0 si le 0 initial n'est pas présent (4 à 9 heures)
        mutate(O_Hr = ifelse(nchar(O_Hr) == 7, paste0("0", O_Hr), O_Hr),
               D_Hr = ifelse(nchar(D_Hr) == 7, paste0("0", D_Hr), D_Hr),
              # On reformate à partir des positions fixes des caractères
               O_Hr = paste0(substr(O_Hr, 1,2), substr(O_Hr, 4,5)),
               D_Hr = paste0(substr(D_Hr, 1,2), substr(D_Hr, 4,5)))
    
    # # Passage des heures du matin (0 à 3) en heures ajoutées (24 à 27)
    # du sale, mais je suis trop fatigué pour faire autrement
    DEP_EMP = DEP_EMP %>% mutate(O_Hr = ifelse(substr(O_Hr,1,2) == "00", paste0("24", substr(O_Hr, 3,4)), O_Hr),
                                 O_Hr = ifelse(substr(O_Hr,1,2) == "01", paste0("25", substr(O_Hr, 3,4)), O_Hr),
                                 O_Hr = ifelse(substr(O_Hr,1,2) == "02", paste0("26", substr(O_Hr, 3,4)), O_Hr),
                                 O_Hr = ifelse(substr(O_Hr,1,2) == "03", paste0("27", substr(O_Hr, 3,4)), O_Hr),
                                 D_Hr = ifelse(substr(D_Hr,1,2) == "00", paste0("24", substr(D_Hr, 3,4)), D_Hr),
                                 D_Hr = ifelse(substr(D_Hr,1,2) == "01", paste0("25", substr(D_Hr, 3,4)), D_Hr),
                                 D_Hr = ifelse(substr(D_Hr,1,2) == "02", paste0("26", substr(D_Hr, 3,4)), D_Hr),
                                 D_Hr = ifelse(substr(D_Hr,1,2) == "03", paste0("27", substr(D_Hr, 3,4)), D_Hr))
    
    # Durée en minutes rondes
    DEP_EMP = DEP_EMP %>% mutate(Duree = as.integer(round(Duree, 0)))
    
    # ATTENTION : il y a des valeurs qui excèdent les bornes
    # Double-checker la durée
    DEP_EMP = DEP_EMP %>% mutate(DuChk = heureHHMMtoM(D_Hr) - heureHHMMtoM(O_Hr))
    
    # Si durée négative + heure de départ > 24, ou bien heure d'arrivée < 4, y'a un bug
    # On va faire au plus simple : couper tout ce qui dépasse
    if (nrow(filter(DEP_EMP, DuChk < 0) > 1)) { rapport("!!!", nrow(filter(DEP_EMP, DuChk<0)),
                                                        "erreurs de délimitation des horaires", info=T) }
    DEP_EMP = filter(DEP_EMP, DuChk >= 0)
    
    # Distances en mètres ronds
    DEP_EMP = DEP_EMP %>%
        mutate(Dis  = as.integer(round(Dis * 1000, 0)),
               Dis_V = as.integer(round(DisV* 1000, 0)))
    
    # Mode principal en format EMD
    numsEMP = c("1.1","1.2","1.3","1.4","2.1","2.2","2.3","2.4","2.5","2.6","2.7",
                "3.1","3.2","3.3","3.4","4.1","4.2","4.3","4.4","5.1","5.2","5.3",
                "5.4","5.5","5.6","5.7","5.8","5.9","5.10",
                "6.1","6.2","6.3","6.4","6.5","7.1","7.2","7.3","8.1","9.1")
    numsEMD = c("01" ,"95" ,"97" ,"94" ,"11x","10x","13" ,"14" ,"15" ,"16" ,"95" ,
                "21" ,"22" ,"21" ,"95" ,"63" ,"61" ,"71" ,"41" ,"31" ,"91" ,"43" ,
                "41" ,"41" ,"32" ,"33" ,"53" ,"52" , NA  ,
                "51" ,"51" ,"53" ,"53", "54" ,"92" ,"92" ,"92" ,"91" ,"94")
    DEP_EMP = DEP_EMP %>%
        mutate(ModeP = plyr::revalue(ModeP, set_names(nm = numsEMP, x = numsEMD)))
    
    # Envie de garder le champ "pluie" !
    DEP_EMP = DEP_EMP %>%
        mutate(Pluie = case_when((O_Pluie >  0 & O_Pluie < 5) | (D_Pluie >  0 & D_Pluie < 5) ~ "jour pluie <5 mm",
                                  O_Pluie >= 5                |  D_Pluie >= 5                ~ "jour pluie ≥5 mm",
                                  O_Pluie == 0 & D_Pluie == 0                                ~ "jour sans pluie"),
               Pluie = as.factor(Pluie))
    
    # Champs vides
    DEP_EMP = DEP_EMP %>%
        mutate(O_Com = NA, O_ZT = NA, O_ZF = NA, D_Com = NA, D_ZT = NA, D_ZF = NA,
               nArrets = NA, IdBcl = NA, IdBcl_Rang = NA, nTrj = NA, HrSol = NA, idDep = id_DEP)
    
    return(DEP_EMP)
    
}

convert_EMPtoEMD_VEH = function(VEH_EMP)
{
    # On met tous les champs au format EMD et on bouche les tous
    
    VEH_EMP = VEH_EMP %>%
        mutate(uid_VEH = paste0("EMP2019", "##", uid_MEN, "-", id_VEH),
               Type = NA,
               Energie = plyr::revalue(Energie, c("1" = "S/plomb", "2" = "Diesel",
                                                  "3" = "Gaz", "4" = "Hybride", "5" = "Électrique",
                                                  "6" = NA)),
               Age = 2019 - Annee,
               Propr = plyr::revalue(Propri, c("1" = "oui", "2" = "non", "3" = "non", "4" = "non",
                                         "5" = "non", "6" = "non")),
               ProprAutre = NA, # on ne peut pas savoir si l'entité est employeur ou non
               ParkingTp = plyr::revalue(Parking, c("1" = "garage", "2" = "ouvert", "3" = "ouvert",
                                            "4" = "rue", "5" = "rue", "6" = "garage")),
               ParkingType = plyr::revalue(Parking, c("1" = NA, "2" = NA, "3" = NA, "4" = "gratuit",
                                                "5" = "payant", "6" = "gratuit")),
               Coeff = CoeffVeh,
               Conso100 = ifelse(Conso100 == 9999, NA, Conso100))
    
    return(VEH_EMP)
}

# Vérifications d'erreurs ====

verif_PCS = function(PER, pdf = NULL)
{
    t1 = table(PER$uid_ENQ, is.na(PER$PCS42S)) %>% as.data.frame()
    t2 = table(PER$uid_ENQ, is.na(PER$PCS8)) %>% as.data.frame()
    t3 = table(PER$EnqAnnee,is.na(PER$PCS42S)) %>% as.data.frame()
    
    t1$Var1 = factor(t1$Var1, levels = unique(t1[order(t1$Var2, t1$Freq),]$Var1))
    t2$Var1 = factor(t2$Var1, levels = unique(t2[order(t2$Var2, t2$Freq),]$Var1))
    
    if (!is.null(pdf)) {imprPdf(chemin = pdf)}
    viz_Barres(grph = as.data.frame(t1), titre = "Part des PCS détaillées connues par enquête",
               soustitre = "Base Unique EMD Cerema (2009~2019)", xlab = "Enquête",
               ylab = "Nombre de répondant.es",
               legende = "PCS détaillée connue",etiquettes=c("oui", "non"),
               couleurs = c("limegreen", "gold"), dejaMelt = T, stack=T)
    
    viz_Barres(grph = as.data.frame(t2), titre = "Part des PCS simples connues par enquête",
               soustitre = "Base Unique EMD Cerema (2009~2019)", xlab = "Enquête",
               ylab = "Nombre de répondant.es",
               legende = "PCS simple connue", etiquettes=c("oui", "non"),
               couleurs = c("limegreen", "gold"), dejaMelt = T, stack=T)
    
    viz_Barres(grph = as.data.frame(t3), titre = "Nombre d'enquêté.es et PCS détaillées par année",
               soustitre = "Base Unique EMD Cerema (2009~2019)", xlab = "Année de l'enquête",
               ylab = "Nombre de répondant.es",
               legende = "PCS détaillée connue",etiquettes=c("oui", "non"),
               couleurs = c("limegreen", "gold"), dejaMelt = T, stack=T)
    if (!is.null(pdf)) {off()}
}

verif_MenGeom = function(MEN)
{
    rapport("Vérification de la géométrie des enquêtes pour lesquelles un fichier géométrique est disponible")
    # Je vais essayer d'associer à chaque ménage une ZF de shp_ZF
    # Ça va permettre de voir si tous les ménages peuvent bien être localisés
    # Inutile d'essayer sur les ménages non compris dans le mobiliscope
    MEN.Mobi = filter(MEN, filtre.Geom == "O")
    rapport("Nombre d'enquêtes Mobiliscope :", length(unique(MEN.Mobi$uid_ENQ)), info=T) # ok
    shp_ZF$Association = "X" 
    MEN.Mobi = left_join(MEN.Mobi, select(shp_ZF, CODE_ZF, Association), by=c("ZF" = "CODE_ZF"))
    rapport("Part des ménages dont la ZF n'a pu être trouvée :",
            round(nrow(filter(MEN.Mobi, is.na(Association))) / nrow(MEN.Mobi) * 100, 2), "%", info=T) # Moins de 2% !
    return(nrow(filter(MEN.Mobi, is.na(Association))) / nrow(MEN.Mobi))
}

verif_doublonsMEN = function(refMEN)
{
    uidMEN = refMEN %>% group_by(uid_MEN) %>% summarize(N = n()) %>% filter(N != 1)
    rapport(nrow(uidMEN), "doublons d'UID trouvés parmi les Ménages.", info=T)
    return(uidMEN$uid_MEN)
}
verif_doublonsPER = function(refPER)
{
    uidPER = refPER %>% group_by(uid_PER) %>% summarize(N = n()) %>% filter(N != 1)
    rapport(nrow(uidPER), "doublons d'UID trouvés parmi les Personnes.", info=T)
    return(uidPER$uid_PER)
}

# Utilisation de la géométrie pour voir si des ménages se trouvent sur des endroits superposés
verif_doublesComptes = function(MEN, shp_ZT, shp_COM)
{
    rapport("Vérification des doubles-comptes à l'aide de la géométrie")
    
    # On vérifie que tout est bien compatible
    shp_COM = st_transform(shp_COM, crs=3857)
    shp_ZT  = st_transform(shp_ZT,  crs=3857)
    
    # On crée des identifiants de secteurs uniques
    shp_ZT$CODE_SEC = paste0(shp_ZT$uid_ENQ, shp_ZT$CODE_SEC)
    
    # On liste les communes des ménages qui ne sont pas localisés dans des secteurs connus,
    # et on leur adjoint la géométrie de leur commune
    MenSansGeom = st_drop_geometry(MEN) %>%
        filter(!ZT %in% shp_ZT$CODE_SEC) %>%
        group_by(Com, EnqAnnee) %>% summarize(.groups="drop") %>%
        left_join(shp_COM, by=c("Com" = "insee")) %>%
        st_as_sf()
    
    # On n'a plus qu'à coller les deux bouts
    Zs = select(shp_ZT, EnqAnnee) %>%
         rbind(select(MenSansGeom, EnqAnnee)) %>%
         rename(Annee = EnqAnnee) # évite des colonnes homonymes
    
    if (!"sf" %in% class(MEN)) { stop("La vérification des doubles-comptes nécessite de la géométrie") }
    
    barre = ui_ProgInit(max(as.integer(Zs$Annee), na.rm=T) - min(as.integer(Zs$Annee) - 1, na.rm=T))
    MEN$EnqRecente = 0

    for(i in min(as.integer(Zs$Annee), na.rm=T):max(as.integer(Zs$Annee), na.rm=T)){
        cat(" année", i)
        
        # Quelles sont les aires d'enquête de l'année i ?
        anneeI = filter(Zs, Annee == as.character(i))
        
        # Le ménage intersecte-t-il avec une aire d'enquête de l'année i?
        MEN = st_join(MEN, anneeI, st_intersects, left = T)
        
        # Si oui, retenir que l'enquête la plus récente est celle-là (si non, on en reste là)
        MEN = mutate(MEN, EnqRecente = ifelse(is.na(Annee), EnqRecente, Annee))
        
        # Supprimer la colonne Annee à chaque fois
        MEN = MEN[,1:(ncol(MEN)-1)]
        
        # On met à jour la barre
        ui_Prog(barre, i+1 - min(as.integer(Zs$Annee), na.rm=T))
    }
    
    # Pour les quelques ménages qui restent, considérer que l'enquête la plus récente est la bonne
    MEN$EnqRecente = ifelse(MEN$EnqRecente == 0, MEN$EnqAnnee, MEN$EnqRecente)
    
    # Considérer que l'année d'enquête la plus récente est celle du doublon légitime
    MEN$Doublon = ifelse(MEN$EnqRecente > MEN$EnqAnnee, T, F)
    MEN$EnqRecente = NULL
    
    nDoublons = nrow(filter(MEN, Doublon == T))
    rapport("Nombre de doublons détectés d'après la géométrie :", nDoublons, "soit",
            nDoublons/nrow(MEN)*100 %>% round(2), "%")
    
    # MEN est retourné avec un champ supplémentaire, le champ "Doublon"
    return(MEN)
}

verif_compoMenages = function(PER, MEN, bloquant = F)
{
    rapport("Vérification de la composition des ménages (depuis la base Personnes).")
    
    npMEN = PER %>%
        mutate(b_Ref  = ifelse(Lien == "1",1,0),
               b_Conj = ifelse(Lien == "2",1,0),
               b_Enft = ifelse(Lien == "3",1,0),
               b_Colc = ifelse(Lien == "4",1,0),
               b_Faml = ifelse(Lien == "5",1,0),
               b_Autr = ifelse(Lien == "6" | Lien == "7",1,0)) %>%
        group_by(uid_MEN) %>% dplyr::summarize(n_Ref = sum(b_Ref), n_Conj = sum(b_Conj),
                                               n_Enft = sum(b_Enft), n_Colc = sum(b_Colc), n_Faml = sum(b_Faml),
                                               n_Autr = sum(b_Autr),
                                               nMEN = n(), uid_ENQ = first(uid_ENQ))
    npMEN = left_join(npMEN, select(MEN, uid_MEN, Coeff), by=c("uid_MEN" = "uid_MEN"))
    
    liste1 = NULL ; liste2 = NULL ; liste3 = NULL
    
    liste1 = filter(npMEN, n_Conj > 1)$uid_MEN
    if (length(liste1)>0)
        { rapport(length(liste1), "ménages comportent plusieurs conjoint.es", info=T) }

    liste2 = filter(npMEN, n_Ref != 1)$uid_MEN
    if (length(liste2)>0)
        { rapport(length(liste2), "ménages comportent plusieurs personnes de référence", info=T) }
    
    liste3 = filter(PER, Lien == "2" & Age<16)$uid_MEN
    if (length(liste3)>0)
        { rapport(length(liste3), "ménages comportent des enfants marqués comme conjoint.es", info=T) }
    
    if ((length(liste1)>0 | length(liste2)>0 | length(liste3)>0) & bloquant)
        { stop("La base personne comprend des erreurs de composition de ménage non corrigées.") }
}

verif_PCSM = function(PER, pdf = NULL)
{
    if (!is.null(pdf)) {imprPdf(pdf)}
    
    barplot(table(PER$PCSMD), horiz=T, main="Effectifs des PCS Ménage")
    
    pnv = PER %>% mutate(NAs = ifelse(is.na(PCSMD), 1, 0)) %>% group_by(substr(uid_MEN, 1, 7)) %>%
        summarize(N = n(), NNV = sum(NAs)) %>% mutate(PNV = NNV / N * 100)
    pnv = pnv[order(pnv$PNV),] ; rownames(pnv) = c(unlist(pnv[,1]))
    par(mar=c(6,6,4,1))
    barplot(pnv$PNV, horiz=T, names.arg = c(unlist(pnv[,1])), axisnames=T, las=2,
            main="Part de PCS Men indéterminées par enquête",
            xlab = "Part (%)", sub = "Champ : Enquêtes EMD Cerema 2009~19 dont les PCS détaillées sont disponibles.")
    
    pnv = PER %>% mutate(NAs = ifelse(is.na(PCSML), 1, 0)) %>% group_by(substr(uid_MEN, 1, 7)) %>%
        summarize(N = n(), NNV = sum(NAs)) %>% mutate(PNV = NNV / N * 100)
    pnv = pnv[order(pnv$PNV),] ; rownames(pnv) = c(unlist(pnv[,1]))
    par(mar=c(6,6,4,1))
    barplot(pnv$PNV, horiz=T, names.arg = c(unlist(pnv[,1])), axisnames=T, las=2,
            main="Part de PCS Men larges indéterminées par enquête",
            xlab = "Part (%)", sub = "Champ : Enquêtes EMD Cerema 2009~19")
    if (!is.null(pdf)) { off() }
}

verif_tempsDepl = function(DEP, pdf = NULL)
{
    DEP$V = (DEP$Dis/1000) / (DEP$Duree/60)
    
    if (!is.null(pdf)) { imprPdf(pdf, paysage=T) }
    
    ggplot(DEP, aes(x = V)) + geom_density() +
        scale_x_log10(labels = ~ format(.x, scientific = FALSE)) + theme_bw() +
        labs(title="Vitesses de déplacement") + xlab("vitesse (km/h)") + ylab("fréquence")
    
    ggplot(filter(DEP, V < 3.9 | V > 4.1), aes(x = V)) + geom_density() +
        scale_x_log10(labels = ~ format(.x, scientific = FALSE)) + theme_bw() +
        labs(title="Vitesses de déplacement", subtitle="Déplacements à 4 km/h +/- 10% exclus") +
        xlab("vitesse (km/h)") + ylab("fréquence")
    
    # En marche à pied, la durée des déplacements problématiques est très souvent de 1, 2 ou 5 minutes
    # (marginalement 10 et 15 : effet des valeurs rondes)
    DEP.MAR = filter(DEP, ModeP %in% mode_MAR) %>% filter(V > 25)
    barplot(table(DEP.MAR$Duree), main = "Effets de valeurs rondes sur la durée des déplacements à pied")
    
    # Un effet d'arrondi : les gens déclarant des durées imprécises, mais des lieux précis, ils se retrouvent
    # en contradiction avec leur propre déclaration. Pour autant la donnée de vitesse ne perd pas toute pertinence
    # puisque la médiane pour la marche est une valeur crédible (4 km/h).
    
    # Même problème à vélo + problèmes de déplacements de 0 minutes
    DEP.VEL = filter(DEP, ModeP %in% mode_VEL) %>% filter(V > 50)
    barplot(table(DEP.VEL$Duree), main = "Effets des valeurs rondes sur la durée des déplacements à vélo")
    
    if (!is.null(pdf)) {off()}
}

verif_dispoDep = function(DEP, pdf = NULL)
{
    if (!is.null(pdf)) {imprPdf(pdf, paysage=T)}
    
    g1 = DEP %>% mutate(DepToNa = ifelse(is.na(Dis) &  is.na(Dis_V), 1, 0),
                   DepPrNa = ifelse(is.na(Dis) & !is.na(Dis_V), 1, 0),
                   DepDisp = ifelse(!is.na(Dis),                1, 0)) %>%
        group_by(uid_ENQ) %>% summarize(DepToNa = sum(DepToNa), DepPrNa = sum(DepPrNa), DepDisp = sum(DepDisp),
                                        Total = n()) %>%
        tab_Tri(i = "uid_ENQ", parCol = "Total") %>% select(-Total) %>%
        pivot_longer(cols = c("DepToNa", "DepPrNa", "DepDisp")) %>%
        mutate(name = case_when(name == "DepToNa" ~ "1", name == "DepPrNa" ~ "2", name == "DepDisp" ~ "3")) %>%
        ggplot(aes(x = uid_ENQ, y = value)) + geom_bar(aes(fill = name), stat="identity") + coord_flip() + theme_bw() +
        scale_fill_manual(values = c("red", "orange", "lightgreen"), name = "Indisponibilité des\nvaleurs de distance",
                          labels=c("Aucune valeur", "Valeurs précises\nmanquantes", "Valeurs présentes")) +
        xlab("Indicatif de l'enquête") + ylab("Nombre d'enregistrements de déplacement")
    
    g2 = DEP %>% mutate(DurInds = ifelse( is.na(Duree), 1, 0),
                   DurDisp = ifelse(!is.na(Duree), 1, 0)) %>%
        group_by(uid_ENQ) %>% summarize(DurInds = sum(DurInds), DurDisp = sum(DurDisp),
                                        Total = n()) %>%
        tab_Tri(i = "uid_ENQ", parCol = "Total") %>% select(-Total) %>%
        pivot_longer(cols = c("DurInds", "DurDisp")) %>%
        mutate(name = case_when(name == "DurInds" ~ "1", name == "DurDisp" ~ "2")) %>%
        ggplot(aes(x = uid_ENQ, y = value)) + geom_bar(aes(fill = name), stat="identity") + coord_flip() + theme_bw() +
        scale_fill_manual(values = c("red", "lightgreen"), name = "Indisponibilité des\nvaleurs de duréee",
                          labels=c("Aucune valeur", "Valeurs présentes")) +
        xlab("Indicatif de l'enquête") + ylab("Nombre d'enregistrements de déplacement")
    
    print(g1)
    print(g2)
    
    if (!is.null(pdf)) {off()}
}

verif_depIntoPer = function(PER)
{
    rapport("Vérifications des données de déplacement agrégées à la personne")
    rapport(paste0("Part d'enquêté.es sans Distance complète : ",
                   round(nrow(filter(PER, is.na(Dis)))/nrow(PER) * 100, 2), " %"), info=T)
    rapport("Nombre d'enquêté.es dont le poids est nul :",
            nrow(filter(filter(PER, !is.na(Tps)), CoeffEnq == 0)), info=T)
    rapport("Cas d'enquêté.es pondéré.es sans déplacement recensé :",
            nrow(filter(filter(PER, is.na(N)), CoeffEnq > 0)), info=T)
    PER$VeilleDepl = plyr::revalue(PER$VeilleDepl,
                                          c("1" = "s'est déplacé·e",
                                            "2" = "ne s'est pas déplacé·e",
                                            "3" = "absent·e",
                                            "4" = "pas de relevé",
                                            "5" = "absent·e (longue durée)"))
    rapport("Activités déclarées la veille :",
            paste(capture.output(table(filter(filter(PER, is.na(N)), CoeffEnq > 0)$VeilleDepl)),
                  collapse = "\n"), info=T)
    rapport("Nombre de non-enquêté.es dont le poids est supérieur à zéro :",
            nrow(filter(filter(PER, is.na(N)), CoeffEnq > 0)), info=T)
}

verif_recensement = function(PER, z_Nomenclature, filtres, pdfGen = NULL, pdfParEnq = NULL,
                             ficRecensement = "Sources/IRIS population.csv")
{ 
    .pardefaut <- par()
    rapport("Comparaison des données EMD et recensement")
    iris.j = read.csv(ficRecensement, sep=";")
    
    # Transcription des PCS pour correspondre aux standards de l'Insee
    PER.CommeRec = PER %>%
        mutate(PCS8 = ifelse(is.na(PCS8), "00", as.character(PCS8))) %>%
        mutate(PCS8 = ifelse(PCS8 == "07", "08", as.character(PCS8))) %>%
        mutate(PCS8 = ifelse(PCS8 == "09", "08", as.character(PCS8))) %>%
        mutate(PCS8 = ifelse(Activ == "32", "07", as.character(PCS8)))
    
    iris.j$REG = as.factor(iris.j$REG) ; iris.j$MODIF_IRIS = as.factor(iris.j$MODIF_IRIS)
    
    # Correction : regroupement de Lyon et de Marseille (la BU détaille les arrondists de Paris)
    arrLyon = as.character(c(69381:69389))
    arrMars = as.character(c(13201:13216))
    iris.j = iris.j %>% mutate(COM = ifelse(COM %in% arrLyon, "69123", COM),
                               COM = ifelse(COM %in% arrMars, "13055", COM))
    
    # Passage du recensement à la commune
    recensement = iris.j %>% group_by(COM) %>% summarize_if(is.integer, sum, na.rm=T)
    
    # Transformation du fichier PER pour le passer par commune
    popEmd = PER.CommeRec %>% mutate(n      = Coeff,
                                     n_H    = ifelse(Genre == "H", Coeff, 0),
                                     n_F    = ifelse(Genre == "F", Coeff, 0),
                                     n_0002 = ifelse(Age %in% c(0:2  ), Coeff, 0),
                                     n_0305 = ifelse(Age %in% c(3:5  ), Coeff, 0),
                                     n_0610 = ifelse(Age %in% c(6:10 ), Coeff, 0),
                                     n_1117 = ifelse(Age %in% c(11:17), Coeff, 0),
                                     n_1824 = ifelse(Age %in% c(18:24), Coeff, 0),
                                     n_2539 = ifelse(Age %in% c(25:39), Coeff, 0),
                                     n_4054 = ifelse(Age %in% c(40:54), Coeff, 0),
                                     n_5564 = ifelse(Age %in% c(55:64), Coeff, 0),
                                     n_6579 = ifelse(Age %in% c(65:79), Coeff, 0),
                                     n_8000 = ifelse(Age > 79, Coeff, 0),
                                     n_15P  = ifelse(Age > 14, Coeff, 0),
                                     n_PCS1 = ifelse(PCS8 == "01" & Age > 14, Coeff, 0),
                                     n_PCS2 = ifelse(PCS8 == "02" & Age > 14, Coeff, 0),
                                     n_PCS3 = ifelse(PCS8 == "03" & Age > 14, Coeff, 0),
                                     n_PCS4 = ifelse(PCS8 == "04" & Age > 14, Coeff, 0),
                                     n_PCS5 = ifelse(PCS8 == "05" & Age > 14, Coeff, 0),
                                     n_PCS6 = ifelse(PCS8 == "06" & Age > 14, Coeff, 0),
                                     n_PCS7 = ifelse(PCS8 == "07" & Age > 14, Coeff, 0),
                                     n_PCS8 = ifelse(PCS8 == "08" & Age > 14, Coeff, 0),
                                     n_PCS0 = ifelse(is.na(PCS8)  & Age > 14 | PCS8 == "00" & Age>14, Coeff, 0)) %>%
        select(uid_ENQ, Com, n:n_PCS0) %>%
        group_by(uid_ENQ, Com) %>% summarize_if(is.numeric, sum, na.rm=T)
    
    # Calcul des proportions, pour le recensement et pour les EMD, avant comparaison
    recensement = mutate(recensement,
                         SexRatio  = P15_POPH       / P15_POPF,
                         Part_0002 = P15_POP0002    / P15_POP,
                         Part_0305 = P15_POP0305    / P15_POP,
                         Part_0610 = P15_POP0610    / P15_POP,
                         Part_1117 = P15_POP1117    / P15_POP,
                         Part_1824 = P15_POP1824    / P15_POP,
                         Part_2539 = P15_POP2539    / P15_POP,
                         Part_4054 = P15_POP4054    / P15_POP,
                         Part_5564 = P15_POP5564    / P15_POP,
                         Part_6579 = P15_POP6579    / P15_POP,
                         Part_8000 = P15_POP80P     / P15_POP,
                         Part_PCS1 = C15_POP15P_CS1 / C15_POP15P,
                         Part_PCS2 = C15_POP15P_CS2 / C15_POP15P,
                         Part_PCS3 = C15_POP15P_CS3 / C15_POP15P,
                         Part_PCS4 = C15_POP15P_CS4 / C15_POP15P,
                         Part_PCS5 = C15_POP15P_CS5 / C15_POP15P,
                         Part_PCS6 = C15_POP15P_CS6 / C15_POP15P,
                         Part_PCS7 = C15_POP15P_CS7 / C15_POP15P,
                         Part_PCS8 = C15_POP15P_CS8 / C15_POP15P)
    popEmd = mutate(popEmd,
                    SexRatio  = n_H / n_F,
                    Part_0002 = n_0002 / n,
                    Part_0305 = n_0305 / n,
                    Part_0610 = n_0610 / n,
                    Part_1117 = n_1117 / n,
                    Part_1824 = n_1824 / n,
                    Part_2539 = n_2539 / n,
                    Part_4054 = n_4054 / n,
                    Part_5564 = n_5564 / n,
                    Part_6579 = n_6579 / n,
                    Part_8000 = n_8000 / n,
                    Part_PCS1 = n_PCS1 / n_15P,
                    Part_PCS2 = n_PCS2 / n_15P,
                    Part_PCS3 = n_PCS3 / n_15P,
                    Part_PCS4 = n_PCS4 / n_15P,
                    Part_PCS5 = n_PCS5 / n_15P,
                    Part_PCS6 = n_PCS6 / n_15P,
                    Part_PCS7 = n_PCS7 / n_15P,
                    Part_PCS8 = n_PCS8 / n_15P,
                    Part_PCS0 = n_PCS0 / n_15P)
    
    # Correction des codes Insee de Corse pour permettre la jointure
    recensement =
        mutate(recensement, COM = ifelse(substr(COM,1,2) == "2A", paste0("20", substr(COM,3,5)), COM)) %>%
        mutate(COM = ifelse(substr(COM,1,2) == "2B",paste0("20",as.character(as.integer(substr(COM,3,5))+500)),COM))
    
    # Jointure de popEmd et des tables du recensement
    popEmd = left_join(popEmd, recensement, by=c("Com" = "COM"), suffix=c(".emd", ".rec"))
    remove(recensement)
    
    # Calcul des rapports par enquête
    popEmd.Enq = select(popEmd, uid_ENQ, n:n_PCS0, P15_POP:C15_POP15P_CS8) %>%
        filter(!is.na(P15_POP)) %>%
        group_by(uid_ENQ) %>% summarize_if(is.numeric, sum) %>%
        mutate(SexRatio.rec  = P15_POPH       / P15_POPF,
               Part_0002.rec = P15_POP0002    / P15_POP,
               Part_0305.rec = P15_POP0305    / P15_POP,
               Part_0610.rec = P15_POP0610    / P15_POP,
               Part_1117.rec = P15_POP1117    / P15_POP,
               Part_1824.rec = P15_POP1824    / P15_POP,
               Part_2539.rec = P15_POP2539    / P15_POP,
               Part_4054.rec = P15_POP4054    / P15_POP,
               Part_5564.rec = P15_POP5564    / P15_POP,
               Part_6579.rec = P15_POP6579    / P15_POP,
               Part_8000.rec = P15_POP80P     / P15_POP,
               Part_PCS1.rec = C15_POP15P_CS1 / C15_POP15P,
               Part_PCS2.rec = C15_POP15P_CS2 / C15_POP15P,
               Part_PCS3.rec = C15_POP15P_CS3 / C15_POP15P,
               Part_PCS4.rec = C15_POP15P_CS4 / C15_POP15P,
               Part_PCS5.rec = C15_POP15P_CS5 / C15_POP15P,
               Part_PCS6.rec = C15_POP15P_CS6 / C15_POP15P,
               Part_PCS7.rec = C15_POP15P_CS7 / C15_POP15P,
               Part_PCS8.rec = C15_POP15P_CS8 / C15_POP15P,
               SexRatio.emd  = n_H / n_F,
               Part_0002.emd = n_0002 / n,
               Part_0305.emd = n_0305 / n,
               Part_0610.emd = n_0610 / n,
               Part_1117.emd = n_1117 / n,
               Part_1824.emd = n_1824 / n,
               Part_2539.emd = n_2539 / n,
               Part_4054.emd = n_4054 / n,
               Part_5564.emd = n_5564 / n,
               Part_6579.emd = n_6579 / n,
               Part_8000.emd = n_8000 / n,
               Part_PCS1.emd = n_PCS1 / (n_15P - n_PCS0),
               Part_PCS2.emd = n_PCS2 / (n_15P - n_PCS0),
               Part_PCS3.emd = n_PCS3 / (n_15P - n_PCS0),
               Part_PCS4.emd = n_PCS4 / (n_15P - n_PCS0),
               Part_PCS5.emd = n_PCS5 / (n_15P - n_PCS0),
               Part_PCS6.emd = n_PCS6 / (n_15P - n_PCS0),
               Part_PCS7.emd = n_PCS7 / (n_15P - n_PCS0),
               Part_PCS8.emd = n_PCS8 / (n_15P - n_PCS0),
               Part_PCS0.emd = n_PCS0 / n_15P,
               e_Pop      = (P15_POP / n                   - 1) * 100,
               e_SexRatio = (SexRatio.emd  / SexRatio.rec  - 1) * 100,
               e_Age0002  = (Part_0002.emd / Part_0002.rec - 1) * 100,
               e_Age0305  = (Part_0305.emd / Part_0305.rec - 1) * 100,
               e_Age0610  = (Part_0610.emd / Part_0610.rec - 1) * 100,
               e_Age1117  = (Part_1117.emd / Part_1117.rec - 1) * 100,
               e_Age1824  = (Part_1824.emd / Part_1824.rec - 1) * 100,
               e_Age2539  = (Part_2539.emd / Part_2539.rec - 1) * 100,
               e_Age4054  = (Part_4054.emd / Part_4054.rec - 1) * 100,
               e_Age5564  = (Part_5564.emd / Part_5564.rec - 1) * 100,
               e_Age6579  = (Part_6579.emd / Part_6579.rec - 1) * 100,
               e_Age8000  = (Part_8000.emd / Part_8000.rec - 1) * 100,
               e_PCS1     = (Part_PCS1.emd / Part_PCS1.rec - 1) * 100,
               e_PCS2     = (Part_PCS2.emd / Part_PCS2.rec - 1) * 100,
               e_PCS3     = (Part_PCS3.emd / Part_PCS3.rec - 1) * 100,
               e_PCS4     = (Part_PCS4.emd / Part_PCS4.rec - 1) * 100,
               e_PCS5     = (Part_PCS5.emd / Part_PCS5.rec - 1) * 100,
               e_PCS6     = (Part_PCS6.emd / Part_PCS6.rec - 1) * 100,
               e_PCS7     = (Part_PCS7.emd / Part_PCS7.rec - 1) * 100,
               e_PCS8     = (Part_PCS8.emd / Part_PCS8.rec - 1) * 100,
               eSAge0002  = (n_0002 / P15_POP0002    - 1) * 100,
               eSAge0305  = (n_0305 / P15_POP0305    - 1) * 100,
               eSAge0610  = (n_0610 / P15_POP0610    - 1) * 100,
               eSAge1117  = (n_1117 / P15_POP1117    - 1) * 100,
               eSAge1824  = (n_1824 / P15_POP1824    - 1) * 100,
               eSAge2539  = (n_2539 / P15_POP2539    - 1) * 100,
               eSAge4054  = (n_4054 / P15_POP4054    - 1) * 100,
               eSAge5564  = (n_5564 / P15_POP5564    - 1) * 100,
               eSAge6579  = (n_6579 / P15_POP6579    - 1) * 100,
               eSAge8000  = (n_8000 / P15_POP80P     - 1) * 100,
               eSPCS1     = (n_PCS1 / C15_POP15P_CS1 - 1) * 100,
               eSPCS2     = (n_PCS2 / C15_POP15P_CS2 - 1) * 100,
               eSPCS3     = (n_PCS3 / C15_POP15P_CS3 - 1) * 100,
               eSPCS4     = (n_PCS4 / C15_POP15P_CS4 - 1) * 100,
               eSPCS5     = (n_PCS5 / C15_POP15P_CS5 - 1) * 100,
               eSPCS6     = (n_PCS6 / C15_POP15P_CS6 - 1) * 100,
               eSPCS7     = (n_PCS7 / C15_POP15P_CS7 - 1) * 100,
               eSPCS8     = (n_PCS8 / C15_POP15P_CS8 - 1) * 100,
               DEV_C1     = (n_PCS1 - C15_POP15P_CS1) / C15_POP15P, # un indicateur abstrait, mais utile
               DEV_C2     = (n_PCS2 - C15_POP15P_CS2) / C15_POP15P,
               DEV_C3     = (n_PCS3 - C15_POP15P_CS3) / C15_POP15P,
               DEV_C4     = (n_PCS4 - C15_POP15P_CS4) / C15_POP15P,
               DEV_C5     = (n_PCS5 - C15_POP15P_CS5) / C15_POP15P,
               DEV_C6     = (n_PCS6 - C15_POP15P_CS6) / C15_POP15P,
               DEV_C7     = (n_PCS7 - C15_POP15P_CS7) / C15_POP15P,
               DEV_C8     = (n_PCS8 - C15_POP15P_CS8) / C15_POP15P)
    
    # Calcul d'une espèce d'indice de déviation
    popEmd.Enq = popEmd.Enq %>%
        mutate(devPCS = sqrt(DEV_C1^2 + DEV_C2^2 + DEV_C3^2 + DEV_C4^2 + DEV_C5^2 + DEV_C6^2 + DEV_C7^2 + DEV_C8^2))
    
    popEmd.Enq = left_join(popEmd.Enq, z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ"))
    
    imprPdf(chemin = pdfGen, paysage = T)
    
    gr = viz_Barres(grph = select(popEmd.Enq, Libelle_Long, e_Pop, e_SexRatio),
               etiquettes = c("e_Pop", "e_SexRatio"),
               titre = "Ecarts de population / Sex ratio EMD/Recensement",
               soustitre = "Enquêtes EMD du Cerema (2009~2019)",
               xlab = "Enquête", ylab="Ecart (%)",
               legende = "Ecart", dejaMelt = F, log = F,
               etiquettes.renom = c("de population", "de sex-ratio hommes/femmes")) ; print(gr)
    
    gr = viz_Barres(grph = select(popEmd.Enq, Libelle_Long, e_Age0002:e_Age8000),
               etiquettes = c("e_Age0002", "e_Age0305", "e_Age0610", "e_Age1117", "e_Age1824",
                              "e_Age2539", "e_Age4054", "e_Age5564", "e_Age6579", "e_Age8000"),
               titre = "Ecarts de population par tranche d'âge entre la base EMD et le recensement",
               soustitre = "Enquêtes EMD du Cerema (2009~2019)",
               xlab = "Enquête", ylab="Ecart entre EMDs et recensement (%)",
               legende = "Classe d'âge", dejaMelt = F, log = F,
               etiquettes.renom = c("0~2 ans", "3~5 ans", "6~10 ans", "11~17 ans",
                                    "18~24 ans", "25~39 ans", "40~54 ans", "55~64 ans",
                                    "65~79 ans", "> 80 ans"),
               lignes=c(10, -10), legendelignes = "Limites +/- 10%") ; print(gr)
    
    g = select(popEmd.Enq, Libelle_Long, e_PCS1:e_PCS8, Part_PCS0.emd) %>%
        mutate(Libelle_Long = paste0(Libelle_Long, " (NR : ", round(Part_PCS0.emd*100,1), " %)"))
    
    gr = viz_Barres(grph = g,
               etiquettes = c("e_PCS1", "e_PCS2", "e_PCS3", "e_PCS4", "e_PCS5", "e_PCS6", "e_PCS7", "e_PCS8"),
               titre = "Ecarts de population par PCS entre la base EMD et le recensement",
               soustitre = "Enquêtes EMD du Cerema (2009~2019)",
               xlab = "Enquête", ylab="Ecart entre EMDs et recensement (%)",
               legende = "PCS8", dejaMelt = F, log = F, #yminmax=c(-50,100),
               etiquettes.renom = c("agriculteur.rices", "indépendant.es", "cadres/prof. intell.", "prof. interm.",
                                    "employé.es", "ouvrier.es", "retraité.es", "inactif.ves"),
               lignes=c(10, -10), legendelignes = "plus ou moins 10%") ; print(gr) ; remove(g)
    
    
    gr = viz_Barres(grph = select(popEmd.Enq, Libelle_Long, eSAge0002:eSAge8000),
               etiquettes = c("eSAge0002", "eSAge0305", "eSAge0610", "eSAge1117", "eSAge1824",
                              "eSAge2539", "eSAge4054", "eSAge5564", "eSAge6579", "eSAge8000"),
               titre = "Ecarts de population par tranche d'âge entre la base EMD et le recensement",
               soustitre = "Enquêtes EMD du Cerema (2009~2019)",
               xlab = "Enquête", ylab="Ecart entre EMDs et recensement (%)",
               legende = "Classe d'âge", dejaMelt = F, log = F,
               etiquettes.renom = c("0~2 ans", "3~5 ans", "6~10 ans", "11~17 ans",
                                    "18~24 ans", "25~39 ans", "40~54 ans", "55~64 ans",
                                    "65~79 ans", "> 80 ans"),
               lignes=c(10, -10), legendelignes = "Limites +/- 10%") ; print(gr)
    
    g = select(popEmd.Enq, Libelle_Long, eSPCS1:eSPCS8, Part_PCS0.emd) %>%
        mutate(Libelle_Long = paste0(Libelle_Long, " (NR : ", round(Part_PCS0.emd*100,1), " %)"))
    
    palPCS = c("darkolivegreen3", "turquoise", "salmon", "mediumpurple2", "cornflowerblue", "orangered3",
               "goldenrod1", "goldenrod3", "darkorange4")
    
    gr = viz_Barres(grph = g,
               etiquettes = c("eSPCS1", "eSPCS2", "eSPCS3", "eSPCS4", "eSPCS5", "eSPCS6", "eSPCS7", "eSPCS8"),
               titre = "Ecarts de population par PCS entre la base EMD et le recensement",
               soustitre = "Enquêtes EMD du Cerema (2009~2019)", couleurs = palPCS,
               xlab = "Enquête", ylab="Ecart entre EMDs et recensement (%)",
               legende = "PCS8", dejaMelt = F, log = F, #yminmax=c(-50,100),
               etiquettes.renom = c("agriculteur.rices", "indépendant.es", "cadres/prof. intell.", "prof. interm.",
                                    "employé.es", "ouvrier.es", "retraité.es", "inactif.ves"),
               lignes=c(10, -10), legendelignes = "plus ou moins 10%") ; print(gr) ; remove(g)
    
    etiq = as.character(c("Inc.", "Agr.", "Ind.", "Cad.", "Int.", "Emp.", "Ouv.", "Ret.", "Inac."))
    
    off(optionDesactivationSiNull = pdfGen)
    
    imprPdf(chemin = pdfParEnq, format="a5", paysage = T)
    rapport("Impression des graphiques de comparaison Recensement/EMD par enquête")
    listeUid = popEmd.Enq$uid_ENQ
    barre = txtProgressBar(min = 1, max=length(listeUid), style=3)
    for(i in 1:length(listeUid)){
        setTxtProgressBar(barre, i)
        subPER = filter(popEmd.Enq, uid_ENQ == listeUid[i])
        
        facteur = 10^3
        
        orig = c(subPER$n_PCS0 / facteur,
                 subPER$n_PCS1 / facteur, subPER$n_PCS2 / facteur, subPER$n_PCS3 / facteur, subPER$n_PCS4 / facteur,
                 subPER$n_PCS5 / facteur, subPER$n_PCS6 / facteur, subPER$n_PCS7 / facteur, subPER$n_PCS8 / facteur)
        
        pctg = c(subPER$Part_PCS0.emd,
                 subPER$Part_PCS1.emd, subPER$Part_PCS2.emd, subPER$Part_PCS3.emd, subPER$Part_PCS4.emd,
                 subPER$Part_PCS5.emd, subPER$Part_PCS6.emd, subPER$Part_PCS7.emd, subPER$Part_PCS8.emd) %>%
            round(3) * 100
        
        inseecomp = c(0, subPER$C15_POP15P_CS1 / facteur, subPER$C15_POP15P_CS2 / facteur,
                      subPER$C15_POP15P_CS3 / facteur, subPER$C15_POP15P_CS4 / facteur,
                      subPER$C15_POP15P_CS5 / facteur, subPER$C15_POP15P_CS6 / facteur,
                      subPER$C15_POP15P_CS7 / facteur, subPER$C15_POP15P_CS8 / facteur)
        
        max = max(c(orig, inseecomp))
        
        barplot(orig, main = paste0("Composition population ", subPER$Libelle_Long),
                names = paste0(etiq, " : ", pctg, " %\n"), col = "grey80", border=NA,
                sub = paste0("Niveau de détail des PCS : ",
                             ifelse(filtres[filtres$uid_ENQ == listeUid[i],]$filtre.PCS, "détaillé", "8 postes"),
                             " - Indice de déviation : ", round(subPER$devPCS,3), "\n",
                             "En vert : répartition équivalente de la population française selon le recensement de 2015"),
                ylab="Nombre de personnes, EMD (milliers)", ylim = c(0, max), cex.names=.6) %>% print()
        
        pctg = c(0,
                 subPER$Part_PCS1.rec, subPER$Part_PCS2.rec, subPER$Part_PCS3.rec, subPER$Part_PCS4.rec,
                 subPER$Part_PCS5.rec, subPER$Part_PCS6.rec, subPER$Part_PCS7.rec, subPER$Part_PCS8.rec) %>%
            round(3) * 100
        
        barplot(inseecomp, add=T, border="limegreen", col=NA,
                names = paste0("\n","R. ", pctg, " %"),
                col.axis = "limegreen", yaxt = "n", cex.names=.6) %>% print()
    }
    close(barre)
    par(.pardefaut)
    
    
    off(optionDesactivationSiNull = pdfParEnq)
    
    return(select(popEmd.Enq, uid_ENQ, devPCS))
}

# Correctifs automatisés ====

filtre_heures = function(DEP)
{
    rapport(nrow(filter(DEP, as.integer(substr(D_Hr,1,2))>27)), "entrées excèdent la plage horaire", info=T)
    DEP$O_Hr = ifelse(as.integer(substr(DEP$O_Hr,3,4))>59, NA, DEP$O_Hr)
    DEP$D_Hr = ifelse(as.integer(substr(DEP$D_Hr,3,4))>59, NA, DEP$D_Hr)
    
    erreurs = filter(DEP, heureHHMMtoM(D_Hr) < heureHHMMtoM(O_Hr))
    rapport(length(unique(erreurs$uid_PER)), "enquêté.es retiré.es",
            "(heures d'arrivée antérieures aux heures de départ)", info = T)
    DEP = filter(DEP, !uid_PER %in% erreurs$uid_PER)
    
    return(DEP)
}

filtre_doublonsUid = function(base, doublonsMEN, doublonsPER = NULL)
{
    base = filter(base, !uid_MEN %in% doublonsMEN)
    if (!is.null(doublonsPER)) { base = filter(base, !uid_PER %in% doublonsPER) }
    return(base)
}

filtre_combinaisonsActivites = function(PER)
{
    rapport("Vérification des cas de combinaisons d'activités impossibles")
    
    nInit = nrow(PER)
    # 877 cas de personnes "retraitées" qui se rendent quand-même sur un lieu de travail, sans pattern géographique
    pb_RetraitesTravail = filter(PER, Activ == "32" & Dis_Tvl > 0)
    # Dans la grande majorité des cas, ce sont des moins de 70 ans, probablement des erreurs sur le statut
    # Mais on trouve également des plus de 80 ans...
    rapport(nrow(pb_RetraitesTravail),
            "enquêté.es retraité.es pratiquent une activité professionnelle → neutralisé⋅es", info=T)
    # Solution adoptée : neutralisation dans DEPintoPER
    PER = filter(PER, !uid_PER %in% pb_RetraitesTravail$uid_PER)
    
    # ~ Des sans emploi qui vont au travail
    # 327 cas de personnes sans emploi ou au foyer qui se déplacent pourtant vers un travail.
    # A Béziers, j'ai même des génies qui sont au chomage ET qui ont travaillé la veille ;
    # aux Sables d'Olonnes, des sans emploi qui... ont un lieu de travail !
    pb_EmploiTravail = filter(PER, Activ %in% c("31", "33") & Dis_Tvl > 0)
    rapport(nrow(pb_EmploiTravail),
            "enquêté.es sans emploi pratiquent une activité professionnelle → neutralisé⋅es", info=T)
    # Solution adoptée : neutralisation
    PER = filter(PER, !uid_PER %in% pb_EmploiTravail$uid_PER)
}

# Filtres spécifiques =======

filtre_travpriv = function(PER)
{
    PCSok = c("23", "36", "22", "46", "47", "48", "54", "55", "61", "66")
    
    PER = PER %>% filter(PCS42S %in% PCSok & typoJo == "TRAV" & Activ %in% c("10", "11", "12"))
    return(PER)
}

# Vizus et sorties ====

tracer_hdp = function(HDP, pdf = NULL)
{
    z_Nomenclature = read.csv("Sources/Dictionnaires/Nomenclature.csv", sep=";", encoding="latin1") %>%
        mutate(Libelle_Long = paste0(Libelle_Simple, " (", Annee, ")"))
    
    hdp_pivot = HDP %>% #pivot_longer(cols = c("pVoit", "pColl"), names_to = "Mode", values_to = "nPER") %>%
        pivot_longer(cols = c("n_Voit", "n_Coll", "med_Voit", "med_Coll", "mad_Voit", "mad_Coll"),
                     names_to = c(".value", "Mode"), names_pattern = "(.+)_(.+)") %>%
        mutate(MedianesVal = med + mad) %>%
        left_join(z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ"))
    
    imprPdf(pdf, paysage=T)
    libelles = unique(hdp_pivot$Libelle_Long)
    b = ui_ProgInit(length(libelles))
    for (i in 1:length(libelles))
    {
        titre = libelles[i]
        g=  hdp_pivot %>%
            filter(Libelle_Long == libelles[i]) %>%
            mutate(n = n/Pop, med = med / Pop, MedianesVal = MedianesVal/Pop) %>%
            mutate(heure = heure / 60) %>%
            mutate(n  = n * 100) %>%
            mutate(MedianesVal = MedianesVal * 100) %>%
            ggplot(aes(x = heure, y = n)) +
            geom_line(aes(color = Mode)) + geom_hline(aes(yintercept = MedianesVal, color = Mode), linetype=2) +
            ylab("Part de la population dans le trafic (%)") +
            labs(title = titre) +
            scale_color_manual(values = c("brown", "navy"), labels = c("Transports en commun", "Trafic automobile"),
                               name = "Type de trafic") +
            theme_bw() + theme(legend.position = "bottom")
        print(g)
        ui_Prog(b, i)
    }
    off(pdf)
}

# Nouveau calcul du carroyage de densité ====

init_densiteZF = function(shp_ZF)
{
  rapport("Calcul de la densité par zone fine d'après carroyage")
  
  # On va tout convertir en Mercator/WGS84 métrique
  
  grille = read_sf("Sources/Mailles/carreaux_200m_met.shp") |>
    st_transform(crs = 3857)
  
  grille_mar = read_sf("Sources/Mailles/car_r02.mid") |>
    st_set_crs(value = 3857)
  
  tab_mar = foreign::read.dbf("Sources/Mailles/car_r02.dbf")
  
  grille_reu = read_sf("Sources/Mailles/car_r04.mid") |>
    st_set_crs(value = 3857)
  
  tab_reu = foreign::read.dbf("Sources/Mailles/car_r04.dbf")
  
  grille_mar = left_join(grille_mar, tab_mar, by = c("id", "idINSPIRE"))
  grille_reu = left_join(grille_reu, tab_reu, by = c("id", "idINSPIRE"))
  
  grille_mar = rename(grille_mar, idcar_200m = id, ind = ind_c) |> select(-idINSPIRE, -idk)
  grille_reu = rename(grille_reu, idcar_200m = id, ind = ind_c) |> select(-idINSPIRE, -idk)
  
  grille_om = rbind(grille_mar, grille_reu)
  grille_om = mutate(grille_om,
                     idcar_1km = NA, idcar_nat = NA,
                     i_est_200 = NA, i_est_1km = NA,
                     lcog_geo  = NA,
                     men = NA, men_pauv = NA, men_1ind = NA, men_5ind = NA,
                     men_prop = NA, men_fmp = NA,
                     ind_snv = NA,
                     men_surf = NA, men_coll = NA, men_mais = NA, log_av45 = NA,
                     log_av45_70 = NA, log_45_70 = NA, log_70_90 = NA, log_ap90 = NA,
                     log_inc = NA, log_soc = NA,
                     ind_0_3 = NA, ind_4_5 = NA, ind_6_10 = NA, ind_11_17 = NA,
                     ind_18_24 = NA, ind_25_39 = NA, ind_40_54 = NA,
                     ind_55_64 = NA, ind_65_79 = NA, ind_80p = NA,
                     ind_inc = NA)
  
  grille_om = select(grille_om, colnames(grille))
  
  grille = rbind(grille, grille_om)
  
  # Approche 1 : avec changement de maillage complet
  # grille$surf1 = as.double(st_area(grille))
  # grille = st_intersection(grille, shp_ZF)
  # grille$surf2 = as.double(st_area(grille))
  # grille$surf1 = grille$surf1 / 10^6
  # grille$surf2 = grille$surf2 / 10^6
  # grille$pop = grille$ind * grille$surf2 / grille$surf1
  # grille$dens = grille$pop / grille$surf2
  # grille = group_by(grille, CODE_ZF) %>%
  #   summarise(densite = weighted.mean(dens, w = pop))
  
  # Approche 2 : en comptant l'intégralité de la pop des carreaux avec lesquels
  # il y a une intersection
  grille$surf = as.double(st_area(grille)/10^6) # for some reason, pas exactement 0,04 km²
  
  grilleZF = st_intersection(grille, st_transform(shp_ZF, crs=3857)) %>%
    group_by(CODE_ZF) %>%
    st_drop_geometry() %>%
    summarise(pop = sum(ind), surf = sum(surf))
  grilleZF$densite = grilleZF$pop / grilleZF$surf
  
  # grille %>%
  #   filter(substr(CODE_ZF, 1, 7) == "LOI2015") %>%
  #   ggplot() + geom_sf(aes(fill = dens)) +
  #   scale_fill_viridis_b(trans = "log10")
  
  shp_ZF = left_join(shp_ZF, st_drop_geometry(select(grilleZF, -pop, -surf)))

  return(shp_ZF)
}

init_densiteCom = function(shp_COM)
{
  rapport("Calcul de la densité communale d'après carroyage")
  
  if (!"grille" %in% ls()) {
    grille = read_sf("Sources/Mailles/carreaux_200m_met.shp")

    grille_mar = read_sf("Sources/Mailles/car_r02.mid") |>
      st_set_crs(value = 3857)
    
    tab_mar = foreign::read.dbf("Sources/Mailles/car_r02.dbf")
    
    grille_reu = read_sf("Sources/Mailles/car_r04.mid") |>
      st_set_crs(value = 3857)
    
    tab_reu = foreign::read.dbf("Sources/Mailles/car_r04.dbf")
    
    grille_mar = left_join(grille_mar, tab_mar, by = c("id", "idINSPIRE"))
    grille_reu = left_join(grille_reu, tab_reu, by = c("id", "idINSPIRE"))
    
    grille_mar = rename(grille_mar, idcar_200m = id, ind = ind_c) |> select(-idINSPIRE, -idk)
    grille_reu = rename(grille_reu, idcar_200m = id, ind = ind_c) |> select(-idINSPIRE, -idk)
    
    grille_om = rbind(grille_mar, grille_reu)
    grille_om = mutate(grille_om,
                       idcar_1km = NA, idcar_nat = NA,
                       i_est_200 = NA, i_est_1km = NA,
                       lcog_geo  = NA,
                       men = NA, men_pauv = NA, men_1ind = NA, men_5ind = NA,
                       men_prop = NA, men_fmp = NA,
                       ind_snv = NA,
                       men_surf = NA, men_coll = NA, men_mais = NA, log_av45 = NA,
                       log_av45_70 = NA, log_45_70 = NA, log_70_90 = NA, log_ap90 = NA,
                       log_inc = NA, log_soc = NA,
                       ind_0_3 = NA, ind_4_5 = NA, ind_6_10 = NA, ind_11_17 = NA,
                       ind_18_24 = NA, ind_25_39 = NA, ind_40_54 = NA,
                       ind_55_64 = NA, ind_65_79 = NA, ind_80p = NA,
                       ind_inc = NA)
    
    grille_om = select(grille_om, colnames(grille))
    
    grille = rbind(grille, grille_om)
    
    grille$surf = as.double(st_area(grille)/10^6)
  }
  
  grilleCom = st_intersection(grille, st_transform(shp_COM, crs=3857)) %>%
    group_by(insee) %>%
    st_drop_geometry() %>%
    summarise(pop = sum(ind), surf = sum(surf))
  grilleCom$densite = grilleCom$pop / grilleCom$surf
  
  shp_COM = left_join(shp_COM, st_drop_geometry(select(grilleCom, -pop, -surf)))
  
  return(shp_COM)
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



# Exécution du tout =====

source = "Base Unique des EMD du Cerema (2008-2018); EMP (2019)"

initBases = function(incrémenter = T, pasDeTemps = pas)
{
  version(incrémenter = incrémenter)
  
  rapport("Initialisation complète des bases depuis les sources.", prim = T)
  rapport("Pas de calcul des indicateurs séquentiels fixé à", pasDeTemps, info=T)
  
  rapport("Chargement base des Pôles Générateurs de Trafic")
  # Constitution manuelle du fichier "PGT" à l'aide de diverses sources inégales
  PGT = init_PGT()
  
  # Fichier des communes
  shp_COM = init_COM() |>
    init_densiteCom()
  
  #Chargement des bases une par une.
  rapport("Chargement base Ménage")
  MEN = init_base("dico MEN.csv", "BU2020 - MEN.fwf") %>%
    select(-CF) %>%
    init_uid_ENQ(cor_Paris2011 = T) %>%
    recode_EnqMeth() %>% recode_Enq() %>% recode_LogOcc() %>%
    init_uid() %>%
    patch_AurelieCodesZT() %>%
    init_codeZF()
  
  # Complément pour EMD44 :
  MEN44 = init_base("dico MEN_44.csv", "EMD44/02a_EDGT_44_MENAGE_FAF_TEL_2015-08-07_modifZF.txt") %>%
    select(ZT, ZF, Ech, LogAnnee, LogCrit_A, LogCrit_B, LogComPrec, LogOccPrec) %>%
    mutate(ZF = paste("LOI2015", paste0("000",ZT,ZF))) %>% select(-ZT) %>%
    mutate(Ech = paste0("0", Ech)) %>%
    mutate(LogOccPrec = plyr::revalue(LogOccPrec, c("1" = "10",
                                                    "2" = "21",
                                                    "3" = "20",
                                                    "4" = "30",
                                                    "5" = "99",
                                                    "9" = NA)))
  
  MEN = left_join(MEN, MEN44, by=c("ZF" = "ZF", "Ech" = "Ech"))
  remove(MEN44)
  
  # Complément : importation des EMP
  MEN_EMP_brut = init_base_csv("dico MEN_emp.csv", "EMP/tcm_men_public_V2.csv") %>%
    mutate(uid_ENQ = "EMP2019") %>% mutate(uid_MEN = paste0("EMP2019", "##", uid_MEN))
  
  MEN_EMP_equi =  init_base_csv("dico MENQ_emp.csv", "EMP/q_menage_public_V2.csv") %>%
    mutate(uid_MEN = paste0("EMP2019", "##", uid_MEN))
  
  MEN_EMP = MEN_EMP_brut %>%
    convert_EMPtoEMD_MEN(MEN_EMP_equi) %>%
    recode_EnqMeth()
  
  MEN = MEN %>%
    # On initialise les nouvelles colonnes propres à l'EMP qu'on souhaite garder
    mutate(enqEMP_vague = NA, VolVelo = NA, VolVelo_Renon = NA,
           # On initialise les colonnes ZoneRang, ZonePosi et ZoneDens qui seront calculées plus tard
           # pour les EMD
           ZoneRang = NA, ZonePosi = NA, ZoneDens = NA)
  # On colle les bouts, en ne gardant que les colonnes communes
  
  MEN = rbind(MEN, select(MEN_EMP, colnames(MEN)))
  remove(MEN_EMP, MEN_EMP_equi)
  
  # Plus besoin du N° d'échantillon et d'une paire de bricoles
  # + On réordonne les colonnes dans un ordre + logique
  MEN = MEN %>% select(-Ech, -LogAnnuaire) %>%
    select(uid_MEN, uid_ENQ, EnqMeth, Enq:ZT, ZoneDens, ZonePosi, ZoneRang,
           LogType:LogInternet, LogAnnee:LogOccPrec,
           VehN:VelParc, VolVelo:VolVelo_Renon, Coeff, filtre.Geom)
  
  rapport("Chargement base Personnes")
  PER = init_base("dico PER.csv", "BU2020 - PER.fwf") %>%
    init_uid_ENQ(cor_Paris2011 = T) %>%
    recode_EnqMeth() %>% recode_Genre() %>% recode_NivEtu() %>% recode_Activ() %>%
    init_uid() %>%
    init_codeZF() %>%
    patch_AurelieCodesZT() %>%
    patch_compoMenagesFev2021() %>%
    mutate(EnqDate_M = ifelse(EnqDate_M > 12, NA, EnqDate_M),
           EnqDate_M = factor(EnqDate_M, levels = as.character(c(1:12))),
           EnqDate_M = plyr::revalue(EnqDate_M, c("1" = "01", "2" = "02", "3" = "03",
                                                  "4" = "04", "5" = "05", "6" = "06",
                                                  "7" = "07", "8" = "08", "9" = "09"))) %>%
    mutate(EnqDate_JS = factor(EnqDate_JS, levels = as.character(c(1:7))),
           EnqDate_JS = plyr::revalue(EnqDate_JS, c("1" = "lun", "2" = "mar", "3" = "mer",
                                                    "4" = "jeu", "5" = "ven", "6" = "sam",
                                                    "7" = "dim"), warn_missing = F)) %>%
    select(uid_MEN, uid_PER, EnqAnnee, ZF, Com, ZT,
           EnqDate_A:EnqDate_JS, PerEnq, Genre:AboTC, TravDom:Travail_ParcVel,
           Fqc_Mch:Fqc_Tco, VeilleDepl, VeilleTrav, Coeff, CoeffEnq)
  
  # On ajoute quatre colonnes : PCS42, Nat, LieuNsc et Handi à PER, conformément à mon référentiel
  # + Gene et différents types de gênes détaillés dans les EMP
  PER = mutate(PER, PCS42 = NA, Nat = NA, LieuNsc = NA, Handi = NA, Gene = NA,
               GeneQd_mch = NA, GeneQd_esc = NA, GeneQd_fau = NA, GeneQd_deb = NA, GeneQd_por = NA,
               GeneQd_bou = NA, GeneQd_vel = NA, GeneQd_veh = NA, GeneQd_ass = NA, GeneQd_cdt = NA,
               GeneQd_stn = NA, GeneQd_dmd = NA, GeneQd_ach = NA, GeneQd_pln = NA, GeneQd_cps = NA,
               GeneQd_dir = NA) %>%
    select(uid_MEN:PCS42S, PCS42, AboTC, Nat, LieuNsc, Handi, Gene:GeneQd_dir, TravDom:CoeffEnq)
  
  # Complément : importation des EMP
  PER_EMP = init_base_csv("dico PER_emp.csv", "EMP/tcm_ind_kish_public_V2.csv")
  PERK_EMP = init_base_csv("dico PERK_emp.csv", "EMP/k_individu_public_V2.csv",
                           encodingBase = "latin1", encodingDico = "latin1")
  
  # On joint :
  PER_EMP = PER_EMP %>%
    convert_EMPtoEMD_PER(MEN_EMP = MEN_EMP_brut, PERK_EMP = PERK_EMP, supprWeekEnd = F) %>%
    select(colnames(PER))
  PER = rbind(PER, PER_EMP) ; remove(PER_EMP)
  
  # On ajoute une colonne uid_ENQ :
  PER = mutate(PER, uid_ENQ = substr(uid_MEN, 1, 7)) %>% select(uid_ENQ, uid_MEN:CoeffEnq)
  
  verif_compoMenages(PER, MEN, bloquant = T)
  
  # Attention aux doublons d'uid.
  doublonsMEN = verif_doublonsMEN(MEN) ; doublonsPER = verif_doublonsPER(PER)
  MEN = filtre_doublonsUid(MEN, doublonsMEN)
  PER = filtre_doublonsUid(PER, doublonsMEN, doublonsPER)
  
  # On peut calculer des variables au niveau des ménages
  MEN = MEN %>% init_PCSM(PER) %>%
    init_typoMen(PER) %>%
    init_nivEtu(PER) %>%
    init_nEnfants(PER) %>%
    patch_facteurs()
  
  PER = PER %>% courroie_PCSM(MEN) %>%
    patch_facteurs()
  
  chk_Bases()
  
  rapport("Chargement base Déplacements")
  DEP = init_base("dico DEP.csv", "BU2020 - DEP.fwf") %>%
    init_uid_ENQ(cor_Paris2011 = T) %>%
    recode_EnqMeth() %>%
    init_uid() %>%
    filtre_heures() %>%
    init_codeZF() %>%
    patch_AurelieCodesZT() %>%
    filtre_doublonsUid(doublonsMEN, doublonsPER) %>%
    patch_deplZero()
  
  # Import des déplacements de l'EMP
  DEP_EMP = init_base_csv("dico DEP_emp.csv", "EMP/k_deploc_public_V2.csv", encodingBase = "latin1") %>%
    convert_EMPtoEMD_DEP()
  
  # On ajoute le champ Pluie dans DEP et on rbind
  DEP = DEP %>% mutate(Pluie = NA) %>%
    select(uid_MEN, uid_PER, uid_ENQ, uid_DEP, idDep,
           O_Motif, O_MotAc, O_Com, O_ZT, O_ZF, O_Hr,
           D_Motif, D_MotAc, D_Com, D_ZT, D_ZF, D_Hr,
           nArrets, IdBcl, IdBcl_Rang, Duree, nTrj, Dis_V, Dis, ModeP,
           HrSol, Pluie)
  DEP_EMP = select(DEP_EMP, colnames(DEP))
  DEP = rbind(DEP, DEP_EMP)
  remove(DEP_EMP)
  
  rapport("Chargement base Trajets")
  TRJ = init_base("dico TRJ.csv", "BU2020 - TRJ.fwf") %>% 
    init_uid_ENQ(cor_Paris2011 = T) %>%
    recode_EnqMeth() %>%
    init_uid() %>%
    init_codeZF() %>%
    patch_AurelieCodesZT() %>%
    filtre_doublonsUid(doublonsMEN, doublonsPER)
  
  rapport("Chargement base Opinions")
  OPI = init_base("dico OPI.csv", "BU2020 - OPI.fwf") %>%
    init_uid_ENQ(cor_Paris2011 = T) %>%
    recode_EnqMeth() %>%
    init_uid() %>%
    init_codeZF() %>%
    patch_AurelieCodesZT() %>%
    filtre_doublonsUid(doublonsMEN, doublonsPER) %>%
    mutate(adjVoi_1 = etqOpiMots(adjVoi_1), adjVoi_2 = etqOpiMots(adjVoi_2), adjVoi_3 = etqOpiMots(adjVoi_3),
           adjTco_1 = etqOpiMots(adjTco_1), adjTco_2 = etqOpiMots(adjTco_2), adjTco_3 = etqOpiMots(adjTco_3),
           adjVel_1 = etqOpiMots(adjVel_1), adjVel_2 = etqOpiMots(adjVel_2), adjVel_3 = etqOpiMots(adjVel_3)) %>%
    # Jointure avec les colonnes d'opinion de l'EMP (on les met dans la même base même si aucune
    # correspondance ligne à ligne, c'est contestable)
    full_join(select(rename(mutate(PERK_EMP, uid_ENQ = "EMP2019"), CoeffOpi = CoeffEnq),
                     uid_MEN, uid_PER, starts_with("Opi")), by=c("uid_MEN" = "uid_MEN", "uid_PER" = "uid_PER")) %>%
    mutate(across(starts_with("OpiTC_"), ~plyr::revalue(., c("1" = "oui", "0" = "non")))) %>%
    select(uid_MEN, uid_PER, uid_ENQ, logImp_secu:adjVel_3, OpiTC:OpiTC_zzz, CoeffOpi)
  
  rapport("Chargement bases de géométrie Mobiliscope")
  shp_ZF = st_read("Sources/ZF_49ED.shp", quiet = T)  %>%
    init_uid_ENQ() %>%
    patch_shpCodeZf() %>% # corrige des problèmes de formatage des codes ZF
    init_codeZF() %>%
    patch_doublonsZF() |>
    init_densiteZF()
  
  shp_ZT = st_read("Sources/SEC_49ED.shp", quiet = T) %>%
    init_uid_ENQ() %>%
    patch_shpCodeZt() %>% # corrige des problèmes de formatage des codes ZT
    mutate(ZT = paste0(uid_ENQ, CODE_SEC))
  
  chk_Bases()
  
  rapport("Application des correctifs")
  # Patchs d'ensemble
  tableZFdeComUniques = charger_tableZFdeComUniques(shp_ZF, PGT, DEP)
  
  MEN = MEN %>%
    patch_ZFenComUniques(tableZFdeComUniques) %>%
    patch_GeomFev2021(shp_ZF, PGT) %>%
    init_geomMen(shp_ZF, shp_COM) %>%
    # Petit bidouillage pour forcer la prise en compte de l'enquête de Saint-Quentin
    mutate(EnqAnnee = ifelse(uid_ENQ == "SQY2010", "2011", EnqAnnee)) %>%
    verif_doublesComptes(shp_ZT, shp_COM) %>%
    mutate(EnqAnnee = ifelse(uid_ENQ == "SQY2010", "2010", EnqAnnee)) %>%
    init_typoCom(shp_COM) %>%
    init_ZTS(shp_ZT, shp_COM, sauvZTS = T)
  
  DEP = DEP %>%
    patch_ZFenComUniques(tableZFdeComUniques) %>%
    patch_GeomFev2021(shp_ZF, PGT)
  TRJ = TRJ %>%
    patch_ZFenComUniques(tableZFdeComUniques) %>%
    patch_GeomFev2021(shp_ZF, PGT)
  OPI = OPI %>%
    patch_ZFenComUniques(tableZFdeComUniques) %>%
    patch_GeomFev2021(shp_ZF, PGT) 
  
  chk_Bases()
  
  # On sauvegarde tout ce qu'on peut car ça casse derrière en local
  save(MEN, file="Data/MEN.rds")
  save(PER, file="Data/PER.rds")
  save(DEP, file="Data/DEP.rds")
  save(TRJ, file="Data/TRJ.rds")
  save(OPI, file="Data/OPI.rds")
  save(shp_ZT, file="Data/shp_ZT.rds")
  save(shp_ZF, file="Data/shp_ZF.rds")
  save(shp_COM,file="Data/shp_COM.rds")
  save(PGT   , file="Data/PGT.rds")
  remove(shp_ZT, shp_COM, OPI, TRJ)
  remove(MEN_EMP_brut, PERK_EMP)
  
  rapport("Calcul des tables de déplacement agrégées")
  # Changement août 2022 : DEPintoPER est intégré directement dans PER.
  PER = PER %>%
    courroie_typoCom(MEN) %>%
    patch_ZFenComUniques(tableZFdeComUniques) %>%
    patch_GeomFev2021(shp_ZF, PGT) %>%
    init_DEP2PER(DEP) %>%
    filtre_combinaisonsActivites()
  chk_Bases()
  
  # On vire MEN pour libérer de la mémoire et on sauvegarde
  save(PER, file="Data/PER.rds")
  remove(MEN)
  
  load("Data/TRJ.rds") ; remove(PGT)
  # On initialise ACT et HDP et on peut en répercuter les valeurs agrégées dans PER
  ACT = init_ACT    (DEP, PER) ; save(ACT, file = "Data/ACT.rds")
  HDP = init_HDP    (ACT, PER) ; tracer_hdp(HDP, pdf= "Sorties/Heures de pointe.pdf") ; save(HDP, file = "Data/HDP.rds")
  PER = init_ACT2PER(PER, ACT) ; save(PER, file = "Data/PER.rds")
  PER = init_patternsAct(PER, ACT, tableZF2Com = charger_tableZF2Com(PER, DEP)) ; save(PER, file = "Data/PER.rds")
  load("Data/HDP.rds")
  PER = init_typoDep(PER, HDP, ACT, TRJ, DEP) ; save(PER, file = "Data/PER.rds") # impossible, ne fonctionne pas en local
  chk_Bases()
  
  # On vire TRJ pour la mémoire
  save(HDP, file="Data/HDP.rds") ; remove(HDP)
  save(TRJ, file="Data/TRJ.rds")
  
  load("Data/PGT.rds")
  load("Data/shp_ZT.rds")
  load("Data/shp_ZF.rds")
  
  rapport("Calcul de la géométrie de la table d'activités")
  PER = PER %>% init_geomAct(ACT, shp_ZF, PGT)
  chk_Bases()
  save(PER, file="Data/PER.rds")
  remove(ACT)
  
  rapport("Chargement de la base des véhicules")
  load("Data/MEN.rds")
  VEH = init_VEH(MEN)
  PER = attribuerVehicule(PER, TRJ)
  
  # On importe le fichier des véhicules de l'EMP qui ressemble déjà beaucoup au fichier des véhicules
  # que je crée (les années NA sont marquées par "." ce qui crée des parsing failures)
  VEH_EMP = init_base_csv("dico VEH_emp.csv", "EMP/q_voitvul_public_V2.csv", encodingBase = "latin1") %>%
    convert_EMPtoEMD_VEH() %>%
    left_join(select(MEN, uid_MEN, PCSMT, PCSMLT, MenTypo, ZoneDens), by="uid_MEN")
  VEH = mutate(VEH, Conso100 = NA)
  VEH = rbind(VEH, select(VEH_EMP, colnames(VEH)))
  
  # On peut attribuer directement à chaque membre de PER son véhicule principal déclaré dans VEH
  PER = attribuerVehiculeEMP(PER, VEH_EMP)
  remove(VEH_EMP)
  
  # on sélectionne les uid des véhicules hors EMP et en EMP, selon la disponibilité
  PER$uid_VEH = ifelse(is.na(PER$uid_VEH.y), PER$uid_VEH.x, PER$uid_VEH.y)
  PER$uid_VEH = as.character(PER$uid_VEH)
  PER$uid_VEH.x = NULL ; PER$uid_VEH.y = NULL
  
  # Nouveau carroyage
  PER = densitesZversPER(PER)
  
  save(VEH, file = "Data/VEH.rds") ; remove(VEH)
  save(PER, file = "Data/PER.rds")
  
  rapport("Vérifications")
  # Vérifications élémentaires
  if (shp_ZF$CODE_ZF %>% nchar() %>% unique() %>% length() != 1)
  { stop ("Les codes ZF ont des longueurs variables dans la base SHP") }
  
  verif_PCS      (PER, pdf= "Sorties/État de la base/Enquêtes PCS non renseignée.pdf")
  verif_PCSM     (PER, pdf= "Sorties/État de la base/Effectifs des PCS ménage.pdf")
  verif_tempsDepl(DEP, pdf= "Sorties/État de la base/Vitesses renseignées dans DEP.pdf")
  verif_dispoDep (DEP, pdf= "Sorties/État de la base/Exhaustivité des déplacements par enquête.pdf")
  verif_depIntoPer(PER)
  chk_Bases()
  
  load("Data/shp_ZF.rds") ; load("Data/PGT.rds")
  if (verif_MenGeom(MEN) > 0.9) { stop ("Plus de 90% des points ne peuvent être situés géométriquement") }

  filtres = init_filtres(PER, DEP, shp_ZF, PGT)
  filtreRec = verif_recensement(PER, z_Nomenclature, filtres,
                                pdfGen = "Sorties/État de la base/Comparaison au recensement (général).pdf",
                                pdfParEnq = "Sorties/État de la base/Comparaison au recensement (par enquête).pdf")
  filtres = left_join(filtres, filtreRec, by=c("uid_ENQ" = "uid_ENQ"))
  save(filtres,file="Data/filtres.rds")
  remove(filtres, filtreRec)
  
  rm(list = ls()[!ls() %in% garder], pos = globalenv())
  initMémoire(f_base = T, BasesCharger = c("PER", "DEP", "ACT"))
  rapport("Calcul de la table d'activités")
  activites = load_activites(PER, DEP, ACT, pasDeTemps = pasDeTemps)
  save(activites, file="activites.rds")
  remove(activites)
  
  load("Data/shp_ZTS.rds")
  # [Octobre 2022] prendre en compte nAct plutôt que N semble intéressant
  rapport("Calcul du champ Nombre d'activités")
  ACT_N = ACT %>%
    filter(substr(Tache,1,1) != "9" & substr(Tache, 1,1) != "0") %>%
    group_by(uid_PER) %>% summarize(nAct = n())
  PER = left_join(PER, ACT_N, by=c("uid_PER" = "uid_PER"))
  PER = mutate(PER, nAct = ifelse(N == 0, 0, nAct))
  
  rapport("Calcul du champ Nombre d'activités hors travail")
  ACT_N = ACT %>%
    filter(substr(Tache,1,1) != "9" & substr(Tache, 1,1) != "0" & substr(Tache, 1,1) != "1") %>%
    group_by(uid_PER) %>% summarize(nActHorsTrav = n())
  PER = left_join(PER, ACT_N, by=c("uid_PER" = "uid_PER"))
  PER = mutate(PER, nActHorsTrav = ifelse(N == 0, 0, nActHorsTrav))
  remove(ACT_N)
  
  rapport("Calcul de champs hors des fonctions de génération principales")
  # Anciens champs préparatoires
  # Calcul de champs travMax pour PER
  PER = calculer_lieuTravail(base = PER, typo = T, ACT = ACT, PER = PER, DEP = DEP)
  
  # Âge factorisé en classes
  PER = filter(PER, !is.na(Age))
  PER$Age5  = etqAge(PER$Age)
  PER$Age10 = etqAge(PER$Age, pas = 10, min = 4, max = 85)
  # Distance réellement estimée par réseau
  PER = PER %>% mutate (DisOk = Dis_VEL + Dis_DRM + Dis_VOI)
  # Variables de MEN utiles dans PER
  load("Data/MEN.rds")
  PER = left_join(PER, select(MEN, LogOcc, LogType, MenCouple, MenEnfants, uid_MEN),
                  by=c("uid_MEN" = "uid_MEN"))
  
  # [Janvier 2023] On va calculer quelques champs utiles pour caractériser un peu +
  # TODO: intégrer ça au script principal de génération de la base si c'est utile
  
  # Longueur moyenne des plages de travail
  lgrPlages = ACT %>%
    filter(substr(Tache,1,1) == "1" | Tache == "810") %>%
    group_by(uid_PER) %>%
    summarize(duTvlPlage = mean(du))
  PER = left_join(PER, lgrPlages, by="uid_PER") ; remove(lgrPlages)
  
  # Part de temps passée sur le lieu de travail principal de la journée
  lieuTravPrinc = ACT %>%
    filter(!is.na(l)) %>%
    filter(substr(Tache,1,1) == "1" | Tache == "810") %>%
    left_join(select(PER, uid_PER, ZF_travMax), by="uid_PER") %>%
    mutate(lieuPrincipal = ifelse(l == ZF_travMax, "oui", "non")) %>%
    group_by(uid_PER, lieuPrincipal) %>%
    summarize(du = sum(du)) %>%
    group_by(uid_PER) %>%
    mutate(pDu = du/sum(du) * 100) %>%
    pivot_wider(names_from = lieuPrincipal, values_from = pDu, names_prefix = "tpsTvl") %>%
    summarize(across(starts_with("tpsTvl"), ~first(na.omit(.)))) %>%
    rename(duTvl_lPrinc = tpsTvloui)
  PER = left_join(PER, select(lieuTravPrinc, uid_PER, duTvl_lPrinc), by="uid_PER")
  remove(lieuTravPrinc)
  
  # Nombre de lieux de travail distincts
  lxDistincts = ACT %>%
    filter(!is.na(l)) %>%
    filter(substr(Tache,1,1) == "1"  | Tache == "810") %>%
    group_by(uid_PER) %>%
    summarize(nbLxTvl = length(unique(l)))
  PER = left_join(PER, lxDistincts, by="uid_PER")
  remove(lxDistincts)
  
  # [Janvier 2023] Et si on regardait uniquement les boucles incluant un trajet pour le travail ?
  # Une boucle = une série de déplacements entre deux passages au domicile
  bouclesTrav = DEP %>%
    mutate(travail = ifelse(O_Motif %in% c("11", "12", "13", "14", "81") |
                              D_Motif %in% c("11", "12", "13", "14", "81"), "travail", "non")) %>%
    group_by(uid_PER, IdBcl) %>%
    summarise(travail = ifelse("travail" %in% travail, "travail", "non"),
              motifs = paste(paste0(O_Motif, "→", D_Motif), collapse = ", "),
              DisBcl = sum(Dis),
              TpsBcl = sum(Duree),
              n = n())
  save(bouclesTrav, file="Data/bouclesTrav.rds")
  
  bouclesTravAg = bouclesTrav %>%
    pivot_wider(values_from = DisBcl, names_from = travail, names_prefix="bcl_") %>%
    mutate(bcl_travail = ifelse(is.na(bcl_travail) & !is.na(bcl_non), 0, bcl_travail)) %>%
    group_by(uid_PER) %>% summarise(Dis_bclTvl = sum(bcl_travail))
  PER = left_join(PER, bouclesTravAg, by="uid_PER") %>%
    mutate(Dis_pBclTvl = Dis_bclTvl/Dis * 100)
  save(PER, file="Data/PER.rds")
  
  # Fond de carte
  initMémoire(f_base = T)
  rapport("Chargement des données de fond de carte")
  fdCarte = map_initCarteParEnquete(simplifier = F)
  save(fdCarte, file = "Data/fdCarte.rds")
  
  
  # Préparation de variables d'analyse dans PER
  initMémoire(BasesCharger = "PER")
  rapport("Préparation des variables")
  if (class(PER$ZoneDens)[1] == "character") { PER$ZoneDens = factor(PER$ZoneDens,
                                                                     levels=c("1","2","3","4")) }
  if (class(PER$ZoneRang)[1] == "character") { PER$ZoneRang = factor(PER$ZoneRang,
                                                                     levels=c("0","1","2","3","4","5")) }
  PER$PCS8  = factor(PER$PCS8,  levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "00"))
  PER$PCSMT = factor(PER$PCSMT, levels = c("1", "2", "3AB", "3C", "4", "5", "6", "7e", "7i"))
  PER$PCS42S  = factor(PER$PCS42S,  levels = sort(levels(PER$PCS42S)))
  
  # Champs spéciaux pour l'analyse factorielle
  PER = PER %>% mutate(
    rapDomCtdDis = disDomCtd / Dis.V,
    pDis_TCO = pDis_BUS + pDis_TRN,
    pDis_CTR = (Dis_Com + Dis_Svc + Dis_San)/ Dis.V,
    pDis_LSR = (Dis_Lsr + Dis_Vst)/Dis.V,
    V = (Dis.V/1000) / (Tps/60),
    DuCtt = DuCom + DuSvc,
    DuExt = DuCom + DuSvc + DuTax + DuLsr,
  ) 
  
  # Variable de Julie associant ZT et ZT_travMax
  PER = PER %>%
    mutate(ZoneDens_duo = case_when(!is.na(ZT) & ZT == ZT_travMax ~
                                      paste0("Local ", etqZoneDens(ZoneDens, supprTrFaible = T)),
                                    !is.na(ZoneDens) & !is.na(ZoneDens_travMax)     ~
                                      paste0(etqZoneDens(ZoneDens, supprTrFaible = T), " → ",
                                             etqZoneDens(ZoneDens_travMax, supprTrFaible = T))),
           ZoneDens_duo = factor(ZoneDens_duo, levels = c("Densité forte → Densité forte",
                                                          "Densité forte → Densité interm.",
                                                          "Densité forte → Densité faible",
                                                          "Densité interm. → Densité forte",
                                                          "Densité interm. → Densité interm.",
                                                          "Densité interm. → Densité faible",
                                                          "Densité faible → Densité forte",
                                                          "Densité faible → Densité interm.",
                                                          "Densité faible → Densité faible",
                                                          "Local Densité forte",
                                                          "Local Densité interm.",
                                                          "Local Densité faible")))
  
  # Certains codes ZF n'ont plus d'uid_ENQ... J'ignore pourquoi (mai 2023)
  # Pansement sur jambe de bois
  PER = patch_codeZF(PER)
  
  
  save(PER, file="Data/PER.rds")
  
  load("Data/MEN.rds")
  enfPlusJeune = PER %>% group_by(uid_MEN) %>%
    summarise(enfPlusJeune = min(Age, na.rm=T)) %>%
    filter(enfPlusJeune < 17)
  MEN = left_join(MEN, enfPlusJeune, by="uid_MEN")
  save(MEN, file="Data/MEN.rds")
  
  # Préparation d'OPI
  load("Data/OPI.rds")
  OPI = OPI %>% left_join(select(PER, uid_PER, PCS8, PCS42S, ZoneDens, Fqc_Tco, Fqc_Vel,
                                 Activ, Age, Age10, PCSMLT, Genre), by="uid_PER")
  OPI = left_join(OPI, select(PER, uid_PER), by="uid_PER")
  save(OPI, file="Data/OPI.rds")
  
  
  load("Data/shp_ZT.rds")
  if (!"ZT" %in% colnames(shp_ZT))
  {
    shp_ZT$ZT = paste0(shp_ZT$uid_ENQ, shp_ZT$CODE_SEC)
  }
  save(shp_ZT, file="Data/shp_ZT.rds")
  
  source("START.R") ; gc(full = T)
  initMémoire(BasesCharger = c("PER", "MEN", "shp_COM", "shp_ZT", "shp_ZTS"))
  
  
  
  # remove(tabPopPER, tabPop)
  
  save(PER, file = "Data/PER.rds")
  
  
  # Effacement des figures
  rapport("Effacement des figures")
  listeFigures = c(paste0("Sorties/Figures/", dir("Sorties/Figures", include.dirs = F)),
                   paste0("Sorties/Figures/Cartes/", dir("Sorties/Figures/Cartes", include.dirs = F)),
                   paste0("Sorties/Figures/Variance/", dir("Sorties/Figures/Variance", include.dirs = F)),
                   paste0("Sorties/Figures/Opinion/", dir("Sorties/Figures/Opinion", include.dirs = F)),
                   paste0("Sorties/Figures/Tris à plat/", dir("Sorties/Figures/Tris à plat", include.dirs = F)),
                   paste0("Sorties/Figures/Prise en charge/", dir("Sorties/Figures/Prise en charge", include.dirs = F)))
  
  listeFigures = listeFigures[grepl(".", listeFigures, fixed = TRUE)]
  #if(!resetCart) {listeFigures[!grepl("Cartes", listeFigures, fixed = TRUE)]}
  
  rapport(length(listeFigures), "fichiers à supprimer", info=T)
  file.remove(listeFigures)
}

carteEnquêtes = function(shp_ZT, shp_ZTS, MEN, PER)
{
  library(mapsf)
  
  rapport("Génération de la carte des enquêtes")
  
  shp_ZT = shp_ZT %>% st_transform(crs = 2154)
  shp_ZTS = shp_ZTS %>% st_transform(crs = 2154)
  
  if ("sf" %in% class(MEN))
  {
    MEN = st_drop_geometry(MEN)
  }
  
  nEnqParEnquête = PER %>% group_by(uid_ENQ) %>% summarize(N = n())
  MENsum         = MEN %>% group_by(uid_ENQ) %>% summarize(Enq = first(Enq))
  nEnqParEnquête = left_join(nEnqParEnquête, MENsum, by=c("uid_ENQ" = "uid_ENQ"))
  
  rapport("Constitution des aires d'enquêtes", info=T)
  surf_ZT  = shp_ZT  %>% group_by(uid_ENQ) %>% summarize()
  surf_ZTS = shp_ZTS %>% group_by(uid_ENQ) %>% summarize()
  centroides = rbind(st_point_on_surface(surf_ZT), st_point_on_surface(surf_ZTS))
  aires = rbind(surf_ZT, surf_ZTS) %>% st_buffer(dist = 100) %>% st_simplify(dTolerance=20)
  
  nEnqParEnquête = left_join(nEnqParEnquête, centroides, by=c("uid_ENQ" = "uid_ENQ")) %>% st_as_sf()
  
  nEnqParEnquête$Enq = as.character(nEnqParEnquête$Enq)
  nEnqParEnquête[nEnqParEnquête$uid_ENQ == "IDF2010",]$Enq = "EGT"
  nEnqParEnquête[nEnqParEnquête$uid_ENQ == "PFO2012",]$Enq = "EDTR"
  nEnqParEnquête$Enq = as.factor(nEnqParEnquête$Enq)
  
  rapport("Préparation du maillage départemental", info=T)
  depts = read_sf("Sources/Fond Carte/DEPARTEMENT.shp")
  
  rapport("Traçage des graphiques", info=T)
  
  orangeTransparent = col = rgb(red = 255, green = 165, blue = 0, alpha = 40, max=255)
  airesGuya = filter(aires, uid_ENQ == "CAY2010")
  
  mf_init(x = depts, expandBB = c(0,.10,0,.25))
  plot(depts$geometry, col = "white", border = "grey80", add=T)
  plot(aires$geometry, col = orangeTransparent,
       border = "orange", add=T)
  mf_map(x = nEnqParEnquête, type = "prop_typo", var = c("N", "Enq"), pal = "Dynamic",
         alpha=.8, inches = .15, leg_pos = c("bottomleft2", "left"),
         leg_title = c("Taille de l'échantillon", "Type d'enquête"))
  
  mf_layout(title = "Localisation des enquêtes origine-destination (France hexagonale) de la Base Unique du Cerema", 
            credits = paste0("Données : Base Unique du Cerema (2018)\nRéalisation : M. Guinepain"))
  
  # carte= ggplot(data = nEnqParEnquête) + coord_sf() +
  #     geom_sf(data = depts, fill = "white", colour = "grey80") +
  #     geom_sf(data = aires, fill = "orange", alpha=.2,
  #             colour = "orange") +
  #     geom_sf(aes(colour = Enq, size = N)) +
  #     scale_size_binned(name = "Échantillon enquêté", range = c(.4, 10)) +
  #     scale_colour_hue(name = "Type d'enquête") +
  #     theme_bw() #+
  # theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  # hexa  = carte + xlim(-520000, 1060000) + ylim(5100000, 6600000)
  # marti = carte + xlim(-6820000, -6760000) + ylim(1612500, 1677500) + labs(title = "Martinique")
  # guya  = carte + xlim(-6130000, -5705000) + ylim(225000, 650000)   + labs(title = "Guyane")
  # reu   = carte + xlim(6138000, 6225000) + ylim(-2451000, -2364000) + labs(title = "Réunion",
  # caption = "B.U. Cerema, 2009-2019 / OpenStreetMap\nFigure : Maxime Guinepain")
  
  # outremers = ggpubr::ggarrange(guya, marti, reu, ncol=1, nrow=3, legend = "none")
  # page = ggpubr::ggarrange(hexa, outremers, ncol=2, nrow=1, common.legend = T, legend="left", widths = c(2.7,1))
  # page = ggpubr::annotate_figure(page, fig.lab = "Répartition des Enquêtes Origine/Destination\nde la Base Unique du Cerema",
  #                                fig.lab.pos = "top.left", fig.lab.face = "bold", fig.lab.size = 16)
  
  #return(page)
}