# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                   E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                     #
#                                                                                                 #
#                               SCRIPTS DE TRAVAIL M. GUINEPAIN                                   #
#                                           SEPT 2022                                             #
#                                                                                                 #  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# Modèles économétriques ====
# lire https://stats.oarc.ucla.edu/r/dae/logit-regression/

regressionLogOpt = function(base, val, listeControles, poids, retirerZ = T, facteurDiv = 1,
                         legVal="valeur mesurée", imprDistrib = T, testIndivControle = T,
                         seuil=50, 
                         verbose=F)
{
    # Donnons un nom standard à toutes les variables
    colnames(base)[colnames(base) == val] = "val"
    colnames(base)[colnames(base) == poids] = "poids"
    for(i in 1:length(listeControles))
    {
        colnames(base)[colnames(base) == listeControles[i]] = paste0("controle_", i)
    }
    
    if (retirerZ) { base = filter(base, val>0) ; if(verbose){cat("\nZéros retirés")}}
    
    # Appliquons le facteur de division
    base$val = base$val/facteurDiv
    
    # Sortie : graphique représentant la distribution de la variable
    if (imprDistrib)
    {
        g2 = ggplot(data = base, aes(x = log(val))) +
            geom_histogram(aes(y = ..density..), colour="black", fill="white") +
            stat_function(fun = dnorm, lwd = 1.3, col = 'red', alpha=.7,
                          args = list(mean = mean(log(base$val), na.rm=T),
                                  sd = sd(log(base$val), na.rm=T))) +
            labs(title = paste0("Distribution logarithmique de la variable ", val),
            caption = "Données Cerema 2009-2019") +
            xlab(legVal) + ylab("densité") 
        print (g2)
    }
    
    # Sortie : contribution individuelle de chaque contrôle
    if (testIndivControle)
    {
        if (verbose) {cat("\nTest individuel des contrôles")}
        table = tibble(variable = character(), r2 = numeric())
        for(i in 1:length(listeControles))
        {
            if (verbose) {cat("\rTest individuel des contrôles :", listeControles[i])}
            mod = lm(paste0("log(val) ~ controle_", i), weights = poids, data=base)
            r2 = summary(mod)$r.squared
            table[i,]$variable = listeControles[i] ; table[i,]$r2 = r2
        }
        table = tab_Tri(table, 1, rev=T, parCol = "r2")
        
        g1 = ggplot(data = table, aes(x = variable, y = r2)) +
             geom_col() +
             labs(title = "Contribution de chaque variable de contrôle dans un modèle simple") +
             xlab("variable de contrôle") + ylab("R²")
        print(g1)
    }
    
    listeIncompats = tibble(val1 = character(), val2 = character(), compat = factor())
    for(i in (1:(length(listeControles)-1)))
    {
        for(j in (i+1):length(listeControles))
        {
            if (verbose) cat("\ncroisement", listeControles[i], "et", listeControles[j])
            testCroisements = table(base[[paste0("controle_", i)]],
                                    base[[paste0("controle_", j)]]) %>%
                as.data.frame()
            
            if(min(testCroisements$Freq) < seuil) {
                listeIncompats =
                    add_row(listeIncompats, val1= paste0("controle_", i),
                            val2=paste0("controle_", j), compat= "incompatible")
                if (verbose) {cat(" → incompatibles")}
            } else {
                listeIncompats =
                    add_row(listeIncompats, val1= paste0("controle_", i),
                        val2=paste0("controle_", j), compat= "compatible")
            }
        }
    }
    print(listeIncompats)
    g3 = ggplot(data = listeIncompats, aes(x = val1, y = val2)) + geom_point(aes(color=compat), size=2)
    print(g3)
    
    listeIncompatsI = pivot_wider(listeIncompats, names_from=val2, values_from=compat)
    listeIncompatsJ = pivot_wider(listeIncompats, names_from=val1, values_from=compat)
    
    formules = character()
    for (i in 1:nrow(listeIncompatsI))
    {
        formule = c(listeIncompatsI[i,]$val1, colnames(listeIncompatsI)[which(listeIncompatsI[i,] == "compatible")])
        if (length(formule > 1))
        {
            formule = paste(formule, collapse = " + ")
            formules = c(formules, formule)
        }
    }
    for (i in 1:nrow(listeIncompatsJ))
    {
        formule = c(listeIncompatsJ[i,]$val2, colnames(listeIncompatsJ)[which(listeIncompatsJ[i,] == "compatible")])
        if (length(formule > 1))
        {
            formule = paste(formule, collapse = " + ")
            formules = c(formules, formule)
        }
    }
    
    if(verbose) {cat("\nTest formule :")}
    table = tibble(variable = character(), r2 = numeric())
    for(i in 1:length(formules))
    {
        if (verbose) {cat("\rTest formule :", formules[i], "                        ")}
        mod = lm(paste0("log(val) ~ ", formules[i]), weights = poids, data=base)
        r2 = summary(mod)$r.squared
        table[i,]$variable = formules[i] ; table[i,]$r2 = r2
    }
    table = tab_Tri(table, 1, rev=T, parCol = "r2")
    
    if (nrow(table)>20) {table = table[1:20,]}
    
    g5 = ggplot(data = table, aes(x = variable, y = r2)) +
        geom_col() +
        labs(title = "Tests de modèles") +
        xlab("modèle testé") + ylab("R²") + coord_flip()
    print(g5)
    
    bestModele = lm(paste0("log(val) ~ ", table[1,]$variable), weights = poids, data=base)
    bestModele2 = lm(paste0("log(val) ~ ", table[2,]$variable), weights = poids, data=base)
    bestModele3 = lm(paste0("log(val) ~ ", table[3,]$variable), weights = poids, data=base)
    return(list(bestModele, bestModele2, bestModele))
}



regressionLog = function(base, val, formule, colComparaison = NULL, colCompNt = F,
                         poids = NULL, retirerZ = T, facteurDiv = 1,
                         titre = "Représentation graphique du modèle",
                         legVal="valeur mesurée", unite = "", imprDistrib = F, verbose=F,
                         refDescr="", caption=src_fig(),
                         valIntervalleSur100 = 1, fLog = T, returnFig = F)
{
    # Le champ refDescr est obsolète ; il n'est laissé que pour ne pas créer de
    # rupture de compatibilité
    
    if (is.null(poids)) { base$poids = 1 ; poids = "poids" }
    
    # Nom standard pour toutes les variables et vérification des formats
    colnames(base)[colnames(base) == val] = "val"
    colnames(base)[colnames(base) == poids] = "poids"
    if (!is.null(colComparaison)) { colnames(base)[colnames(base) == colComparaison] = "compare"
        if (!is.factor(base$compare)) { stop("La colonne à comparer doit êre un facteur") }
    }
    
    # Retirer les 0 de la variable à tester pour éviter les problèmes avec le logarithme
    if (retirerZ) { base = filter(base, val>0) ; if(verbose){cat("\nZéros retirés")}}
    
    # Facteur de division pour travailler en km ou en h plutôt qu'en m ou en mn
    base$val = base$val/facteurDiv
    
    # Sortie : graphique représentant la distribution de la variable
    # (vérifier que la distribution est à peu près normale)
    if (imprDistrib)
    {
        if (fLog)
        {
            g2 = ggplot(data = base, aes(x = log(val))) +
                geom_histogram(aes(y = ..density..), colour="black", fill="white") +
                stat_function(fun = dnorm, lwd = 1.3, col = 'red', alpha=.7,
                              args = list(mean = mean(log(base$val), na.rm=T),
                                          sd = sd(log(base$val), na.rm=T))) +
                labs(title = paste0("Distribution logarithmique de la variable ", val),
                     caption = caption) +
                xlab(paste(legVal, "au logarithme")) + ylab("densité") 
        } else {
            g2 = ggplot(data = base, aes(x = val)) +
                geom_histogram(aes(y = ..density..), colour="black", fill="white") +
                stat_function(fun = dnorm, lwd = 1.3, col = 'red', alpha=.7,
                              args = list(mean = mean(base$val), na.rm=T),
                                          sd = sd(base$val), na.rm=T) +
                labs(title = paste0("Distribution logarithmique de la variable ", val),
                     caption = caption) +
                xlab(paste(legVal)) + ylab("densité")
        }
        
        if (!is.null(colComparaison)) { g2 = g2 + facet_wrap(~compare) }
        
        print (g2)
    }
    
    # Si colComparaison est le nom d'une variable, on fait tourner plusieurs variantes du modèle
    # pour des sous-échantillons qui correspondent aux différentes valeurs
    # Exemple : si colComparaison est "Genre", on fait un modèle pour les H et un modèle pour les F
    # Sinon, un seul modèle simple
    
    if (is.null(colComparaison)) {
        if (fLog) {
            mod = lm(paste0("log(val) ~ ", formule), weights = poids, data=base)
        } else {
            mod = lm(paste0("val ~ ", formule), weights = poids, data=base)
        }
        
        
        # On transforme les sorties du modèle en tableau ggplot compatible
        if (fLog) {
            table = tibble(variable =  rownames(summary(mod)$coefficients),
                           coeff = as.double(exp(summary(mod)$coefficients[,1])),
                           p_value = as.double(summary(mod)$coefficients[,4]),
                           ref = "")
        } else {
            table = tibble(variable =  rownames(summary(mod)$coefficients),
                           coeff = as.double(summary(mod)$coefficients[,1]),
                           p_value = as.double(summary(mod)$coefficients[,4]),
                           ref = "")
        }
        
        # La première ligne est l'intercept, qu'on retire du tableau
        intercept = table[1,]$coeff
        table = table[2:nrow(table),]
        
        # On ajoute au tableau les valeurs de référence de chaque variable facteur
        # Elles correspondent au premier niveau de chaque facteur
        # Si la variable n'est pas un facteur, elle donnera un NULL qui sera expurgé
        vars = as.character(unlist(strsplit(as.character(unlist(strsplit(formule, " + ", fixed=T))),
                                            " * ", fixed = T)))
        defs = sapply(X = vars, FUN = function(x){levels(base[[x]])[1]})
        ref = tibble(variable = paste0(vars, defs),
                     coeff = NA,
                     p_value = NA, ref = "< valeur de référence >")  %>%
            filter(!substr(variable, nchar(variable)-3, nchar(variable)) == "NULL")
        
        table = rbind(table, ref)
        
    } else {
        # Pour faire plusieurs modèles, on prépare à l'avance le même tableau (avec col compare)
        # On va le remplir avec une boucle récursive
        table = tibble(coeff = numeric(), p_value = numeric(), variable = character(), compare = character())
        
        # On connaît déjà les valeurs de référence : on peut générer l'extrait qui sera ajouté
        # dans chaque variante du modèle
        vars = as.character(unlist(strsplit(as.character(unlist(strsplit(formule, " + ", fixed=T))), " * ", fixed = T)))
        defs = sapply(X = vars, FUN = function(x){levels(base[[x]])[1]})
        ref = tibble(variable = paste0(vars, defs),
                     coeff = NA,
                     p_value = NA, ref = "< valeur de référence >") %>%
            filter(!substr(variable, nchar(variable)-3, nchar(variable)) == "NULL")
        
        # Init de la barre de progression
        b = ui_ProgInit(length(levels(base$compare)))
        
        # Boucle : pour chaque variante du modèle
        for(i in 1:length(levels(base$compare)))
        {
            # Il faut qu'il y ait au moins 1 individu pour que ce soit mathématiquement possible...
            # TODO: définir un seuil supérieur à partir duquel le modèle fait sens
            if (nrow(filter(base, compare == levels(compare)[i]))>1)
            {
                if (fLog)
                {
                    mod = lm(paste0("log(val) ~ ", formule), weights = poids,
                             data = filter(base, compare == levels(compare)[i]))
                    tableV = tibble(variable =  rownames(summary(mod)$coefficients),
                                    coeff = as.double(exp(summary(mod)$coefficients[,1])),
                                    p_value = as.double(summary(mod)$coefficients[,4]),
                                    ref = "")
                    tableV = rbind(tableV, ref)
                } else {
                    mod = lm(paste0("val ~ ", formule), weights = poids,
                             data = filter(base, compare == levels(compare)[i]))
                    tableV = tibble(variable =  rownames(summary(mod)$coefficients),
                                    coeff = as.double(summary(mod)$coefficients[,1]),
                                    p_value = as.double(summary(mod)$coefficients[,4]),
                                    ref = "")
                    tableV = rbind(tableV, ref)
                }
                
                # Le R² ne pourra plus être calculé ensuite, donc on le stocke... dans la colonne
                # du champ identifiant chaque modèle (il sera ainsi montré sur le graphique)
                tableV$compare = paste0(levels(base$compare)[i],
                                        "\nValeur de base : ", round(tableV[1,]$coeff, 1), " ", unite,
                                        ifelse(length(levels(base$compare))>8, "\n", " - "),
                                        "R² : ", round(summary(mod)$r.squared,2))
                tableV = tableV[2:nrow(tableV),]
                table = rbind(table, tableV)
            }
            
            ui_Prog(b, i)
        }
        
        # Si on veut comparer avec tout le monde dans le même panier
        if (colCompNt)
        {
            if (fLog) {
            mod = lm(paste0("log(val) ~ ", formule), weights = poids,
                     data = base)
            } else {
                mod = lm(paste0("val ~ ", formule), weights = poids,
                         data = base)
            }
            
            tableV = tibble(variable =  rownames(summary(mod)$coefficients),
                            coeff = as.double(exp(summary(mod)$coefficients[,1])),
                            p_value = as.double(summary(mod)$coefficients[,4]),
                            ref = "")
            tableV = rbind(tableV, ref)
            
            # Le R² ne pourra plus être calculé ensuite, donc on le stocke... dans la colonne
            # du champ identifiant chaque modèle (il sera ainsi montré sur le graphique)
            tableV$compare = paste0("< Tous les individus >",
                                    "\nValeur de base : ", round(tableV[1,]$coeff, 1), " ", unite,
                                    ifelse(length(levels(base$compare))>8, "\n", " - "),
                                    "R² : ", round(summary(mod)$r.squared,2))
            tableV = tableV[2:nrow(tableV),]
            table = rbind(table, tableV)
        }
    }
    
    # On transforme la p-value continue en un champ de 5 modalités
    table = mutate(table, p_value_adj = case_when(p_value <= .001 ~ "< 0,1%",
                                                  p_value >  .001 & p_value <= .01 ~ "< 1%",
                                                  p_value >  .01  & p_value <= .05 ~ "< 5%",
                                                  p_value >  .05  & p_value <= .1  ~ "< 10%",
                                                  p_value >  .1   ~ "> 10%"))
    etq_pVals = c("< 0,1%", "< 1%", "< 5%", "< 10%", "> 10%")
    table$p_value_adj = factor(table$p_value_adj, etq_pVals)
    
    # Les coefficients (déjà exponentialisés) sont convertis en % si nécessaire
    if (fLog) {
        table$coeff = (table$coeff - 1) * 100
    }
    
    # On trie les variables une première fois
    table = tab_Tri(table, i="variable", parCol="variable", rev=T)
    
    # On envoie les modalités des champs (avec leur préfixe, exemple : ZoneDens3) dans les champs
    # de réattribution des facteurs et ceux de transformation des noms d'étiquettes
    # Il en sortira des modalités lisibles
    table$variable = table$variable %>%
        etqActiv    (prefixe=T) %>%
        etqGenre    (prefixe=T) %>%
        etqLogOcc   (prefixe=T) %>%
        etqNivEtu   (prefixe=T) %>%
        etqNivEtuSpl(prefixe=T) %>%
        etqNivDip   (prefixe=T) %>%
        etqPCS8     (prefixe=T) %>%
        etqPCS42S   (prefixe=T) %>%
        etqPCSM     (prefixe=T) %>%
        etqZoneDens (prefixe=T, supprTrFaible = F) %>%
        etqZoneRang (prefixe=T) %>%
        etqFqc      (prefixe=T) %>%
        retirerPrefixeAge() %>%
        etqInverseDs() %>%
        etqInverseMotifs() |>
        etqDiversPrefixe()
    
    print(summary(mod))
    
    # Pour les dummy variables identifiables d'après leurs modalités possibles, on ajoute une
    # colonne. Par exemple, la fonction reconnaît "Homme" et "Femme" et restitue la variable
    # d'origine, "Genre"
    table$varNom = idSerieEtiquettes(table$variable)
    
    # Si ce n'est pas déjà écrit pour chaque modèle, on prépare un label avec l'intercept du modèle
    if (is.null(colComparaison))
    {
        lab_valref = paste0("Valeur de base : ", round(intercept,1), " ", unite,
                            " - R² : ", round(summary(mod)$r.squared,2))
    } else {
        lab_valref = NULL
    }
    
    # On initialise l'échelle de l'intervalle de Luc
    if (fLog) {
    ticks = c(-1 * valIntervalleSur100 * 2, -1 * valIntervalleSur100, 0,
              valIntervalleSur100, valIntervalleSur100 * 2) %>%
        transf_echelle_sur100_inverse() %>% round(0)
    
    # On détermine s'il faut mettre l'étiquette du graphique à gauche ou à droite
    # Avec l'Intervalle de Luc, il faut prendre en compte la symétrie autour de 0
    table$hjust = ifelse(table$coeff>0,
                         ifelse(table$coeff>ticks[4], 1,0),
                         ifelse(table$coeff<ticks[2], 0,1))
    } else {
        # sans intervalle de luc, on va juste éviter le 0,8x max
        
        table$hjust = ifelse(table$coeff>0, 1, 0)
        # ifelse(table$coeff>.8*max(table$coeff, na.rm=T), 1, 0)
    }
    
    if (verbose) {print(as.data.frame(table))}
    
    centreX = ifelse(fLog, 0, mean(c(min(table$coeff, na.rm=T), max(table$coeff, na.rm=T))))
    
    # Graphique en sortie
    gBilan = ggplot(data=filter(table, variable!="(Intercept)"), aes(x= variable, y=coeff)) +
        geom_col(aes(fill = p_value_adj)) +
        geom_text(aes(x = variable, y = centreX, label = ref, hjust=.5), colour = "grey40", size=2.8) +
        labs(title = titre,
             subtitle = lab_valref,
             caption = caption)+
        xlab("modalités testées") + ylab("effet (%) sur la valeur de référence") +
        scale_fill_manual(values=c("grey60", "grey80", "grey90", "#fff3c0", "#ffdfa8"),
                          breaks = etq_pVals, name="p-value")  #+ theme(legend.position = "bottom")
    
    if (fLog) {
        gBilan = gBilan + geom_text(aes(x= variable, y= coeff, label=paste0(" ", transf_echelle_sur100_labcourt(coeff), " "),
                                                hjust = hjust), size=3.5)
    } else {
        gBilan = gBilan + geom_text(aes(x= variable, y= coeff,
                                        label=paste0(" ", ifelse(coeff > 0, "+", ""), round(coeff,1), " ", unite, " "),
                                        hjust = hjust), size=3.5)
    }
    
    if (fLog) {
       gBilan = gBilan +
            scale_y_continuous(trans = trans_sur100, breaks = ticks,
                               labels = transf_echelle_sur100_lab(ticks,
                                                               deuxLignes = ifelse(!is.null(colComparaison), T, F))) +
            coord_flip(ylim = c(transf_echelle_sur100_inverse(-1 * valIntervalleSur100 * 2 - .1), 
                                transf_echelle_sur100_inverse(     valIntervalleSur100 * 2 + .1)))
    } else {
        gBilan = gBilan + coord_flip()
    }
    
    # Si on trouve bien des dummy variables, on leur crée des lignes en facet dans le graphique
    if (nrow(filter(table, !is.na(varNom)))>0 & is.null(colComparaison))
    { gBilan = gBilan + facet_grid("varNom", scales = "free_y", space = "free_y") }
    
    # S'il y a en plus plusieurs modèles testés, on leur crée des colonnes
    if (nrow(filter(table, !is.na(varNom)))>0 & !is.null(colComparaison))
    { gBilan = gBilan + facet_grid(varNom~compare, scales = "free_y", space = "free_y") }
    
    if (!returnFig) {
      # Sortie graphique
      print(gBilan)
      
      # Sortie modèle ;
      # TODO: sortir une collection de modèle quand colComparaison est activé
      return(mod)
    } else {
      return(gBilan)
    }

}

regression = function(base, val, formule, colComparaison = NULL, colCompNt = F,
                         poids = NULL, retirerZ = T, facteurDiv = 1,
                         titre = "Représentation graphique du modèle",
                         legVal="valeur mesurée", unite = "", imprDistrib = F, verbose=F,
                         refDescr="", caption=src_fig(),
                         valIntervalleSur100 = 1, returnFig = F)
{
    regressionLog(base = base, val = val, formule = formule, colComparaison = colComparaison,
                  colCompNt = colCompNt, poids = poids, retirerZ = retirerZ, facteurDiv = facteurDiv,
                  titre = titre, legVal = legVal, unite = unite, imprDistrib = imprDistrib,
                  verbose = verbose, refDescr = refDescr, caption = caption,
                  valIntervalleSur100 = valIntervalleSur100, fLog = F, returnFig = returnFig)
}

# Evaluation de l'efficacité du modèle logit
efficaciteBinom = function(mod, d, nomChamp){
  
  # Predicting the values for train dataset
  d$ClassPredicted <- predict(mod, newdata = d, "class")
  
  # Building classification table
  tab <- table(d[[nomChamp]], d$ClassPredicted)
  
  # Calculating accuracy - sum of diagonal elements divided by total obs
  return(round((sum(diag(tab))/sum(tab))*100,2))
}

logit = function(tab, val, formule, colComparaison = NULL, poids = NULL,
                 titre = "Représentation graphique du modèle",
                 legVal="valeur mesurée", unite = "", imprDistrib = F, verbose=F,
                 caption=src_fig(),
                 valIntervalleSur100 = 1, petit = F,
                 returnFig = T)
{
  # Vérification des paquets installés
  if(! "performance" %in% installed.packages()) { stop("Paquet \"performance\" requis") }
  
  # Nom standard pour toutes les variables et vérification des formats
  colnames(tab)[colnames(tab) == val] = "val"
  if (is.null(poids)) { tab$poids = 1 ; poids = "poids" }
  colnames(tab)[colnames(tab) == poids] = "poids"
  if (!is.null(colComparaison)) { colnames(tab)[colnames(tab) == colComparaison] = "compare"
  if (!is.factor(tab$compare)) { stop("La colonne à comparer doit êre un facteur") }
  if (setequal(levels(tab$compare), niv_ds)) { tab$compare = factor(tab$compare, levels=niv_ds) }
  }
  
  # Si colComparaison est le nom d'une variable, on fait tourner le modèle plusieurs fois
  # pour des sous-échantillons qui correspondent aux différentes valeurs
  # Exemple : si colComparaison est "Genre", on fait un modèle pour les H et un modèle pour les F
  # Sinon, un seul modèle simple
  
  if (is.null(colComparaison)) {
    # Un dummy modèle pour calculer le R2 de McFadden
    nulMod = glm("val ~ 1", weights=poids, data=tab, family = "binomial")
    
    # Formule principale 
    mod = glm(paste0("val ~ ", formule), weights = poids, data=tab,
              family = "binomial")
    
    # On transforme les sorties du modèle en tableau ggplot compatible
    table = tibble(variable =  rownames(summary(mod)$coefficients),
                   coeff = as.double(exp(summary(mod)$coefficients[,1])),
                   p_value = as.double(summary(mod)$coefficients[,4]),
                   ref = "")
    
    # La première ligne est l'intercept, qu'on retire du tableau
    intercept = table[1,]$coeff
    table = table[2:nrow(table),]
    
    # On ajoute au tableau les valeurs de référence de chaque variable facteur
    # Elles correspondent au premier niveau de chaque facteur
    # Si la variable n'est pas un facteur, elle donnera un NULL qui sera expurgé
    vars = as.character(unlist(strsplit(as.character(unlist(strsplit(formule, " + ", fixed=T))),
                                        " * ", fixed = T)))
    defs = sapply(X = vars, FUN = function(x){levels(tab[[x]])[1]})
    ref = tibble(variable = paste0(vars, defs),
                 coeff = NA,
                 p_value = NA, ref = "< valeur de référence >")  %>%
      filter(!substr(variable, nchar(variable)-3, nchar(variable)) == "NULL")
    
    table = rbind(table, ref)
    
  } else {
    # Pour faire plusieurs modèles, on prépare à l'avance le même tableau (avec col compare)
    # On va le remplir avec une boucle récursive
    table = tibble(coeff = numeric(), p_value = numeric(), variable = character(), compare = character())
    
    # On connaît déjà les valeurs de référence : on peut générer l'extrait qui sera ajouté
    # dans chaque variante du modèle
    vars = as.character(unlist(strsplit(as.character(unlist(strsplit(formule, " + ", fixed=T))), " * ", fixed = T)))
    defs = sapply(X = vars, FUN = function(x){levels(tab[[x]])[1]})
    ref = tibble(variable = paste0(vars, defs),
                 coeff = NA,
                 p_value = NA, ref = "< valeur de référence >") %>%
      filter(!substr(variable, nchar(variable)-3, nchar(variable)) == "NULL")
    
    # Init de la barre de progression
    b = ui_ProgInit(length(levels(tab$compare)))
    
    mliste = list()
    
    # Boucle : pour chaque variante du modèle
    for(i in 1:length(levels(tab$compare)))
    {
      # Il faut qu'il y ait au moins 1 individu pour que ce soit mathématiquement possible...
      # TODO: définir un seuil supérieur à partir duquel le modèle fait sens
      if (nrow(filter(tab, compare == levels(tab$compare)[i]))>1)
      {
        # Un dummy modèle pour calculer le R2 de McFadden
        nulMod = glm("val ~ 1", weights=poids,
                     data=filter(tab, compare == levels(compare)[i]),
                     family = "binomial")
        
        # Même procédure : modèle, passage dans un tableV
        mod = glm(paste0("val ~ ", formule), weights = poids,
                  data = filter(tab, compare == levels(compare)[i]),
                  family = "binomial")
        tableV = tibble(variable =  rownames(summary(mod)$coefficients),
                        coeff = as.double(exp(summary(mod)$coefficients[,1])),
                        p_value = as.double(summary(mod)$coefficients[,4]),
                        ref = "")
        tableV = rbind(tableV, ref)
        
        # Le R² ne pourra plus être calculé ensuite, donc on le stocke... dans la colonne
        # du champ identifiant chaque modèle (il sera ainsi montré sur le graphique)
        tableV$compare = paste0(i, ") ", levels(tab$compare)[i],
                                ifelse(length(levels(tab$compare))>8, "\n", " - "),
                                "R² McFadden : ",
                                round(1-(logLik(mod)/logLik(nulMod)), 3)[1])
        AtableV = tableV[2:nrow(tableV),]
        table = rbind(table, tableV)
        
        mliste[[i]] = mod
      }
      
      ui_Prog(b, i)
    }
  }
  
  # On transforme la p-value continue en un champ de 5 modalités
  table = mutate(table, p_value_adj = case_when(p_value <= .001 ~ "< 0,1%",
                                                p_value >  .001 & p_value <= .01 ~ "< 1%",
                                                p_value >  .01  & p_value <= .05 ~ "< 5%",
                                                p_value >  .05  & p_value <= .1  ~ "< 10%",
                                                p_value >  .1   ~ "> 10%"))
  etq_pVals = c("< 0,1%", "< 1%", "< 5%", "< 10%", "> 10%")
  table$p_value_adj = factor(table$p_value_adj, etq_pVals)
  
  # Les coefficients (déjà exponentialisés) sont convertis en %
  table$coeff = (table$coeff - 1) * 100
  
  # On trie les variables une première fois
  table = tab_Tri(table, i="variable", parCol="variable", rev=T)
  
  # On détermine s'il faut mettre l'étiquette du graphique à gauche ou à droite
  # Avec l'Intervalle de Luc, il faut prendre en compte la symétrie autour de 0
  limiteSup = .5*max(table$coeff, na.rm=T)
  limiteInf = .5*min(table$coeff, na.rm=T)
  table$hjust = ifelse(table$coeff>0,
                       ifelse(table$coeff>limiteSup, 1,0),
                       ifelse(table$coeff<limiteInf, 0,1))
  
  # On envoie les modalités des champs (avec leur préfixe, exemple : ZoneDens3) dans les champs
  # de réattribution des facteurs et ceux de transformation des noms d'étiquettes
  # Il en sortira des modalités lisibles
  table$variable = table$variable %>%
    etqActiv   (prefixe=T) %>%
    etqGenre   (prefixe=T) %>%
    etqLogOcc  (prefixe=T) %>%
    etqNivEtu  (prefixe=T) %>%
    etqPCS8    (prefixe=T) %>%
    etqPCS42S  (prefixe=T) %>%
    etqPCSM    (prefixe=T) %>%
    etqZoneDens(prefixe=T) %>%
    etqZoneRang(prefixe=T) %>%
    etqFqc     (prefixe=T) %>%
    retirerPrefixeAge() %>%
    etqInverseDs() %>%
    etqInverseMotifs() |>
    etqDiversPrefixe()
  
  # On imprime les détails
  print(as.data.frame(table))
  print(summary(mod))
  
  # Pour les dummy variables identifiables d'après leurs modalités possibles, on ajoute une
  # colonne. Par exemple, la fonction reconnaît "Homme" et "Femme" et restitue la variable
  # d'origine, "Genre"
  table$varNom = idSerieEtiquettes(table$variable)
  
  # On initialise l'échelle de l'intervalle de Luc
  ticks = c(-1 * valIntervalleSur100 * 2, -1 * valIntervalleSur100, 0,
            valIntervalleSur100, valIntervalleSur100 * 2) %>%
    transf_echelle_sur100_inverse() %>% round(0)
  
  # On crée le graphique de base
  gBilan = ggplot(data=filter(table, variable!="(Intercept)"), aes(x= variable, y=coeff)) +
    geom_col(aes(fill = p_value_adj)) +
    geom_text(aes(x= variable, y= coeff, label=paste0(" ", transf_echelle_sur100_labcourt(coeff), " "),
                  hjust = hjust), size=3.5) +
    geom_text(aes(x = variable, y = 0, label = ref, hjust=.5), colour = "grey40", size=2.8) +
    labs(title = titre,
         caption = caption) +
    ylab("odds ratio") + xlab("modalités testées")  +
    scale_fill_manual(values=c("grey60", "grey80", "grey90", "#fff3c0", "#ffdfa8"),
                      breaks = etq_pVals, name="p-value") +
    scale_y_continuous(trans = trans_sur100, breaks = ticks,
                       labels = transf_echelle_sur100_lab(ticks, deuxLignes = petit)) +
    coord_flip(ylim = c(transf_echelle_sur100_inverse(-1 * valIntervalleSur100 * 2 - .1), 
                        transf_echelle_sur100_inverse(     valIntervalleSur100 * 2 + .1)))  #+ theme(legend.position = "bottom")
  
  if (is.null(colComparaison))
  {
    gBilan = gBilan + labs(subtitle = paste0("Pseudo-R² de McFadden : ",
                                             round(1-(logLik(mod)/logLik(nulMod)), 3)))
  }
  
  # Si on trouve bien des dummy variables, on leur crée des lignes en facet dans le graphique
  if (nrow(filter(table, !is.na(varNom)))>0 & is.null(colComparaison))
  { gBilan = gBilan + facet_grid("varNom", scales = "free", space = "free_y") }
  
  # S'il y a en plus plusieurs modèles testés, on leur crée des colonnes
  if (nrow(filter(table, !is.na(varNom)))>0 & !is.null(colComparaison))
  { gBilan = gBilan + facet_grid(varNom~compare, scales = "free", space = "free_y") }
  
  if (!returnFig)
  {
    # Sortie graphique
    print(gBilan)
    
    # Sortie modèle ;
    # TODO: sortir une collection de modèle quand colComparaison est activé
    
    if (is.null(colComparaison))
    {
      return(mod)
    } else {
      return(mliste)
    }
  }
  
  if (returnFig)
  {
    return(gBilan)
  }
}

# Analyses factorielles (ACM ou ACP) ====


analyseFacto = function(base, colVar, colSup = NULL, colPoids = NULL, colUids, sortieTexte = F,
                        titre = "", 
                        silence=F, scaleW = F, desacFiltreNa = F, sortieBrute = F)
{
    colVar = select(base, colVar)

    if (!is.null(colSup)) { colSup = select(base, colSup) }
    if (!is.null(colPoids)) { colPoids = select(base, colPoids)[[1]] } 
    else { colPoids = rep.int(1, times = nrow(colVar)) }
    colUids  = select(base, colUids) [[1]]
    
    colVarNum = select(colVar, where(is.numeric))
    colVarFac = select(colVar, where(is.factor))
    
    if (!is.null(colSup)) {colSupFac = select(colSup, where(is.factor))} else { colSupFac = NULL }
    
    if (!desacFiltreNa)
    {
        # retirer les NAs quand il y en a dans les colonnes à analyser
        coltest = cbind(colVarNum, colVarFac, colPoids) %>% complete.cases()
        colVarNum = colVarNum[coltest,]
        colVarFac = colVarFac[coltest,]
        colPoids  = colPoids [coltest]
        # if (!is.null(colSup)) {colSupFac = colSupFac[coltest,]} else {colSupFac = data.frame()}
        colUids   = colUids  [coltest]
        colSup    = colSup   [coltest,]
    }
    
    if (ncol(colVarNum) > 0 & ncol(colVarFac) > 0)
    {
        stop("Variables quanti et quali détectées : faire un choix")
    }
    if (ncol(colVarNum) == 0 & ncol(colVarFac) == 0)
    {
        stop("Aucune variable (quanti ou quali) ne permet de conduire l'analyse")
    }
    

    if (ncol(colVarNum)>0 & ncol(colVarFac) == 0)
    {
        rapport("Calcul d'une ACP")
        etq = "ACP"
        
        if (scaleW)
        {
            poids = tibble(poids = colPoids)
            colVarNum = cbind(colVarNum, poids) %>%
                mutate(across(!poids, ~scale_w(., poids))) %>%
                select(-poids)
            rapport("Données numériques centrées-réduites", info=T)
        }
        
        af = ade4::dudi.pca(df = colVarNum, row.w = colPoids, scannf = F, nf = 5)
    }
    
    if (ncol(colVarNum) == 0 & ncol(colVarFac) > 0)
    {
        rapport("Calcul d'une ACM")
        etq = "ACM"
        
        af = ade4::dudi.acm(df = colVarFac, row.w = colPoids, scannf = F, nf = 5)
    }
    
    if (!silence)
    {
        
        resume = tibble(
            eig = af$eig,
            pVar = 100 * af$eig / sum(af$eig),
            pVarCum = cumsum(pVar)
        )
        resume$axe = paste0("C", c(1:nrow(resume)))
    
    # Variance expliquée par les axes
        
    rapport("Variance expliquée par les 5 premiers axes :", round(resume$pVarCum[5],3), "%", info=T)
    rapport("Nombre d'individus pris en compte :", length(colPoids), "soit",
            round(length(colPoids)/nrow(base)*100, 2), "% du total")
    
    g1 = ggplot(resume, aes(x = axe, y = pVar)) +
        geom_col() +
        xlab("Composantes") + ylab("Part de la variance (inertie, %)") +
        geom_line(aes(y = pVarCum, group = "variance\ncumulée", color = "variance\ncumulée")) +
        scale_color_manual(values = "grey", name = NULL) +
        geom_label(aes(label = paste0(round(pVar,1), "%"))) +
        labs(title = paste0(etq, " : Variance expliquée par les axes"),
             caption = src_fig(base))
    print(g1)

    if (sortieBrute) { return(af) }
    
    ade4::s.corcircle(af$co, sub = "Corrélations sur les axes 1 et 2")
    if (!is.null(colSup))
    {
        colSupNum = select(colSup, where(is.numeric))
        if (ncol(colSupNum) > 0)
        {
            afSup = ade4::supcol(af, colSupNum)
            ade4::s.corcircle(afSup$cosup, add.plot = T)
        }
    }
    
    ade4::s.corcircle(af$co, yax = 3, sub = "Corrélations sur les axes 1 et 3")
    if (!is.null(colSup))
    {
        colSupNum = select(colSup, where(is.numeric))
        if (ncol(colSupNum) > 0)
        {
            afSup = ade4::supcol(af, colSupNum)
            ade4::s.corcircle(afSup$cosup, add.plot = T)
        }
    }
    
    titre = paste0(etq, ": ", titre)
    
    if (nrow(base) > 10000) {
        g1 = ggplot(af$li, aes(x = Axis1, y = Axis2)) +
        geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        geom_density2d() +
        labs(title = paste0(titre, "\nRépartition des individus sur les axes 1 et 2"),
             caption = src_fig(base))
        g2 = ggplot(af$li, aes(x = Axis1, y = Axis3)) +
            geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
            geom_density2d() +
            labs(title = paste0(titre, "\nRépartition des individus sur les axes 1 et 3"),
                 caption = src_fig(base))
        cowplot::plot_grid(g1,g2, nrow=1) %>% print()
    } else {
        g1 = ggplot(af$li, aes(x = Axis1, y = Axis2)) +
            geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
            geom_point() +
            labs(title = paste0(titre, "\nRépartition des individus sur les axes 1 et 2"),
                 caption = src_fig(base))
        g2 = ggplot(af$li, aes(x = Axis1, y = Axis3)) +
            geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
            geom_point() +
            labs(title = paste0(titre, "\nRépartition des individus sur les axes 1 et 3"),
                 caption = src_fig(base))
        cowplot::plot_grid(g1, g2, nrow=1) %>% print()
    }
    
    if (!is.null(colSup))
    {
        colSupFac = select(colSup, where(is.factor))
        tabFac = cbind(af$li, colSupFac)
        
        for(champ in colnames(tabFac)[substr(colnames(tabFac), 1,4) != "Axis"])
        {
            g = tibble(Axis1 = tabFac$Axis1, Axis2 = tabFac$Axis2, champ = tabFac[[champ]],
                   poids = colPoids) %>%
                group_by(champ) %>% summarise(xMoy = weighted.mean(Axis1, poids),
                                              yMoy = weighted.mean(Axis2, poids))
            gg1 = ggplot(g, aes(x = xMoy, y = yMoy)) +
                geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
                geom_density2d(data = af$li, aes(x = Axis1, y = Axis2), colour = "grey", alpha = .4) +
                geom_label(aes(label = champ)) +
                coord_cartesian(xlim = c(min(g$xMoy) - .5, max(g$xMoy) + .5),
                                ylim = c(min(g$yMoy) - .5, max(g$yMoy) + .5)) +
                labs(title = paste0(titre,"\nVariable illustrative : ", champ))
            
            g = tibble(Axis1 = tabFac$Axis1, Axis3 = tabFac$Axis3, champ = tabFac[[champ]],
                       poids = colPoids) %>%
                group_by(champ) %>% summarise(xMoy = weighted.mean(Axis1, poids),
                                              yMoy = weighted.mean(Axis3, poids))
            gg2 = ggplot(g, aes(x = xMoy, y = yMoy)) +
                geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
                geom_density2d(data = af$li, aes(x = Axis1, y = Axis3), colour = "grey", alpha = .4) +
                geom_label(aes(label = champ)) +
                coord_cartesian(xlim = c(min(g$xMoy) - .5, max(g$xMoy) + .5),
                                ylim = c(min(g$yMoy) - .5, max(g$yMoy) + .5)) +
                labs(caption = src_fig(base))
            
            print(cowplot::plot_grid(gg1, gg2, nrow=1, align="h"))
        }
    }
    
    contrib = ade4::inertia.dudi(af, col.inertia = T)
    
    g = tibble(var = rownames(contrib$col.abs),
           Dim.1 = contrib$col.abs$Axis1, Dim.2 = contrib$col.abs$Axis2, Dim.3 = contrib$col.abs$Axis3) %>%
        tab_Tri("var", parCol = "Dim.1", rev = T) %>%
        pivot_longer(cols = starts_with("Dim."), values_to = "p", names_to = "Axe") %>%
        ggplot(aes(x = var, y = p)) +
        geom_col(aes(fill = Axe), position = "dodge") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        xlab("Variable") + ylab("Part de l'inertie expliquée par la variable (%)") +
        scale_fill_manual(values = c("grey75", "grey50", "grey25")) +
        labs(title = paste0("Contribution des variables à chaque axe de l'", etq),
             caption = src_fig(base))
    print(g)

    if (ncol(colVarNum)>0 & ncol(colVarFac) == 0)
    {
        g = tibble(var = rownames(contrib$col.rel),
               Dim.1 = contrib$col.rel$Axis1, Dim.2 = contrib$col.rel$Axis2, Dim.3 = contrib$col.rel$Axis3) %>%
            mutate(across(starts_with("Dim."), abs)) %>%
            tab_Tri("var", parCol = "Dim.1", rev = T) %>%
            pivot_longer(cols = starts_with("Dim."), values_to = "p", names_to = "Axe") %>%
            ggplot(aes(x = var, y = p)) +
            geom_col(aes(fill = Axe), position = "dodge") +
            theme(axis.text.x = element_text(angle = 45, hjust=1)) +
            xlab("Variable") + ylab("Qualité de représentation de la var par l'axe (%)") +
            scale_fill_manual(values = c("grey75", "grey50", "grey25")) +
            labs(title = paste0("Qualité de représentation des variables de l'", etq),
                 caption = src_fig(base))
        print(g)
    }
    
    if (nchar(titre) == 0) { titre = paste(colnames(colVar, collapse=", ")) }
    
    if (sortieTexte)
    {
        viz_Texte(texte=summary(af), wrap=F, lignesParPage = 40, largLigne = 110,
                  titre = titre) %>% print()
        # viz_Texte(texte=FactoMineR::dimdesc(af), wrap=F, lignesParPage = 40, largLigne = 110,
        #           titre = titre) %>% print()
        # print(summary(af))
        # print(FactoMineR::dimdesc(af))
    }

    # if (etq == "ACP") 
    # {
    #     g = plot(af, choix="varcor",
    #          title=paste0(titre, "\nCorrélation des variables sur les axes 1 et 2"))
    #     
    #     print (g + labs(caption = src_fig(base)))
    #     
    #     if (!is.null(colSupFac))
    #     {
    #         if (ncol(colSupFac) > 0)
    #         {
    #             g = plot(af, invisible="ind", cex=.7, autoLab = "yes",
    #                  title = paste0(titre,
    #                                 "\nPositionnement des variables illustratives sur les axes 1 et 2"))
    #             print (g + labs(caption = src_fig(base)))
    #         }
    #     }
    #     
    #     g = plot(af, choix="varcor", axes=c(1,3),
    #          title=paste0(titre, "\nCorrélation des variables sur les axes 1 et 3"))
    #     print (g + labs(caption = src_fig(base)))
    #     
    #     if (!is.null(colSupFac))
    #     {
    #         if (ncol(colSupFac) > 0)
    #         {
    #             g = plot(af, invisible="ind", cex=.7, autoLab = "yes", axes=c(1,3),
    #                  title = paste0(titre,
    #                                 "\nPositionnement des variables illustratives sur les axes 1 et 3"))
    #             print (g + labs(caption = src_fig(base)))
    #         }
    #     }
    # 
    #     #TODO : vérifier s'il y a des situations où colSupFac n'est pas null mais n'a aucune colonne
    # }
    
    # if (etq == "ACM")
    # {
    #     g = plot(af, invisible="ind", cex=.7, autoLab = "yes",
    #          title = paste0(titre,
    #                         "\nPositionnement des variables sur les axes 1 et 2"))
    #     print (g + labs(caption = src_fig(base)))
    #     
    #     g = plot(af, invisible="ind", cex=.7, autoLab = "yes", axes=c(1,3),
    #          title = paste0(titre,
    #                         "\nPositionnement des variables sur les axes 1 et 3"))
    #     print (g + labs(caption = src_fig(base)))
    # }

    }
    
    tibble(colUids = colUids,
           Dim.1 = af$li$Axis1,
           Dim.2 = af$li$Axis2,
           Dim.3 = af$li$Axis3,
           Dim.4 = af$li$Axis4,
           Dim.5 = af$li$Axis5) %>%
        return()
}

# Fonction copiée de R et espace, pour les acp
acp_PlotAxes = function(acp){
  summaryPca <- data.frame(
    EIG = acp$eig,
    PCTVAR = 100 * acp$eig / sum(acp$eig),
    CUMPCTVAR = cumsum(100 * acp$eig / sum(acp$eig))
  )
  barplot(summaryPca$PCTVAR,
          xlab = "Composantes",
          ylab = "Pourcentage de la variance (inertie)",
          names = paste("C", seq(1, nrow(summaryPca), 1)),
          col = "black",
          border = "white")
}

# Fonction pour caractériser une ACM
acm_PlotCategs = function(baseTriée, colCateg, colPoids, colSups = NULL)
{
  
  colnames(baseTriée)[colnames(baseTriée)==colCateg] = "categ"
  colnames(baseTriée)[colnames(baseTriée)==colPoids] = "poids"
  
  colVars = colnames(baseTriée)[!colnames(baseTriée) %in% c("categ", "poids", "uid_PER", "uid_DEP")]   
  
  # baseTriée = baseTriée %>% mutate(across(where(is.factor), ~factor(.x, levels = levels(.x), ordered = T)))
  
  # Etape 1. Tableau de la part de chaque valeur pour chaque variable
  tabMoy = baseTriée %>%
    pivot_longer(cols=colVars) %>%
    group_by(name, value) %>% summarize(poids = sum(poids), .groups="drop") %>%
    group_by(name) %>% mutate(moyPart = poids / sum(poids)) %>% ungroup() %>% select(-poids)
  
  # Etape 2. Tableau de la part de chaque valeur pour chaque variable PAR GROUPE
  tab = baseTriée %>%
    pivot_longer(cols=colVars) %>%
    group_by(categ, name, value) %>% summarize(poids = sum(poids), .groups="drop") %>%
    group_by(categ, name) %>% mutate(part = poids / sum(poids)) %>% ungroup()
  
  # Etape 3. Jointure et calcul de la sur ou sous-représentation
  tab = tab %>%
    left_join(tabMoy, by=c("name" = "name", "value" = "value")) %>%
    mutate(rapport = (part / moyPart * 100) - 100)
  
  # Etape 4. Dessin des graphiques individuels de sur ou sous-représentation
  # Une application particulièrement efficace de la fonction _lapply_ trouvée sur
  # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
  
  min = min(tab$rapport) ; max = max(tab$rapport)
  
  grapheVar = function(tab, col)
  {
    sousTab = tab %>% filter(name == col)
    g = ggplot(sousTab, aes(x = value, y=rapport))
    
    if (!is.null(palListe[[col]]))
    {
      g = g + geom_bar(aes(x=value, y=rapport, fill = value), stat="identity") +
        scale_fill_manual(values = palListe[[col]]) + guides(fill = F)
    } else {
      g = g + geom_bar(aes(x=value, y=rapport), stat="identity")
    }
    
    g=g+facet_grid(name~categ) + ylab("Surreprésentation de la catégorie (%)") +
      coord_flip(ylim=c(-100, 300)) + scale_x_discrete(limits=sort(levels(baseTriée[[col]]), decreasing=T)) +
      theme_bw() +
      theme(axis.title.y = element_blank())
    
    if (col != colVars[length(colVars)])
    { g = g + theme(axis.title.x = element_blank(),
                    axis.ticks.x = element_blank(), axis.text.x = element_blank()) }
    
    if (col != colVars[1])
    { g = g + theme(strip.background.x = element_blank(), strip.text.x = element_blank()) }
    
    return(g)
  }
  colPlots = lapply(X= colVars, FUN= grapheVar, tab=tab)
  
  # Etape 5. Assemblage sur une page
  hauteurs = unlist(lapply(colVars, function(x){length(levels(baseTriée[[x]]))}))
  hauteurs = hauteurs + 2
  hauteurs[1] = hauteurs[1] + 2
  hauteurs[length(hauteurs)] = hauteurs[length(hauteurs)] + 3
  
  grille = cowplot::plot_grid(plotlist = colPlots, ncol=1, align="v", rel_heights = hauteurs)
  
  return(grille)
}

# kMeans ====

nbCateg = function(inertie.expl, seuil = 1.1)
{
    if (length(inertie.expl) < 4) { stop ("Pas assez de catégories k-means possibles") }
    for (k in 3:length(inertie.expl)){
        taux = inertie.expl[k]/inertie.expl[k-1]
        if (taux < seuil) { return(k) }
    }
    stop("Aucune valeur optimale n'a été trouvée pour le nombre de catégories k-means")
}

categ_kMeans = function(coords, colUids = "colUids", nstart = 3,
                        imprVizInertie = T, seuilCateg = 1.1, nCateg = NULL,
                        nomColonne = "cluster",
                        titre = "Répartition des individus dans les classes k-means",
                        sub="")
{
    rapport("Catégorisation k-means")
    
    # si l'argument est un objet FactoMineR, on en récupère les coordonnées
    if ("MCA" %in% class(coords) | "PCA" %in% class(coords))
    {
        coords = coords$ind$coord
    }
    
    if (!colUids %in% colnames(coords)) { print(coords);stop("Colonne d'identifiants uniques introuvable") }
    uids = select(coords, colUids)[[1]]
    coords = select(coords, -colUids)
    
    
    # Si on force un nb de categs et qu'il n'y a pas de graphique, pas besoin de calculer l'optimum
    if (!is.null(nCateg) | imprVizInertie)
    {
        # Détermination automatique du nombre de catégories nécessaire
        inertie.expl <- rep(0,times=20)
        for (k in 2:20){
            clus <- kmeans(coords,centers=k, nstart=nstart, iter.max=500) 
            inertie.expl[k] <- clus$betweenss/clus$totss
        }
        nbGp = nbCateg(inertie.expl, seuil=seuilCateg)
        
        # Graphique représentant l'évolution de l'intertie expliquée par les catégories
        if (imprVizInertie) {
            tab = tibble(k = 1:20, i = inertie.expl)
            
            g = ggplot(tab, aes(x = k, y = i)) +
                geom_line(color = "grey70") +
                geom_point() +
                labs(title = "Détermination du nombre optimal de catégories k-means") +
                xlab("Nombre de groupes") + ylab("% inertie expliquée") +
                geom_vline(xintercept = nbGp, color = "violet") 
            
            if (!is.null(nCateg)) { g = g + geom_vline(xintercept = nCateg, color="red") }
            
            print(g)
        }
    }
    
    # Si un nb est imposé dans les arguments, il prend le pas sur l'optimum calculé
    if (!is.null(nCateg)) { nbGp = nCateg }
    
    rapport("Avec le réglage choisi", paste0("(k = ", nbGp, "),"), round(inertie.expl[k]*100,2),
            "% de l'inertie est expliquée par les classes k-means", info=T)
    
    # On calcule pour de bon
    kM = kmeans(coords, centers=nbGp, nstart=nstart, iter.max=500) 

    ret = tibble(uid = uids, cluster = kM$cluster)
    
    # On réordonne les groupes k-means dans la table (le plus gros vient toujours en premier)
    nvO=ret %>% group_by(cluster) %>% summarize(n = n()) %>%
        tab_Tri(i = "cluster", parCol="n", rev=T)
    nvO$to = c(1:nrow(nvO))
    lOrdre = as.character(nvO$to)
    names(lOrdre) = nvO$cluster
    ret$cluster = plyr::revalue(as.character(ret$cluster), lOrdre)
    
    g = group_by(ret, cluster) %>% summarize(n = n()) %>% 
        ggplot(aes(x = cluster, y = n)) +
        geom_col() + xlab("classe kM") + ylab("Nombre d'individus (non pondéré)") +
        labs(title = titre, caption = sub) 
    plot(g)
    
    colnames(ret)[colnames(ret) == "cluster"] = nomColonne
    
    return(ret)
}


kmeans_explo = function(tab, titre)
{
  inertie.expl <- rep(0,times=20)
  for (k in 2:20){
    clus <- kmeans(tab,centers=k, nstart=10, iter.max=500) 
    inertie.expl[k] <- clus$betweenss/clus$totss
  }
  p = plot(1:20,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée",
           main = paste("Inertie expliquée selon le nombre de classes k-means\n", titre))
  return(p)
}

kmeans_ordonner = function(tab, colKM, colPds = NULL)
{
  colKM_orig = colKM
  
  colnames(tab)[colnames(tab) == colKM]    = "colKM"
  
  if (is.null(colPds)) # si aucune col de poids précisée, alors tout le monde sera 1
  {
    tab$colPds = 1
    colPds_orig = NA
  }
  else
  {
    colPds_orig = colPds
    colnames(tab)[colnames(tab) == colPds] = "colPds"
  }
  
  nvO=tab %>% group_by(colKM) %>% summarize(tot = sum(colPds)) %>%
    tab_Tri(i = "colKM", parCol="tot", rev = T)
  nvO$to = c(1:nrow(nvO)) ; lOrdre = as.character(nvO$to) ; names(lOrdre) = nvO$colKM
  tab$colKM = plyr::revalue(as.character(tab$colKM), lOrdre)
  remove(nvO, lOrdre)
  
  colnames(tab)[colnames(tab) == "colKM"]    = colKM_orig
  
  if (!is.na(colPds_orig)) { colnames(tab)[colnames(tab) == "colPds"] = colPds_orig }
  else { tab = select(tab, -colPds) }
  
  return(tab)
}

km_Variance = function(tab)
{
  inertie.expl <- rep(0,times=20)
  b = ui_ProgInit(20)
  for (k in 2:20){
    clus <- kmeans(tab,centers=k, nstart=10, iter.max=500) 
    inertie.expl[k] <- clus$betweenss/clus$totss
    ui_Prog(barre = b, i = k)
  }
  plot(1:20,inertie.expl,type="b",xlab="Nb. de groupes",
       ylab="% inertie expliquée", main="Ajustement des k-means")
}


# CAH ====

categ_cah = function(coords, colUids = "colUids",
          nCateg = NULL, imprVizInertie=T,
          nomColonne = "cluster", nClassesMax = 30,
          titre = "Répartition des individus dans les classes de CAH",
          sub="")
{
    
    colnames(coords)[colnames(coords) == colUids] = "colUids"
    
    if ("cluster" %in% installed.packages())
    {
        rapport("CAH à l'aide du paquet \"Cluster\"")
        
        cah = cluster::agnes(select(coords, -colUids), metric = "euclidean", method = "ward")
        dendro = as.dendrogram(cah)
        
        
        if (imprVizInertie) {
            sort(cah$height, decreasing = T) %>% head(n=nClassesMax) %>%
                plot(type = "h", xlab=paste0(nClassesMax, " premiers nœuds"), ylab="Niveau d'agrégation",
                     main = paste0(titre, "\nNiveau d'agrégation sur les premiers nœuds"))
            
            relHeight = sort(cah$height, decreasing = T) / sum(sort(cah$height, decreasing = T)) * 100
            cumHeight = cumsum(relHeight)
            
            barplot(relHeight[1:nClassesMax], names.arg = c(1:nClassesMax), col = "black",
                    xlab = paste0(nClassesMax, " premiers nœuds"),
                    ylab="Part de l'inertie totale (%)")
            barplot(cumHeight[1:nClassesMax], names.arg = c(1:nClassesMax), col = "black",
                    xlab = paste0(nClassesMax, " premiers nœuds"),
                    ylab="Part de l'inertie totale (%)")
            
            plot(dendro, leaflab="none")
        }
        
        if (is.null(nCateg))
        {
            if (!imprVizInertie) { dev.new() ; plot(dendro, leaflab="none") ; off() }
            nCateg = readline(prompt = "Combien de catégories utiliser selon le graphe ci-contre ? ") %>%
                as.numeric()
        }
        
        coords$cluster = as.factor(cutree(cah, k = nCateg))
        colnames(coords)[colnames(coords) == "cluster"] = nomColonne
        colnames(coords)[colnames(coords) == "colUids"] = colUids
        return(coords)
    }
    
    
}


plotCateg = function(tab, cols, colCateg, colPoids = NULL, colUid, seuilAbsurde = 200,
                     titre="", capt="",
                     varActives = NULL, viewTab=F, valIntervalleSur100 = 2)
{
    if (is.null(colPoids)) { tab$colPoids = 1 ; colPoids = "colPoids" }
    colnames(tab)[colnames(tab) == colCateg] = "categ"
    colnames(tab)[colnames(tab) == colPoids] = "poids"
    colnames(tab)[colnames(tab) == colUid] = "uid"
    tab = select(tab, c("categ", "poids", "uid", cols))
    
    colnames(tab)[!colnames(tab) %in% c("categ", "poids", "uid")] =
      paste0("var_", colnames(tab)[!colnames(tab) %in% c("categ", "poids", "uid")])
    
    tab$categ = as.character(tab$categ)
    
    # Adoption de facteurs et d'intitulés lisibles
    tab = refactoriser(tab, num=T, simplifier=F)
    
    # Si les colonnes sont numériques, on veut une moyenne
    Nums = tab %>%
        group_by(categ) %>%
        summarize(across(where(is.numeric) & starts_with("var_"), ~weighted.mean(., poids, na.rm=T)))
    NumsT = tab %>% mutate(categ = "total") %>%
        group_by(categ) %>%
        summarize(across(where(is.numeric) & starts_with("var_"), ~weighted.mean(., poids, na.rm=T)))
    
    # Si les colonnes sont des facteurs, on veut la part de chaque level
    colFacs = tab %>%
        select(where(is.factor), categ, uid, poids)
    
    Facs = tibble(categ = sort(unique(tab$categ)))
    
    if (ncol(colFacs) > 3)
    {
        for(i in 1:(ncol(colFacs)-3))
        {
            cols = colFacs
            col_i = colnames(cols)[i]
            colnames(cols)[i] = "facteur"
            cols_i = pivot_wider(cols, names_from = facteur, names_sort = T,
                                 values_from = poids, names_prefix = paste0(col_i, ":")) %>% 
                group_by(categ) %>% summarize(across(where(is.numeric), ~sum(., na.rm=T))) 
            
            Facs = left_join(Facs, cols_i, by=c("categ" = "categ"))
        }
        
        popTot = tab %>% group_by(categ) %>% summarize(pop = sum(poids))
        Facs = left_join(popTot, Facs, by=c("categ" = "categ")) %>%
            mutate(across(where(is.numeric), ~.x/pop * 100)) %>% select(-pop)
        
        FacsT = tibble(categ = "total")
        for(i in 1:(ncol(colFacs)-3))
        {
            cols = colFacs
            col_i = colnames(cols)[i]
            colnames(cols)[i] = "facteur"
            cols_i = pivot_wider(cols, names_from = facteur, names_sort = T,
                                 values_from = poids, names_prefix = paste0(col_i, ":")) %>%
                mutate(categ="total") %>%
                group_by(categ) %>% summarize(across(where(is.numeric), ~sum(., na.rm=T))) 
    
            FacsT = left_join(FacsT, cols_i, by=c("categ" = "categ"))
        }
        
        popTot = tab %>% mutate(categ = "total") %>% group_by(categ) %>% summarize(pop = sum(poids))
        FacsT = left_join(popTot, FacsT, by=c("categ" = "categ")) %>%
            mutate(across(where(is.numeric), ~.x/pop * 100)) %>% select(-pop)
    }
    
    if (ncol(Nums) > 1) {
        Nums = pivot_longer(Nums, where(is.numeric), names_to = "variable", values_to = "valeur")
        NumsT = pivot_longer(NumsT, cols = where(is.numeric), names_to="variable", values_to="total") %>%
            select(-categ)
        Nums = left_join(Nums, NumsT, by=c("variable" = "variable"))
    }

    if (ncol(Facs) > 1) {
        Facs = pivot_longer(Facs, where(is.numeric), names_to = "variable", values_to = "valeur")
        FacsT = pivot_longer(FacsT, cols = where(is.numeric), names_to="variable", values_to="total") %>%
            select(-categ)
        Facs = left_join(Facs, FacsT, by=c("variable" = "variable"))
    }
    
    if (ncol(Nums) > 1 & ncol(Facs) > 1)
    {
        ret = rbind(Nums, Facs)
    }
    if (ncol(Nums) > 1 & ncol(Facs)== 1) { ret = Nums }
    if (ncol(Nums) ==1 & ncol(Facs) > 1) { ret = Facs }

    ret = mutate(ret, surrep = (valeur/total * 100) - 100)
    
    # On vire le préfixe var_ pour rendre le tableau plus lisible
    ret$variable = gsub(x = ret$variable, pattern = "var_", replacement = "")
    
    # Pour souligner dans le graphique quelles variables étaient actives :
    if (!is.null(varActives))
    {
        ret$varActive = ifelse(ret$variable %in% varActives, "active", "illustrative")
    }
    
    ret$varMain = ifelse(grepl(ret$variable, pattern=":"),
                         as.character(t(as.data.frame(strsplit(ret$variable, split = ":")))[,1]),
                         "Moyennes")
    
    ret$varSecd = ifelse(grepl(ret$variable, pattern=":"),
                         as.character(t(as.data.frame(strsplit(ret$variable, split = ":")))[,2]),
                         ret$variable)

    # On remplace les noms génériques par des noms lisibles
    ret$variable = nomColsLisibles(ret$variable)
    ret$varMain  = nomColsLisibles(ret$varMain)
    ret$varSecd  = nomColsLisibles(ret$varSecd)
    
    
    ret = ret %>% #mutate(across(is.numeric, ~ifelse(.>seuilAbsurde, seuilAbsurde, .))) %>%
        filter(!is.na(categ))
    
    if (viewTab) {cat("Variables actives", varActives) ; View (ret)}
    
    ticks = c(-1 * valIntervalleSur100 * 2, -1 * valIntervalleSur100, 0,
              valIntervalleSur100, valIntervalleSur100 * 2) %>%
        transf_echelle_sur100_inverse() %>% round(0)
    
    g = ggplot(data = ret, aes(x = varSecd, y = surrep)) +
        coord_flip(ylim = c(transf_echelle_sur100_inverse(-1 * valIntervalleSur100 * 2 - .1), 
                            transf_echelle_sur100_inverse(     valIntervalleSur100 * 2 + .1))) +
        labs(title = titre, caption = capt) +
        ylab("écart à la moyenne (%)") + xlab("Variables") +
        facet_grid(varMain~categ, scales = "free", space = "free_y") +
        scale_y_continuous(trans = trans_sur100, breaks = ticks) 
    
    if (is.null(varActives))  { g = g + geom_col() }
    if (!is.null(varActives)) { g = g + geom_col(aes(fill = varActive)) +
        scale_fill_manual(values = c("orange", "grey30"), name = "Variable")}
    
    print(g)
}


# Fonction inspirée / copiée-collée depuis R et espace
# Groupe ElementR (2014). R et espace: traitement de l’information géographique,
# Lyon : Framasoft (coll. «Framabook»).
cah_plotCateg = function(tableau, colonneCateg, etiquettes=NULL, couleurs=NULL)
{
  tableau = select_if(tableau, is.numeric)
  tableau$Categ = colonneCateg
  
  poidsCateg = table(tableau$Categ) %>% as.data.frame() ; sommeCateg = sum(poidsCateg$Freq)
  poidsCateg$Prop = poidsCateg$Freq / sommeCateg * 100
  profils <- aggregate(tableau[, 1:ncol(tableau)-1], by = list(tableau$Categ),
                       mean)
  
  colnames(profils)[1] = "Categ"
  profils$Categ = paste0(profils$Categ, " (", round(poidsCateg$Prop,1), "%)")
  
  if (!is.null(etiquettes)) {colnames(profils)[2:(length(etiquettes)+1)] = etiquettes}
  
  profils <- reshape2::melt(profils, id.vars = "Categ")
  g = ggplot(profils) +
    geom_bar(aes(x = variable, y = value, fill = Categ),
             stat = "identity") + ylab("Valeur centrée-réduite") + xlab("Variable") +
    guides(fill="none") +
    facet_wrap(~ Categ) +
    coord_flip() + theme_bw()
  
  if (is.null(couleurs))  {g = g + scale_fill_grey()}
  if (!is.null(couleurs)) {g = g + scale_fill_manual(values = couleurs)}
  
  return(g)
}

# objet de classe agnes / Fonction copiée dans le manuel R et espace
cah_PlotClasses = function(cah)
{
  sortedHeight = sort(cah$height, decreasing = TRUE)
  relHeight <- sortedHeight / sum(sortedHeight) * 100
  cumHeight <- cumsum(relHeight)
  barplot(relHeight[1:30], names.arg = seq(1, 30, 1),
          col = "black", border = "white", xlab = "Noeuds",
          ylab = "Part de l'inertie totale (%)")
}
tab_partCategsZT = function(tab, names_from, values_from, prefixe, champZT = "ZT", garderTotal=F)
{
  colnames(tab)[colnames(tab) == champZT] = "champZT"
  
  ret = pivot_wider(data = tab, names_from = names_from, values_from = values_from,
                    names_prefix = prefixe) %>%
    group_by(champZT) %>% summarise(across(starts_with(prefixe), sum, na.rm=T))
  
  ret$total = rowSums(ret[2:ncol(ret)])
  
  ret = mutate(ret, across(starts_with(prefixe), ~./total*100))
  
  if (!garderTotal) { ret = select(ret, -total) }
  
  colnames(ret)[colnames(ret) == "champZT"] = champZT
  
  return(ret)
}

profilageCategories = function(PER, champCateg)
{
  tableau = PER
  colnames(tableau)[colnames(tableau) == champCateg] = "categ"
  
  g1= wtd.table(tableau$categ, weights=tableau$CoeffRecEnq) %>% freq() %>%
    mutate(x = match(n, unique(n)), x = as.factor(x)) %>%
    ggplot(aes(x = x, y = n)) + geom_col() +
    geom_label(aes(label = paste(`val%`, "%"))) +
    labs(title = "Répartition des individus dans les catégories de l'analyse") +
    xlab("Catégorie") + ylab("Population pondérée") +
    theme_bw()
  
  g2= tableau %>% group_by(categ) %>%
    summarize(dis.V = weighted.mean(Dis.V, w = CoeffRecEnq, na.rm=T)/1000) %>%
    ggplot(aes(x = categ, y = dis.V)) + geom_col() +
    geom_label(aes(label = paste(round(dis.V,1), "km"))) +
    labs(title = "Distance moyenne totale par individu, par catégorie") +
    xlab("Catégorie") + ylab("Distance") +
    theme_bw() 
  
  g3= tableau %>% group_by(categ) %>%
    summarize(Tps = weighted.mean(Tps, w = CoeffRecEnq, na.rm=T)) %>%
    ggplot(aes(x = categ, y = Tps)) + geom_col() +
    geom_label(aes(label = heureMinToHr(Tps))) +
    labs(title = "Temps moyen en déplacement par catégorie") +
    xlab("Catégorie") + ylab("Temps en déplacement (minutes)") +
    theme_bw()
  
  g4= tableau %>% group_by(categ) %>%
    summarize(nAct = weighted.mean(nAct, w = CoeffRecEnq, na.rm=T)) %>%
    ggplot(aes(x = categ, y = nAct)) + geom_col() +
    labs(title = "Nombre d'activités moyen") +
    xlab("Catégorie") + ylab("Nombre d'activités hors domicile") +
    theme_bw() 
  
  if (!is.numeric(tableau$JoDeb)) {
    tableau = tableau %>%
      mutate(JoDeb = heureHHMMtoM(JoDeb), JoFin = heureHHMMtoM(JoFin))
  }
  
  g5= tableau %>% group_by(categ) %>%
    summarize(JoDeb = weighted.mean(JoDeb, w = CoeffRecEnq, na.rm=T),
              JoFin = weighted.mean(JoFin, w = CoeffRecEnq, na.rm=T)) %>%
    mutate(categ = factor(categ, levels=rev(sort(unique(categ))))) %>%
    pivot_longer(cols=c("JoDeb", "JoFin"), names_to="Heure", values_to="h") %>%
    ggplot(aes(x = categ, y = h/60)) + geom_point(aes(color = Heure)) +
    labs(title = "Horaires moyens de départ et retour au domicile") +
    xlab("Catégorie") + ylab("Heure") +
    geom_label(aes(label = heureMinToHr(h, secondes=F)), nudge_x = -.25) +
    scale_color_hue(labels = c("...de départ", "...de retour")) +
    coord_flip() +
    theme_bw() 
  
  g5b= tableau %>% group_by(categ) %>%
    summarize(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq, na.rm=T),
              JoTvFin = weighted.mean(JoTvFin, w = CoeffRecEnq, na.rm=T)) %>%
    mutate(categ = factor(categ, levels=rev(sort(unique(categ))))) %>%
    pivot_longer(cols=c("JoTvDeb", "JoTvFin"), names_to="Heure", values_to="h") %>%
    ggplot(aes(x = categ, y = h)) + geom_point(aes(color = Heure)) +
    labs(title = "Horaires moyens de départ et retour au lieu de travail") +
    xlab("Catégorie") + ylab("Heure") +
    geom_label(aes(label = heureMinToHr(h*60, secondes=F)), nudge_x = -.25) +
    scale_color_hue(labels = c("...de départ", "...de retour")) +
    coord_flip() +
    theme_bw() 
  
  partF = sum(filter(tableau, Genre == "F")$CoeffRecEnq, na.rm=T) / sum(tableau$CoeffRecEnq, na.rm=T) * 100
  
  g6= tableau %>% 
    mutate(pop = CoeffRecEnq) %>%
    pivot_wider(names_from = "Genre", names_prefix = "Genre_", values_from = CoeffRecEnq) %>%
    group_by(categ) %>% summarize(across(starts_with("Genre_"), sum, na.rm=T),
                                  pop = sum(pop, na.rm=T)) %>%
    mutate(across(starts_with("Genre_"), ~./pop*100)) %>%
    ggplot(aes(x = categ, y=Genre_F)) +
    geom_col() +
    geom_hline(yintercept = partF) +
    geom_label(aes(label = paste(round(Genre_F,1), "%"))) +
    coord_flip() +
    labs(title = "Part des femmes dans chaque catégorie") +
    xlab("Catégorie") + ylab("Part des femmes (%)") +
    theme_bw()
  
  g7= tableau %>% select(-PCS_insee) %>%
    mutate(pop = CoeffRecEnq) %>%
    pivot_wider(names_from = "PCS8", names_prefix = "PCS_", values_from = CoeffRecEnq) %>%
    group_by(categ) %>% summarize(across(starts_with("PCS_"), sum, na.rm=T),
                                  pop = sum(pop, na.rm=T)) %>%
    mutate(across(starts_with("PCS_"), ~./pop*100)) %>%
    pivot_longer(cols = starts_with("PCS_"), names_to = "PCS", values_to = "part") %>%
    ggplot(aes(x = categ, y=part)) +
    geom_col() +
    geom_text(aes(label = paste(round(part,1), "%")), nudge_y = 5) +
    coord_flip() +
    labs(title = "Part des PCS dans chaque catégorie") +
    xlab("Catégorie") + ylab("Part de la PCS (%)") +
    facet_wrap(~PCS) +
    theme_bw()
  
  g8= tableau %>% 
    mutate(pop = CoeffRecEnq) %>%
    filter(!is.na(typoModes) & !is.na(categ)) %>%
    pivot_wider(names_from = "typoModes", names_prefix = "typomodes_", values_from = CoeffRecEnq,
                names_repair = "unique") %>%
    group_by(categ) %>% summarize(across(starts_with("typomodes_"), sum, na.rm=T),
                                  pop = sum(pop, na.rm=T)) %>%
    mutate(across(starts_with("typomodes_"), ~./pop*100),
           partVoiture = typomodes_voiture + `typomodes_voiture+tc` + `typomodes_voiture+train`,
           partTC = `typomodes_voiture+tc` + typomodes_tc + `typomodes_voiture+train` + typomodes_train) %>%
    pivot_longer(cols = starts_with("typomodes_"), names_to = "typoModes", values_to = "part") %>%
    mutate(typoModes = substr(typoModes,11,nchar(typoModes)),
           typoModes = factor(typoModes, levels = levels(PER$typoModes)),
           categ = factor(categ, levels = rev(sort(unique(categ))))) %>%
    ggplot(aes(x = categ, y=part)) + geom_col(aes(fill = typoModes)) +
    labs(title = "Typologie des modes de transport utilisés\nselon le type de journée") +
    xlab("Catégorie journée") + ylab("Part de la population (%)") +
    scale_fill_manual(values = c(pal22_typoModes)) +
    geom_label(y=50, aes(label = paste0("Dont motorisé : ", round(partVoiture,1), " %"))) +
    geom_label(y=10, aes(label = paste0("Dont TC : ", round(partTC,1), " %"))) +
    coord_flip() + theme_bw()
  
  g9= tableau %>%
    filter(!is.na(categ)) %>%
    ggplot(aes(x = Dis/1000)) + geom_density(aes(color = categ)) +
    labs(title = "Répartition des journées par distance parcourue au sein de chaque catégorie") +
    xlab("distance (km)") + ylab("densité") + scale_color_hue(name = "catégorie") +
    coord_cartesian(xlim=c(0,250)) +
    theme_bw()
  
  print(g1)
  print(g2)
  print(g3)
  print(g4)
  print(g5)
  print(g5b)
  print(g6)
  print(g7)
  print(g8)
  print(g9)
  
  g10 = tableau %>%
    filter(!is.na(categ)) %>%
    group_by(categ, PCS8) %>% summarise(n = sum(CoeffRecEnq, na.rm=T)) %>%
    mutate(PCS8 = etqPCS8(PCS8)) %>%
    mutate(p = n / sum(n) * 100) %>%
    ggplot(aes(x = categ, y = p)) + geom_col(aes(fill = PCS8), position = "stack") +
    scale_fill_manual(values = pal_PCS8[2:6])
  
  print(g10)
  
  g11 = tableau %>%
    filter(!is.na(categ)) %>%
    group_by(categ, ZoneDens) %>% summarise(n = sum(CoeffRecEnq, na.rm=T)) %>%
    mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
    mutate(p = n / sum(n) * 100) %>%
    ggplot(aes(x = categ, y = p)) + geom_col(aes(fill = ZoneDens), position = "stack") +
    scale_fill_manual(values = pal_ZoneDens)
  
  print(g11)
  
  g12 = tableau %>%
    filter(!is.na(categ)) %>%
    group_by(categ, ZoneRang) %>% summarise(n = sum(CoeffRecEnq, na.rm=T)) %>%
    mutate(ZoneRang = etqZoneRang(ZoneRang)) %>%
    mutate(p = n / sum(n) * 100) %>%
    ggplot(aes(x = categ, y = p)) + geom_col(aes(fill = ZoneRang), position = "stack") +
    scale_fill_manual(values = pal_ZoneRang)
  
  print(g12)
}
