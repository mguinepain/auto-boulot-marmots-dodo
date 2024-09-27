# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                                                                   #
#   E N Q U Ê T E S   O R I G I N E - D E S T I N A T I O N   C E R E M A                           #
#   S C R I P T S   D E   T R A V A I L                                                             #
#   M A X I M E   G U I N E P A I N   M A I   2 0 2 1                                               #
#                                                                                                   #
#   R é a t t r i b u t i o n   d e s   L i b e l l e s   e t   F a c t e u r s                     #
#                                                                                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# Description de la situation de référence
ref = "(femme sans enfant, prof. interm.,\nvivant dans le centre d'une ville de plus de 200.00 hab.)"

# Liste de PCS du secteur privé, utilisée comme population de référence dans la thèse
PCS_privé = c("23", "36", "22", "46", "47", "48", "54", "55", "61", "66")

# Nomenclature =====

# Pour transcrire les uid_ENQ en intitulés lisibles :
z_Nomenclature = read.csv("Sources/Dictionnaires/Nomenclature.csv", sep=";", encoding = "utf8") %>%
  mutate(Libelle_Long = paste0(Libelle_Simple, " (", Annee, ")"))

# Étiquettes ========================================================================================

niv_Acti = c("Plein-temps", "Temps partiel", "Stage", "Scolarité", "Post-bac", "Sans emploi",
             "Retraite", "Foyer")

niv_PCSM = c("I. dom. cadres",
             "II. dom. cadres et prof. int.",
             "III. dom. interm. et employé.es",
             "IV. dom. indépendant.es",
             "V. dom. ouvrier.es et employé.es",
             "VI. monoactif ouvr./emp.",
             "VII. inactif")

niv_PCSMT = c("I. dom. cadres",
              "II. dom. cadres et prof. int.",
              "III-AB. dom. interm.",
              "III-C. couple d'employé.es",
              "IV. dom. indépendant.es",
              "V. dom. ouvrier.es et employé.es",
              "VI. monoactif ouvr./emp.",
              "VII. inactif étudiant",
              "VII. inactif autres")

niv_PCSMT_num = c("I", "II", "III", "IV", "V", "VI", "VII",
                 "III-AB", "III-C", "VII-Ét.", "VII-In.")

niv_PCSMT_lab = c("(dom. cadre)", "(dom. cadre/interm.)",
                  "(dom. interm./emp.)", "(dom. indép.)",
                  "(dom. ouvr./emp.)", "(monoactif ouvr./emp.)",
                  "(inactif)",
                  "(dom. interm.)", "(couple d'employé·es)",
                  "(inactif étudiant)", "(inactif hors ét.)")


niv_LogType = c("Maison individuelle",
                "Mitoyen",
                "Petit immeuble",
                "Grand immeuble (>5 étg.)",
                "Autre")

niv_LogOccSans22 = c("Propriétaire",
                     "Locataire",
                     "Locataire HLM",
                     "Logé.e gratuitement",
                     "Résid. Univ",
                     "Autre") # note : 22 exclus

niv_ZoneRang = c("Hors AAV", "AAV <50k hab.", "AAV 50~200k hab.",
                 "AAV 200~700k hab.", "AAV >700k hab.", "AAV Paris")

niv_ZonePosi = c("Commune-centre",
                 "Centre hors com. centre",
                 "Centre secondaire",
                 "Couronne",
                 "Hors d'une aire")

niv_ZoneDens = paste("Densité", c("forte", "interm.", "faible", "très faible"))

niv_MenCouple = c("P.Réf. célibataire",
                  "P.Réf. en couple")

niv_MenEnfants = c("Pas d'enfants",
                   "Enfants")

niv_MenTypo = c("Célibataire seul.e",
                "Parent isolé",
                "Célibataire et entourage",
                "Parent isolé et entourage",
                "Couple seul",
                "Parents et enfants",
                "Couple et entourage",
                "Parents, enfants et entourage",
                "Colocation",
                "Autre / N.D.")

niv_AgeMd.Q = c("16-19 ans",
                "20-29 ans",
                "30-39 ans",
                "40-49 ans",
                "50-59 ans",
                "60-69 ans",
                "70-69 ans",
                "80-89 ans",
                "Plus de 90 ans")

niv_EnfantsN = c("Pas d'enfants",
                 "1 enfant",
                 "2 enfants",
                 "3 enfants",
                 "4 enfants et plus")

niv_MenNivEtu = c("Pas au-delà du collège", "Niveau secondaire",
                  "Niveau Bac", "Niveau supérieur", "Études en cours")

niv_LogOcc = c("Propriétaire", "Locataire", "Locataire HLM", "Loc. non précisé", "Logé.e gratuitement",
               "Résid. Univ", "Autre")

niv_Genre = c("Femmes", "Hommes")

niv_NivEtu  = c("Pas d'études", "Études primaires", "Collège", "Lycée",
                 "Bac", "Supérieur", "En cours")

niv_NivEtu_Spl = c("Pas d'études", "Niveau primaire", "Niveau collège",
               "Niveau lycée", "Niveau lycée", "Niveau lycée", "Niveau bac",
               "Niveau sup.", "Niveau sup.", "Niveau sup.", "Ét. en cours")

niv_NivEtu_Det = c("Pas d'études", "Niveau primaire", "Niveau collège", "Niveau lycée",
                   "Et. apprentissage", "Niveau lycée*", "Niveau bac", "Appr. post-bac",
                   "Niveau L1/L2", "Niveau >Bac+2", "Ét. en cours")

niv_NivDip = c("Aucun diplôme", "Avant Bac", "Baccalauréat", "Supérieur (2 ans)", "Supérieur (L3 ou plus)",
               "Études en cours")

niv_Activ   = c("Emploi Plein-Temps", "Emploi Tps. Partiel", "Stage", "Scolarité", "Études sup.",
                "Sans emploi", "Retraité.e", "Au foyer")

niv_PCS8  = c("Agriculteur·rice",
              "Indépendant·e",
              "Cadre / Prof. Intel.",
              "Prof. Interm.",
              "Employé·e",
              "Ouvrier·e",
              "En études",
              "Inactif·ve",
              "PCS inconnue", "PCS inconnue")

niv_PCS8_H  = c("Agriculteur",
              "Indépendant",
              "Cadre / Prof. Intel.",
              "Prof. Interm.",
              "Employé",
              "Ouvrier",
              "En études",
              "Inactif",
              "PCS inconnue", "PCS inconnue")

niv_PCS8_F  = c("Agricultrice",
              "Indépendante",
              "Cadre / Prof. Intel.",
              "Prof. Interm.",
              "Employée",
              "Ouvrière",
              "En études",
              "Inactive",
              "PCS inconnue", "PCS inconnue")

niv_PCS42S = c(NA, "Agriculteur·rice",
               "Artisan·e", "Commerçant·e", "Chef·fe d'entreprise",
               "Prof. Libérale", "Cadre F.Pub., Ens./Rech., Arts", "Cadre d'entreprise",
               "Prof. Interm. F.Pub ou rel.", "Prof. Interm. Entreprise", "Technicien·ne",
               "Agent·e de maîtrise",
               "Employé·e du public/sécurité", "Employé·e de bureau", "Employé·e de commerce",
               "Employé·e aide à la personne", "Ouvrier·e qualifié·e", "Ouvrier·e non qualifié·e",
               "Ouvrier·e agricole",
               "Chômeur·se n'ayant jamais travaillé", "Inactif·ve", "Écolier·e", "Collégien·ne",
               "Lycéen·ne", "Lycéen·ne", "Étudiant·e licence", "Étudiant·e M/D", "Apprenti·e",
               "Lycéen·ne")

niv_PCS42S_H = c(NA, "Agriculteur",
               "Artisan", "Commerçant", "Chef d'entreprise",
               "Prof. Libérale", "Cadre F.Pub., Ens./Rech., Arts", "Cadre d'entreprise",
               "Prof. Interm. F.Pub ou rel.", "Prof. Interm. Entreprise", "Technicien",
               "Agent de maîtrise",
               "Employé du public/sécurité", "Employé de bureau", "Employé de commerce",
               "Employé aide à la personne", "Ouvrier qualifié", "Ouvrier non qualifié",
               "Ouvrier agricole",
               "Chômeur n'ayant jamais travaillé", "Inactif", "Écolier", "Collégien",
               "Lycéen", "Lycéen", "Étudiant licence", "Étudiant M/D", "Apprenti",
               "Lycéen")

niv_PCS42S_F = c(NA, "Agricultrice",
               "Artisane", "Commerçante", "Cheffe d'entreprise",
               "Prof. Libérale", "Cadre F.Pub., Ens./Rech., Arts", "Cadre d'entreprise",
               "Prof. Interm. F.Pub ou rel.", "Prof. Interm. Entreprise", "Technicienne",
               "Agente de maîtrise",
               "Employée du public/sécurité", "Employée de bureau", "Employée de commerce",
               "Employée aide à la personne", "Ouvrière qualifiée", "Ouvrière non qualifiée",
               "Ouvrière agricole",
               "Chômeuse n'ayant jamais travaillé", "Inactive", "Écolière", "Collégienne",
               "Lycéenne", "Lycéenne", "Étudiante licence", "Étudiante M/D", "Apprentie",
               "Lycéenne")

niv_Lien = c("Pers. de Réf.", "Conj. Réf.", "Enfant Réf.", "Colocataire", "Famille Réf.", "Hors famille Réf.", "Inconnu")

niv_ds = c("< 200", "200 à 400", "400 à 800",
          "800 à 1600", "1600 à 3200", "3200 à 6400", "6400 à 12800",
          "12800 à 25000", "> 25000")

niv_fqc_Vco = c("Conduit voit. au quotidien",
                "Conduit voit. 1× par semaine",
                "Conduit voit. 1× par mois",
                "Ne conduit pas de voiture")

niv_fqc_Vpa = c("Passager⋅e voiture au quotidien",
                "Passager⋅e voiture 1× par semaine",
                "Passager⋅e voiture 1× par mois",
                "Jamais ou rarement passager⋅e voit.")

niv_fqc_Drm = c("Conduit un DRM au quotidien",
                "Conduit un DRM 1× par semaine",
                "Conduit un DRM 1× par mois",
                "Ne conduit pas de DRM")

niv_fqc_Tco = c("Utilise les T.C. au quotidien",
                "Utilise les T.C. 1× par semaine",
                "Utilise les T.C. 1× par mois",
                "N'utilise pas les T.C.")

niv_fqc_Vel = c("Fait du vélo au quotidien",
                "Fait du vélo 1× par semaine",
                "Fait du vélo 1× par mois",
                "Ne fait pas de vélo")

niv_fqc_Mch = c("Marche au quotidien",
                "Marche 1× par semaine",
                "Marche 1× par mois",
                "Ne se déplace pas à pied")

etq_Var = list(
    "Genre"    = c("Genre", "le genre"),
    "Lien"     = c("Statut ds. ménage", "le statut dans le ménage"),
    "Age"      = c("Âge", "l'âge"),
    "Age5"     = c("Classe d'âge", "la classe d'âge"),
    "NivEtu"   = c("Niv. diplôme", "le niveau du dernier diplôme obtenu"),
    "Activ"    = c("Activité", "le type d'activité"),
    "Activ2"   = c("Activité secondaire", "l'activité secondaire"),
    "ActivCpl" = c("Activ. mb. du couple", "l'activité des 2 membres du couple"),
    "PCS8"     = c("PCS", "la PCS individuelle"),
    "PCS42S"    = c("PCS", "la PCS individuelle"),
    "PCSM"     = c("PCS Ménage", "la PCS Ménage"),
    "PCSML"    = c("PCS Ménage estimée", "la PCS Ménage estimée"),
    "PCSMT"    = c("PCS Ménage", "la PCS Ménage"),
    "ZoneRang" = c("Taille d'agglom.", "la taille de l'AAV de résidence"),
    "ZonePosi" = c("Empl. dans l'agglom.", "l'emplacement de la com. de résidence dans l'AAV"),
    "ZoneDens" = c("Densité communale", "la densité de pop. dans la commune de résidence"),
    "plusLieuxTravail" = c("L. travail", "le nombre de lieux de travail")
)

etq = function(variable, longue = F)
{
    i = ifelse(longue, 2, 1)
    return(etq_Var[[variable]][i])
}

mode_MAR = c("01", "93", "94", "96", "97") # les fauteuils roulants sont agrégés à la catég marche
mode_VEL = c("10", "11", "12", "17", "18", "10x", "11x")
mode_DRM = c("13", "14", "15", "16", "19", "20")
mode_VOI = c("21", "22", "61", "62", "81", "82")
mode_TCO = c("31", "32", "33", "34", "37", "38", "39", "41", "42", "43", "51", "52", "53", "54", "71")

mode_BUS = c("31", "32", "33", "34", "37", "38", "39", "41", "42", "43")
mode_TRN = c("51", "52", "53", "54")

motif_Dom = c("01", "02")
motif_Tvl = c("11", "12", "13", "14", "81")
motif_Etu = c("21", "22", "23", "24", "96", "25", "26", "27", "28", "29", "97")
motif_Com = c("30", "31", "32", "33", "34", "35", "98", "82")
motif_San = c("41")
motif_Svc = c("42", "43")
motif_Lsr = c("51", "52", "53")
motif_Vst = c("54")
motif_Acc = c("61", "62", "63", "64", "71", "72", "73", "74", "68", "69")


classesDensites = function(x)
{
  y = x
  y = ifelse(x <= 200, "< 200",  y)
  y = ifelse(x <= 400 & x > 200, "200 à 400",  y)
  y = ifelse(x <= 800 & x > 400, "400 à 800",  y)
  y = ifelse(x <=1600 & x > 800, "800 à 1600", y)
  y = ifelse(x <=3200 & x >1600, "1600 à 3200",y)
  y = ifelse(x <=6400 & x >3200, "3200 à 6400",y)
  y = ifelse(x <=12800 & x >6400,"6400 à 12800",y)
  y = ifelse(x <=25000 & x >12800,"12800 à 25000",y)
  y = ifelse(x > 25000,          "> 25000", y)
  
  y = factor(y,
             c("< 200", "200 à 400", "400 à 800",
               "800 à 1600", "1600 à 3200", "3200 à 6400", "6400 à 12800",
               "12800 à 25000", "> 25000"))
  
  return(y)
}

# Palettes ==========================================================================================

# Nota : ces palettes sont conçues pour viz_Barres, qui génère automatiquement un dégradé quand on lui
#        fournit 2 couleurs pour un nombre plus grand d'entités de couleurs différentes à dessiner

pal_Motifs = c("royalblue",
               "turquoise",
               "plum",
               "olivedrab3",
               "darkolivegreen2",
               "orange",
               "orangered",
               "firebrick")

palMotifs = function(codeMotif)
{
    codeMotif = substr(codeMotif, 1,1)
    codeMotif = plyr::revalue(codeMotif, c("0" = "royalblue",
                                           "1" = "turquoise",
                                           "2" = "plum",
                                           "3" = "olivedrab3",
                                           "4" = "darkolivegreen2",
                                           "5" = "orange",
                                           "6" = "orangered",
                                           "7" = "firebrick",
                                           "9" = "grey"), warn_missing = F)
    return(codeMotif)
}


pal_PCSM = c("firebrick3",
             "darkorange3",
             "yellowgreen",
             "darkslategray3",
             "royalblue1",
             "slateblue",
             "plum")

# pal_PCSMT = c("firebrick3",
#               "darkorange3",
#               "yellowgreen",
#               "palegreen2",
#               "darkslategray3",
#               "royalblue1",
#               "slateblue",
#               "plum3",
#               "plum4")

pal_PCSMT = c("firebrick3",
              "darkorange3",
              "yellowgreen",
              "greenyellow",
              "palegreen2",
              "royalblue1",
              "plum",
              "violet",
              "violetred2")

pal_LogType = c("#fad572",
                "#fff4ba",
                "#daf8f5",
                "#b6daec",
                "#f2f3f4")

pal_LogOcc      = c("#E55451",
                    "#8EEBEC",
                    "#77BFC7",
                    "#3B9C9C",
                    "#B5EAAA",
                    "plum",
                    "grey70")

pal_LogOccSans22 = pal_LogOcc[c(1:4,6,7)]

pal_ZoneRang = c("palegreen", "lightskyblue1", "lightskyblue2", "skyblue2", "dodgerblue2", "royalblue2")
pal_ZonePosi = c("skyblue4", "skyblue3", "skyblue", "turquoise", "palegreen2")
pal_ZoneDens = c("slategray", "slategray2",  "wheat", "tan")

pal_MenCouple = c("tomato", "lightseagreen")
pal_MenEnfants = c("tomato", "lightseagreen")
pal_MenTypo = c("darkseagreen1", "darkseagreen2", "darkseagreen3", "darkseagreen4",
                "thistle1", "thistle2", "thistle3", "thistle4",
                "indianred1", "grey90")

pal_MenAgeMd = c("goldenrod1", "orangered3")
pal_EnfantsN = c("plum1", "orchid4")
pal_TypoCharge = c("firebrick2", "goldenrod1", "lightgoldenrod1", "darkseagreen1", "palegreen3")
pal_MenNivEtu = c("lightsteelblue1", "slateblue3")
pal_uid_ENQ = c("darkorange")

pal_NivEtu   = c("navajowhite2", "darkolivegreen3", "darkseagreen3",
                 "cadetblue1", "lightsteelblue3",
                 "steelblue3", "darksalmon")
pal_Activ   = c("royalblue3", "steelblue2", "slategray2", "aquamarine2", "aquamarine3",
                "seashell3", "lightsalmon1", "brown1")
pal_PCS8  = c("darkolivegreen3", "turquoise", "salmon", "mediumpurple2", "cornflowerblue",
              "orangered3", "goldenrod1", "goldenrod3", "darkorange4")
pal_Lien = c("brown3", "plum3", "seagreen3", "turquoise", "lightgoldenrod2", "lightsalmon2", "tan")

pal_PCS8_v  = c("darkolivegreen3", "darkturquoise", "salmon", "mediumpurple2", "cornflowerblue",
              "orangered3", "goldenrod1", "goldenrod3", "darkorange4")

pal_PCS8  = c("darkolivegreen3",
              "goldenrod",
              "steelblue",
              "darkorchid",
              "rosybrown",
              "firebrick3",
              "grey60", "grey", "grey40")

pal_PCS42S_d  = c("grey80",
                "darkolivegreen3",
                "gold", "goldenrod", "peru",
                "steelblue1", "steelblue", "steelblue4",
                "orchid1", "orchid2", "orchid3", "orchid4",
                "lightcyan", "lightblue", "paleturquoise", "turquoise",
                "indianred","salmon","chocolate")

pal_PCS42S = c(pal_PCS8[1],
               rep(pal_PCS8[2], 3),
               rep(pal_PCS8[3], 3),
               rep(pal_PCS8[4], 4),
               rep(pal_PCS8[5], 4),
               rep(pal_PCS8[6], 3))

pal22_typoModes = c("#f9ff85", "#b7eb7e", "#ffd193", "#f9907b", "#f4769c", "#eab9f4", "#aab4f5", "plum")

palListe = list(
    "Motif"     = pal_Motifs,
    "PCSM"      = pal_PCSM,
    "PCSML"     = pal_PCSM,
    "PCSMT"     = pal_PCSMT,
    "LogType"   = pal_LogType,
    "LogOcc"    = pal_LogOcc,
    "ZoneRang"  = pal_ZoneRang,
    "ZonePosi"  = pal_ZonePosi,
    "ZoneDens"  = pal_ZoneDens,
    "MenCouple" = pal_MenCouple,
    "MenEnfants"= pal_MenEnfants,
    "MenTypo"   = pal_MenTypo,
    "NivEtu"    = pal_NivEtu,
    "Activ"     = pal_Activ,
    "PCS8"      = pal_PCS8,
    "Lien"      = pal_Lien,
    "ActivSScol"= c("royalblue3", "steelblue2", "slategray2", "aquamarine3", "seashell3", "lightsalmon1", "brown1"),
    "Age"       = "lightgoldenrod",
    "Age5"      = "lightgoldenrod",
    "ActivCpl"  = "aquamarine"
)

palModes = function(champ)
{
    champ = plyr::revalue(champ, c("marche" = "tan",
                                   "voiture" = "brown",
                                   "vélo" = "lightgreen",
                                   "vélo élec." = "lightseagreen",
                                   "deux roues" = "purple",
                                   "voiture (pass.)" = "brown1",
                                   "bus urbain" = "deepskyblue",
                                   "tramway" = "deepskyblue3",
                                   "métro" = "deepskyblue4",
                                   "t. c. non précisé" = "deepskyblue",
                                   "transp. à la demande" = "coral",
                                   "car interurbain" = "slateblue1",
                                   "car longue distance" = "slateblue3",
                                   "TGV" = "magenta",
                                   "TER" = "hotpink",
                                   "train" = "palevioletred1",
                                   "train" = "palevioletred1",
                                   "taxi" = "coral",
                                   "VTC" = "coral2",
                                   "car employeur" = "yellow4",
                                   "utilitaire" = "tomato2",
                                   "utilitaire (pass.)" = "tomato1",
                                   "bateau" = "cyan",
                                   "avion" = "cornflowerblue",
                                   "roller, skate, trot." = "khaki",
                                   "fauteuil roulant" = "yellowgreen",
                                   "mode non précisé" = "gray",
                                   "petit engin électrique" = "gold",
                                   "01" = "tan",
                                   "10" = "brown",
                                   "11" = "lightgreen",
                                   "12" = "lightgreen",
                                   "17" = "lightseagreen",
                                   "18" = "lightseagreen",
                                   "13" = "purple",
                                   "14" = "purple",
                                   "15" = "purple",
                                   "16" = "purple",
                                   "17" = "purple",
                                   "19" = "purple",
                                   "20" = "purple",
                                   "21" = "brown",
                                   "22" = "brown1",
                                   "31" = "deepskyblue",
                                   "32" = "deepskyblue3",
                                   "33" = "deepskyblue4",
                                   "34" = "deepskyblue",
                                   "37" = "coral",
                                   "38" = "gray",
                                   "39" = "gray",
                                   "41" = "slateblue1",
                                   "42" = "slateblue3",
                                   "43" = "slateblue1",
                                   "51" = "magenta",
                                   "52" = "hotpink",
                                   "53" = "palevioletred1",
                                   "54" = "palevioletred1",
                                   "61" = "coral",
                                   "62" = "coral2",
                                   "71" = "yellow4",
                                   "81" = "tomato2",
                                   "82" = "tomato2",
                                   "91" = "cyan",
                                   "92" = "cornflowerblue",
                                   "93" = "khaki",
                                   "94" = "yellowgreen",
                                   "95" = "gray",
                                   "96" = "gold",
                                   "97" = "khaki"), warn_missing = F)
    return(champ)
}


# Fonctions de réétiquetage =========================================================================


etqActiv = function(champ, groupes=F, num=F, prefixe=F)
{
    numeros = as.character(c("10", "11", "12", "21", "22", "31", "32", "33"))
    etiqs   = niv_Acti
    
    if (groupes) {
        etiqs[1:3] = paste("[Travail]", etiqs[1:3])
        etiqs[4:5] = paste("[Études]",  etiqs[4:5])
        etiqs[6:8] = paste("[Inac.]",   etiqs[6:8])
    }
    
    if(prefixe){ numeros = paste0("Activ", numeros) }
    
    if (!num)
    {
        liste = set_names(nm = numeros, x = etiqs)
        
        champ = plyr::revalue(champ, liste, warn_missing = F)
        return(champ)
    }
    
    champ = factor(champ, levels=numeros,
                   labels = paste(numeros, etiqs))
    return(champ)
}

etqPCS8 = function(champ, num=F, low=F, prefixe = F, rev=F, genre="both")
{
    numeros = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "00")
    
    if (genre[1] == "both") { etiqs = niv_PCS8 }
    if (genre[1] == "H") { etiqs = niv_PCS8_H }
    if (genre[1] == "F") { etiqs = niv_PCS8_F }
    
    if(prefixe){ numeros = paste0("PCS8", numeros) }
    
    if (num) { etiqs[1:7] = paste(numeros[1:7], etiqs[1:7]) }
    if (low) { etiqs = tolower(etiqs) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    if(!prefixe){
        if (!rev) { champ = factor(x = champ, levels = unique(etiqs)) }
        if (rev)  { champ = factor(x = champ, levels = rev(unique(etiqs))) }
    }
    
    return(champ)
}

etqPCS42S = function(champ, num=F, prefixe=F, rev=F, genre = "both")
{
    numeros = c("00", "10",
                "21", "22", "23",
                "31", "32", "36",
                "41", "46", "47", "48",
                "51", "54", "55", "56",
                "61", "66", "69",
                "81", "82", "83", "84", "85", "86", "87", "88", "89", "90")

    if (genre[1] == "both") { etiqs = niv_PCS42S }
    if (genre[1] == "H") { etiqs = niv_PCS42S_H }
    if (genre[1] == "F") { etiqs = niv_PCS42S_F }
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("PCS42S", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    if (!prefixe) {
        if (!rev) { champ = factor(x = champ, levels = unique(etiqs)) }
        if (rev)  { champ = factor(x = champ, levels = rev(unique(etiqs))) }
    }

    return(champ)
}

etqZoneDens = function(champ, supprTrFaible = F, num=F, prefixe=F)
{
    numeros = as.character(c(1:4))
    etiqs   = niv_ZoneDens
    
    if (supprTrFaible) { champ = plyr::revalue(champ, c("4" = "3")) ; etiqs[4] = etiqs[3] }
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numerosPf = c(paste0("ZoneDens", numeros)) } else { numerosPf = numeros }
    
    liste = set_names(nm = numerosPf, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    if(prefixe) #2e essai pour ZT_travMax
    {
        numerosPf = c(paste0("ZoneDens_travMax", numeros))
        liste = set_names(nm = numerosPf, x = paste0(etiqs, " (travail)"))
        champ = plyr::revalue(champ, liste, warn_missing = F)
    }
    
    return(champ)
}



etqZoneRang = function(champ, num=F, prefixe=F)
{
    numeros = as.character(c(0:5))
    etiqs   = niv_ZoneRang
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("ZoneRang", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqZonePosi = function(champ, num=F, prefixe=F, court = F)
{
    numeros = as.character(c("11", "12", "13", "20", "30"))
    etiqs   = niv_ZonePosi
    
    if(court) { etiqs = gsub(" ", "\n", etiqs)
                etiqs = gsub("-", "-\n", etiqs)}
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("ZonePosi", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    if (!prefixe) {
        champ = factor(x = champ, levels = unique(etiqs))
    }
    
    return(champ)
}

NivEtuVersNivDip = function(champ)
{
    champ = plyr::revalue(champ, c("00" = "0",
                                   "10" = "0",
                                   "20" = "0",
                                   "30" = "1",
                                   "31" = "1",
                                   "32" = "1",
                                   "33" = "1",
                                   "40" = "2",
                                   "41" = "2",
                                   "45" = "3",
                                   "50" = "4",
                                   "90" = "9"))
}

etqNivEtu = function(champ, spl=T, num=F, prefixe=F)
{
    numeros = c("00", "10", "20", "30", "31", "32", "40", "41", "45", "50", "90")
    
    if (!spl)
    {
        etiqs   = niv_NivEtu_Det
    }
    if (spl)
    {
        etiqs = niv_NivEtu_Spl
    }
    
    if (num & !spl) { etiqs = paste0("[", numeros, "] ", etiqs) }
    if (num &  spl) { etiqs = paste0("[", substr(numeros, 1,1), "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("NivEtu", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqNivEtuSpl = function(champ, num=F, prefixe=F)
{
    numeros = c("0", "1", "2", "3", "4", "5", "9")
    
    etiqs = niv_NivEtu_Spl[c(1,2,3,4,7,10,11)]
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("NivEtu", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqNivDip = function(champ, num=F, prefixe=F)
{
    numeros = c("0", "1", "2", "3", "4", "9")
    
    etiqs = niv_NivDip
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("NivDip", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqAge = function(champ, pas = 5, min = 6, max = 85, réordo = T, forceMin = F)
{
    
    # S'il y a des valeurs hors des bornes, on les remplace par les bornes le temps du calcul,
    # puis on renomme les bornes avec les extrêmes de la distribution pour en tenir compte dans la légende
    # Permet de rassembler les valeurs extrêmes dans le groupe le plus jeune et surtout le plus âgé
    maxréel = NULL ; minréel = NULL
    if (!is.null(max)){
        if (max(champ, na.rm=T) > max) { 
            maxréel = max(champ, na.rm=T)
            champ = ifelse(champ > max, max, champ) }}
    if (!is.null(min)){
        if (min(champ, na.rm=T) < min) { 
            minréel = min(champ, na.rm=T)
            champ = ifelse(champ < min, min, champ) }}
    
    if (!is.numeric(champ)) { stop("[Réattr. Facteurs] Le champ Âge doit être un nombre !") }
    if (!is.numeric(pas))   { stop("[Réattr. Facteurs] Entrer un nombre pour sélectionner un pas de simplification de l'âge") }
    
    champ = trunc(champ/pas) * pas
    agebas  = as.character(champ)
    agehaut = as.character(champ + (pas - 1))
    
    # Si un groupe s'étend au-delà des bornes (ex: 15-19 ans alors que le min est de 16), il est renommé
    if (!is.null(min)) { agebas = ifelse (as.numeric(agebas)  < min, as.numeric(min), agebas) }
    if (!is.null(max)) { agehaut= ifelse (as.numeric(agehaut) > max, as.numeric(max), agehaut)}
    
    # On renomme les classes auxquelles les extrêmes ont été agrégés plus haut
    if (!is.null(min) & !is.null(minréel) & !forceMin) { agebas = ifelse (as.numeric(agebas ) ==min, minréel, agebas)}
    if (!is.null(max) & !is.null(maxréel)) { agehaut= ifelse (as.numeric(agehaut) ==max, maxréel, agehaut)}
    
    if (réordo == T)
    {
        delta = nchar(as.character(max(as.numeric(agebas), na.rm=T))) -
            nchar(as.character(min(as.numeric(agebas), na.rm=T)))
        
        if (delta > 0)
        {
            agebas = formatC(agebas, width=nchar(as.character(max(as.numeric(agebas), na.rm=T))), flag="0")
        }
    }
    
    champ = paste0(agebas, " à ", agehaut, " ans")
    champ = as.factor(champ)
    
    return(champ)
}


etqLien = function(champ)
{
    champ = plyr::revalue(champ, c("1" = niv_Lien[1],
                                   "2" = niv_Lien[2],
                                   "3" = niv_Lien[3],
                                   "4" = niv_Lien[4],
                                   "5" = niv_Lien[5],
                                   "6" = niv_Lien[6],
                                   "7" = niv_Lien[7]), warn_missing = F)
    return(champ)
}


etqLien = function(champ)
{
    champ = plyr::revalue(champ, c("1" = niv_Lien[1],
                                   "2" = niv_Lien[2],
                                   "3" = niv_Lien[3],
                                   "4" = niv_Lien[4],
                                   "5" = niv_Lien[5],
                                   "6" = niv_Lien[6],
                                   "7" = niv_Lien[7]), warn_missing = F)
    return(champ)
}

etqPCSM = function(champ, dét = F, prefixe=F, retourChariot=T)
{
    numeros = c("1", "2", "3", "4", "5", "6", "7",
                "3AB", "3C", "7e", "7i")

    etiqs   = niv_PCSMT_num

    separateur = ifelse(retourChariot, "\n", " ")
    
    if(dét) {
        etiqs = paste0(etiqs, separateur,
                       niv_PCSMT_lab)
    }

    if(prefixe){ numeros = c(paste0("PCSMT", numeros), paste0("PCSMLT", numeros))
                 etiqs = rep(paste0("PCS M. ", etiqs), times = 2) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqMenTypo = function(champ)
{
    champ = plyr::revalue(champ, c("10" = "célib. seul",
                                   "11" = "célib. parent",
                                   "12" = "célib. entouré",
                                   "13" = "célib. parent entouré",
                                   "20" = "couple seul",
                                   "21" = "couple parents",
                                   "22" = "couple entouré",
                                   "23" = "couple parents entouré",
                                   "30" = "coloc",
                                   "99" = NA), warn_missing = F)
    
    return(champ)
}

etqFqc = function(champ, num=F, prefixe=F)
{
    if (!num) {
    champ = plyr::revalue(champ, c("1" = "Quotid.",
                                   "2" = "Mensuel",
                                   "3" = "Parfois",
                                   "4" = "Jamais"), warn_missing=F)
    if (!prefixe) {champ = factor(champ, levels = c("Quotid.", "Mensuel", "Parfois", "Jamais")) }
    }
    if (num) {
        champ = plyr::revalue(champ, c("1" = "1- Quotid.",
                                       "2" = "2- Mensuel",
                                       "3" = "3- Parfois",
                                       "4" = "4- Jamais"), warn_missing=F)
    }
    champ = as.factor(champ)
    return(champ)
}


etqMode = function(champ, simple = F)
{
  if (!simple)
  {
    champ = plyr::revalue(champ, c("01" = "marche",
                                   "10" = "voiture",
                                   "11" = "vélo",
                                   "12" = "vélo",
                                   "17" = "vélo élec.",
                                   "18" = "vélo élec.",
                                   "13" = "deux roues",
                                   "14" = "deux roues",
                                   "15" = "deux roues",
                                   "16" = "deux roues",
                                   "17" = "deux roues",
                                   "19" = "deux roues",
                                   "20" = "deux roues",
                                   "21" = "voiture",
                                   "22" = "voiture (pass.)",
                                   "31" = "bus urbain",
                                   "32" = "tramway",
                                   "33" = "métro",
                                   "34" = "t. c. non précisé",
                                   "37" = "transp. à la demande",
                                   "38" = "t. c. non précisé",
                                   "39" = "t. c. non précisé",
                                   "41" = "car interurbain",
                                   "42" = "car longue distance",
                                   "43" = "car interurbain",
                                   "51" = "TGV",
                                   "52" = "TER",
                                   "53" = "train",
                                   "54" = "train",
                                   "61" = "taxi",
                                   "62" = "VTC",
                                   "71" = "car employeur",
                                   "81" = "utilitaire",
                                   "82" = "utilitaire (pass.)",
                                   "91" = "bateau",
                                   "92" = "avion",
                                   "93" = "roller, skate, trot.",
                                   "94" = "fauteuil roulant",
                                   "95" = "mode non précisé",
                                   "96" = "petit engin électrique",
                                   "97" = "mode non précisé"), warn_missing = F)
  }
  if (simple)
  {
    champ = plyr::revalue(champ, c("01" = "marche",
                                   "10" = "vélo",
                                   "11" = "vélo",
                                   "12" = "vélo",
                                   "17" = "vélo",
                                   "18" = "vélo",
                                   "13" = "deux roues",
                                   "14" = "deux roues",
                                   "15" = "deux roues",
                                   "16" = "deux roues",
                                   "17" = "deux roues",
                                   "19" = "deux roues",
                                   "20" = "deux roues",
                                   "21" = "voiture",
                                   "22" = "voiture",
                                   "31" = "TC local",
                                   "32" = "TC local",
                                   "33" = "TC local",
                                   "34" = "TC local",
                                   "37" = "TC local",
                                   "38" = "TC local",
                                   "39" = "TC local",
                                   "41" = "car",
                                   "42" = "car",
                                   "43" = "car",
                                   "51" = "train",
                                   "52" = "train",
                                   "53" = "train",
                                   "54" = "train",
                                   "61" = "taxi",
                                   "62" = "taxi",
                                   "71" = "TC local",
                                   "81" = "voiture",
                                   "82" = "voiture",
                                   "91" = "autres",
                                   "92" = "autres",
                                   "93" = "marche",
                                   "94" = "marche",
                                   "95" = "[NA]",
                                   "96" = "marche",
                                   "97" = "[NA]",
                                   "11x" = "vélo",
                                   "10x" = "vélo",
                                   "63" = "[NA]",
                                   "9999" = "[NA]"), warn_missing = F)
    champ = factor(champ, levels=c("marche", "vélo", "deux roues", "voiture",
                                   "taxi", "TC local", "car", "train", "TGV",
                                   "autres", "NA"))
  }
    
    return(champ)
}


etqPro = function(Genre, PCS, Activ, specGenre=T)
{
    
    Genre = as.character(Genre)
    PCS   = as.character(PCS)
    Activ = as.character(Activ)
    
    tab = cbind(Genre, PCS, Activ) %>% as.data.frame()
    
    if (specGenre) {
        tab = mutate(tab, Pro = case_when(Genre == "H" & PCS == "00" ~ "homme, prof. inconnue",
                                          Genre == "H" & PCS == "01" ~ "agriculteur",
                                          Genre == "H" & PCS == "02" ~ "indépendant",
                                          Genre == "H" & PCS == "03" ~ "homme, cadre ou prof. sup.",
                                          Genre == "H" & PCS == "04" ~ "homme, prof. interm.",
                                          Genre == "H" & PCS == "05" ~ "employé",
                                          Genre == "H" & PCS == "06" ~ "ouvrier",
                                          Genre == "H" & PCS == "07" & Activ == "21" ~ "élève (garçon)",
                                          Genre == "H" & PCS == "07" & Activ == "22" ~ "étudiant",
                                          Genre == "H" & PCS == "08" ~ "chômeur n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "09" ~ "inactif n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "10" ~ "agriculteur",
                                          Genre == "H" & PCS == "21" ~ "artisan",
                                          Genre == "H" & PCS == "22" ~ "commerçant",
                                          Genre == "H" & PCS == "23" ~ "chef d'entreprise",
                                          Genre == "H" & PCS == "31" ~ "homme, prof. libérale",
                                          Genre == "H" & PCS == "32" ~ "enseignant ou fonction publique cat. A",
                                          Genre == "H" & PCS == "36" ~ "homme, cadre d'entreprise",
                                          Genre == "H" & PCS == "41" ~ "homme, prof. école, infirmier ou fn publique interm.",
                                          Genre == "H" & PCS == "46" ~ "salarié du privé, administration ou commerce",
                                          Genre == "H" & PCS == "47" ~ "technicien",
                                          Genre == "H" & PCS == "48" ~ "agent de maîtrise",
                                          Genre == "H" & PCS == "51" ~ "employé de la fonction publique ou de sécurité",
                                          Genre == "H" & PCS == "54" ~ "employé de bureau",
                                          Genre == "H" & PCS == "55" ~ "employé de commerce",
                                          Genre == "H" & PCS == "56" ~ "employé de ménage ou domestique",
                                          Genre == "H" & PCS == "61" ~ "ouvrier qualifié ou chauffeur",
                                          Genre == "H" & PCS == "66" ~ "ouvrier non qualifié",
                                          Genre == "H" & PCS == "69" ~ "ouvrier agricole",
                                          Genre == "H" & PCS == "81" ~ "chômeur n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "82" & Activ != "33" ~ "homme sans activité officielle",
                                          Genre == "H" & PCS == "82" & Activ == "33" ~ "homme au foyer",
                                          Genre == "H" & PCS == "83" ~ "écolier",
                                          Genre == "H" & PCS == "84" ~ "collégien",
                                          Genre == "H" & PCS == "85" ~ "lycéen",
                                          Genre == "H" & PCS == "86" ~ "homme, élève de BTS",
                                          Genre == "H" & PCS == "90" ~ "homme, élève du secondaire",
                                          Genre == "H" & PCS == "87" ~ "étudiant de licence",
                                          Genre == "H" & PCS == "80" ~ "étudiant de licence ou BTS",
                                          Genre == "H" & PCS == "88" ~ "étudiant (Bac+3 ou plus)",
                                          Genre == "H" & PCS == "89" ~ "apprenti",
                                          Genre == "H" & PCS == "00" ~ "homme, prof. inconnue",
                                          Genre == "F" & PCS == "01" ~ "agricultrice",
                                          Genre == "F" & PCS == "02" ~ "indépendante",
                                          Genre == "F" & PCS == "03" ~ "femme, cadre ou prof. sup.",
                                          Genre == "F" & PCS == "04" ~ "femme, prof. interm.",
                                          Genre == "F" & PCS == "05" ~ "employée",
                                          Genre == "F" & PCS == "06" ~ "ouvrière",
                                          Genre == "F" & PCS == "07" & Activ == "21" ~ "élève (fille)",
                                          Genre == "F" & PCS == "07" & Activ == "22" ~ "étudiante",
                                          Genre == "F" & PCS == "08" ~ "chômeuse n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "09" ~ "inactive n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "10" ~ "agricultrice",
                                          Genre == "F" & PCS == "21" ~ "artisante",
                                          Genre == "F" & PCS == "22" ~ "commerçante",
                                          Genre == "F" & PCS == "23" ~ "cheffe d'entreprise",
                                          Genre == "F" & PCS == "31" ~ "femme, prof. libérale",
                                          Genre == "F" & PCS == "32" ~ "enseignante ou fonction publique cat. A",
                                          Genre == "F" & PCS == "36" ~ "femme, cadre d'entreprise",
                                          Genre == "F" & PCS == "41" ~ "femme, prof. école, infirmière ou fn publique interm.",
                                          Genre == "F" & PCS == "46" ~ "salariée du privé, administration ou commerce",
                                          Genre == "F" & PCS == "47" ~ "technicienne",
                                          Genre == "F" & PCS == "48" ~ "agente de maîtrise",
                                          Genre == "F" & PCS == "51" ~ "employée de la fonction publique ou de sécurité",
                                          Genre == "F" & PCS == "54" ~ "employée de bureau",
                                          Genre == "F" & PCS == "55" ~ "employée de commerce",
                                          Genre == "F" & PCS == "56" ~ "employée de ménage ou domestique",
                                          Genre == "F" & PCS == "61" ~ "ouvrière qualifiée ou chauffeuse",
                                          Genre == "F" & PCS == "66" ~ "ouvrière non qualifiée",
                                          Genre == "F" & PCS == "69" ~ "ouvrière agricole",
                                          Genre == "F" & PCS == "81" ~ "chômeuse n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "82" & Activ != "33" ~ "femme sans activité officielle",
                                          Genre == "F" & PCS == "82" & Activ == "33" ~ "femme au foyer",
                                          Genre == "F" & PCS == "83" ~ "écolière",
                                          Genre == "F" & PCS == "84" ~ "collégienne",
                                          Genre == "F" & PCS == "85" ~ "lycéenne",
                                          Genre == "F" & PCS == "86" ~ "femme, élève de BTS",
                                          Genre == "F" & PCS == "90" ~ "femme, élève du secondaire",
                                          Genre == "F" & PCS == "87" ~ "étudiante de licence",
                                          Genre == "F" & PCS == "80" ~ "étudiante de licence ou BTS",
                                          Genre == "F" & PCS == "88" ~ "étudiante (Bac+3 et plus)",
                                          Genre == "F" & PCS == "89" ~ "apprentie",
                                          Genre == "H"               ~ "homme, situation indéterminée",
                                          Genre == "F"               ~ "femme, situation indéterminée")) %>%
            mutate(  Pro = ifelse(Activ == "11", paste(Pro, "à temps partiel"), Pro),
                     Pro = ifelse(Activ == "31", paste(Pro, "au chômage"), Pro),
                     Pro = ifelse(Activ == "32", paste(Pro, "à la retraite"), Pro),
                     Pro = ifelse(Activ == "33" & Genre == "H" & Pro != "homme au foyer", paste(Pro, "ayant cessé de travailler"), Pro),
                     Pro = ifelse(Activ == "33" & Genre == "F" & Pro != "femme au foyer", paste(Pro, "ayant cessé de travailler"), Pro))
    }
    
    if (!specGenre) {
        tab = mutate(tab, Pro = case_when(Genre == "H" & PCS == "00" ~ "de prof. inconnue",
                                          Genre == "H" & PCS == "01" ~ "agriculteur",
                                          Genre == "H" & PCS == "02" ~ "indépendant",
                                          Genre == "H" & PCS == "03" ~ "cadre",
                                          Genre == "H" & PCS == "04" ~ "prof. interm.",
                                          Genre == "H" & PCS == "05" ~ "employé",
                                          Genre == "H" & PCS == "06" ~ "ouvrier",
                                          Genre == "H" & PCS == "07" & Activ == "21" ~ "élève",
                                          Genre == "H" & PCS == "07" & Activ == "22" ~ "étudiant",
                                          Genre == "H" & PCS == "08" ~ "au chômage, n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "09" ~ "n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "10" ~ "agriculteur",
                                          Genre == "H" & PCS == "21" ~ "artisan",
                                          Genre == "H" & PCS == "22" ~ "commerçant",
                                          Genre == "H" & PCS == "23" ~ "chef d'entreprise",
                                          Genre == "H" & PCS == "31" ~ "prof. libérale",
                                          Genre == "H" & PCS == "32" ~ "enseignant ou fonction publique cat. A",
                                          Genre == "H" & PCS == "36" ~ "cadre d'entreprise",
                                          Genre == "H" & PCS == "41" ~ "prof. école, infirmier ou fn publique interm.",
                                          Genre == "H" & PCS == "46" ~ "salarié du privé, administration ou commerce",
                                          Genre == "H" & PCS == "47" ~ "technicien",
                                          Genre == "H" & PCS == "48" ~ "agent de maîtrise",
                                          Genre == "H" & PCS == "51" ~ "employé de la fonction publique ou de sécurité",
                                          Genre == "H" & PCS == "54" ~ "employé de bureau",
                                          Genre == "H" & PCS == "55" ~ "employé de commerce",
                                          Genre == "H" & PCS == "56" ~ "employé de ménage ou domestique",
                                          Genre == "H" & PCS == "61" ~ "ouvrier qualifié ou chauffeur",
                                          Genre == "H" & PCS == "66" ~ "ouvrier non qualifié",
                                          Genre == "H" & PCS == "69" ~ "ouvrier agricole",
                                          Genre == "H" & PCS == "81" ~ "chômeur n'ayant jamais travaillé",
                                          Genre == "H" & PCS == "82" & Activ != "33" ~ "sans activité officielle",
                                          Genre == "H" & PCS == "82" & Activ == "33" ~ "au foyer",
                                          Genre == "H" & PCS == "83" ~ "écolier",
                                          Genre == "H" & PCS == "84" ~ "collégien",
                                          Genre == "H" & PCS == "85" ~ "lycéen",
                                          Genre == "H" & PCS == "86" ~ "élève de BTS",
                                          Genre == "H" & PCS == "90" ~ "élève du secondaire",
                                          Genre == "H" & PCS == "87" ~ "étudiant de licence",
                                          Genre == "H" & PCS == "80" ~ "étudiant de licence ou BTS",
                                          Genre == "H" & PCS == "88" ~ "étudiant (Bac+3 ou plus)",
                                          Genre == "H" & PCS == "89" ~ "apprenti",
                                          Genre == "H" & PCS == "00" ~ "de prof. inconnue",
                                          Genre == "F" & PCS == "01" ~ "agricultrice",
                                          Genre == "F" & PCS == "02" ~ "indépendante",
                                          Genre == "F" & PCS == "03" ~ "cadre ou prof. sup.",
                                          Genre == "F" & PCS == "04" ~ "prof. interm.",
                                          Genre == "F" & PCS == "05" ~ "employée",
                                          Genre == "F" & PCS == "06" ~ "ouvrière",
                                          Genre == "F" & PCS == "07" & Activ == "21" ~ "élève",
                                          Genre == "F" & PCS == "07" & Activ == "22" ~ "étudiante",
                                          Genre == "F" & PCS == "08" ~ "au chomage, n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "09" ~ "n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "10" ~ "agricultrice",
                                          Genre == "F" & PCS == "21" ~ "artisante",
                                          Genre == "F" & PCS == "22" ~ "commerçante",
                                          Genre == "F" & PCS == "23" ~ "cheffe d'entreprise",
                                          Genre == "F" & PCS == "31" ~ "prof. libérale",
                                          Genre == "F" & PCS == "32" ~ "enseignante ou fonction publique cat. A",
                                          Genre == "F" & PCS == "36" ~ "cadre d'entreprise",
                                          Genre == "F" & PCS == "41" ~ "prof. école, infirmière ou fn publique interm.",
                                          Genre == "F" & PCS == "46" ~ "salariée du privé, administration ou commerce",
                                          Genre == "F" & PCS == "47" ~ "technicienne",
                                          Genre == "F" & PCS == "48" ~ "agente de maîtrise",
                                          Genre == "F" & PCS == "51" ~ "employée de la fonction publique ou de sécurité",
                                          Genre == "F" & PCS == "54" ~ "employée de bureau",
                                          Genre == "F" & PCS == "55" ~ "employée de commerce",
                                          Genre == "F" & PCS == "56" ~ "employée de ménage ou domestique",
                                          Genre == "F" & PCS == "61" ~ "ouvrière qualifiée ou chauffeuse",
                                          Genre == "F" & PCS == "66" ~ "ouvrière non qualifiée",
                                          Genre == "F" & PCS == "69" ~ "ouvrière agricole",
                                          Genre == "F" & PCS == "81" ~ "chômeuse n'ayant jamais travaillé",
                                          Genre == "F" & PCS == "82" & Activ != "33" ~ "sans activité officielle",
                                          Genre == "F" & PCS == "82" & Activ == "33" ~ "au foyer",
                                          Genre == "F" & PCS == "83" ~ "écolière",
                                          Genre == "F" & PCS == "84" ~ "collégienne",
                                          Genre == "F" & PCS == "85" ~ "lycéenne",
                                          Genre == "F" & PCS == "86" ~ "élève de BTS",
                                          Genre == "F" & PCS == "90" ~ "élève du secondaire",
                                          Genre == "F" & PCS == "87" ~ "étudiante de licence",
                                          Genre == "F" & PCS == "80" ~ "étudiante de licence ou BTS",
                                          Genre == "F" & PCS == "88" ~ "étudiante (Bac+3 et plus)",
                                          Genre == "F" & PCS == "89" ~ "apprentie",
                                          Genre == "H"               ~ "de situation indéterminée",
                                          Genre == "F"               ~ "de situation indéterminée")) %>%
            mutate(  Pro = ifelse(Activ == "11", paste(Pro, "à temps partiel"), Pro),
                     Pro = ifelse(Activ == "31", paste(Pro, "au chômage"), Pro),
                     Pro = ifelse(Activ == "32", paste(Pro, "à la retraite"), Pro),
                     Pro = ifelse(Activ == "33" & Genre == "H" & Pro != "au foyer", paste(Pro, "ayant cessé de travailler"), Pro),
                     Pro = ifelse(Activ == "33" & Genre == "F" & Pro != "au foyer", paste(Pro, "ayant cessé de travailler"), Pro))
    }
    
    return(as.character(tab$Pro))
}

etqMotifLieu = function(champ){
    champ = plyr::revalue(champ, c("01" = "Domicile",
                                   "02" = "Logement occasionnel",
                                   "10" = "Lieu de travail",
                                   "11" = "Lieu de travail",
                                   "12" = "Lieu de travail",
                                   "13" = "Lieu de travail",
                                   "14" = "Lieu de travail",
                                   "21" = "Garderie",
                                   "22" = "École",
                                   "23" = "Collège",
                                   "24" = "Lycée",
                                   "96" = "Collège/Lycée",
                                   "25" = "Université",
                                   "26" = "École",
                                   "27" = "Collège",
                                   "28" = "Lycée",
                                   "97" = "Collège/Lycée",
                                   "29" = "Université",
                                   "30" = "Commerce sans achat",
                                   "31" = "Centre commercial",
                                   "32" = "Supermarché",
                                   "33" = "Petit commerce",
                                   "34" = "Marché",
                                   "35" = "Drive ou Point-relais",
                                   "98" = "Achats",
                                   "41" = "Service de santé",
                                   "42" = "Démarche admin.",
                                   "43" = "Recherche d'emploi",
                                   "51" = "Loisirs",
                                   "52" = "Promenade",
                                   "53" = "Restaurant",
                                   "54" = "Visite",
                                   "60" = "Accompagnement",
                                   "61" = "Dépôt d'une personne",
                                   "62" = "Accompagnement",
                                   "63" = "Accompagnement",
                                   "64" = "Embarquement d'une personne",
                                   "70" = "Accompagnement",
                                   "71" = "Dépôt d'une personne",
                                   "72" = "Accompagnement",
                                   "73" = "Accompagnement",
                                   "74" = "Embarquement d'une personne",
                                   "81" = "Tournée professionnelle",
                                   "82" = "Tournée de magasins",
                                   "91" = "Lieu indéterminé",
                                   "68" = "Dépôt d'une personne",
                                   "69" = "Embarquement d'une personne",
                                   "99" = "Espace public"), warn_missing = F)
    return(champ)
}

etqMotifActiv = function(champ){
    champ = plyr::revalue(champ, c("01" = "Domicile",
                                   "02" = "Logement occasionnel",
                                   "11" = "Travail",
                                   "12" = "Travail",
                                   "13" = "Travail",
                                   "14" = "Travail",
                                   "21" = "En garderie",
                                   "22" = "Temps scolaire",
                                   "23" = "Temps scolaire",
                                   "24" = "Temps scolaire",
                                   "25" = "Études",
                                   "26" = "Temps scolaire",
                                   "27" = "Temps scolaire",
                                   "28" = "Temps scolaire",
                                   "29" = "Études",
                                   "96" = "Temps scolaire",
                                   "97" = "Temps scolaire",
                                   "30" = "Visite commerce sans achat",
                                   "31" = "Achats en centre commercial",
                                   "32" = "Achats en supermarché",
                                   "33" = "Achats en petit commerce",
                                   "34" = "Achats sur marché",
                                   "35" = "Drive ou Point-Relais",
                                   "98" = "Achats, sans précision",
                                   "41" = "Soins de santé",
                                   "42" = "Démarches admin.",
                                   "43" = "Recherche d'emploi",
                                   "44" = "Démarche indéterminée",
                                   "51" = "Loisirs sportifs, culturels ou assoc.",
                                   "52" = "Promenade",
                                   "53" = "Restaurant ou Bar",
                                   "54" = "Visite d'un·e proche",
                                   "60" = "Accompagnement",
                                   "61" = "Accompagnement",
                                   "62" = "Accompagnement",
                                   "63" = "Accompagnement",
                                   "64" = "Accompagnement",
                                   "70" = "Accompagnement",
                                   "71" = "Accompagnement",
                                   "72" = "Accompagnement",
                                   "73" = "Accompagnement",
                                   "74" = "Accompagnement",
                                   "81" = "Tournée professionnelle",
                                   "82" = "Tournée de magasins",
                                   "91" = "Activité indéterminée",
                                   "99" = "Déplacement"), warn_missing = F)
    return(champ)
}

niv_MotifActiv = c("Domicile",
                   "Logement occasionnel",
                   "Travail",
                   "En garderie",
                   "Temps scolaire",
                   "Études",
                   "Visite commerce sans achat",
                   "Achats en centre commercial",
                   "Achats en supermarché",
                   "Achats en petit commerce",
                   "Achats sur marché",
                   "Drive ou Point-Relais",
                   "Achats, sans précision",
                   "Soins de santé",
                   "Démarches admin.",
                   "Recherche d'emploi",
                   "Démarche indéterminée",
                   "Loisirs sportifs, culturels ou assoc.",
                   "Promenade",
                   "Restaurant ou Bar",
                   "Visite d'un·e proche",
                   "Accompagnement",
                   "Tournée professionnelle",
                   "Tournée de magasins",
                   "Activité indéterminée",
                   "Déplacement",
                   "Autres")

etqMotifActiv_Dom = function(champ){
    champ = plyr::revalue(champ, c("0" = "Résidence",
                                   "1" = "Travail",
                                   "2" = "Études",
                                   "3" = "Commerce",
                                   "4" = "Démarches",
                                   "5" = "Loisirs",
                                   "6" = "Accompagnement",
                                   "7" = "Accompagnement",
                                   "8" = "Tournées",
                                   "9" = "Indéterminé"), warn_missing = F)
    return(champ)
}

etqPronom = function(Genre, Lien)
{
    if (!Lien %in% c("1", "2", "3"))
    {
        if (Genre == "H") {return ("un")}
        if (Genre == "F") {return ("une")}
    }
    if (Lien %in% c("1", "2", "3"))
    {
        if (Genre == "H") {return ("son")}
        if (Genre == "F") {return ("sa")}
    }
}

etqLienDepuis = function(LienEgo, LienAlter, GenreAlter)
{
    Genre = GenreAlter
    if (LienEgo == "1" & LienAlter == "2")
    {if(Genre == "H") {return("conjoint")} ; if(Genre == "F") {return("conjointe")}}
    if (LienEgo == "2" & LienAlter == "1")
    {if(Genre == "H") {return("conjoint")} ; if(Genre == "F") {return("conjointe")}}
    if (LienEgo == "1" & LienAlter == "3")
    {if(Genre == "H") {return("fils")} ; if(Genre == "F") {return("fille")}}
    if (LienEgo == "2" & LienAlter == "3")
    {if(Genre == "H") {return("fils")} ; if(Genre == "F") {return("fille")}}
    if (LienAlter == "4")
    {return("colocataire")}
    if (LienAlter == "5")
    {if(Genre == "H") {return("parent")} ; if(Genre == "F") {return("parente")}}
    if (LienEgo == "3" & LienAlter == "1")
    {if(Genre == "H") {return("père")} ; if(Genre == "F") {return("mère")}}
    if (LienEgo == "3" & LienAlter == "2")
    {if(Genre == "H") {return("père")} ; if(Genre == "F") {return("mère")}}
    if (LienEgo == "3" & LienAlter == "3")
    {if(Genre == "H") {return("frère")} ; if(Genre == "F") {return("sœur")}}
    # autres cas
    return ("personne")
}

etqPersonne = function(uid_PER, uid_ego)
{
    lien_alt = PER[PER$uid_PER == uid_PER,]$Lien %>% as.character()
    genr_alt = PER[PER$uid_PER == uid_PER,]$Genre %>% as.character()
    csp_alt  = ifelse(is.na(PER[PER$uid_PER == uid_PER,]$PCS42S),
                      PER[PER$uid_PER == uid_PER,]$PCS8  %>% as.character(),
                      PER[PER$uid_PER == uid_PER,]$PCS42S  %>% as.character())
    acti_alt = PER[PER$uid_PER == uid_PER,]$Activ  %>% as.character()
    age_alt  = PER[PER$uid_PER == uid_PER,]$Age
    lien_ego = PER[PER$uid_PER == uid_ego,]$Lien %>% as.character()
    
    retour = paste(etqPronom(genr_alt, lien_alt),
                   etqLienDepuis(lien_ego, lien_alt, genr_alt),
                   etqPro(genr_alt, csp_alt, acti_alt, specGenre=F),
                   "de", age_alt, "ans")
}

etqMenage = function(uid_PER)
{
    # Décrit les autres membres du ménage en fonction d'ego
    
    if (!"PER" %in% ls()) { load("Data/PER.rds") }
    if (!"MEN" %in% ls()) { load("Data/MEN.rds") }
    
    men = PER[PER$uid_PER == uid_PER,]$uid_MEN
    moi = uid_PER
    
    listeMembresMen = filter(PER, uid_MEN == men) %>%
        filter(uid_PER != moi) %>%
        select(uid_PER, Lien, PCS8, Genre, Age)
    
    if (nrow(listeMembresMen)>0)
    {
        liste = lapply(listeMembresMen$uid_PER, etqPersonne,
                       uid_ego = uid_PER)
        return(paste0("vit avec ", paste(liste, collapse=", ")))
    }
    else
    {
        if (PER[PER$uid_PER == moi,]$Genre == "H")
        { return("vit seul") } else { return("vit seule") }
        
    }
}

etqLogement = function(uid_MEN, introLigne = F)
{
    if (!"MEN" %in% ls()) { load("Data/MEN.rds") }
    
    statut = MEN[MEN$uid_MEN == uid_MEN,]$LogOcc %>% as.character()
    type   = MEN[MEN$uid_MEN == uid_MEN,]$LogType %>% as.character()
    
    if (is.na(type)) { return("") }
    
    typeS = "logement non déterminé"
    if (!is.na(type)) {
        if (type == "1") {typeS = "pavillon"}
        if (type == "2") {typeS = "pavillon mitoyen"}
        if (type == "3") {typeS = "appartement"}
        if (type == "4") {typeS = "appartement"}
        if (type == "5") {typeS = "logement non déterminé"}
    }
    
    statutS = "(statut indéterminé)"
    if (!is.na(statut)) {
        if(statut=="10") {statutS = "en propriété"}
        if(statut=="20") {statutS = "en location"}
        if(statut=="21") {statutS = "en location HLM"}
        if(statut=="22") {statutS = "en location (?)"}
        if(statut=="30") {statutS = "en hébergement gratuit"}
        if(statut=="40") {statutS = "en résidence univ."}
    }
    
    rL = ifelse(introLigne, "\n", "")
    
    return(paste0(rL, "en ", typeS, " ", statutS))
}

etqLogOcc = function(champ, num=F, prefixe=F) {
    
    numeros = as.character(c(10, 20:22, 30, 40, 99))
    etiqs   = c("propriétaires", "locataires", "locataires HLM", "locataires n.d.",
                "logé·es gratuitement", "en résid. univ.", NA)
    
    if (num) { etiqs = paste0("[", numeros, "] ", etiqs) }
    
    if(prefixe){ numeros = paste0("LogOcc", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqGenre = function(champ, prefixe=F) {
    numeros = c("H", "F")
    etiqs   = c("Homme", "Femme")
    
    if(prefixe){ numeros = paste0("Genre", numeros) }
    
    liste = set_names(nm = numeros, x = etiqs)
    champ = plyr::revalue(champ, liste, warn_missing = F)
    
    return(champ)
}

etqDiversPrefixe = function(champ) {
    
    champ = plyr::revalue(champ, c("MenEnfantsTRUE" = "Ménage avec enfants",
                                   "MenEnfantsFALSE" = "Ménage sans enfants",
                                   "GenreH:MenEnfantsTRUE" = "Homme + Ménage avec enfants",
                                   
                                   "PCS802:GenreH" = "Indépendant + Homme",
                                   "PCS803:GenreH" = "Cadre / Prof. Intel. + Homme",
                                   "PCS804:GenreH" = "Prof. Interm. + Homme",
                                   "PCS805:GenreH" = "Employé + Homme",
                                   "PCS806:GenreH" = "Ouvrier + Homme",
                                   "PCS802:GenreF" = "Indépendante + Femme",
                                   "PCS803:GenreF" = "Cadre / Prof. Intel. + Femme",
                                   "PCS804:GenreF" = "Prof. Interm. + Femme",
                                   "PCS805:GenreF" = "Employée + Femme",
                                   "PCS806:GenreF" = "Ouvrière + Femme",
                                   
                                   "MenType1" = "Ménage avec enfants (>3 ans)",
                                   "MenType2" = "Ménage avec jeunes enfants",
                                   "MenType0" = "Ménage sans enfants",
                                   
                                   "nAct" = "Nombre d'activités hors domicile",
                                   "nActHorsTrav" = "Nombre d'activités hors domicile et travail",
                                   "DuTvlInv" = "Temps activités\nhors emploi ou domicile",
                                   "Tps" = "Temps en déplacement",
                                   
                                   "DuDep" = "Temps passé en déplacement",
                                   "DuTvl" = "Temps passé sur\nlieu d'emploi (mn.)",
                                   "DuTvlHr" = "Temps passé sur\nlieu d'emploi (h)",
                                   "Dis" = "Dist. parcourue sur 24h (km)",
                                   "Dis10" = "Dist. parcourue sur 24h\n(km ×10)",
                                   "Dist" = "Longueur du déplacement (km)",
                                   
                                   "plusLieuxTravailplusieurs lieux de travail" = "Plusieurs lieux de travail",
                                   "plusLieuxTravailun lieu de travail" = "Un seul lieu de travail",
                                   
                                   "ageMinEnfantpas d'enfant à charge" = "Pas d'enfant à charge",
                                   "ageMinEnfantplus jeune enfant : 00 à 3 ans" = "Plus jeune enfant : 0 à 3 ans",
                                   "ageMinEnfantplus jeune enfant : 04 à 9 ans" = "Plus jeune enfant : 4 à 9 ans",
                                   "ageMinEnfantplus jeune enfant : 10 à 14 ans" = "Plus jeune enfant : 10 à 14 ans",
                                   "ageMinEnfantplus jeune enfant : 15 à 18 ans" = "Plus jeune enfant : 15 à 18 ans",
                                   
                                   "categUsgConducteur⋅rices" = "Conducteur⋅rices",
                                   "categUsgPassager⋅es" = "Passager⋅es",
                                   "categUsgUsager⋅es de deux-roues" = "Usager⋅es de deux-roues",
                                   "categUsgAutomobilistes exclusif⋅ves" = "Automobilistes exclusif⋅ves",
                                   
                                   "Fqc_Vco1" = niv_fqc_Vco[1],
                                   "Fqc_Vco2" = niv_fqc_Vco[2],
                                   "Fqc_Vco3" = niv_fqc_Vco[3],
                                   "Fqc_Vco4" = niv_fqc_Vco[4],
                                   
                                   "Fqc_Vpa1" = niv_fqc_Vpa[1],
                                   "Fqc_Vpa2" = niv_fqc_Vpa[2],
                                   "Fqc_Vpa3" = niv_fqc_Vpa[3],
                                   "Fqc_Vpa4" = niv_fqc_Vpa[4],
                                   
                                   "Fqc_Drm1" = niv_fqc_Drm[1],
                                   "Fqc_Drm2" = niv_fqc_Drm[2],
                                   "Fqc_Drm3" = niv_fqc_Drm[3],
                                   "Fqc_Drm4" = niv_fqc_Drm[4],
                                   
                                   "Fqc_Tco1" = niv_fqc_Tco[1],
                                   "Fqc_Tco2" = niv_fqc_Tco[2],
                                   "Fqc_Tco3" = niv_fqc_Tco[3],
                                   "Fqc_Tco4" = niv_fqc_Tco[4],
                                   
                                   "Fqc_Vel1" = niv_fqc_Vel[1],
                                   "Fqc_Vel2" = niv_fqc_Vel[2],
                                   "Fqc_Vel3" = niv_fqc_Vel[3],
                                   "Fqc_Vel4" = niv_fqc_Vel[4],
                                   
                                   "Fqc_Mch1" = niv_fqc_Mch[1],
                                   "Fqc_Mch2" = niv_fqc_Mch[2],
                                   "Fqc_Mch3" = niv_fqc_Mch[3],
                                   "Fqc_Mch4" = niv_fqc_Mch[4],
                                   
                                   "sortiesTRUE" = "Act. non professionnelles",
                                   "sortiesFALSE" = "Pas d'act. non professionnelles",
                                   "tcTRUE" = "Recours aux TC",
                                   "tcFALSE" = "Pas de recours aux TC",
                                   
                                   "dsDom1er quartile" = "1er quartile (résid.)",
                                   "dsDom2e quartile" = "2e quartile (résid.)",
                                   "dsDom3e quartile" = "3e quartile (résid.)",
                                   "dsDom4e quartile" = "4e quartile (résid.)",
                                   
                                   "dsTvl1er quartile" = "1er quartile (trav.)",
                                   "dsTvl2e quartile" = "2e quartile (trav.)",
                                   "dsTvl3e quartile" = "3e quartile (trav.)",
                                   "dsTvl4e quartile" = "4e quartile (trav.)"
                                   ),
                          warn_missing = F)
    
    etiqsCouple = paste0(sort(rep(etqPCS8(c("02","03","04","05","06")), times=5)), " + ",
                         rep(etqPCS8(c("02", "03", "04", "05", "06")), times=5))
    
    rplcCouple = set_names(x = etiqsCouple, nm = paste0("couple", etiqsCouple))
    
    
    
    
    champ = plyr::revalue(champ, rplcCouple)
    
    
    return(champ)
    
}

etqInverseDs = function(champ)
{
  niv_ds_etq = paste0(niv_ds, " hab./km²")
  liste = set_names(nm = c(paste0("dsDomEtq", niv_ds),
                           paste0("dsTvlEtq", niv_ds),
                           paste0("dsO_Etq", niv_ds)),
                    x  = rep(niv_ds_etq, 3))
  champ = plyr::revalue(champ, liste, warn_missing=F)
  champ = factor(champ, c(niv_ds_etq, levels(champ)[!levels(champ) %in% niv_ds_etq]))
  return(champ)
}

etqInverseMotifs = function(champ)
{
  niv_motifs_duo = paste0(sort(rep(niv_MotifActiv, length(niv_MotifActiv))), "-", rep(niv_MotifActiv, length(niv_MotifActiv)))
  niv_motifs_duo = c(niv_motifs_duo, "Autres")
  liste = set_names(nm = paste0("Motif", niv_motifs_duo), x = niv_motifs_duo)
  champ = plyr::revalue(champ, liste, warn_missing=F)
  return(champ)
}

etqOpiMots = function(champ)
{
    plyr::revalue(champ,
                  c("01" = "p_rapide",
                    "02" = "p_pascher",
                    "03" = "p_pratique",
                    "04" = "p_ecolo",
                    "05" = "p_sur",
                    "06" = "p_confort",
                    "07" = "p_autonomie",
                    "08" = "p_silence",
                    "09" = "p_repos",
                    "10" = "p_utile",
                    "11" = "p_agreable",
                    "12" = "p_detente",
                    "13" = "p_necessaire",
                    "14" = "p_convivial",
                    "15" = "p_loisir",
                    "16" = "p_facile",
                    "17" = "p_sport",
                    "18" = "p_flexible",
                    "19" = "p_zzz",
                    "21" = "n_lent",
                    "22" = "n_cher",
                    "23" = "n_paspratique",
                    "24" = "n_polluant",
                    "25" = "n_danger",
                    "26" = "n_inconfort",
                    "27" = "n_contrainte",
                    "28" = "n_bruit",
                    "29" = "n_fatigue",
                    "30" = "n_inutile",
                    "31" = "n_desagreable",
                    "32" = "n_stress",
                    "33" = "n_inadapte",
                    "34" = "n_bonde",
                    "35" = "n_sale",
                    "36" = "n_encombrant",
                    "37" = "n_pasfiable",
                    "38" = "n_insuffisant",
                    "39" = "n_zzz",
                    "40" = "z_autre",
                    "41" = "n_vol",
                    "81" = NA,
                    "82" = NA,
                    "83" = NA,
                    "00" = "z_sansopi")) %>% return()
}

etqEnq = function(uid_ENQ)
{
  l = length(uid_ENQ)
  enq = vector(mode="character", length=l)
  if (l > 10)
  { b = ui_ProgInit(l) }
  for(i in 1:l)
  {
    enq[i] = z_Nomenclature[z_Nomenclature$uid_ENQ == uid_ENQ[i],]$Libelle_Long
    if (l>10)
    {
      ui_Prog(barre = b, i = i)
    }
  }
  return(enq)
}


# Fonctions de gestion des étiquettes ===============================================================

refactoriser = function(table, num=F, simplifier=F) {
    # Fonction qui repère les colonnes dans une table et en remplace les facteurs par des valeurs
    # lisibles.
    
    if ("Activ" %in% colnames(table))
    { table$Activ = etqActiv(champ = table$Activ, num = num) }
    
    if ("PCS8" %in% colnames(table))
    { table$PCS8  = etqPCS8(champ = table$PCS8, num = num) }
    
    if ("PCS42S" %in% colnames(table))
    { table$PCS42S = etqPCS42S(champ = table$PCS42S, num = num) }
    
    if ("ZoneDens" %in% colnames(table))
    { table$ZoneDens = etqZoneDens(champ = table$ZoneDens, num = num, supprTrFaible = T) }
    
    if ("ZoneRang" %in% colnames(table))
    { table$ZoneRang = etqZoneRang(champ = table$ZoneRang, num = num) }
    
    if ("ZoneDens_travMax" %in% colnames(table))
    { table$ZoneDens_travMax = etqZoneDens(champ = table$ZoneDens_travMax, num = num, supprTrFaible = T) }
    
    if ("ZoneRang_travMax" %in% colnames(table))
    { table$ZoneRang_travMax = etqZoneRang(champ = table$ZoneRang_travMax, num = num, supprTrFaible = T) }
    
    if ("NivEtu" %in% colnames(table))
    { table$NivEtu = etqNivEtu(champ = table$NivEtu, spl = simplifier, num = num) }
    
    if ("Lien" %in% colnames(table))
    { table$Lien = etqLien(champ = table$Lien) }
    
    if ("PCSML" %in% colnames(table))
    { table$PCSML = etqPCSM(champ = table$PCSML, dét = T) }
    
    if ("PCSMT" %in% colnames(table))
    { table$PCSMT = etqPCSM(champ = table$PCSMT, dét = T) }
    
    # On s'attaque à toutes les colonnes Fqc_ à la fois
    if (length(which(grepl(colnames(table), pattern="Fqc_"))) > 0)
    {
        for (i in 1:length(which(grepl(colnames(table), pattern="Fqc_"))))
        {
            table[,which(grepl(colnames(table), pattern="Fqc_"))[i]] = 
                etqFqc(as.character(unlist(table[,which(grepl(colnames(table), pattern="Fqc_"))[i]])),
                       num=num)
        }
    }

    if ("Mode" %in% colnames(table))
    { table$Mode = etqMode(champ = table$Mode) }
    
    if ("LogOcc" %in% colnames(table))
    { table$LogOcc = etqLogOcc(champ = table$LogOcc, num = num) }
    
    if ("Genre" %in% colnames(table))
    { table$Genre = etqGenre(champ = table$Genre) }
    
    
    return(table)
}


nomColsLisibles = function(colnames_l)
{
    colnames_l = plyr::revalue(colnames_l, c("Activ"   = "Statut professionnel",
                                                       "PCS8"    = "PCS (8 postes)",
                                                       "PCS42S"   = "PCS",
                                                       "PCSM"    = "PCS Ménage",
                                                       "PCSML"   = "PCS Ménage (approx.)",
                                                       "PCSMT"   = "PCS Ménage modifiée",
                                                       "PCSMLT"  = "PCS Ménage modifiée",
                                                       "Age5"    = "Classe d'âge",
                                                       "Age10"   = "Classe d'âge",
                                                       "LogOcc"  = "Statut d'occup. logt.",
                                                       "ZoneDens"= "Densité com. de résid.",
                                                       "ZoneRang"= "AAV de résidence",
                                                       "ZoneDens_travMax" = "Densité com. de travail",
                                                       "ZoneRang_travMax" = "AAV de la com. de travail",
                                                       "NivEtu"  = "Niveau de diplôme",
                                             
                                                       "plusLieuxTravail" = "L. Travail",
                                             
                                                       "MenEnfants" = "Enfant(s) (<16 ans)",
                                                       
                                                       "Fqc_Mch" = "Recours marche décl.",
                                                       "Fqc_Vel" = "Recours vélo décl.",
                                                       "Fqc_Drm" = "Recours DRM décl.",
                                                       "Fqc_Vco" = "Recours voiture (cond.) décl.",
                                                       "Fqc_Vpa" = "Recours voiture (pass.) décl.",
                                                       "Fqc_Tco" = "Recours TC décl.",
                                                       
                                                       "Dis"     = "Dis. totale estimée",
                                                       "DisOk"   = "Dis. parcourue h/ marche et TC",
                                                       "Dis.V"   = "Somme dépl. à vol d'oiseau",
                                                       "Dis.M"   = "Longueur médiane déplacement",
                                                       "Tps"     = "Temps passé en déplacement",
                                                       "N"       = "Nombre de déplacements",
                                                       "nAct"    = "Nb. d'activités hors domicile",
                                                       "V"       = "Vitesse moyenne de déplacement",
                                                       
                                                       
                                                       "pDis_MAR"= "Part modale marche (veille)",
                                                       "pDis_VOI"= "Part modale autom. (veille)",
                                                       "pDis_VEL"= "Part modale vélo (veille)",
                                                       "pDis_DRM"= "Part modale DRM (veille)",
                                                       "pDis_BUS"= "P. mod. TC s. trains (veille)",
                                                       "pDis_TRN"= "Part modale train (veille)",
                                                       "pDis_TCO"= "Part modale TC (veille)",
                                                       
                                                       "pDis_CTR"= "Part dépct. contraints",
                                                       "pDis_LSR"= "Part dépct. pour loisirs",
                                                       
                                                       "mDis_Tvl"= "Lgr. méd. déplct. pro",
                                                       
                                                       "JoDeb"   = "Heure départ domicile",
                                                       "JoFin"   = "Heure retour domicile",
                                                       "DuTvl"   = "Temps passé sur l. de travail",
                                                       "DuDom"   = "Temps passé au domicile",
                                                       "DuExt"   = "Temps activités ext. hors travail",
                                                       
                                                       "rapDomCtdDis" = "Indicateur d'excentricité",
                               
                                                       "dsDomEtq" = "Densité zone de résidence",
                                                       "dsTvlEtq" = "Densité zone d'emploi",
                                                       "dsO_Etq" = "Densité au départ",
                               
                                                       "sorties" = "Sorties non-professionnelles",
                                                       "Motif" = "Motif de dépl.",
                                             
                                                       "dsDom" = "Densité zone\nde résidence",
                                                       "dsTvl" = "Densité zone\nd'emploi"),
                                    warn_missing = F)
    
    return(colnames_l)
}

 identificationInverseEtq = function(etiquette)
{
    # Cette fonction réattribue la variable à laquelle correspondent les étiquettes, pour les modèles
    
    if (etiquette %in% niv_Acti)     { return("Statut pro.") }
    if (etiquette %in% niv_PCS8)     { return("PCS") }
    if (etiquette %in% niv_PCS42S)   { return("PCS") }
    if (etiquette %in% niv_ZoneDens) { return("Com. Résid.") }
    if (etiquette %in% paste0(niv_ZoneDens, " (travail)")) { return("Com. Travail") }
    if (etiquette %in% niv_ZoneRang) { return("AAV") }
    if (etiquette %in% c(niv_NivEtu_Det, niv_NivEtu_Spl)) { return("Niv. Études") }
    if (etiquette %in% niv_Lien)     { return("Lien P. Réf.") }
    if (etiquette %in% c("Homme", "Femme")) { return("Genre") }
    if (etiquette %in% niv_NivDip)   { return("Diplôme")}
    if (etiquette %in% c("Ménage sans enfants", "Ménage avec enfants")) { return("Enfts.") }
    if (etiquette %in% c("Act. non professionnelles", "Pas d'act. non professionnelles")) { return("Activités") }
    if (etiquette %in% paste0(niv_ds, " hab./km²")) { return("Densité") }
    if (etiquette %in% c("Recours aux TC", "Pas de recours aux TC")) { return("Modes")}
    
    if (substr(etiquette, 1, 7) == "Fqc_Tco") { return("Fqc. usage TC") }
    if (substr(etiquette, 1, 7) == "Fqc_Vco") { return("Fqc. conduite véh.") }
    if (etiquette %in% niv_fqc_Drm) { return("Fqc. DRM") }
    if (etiquette %in% niv_fqc_Mch) { return("Fqc. marche") }
    if (etiquette %in% niv_fqc_Tco) { return("Fqc. T.C.") }
    if (etiquette %in% niv_fqc_Vco) { return("Fqc. conduite") }
    if (etiquette %in% niv_fqc_Vpa) { return("Fqc. v. pass.") }
    if (etiquette %in% niv_fqc_Vel) { return("Fqc. vélo") }
     
    if (etiquette %in% c(niv_PCSMT_num,
                         paste0(niv_PCSMT_num, " ", niv_PCSMT_lab),
                         paste0(niv_PCSMT_num, "\n", niv_PCSMT_lab),
                         paste0("PCS M. ", niv_PCSMT_num))) { return ("PCS Ménage") }
     
    if (substr(etiquette, 1, 12) == "ageMinEnfant") { return ("Enfants") }
     
    if (substr(etiquette, 1, 19) == "Plus jeune enfant :") { return ("Enfants") }
    if (etiquette %in% c("Pas d'enfant à charge")) { return ("Enfants") }
     
    if (etiquette %in% c("Plusieurs lieux de travail",
                         "Un seul lieu de travail")) { return("L. trav.") }
   
   if (etiquette %in% c("Conducteur⋅rices", "Passager⋅es", "Automobilistes exclusif⋅ves", "Usager⋅es de deux-roues")) { return("Profil modal") }
    
    # Pour les intervalles d'âge, on va prévoir large
    etiqsAge = paste(sort(rep(5:120, times=115)), paste("à", c(5:120), "ans"))
    etiqsAge = c(etiqsAge, "60 à 65 ans") # grand mystère : n'est pas automatiquement calculée...
    etiqsAge = c(etiqsAge, paste0("Age10", etiqsAge), paste0("Age5", etiqsAge))
    
    etiqsCouple = paste0(sort(rep(etqPCS8(c("02","03","04","05","06")), times=5)), " + ",
                         rep(etqPCS8(c("02", "03", "04", "05", "06")), times=5))
    
    if (etiquette %in% etiqsCouple) { return ("Professions") }
    if (etiquette %in% etiqsAge) { return("Âge") }
    
    if (etiquette %in% paste0(c("1er", "2e", "3e", "4e"), " quartile ", rep("(résid.)", times = 4)))
    { return("Densité s. résid.") }
    if (etiquette %in% paste0(c("1er", "2e", "3e", "4e"), " quartile ", rep("(trav.)", times = 4)))
    { return("Densité s. trav.") }
    
    if (substr(etiquette, 1, 7) == "O_Motif") { return ("Activité départ") }
    if (substr(etiquette, 1, 7) == "D_Motif") { return ("Activité arrivée") }

    niv_motifs_duo = paste0(sort(rep(niv_MotifActiv, length(niv_MotifActiv))), "-", rep(niv_MotifActiv, length(niv_MotifActiv)))
    niv_motifs_duo = c(niv_motifs_duo, "Autres")
    if (etiquette %in% niv_motifs_duo) { return ("Motif de déplacement") }
    
    return("")
}

idSerieEtiquettes = function(etiquettes)
{
    sapply(X = etiquettes, FUN = identificationInverseEtq) %>% return()
}

retirerPrefixeAge = function(champ, desacAge = F)
{
    etiquettes = levels(champ)
    
    etiquettes = ifelse(nchar(etiquettes)>5, ifelse(test = substr(etiquettes,1,5) == "Age10",
                                                    yes = substr(etiquettes, 6, nchar(etiquettes)),
                                                    no = etiquettes), etiquettes)
    
    etiquettes = ifelse(nchar(etiquettes)>4, ifelse(test = substr(etiquettes,1,4) == "Age5" & etiquettes != "Age50 à 59 ans",
                                                    yes = substr(etiquettes, 5, nchar(etiquettes)),
                                                    no = etiquettes), etiquettes)
    
    
    if (!desacAge) {
    # On peut désactiver, au cas où il y aurait des étiquettes du genre "Agens"
    etiquettes = ifelse(nchar(etiquettes)>3, ifelse(test = substr(etiquettes,1,3) == "Age" & etiquettes != "Agent·e de maîtrise",
                                                    yes = substr(etiquettes, 4, nchar(etiquettes)),
                                                    no = etiquettes), etiquettes)
    }
    
    levels(champ) = etiquettes
    return(champ)
}


# Fonction pour adopter les mêmes valeurs de référence partout
valref = function(PER) {
  if ("Age5" %in% colnames(PER))    {PER$Age5     = relevel(PER$Age5,    "40 à 44 ans")}
  if ("Age10" %in% colnames(PER))   {PER$Age10    = relevel(PER$Age10,   "40 à 49 ans")}
  if ("PCS8" %in% colnames(PER))    {PER$PCS8     = relevel(PER$PCS8,    "04")}
  if ("PCSMLT" %in% colnames(PER))  {PER$PCSMLT  = relevel(PER$PCSMLT,  "3AB")} 
  if ("ZoneRang" %in% colnames(PER)){PER$ZoneRang = relevel(PER$ZoneRang, "4")}
  if ("PCS42S" %in% colnames(PER))  {PER$PCS42S   = relevel(PER$PCS42S,   "41")}
  if ("LOI2015" %in% unique(PER$uid_ENQ)) {PER$uid_ENQ.f= relevel(as.factor(PER$uid_ENQ),  "LOI2015")}
  if ("dsDomEtq" %in% colnames(PER)) { PER$dsDomEtq = relevel(PER$dsDomEtq, "1600 à 3200") }
  if ("dsTvlEtq" %in% colnames(PER)) { PER$dsTvlEtq = relevel(PER$dsTvlEtq, "1600 à 3200") }
  if ("dsO_Etq" %in% colnames(PER)) { PER$dsO_Etq = relevel(PER$dsO_Etq, "1600 à 3200") }
  if ("NivDip" %in% colnames(PER))  {PER$NivDip   = relevel(PER$NivDip, "2")}
  if ("Fqc_Drm" %in% colnames(PER)) {PER$Fqc_Drm = relevel(PER$Fqc_Drm, "3")}
  if ("Fqc_Mch" %in% colnames(PER)) {PER$Fqc_Mch = relevel(PER$Fqc_Mch, "3")}
  if ("Fqc_Tco" %in% colnames(PER)) {PER$Fqc_Tco = relevel(PER$Fqc_Tco, "3")}
  if ("Fqc_Vco" %in% colnames(PER)) {PER$Fqc_Vco = relevel(PER$Fqc_Vco, "3")}
  if ("Fqc_Vel" %in% colnames(PER)) {PER$Fqc_Vel = relevel(PER$Fqc_Vel, "3")}
  if ("Fqc_Vpa" %in% colnames(PER)) {PER$Fqc_Vpa = relevel(PER$Fqc_Vpa, "3")}
  return(PER)
}


découperDuo = function(table, local = T) {
  table$duoRes = plyr::revalue(table$ZoneDens_duo, c("Densité forte → Densité forte"     = "Commune dense",
                                                     "Densité forte → Densité interm."   = "Commune dense",
                                                     "Densité forte → Densité faible"    = "Commune dense",
                                                     "Densité interm. → Densité forte"   = "Commune interm.",
                                                     "Densité interm. → Densité interm." = "Commune interm.",
                                                     "Densité interm. → Densité faible"  = "Commune interm.",
                                                     "Densité faible → Densité forte"    = "Commune peu dense",
                                                     "Densité faible → Densité interm."  = "Commune peu dense",
                                                     "Densité faible → Densité faible"   = "Commune peu dense",
                                                     "Local Densité forte"               = "Commune dense",
                                                     "Local Densité interm."             = "Commune interm.",
                                                     "Local Densité faible"              = "Commune peu dense"))
  table$duoTvl = plyr::revalue(table$ZoneDens_duo, c("Densité forte → Densité forte"     = "vers Commune dense",
                                                     "Densité forte → Densité interm."   = "vers Commune interm.",
                                                     "Densité forte → Densité faible"    = "vers Commune peu dense",
                                                     "Densité interm. → Densité forte"   = "vers Commune dense",
                                                     "Densité interm. → Densité interm." = "vers Commune interm.",
                                                     "Densité interm. → Densité faible"  = "vers Commune peu dense",
                                                     "Densité faible → Densité forte"    = "vers Commune dense",
                                                     "Densité faible → Densité interm."  = "vers Commune interm.",
                                                     "Densité faible → Densité faible"   = "vers Commune peu dense",
                                                     "Local Densité forte"               = "vers Même secteur",
                                                     "Local Densité interm."             = "vers Même secteur",
                                                     "Local Densité faible"              = "vers Même secteur"))
  return(table)
}
