# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                                                                 #
#     E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                                   #
#                                                                                                 #
#     SCRIPTS DE TRAVAIL M. GUINEPAIN                                                             #
#     ANALYSES POUR LE CHAPITRE 5 : RYTHMES DE LA HIÉRARCHIE SOCIALE                              #
#                                                                                                 #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

options(scipen= 1000)
# Pour retourner à 90° une des images
library(magick)

# setwd("/home/maxime/Données/EMD")
# Pour exécuter ce script lui-même : > source("Scripts/analyses_horaires.R", print.eval=T)

rm(list = ls()) ; gc()
source("START.R", print.eval=T)

initMémoire(BasesCharger = c("PER", "ACT"))

# Répertoire des figures : "Horaires"
if (!dir.exists("Sorties/Horaires")) { dir.create("Sorties/Horaires") }

PER_ff = PER |> init_PER_ff()
remove(PER)

# Table des activités par heure ====

rapport("Tables des activités", prim = T)


if (file.exists("Data/activites.rds"))
{
  load("Data/activites.rds")
} else
{
  initMémoire(f_base = T, BasesCharger = c("PER", "ACT", "DEP")) ; gc()
  
  load_activites(PER, DEP, ACT, pasDeTemps = 30)
  
  save(activites, file = "Data/activites.rds")
  
  initMémoire(f_base = F, BasesCharger = c("PER", "ACT", "activites"))
  PER_ff = init_PER_ff() |> densitesZversPER()
  remove(PER)
}

sortie("Horaires/Activités, tables exploratoires", format = "pdf", taille = "a4")

activites %>% filter(substr(Tache, 1, 2) == "99") %>%
  pivot_wider(names_from = Activ, values_from = CoeffEnq, names_prefix = "Activ_") %>%
  #mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  group_by(heure) %>% summarize(across(starts_with("Activ_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("Activ_"), values_to = "pop", names_to = "activ") %>%
  mutate(heure = heure/60) %>%
  mutate(activ = etqActiv(substr(activ, 7, 8), num=T)) %>%
  mutate(activ = factor(activ, levels = rev(levels(activ)))) %>%
  #left_join(y = z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ")) %>%
  ggplot(aes(x = heure, y = pop)) + geom_area(aes(fill = activ)) +# + facet_wrap(~Libelle_Long)
  theme_bw() + labs(title = "Une représentation de la population en déplacement heure par heure",
                    subtitle = "par statut d'activité professionnelle",
                    catpion = paste("Production : M. Guinepain, 2022 ; d'après", source)) +
  xlab("heure") + ylab("nombre de personnes") +
  scale_fill_manual(values = c(rev(pal_Activ), "grey"), name = "Statut d'activité") %>% print()

PER %>% pivot_wider(names_from = Activ, values_from = CoeffEnq, names_prefix = "Activ_") %>%
  summarize(across(starts_with("Activ_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("Activ_"), values_to = "pop", names_to = "activ") %>%
  mutate(activ = etqActiv(substr(activ, 7, 8), num=T)) %>%
  mutate(activ = factor(activ, levels = rev(levels(activ)))) %>%
  mutate(total = "Total") %>%
  ggplot(aes(x = total, y = pop)) + geom_col(aes(fill = activ)) + theme_bw() +
  scale_fill_manual(values = c(rev(pal_Activ), "grey"), name = "Statut d'activité") +
  xlab("") + ylab("population totale") %>% print()

activites %>% filter(substr(Tache, 1, 2) == "99") %>%
  left_join(y = select(PER, uid_PER, typoJo), by=c("uid_PER" = "uid_PER")) %>%
  pivot_wider(names_from = typoJo, values_from = CoeffEnq, names_prefix = "typoJo_") %>%
  #mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  group_by(heure) %>% summarize(across(starts_with("typoJo_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("typoJo_"), values_to = "pop", names_to = "typoJo") %>%
  mutate(heure = heure/60) %>%
  mutate(typoJo = plyr::revalue(typoJo, c("typoJo_CHOM" = "1. Journée\nsans sortie\npour études ni\ntravail",
                                          "typoJo_ETUD" = "2. Journée\nsans sortie\npour travail,\navec sortie\npour études",
                                          "typoJo_TRAV" = "3. Journée\navec sortie pour travail"))) %>%
  #left_join(y = z_Nomenclature, by=c("uid_ENQ" = "uid_ENQ")) %>%
  ggplot(aes(x = heure, y = pop)) + geom_area(aes(fill = typoJo)) +# + facet_wrap(~Libelle_Long)
  theme_bw() + labs(title = "Une représentation de la population en déplacement heure par heure",
                    subtitle = "selon si la personne est allée travailler le jour d'enquête",
                    catpion = paste("Production : M. Guinepain, 2022 ; d'après", source)) +
  xlab("heure") + ylab("nombre de personnes") +
  scale_fill_manual(values=c("grey40", "grey70", "orange"),
                    name = "Activités\nce jour-là") %>% print()

PER %>%         filter(!is.na(typoJo)) %>%
  pivot_wider(names_from = typoJo, values_from = CoeffEnq, names_prefix = "typoJo_") %>%
  summarize(across(starts_with("typoJo_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("typoJo_"), values_to = "pop", names_to = "typoJo") %>%
  mutate(total = "Total") %>%
  ggplot(aes(x = total, y = pop)) + geom_col(aes(fill = typoJo)) + theme_bw() +
  scale_fill_manual(values=c("grey40", "grey70", "orange")) +
  xlab("") + ylab("population totale") %>% print()

activites %>%
  left_join(select(PER, uid_PER, typoJo, PCS8), by = c("uid_PER" = "uid_PER")) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06") & typoJo == "TRAV") %>%
  mutate(auTravail = ifelse(substr(Tache,1,2) == "10", CoeffEnq,0),
         PCS8 = etqPCS8(PCS8),
         heure=heure/60) %>%
  group_by(heure, PCS8) %>% summarize(sommeTravail = sum(auTravail, na.rm=T),
                                      sommeTotale  = sum(CoeffEnq, na.rm=T)) %>%
  mutate(ratioTravail = sommeTravail/sommeTotale * 100) %>%
  ggplot(aes(x = heure, y = ratioTravail)) + geom_line(aes(color = PCS8), size=1.5, alpha=.8) +
  scale_color_manual(values = pal_PCS8[2:6], name = "PCS individuelle") +
  ylab("Part des individus présent⋅es sur un lieu de travail (%)") +
  labs(title=ml("Présence des travailleur⋅ses (un jour où elles/ils se rendent au travail)",
                "sur leur lieu de travail en fonction de l'heure"),
       caption = src_fig(bu = T, date = "2022")) +
  theme_bw() %>% print()

activites %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(uid_ENQ %in% c("IDF2010", "LOI2015", "LYO2015", "REU2016")) %>%
  left_join(select(PER, uid_PER, typoJo, PCS8), by = c("uid_PER" = "uid_PER")) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06") & typoJo == "TRAV") %>%
  mutate(auTravail = ifelse(substr(Tache,1,2) == "10", CoeffEnq,0),
         PCS8 = etqPCS8(PCS8),
         heure=heure/60) %>%
  group_by(uid_ENQ, heure, PCS8) %>% summarize(sommeTravail = sum(auTravail, na.rm=T),
                                               sommeTotale  = sum(CoeffEnq, na.rm=T)) %>%
  mutate(ratioTravail = sommeTravail/sommeTotale * 100) %>%
  ggplot(aes(x = heure, y = ratioTravail)) + geom_line(aes(color = PCS8), size=1.5, alpha=.8) +
  scale_color_manual(values = pal_PCS8[2:6], name = "PCS individuelle") +
  ylab("Part des individus présent⋅es sur un lieu de travail (%)") +
  labs(title=ml("Présence des travailleur⋅ses (un jour où elles/ils se rendent au travail)",
                "sur leur lieu de travail en fonction de l'heure et de l'EMD"),
       caption = src_fig(bu = T, date = "2022")) +
  facet_wrap(~uid_ENQ) +
  theme_bw() %>% print()

activites %>%
  left_join(select(PER, uid_PER, typoJo, PCS8), by = c("uid_PER" = "uid_PER")) %>%
  filter(PCS8 %in% c("03", "05", "06") & typoJo == "TRAV") %>%
  mutate(auTravail = ifelse(substr(Tache,1,2) == "10", CoeffEnq,0),
         PCS8 = etqPCS8(PCS8), Genre=etqGenre(Genre),
         heure=heure/60) %>%
  group_by(heure, Genre, PCS8) %>% summarize(sommeTravail = sum(auTravail, na.rm=T),
                                             sommeTotale  = sum(CoeffEnq, na.rm=T)) %>%
  mutate(ratioTravail = sommeTravail/sommeTotale * 100) %>%
  ggplot(aes(x = heure, y = ratioTravail)) +
  geom_line(aes(color = PCS8, linetype=Genre), size=1.5, alpha=.8) +
  scale_color_manual(values = pal_PCS8[c(3,5,6)], name = "PCS individuelle") +
  ylab("Part des individus présent⋅es sur un lieu de travail (%)") +
  labs(title=ml("Présence des travailleur⋅ses (un jour où elles/ils se rendent au travail)",
                "sur leur lieu de travail en fonction de l'heure et de leur genre"),
       caption = src_fig(bu = T, date = "2022")) +
  theme_bw() %>% print()

activites %>%
  left_join(select(PER, uid_PER, typoJo, PCS8), by = c("uid_PER" = "uid_PER")) %>%
  filter(PCS8 %in% c("03", "05", "06") & typoJo == "TRAV") %>%
  mutate(auTravail = ifelse(substr(Tache,1,2) == "10", CoeffEnq,0),
         PCS8 = etqPCS8(PCS8), Genre=etqGenre(Genre),
         heure=heure/60) %>%
  group_by(heure, Genre, PCS8) %>% summarize(sommeTravail = sum(auTravail, na.rm=T),
                                             sommeTotale  = sum(CoeffEnq, na.rm=T)) %>%
  mutate(ratioTravail = sommeTravail/sommeTotale * 100) %>%
  pivot_wider(names_from = Genre, values_from = ratioTravail) %>%
  group_by(heure, PCS8) %>% summarize(ratioFemmes = first(na.omit(Femme)),
                                      ratioHommes = first(na.omit(Homme))) %>%
  mutate(ratioGenre = (ratioFemmes/ratioHommes - 1) * 100) %>%
  ggplot(aes(x = heure, y = ratioGenre)) +
  geom_area(aes(fill = PCS8), alpha=.15, position = "identity") +
  geom_line(aes(color = PCS8), size=1.2) +
  scale_fill_manual (values = pal_PCS8[c(3,5,6)], guide="none") +
  scale_color_manual(values = pal_PCS8[c(3,5,6)], name = "PCS individuelle") +
  ylab("rapport présence des femmes / présence des hommes") +
  labs(title=ml("Présence des travailleur⋅ses (un jour où elles/ils se rendent au travail)",
                "sur leur lieu de travail en fonction de l'heure et de leur genre"),
       caption = src_fig(bu = T, date = "2022")) +
  theme_bw() %>% print()

remove(activites) # la table activités est hyper lourde

off()

# Part travailleur⋅ses au travail ====

rapport("Analyse des horaires", prim = T)

# Plusieurs approches : masse salariale dans l'ensemble, ou bien part de salariés concernés.

# Commençons par la masse salariale

# 1) quelle part de la main d'oeuvre est au travail ?


# activites = left_join(activites, select(PER, uid_PER, typoJo), by="uid_PER")

# activites %>%
#   filter(Activ %in% c("10", "11")) %>%
#   group_by(heure) %>%
#   summarise(nTrav = sum(ifelse(substr(Tache,1,2) == "10", 1, 0)),
#             nDepl = sum(ifelse(substr(Tache,1,2) == "99", 1, 0)),
#             nActi = sum(ifelse(!substr(Tache,1,1) %in% c("0","1","9"), 1, 0)),
#             n = n()) %>%
#   mutate(pTrav = nTrav/n * 100,
#          pDepl = nDepl/n * 100,
#          pActi = nActi/n * 100,
#          pDomi = 100 - pTrav - pDepl - pActi) %>%
#   pivot_longer(cols = starts_with("p"), names_to = "variable", values_to = "p") %>%
#   mutate(variable = plyr::revalue(as.factor(variable), c("pTrav" = "au lieu d'emploi",
#                                                          "pDepl" = "en déplacement",
#                                                          "pActi" = "sur autre l. d'activité",
#                                                          "pDomi" = "au domicile")),
#          variable = factor(variable, levels =
#                              rev(c("au lieu d'emploi", "en déplacement", "sur autre l. d'activité", "au domicile")))) %>%
#   ggplot(aes(x = heure/60, y = p)) + geom_area(aes(fill = variable)) +
#   scale_fill_manual(values = c("snow1", "darkseagreen2", "thistle", "steelblue"), name = "localisation") +
#   scale_x_continuous(breaks = c(4:24)) + xlab("heure du jour") + ylab("part de la population active (%)") +
#   labs(title = "Situation de la population active selon l'heure de la journée",
#        subtitle = "Ensemble des enquêtes", caption = src_fig(activites)) +
#   theme(panel.ontop=TRUE, panel.background = element_rect(fill = NA))

g = activites %>%
  filter(uid_PER %in% PER_ff$uid_PER) |> filter(!is.na(CoeffRecEnq)) |>
  group_by(heure) %>%
  summarise(nTrav = sum(ifelse(substr(Tache,1,2) == "10", CoeffRecEnq, 0)),
            nDepl = sum(ifelse(substr(Tache,1,2) == "99", CoeffRecEnq, 0)),
            nActi = sum(ifelse(!substr(Tache,1,1) %in% c("0","1","9"), CoeffRecEnq, 0)),
            nb = sum(CoeffRecEnq),
            n = n()) %>%
  mutate(pTrav = nTrav/nb * 100,
         pDepl = nDepl/nb * 100,
         pActi = nActi/nb * 100,
         pDomi = 100 - pTrav - pDepl - pActi) %>%
  pivot_longer(cols = starts_with("p"), names_to = "variable", values_to = "p") %>%
  mutate(variable = plyr::revalue(as.factor(variable), c("pTrav" = "au lieu d'emploi",
                                                         "pDepl" = "en déplacement",
                                                         "pActi" = "sur autre l. d'activité",
                                                         "pDomi" = "au domicile")),
         variable = factor(variable, levels =
                             rev(c("au lieu d'emploi", "en déplacement", "sur autre l. d'activité", "au domicile")))) %>%
  ggplot(aes(x = heure/60, y = p)) + geom_area(aes(fill = variable)) +
  scale_fill_manual(values = c("snow1", "darkseagreen2", "thistle", "steelblue"), name = "localisation") +
  scale_x_continuous(breaks = c(2:12)*2) + xlab("heure du jour") + ylab("part de la population active (%)") +
  labs(title = "Situation de la population active selon l'heure de la journée",
       subtitle = "Personnes s'étant déplacées sur lieu d'activité professionnelle\nlors du jour d'enquête", caption = src_fig(bu=T,emp=T)) +
  theme(panel.ontop=TRUE, panel.background = element_rect(fill = NA)) +
  geom_errorbarh(aes(y = 16,  xmin = 4,  xmax = 5,  color = "loi PACTE (2019)"),   size = 2, height = 3, alpha = .6) +
  geom_errorbarh(aes(y = 16, xmin = 24, xmax = 27, color = "loi PACTE (2019)"),    size = 2, height = 3, alpha = .6) +
  geom_errorbarh(aes(y = 22,  xmin = 4,  xmax = 6,  color = "ancienne définition"),size = 2, height = 3, alpha = .6) +
  geom_errorbarh(aes(y = 22, xmin = 21, xmax = 27, color = "ancienne définition"), size = 2, height = 3, alpha = .6) +
  scale_color_manual(values = c("turquoise", "navy"), name = "travail de nuit")

sortie("Horaires/Horaires globaux")
  print(g)
off()

# Figure écarts moyenne EMP

hph_enq = activites %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(uid_ENQ != "EMP2019") %>%
  group_by(uid_ENQ, heure) %>%
  summarise(nTrav = sum(ifelse(substr(Tache,1,2) == "10", CoeffRecEnq, 0)),
            n = sum(CoeffRecEnq)) %>%
  mutate(pTrav = nTrav/n * 100) 

hph_emp = activites %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(uid_ENQ == "EMP2019") %>%
  select(-uid_ENQ) %>%
  group_by(heure) %>%
  summarise(nTravEMP = sum(ifelse(substr(Tache,1,2) == "10", CoeffRecEnq, 0)),
            nEMP = sum(CoeffRecEnq)) %>%
  mutate(pTravEMP = nTravEMP/nEMP * 100)

hph_enq = left_join(hph_enq, hph_emp, by="heure") %>%
  mutate(ratio = pTrav/pTravEMP-1) %>%
  group_by(uid_ENQ) %>% summarise(ecartMoyen = mean(abs(ratio))) %>%
  tab_Tri(parCol = "ecartMoyen", rev=T)

sortie("Horaires/Horaires cas particuliers", taille = "man", l = "17", h = "10")
activites %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(uid_ENQ %in% filter(hph_enq, ecartMoyen>.33)$uid_ENQ) %>%
  group_by(uid_ENQ, heure) %>%
  summarise(nTrav = sum(ifelse(substr(Tache,1,2) == "10", CoeffRecEnq, 0)),
            n = sum(CoeffRecEnq)) %>%
  mutate(pTrav = nTrav/n * 100)  %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  ggplot(aes(x = heure/60, y = pTrav)) +
  geom_line(data = hph_emp, aes(y = pTravEMP, size = "EMP (Hexagone)"), alpha=.4) +
  geom_line(aes(color = Libelle_Long, size = "Enquête locale")) +
  scale_size_manual(values = c(2,.4), name = "Portée de l'enquête") +
  scale_colour_hue(name = "Enquêtes locales") +
  xlab("heure") + ylab("part des travailleur·ses sur leur lieu de travail") +
  scale_x_continuous(breaks = c(2:12)*2) +
  labs(title = "Écarts au profil temporel moyen de l'Hexagone",
       subtitle = "Pondérations prises en compte",
       caption = src_fig(PER_ff))
off()

# Essayer de faire un genre de coeff de Gini avec ça ?
activites %>%
  filter(Activ %in% c("10", "11") & typoJo == "TRAV") %>%
  group_by(heure) %>%
  summarise(nTrav = sum(ifelse(substr(Tache,1,2) == "10", 1, 0)),
            nDepl = sum(ifelse(substr(Tache,1,2) == "99", 1, 0)),
            nActi = sum(ifelse(!substr(Tache,1,1) %in% c("0","1","9"), 1, 0)),
            n = n()) %>%
  mutate(pTrav = nTrav/n * 100,
         pDepl = nDepl/n * 100,
         pActi = nActi/n * 100,
         pDomi = 100 - pTrav - pDepl - pActi) %>%
  mutate(heure = heureMtoHHMM(heure)) %>%
  tab_Tri(parCol = "pTrav", rev=T)

remove(activites)

# Travail de nuit ====

# Etude : part des personnes présentes le soir, la nuit, le matin selon la PCS et la PCS détaillée
# Faisons des colonnes dans ACT, puis comptons qui coche les colonnes

ACT_h = ACT %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(Tache %in% c("101", "102", "103", "104", "105", "106", "109", "810")) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  mutate(soir = ifelse(hFin >= 21*60 & hDeb < 24*60, 1, 0),
         nuit = ifelse(hFin >= 24*60 | hDeb < 5* 60, 1, 0),
         mati = ifelse(hFin >= 5*60  & hDeb < 6* 60, 1, 0)) |>
  group_by(uid_PER) |> summarise(soir = sum(soir) > 0,
                                 nuit = sum(nuit) > 0,
                                 mati = sum(mati) > 0) |>
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by = "uid_PER")

ACT_h %>% group_by(soir, nuit, mati) %>%
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  mutate(p = nb/sum(nb)*100)

ACT_h %>% group_by(nuit) %>%
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  mutate(p = nb/sum(nb)*100)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  group_by(nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  mutate(p = nb/sum(nb)*100)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, Genre), by="uid_PER") |>
  group_by(Genre, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(Genre) |>
  mutate(p = nb/sum(nb)*100)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, PCS8, Genre), by="uid_PER") |>
  group_by(PCS8, Genre, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(PCS8, Genre) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, PCS42S, Genre), by="uid_PER") |>
  group_by(PCS42S, Genre, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(PCS42S, Genre) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

PER_ff |>
  filter(PCS42S %in% c("61", "66", "69"), !is.na(CoeffRecEnq)) |>
  group_by(Genre, PCS42S) |> summarise(nb = sum(CoeffRecEnq)) |>
  group_by(Genre) |> mutate(p = nb / sum(nb))

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, NivEtu), by="uid_PER") |>
  mutate(NivDip = NivEtuVersNivDip(NivEtu),
         NivDip = etqNivDip(NivDip)) |>
  group_by(NivDip, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(NivDip) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, PCSMT), by="uid_PER") |>
  group_by(PCSMT, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(PCSMT) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) |>
  left_join(z_Nomenclature, by = "uid_ENQ") |>
  group_by(Libelle_Long, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(Libelle_Long) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) |>
  filter(substr(uid_ENQ, 1, 3) == "VLN") |>
  left_join(z_Nomenclature, by = "uid_ENQ") |>
  group_by(Libelle_Long, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(Libelle_Long) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) |>
  left_join(select(PER_ff, uid_PER, PCS8), by = "uid_PER") |>
  left_join(z_Nomenclature, by = "uid_ENQ") |>
  group_by(Libelle_Long, PCS8) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(Libelle_Long) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(PCS8 == "06") |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) |>
  left_join(select(PER_ff, uid_PER, PCS8), by = "uid_PER") |>
  filter(PCS8 == "06") |>
  left_join(z_Nomenclature, by = "uid_ENQ") |>
  group_by(Libelle_Long, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(Libelle_Long) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl, PCS8), by = "uid_PER") |>
  filter(PCS8 == "06") |>
  filter(!is.na(dsTvl)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsTvl, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(dsTvl) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl), by = "uid_PER") |>
  filter(!is.na(dsDom), !is.na(dsTvl)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsDom, dsTvl, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(dsDom, dsTvl) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

# répartition des ouvrier⋅es

ACT_h %>%
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl, PCS8), by = "uid_PER") |>
  filter(!is.na(dsDom), !is.na(dsTvl)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsTvl, PCS8) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(dsTvl) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(PCS8 == "06") |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>%
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl, PCS8), by = "uid_PER") |>
  filter(!is.na(dsDom), !is.na(dsTvl)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsDom, dsTvl, PCS8) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(dsDom, dsTvl) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(PCS8 == "06") |>
  tab_Tri(parCol = "p", rev = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl, PCS8), by = "uid_PER") |>
  filter(PCS8 == "06") |>
  filter(!is.na(dsDom), !is.na(dsTvl)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsDom, dsTvl, nuitExt) |>
  summarise(n = n(),
            nb = sum(CoeffRecEnq, na.rm=T), .groups="drop") %>%
  group_by(dsDom, dsTvl) |>
  mutate(p = nb/sum(nb)*100) |>
  filter(nuitExt) |>
  tab_Tri(parCol = "p", rev = T)

echelle_h = scale_fill_manual(values = c("wheat", "lightpink2", "orchid", "darkorchid", "navy", "darkgrey"),
                              name = "schéma horaire")


sortie("Horaires/Horaires de nuit", taille = "carré")
PER_ff %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit), !is.na(PCS42S)) %>%
  mutate(PCS42S = etqPCS42S(PCS42S), PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS42S, soir, nuit, mati) %>% summarise(n = n(), PCS8 = first(PCS8), .groups="drop") %>%
  group_by(Genre, PCS42S) %>% mutate(p = n/sum(n)*100) %>%
  mutate(sch = case_when(soir == 1 & nuit == 1 & mati == 1 ~ "soir + nuit + matin",
                         soir == 1 & nuit == 0 & mati == 0 ~ "soir (21-0h)",
                         soir == 1 & nuit == 1 & mati == 0 ~ "soir + nuit",
                         soir == 0 & nuit == 1 & mati == 0 ~ "nuit (0-5h)",
                         soir == 0 & nuit == 1 & mati == 1 ~ "nuit + matin",
                         soir == 0 & nuit == 0 & mati == 1 ~ "matin (5-6h)")) %>%
  mutate(sch = factor(sch, levels = rev(c("soir + nuit + matin", "soir (21-0h)", "soir + nuit", "nuit (0-5h)", "nuit + matin", "matin (5-6h)")))) %>%
  group_by(Genre, PCS42S, sch) %>% summarise(PCS8 = first(PCS8), n = sum(n)) %>%
  group_by(Genre, PCS42S) %>% mutate(p = n / sum(n) * 100) %>%
  filter(!is.na(sch)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = sch), position = "stack") +
  echelle_h + ylab("part (%)") + xlab("PCS") +
  labs(title = "Présence nocturne au lieu de travail",
       subtitle = "selon la PCS individuelle", caption = src_fig(ACT_h)) +
  facet_grid(PCS8~Genre, scales = "free_y", space = "free_y") + coord_flip() +
  guides(fill = guide_legend(reverse = T))
off()

sortie("Horaires de nuit, par nivDip", taille = "carré")
PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  mutate(NivDip = NivEtuVersNivDip(NivEtu), NivDip = etqNivDip(NivDip)) %>%
  filter(!is.na(NivDip)) %>%
  group_by(NivDip, soir, nuit, mati) %>% summarise(n = n(), .groups="drop") %>%
  group_by(NivDip) %>% mutate(p = n/sum(n)*100) %>%
  mutate(sch = case_when(soir == 0 & nuit == 0 & mati == 0 ~ "typique",
                         soir == 1 & nuit == 0 & mati == 0 ~ "soir",
                         soir == 1 & nuit == 1 & mati == 0 ~ "soir + nuit",
                         soir == 0 & nuit == 1 & mati == 0 ~ "nuit",
                         soir == 0 & nuit == 1 & mati == 1 ~ "nuit + matin",
                         soir == 0 & nuit == 0 & mati == 1 ~ "matin")) %>%
  mutate(sch = factor(sch, levels = rev(c("typique", "soir", "soir + nuit", "nuit", "nuit + matin", "matin")))) %>%
  group_by(NivDip, sch) %>% summarise(n = sum(n)) %>%
  group_by(NivDip) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = NivDip, y = p)) + geom_col(aes(fill = sch), position = "stack") +
  coord_flip() + echelle_h + ylab("part (%)") + xlab("niveau de diplôme") +
  labs(title = "Horaires atypiques selon le niveau de diplôme", caption = src_fig(ACT_h))
off()

sortie("Horaires de nuit, par PCSM", taille = "carré")
PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  filter(!is.na(PCSMT) & PCSMT != "7e" & PCSMT != "7i") %>%
  mutate(PCSMT = etqPCSM(PCSMT)) %>%
  group_by(PCSMT, soir, nuit, mati) %>% summarise(n = n(), .groups="drop") %>%
  group_by(PCSMT) %>% mutate(p = n/sum(n)*100) %>%
  mutate(sch = case_when(soir == 0 & nuit == 0 & mati == 0 ~ "typique",
                         soir == 1 & nuit == 0 & mati == 0 ~ "soir",
                         soir == 1 & nuit == 1 & mati == 0 ~ "soir + nuit",
                         soir == 0 & nuit == 1 & mati == 0 ~ "nuit",
                         soir == 0 & nuit == 1 & mati == 1 ~ "nuit + matin",
                         soir == 0 & nuit == 0 & mati == 1 ~ "matin")) %>%
  mutate(sch = factor(sch, levels = rev(c("typique", "soir", "soir + nuit", "nuit", "nuit + matin", "matin")))) %>%
  group_by(PCSMT, sch) %>% summarise(n = sum(n)) %>%
  group_by(PCSMT) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = PCSMT, y = p)) + geom_col(aes(fill = sch), position = "stack") +
  coord_flip() + echelle_h + ylab("part (%)") + xlab("PCS Ménage") +
  labs(title = "Horaires atypiques selon la PCS Ménage", caption = src_fig(ACT_h))
off()

# ACT_h = ACT %>%
#   filter(Tache %in% c("101", "102", "103", "104", "105", "106", "109", "810")) %>%
#   mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
#   mutate(nuit = ifelse(hFin >= 21*60 | hDeb < 6* 60, 1, 0))

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(Genre, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100)

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(Genre, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100)

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(Genre, PCS8, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100) %>%
  filter(nuit == 1) %>% View()

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  mutate(NivDip = NivEtuVersNivDip(NivEtu), NivDip = etqNivDip(NivDip)) %>%
  group_by(NivDip, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100) %>%
  filter(nuit == 1) %>% View()

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  mutate(PCSMT = etqPCSM(PCSMT)) %>%
  group_by(PCSMT, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100) %>%
  filter(nuit == 1) %>% View()

PER %>%
  left_join(ACT_h, by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(ZoneDens_duo, nuit) %>% summarise(n = n(), .groups="drop_last") %>% mutate(p = n/sum(n)*100) %>%
  filter(nuit == 1) %>% View()

PER %>%
  left_join(select(ACT_h, -CoeffEnq), by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(uid_ENQ, nuit) %>%
  summarise(n = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(p = n/sum(n)*100) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  filter(nuit == 1) %>% View()

PER %>%
  filter(PCS8 == "06") %>%
  left_join(select(ACT_h, -CoeffEnq), by="uid_PER") %>%
  filter(!is.na(nuit)) %>%
  group_by(uid_ENQ, nuit) %>%
  summarise(n = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(p = n/sum(n)*100) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  filter(nuit == 1) %>% View()

PER %>% filter(DuTvl>0) %>%
  group_by(uid_ENQ, PCS8) %>%
  summarise(n = sum(CoeffEnq)) %>% mutate(p = n/sum(n)*100) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  filter(PCS8 == "06") %>% View()

# (sept24) il faut un modèle...
qRes = paste0(c("1er", "2e", "3e", "4e"), " quartile")
qTvl = paste0(c("1er", "2e", "3e", "4e"), " quartile")

g = ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, dsDom, dsTvl, Genre, PCS8, Age10), by = "uid_PER") |>
  valref() |>
  filter(!is.na(dsDom), !is.na(dsTvl), !is.na(Genre), !is.na(PCS8), !is.na(Age10)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  mutate(dsDom = factor(dsDom, labels = qRes),
         dsTvl = factor(dsTvl, labels = qTvl)) |>
 logit(val = "nuitExt", formule = "dsDom + dsTvl + Genre + PCS8 + Age10",
       titre = "Modèle logit : probabilité du travail de nuit",
       caption = src_fig(emp=F), valIntervalleSur100 = 1.5, petit = T)

ACT_h %>% mutate(nuitExt = nuit | soir | mati) |>
  left_join(select(PER_ff, uid_PER, LogOcc, dsTvl, Genre, PCS8, Age10), by = "uid_PER") |>
  valref() |>
  filter(!is.na(LogOcc), !is.na(dsTvl), !is.na(Genre), !is.na(PCS8), !is.na(Age10)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  mutate(dsTvl = factor(dsTvl, labels = qTvl)) |>
  logit(val = "nuitExt", formule = "LogOcc + dsTvl + Genre + PCS8 + Age10",
        titre = "Modèle logit : probabilité du travail de nuit",
        caption = src_fig(PER_ff), valIntervalleSur100 = 1.5, petit = T)

sortie("Horaires/Modèle travail de nuit", portrait = T)
print(g)
off()

# r&d : temps passé en déplacement par les gens qui travaillent de nuit

sortie("Horaires de nuit, déplacements")
activites %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(ACT_h, uid_PER, nuit), by="uid_PER") %>%
  group_by(nuit, heure) %>%
  summarise(nDepl = sum(ifelse(substr(Tache,1,2) == "99", 1, 0)),
            n = n()) %>%
  mutate(pDepl = nDepl/n * 100) %>%
  ggplot(aes(x = heure/60, y = pDepl)) + geom_area(fill = "rosybrown") +
  scale_fill_manual(values = c("snow1", "darkseagreen2", "thistle", "steelblue"), name = "localisation") +
  scale_x_continuous(breaks = c(2:12)*2) + xlab("heure du jour") + ylab("part de la population active (%)") +
  labs(title = "Situation de la population active selon l'heure de la journée",
       subtitle = "Personnes s'étant déplacées sur lieu d'activité professionnelle\nlors du jour d'enquête", caption = src_fig(activites)) +
  theme(panel.ontop=TRUE, panel.background = element_rect(fill = NA)) +
  facet_wrap(~nuit)
off()

# Fragmentation des horaires ====

# r&d : comment faire pour identifier la fragmentation ?
# possibilité : reformater ACT, créer des activités interstitielles virtuelles, les mesurer

interstices = ACT %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  left_join(select(PER_ff, uid_PER, JoTvDeb, JoTvFin), by="uid_PER") %>%
  mutate(JoTvDeb = JoTvDeb * 60, JoTvFin = JoTvFin * 60) %>%
  mutate(travail = ifelse(Tache %in% c("101", "102", "103", "104", "105", "106", "109", "810"), T, F),
         interstice = ifelse(hDeb > JoTvDeb & hFin < JoTvFin & travail == F, T, F)) %>%
  filter(interstice, du >= 180) %>%
  # On ne veut pas compter les journées intercalaires des travailleur·ses de nuit...
  filter(JoTvDeb != 240 & JoTvFin != 1680)

PER_ff$interstice = PER_ff$uid_PER %in% interstices$uid_PER

PER_ff %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>% group_by(interstice) %>% summarise (n = n()) %>% mutate (p = n / sum(n) * 100)

PER_ff %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>%
  group_by(Genre, interstice) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(interstice)

PER_ff %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>%
  group_by(PCS8, interstice) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(interstice)

sortie("Horaires/Horaires fragmentés par nivDip", taille = "man", h = 7, l = 17)
PER_ff %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>%
  mutate(NivDip = NivEtuVersNivDip(NivEtu), NivDip = etqNivDip(NivDip)) %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(NivDip)) %>%
  group_by(Genre, NivDip, interstice) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(interstice) %>%
  ggplot(aes(x = NivDip, y = p)) + geom_col() + facet_wrap(~Genre) + coord_flip() +
  ylab("part (%)") + xlab("niveau de diplôme") +
  labs(title="Part des travailleur·ses concerné·es par un interstice\nde plus de 3h lors de leur journée de travail",
       subtitle="Hors travailleur·ses de nuit",
       caption = src_fig(PER_ff))
off()

sortie("Horaires/Horaires fragmentés par PCS42S")
PER %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(PCS42S)) %>%
  group_by(Genre, PCS42S, interstice) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(interstice) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col() + facet_wrap(~Genre) + coord_flip() +
  ylab("part (%)") + xlab("PCS42S") +
  labs(title="Part des travailleur·ses concerné·es par un interstice\nde plus de 3h lors de leur journée de travail",
       subtitle="Hors travailleur·ses de nuit",
       caption = src_fig(PER))
off()

# → on va en faire indicateur d'atypicité
#   horaires atypiques = personne dans ACT_h et/ou dans Interstices

PER_ff = mutate(PER_ff, hoAtypiques = case_when(uid_PER %in% filter(ACT_h, soir | nuit | mati)$uid_PER |
                                            uid_PER %in% interstices$uid_PER ~ T,
                                          uid_PER %in% ACT_h$uid_PER ~ F))

nrow(filter(PER_ff, hoAtypiques)) / nrow(filter(PER_ff, !hoAtypiques))

PER_ff %>% filter(DuTvl > 0) %>% group_by(hoAtypiques) %>% summarise (n = n()) %>% mutate (p = n / sum(n) * 100)

PER_ff %>% filter(DuTvl > 0) %>%
  group_by(Genre, hoAtypiques) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(hoAtypiques)

PER_ff %>% filter(DuTvl > 0 & JoTvDeb != 240 & JoTvFin != 1680) %>%
  group_by(PCS8, hoAtypiques) %>%
  summarise (n = n(), .groups = "drop_last") %>% mutate (p = n / sum(n) * 100) %>%
  filter(hoAtypiques)

# Décalage horaire social ====

PER_ff %>%
  pivot_longer(cols = c("JoTvDeb", "JoTvFin"), names_to = "nature", values_to = "x") %>%
  ggplot(aes(x = x/60)) + geom_density(aes(colour = nature))

# amusant en vrai comment les bornes sont souvent des heures rondes
sortie("Horaires/Horaires, valeurs rondes")
PER_ff %>%
  mutate(JoTvDeb_rond = trunc(JoTvDeb/60)*60, JoTvFin_rond = trunc(JoTvDeb/60)*60) %>%
  mutate(rondDeb = (round(JoTvDeb) == JoTvDeb_rond),
         rondFin = (round(JoTvFin) == JoTvFin_rond)) %>%
  group_by(tout = T) %>%
  summarise(nDeb = sum(ifelse(rondDeb, 1, 0), na.rm=T),
            nFin = sum(ifelse(rondFin, 1, 0), na.rm=T),
            n = n()) %>%
  mutate(pDeb = nDeb / n, pFin = nFin / n) %>% print()
off()

# Chiffres

# Plan général
# On va mesurer par quart d'heure

heureMinToHr(weighted.quantile((PER_ff$JoTvDeb), probs = c(.25, .5, .75), w = PER_ff$CoeffRecEnq, na.rm=T) * 60)
heureMinToHr(weighted.quantile((PER_ff$JoTvFin), probs = c(.25, .5, .75), w = PER_ff$CoeffRecEnq, na.rm=T) * 60)

heureMinToHr(weighted.quantile((heureHHMMtoM(PER_ff$JoFin)), probs = c(.25, .5, .75), w = PER_ff$CoeffRecEnq, na.rm=T))

# PER_ff %>%
#   pivot_longer(cols = c("JoDeb", "JoFin"), names_to = "nature", values_to = "x") %>%
#   ggplot(aes(x = x/60)) + geom_density(aes(colour = nature))

tabDeb = PER_ff %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFin = PER_ff %>% 
  mutate(joFin = round((JoTvFin*60)/15)*15) %>%
  filter(joFin < 27*60) %>%
  group_by(joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

left_join(tabDeb, tabFin, by="h") %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) + geom_col(aes(fill = variable))

moyDeb = weighted.mean((filter(PER_ff, !is.na(CoeffRecEnq))$JoTvDeb),
                       w = filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq, na.rm=T)
moyFin = weighted.mean((filter(PER_ff, !is.na(CoeffRecEnq))$JoTvFin),
                       w = filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq, na.rm=T)

xDeb = tabDeb$h[which(tabDeb$h>moyDeb*60)[1]]
if (xDeb - (moyDeb*60) > 7) { xDeb = tabDeb$h[which(tabDeb$h>moyDeb*60)[1] - 1] }

xFin = tabFin$h[which(tabFin$h>moyFin*60)[1]]
if (xFin - (moyFin*60) > 7) { xFin = tabFin$h[which(tabFin$h>moyFin*60)-1][1] }

yDeb = tabDeb[tabDeb$h == xDeb,]$pDeb
yFin = tabFin[tabFin$h == xFin,]$pFin

labsDebFin = c("d'arrivée au\nlieu d'emploi", "de départ du\nlieu d'emploi")

sortie("Horaires/Horaires deb et fin", taille = "man", h = 10, l = 17)
left_join(tabDeb, tabFin, by="h") %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h/60, y = p)) +
  geom_area(aes(fill = variable), alpha=.25, position = "identity") +
  geom_line(aes(colour = variable)) +
  scale_color_manual(values = c("darkorange3", "darkorchid"), name = "heure",labels = labsDebFin) +
  scale_fill_manual(values = c("darkorange3", "darkorchid"), name = "heure", labels = labsDebFin) +
  scale_x_continuous(breaks = c(2:12)*2) + scale_y_continuous(breaks=c(0:12)) +
  annotate("point", x = xDeb/60, y = yDeb, color = "darkorange3") +
  annotate("label",  x = xDeb/60, y = yDeb,
           label = paste0("Moyenne : ", heureMinToHr(moyDeb*60)), hjust=-.1,
           color = "darkorange3") +
  annotate("point", x = xFin/60, y = yFin, color = "darkorchid") +
  annotate("label",  x = xFin/60, y = yFin,
           label = paste0("Moyenne : ", heureMinToHr(moyFin*60)), hjust=-.1,
           color = "darkorchid") +
  ylab("part des travailleur·ses (%) par quart d'heure") + xlab("heure de la journée") +
  labs(title = "Répartition des heures d'arrivée et de départ au lieu d'emploi",
       subtitle = "Journées des travailleur⋅ses s'étant déplacé⋅es sur un lieu de travail le jour d'enquête",
       caption = src_fig(PER_ff))
off()

tabDebGenre = PER_ff %>%
  mutate(joDeb = round(heureHHMMtoM(JoDeb)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(Genre, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinGenre = PER_ff %>% 
  mutate(joFin = round(heureHHMMtoM(JoFin)/15)*15) %>%
  filter(joFin < 27*60) %>%
  group_by(Genre, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

sortie("Horaires/Horaires selon genre")
left_join(tabDebGenre, tabFinGenre, by=c("Genre", "h")) %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h/60, y = p)) +
  geom_line(aes(colour = variable, linetype=Genre)) +
  scale_color_manual(values = c("darkorange3", "darkorchid"), name = "heure",labels = labsDebFin) +
  scale_x_continuous(breaks = c(2:12)*2) + scale_y_continuous(breaks=c(0:12)) +
  scale_linetype(labels=c("femmes", "hommes")) +
  ylab("part des travailleur·ses (%) par quart d'heure") + xlab("heure de la journée") +
  labs(title = "Répartition des heures d'arrivée et de départ au lieu d'emploi",
       subtitle = "Journées à temps plein complètes uniquement",
       caption = src_fig(PER_ff))
off()

heureMinToHr(weighted.mean(filter(PER_ff, !is.na(CoeffRecEnq), Genre == "H")$JoTvDeb*60,
                           w = filter(PER_ff, !is.na(CoeffRecEnq), Genre == "H")$CoeffRecEnq, na.rm=T))
heureMinToHr(weighted.mean(filter(PER_ff, !is.na(CoeffRecEnq), Genre == "F")$JoTvDeb*60,
                           w = filter(PER_ff, !is.na(CoeffRecEnq), Genre == "F")$CoeffRecEnq, na.rm=T))

heureMinToHr(weighted.mean(filter(PER_ff, !is.na(CoeffRecEnq), Genre == "H")$JoTvFin*60,
                           w = filter(PER_ff, !is.na(CoeffRecEnq), Genre == "H")$CoeffRecEnq, na.rm=T))
heureMinToHr(weighted.mean(filter(PER_ff, !is.na(CoeffRecEnq), Genre == "F")$JoTvFin*60,
                           w = filter(PER_ff, !is.na(CoeffRecEnq), Genre == "F")$CoeffRecEnq, na.rm=T))

PER_ff |>
  filter(!is.na(CoeffRecEnq), Activ == "10") |>
  group_by(PCS8) |>
  summarise(JoTvDeb = heureMinToHr(weighted.mean(JoTvDeb, CoeffRecEnq, na.rm=T) * 60),
            JoTvFin = heureMinToHr(weighted.mean(JoTvFin, CoeffRecEnq, na.rm=T) * 60))

tabDebPCS = PER_ff %>% filter(Activ == "10") %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(PCS8, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinPCS = PER_ff %>% filter(Activ == "10") %>%
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(PCS8, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

g1 = tabDebPCS %>% filter(PCS8 %in% c("02", "03", "06")) %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(data = tabDeb, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = PCS8)) +
  scale_color_manual(values = c(pal_PCS8[c(2,3,6)], "gray"), name = "PCS") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) + scale_y_continuous(breaks=c(0:15)) +
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Répartition des heures d'arrivée et de départ au lieu d'emploi",
                  "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es"),
       subtitle = "Heure d'arrivée au lieu de travail (arrondie au quart d'heure)")

tabFinH = PER_ff %>% filter(Activ == "10") %>% 
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

g2 = tabFinPCS %>% filter(PCS8 %in% c("02", "03", "06")) %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(data = tabFinH, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = PCS8)) +
  scale_color_manual(values = c(pal_PCS8[c(2,3,6)], "gray"), name = "PCS",
                     labels = c("indépendant·es", "cadres", "ouvrier·es", "ensemble"), guide="none") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) +
  ylab("part des travailleur·ses (%) par heure") + xlab("heure de la journée") +
  labs(subtitle = "Heure de départ du lieu de travail (arrondie à l'heure)",
       caption = src_fig(PER_ff))

sortie("Horaires/Horaires planche PCS Distrib", taille = "man", h = 22, l = 17)
cowplot::plot_grid(g1 + xlab(NULL), g2, nrow=2, align="v", axis="lr", labels=c("a","b"))
off()

heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "1", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "4", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))

heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "1", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "4", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))

heureMinToHr(mean(filter(PER, PCS8 == "06", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, PCS8 == "03", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))

heureMinToHr(mean(filter(PER, PCS8 == "06", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(PER, PCS8 == "03", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))

nrow(filter(PER, PCS8 == "03", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvDeb < 7.5 & JoTvFin < 27)) /
  nrow(filter(PER, PCS8 == "03", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27))

nrow(filter(PER, PCS8 == "06", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvDeb < 7.5 & JoTvFin < 27)) /
  nrow(filter(PER, PCS8 == "06", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27))

heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "1", PCS8 == "05", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "4", PCS8 == "05", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvDeb*60, na.rm=T))

heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "1", PCS8 == "05", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(mutate(PER, nivDip = NivEtuVersNivDip(NivEtu)), nivDip == "4", PCS8 == "05", DuTvl>0, Activ == "10", JoTvDeb > 4 & JoTvFin < 27)$JoTvFin*60, na.rm=T))

g1 = PER_ff %>%
  filter(Activ == "10", !is.na(CoeffRecEnq), !is.na(PCS42S)) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T)) %>%
  group_by(PCS42S) %>%
  summarise(joTvDeb = weighted.median(JoTvDeb * 60, w=CoeffRecEnq, na.rm=T),
            joTvFin = weighted.median(JoTvFin * 60, w=CoeffRecEnq, na.rm=T),
            joTvDeb_q1 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvDeb_q3 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.75, na.rm=T),
            joTvFin_q1 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvFin_q3 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.75, na.rm=T),
            PCS8 = mode(PCS8)) %>%
  tab_Tri(i = "PCS42S", parCol = "joTvDeb", rev = T) %>%
  pivot_longer(cols = c("joTvDeb", "joTvFin"), names_to = "variable", values_to = "x") %>%
  ggplot(aes(x = x/60, y = PCS42S)) +
  geom_errorbarh(aes(xmin = joTvDeb_q1/60, xmax = joTvDeb_q3/60, color = PCS8,
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_errorbarh(aes(xmin = joTvFin_q1/60, xmax = joTvFin_q3/60, color = PCS8,
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_point(aes(shape = variable, color=PCS8)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "PCS", labels = niv_PCS8) +
  scale_x_continuous(breaks = c(3:10)*2, limits=c(6,20)) +
  scale_linetype(name = NULL) +
  xlab(NULL) + ylab("PCS détaillée") +
  scale_shape_manual(values = c(16, 15),
                     name = "Heure médiane",
                     labels=c("de début de journée", "de fin de journée")) +
  labs(title = "Heure de début et de fin de présence au lieu d'emploi",
       subtitle = "En fonction de la PCS détaillée de l'enquêté·e\n(si disponible)")

g2 = PER_ff %>%
  filter(Activ == "10", !is.na(CoeffRecEnq)) %>%
  mutate(NivDip = etqNivDip(NivEtuVersNivDip(NivEtu))) %>%
  filter(!NivDip == "Études en cours", !is.na(NivDip)) %>%
  group_by(NivDip) %>%
  summarise(joTvDeb = weighted.median(JoTvDeb * 60, w=CoeffRecEnq, na.rm=T),
            joTvFin = weighted.median(JoTvFin * 60, w=CoeffRecEnq, na.rm=T),
            joTvDeb_q1 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvDeb_q3 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.75, na.rm=T),
            joTvFin_q1 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvFin_q3 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.75, na.rm=T)) %>%
  tab_Tri(i = "NivDip", parCol = "joTvDeb", rev = T) %>%
  pivot_longer(cols = c("joTvDeb", "joTvFin"), names_to = "variable", values_to = "x") %>%
  ggplot(aes(x = x/60, y = NivDip)) +
  geom_errorbarh(aes(xmin = joTvDeb_q1/60, xmax = joTvDeb_q3/60, 
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_errorbarh(aes(xmin = joTvFin_q1/60, xmax = joTvFin_q3/60, 
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_point(aes(shape = variable)) +
  scale_x_continuous(breaks = c(3:10)*2, limits=c(6,20)) +
  scale_linetype(guide="none") +
  xlab("heure") + ylab("Niveau de diplôme") +
  scale_shape_manual(values = c(16, 15), guide="none") +
  labs(subtitle = "En fonction du niveau de diplôme\n(même échantillon, quand études terminées)", caption=src_fig())

sortie("Horaires/Horaires planche PCS", taille = "man", h = 22, l = 17)
cowplot::plot_grid(g1, g2, nrow=2, align="v", axis="lr", rel_heights=c(.7,.3), labels=c("a","b"))
off()

g2_05 = PER_ff %>%
  filter(Activ == "10", !is.na(CoeffRecEnq), PCS8 == "05") %>%
  mutate(NivDip = etqNivDip(NivEtuVersNivDip(NivEtu))) %>%
  filter(!NivDip == "Études en cours", !is.na(NivDip)) %>%
  group_by(NivDip) %>%
  summarise(joTvDeb = weighted.median(JoTvDeb * 60, w=CoeffRecEnq, na.rm=T),
            joTvFin = weighted.median(JoTvFin * 60, w=CoeffRecEnq, na.rm=T),
            joTvDeb_q1 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvDeb_q3 = weighted.quantile(JoTvDeb*60, w=CoeffRecEnq, probs=.75, na.rm=T),
            joTvFin_q1 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.25, na.rm=T),
            joTvFin_q3 = weighted.quantile(JoTvFin*60, w=CoeffRecEnq, probs=.75, na.rm=T)) %>%
  tab_Tri(i = "NivDip", parCol = "joTvDeb", rev = T) %>%
  pivot_longer(cols = c("joTvDeb", "joTvFin"), names_to = "variable", values_to = "x") %>%
  ggplot(aes(x = x/60, y = NivDip)) +
  geom_errorbarh(aes(xmin = joTvDeb_q1/60, xmax = joTvDeb_q3/60, 
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_errorbarh(aes(xmin = joTvFin_q1/60, xmax = joTvFin_q3/60, 
                     linetype = "intervalle\ninterquartile"), size = .5, height=.2, alpha = 1) +
  geom_point(aes(shape = variable)) +
  scale_x_continuous(breaks = c(3:10)*2, limits=c(6,20)) +
  scale_linetype(guide="none") +
  xlab("heure") + ylab("Niveau de diplôme") +
  scale_shape_manual(values = c(16, 15), guide="none") +
  labs(subtitle = "Employé⋅es uniquement\nEn fonction du niveau de diplôme\n(même échantillon, quand études terminées)", caption=src_fig())





g3 = PER %>%
  filter(DuTvl > 0 & Activ == "10" & PCS42S %in% as.character(c(10:69))) %>%
  mutate(PCS8 = etqPCS8(PCS8, rev = T)) %>%
  group_by(PCS8) %>% summarise(joTvDeb = median(JoTvDeb * 60, na.rm=T),
                               joTvFin = median(JoTvFin * 60, na.rm=T),
                               joTvDeb_q1 = quantile(JoTvDeb*60, probs=.25, na.rm=T),
                               joTvDeb_q3 = quantile(JoTvDeb*60, probs=.75, na.rm=T),
                               joTvFin_q1 = quantile(JoTvFin*60, probs=.25, na.rm=T),
                               joTvFin_q3 = quantile(JoTvFin*60, probs=.75, na.rm=T)) %>%
  tab_Tri(i = "PCS8", parCol = "joTvDeb", rev = T) %>%
  pivot_longer(cols = c("joTvDeb", "joTvFin"), names_to = "variable", values_to = "x") %>%
  ggplot(aes(x = x/60, y = PCS8)) +
  geom_errorbarh(aes(xmin = joTvDeb_q1/60, xmax = joTvDeb_q3/60, color = PCS8,
                     linetype = "intervalle\ninterquartile"), size = 1.5, height=.2, alpha = .2) +
  geom_errorbarh(aes(xmin = joTvFin_q1/60, xmax = joTvFin_q3/60, color = PCS8,
                     linetype = "intervalle\ninterquartile"), size = 1.5, height=.2, alpha = .2) +
  geom_point(aes(shape = variable, color=PCS8)) +
  scale_color_manual(values = rev(pal_PCS8[c(6,1,5,4,2,3)]), name = "PCS", guide = "none") +
  scale_x_continuous(breaks = c(3:10)*2, limits=c(6,20)) +
  scale_linetype(name = NULL) +
  xlab(NULL) + ylab("PCS") +
  scale_shape(name = "Heure médiane", labels=c("de début de journée", "de fin de journée")) +
  labs(title = "Heure de début et de fin de présence au lieu d'emploi",
       subtitle = "En fonction de la PCS détaillée de l'enquêté·e\n(si disponible)")

sortie("Horaires/Horaires planche PCS mini", taille = "carré", portrait = T)
cowplot::plot_grid(g3, g2, nrow=2, align="v", axis="lr", labels=c("a","b"))
off()

tabDebDip = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(nivDip = NivEtuVersNivDip(NivEtu)) %>%
  mutate(joDeb = round((JoTvDeb*60)/60)*60) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(nivDip, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinDip = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(nivDip = NivEtuVersNivDip(NivEtu)) %>%
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(nivDip, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

sortie("Horaires/Heure d'arrivée lieu de travail, par nivDip")
tabDebDip %>% filter(nivDip %in% c("0","1","2","3","4")) %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(aes(colour = nivDip)) +
  scale_color_hue(name = "niveau de diplôme") +
  scale_x_continuous(breaks = c(2:12)*2) + 
  ylab("part des travailleur·ses (%) par quart d'heure") + xlab("heure de la journée") +
  labs(title = "Répartition des heures d'arrivée et de départ au lieu d'emploi",
       subtitle = "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es",
       caption = src_fig(PER))
off()

sortie("Horaires/Heure de départ lieu de travail, par nivDip")
tabFinDip %>% filter(nivDip %in% c("0","1","2","3","4")) %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(aes(colour = nivDip)) +
  scale_color_hue(name = "niveau de diplôme") +
  scale_x_continuous(breaks = c(2:12)*2) + 
  ylab("part des travailleur·ses (%) par quart d'heure") + xlab("heure de la journée") +
  labs(title = "Répartition des heures d'arrivée et de départ au lieu d'emploi",
       subtitle = "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es",
       caption = src_fig(PER))
off()

filter(tabDebDip, h<=14*60) %>% 
  full_join(filter(tabFinDip, h>=14*60), by=c("h", "nivDip")) %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  filter(nivDip %in% c("0","1","2","3","4")) %>%
  ggplot(aes(x = h/60, y = p)) +
  geom_line(aes(colour = nivDip)) +
  scale_color_hue(name = "niveau de diplôme") +
  scale_x_continuous(breaks=c(2:12)*2) + scale_y_continuous(breaks=c(0:15)) +
  ylab("part des travailleur·ses (%) par quart d'heure") + xlab("heure de la journée") +
  labs(title = "Répartition des heures d'arrivée et de départ au lieu d'emploi",
       subtitle = "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es",
       caption = src_fig(PER)) +
  facet_grid(~variable, space = "free", scales = "free")

PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsTvl) |>
  summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq, na.rm = T),
            JoTvFin = weighted.mean(JoTvFin, w = CoeffRecEnq, na.rm = T)) |>
  mutate(JoTvDeb = heureMinToHr(JoTvDeb * 60), JoTvFin = heureMinToHr(JoTvFin * 60))

heureMinToHr(mean(filter(PER, ZoneDens == "1", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens == "3", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens == "1", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens == "3", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))

heureMinToHr(mean(filter(PER, ZoneDens_travMax == "1", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens_travMax == "3", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens_travMax == "1", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneDens_travMax == "3", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))

heureMinToHr(mean(filter(PER, ZoneRang == "1", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneRang == "4", DuTvl>0, Activ == "10")$JoTvDeb*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneRang == "1", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))
heureMinToHr(mean(filter(PER, ZoneRang == "4", DuTvl>0, Activ == "10")$JoTvFin*60, na.rm=T))

tabDebZoneD = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(ZoneDens, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinZoneD = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(ZoneDens, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

sortie("Horaires/Heure d'arrivée lieu travail, selon commune résidence")
tabDebZoneD %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(data = tabDeb, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneDens)) +
  scale_color_manual(values = c(pal_ZoneDens, "gray"), name = "Type de commune rés.") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) + scale_y_continuous(breaks=c(0:15)) +
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Répartition des heures d'arrivée et de départ au lieu d'emploi",
                  "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es"),
       subtitle = "Heure d'arrivée au lieu de travail (arrondie au quart d'heure)")
off()

sortie("Horaires/Heure de départ lieu travail, selon commune résidence")
tabFinZoneD %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(data = tabFinH, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneDens)) +
  scale_color_manual(values = c(pal_ZoneDens, "gray"), name = "Type de commune rés.") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) +
  ylab("part des travailleur·ses (%) par heure") + xlab("heure de la journée") +
  labs(subtitle = "Heure de départ du lieu de travail (arrondie à l'heure)",
       caption = src_fig(PER))

tabDebZoneD_PCS = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(PCS8, ZoneDens, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinZoneD_PCS = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(PCS8, ZoneDens, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

sortie("Horaires/Heure d'arrivée lieu d'emploi par PCS8")
tabDebZoneD_PCS %>% filter(PCS8 %in% c("03","04","05","06")) %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(data = filter(tabDebPCS, PCS8 %in% c("03","04","05","06")), aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneDens)) +
  scale_color_manual(values = c(pal_ZoneDens, "gray"), name = "Type de commune rés.") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) + 
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Répartition des heures d'arrivée et de départ au lieu d'emploi",
                  "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es"),
       subtitle = "Heure d'arrivée au lieu de travail (arrondie au quart d'heure)") +
  facet_wrap(~PCS8)
off()
sortie("Horaires/Heure de départ lieu d'emploi par PCS8")
tabFinZoneD_PCS %>% filter(PCS8 %in% c("03","04","05","06")) %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(data = filter(tabFinPCS, PCS8 %in% c("03","04","05","06")), aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneDens)) +
  scale_color_manual(values = c(pal_ZoneDens, "gray"), name = "Type de commune rés.") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) +
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Répartition des heures d'arrivée et de départ au lieu d'emploi",
                  "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es"),
       subtitle = "Heure de départ du de travail (arrondie au quart d'heure)") +
  facet_wrap(~PCS8)
off()

nrow(filter(PER, DuTvl>0, Activ == "10", ZoneDens_travMax == "4")) / nrow(filter(PER, DuTvl>0, Activ == "10"))

tabDebZoneD_PCS = PER_ff %>% filter(Activ == "10") %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15,
         dsTvl = discretisation(dsTvl, methode = "quartiles")) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(PCS8, dsTvl, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinZoneD_PCS = PER_ff %>% filter(Activ == "10") %>%
  mutate(joFin = round((JoTvFin*60)/60)*60,
         dsTvl = discretisation(dsTvl, methode = "quartiles")) %>%
  filter(joFin < 27*60) %>%
  group_by(PCS8, dsTvl, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

g1 = tabDebZoneD_PCS %>% filter(PCS8 %in% c("03","04","05","06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  filter(!is.na(dsTvl)) %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(aes(colour = dsTvl)) +
  scale_color_manual(values = rev(pal_ZoneDens[c(1:4)]),
                     name = "Quartile de densité\ndu secteur de travail",
                     labels = paste0(c("1er", "2e", "3e", "4e"),
                                     "  quartile\n",
                                     levels(tabDebZoneD_PCS$dsTvl),
                                    " hab/km²")) +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) + 
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Distribution des heures d'arrivée et de départ au lieu d'emploi",
                  "selon la PCS et le type de secteur de travail"),
       subtitle = "Heure d'arrivée au lieu de travail (arrondie au quart d'heure)") +
  facet_wrap(~PCS8)
g2 = tabFinZoneD_PCS %>% filter(PCS8 %in% c("03","04","05","06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  filter(!is.na(dsTvl)) %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(aes(colour = dsTvl)) +
  scale_color_manual(values = rev(pal_ZoneDens[c(1:4)]),
                     name = "Quartile de densité\ndu secteur de travail",
                     labels = paste0(c("1er", "2e", "3e", "4e"),
                                     "  quartile\n",
                                     levels(tabDebZoneD_PCS$dsTvl),
                                     " hab/km²"), guide = "none") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) +
  ylab("part des travailleur·ses (%) par heure") + xlab("heure de la journée") +
  labs(subtitle = "Heure de départ du lieu de travail (arrondie à l'heure)",
       caption = src_fig(emp = F)) +
  facet_wrap(~PCS8)

sortie("Horaires/Horaires planche PCS et ZoneDens", taille = "page", portrait = T)
cowplot::plot_grid(g1, g2, nrow=2, align="v", axis="lr", labels=c("a","b"))
off()

tabDebZoneR = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joDeb = round((JoTvDeb*60)/15)*15) %>%
  filter(joDeb > 4*60 & joDeb < 27*60) %>%
  group_by(ZoneRang, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFinZoneR = PER %>% filter(DuTvl>0, Activ == "10") %>%
  mutate(joFin = round((JoTvFin*60)/60)*60) %>%
  filter(joFin < 27*60) %>%
  group_by(ZoneRang, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

tabDebZoneR %>%
  ggplot(aes(x = h/60, y = pDeb)) +
  geom_line(data = tabDeb, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneRang)) +
  scale_color_manual(values = c(pal_ZoneRang, "gray"), name = "Taille agglom. (AAV)") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) + scale_y_continuous(breaks=c(0:15)) +
  ylab("part des travailleur·ses (%) par 1/4h") + xlab("heure de la journée") +
  labs(title = ml("Répartition des heures d'arrivée et de départ au lieu d'emploi",
                  "Journées à temps plein complètes : indépendant·es, cadres, ouvrier·es"),
       subtitle = "Heure d'arrivée au lieu de travail (arrondie au quart d'heure)")
tabFinZoneR %>%
  ggplot(aes(x = h/60, y = pFin)) +
  geom_line(data = tabFinH, aes(colour = "moyenne"), size = 3, alpha=.6) +
  geom_line(aes(colour = ZoneRang)) +
  scale_color_manual(values = c(pal_ZoneRang, "gray"), name = "Taille agglom. (AAV)") +
  scale_x_continuous(breaks = c(2:12)*2, limits=c(4,27)) +
  ylab("part des travailleur·ses (%) par heure") + xlab("heure de la journée") +
  labs(subtitle = "Heure de départ du lieu de travail (arrondie à l'heure)",
       caption = src_fig(PER))

sortie("Horaires/Horaires, planche PCS8", format = "page", portrait = T)
PER %>%
  filter(DuTvl > 0 & Activ == "10" & !is.na(ZoneDens_duo)) %>%
  group_by(ZoneDens_duo) %>% summarise(joTvDeb = median(JoTvDeb * 60, na.rm=T),
                                       joTvFin = median(JoTvFin * 60, na.rm=T),
                                       joTvDeb_q1 = quantile(JoTvDeb*60, probs=.25, na.rm=T),
                                       joTvDeb_q3 = quantile(JoTvDeb*60, probs=.75, na.rm=T),
                                       joTvFin_q1 = quantile(JoTvFin*60, probs=.25, na.rm=T),
                                       joTvFin_q3 = quantile(JoTvFin*60, probs=.75, na.rm=T)) %>%
  tab_Tri(i = "ZoneDens_duo", parCol = "joTvDeb", rev = T) %>%
  pivot_longer(cols = c("joTvDeb", "joTvFin"), names_to = "variable", values_to = "x") %>%
  ggplot(aes(x = x/60, y = ZoneDens_duo)) +
  geom_errorbarh(aes(xmin = joTvDeb_q1/60, xmax = joTvDeb_q3/60,
                     linetype = "intervalle\ninterquartile"), size = 1.5, height=.2, alpha = .2) +
  geom_errorbarh(aes(xmin = joTvFin_q1/60, xmax = joTvFin_q3/60,
                     linetype = "intervalle\ninterquartile"), size = 1.5, height=.2, alpha = .2) +
  geom_point(aes(shape = variable)) +
  scale_x_continuous(breaks = c(3:10)*2, limits=c(6,20)) +
  scale_linetype(name = NULL) +
  xlab(NULL) + ylab("Trajectoire pendulaire") +
  scale_shape(name = "Heure médiane", labels=c("de début de journée", "de fin de journée")) +
  labs(title = "Heure de début et de fin de présence au lieu d'emploi",
       subtitle = "En fonction de la PCS détaillée de l'enquêté·e\n(si disponible)") %>%
  print()
off()

# selon le statut

tabDeb = PER_trav %>% mutate(joDeb = trunc((JoDeb/30))*30/60) %>%
  group_by(Activ, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFin = PER_trav %>% mutate(joFin = trunc((JoFin/30))*30/60) %>%
  group_by(Activ, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)
left_join(tabDeb, tabFin, by=c("Activ", "h")) %>%
  filter(Activ %in% c("10", "11")) %>%
  mutate(Activ = etqActiv(Activ)) %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) +
  geom_area(aes(fill = variable), alpha=.25, position = "identity") +
  geom_line(aes(colour = variable)) + facet_wrap(~Activ)

# selon la PCS et le genre

tabDeb = PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joDeb = trunc((JoDeb/30))*30/60) %>%
  group_by(Genre, PCS8, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFin = PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joFin = trunc((JoFin/30))*30/60) %>%
  group_by(Genre, PCS8, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)
left_join(tabDeb, tabFin, by=c("Genre", "PCS8", "h")) %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8)) %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) +
  geom_area(aes(fill = variable), alpha=.25, position = "identity") +
  geom_line(aes(colour = variable)) + facet_grid(PCS8~Genre)


PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joDeb = trunc(JoDeb/60)) %>%
  group_by(Genre, PCS8, joDeb) %>% summarise(n = n()) %>%
  filter(joDeb >= 4 & joDeb <= 11) %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(joDeb = factor(joDeb, levels = sort(c(4:11), decreasing = T))) %>%
  ggplot(aes(x = PCS8, y = p)) +
  geom_col(aes(fill = joDeb), position = "stack") +
  coord_flip() +
  facet_wrap(~Genre) %>% print()

PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joFin = trunc(JoFin/60)) %>%
  group_by(Genre, PCS8, joFin) %>% summarise(n = n()) %>%
  filter(joFin >= 16 & joFin <= 23) %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(joFin = factor(joFin, levels = sort(c(16:23), decreasing = T))) %>%
  ggplot(aes(x = PCS8, y = p)) +
  geom_col(aes(fill = joFin), position = "stack") +
  coord_flip() +
  facet_wrap(~Genre) %>% print()

tabDeb = PER_trav %>% mutate(joDeb = trunc((JoTvDeb*60/30))*30/60) %>%
  group_by(joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFin = PER_trav %>% mutate(joFin = trunc((JoTvFin*60/30))*30/60) %>%
  group_by(joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)

left_join(tabDeb, tabFin, by="h") %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) + geom_col(aes(fill = variable))

left_join(tabDeb, tabFin, by="h") %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) +
  geom_area(aes(fill = variable), alpha=.25, position = "identity") +
  geom_line(aes(colour = variable))

tabDeb = PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joDeb = trunc((JoTvDeb*60/30))*30/60) %>%
  group_by(Genre, PCS8, joDeb) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joDeb, nDeb = n, pDeb = p)
tabFin = PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joFin = trunc((JoTvFin*60/30))*30/60) %>%
  group_by(Genre, PCS8, joFin) %>% summarise(n = n()) %>% mutate(p = n / sum(n) * 100) %>%
  rename(h = joFin, nFin = n, pFin = p)
left_join(tabDeb, tabFin, by=c("Genre", "PCS8", "h")) %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8)) %>%
  pivot_longer(cols = c("pDeb", "pFin"), names_to = "variable", values_to = "p") %>%
  ggplot(aes(x = h, y = p)) +
  geom_area(aes(fill = variable), alpha=.25, position = "identity") +
  geom_line(aes(colour = variable)) + facet_grid(PCS8~Genre)

PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joDeb = trunc(JoTvDeb*60/60)) %>%
  group_by(Genre, PCS8, joDeb) %>% summarise(n = n()) %>%
  filter(joDeb >= 4 & joDeb <= 11) %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(joDeb = factor(joDeb, levels = sort(c(4:11), decreasing = T))) %>%
  ggplot(aes(x = PCS8, y = p)) +
  geom_col(aes(fill = joDeb), position = "stack") +
  coord_flip() +
  facet_wrap(~Genre)

PER_trav %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(joFin = trunc(JoTvFin*60/60)) %>%
  group_by(Genre, PCS8, joFin) %>% summarise(n = n()) %>%
  filter(joFin >= 15 & joFin <= 22) %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(joFin = factor(joFin, levels = sort(c(15:22), decreasing = T))) %>%
  ggplot(aes(x = PCS8, y = p)) +
  geom_col(aes(fill = joFin), position = "stack") +
  coord_flip() +
  facet_wrap(~Genre)


PER %>% filter(DuTvl > 0,  Activ == "10", JoTvDeb > 0, JoTvFin < 27) %>%
  filter(PCS8 %in% c("03","04","05","06")) %>%
  mutate(joFin = round((JoTvFin*15)/15),
         joDeb = round((JoTvDeb*15)/15)) %>%
  group_by(PCS8, joFin, joDeb) %>% summarise(n = n()) %>%
  group_by(PCS8) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = joDeb, y = joFin)) +
  geom_contour(aes(z = p, colour = PCS8),  alpha=.6) +
  scale_color_manual(values = pal_PCS8[3:6]) +
  facet_wrap(~PCS8)

PER_ff %>% filter(JoTvDeb > 0, JoTvFin < 27) %>%
  filter(PCS8 %in% c("03","04","05","06")) %>%
  mutate(joFin = round((JoTvFin*15)/15),
         joDeb = round((JoTvDeb*15)/15)) %>%
  group_by(PCS8, joFin, joDeb) %>% summarise(n = n()) %>%
  group_by(PCS8) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = joDeb, y = joFin)) +
  geom_raster(aes(fill = p)) +
  scale_fill_gradient(low = "whitesmoke", high = "firebrick") +
  scale_x_continuous(limits=c(4,12)) + scale_y_continuous(limits=c(11,22)) +
  facet_wrap(~PCS8)

PER %>% filter(DuTvl > 0,  Activ == "11", JoTvDeb > 0, JoTvFin < 27) %>%
  filter(PCS8 %in% c("03","04","05","06")) %>%
  mutate(joFin = round((JoTvFin*15)/15),
         joDeb = round((JoTvDeb*15)/15)) %>%
  group_by(PCS8, joFin, joDeb) %>% summarise(n = n()) %>%
  group_by(PCS8) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = joDeb, y = joFin)) +
  geom_raster(aes(fill = p)) +
  scale_fill_gradient(low = "whitesmoke", high = "firebrick") +
  facet_wrap(~PCS8)

# Cartographie des horaires par enquête ====

# carte lissée ?

load("Data/shp_ZF.rds")
load("Data/shp_ZT.rds")
load("Data/fdCarte.rds")

plot_inter(fun = "e", span = 5000, beta = 2, limit = 30000)

plot_inter(fun = "p", span = 5000, beta = 10, limit = 100000)

brks=c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9) - .25

quantile(PER_ff$JoTvDeb, probs=c(1:9)/10, na.rm=T)

sortie("Horaires/Heures début de journée, cartes lissées", format = "pdf",
       taille = "a4")

ggplot(data = PER_ff, aes(x = JoTvDeb)) + geom_density()

#TODO: bugué pour raisons inconnues + il faudrait vérifier les coeffs + utiliser discretisation()

ggCartePotentiel(PER = filter(PER_ff, JoTvDeb > 4), enq = "IDF2010", proj = 2154,
                 var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                 titre = "Région parisienne", leg = "Heure début\njournée trav.", forcerDecoupage = T)


ggCartePotentiel(PER = filter(PER, JoTvDeb > 4), enq = "LOI2015", proj = 2154,
                 var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                 titre = "Nantes", leg = "Heure début\njournée trav.", forcerDecoupage = T)


ggCartePotentiel(PER = filter(PER, JoTvDeb > 4), enq = "LOI2015", proj = 2154,
                 var = "JoTvDeb", w = "CoeffEnq", fun = "p", span = 5000, beta = 10, limit = 100000,
                 res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                 titre = "Nantes", leg = "Heure début\njournée trav.", forcerDecoupage = T)



ggCartePotentiel(PER = filter(PER, JoTvDeb > 4), enq = c("ALE2018", "CHE2016", "CAL2011"), proj = 2154,
                 var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                 titre = "Nantes", leg = "Heure début\njournée trav.", forcerDecoupage = T)

off()

quadriPotPCS = function(enq, proj=2154, titre=NULL, brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9))
{
  c3 = ggCartePotentiel(PER = filter(PER, JoTvDeb > 4, PCS8 == "03"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = "Cadres", leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  c4 = ggCartePotentiel(PER = filter(PER, JoTvDeb > 4, PCS8 == "04"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = "Professions intermédiaires", leg = "Heure début\njournée travail",
                        forcerDecoupage = T, detailNoms = 3)
  c5 = ggCartePotentiel(PER = filter(PER, JoTvDeb > 4, PCS8 == "05"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = "Employé·es", leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  c6 = ggCartePotentiel(PER = filter(PER, JoTvDeb > 4, PCS8 == "06"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = "Ouvrier·es", leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  
  p = cowplot::plot_grid(c3 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c4 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c5 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c6 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         nrow=2, align = "hv", axis="lrtb")
  p = cowplot::plot_grid(p, cowplot::get_legend(c3), nrow=1, ncol=2, rel_widths = c(8,2))
  
  if (!is.null(titre))
  {
    p = viz_Titre(p, titre)
  }
  p = viz_Pied(p, pied = src_fig(filter(PER, uid_ENQ %in% enq)), rel_heights = c(.95, .05))
  
  return(p)
}

quadriPCS = function(enq, proj=2154, titre=NULL,
                     brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9),
                     detLab = 3)
{
  brksPop = c(1000, 2000, 5000)
  
  PERf = filter(PER_ff, uid_ENQ == enq, JoTvDeb > 4)
  
  PERf3 = filter(PERf, PCS8 == "03") %>%
    group_by(ZT_travMax) %>%
    summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c3 = shp_ZT %>% filter(uid_ENQ == enq) %>%
    left_join(PERf3, by=c("ZT" = "ZT_travMax")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "darkorchid", high = "darkorange", na.value = "gray65",
                         name = "heure arrivée\nau lieu d'emploi",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "population en\nemploi sur\nle secteur",
               breaks = brksPop) +
    labs(title = "Cadres")
  c3 = cartoLib(c3, etendue = filter(shp_ZT, uid_ENQ == enq), detail = detLab, carte = fdCarte)
  c3 = cartoFinish(c3, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  PERf4 = filter(PERf, PCS8 == "04") %>%
    group_by(ZT_travMax) %>%
    summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c4 = shp_ZT %>% filter(uid_ENQ == enq) %>%
    left_join(PERf4, by=c("ZT" = "ZT_travMax")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "darkorchid", high = "darkorange", na.value = "gray65",
                         name = "heure arrivée\nau lieu d'emploi",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "population en\nemploi sur\nle secteur",
               breaks = brksPop) +
    labs(title = "Professions intermédiaires")
  c4 = cartoLib(c4, etendue = filter(shp_ZT, uid_ENQ == enq), detail = detLab, carte = fdCarte)
  c4 = cartoFinish(c4, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  PERf5 = filter(PERf, PCS8 == "05") %>%
    group_by(ZT_travMax) %>%
    summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c5 = shp_ZT %>% filter(uid_ENQ == enq) %>% left_join(PERf5, by=c("ZT" = "ZT_travMax")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "darkorchid", high = "darkorange", na.value = "gray65",
                         name = "heure arrivée\nau lieu d'emploi",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "population en\nemploi sur\nle secteur",
               breaks = brksPop) +
    labs(title = "Employé·es")
  c5 = cartoLib(c5, etendue = filter(shp_ZT, uid_ENQ == enq), detail = detLab, carte = fdCarte)
  c5 = cartoFinish(c5, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  PERf6 = filter(PERf, PCS8 == "06") %>%
    group_by(ZT_travMax) %>%
    summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c6 = shp_ZT %>% filter(uid_ENQ == enq) %>%
    left_join(PERf6, by=c("ZT" = "ZT_travMax")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "darkorchid", high = "darkorange", na.value = "gray65",
                         name = "heure arrivée\nau lieu d'emploi",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "population en\nemploi sur\nle secteur",
               breaks = brksPop) +
    labs(title = "Ouvrier·es")
  c6 = cartoLib(c6, etendue = filter(shp_ZT, uid_ENQ == enq), detail = detLab, carte = fdCarte)
  c6 = cartoFinish(c6, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  p = cowplot::plot_grid(c3 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c4 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c5 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c6 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         nrow=2, align = "hv", axis="lrtb")
  p = cowplot::plot_grid(p, cowplot::get_legend(c3), nrow=1, ncol=2, rel_widths = c(8,2))
  
  if (!is.null(titre))
  {
    p = viz_Titre(p, titre)
  }
  p = viz_Pied(p, pied = src_fig(PERf), rel_heights = c(.95, .05))
  
  return(p)
}

# sortie("Horaires/Horaires (quadri, idf, pcs)")
# quadriPCS("IDF2010")
# off()

sortie("Horaires/Horaires (quadri, 44, pcs).png", taille = "carré")
quadriPCS("LOI2015", titre = "Heure moyenne d'arrivée au lieu de travail\nen Loire-Atlantique selon la PCS",
          brks = seq(from = 6.5, to = 10, by = .5))
off()

# sortie("Horaires/Horaires (quadri, normandie, pcs)")
# quadriPotPCS(c("ALE2018", "CHE2016", "CAL2011"))
# off()
# 
# sortie("Horaires/Horaires (quadi, lyon, pcs)")
# quadriPotPCS("LYO2015")
# off()

sortie("Horaires/Horaires (quadri, 13)", taille = "carré")
quadriPCS("MAR2009", titre = "Heure moyenne d'arrivée au lieu de travail\nen région marseillaise selon la PCS",
          brks = seq(from = 6.5, to = 10, by = .5), detLab = 2)
off()

sortie("Horaires/Horaires (quadri, bordeaux)")
quadriPotPCS("BOR2009")
off()

quadriPotPCS_nivDip = function(enq, proj=2154, titre=NULL, brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9))
{
  PERf = PER %>% filter(JoTvDeb > 4) %>%
    mutate(nivDip = NivEtuVersNivDip(NivEtu))
  
  c3 = ggCartePotentiel(PER = filter(PERf, nivDip == "1"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = etqNivDip("1"), leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  c4 = ggCartePotentiel(PER = filter(PERf, nivDip == "2"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = etqNivDip("2"), leg = "Heure début\njournée travail",
                        forcerDecoupage = T, detailNoms = 3)
  c5 = ggCartePotentiel(PER = filter(PERf, nivDip == "3"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = etqNivDip("3"), leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  c6 = ggCartePotentiel(PER = filter(PERf, nivDip == "4"), enq = enq, proj = proj,
                        var = "JoTvDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "darkorchid", coulHaut="darkorange", 
                        titre = etqNivDip("4"), leg = "Heure début\njournée travail", forcerDecoupage = T,
                        detailNoms = 3)
  
  p = cowplot::plot_grid(c3 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c4 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c5 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c6 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         nrow=2, align = "hv", axis="lrtb")
  p = cowplot::plot_grid(p, cowplot::get_legend(c3), nrow=1, ncol=2, rel_widths = c(8,2))
  
  if (!is.null(titre))
  {
    p = viz_Titre(p, titre)
  }
  p = viz_Pied(p, pied = src_fig(filter(PER, uid_ENQ %in% enq)), rel_heights = c(.95, .05))
  
  return(p)
}

brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9) - .25

sortie("Horaires/Horaires (quadri, 44, nivDip)", taille = "carré")
quadriPotPCS_nivDip("LOI2015", titre = ml("Heure moyenne d'arrivée au lieu de travail",
                                          "en Loire-Atlantique selon le diplôme"),
                    brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9) - .25)
off()


quadriPotPCS_joDeb = function(enq, proj=2154, titre=NULL, brks=c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9))
{
  PERf = PER %>%
    filter(typoJo == "TRAV") %>%
    mutate(JoDeb = heureHHMMtoM(JoDeb)/60) %>%
    filter(JoDeb > 4)
  
  c3 = ggCartePotentiel(PER = filter(PERf, PCS8 == "03"), enq = enq, proj = proj,
                        var = "JoDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "slateblue", coulHaut="lightgoldenrod", 
                        titre = "Cadres", leg = "Heure départ\ndu domicile", forcerDecoupage = T,
                        detailNoms = 3)
  c4 = ggCartePotentiel(PER = filter(PERf, PCS8 == "04"), enq = enq, proj = proj,
                        var = "JoDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "slateblue", coulHaut="lightgoldenrod", 
                        titre = "Professions intermédiaires", leg = "Heure départ\ndu domicile",
                        forcerDecoupage = T, detailNoms = 3)
  c5 = ggCartePotentiel(PER = filter(PERf, PCS8 == "05"), enq = enq, proj = proj,
                        var = "JoDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "slateblue", coulHaut="lightgoldenrod", 
                        titre = "Employé·es", leg = "Heure départ\ndu domicile", forcerDecoupage = T,
                        detailNoms = 3)
  c6 = ggCartePotentiel(PER = filter(PERf, PCS8 == "06"), enq = enq, proj = proj,
                        var = "JoDeb", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                        res_grille = 1000, brks=brks, coulBas = "slateblue", coulHaut="lightgoldenrod", 
                        titre = "Ouvrier·es", leg = "Heure départ\ndu domicile", forcerDecoupage = T,
                        detailNoms = 3)
  
  p = cowplot::plot_grid(c3 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c4 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c5 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c6 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         nrow=2, align = "hv", axis="lrtb")
  p = cowplot::plot_grid(p, cowplot::get_legend(c3), nrow=1, ncol=2, rel_widths = c(8,2))
  
  if (!is.null(titre))
  {
    p = viz_Titre(p, titre)
  }
  p = viz_Pied(p, pied = src_fig(filter(PER, uid_ENQ %in% enq)), rel_heights = c(.95, .05))
  
  return(p)
}

quadriPCS_joDeb = function(enq, proj=2154, titre=NULL,
                           brks = c(7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9))
{
  PERf = PER_ff %>%
    filter(uid_ENQ == enq) %>%
    mutate(JoDeb = heureHHMMtoM(JoDeb)/60) %>%
    filter(JoDeb > 4)
  
  brksPop = c(1000, 2000, 5000)
  
  PERf3 = filter(PERf, PCS8 == "03") %>%
    group_by(ZT) %>%
    summarise(JoTvDeb = weighted.mean(JoDeb, w = CoeffRecEnq), p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c3 = shp_ZT %>% filter(uid_ENQ == enq) %>% left_join(PERf3, by=c("ZT")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "slateblue", high = "lightgoldenrod", na.value = "gray65",
                         name = "heure départ\ndu domicile",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "nombre de\ntravailleur⋅ses\nrésidant⋅es",
               breaks = brksPop, limits = c(0,5000)) +
    labs(title = "Cadres")
  c3 = cartoLib(c3, etendue = filter(shp_ZT, uid_ENQ == enq), detail = 3, carte = fdCarte)
  c3 = cartoFinish(c3, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  PERf4 = filter(PERf, PCS8 == "04") %>%
    group_by(ZT) %>%
    summarise(JoTvDeb = weighted.mean(JoDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c4 = shp_ZT %>% filter(uid_ENQ == enq) %>% left_join(PERf4, by=c("ZT")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "slateblue", high = "lightgoldenrod", na.value = "gray65",
                         name = "heure départ\ndu domicile",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "nombre de\ntravailleur⋅ses\nrésidant⋅es",
               breaks = brksPop, limits = c(0,5000)) +
    labs(title = "Professions intermédiaires")
  c4 = cartoLib(c4, etendue = filter(shp_ZT, uid_ENQ == enq), detail = 3, carte = fdCarte)
  c4 = cartoFinish(c4, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  PERf5 = filter(PERf, PCS8 == "05") %>%
    group_by(ZT) %>%
    summarise(JoTvDeb = weighted.mean(JoDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c5 = shp_ZT %>% filter(uid_ENQ == enq) %>% left_join(PERf5, by=c("ZT")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "slateblue", high = "lightgoldenrod", na.value = "gray65",
                         name = "heure départ\ndu domicile",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "nombre de\ntravailleur⋅ses\nrésidant⋅es",
               breaks = brksPop, limits = c(0,5000)) +
    labs(title = "Employé·es")
  c5 = cartoLib(c5, etendue = filter(shp_ZT, uid_ENQ == enq), detail = 3, carte = fdCarte)
  c5 = cartoFinish(c5, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  
  PERf6 = filter(PERf, PCS8 == "06") %>%
    group_by(ZT) %>%
    summarise(JoTvDeb = weighted.mean(JoDeb, w = CoeffRecEnq),
              p = sum(CoeffRecEnq), n = n()) %>%
    filter(n >= 10)
  c6 = shp_ZT %>% filter(uid_ENQ == enq) %>% left_join(PERf6, by=c("ZT")) %>%
    st_point_on_surface() %>%
    ggplot() +
    geom_sf(data = filter(shp_ZT, uid_ENQ == enq), fill = "gray95", color = "gray85") +
    geom_sf(aes(color = JoTvDeb, size = p), alpha = .8) +
    scale_color_gradient(low = "slateblue", high = "lightgoldenrod", na.value = "gray65",
                         name = "heure départ\ndu domicile",
                         breaks = brks, limits = brks[c(1, length(brks))]) +
    scale_size(name = "nombre de\ntravailleur⋅ses\nrésidant⋅es",
               breaks = brksPop, limits = c(0,5000)) +
    labs(title = "Ouvrier·es")
  c6 = cartoLib(c6, etendue = filter(shp_ZT, uid_ENQ == enq), detail = 3, carte = fdCarte)
  c6 = cartoFinish(c6, etendue = filter(shp_ZT, uid_ENQ == enq))
  
  p = cowplot::plot_grid(c3 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c4 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c5 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         c6 + theme(legend.position = "none") + labs(subtitle = NULL, caption=NULL),
                         nrow=2, align = "hv", axis="lrtb")
  p = cowplot::plot_grid(p, cowplot::get_legend(c3), nrow=1, ncol=2, rel_widths = c(8,2))
  
  if (!is.null(titre))
  {
    p = viz_Titre(p, titre)
  }
  p = viz_Pied(p, pied = src_fig(PERf), rel_heights = c(.95, .05))
  
  return(p)
}

# sortie("Horaires/Horaires (quadri, idf, depDom)")
# quadriPCS_joDeb("IDF2010")
# off()

sortie("Horaires/Horaires (quadri, 44, depDom)", taille = "carré")
quadriPCS_joDeb("LOI2015", titre = "Heure moyenne de départ du domicile\nen Loire Atlantique",
                brks = seq(from = 5.5, to = 9.5, by = .5))
off()

# PER %>% filter(typoJo == "TRAV" & JoTvDeb > 4) %>%
#   filter(uid_ENQ == "LOI2015") %>%
#   ggplot(aes(x = Tps/60, y = JoTvDeb)) + geom_jitter() + scale_x_log10()
# 
# PER %>% filter(typoJo == "TRAV" & JoTvDeb > 4) %>%
#   filter(uid_ENQ == "LOI2015") %>%
#   ggplot(aes(x = Tps/60, y = JoTvDeb)) + geom_hex() + scale_x_log10()
# 
# PER %>% filter(typoJo == "TRAV" & JoTvDeb > 5 & JoTvDeb < 10) %>%
#   filter(uid_ENQ == "LOI2015") %>%
#   ggplot(aes(x = Tps/60, y = JoTvDeb)) + geom_hex() + scale_x_log10()
# 
# test = PER %>% filter(typoJo == "TRAV" & JoTvDeb > 5 & JoTvDeb < 10) %>%
#   filter(uid_ENQ == "LOI2015")
# cor.test(test$JoTvDeb, test$Tps)
# cor.test(test$JoTvDeb, test$Tps, method = "spearman")
# 
# test = PER %>% filter(typoJo == "TRAV" & JoTvDeb > 5 & JoTvDeb < 10) %>%
#   filter(uid_ENQ == "IDF2010")
# cor.test(test$JoTvDeb, test$Tps, method = "spearman")
# 
# test = PER %>% filter(typoJo == "TRAV" & JoTvDeb > 5 & JoTvDeb < 10 & PCS8 == "05") %>%
#   filter(uid_ENQ == "LOI2015")
# cor.test(test$JoTvDeb, test$Tps, method = "spearman")

# Modélisation et régressions ====

# Heure moyenne jotvdeb
PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  summarise(JoTvDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq, na.rm=T)) |>
  mutate(JoTvDeb = heureMinToHr(JoTvDeb*60))

# Simple projection où l'axe X est l'heure d'arrivée et l'axe Y l'heure de départ
PER_ff %>%
  filter(!is.na(JoTvDeb) & !is.na(JoTvFin) & !is.na(CoeffRecEnq)) %>%
  group_by(Genre, PCS8) %>%
  summarise(JoTvDeb = weighted.mean(JoTvDeb, CoeffRecEnq),
            JoTvFin = weighted.mean(JoTvFin, CoeffRecEnq)) %>%
  ggplot(aes(x = JoTvDeb, y = JoTvFin)) + geom_point(aes(colour = PCS8, shape=Genre))

PER_ff %>%
  filter(!is.na(JoTvDeb) & !is.na(JoTvFin) & !is.na(CoeffRecEnq)) %>%
  group_by(Genre, PCS8, PCS42S) %>%
  summarise(JoTvDeb = weighted.mean(JoTvDeb, CoeffRecEnq),
            JoTvFin = weighted.mean(JoTvFin, CoeffRecEnq)) %>%
  ggplot(aes(x = JoTvDeb, y = JoTvFin)) +
  geom_point(aes(colour = PCS8, shape=Genre)) +
  geom_text(aes(label = PCS42S), nudge_y = -.06)

# Modélisation (à filtrer au sein des horaires "typiques")

# regression(base = PER_trav,
#            val = "JoDeb", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "heure de début journée",
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "h",
#            imprDistrib = T) %>% summary()
# 
# regressionLog(base = PER_trav,
#               val = "JoDeb", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#               poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "heure de début journée",
#               titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "h",
#               imprDistrib = T) %>% summary()
# 
# regression(base = PER_trav,
#            val = "JoFin", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "heure de fin journée",
#            titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "h",
#            imprDistrib = T) %>% summary()
# 
# regressionLog(base = PER_trav,
#               val = "JoFin", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#               poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "heure de fin journée",
#               titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "h",
#               imprDistrib = T) %>% summary()
# 
# sortie("Horaires/Modèles exploratoires horaires", format = "pdf", taille = "a4", portrait = T)
# 
# regression(base = filter(PER_trav, JoDeb < 12*60),
#            val = "JoDeb", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée",
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoFin > 15*60 & JoFin < 23*60),
#            val = "JoFin", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de fin journée",
#            titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoDeb < 12*60),
#            val = "JoDeb", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée",
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoFin > 15*60 & JoFin < 23*60),
#            val = "JoFin", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de fin journée",
#            titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# 
# regression(base = filter(PER_trav, JoTvDeb >= 5 & JoTvDeb < 12),
#            val = "JoTvDeb", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoTvFin > 15 & JoTvFin < 23),
#            val = "JoTvFin", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de fin journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoTvDeb >= 5 &  JoTvDeb < 12),
#            val = "JoTvDeb", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(PER_trav, JoTvFin > 13.5 & JoTvFin < 21.5),
#            val = "JoTvFin", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de fin journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de retour au domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# # TODO: refaire tout ça avec les horaires de travail
# #       établir quelles sont les activités qui s'intercalent
# #       regarder s'il existe une tendance géographique aussi
# #       récupérer la carte des tendances horaires (c'était sympa) et essayer de la lisser ?
# 
# regression(base = filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                          JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11")),
#            val = "JoTvDeb", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = mutate(filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                                 JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11")),
#                          NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(NivDip, "2")),
#            val = "JoTvDeb", formule = "PCS8 + NivDip + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = mutate(filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                                 JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11"),
#                                 Age < 70),
#                          NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(NivDip, "2")),
#            val = "JoTvDeb", formule = "PCS8 + NivDip + Age10 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# base = filter(mutate(filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                             JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11"),
#                             Age < 70),
#                      Age = etqAge(Age, pas = 10, min = 16, max = 70, forceMin = T),
#                      NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(NivDip, "2")),
#               NivDip != "9")
# 
# table(base$NivDip, base$PCS8)
# 
# regression(base = base,
#            val = "JoTvDeb", formule = "PCS8 + NivDip + Age10 + Activ + ZoneDens + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modèle H1 (tout l'échantillon)\nheure d'arrivée au lieu de travail", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = mutate(filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                                 PCS42S %in% PCS_privé,
#                                 JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11"),
#                                 Age < 70),
#                          NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(NivDip, "2")),
#            val = "JoTvDeb", formule = "PCS8 + NivDip + Age10 + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = mutate(filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                                 JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11"),
#                                 Age < 70),
#                          NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(NivDip, "2"),
#                          secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
#                                              PCS42S %in% c("32", "41", "51") ~ "public"),
#                          secteur = as.factor(secteur)),
#            val = "JoTvDeb", formule = "PCS8 + NivDip + Age10 + Activ + ZoneDens + ZoneRang + Genre + MenEnfants",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T, colComparaison = "secteur") %>% summary()
# 
# regression(base = mutate(base, etqNivDip(NivDip, num = T)),
#            val = "JoTvDeb", formule = "PCS8 + Age10 + Activ + ZoneDens + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "NivDip",
#            titre = "Modèles H3 (par niv. études)\nheure d'arrivée au lieu de travail", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# regression(base = filter(valref(PER), PCS8 %in% c("02","03","04","05","06"),
#                          JoTvDeb >= 5 & JoTvDeb < 11, Activ %in% c("10","11")),
#            val = "JoTvDeb", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
#            retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
#            titre = "Modélisation de l'heure de départ du domicile (un jour travaillé)", unite = "min",
#            imprDistrib = T) %>% summary()
# 
# off()

sortie("Horaires/Modèle horaires (mod H1)", portrait = T)
PER_ff |>
  filter(JoTvDeb > 5, JoTvDeb < 11) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles"),
         dsTvl = factor(dsTvl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
         NivDip = NivEtuVersNivDip(NivEtu)) |>
  valref() |>
regression(val = "JoTvDeb", formule = "PCS8 + NivDip + Age10 + Activ + dsTvl + Genre",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
           titre = "Modèle H1 (tout l'échantillon)\nheure d'arrivée au lieu de travail", unite = "min",
           imprDistrib = T, caption = src_fig(emp=F)) %>% summary()
off()

sortie("Horaires/Modèle horaires (mod H2)", taille = "page", portrait = F)
PER_ff |>
  filter(JoTvDeb > 5, JoTvDeb < 11) |> filter(PCS8 != "01") |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles"),
         dsTvl = factor(dsTvl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
         NivDip = NivEtuVersNivDip(NivEtu)) |>
  valref() |>
  mutate(PCS8 = etqPCS8(PCS8, num = T)) |>
  regression(val = "JoTvDeb", formule = "NivDip + Age10 + Activ + dsTvl + Genre",
             retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "PCS8",
             titre = "Modèles H2 (par CS)\nheure d'arrivée au lieu de travail", unite = "min",
             imprDistrib = T) %>% summary()
off()

# Retourner l'image pour l'insérer facilement...
img = image_read(path = "Sorties/Horaires/Modèle horaires (mod H2).png")
img = image_rotate(img, 270)
image_write(img, path = "Sorties/Horaires/Modèle horaires (mod H2).png")

# Pour contrôler les croisements pour le modèle
table(mutate(PER_ff, NivDip = NivEtuVersNivDip(NivEtu))$NivDip, PER_ff$PCS8) / nrow(PER_ff)

# Horaires et centralité ====

rapport("Indices de centralité par rapport aux AAV", prim = T)

# Procédure :
# Calculer la distance entre chaque ZF et le centre de l'AAV
# Centre de l'AAV = centroïde de la commune-centre

ZFs = centroidesAAV()

# on calcule la distance au centre de tout un chacun, et on s'amuse !
PER = left_join(PER, rename(ZFs, disAAV_dom = dis), by="ZF") %>%
  left_join(rename(ZFs, disAAV_tvl = dis), by=c("ZF_travMax" = "ZF"))

PER %>%
  group_by(PCS8) %>% summarise(medDisAAV_dom = median(disAAV_dom, na.rm=T),
                               medDisAAV_tvl = median(disAAV_tvl, na.rm=T))

activites = left_join(activites, ZFs, by=c("ZF.ACT" = "ZF"))

# save(activites, file = "Data/activites.rds")

g1= activites %>%
  filter(DuTvl > 0, PCS8 %in% c("01","02","03","04","05","06")) %>%
  group_by(heure, PCS8) %>% summarise(disCtrMed = median(dis, na.rm=T)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  ggplot(aes(x = heure/24, y = disCtrMed/1000)) + geom_line(aes(colour = PCS8)) +
  scale_colour_manual(values = pal_PCS8[1:6]) +
  xlab("heure") + ylab("distance médiane au centre (km)")

sortie("Horaires/Centralité selon heure et PCS8")
print(g1)
off()

g2= activites %>%
  filter(DuTvl > 0, PCS8 %in% c("01","02","03","04","05","06")) %>%
  left_join(y = select(PER, uid_PER, ZoneRang), by="uid_PER") %>%
  filter(ZoneRang %in% c("2","3","4","5")) %>%
  mutate(ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneRang, heure, PCS8) %>% summarise(disCtrMed = median(dis, na.rm=T)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  ggplot(aes(x = heure/24, y = disCtrMed)) + geom_line(aes(colour = PCS8)) +
  scale_colour_manual(values = pal_PCS8[1:6]) + facet_wrap(~ZoneRang) +
  xlab("heure") + ylab("distance médiane au centre (km)")

sortie("Horaires/Centralité selon heure, PCS8 et ZoneRang")
print(g2)
off()

tableDisDep =
  activites %>%
  filter(heure == 240) %>%
  mutate(disDep = trunc(dis / 2500) * 2.5) %>%
  select(uid_PER, disDep)

g3 = left_join(activites, tableDisDep, by="uid_PER") %>%
  filter(disDep <= 30) %>%
  filter(DuTvl > 0, PCS8 %in% c("01","02","03","04","05","06")) %>%
  group_by(disDep, heure) %>% summarise(disCtrMed = median(dis, na.rm=T)) %>%
  ggplot(aes(x = heure/60, y = disCtrMed)) + geom_line(aes(colour = disDep, group = disDep)) +
  xlab("heure") + ylab("distance médiane au centre") +
  scale_colour_continuous(name = "distance initiale\nau centre de l'AAV")
sortie("Horaires/Centralité selon heure, PCS8 et distance initiale")
print(g3)
off()

g4 = left_join(activites, tableDisDep, by="uid_PER") %>%
  filter(disDep <= 30) %>%
  filter(DuTvl > 0, PCS8 %in% c("01","02","03","04","05","06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, heure, disDep) %>%
  summarise(disCtrMed = median(dis, na.rm=T), n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = heure/60, y = disCtrMed)) + geom_line(aes(colour = p, group = disDep)) +
  xlab("heure") + ylab("distance médiane au centre") +
  scale_color_gradient(name = "part de la PCS dans\nla tranche de distance",
                       low = "snow2", high = "firebrick4") + facet_wrap(~PCS8)
sortie("Horaires/Centralité selon heure, PCS8 et distance initiale (avancé)")
print(g4)
off()

tableDisDep =
  activites %>%
  filter(heure == 240) %>%
  mutate(disDep = trunc(dis / 2000) * 2) %>%
  select(uid_PER, disDep)

g5 = left_join(activites, tableDisDep, by="uid_PER") %>%
  filter(disDep <= 20) %>%
  filter(DuTvl > 0, PCS8 %in% c("01","02","03","04","05","06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, heure, disDep) %>%
  summarise(disCtrMoy = mean(dis, na.rm=T), n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = heure/60, y = disCtrMoy /1000)) + geom_line(aes(colour = p, group = disDep)) +
  xlab("heure") + ylab("distance moyenne au centre (km)") +
  scale_color_gradient(name = "part de la PCS dans\nla tranche de distance",
                       low = "snow2", high = "firebrick4") + facet_wrap(~PCS8)

sortie("Horaires/Centralité selon heure, PCS8 et distance initiale (moins de 20 km)")
print(g5)
off()

heures = c(16, 18, 20, 22, 24) * 60

activites %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(heure %in% heures) %>%
  mutate(heure = as.factor(paste0(heure/60, " h")), PCS8 = etqPCS8(PCS8)) %>%
  mutate(disPar5 = trunc(dis / 2500) * 2.5) %>%
  group_by(PCS8, heure, disPar5) %>% summarise(n = n()) %>%
  group_by(PCS8, heure) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = disPar5, y = p)) + geom_line(aes(color = heure, group = heure)) +
  coord_cartesian(ylim = c(0,20), xlim = c(0,40)) +
  xlab("Distance du lieu de travail par rapport au centre de l'AAV") +
  ylab("Part des cadres présent⋅es sur leur lieu d'emploi à l' heure indiquée") +
  labs(title = "Distance du lieu d'emploi au centre de l'AAV selon l'heure",
       subtitle = "Cadres uniquement",
       caption = src_fig(emp = F)) +
  facet_wrap(~PCS8)

# Faire le graphique heure par heure ensuite, avec acti professionnelles uniquement

# Essais de calculs d'exposition pour les employé·es de commerce ?
# N'est-ce pas un peu trop spéculatif ? Si hein ?

# Horaires de fréquentation des transports ====
# Maintenant, regardons qui prend les transports heure par heure

# Qui est dans le métro ?
activites %>%
  filter(Tache == "995") %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(!uid_ENQ %in% c("BRH2009", "EMP2019", "CAL2011")) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  group_by(Libelle_Long, heure, PCS8) %>% summarise(n = sum(CoeffEnq)) %>%
  group_by(Libelle_Long, heure) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = heure, y = p)) +
  geom_area(aes(fill = PCS8)) +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  facet_wrap(~Libelle_Long)

seuil = seuilSignifiant

activites %>%
  filter(Tache == "995") %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(!uid_ENQ %in% c("BRH2009", "EMP2019", "CAL2011")) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  group_by(Libelle_Long, heure) %>%
  summarise(pCad = sum(ifelse(PCS8 == "03", CoeffEnq, 0))/sum(CoeffEnq),
            pInt = sum(ifelse(PCS8 == "04", CoeffEnq, 0))/sum(CoeffEnq),
            pEmp = sum(ifelse(PCS8 == "05", CoeffEnq, 0))/sum(CoeffEnq),
            pOuv = sum(ifelse(PCS8 == "06", CoeffEnq, 0))/sum(CoeffEnq),
            n = n()) %>%
  filter(n>seuil) %>%
  pivot_longer(cols = c("pCad", "pInt", "pEmp", "pOuv"), names_to = "PCS8", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(color = PCS8)) +
  scale_color_manual(values = pal_PCS8[3:6]) +
  facet_wrap(~Libelle_Long) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28))

activites %>%
  filter(Tache == "995") %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(!uid_ENQ %in% c("BRH2009", "EMP2019", "CAL2011")) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  group_by(Libelle_Long, heure) %>%
  summarise(pCad = sum(ifelse(PCS8 == "03", CoeffEnq, 0))/sum(CoeffEnq),
            pOuv = sum(ifelse(PCS8 == "06", CoeffEnq, 0))/sum(CoeffEnq),
            n = n()) %>%
  filter(n>seuil) %>%
  pivot_longer(cols = c("pCad", "pOuv"), names_to = "PCS8", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(color = PCS8)) +
  scale_color_manual(values = pal_PCS8[c(3,6)]) +
  facet_wrap(~Libelle_Long) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28))

# Études de cas ====

# On va utiliser la base des PGT

load("Data/PGT.rds")

PGT = PGT %>% mutate(ZF = paste(ENQ, ZF))

# Combien de personnes par PGT ?
PGT = left_join(PGT, summarise(group_by(ACT, l), n = n()), by = c("ZF" = "l"))
PGT = tab_Tri(PGT, parCol = "n", rev = T)

# Combien de personnes pour le travail ?
PGT = left_join(PGT, summarise(group_by(filter(ACT, substr(Tache,1,1) == "1"), l),
                               nTrav = n()), by = c("ZF" = "l"))

listeZC = c("Cc", "Ccial", "Centre Commercial", "Auchan", "Leclerc", "Supermarch", "Cora",
            "Monoprix", "Zc", "Hypermarch", "Lidl", "Zone Commerciale")
listeZI = c("Za_zi", "Zone Artisanale", "Zone Industrielle", "Technopole", "Zones Activites", "ZI")
listeInd = c("Usine", "Arcelor Mittal", "Edf", "Stx", "Centrale Nucléaire")

# Détection des zones commerciales, industrielles, et surtout des usines
PGT = mutate(PGT, nat_ZoneCom = grepl(paste(listeZC, collapse = "|"), LIB),
             nat_ZoneInd = grepl(paste(listeZI, collapse = "|"), LIB),
             nat_Usine   = grepl(paste(listeInd,collapse = "|"), LIB))

filter(PGT, nat_ZoneCom == T)
filter(PGT, nat_Usine == T)

# Maintenant, on voudrait voir comment ça se passe à l'échelle d'une seule usine
# Idées : représenter par exemple un graphe de présence tt au long journée ?

grossesUsines = filter(PGT, nat_Usine == T & nTrav > 50)$ZF
nomsUsines = paste0(filter(PGT, nat_Usine == T & nTrav > 50)$LIB, " (",
                    filter(PGT, nat_Usine == T & nTrav > 50)$ENQ, ")")

usines = activites %>%
  filter(ZF.ACT %in% grossesUsines) %>%
  group_by(heure, ZF.ACT, PCS8) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n)) %>%
  mutate(nom = plyr::revalue(ZF.ACT, nv(grossesUsines, nomsUsines), warn_missing = F))

ggplot(usines, aes(x = heure/60, y = n)) +
  geom_area(aes(fill = PCS8)) +
  scale_fill_manual(values = pal_PCS8) +
  facet_wrap(~nom)

ggplot(usines, aes(x = heure/60, y = p)) +
  geom_area(aes(fill = PCS8)) +
  scale_fill_manual(values = pal_PCS8) +
  facet_wrap(~nom)

ggplot(usines, aes(x = heure/60, y = n)) +
  geom_line(aes(color = PCS8)) +
  scale_color_manual(values = pal_PCS8) +
  facet_wrap(~nom)

g1 = ACT %>%
  filter(l == "CHE2016 000013204") %>%
  left_join( y = select(PER, uid_PER, PCS8, JoTvDeb), by = "uid_PER" ) %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  tab_Tri(parCol = "JoTvDeb", rev = T) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  ggplot(aes(x = hDeb/60, y = uid_PER)) +
  geom_linerange(aes(xmin = hDeb/60, xmax = hFin/60, color = PCS8), key_glyph = draw_key_path) +
  scale_color_manual(values = pal_PCS8[3:6]) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab(NULL) + ylab("individus, triés par heures de début de journée") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

ACT %>%
  filter(l == "CHE2016 000013204", substr(Tache, 1, 1) == "1") %>%
  left_join( y = select(PER, uid_PER, Genre, JoTvDeb), by = "uid_PER" ) %>%
  tab_Tri(parCol = "JoTvDeb", rev = T) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  ggplot(aes(x = hDeb/60, y = uid_PER)) +
  geom_linerange(aes(xmin = hDeb/60, xmax = hFin/60, color = Genre), key_glyph = draw_key_path) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab(NULL) + ylab("individus, triés par heures de début de journée") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 


g2 = activites %>%
  filter(ZF.ACT == "CHE2016 000013204") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  group_by(heure) %>%
  summarise(pBac = sum(ifelse(NivDip %in% c("2", "3", "4"), 1, 0)) / n() * 100,
            pSup = sum(ifelse(NivDip %in% c(     "3", "4"), 1, 0)) / n() * 100) %>%
  pivot_longer(cols = c("pBac", "pSup"), names_to = "diplome", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(linetype = diplome, group = diplome)) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) + scale_y_continuous(limits=c(0,100)) +
  xlab("heure") + ylab("%")

activites %>%
  filter(ZF.ACT == "CHE2016 000013204") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  group_by(heure) %>%
  summarise(pCad = sum(ifelse(PCS8 == "03", 1, 0)) / n() * 100,
            pOuv = sum(ifelse(PCS8 == "06", 1, 0)) / n() * 100) %>%
  pivot_longer(cols = c("pCad", "pOuv"), names_to = "categ", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(linetype = categ, group = categ))  +
  scale_linetype_manual(values = c(2, 1), name = "part", labels = c("des cadres", "des ouvrier·es")) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("heure") + ylab("%") +
  labs(caption = src_fig(emp = F))

activites %>%
  filter(ZF.ACT == "CHE2016 000013204") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  mutate(NivDip = etqNivDip(NivDip)) %>%
  group_by(heure, NivDip) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n/sum(n)) %>%
  ggplot(aes(x = heure/60, y = p * 100)) +
  geom_col(aes(fill = NivDip)) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab("heure") + ylab("%")

temp = ACT %>%
  filter(substr(Tache, 1,1) == "1", l == "CHE2016 000013204")
PER %>% filter(uid_PER %in% temp$uid_PER) %>%
  group_by(PCS8) %>% summarise(tpsMoy = mean(DuTvl)) %>%
  mutate(tpsMoy = tpsMoy/60)

g3 = ACT %>%
  filter(l == "ROU2017 000036302") %>%
  left_join( y = select(PER, uid_PER, PCS8, JoTvDeb), by = "uid_PER" ) %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  tab_Tri(parCol = "JoTvDeb", rev = T) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  ggplot(aes(x = hDeb/60, y = uid_PER)) +
  geom_linerange(aes(xmin = hDeb/60, xmax = hFin/60, color = PCS8), key_glyph = draw_key_path) +
  scale_color_manual(values = pal_PCS8[3:6], name = "catégorie\nsocio-\nprofessionnelle") +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab(NULL) + ylab("individus, triés par heures de début de journée") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

activites %>%
  filter(ZF.ACT == "ROU2017 000036302") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  mutate(NivDip = etqNivDip(NivDip)) %>%
  group_by(heure, NivDip) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n/sum(n)) %>%
  ggplot(aes(x = heure/60, y = p * 100)) +
  geom_col(aes(fill = NivDip)) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab("heure") + ylab("%")

g4 = activites %>%
  filter(ZF.ACT == "ROU2017 000036302") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  group_by(heure) %>%
  summarise(pBac = sum(ifelse(NivDip %in% c("2", "3", "4"), 1, 0)) / n() * 100,
            pSup = sum(ifelse(NivDip %in% c(     "3", "4"), 1, 0)) / n() * 100) %>%
  pivot_longer(cols = c("pBac", "pSup"), names_to = "diplome", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(linetype = diplome, group = diplome))  +
  scale_linetype(name = "niveau\nde diplôme", labels = c("baccalauréat", "supérieur")) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("heure") + ylab("%") 

activites %>%
  filter(ZF.ACT == "ROU2017 000036302") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  group_by(heure) %>%
  summarise(pCad = sum(ifelse(PCS8 == "03", 1, 0)) / n() * 100,
            pOuv = sum(ifelse(PCS8 == "06", 1, 0)) / n() * 100) %>%
  pivot_longer(cols = c("pCad", "pOuv"), names_to = "categ", values_to = "p") %>%
  ggplot(aes(x = heure/60, y = p)) +
  geom_line(aes(linetype = categ, group = categ))  +
  scale_linetype_manual(values = c(2, 1), name = "part", labels = c("des cadres", "des ouvrier·es")) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("heure") + ylab("%") +
  labs(caption = src_fig(emp = F))

ACT %>%
  filter(l == "ROU2017 000036302", substr(Tache, 1, 1) == "1") %>%
  left_join( y = select(PER, uid_PER, Genre, JoTvDeb), by = "uid_PER" ) %>%
  tab_Tri(parCol = "JoTvDeb", rev = T) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  ggplot(aes(x = hDeb/60, y = uid_PER)) +
  geom_linerange(aes(xmin = hDeb/60, xmax = hFin/60, color = Genre), key_glyph = draw_key_path) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab(NULL) + ylab("individus, triés par heures de début de journée") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

temp = ACT %>%
  filter(substr(Tache, 1,1) == "1", l == "ROU2017 000036302")
PER %>% filter(uid_PER %in% temp$uid_PER) %>%
  group_by(PCS8) %>% summarise(tpsMoy = mean(DuTvl)) %>%
  mutate(tpsMoy = tpsMoy/60)

page = cowplot::plot_grid(ggdraw() + draw_label(label = "Un site industriel\nde haute technologie", size = 8, fontface = "italic", y=1, x = 0, hjust = 0),
                          ggdraw() + draw_label(label = "Un site industriel\nde production automobile", size = 8, fontface = "italic", y=1, x = 0, hjust = 0),
                          NULL,
                          g1 + theme(legend.position = "none") + labs(subtitle = "Horaires de présence"),
                          g3 + theme(legend.position = "none") + labs(subtitle = "") + theme(axis.title.y = element_blank()),
                          cowplot::get_legend(g3),
                          g2 + theme(legend.position = "none") + labs(subtitle = "Part de diplômé·es") ,
                          g4 + theme(legend.position = "none") + theme(axis.title.y = element_blank()),
                          cowplot::get_legend(g4),
                          nrow = 3, ncol = 3,
                          align = "hv", axis = "lrt", labels = c("A", "B", "", "", "", ""),
                          rel_heights = c(1, 7, 3), rel_widths = c(50, 50, 20)) 

sortie("Horaires/Exemples sites industriels")
page %>%
  viz_Titre("Les horaires des travailleur·ses de deux sites industriels localisés", align = "g", rel_heights = c(5,95)) %>%
  viz_Pied(src_fig(emp = F), rel_heights = c(95,5)) %>% print()
off()

## 6, 13, 14

CentresComs = c("ROU2017 000049102", "CAL2011 000122302", "CHE2016 000012202")

ACT %>%
  mutate(activite = substr(Tache, 1, 1)) %>%
  filter(activite %in% c("1", "3")) %>%
  filter(l == "ROU2017 000049102") %>%
  left_join( y = select(PER, uid_PER, PCS8, JoDeb), by = "uid_PER" ) %>%
  mutate(PCS8 = plyr::revalue(PCS8, c("07" = "08", "09" = "08")),
         PCS8 = etqPCS8(PCS8)) %>%
  mutate(JoDeb = heureHHMMtoM(JoDeb)) %>%
  tab_Tri(parCol = "JoDeb", rev = T) %>%
  mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
  ggplot(aes(x = hDeb/60, y = uid_PER)) +
  geom_linerange(aes(xmin = hDeb/60, xmax = hFin/60, color = PCS8), key_glyph = draw_key_path) +
  scale_color_manual(values = c(pal_PCS8[1:6], "lightgray") ) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab(NULL) + ylab("individus, triés par heures de début de journée") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  facet_grid(rows = "activite", scales = "free_y", space = "free_y")

activites %>%
  filter(ZF.ACT == "ROU2017 000049102", substr(Tache,1,1) == "3") %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(heure, PCS8) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n/sum(n)) %>%
  ggplot(aes(x = heure/60, y = n)) +
  geom_col(aes(fill = PCS8)) +
  scale_fill_manual(values = pal_PCS8) +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4,28)) +
  xlab("heure") + ylab("%")

# Contrôle des horaires ====

# R&D : est-ce qu'on peut trouver des indices dans la base qui laisseraient à penser que les horaires
# des ouvrier·es sont plus contrôlés que ceux des cadres ?

# Pour le savoir, il faudrait cibler une tranche horaire précise (exemple : 7h à 9h) et voir si moyen
# de mesurer des micro-variations, par exemple sur des intervalles glissants d'une demi-heure ?

# Prenons l'ensemble des travailleurs commençant entre 7h45 et 8h15, quelle est la variation autour ?
# Peu concluant

PER %>%
  filter(DuTvl > 0, JoTvDeb > 7.75, JoTvDeb < 8.25) %>%
  group_by(PCS8) %>%
  summarise(moy = mean(JoTvDeb),
            et  = sd(JoTvDeb))

PER %>%
  filter(DuTvl > 0, JoTvDeb > 7.5, JoTvDeb < 8.5) %>%
  group_by(PCS8) %>%
  summarise(moy = mean(JoTvDeb),
            et  = sd(JoTvDeb))

# Intéressant : ce sont les cadres qui présentent finalement l'écart-type le plus faible...
PER %>%
  filter(DuTvl > 0, JoTvDeb > 4, JoTvDeb < 12) %>%
  group_by(PCS8) %>%
  summarise(moy = mean(JoTvDeb),
            et  = sd(JoTvDeb))
PER %>%
  filter(DuTvl > 0, JoTvFin > 12, JoTvFin < 24) %>%
  group_by(PCS8) %>%
  summarise(moy = mean(JoTvFin),
            et  = sd(JoTvFin))

PER %>%
  filter(DuTvl > 0, JoTvDeb > 4, JoTvDeb < 12) %>%
  group_by(PCS8, Genre) %>%
  summarise(moy = mean(JoTvDeb),
            et  = sd(JoTvDeb))
PER %>%
  filter(DuTvl > 0, JoTvFin > 12, JoTvFin < 24) %>%
  group_by(PCS8, Genre) %>%
  summarise(moy = mean(JoTvFin),
            et  = sd(JoTvFin))
# Intéressant : l'écart-type du matin est plus faible pour les femmes, mais pas celui du soir.


PER %>%
  filter(DuTvl > 0, JoTvDeb > 4, JoTvDeb < 12) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>%
  summarise(moy = mean(JoTvDeb),
            et  = sd(JoTvDeb))
PER %>%
  filter(DuTvl > 0, JoTvFin > 12, JoTvFin < 24) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>%
  summarise(moy = mean(JoTvFin),
            et  = sd(JoTvFin))
# On retrouve bien le paradoxe relevé par Emmanuel Munch...

# Part d'horaires non-ronds
# Peu concluant
PER %>%
  filter(DuTvl > 0, JoTvDeb >= 6, JoTvDeb <= 10) %>%
  group_by(PCS8) %>%
  summarise(partRond = sum(ifelse(JoTvDeb %in% seq(from = 6, to = 10, by = .25), 1, 0)) / n())

# Ou alors, il faudrait être en mesure de corréler l'heure d'arrivée à des trucs randoms genre
# la pluie. Ça ce serait vraiment marrant.

pluie_lyon = read_delim("Sources/infoclimat_lyon_2015.csv", delim = ";", col_names = T, skip = 5) %>%
  filter(station_id != "station_id") %>%
  mutate(temperature = as.double(temperature),
         pluie_1h = as.double(pluie_1h)) %>%
  mutate(dateJ = as.integer(substr(dh_utc, 1, 2)),
         dateM = as.factor(substr(dh_utc, 4, 5)),
         dateA = substr(dh_utc, 7, 10),
         heure = substr(dh_utc, 12, 13),
         heure = as.integer(heure)) %>% group_by(dateA, dateM, dateJ) %>%
  summarise(pluie_24h = mean(pluie_1h),
            pluie_mat = mean(ifelse(heure < 12, pluie_1h, NA), na.rm=T),
            pluie_am  = mean(ifelse(heure > 10 & heure < 22, pluie_1h, NA), na.rm=T))

PER_pluie = filter(PER, uid_ENQ == "LYO2015") %>%
  left_join(pluie_lyon, by = c("EnqDate_J" = "dateJ",
                               "EnqDate_M" = "dateM",
                               "EnqDate_A" = "dateA")) %>%
  filter(DuTvl > 0, PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  valref() %>%
  mutate(pluie_mat_f = case_when(pluie_mat == 0 ~ "pas de pluie",
                                 pluie_mat <  2 ~ "légère pluie",
                                 pluie_mat >= 2 ~ "pluie"),
         pluie_mat_f = as.factor(pluie_mat_f),
         pluie_mat_f = relevel(pluie_mat_f, "pas de pluie"),
         pluie_am_f = case_when(pluie_am == 0 ~ "pas de pluie",
                                pluie_am <  2 ~ "légère pluie",
                                pluie_am >= 2 ~ "pluie"),
         pluie_am_f = as.factor(pluie_am_f),
         pluie_am_f = relevel(pluie_am_f, "pas de pluie"))

regression(base = mutate(PER_pluie, PCS8 = etqPCS8(PCS8, num = T)),
           val = "JoTvDeb", formule = "NivDip + Age10 + Activ + ZoneDens_travMax + Genre",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "PCS8",
           titre = "Modèles H2 (par CS)\nheure d'arrivée au lieu de travail", unite = "min",
           imprDistrib = T) %>% summary()

regression(base = mutate(PER_pluie, PCS8 = etqPCS8(PCS8, num = T)),
           val = "JoTvDeb", formule = "NivDip + Age10 + Activ + ZoneDens_travMax + Genre + pluie_mat_f",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "PCS8",
           titre = "Modèles H2 (par CS)\nheure d'arrivée au lieu de travail", unite = "min",
           imprDistrib = T) %>% summary()

regression(base = mutate(PER_pluie),
           val = "JoTvDeb", formule = "PCS8 + Activ + Genre + pluie_mat_f",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60,
           titre = "", unite = "min",
           imprDistrib = T) %>% summary()

regression(base = mutate(PER_pluie, PCS8 = etqPCS8(PCS8, num = T)),
           val = "JoTvFin", formule = "NivDip + Age10 + Activ + ZoneDens_travMax + Genre + pluie_am_f",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "PCS8",
           titre = "Modèles H2 (par CS)\nheure d'arrivée au lieu de travail", unite = "min",
           imprDistrib = T) %>% summary()

regression(base = mutate(PER_pluie),
           val = "JoTvFin", formule = "PCS8 + Age10 + Activ + ZoneDens_travMax + Genre + pluie_am_f",
           retirerZ = T, legVal = "heure de début journée", facteurDiv = 1/60, colComparaison = "typoModes",
           titre = "Modèles H2 (par CS)\nheure d'arrivée au lieu de travail", unite = "min",
           imprDistrib = T) %>% summary()

# Horaires au sein du ménage ====

# Horaires de travail H/F au sein du ménage

# Contrôle : horaires de travail comparés.

# On constitue une base de ménages centrés sur un couple de référence hétéro
Menages = PER_ff %>%
  group_by(uid_MEN) %>%
  summarise(PCSMT = first(PCSMT),
            joTvDeb_H = first(na.omit(ifelse(Genre == "H" & Lien %in% c("1", "2"), JoTvDeb, NA))),
            joTvFin_H = first(na.omit(ifelse(Genre == "H" & Lien %in% c("1", "2"), JoTvFin, NA))),
            joTvDeb_F = first(na.omit(ifelse(Genre == "F" & Lien %in% c("1", "2"), JoTvDeb, NA))),
            joTvFin_F = first(na.omit(ifelse(Genre == "F" & Lien %in% c("1", "2"), JoTvFin, NA)))) %>%
  filter(!is.na(joTvDeb_H), !is.na(joTvFin_H), !is.na(joTvDeb_F), !is.na(joTvFin_F))

# Ça représente pas beaucoup de ménages !
nrow(filter(PER, !is.na(JoTvDeb))) / nrow(PER)
nrow(Menages) / nrow(MEN)

Menages %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = .25))) %>%
  group_by(joTvDeb_H) %>% summarise(joTvDeb_F = median(joTvDeb_F)) %>%
  ggplot(aes(x = joTvDeb_H, y = joTvDeb_F)) + geom_line()

g1 = Menages %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_H) %>% summarise(joTvDeb_F = mean(joTvDeb_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_H, y = joTvDeb_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée femme") +
  ylab("heure d'arrivée moyenne homme")

g2 = Menages %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_F) %>% summarise(joTvDeb_H = mean(joTvDeb_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_F, y = joTvDeb_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée homme") +
  ylab("heure d'arrivée moyenne femme")

g3 = Menages %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_H) %>% summarise(joTvFin_F = mean(joTvFin_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_H, y = joTvFin_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ femme") +
  ylab("heure de départ moyenne homme")

g4 = Menages %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_F) %>% summarise(joTvFin_H = mean(joTvFin_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_F, y = joTvFin_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ homme") +
  ylab("heure de départ moyenne femme")

cor.test(Menages$joTvDeb_H, Menages$joTvDeb_F)
cor.test(Menages$joTvFin_H, Menages$joTvFin_F)

cor.test(Menages$joTvDeb_H, Menages$joTvDeb_F, method = "spearman")
cor.test(Menages$joTvFin_H, Menages$joTvFin_F, method = "spearman")

page = cowplot::plot_grid(g1, g2, g3, g4, nrow=2, ncol=2, align="hv", labels = c("a", "b", "c", "d"))
page = viz_Titre(page, "Heures d'arrivée et de départ au lieu d'emploi\ndes deux membres des couples de référence homme-femme")
page = viz_Pied(page, src_fig())

sortie("Horaires/Heures d'arrivée et de départ des deux membres des couples H-F", taille = "carré")
print(page)
off()

# Idem, CSP+

g1 = Menages %>% filter(PCSMT %in% c("1", "2", "3AB")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_H) %>% summarise(joTvDeb_F = mean(joTvDeb_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_H, y = joTvDeb_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée femme") +
  ylab("heure d'arrivée moyenne homme")

g2 = Menages %>% filter(PCSMT %in% c("1", "2", "3AB")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_F) %>% summarise(joTvDeb_H = mean(joTvDeb_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_F, y = joTvDeb_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée homme") +
  ylab("heure d'arrivée moyenne femme")

g3 = Menages %>% filter(PCSMT %in% c("1", "2", "3AB")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_H) %>% summarise(joTvFin_F = mean(joTvFin_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_H, y = joTvFin_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ femme") +
  ylab("heure de départ moyenne homme")

g4 = Menages %>% filter(PCSMT %in% c("1", "2", "3AB")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_F) %>% summarise(joTvFin_H = mean(joTvFin_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_F, y = joTvFin_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ homme") +
  ylab("heure de départ moyenne femme")

cor.test(filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvDeb_H,
         filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvDeb_F)
cor.test(filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvFin_H,
         filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvFin_F)

cor.test(filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvDeb_H,
         filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvDeb_F, method = "spearman")
cor.test(filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvFin_H,
         filter(Menages, PCSMT %in% c("1", "2", "3AB"))$joTvFin_F, method = "spearman")

page = cowplot::plot_grid(g1, g2, g3, g4, nrow=2, ncol=2, align="hv")
page = viz_Titre(page, ml("Heures d'arrivée et de départ au lieu d'emploi",
                          "des deux membres des couples de référence hétérosexuels",
                          "ménages de PCS Ménage 1, 2 ou 3AB"), rel_heights = c(15, 85))
page = viz_Pied(page, src_fig())

print(page)

# Idem, CSP-

g1 = Menages %>% filter(PCSMT %in% c("3C", "5")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_H) %>% summarise(joTvDeb_F = mean(joTvDeb_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_H, y = joTvDeb_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée femme") +
  ylab("heure d'arrivée moyenne homme")

g2 = Menages %>% filter(PCSMT %in% c("3C", "5")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvDeb_F) %>% summarise(joTvDeb_H = mean(joTvDeb_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvDeb_F, y = joTvDeb_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(7,10)) +
  xlab("heure d'arrivée homme") +
  ylab("heure d'arrivée moyenne femme")

g3 = Menages %>% filter(PCSMT %in% c("3C", "5")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_H) %>% summarise(joTvFin_F = mean(joTvFin_F), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_H, y = joTvFin_F)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ femme") +
  ylab("heure de départ moyenne homme")

g4 = Menages %>% filter(PCSMT %in% c("3C", "5")) %>%
  mutate(across(starts_with("joTv"), ~paliers(., palier = 1))) %>%
  group_by(joTvFin_F) %>% summarise(joTvFin_H = mean(joTvFin_H), effectif = n()) %>%
  filter(effectif > seuilSignifiant) %>%
  ggplot(aes(x = joTvFin_F, y = joTvFin_H)) + geom_line() +
  scale_x_continuous(breaks = c(2:14) * 2, limits = c(4, 28)) +
  scale_y_continuous(limits=c(15,19)) +
  xlab("heure de départ homme") +
  ylab("heure de départ moyenne femme")

cor.test(filter(Menages, PCSMT %in% c("3C", "5"))$joTvDeb_H,
         filter(Menages, PCSMT %in% c("3C", "5"))$joTvDeb_F)
cor.test(filter(Menages, PCSMT %in% c("3C", "5"))$joTvFin_H,
         filter(Menages, PCSMT %in% c("3C", "5"))$joTvFin_F)

cor.test(filter(Menages, PCSMT %in% c("3C", "5"))$joTvDeb_H,
         filter(Menages, PCSMT %in% c("3C", "5"))$joTvDeb_F, method = "spearman")
cor.test(filter(Menages, PCSMT %in% c("3C", "5"))$joTvFin_H,
         filter(Menages, PCSMT %in% c("3C", "5"))$joTvFin_F, method = "spearman")

page = cowplot::plot_grid(g1, g2, g3, g4, nrow=2, ncol=2, align="hv")
page = viz_Titre(page, ml("Heures d'arrivée et de départ au lieu d'emploi",
                          "des deux membres des couples de référence hétérosexuels",
                          "ménages de PCS Ménage 1, 2 ou 3AB"), rel_heights = c(15, 85))
page = viz_Pied(page, src_fig())

print(page)

# Maintenant, on va évaluer la coprésence

# Quels types de ménage voient leurs couples être les plus coprésents à domicile ?

load("Data/PER.rds")
PER = densitesZversPER(PER)

# On va regarder ce que font les 2 membres du ménage par quart d'heure
Menages = PER %>%
  group_by(uid_MEN) %>%
  summarise(PCSMLT = first(PCSMLT),
            uid_mec  = first(na.omit(ifelse(Genre == "H" & Lien %in% c("1", "2"), uid_PER, NA))),
            uid_meuf = first(na.omit(ifelse(Genre == "F" & Lien %in% c("1", "2"), uid_PER, NA)))) %>%
  filter(!is.na(uid_mec) & !is.na(uid_meuf))

Menages = left_join(Menages,
                    select(PER, uid_PER, dsDom, dsTvl), by = c("uid_meuf" = "uid_PER"))

mean(Menages$dsDom, na.rm=T)
mean(PER_ff$dsDom, na.rm=T)
weighted.mean(filter(PER, !is.na(CoeffRecEnq))$dsDom,
              w = filter(PER, !is.na(CoeffRecEnq))$CoeffRecEnq, na.rm=T)



# Mauvaise approche...
# acti_Menages = activites %>%
#     filter(substr(uid_PER, 1, 22) %in% Menages$uid_MEN) %>%
#     filter(!is.na(ZF.ACT), nchar(ZF.ACT) == 17) %>%
#     pivot_wider(names_from = "heure", values_from = "Tache", names_prefix = "heure_")
# Menages = Menages %>%
#     left_join(select(acti_Menages, uid_PER, starts_with("heure_")), by = c("uid_mec" = "uid_PER")) %>%
#     left_join(select(acti_Menages, uid_PER, starts_with("heure_")), by = c("uid_meuf"= "uid_PER"), suffix = c("_H", "_F"))

# On fait une jointure avec activites, en colonnes
Menages = Menages %>%
  left_join(select(activites, uid_PER, heure, Tache, ZF.ACT), by = c("uid_mec" = "uid_PER")) %>%
  left_join(select(activites, uid_PER, heure, Tache, ZF.ACT), by = c("uid_meuf"= "uid_PER", "heure" = "heure"), suffix = c("_H", "_F"))

# Il faut purger pour retirer les grilles où il n'y a rien en commun
checkMenages = Menages %>%
  group_by(uid_MEN) %>%
  summarise(nAct_H = (length(Tache_H) - length(na.omit(Tache_H))) / length(Tache_H),
            nAct_F = (length(Tache_F) - length(na.omit(Tache_F))) / length(Tache_F)) %>%
  filter(nAct_H == 0 & nAct_F == 0)

listeMenages = checkMenages$uid_MEN ; remove(checkMenages)
Menages = filter(Menages, uid_MEN %in% listeMenages)

# On peut désormais vérifier si les deux personnes sont au domicile
Menages = Menages %>%
  mutate(coprezDom = (Tache_H == "010") & (Tache_F == "010"))

# Agrégation
MenagesCoprezDom = Menages %>%
  group_by(PCSMLT, uid_MEN) %>%
  summarise(tempsCoprezDom = sum(ifelse(coprezDom, pasDeTemps, 0)),
            partTempsCoprezDom = tempsCoprezDom / 1440 * 100)
# MEN = left_join(MEN, MenagesCoprezDom, by="uid_MEN")

filter(PER_ff, DuTvl > 0) %>%
  group_by(PCSMLT) %>% summarise(DuDom = mean(DuDom)) %>%
  mutate(DuDom = heureMinToHr(DuDom))

# Alors, qui est le + ensemble ? Comme attendu...
# tab1 = MEN %>%
#     filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
#     group_by(PCSMT) %>% summarise(moyTempsCoprezDom = mean(tempsCoprezDom, na.rm=T))

# Maintenant, on ne veut que les gens qui ont travaillé !
MenagesTrav = PER %>%
  group_by(uid_MEN) %>%
  summarise(trav_mec  = first(na.omit(ifelse(Genre == "H" & Lien %in% c("1", "2"), DuTvl, NA))),
            trav_meuf = first(na.omit(ifelse(Genre == "F" & Lien %in% c("1", "2"), DuTvl, NA)))) %>%
  filter(trav_mec > 0 & trav_meuf > 0) %>% select(uid_MEN)

MenagesTravMin = PER %>%
  group_by(uid_MEN) %>%
  summarise(trav_mec  = first(na.omit(ifelse(Genre == "H" & Lien %in% c("1", "2"), DuTvl, NA))),
            trav_meuf = first(na.omit(ifelse(Genre == "F" & Lien %in% c("1", "2"), DuTvl, NA)))) %>%
  filter(trav_mec > 0 | trav_meuf > 0)  %>% select(uid_MEN)

remove(PER) ; gc()

# tab2 = MEN %>%
#     filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
#     filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
#     group_by(PCSMT) %>% summarise(moyTempsCoprezDom = mean(tempsCoprezDom, na.rm=T), n = n())
# 
# left_join(tab1, tab2, by="PCSMT", suffix = c(".tout", ".trav")) %>%
#     pivot_longer(cols = starts_with("moyTemps"), values_to = "tps", names_to = "mesure") %>%
#     ggplot(aes(x = PCSMT, y = tps/60)) +
#     geom_col(aes(fill = mesure), position = "dodge") +
#     scale_fill_hue(name = "couples hétérosexuels",
#                    labels = c("tous confondus", "dont les deux\nmembres travaillent")) +
#     xlab("PCS Ménage") + ylab("temps passé ensemble à domicile (heures)")

# Calculs simples : synchronisation des tâches ? des lieux ?

Menages = Menages %>%
  mutate(ZF.ACT_F = ifelse(nchar(ZF.ACT_F) == 17, ZF.ACT_F, NA),
         ZF.ACT_H = ifelse(nchar(ZF.ACT_H) == 17, ZF.ACT_H, NA)) %>%
  mutate(meme_act = Tache_H == Tache_F,
         meme_zf  = ZF.ACT_H == ZF.ACT_F)

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  group_by(uid_MEN) %>%
  summarise(temps_meme_act = sum(ifelse(meme_act, pasDeTemps, 0)),
            partTemps_meme_act = temps_meme_act / 1440 * 100) %>%
  summarise(temps_meme_act = mean(temps_meme_act), partTemps_meme_act = mean(partTemps_meme_act))

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  group_by(PCSMLT, uid_MEN) %>%
  summarise(temps_meme_act = sum(ifelse(meme_act, pasDeTemps, 0)),
            partTemps_meme_act = temps_meme_act / 1440 * 100) %>%
  group_by(PCSMLT) %>% summarise(partTemps_meme_act = mean(partTemps_meme_act))

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  left_join(select(MEN, uid_MEN, ZoneDens), by="uid_MEN") %>%
  group_by(ZoneDens, uid_MEN) %>%
  summarise(temps_meme_act = sum(ifelse(meme_act, pasDeTemps, 0)),
            partTemps_meme_act = temps_meme_act / 1440 * 100) %>%
  group_by(ZoneDens) %>% summarise(partTemps_meme_act = mean(partTemps_meme_act))

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  group_by(uid_MEN) %>%
  summarise(temps_meme_zf = sum(ifelse(meme_zf, pasDeTemps, 0)),
            partTemps_meme_zf = temps_meme_zf / 1440 * 100) %>%
  summarise(temps_meme_zf = mean(temps_meme_zf, na.rm=T),
            partTemps_meme_zf = mean(partTemps_meme_zf, na.rm=T))

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  group_by(PCSMLT, uid_MEN) %>%
  summarise(temps_meme_zf = sum(ifelse(meme_zf, pasDeTemps, 0)),
            partTemps_meme_zf = temps_meme_zf / 1440 * 100) %>%
  group_by(PCSMLT) %>% summarise(partTemps_meme_zf = mean(partTemps_meme_zf, na.rm=T))

Menages %>%
  filter(uid_MEN %in% MenagesTravMin$uid_MEN) %>%
  left_join(select(MEN, uid_MEN, ZoneDens), by="uid_MEN") %>%
  group_by(ZoneDens, uid_MEN) %>%
  summarise(temps_meme_zf = sum(ifelse(meme_zf, pasDeTemps, 0)),
            partTemps_meme_zf = temps_meme_zf / 1440 * 100) %>%
  group_by(ZoneDens) %>% summarise(partTemps_meme_zf = mean(partTemps_meme_zf, na.rm=T))


# Voyons qui reste seul·e au domicile...
Menages = Menages %>%
  mutate(dom_femmeSeule = (Tache_H != "010") & (Tache_F == "010"),
         dom_hommeSeul  = (Tache_H == "010") & (Tache_F != "010"))

sortie("Horaires/Sans conjoint·e à domicile selon heure")
Menages %>%
  left_join(select(MEN, uid_MEN, MenEnfants), by="uid_MEN") %>%
  filter(uid_MEN %in% PER_ff$uid_MEN) |>
  group_by(heure) %>%
  summarise(nFemmesSeules = sum(ifelse(dom_femmeSeule, 1, 0)),
            nHommesSeuls  = sum(ifelse(dom_hommeSeul,  1, 0)),
            n = n()) %>%
  mutate(pFemmesSeules = nFemmesSeules / n, pHommesSeuls = nHommesSeuls/n) %>%
  pivot_longer(cols = c("pFemmesSeules", "pHommesSeuls"), values_to = "p", names_to = "genre") %>%
  ggplot(aes(x = heure/60, y = p * 100)) + geom_line(aes(color = genre)) +
  scale_x_continuous(breaks = c(2:13) * 2) +
  scale_color_hue(name = "Genre", labels = c("Femmes", "Hommes")) +
  xlab("heure") + ylab("part des travailleur·ses") +
  labs(title = "Part des travailleur·ses se trouvant\nsans leur conjoint·e à leur domicile",
       caption = src_fig())
off()

MenagesSoloDom = Menages %>%
  group_by(uid_MEN) %>%
  summarise(tempsFemmeSeule = sum(ifelse(dom_femmeSeule, pasDeTemps, 0)),
            tempsHommeSeul  = sum(ifelse(dom_hommeSeul,  pasDeTemps, 0)),
            partTempsFemmeSeule = tempsFemmeSeule / 1440 * 100,
            partTempsHommeSeul  = tempsHommeSeul  / 1440 * 100)
MEN = left_join(MEN, MenagesSoloDom, by="uid_MEN")

MEN %>%
  filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
  filter(uid_MEN %in% PER_ff$uid_MEN) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T),
            tempsHommeSeul  = mean(tempsHommeSeul,  na.rm=T))

tab1 = MEN %>%
  filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
  filter(!uid_MEN %in% PER_ff$uid_MEN) %>%
  group_by(PCSMT) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T),
            tempsHommeSeul  = mean(tempsHommeSeul,  na.rm=T))

tab2 = MEN %>%
  filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
  filter(uid_MEN %in% PER_ff$uid_MEN) %>%
  group_by(PCSMT) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T),
            tempsHommeSeul  = mean(tempsHommeSeul,  na.rm=T),
            ratio = tempsFemmeSeule/tempsHommeSeul)

sortie("Horaires/Temps seul·e selon biactivité")
left_join(pivot_longer(tab1, cols = starts_with("temps"), names_to = "Genre", values_to = "temps"),
          pivot_longer(tab2, cols = starts_with("temps"), names_to = "Genre", values_to = "temps"),
          by=c("PCSMT", "Genre"), suffix = c(".tout", ".trav")) %>%
  mutate(Genre = plyr::revalue(Genre, c("tempsFemmeSeule" = "femmes", "tempsHommeSeul" = "hommes"))) %>%
  pivot_longer(cols = starts_with("temps"), values_to = "tps", names_to = "mesure") %>%
  ggplot(aes(x = PCSMT, y = tps/60)) +
  geom_col(aes(fill = mesure), position = "dodge") +
  scale_fill_manual(name = "couples hommes-femmes",
                    labels = c("inactif·ves", "dont les deux\nmembres travaillent"),
                    values = c("grey55", "grey85")) +
  xlab("PCS Ménage") + ylab("heures passées seul·e à domicile") +
  facet_wrap(~Genre) +
  geom_text(aes(label = heureMinToHr(tps), group = mesure),
            position = position_dodge(width = 1), angle = 90, hjust=1.1) +
  labs(title = "Temps passé sans le/la conjoint·e à domicile (couples H-F)\nSituation des couples biactifs",
       caption = src_fig()) + theme(legend.position = "bottom")
off()

sortie("Horaires/Temps seul·e selon genre et enfants")
MEN %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT) & !is.na(MenEnfants)) %>%
  group_by(PCSMT, MenEnfants) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T),
            tempsHommeSeul  = mean(tempsHommeSeul,  na.rm=T)) %>%
  pivot_longer(cols = starts_with("temps"), names_to = "Genre", values_to = "tps") %>%
  mutate(Genre = plyr::revalue(Genre, c("tempsFemmeSeule" = "femmes", "tempsHommeSeul" = "hommes"))) %>%
  ggplot(aes(x = PCSMT, y = tps/60)) +
  geom_col(aes(fill = MenEnfants), position = "dodge") +
  xlab("PCS Ménage") + ylab("heures passées seul·e à domicile") +
  scale_fill_manual(values=c("grey55", "grey85"), name = "Enfants\nà charge", labels = c("non", "oui")) +
  facet_wrap(~Genre) +
  geom_text(aes(label = heureMinToHr(tps), group = MenEnfants),
            position = position_dodge(width = 1), angle = 90, hjust=1.1) +
  labs(title = "Temps passé sans le/la conjoint·e à domicile (couples homme-femme)\nSelon le genre et la présence d'enfants de 16 ans ou moins",
       caption = src_fig())
off()

enftPlusJeune = PER %>%
  group_by(uid_MEN) %>%
  summarise(ageMinEnfant = min(ifelse(Lien == "3", Age, NA), na.rm=T)) %>%
  mutate(ageMinEnfant = ifelse(is.infinite(ageMinEnfant), NA, ageMinEnfant))

MEN = left_join(MEN, enftPlusJeune, by="uid_MEN")

sortie("Horaires/Temps seul·e selon genre et âge enfants", taille = "carré")
MEN %>%
  filter(!PCSMT %in% c("6", "7e", "7i") & !is.na(PCSMT)) %>%
  mutate(ageMinEnfant = case_when(ageMinEnfant < 4                       ~ "Plus jeune enfant :\n00 à 3 ans",
                                  ageMinEnfant >=4  & ageMinEnfant < 10 ~ "Plus jeune enfant :\n04 à 9 ans",
                                  ageMinEnfant >=10 & ageMinEnfant < 15 ~ "Plus jeune enfant :\n10 à 14 ans",
                                  ageMinEnfant >=15 & ageMinEnfant < 19 ~ "Plus jeune enfant :\n15 à 18 ans",
                                  T ~ "pas d'enfant\nà charge")) %>%
  group_by(PCSMLT, ageMinEnfant) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T),
            tempsHommeSeul  = mean(tempsHommeSeul,  na.rm=T)) %>%
  pivot_longer(cols = starts_with("temps"), names_to = "Genre", values_to = "tps") %>%
  mutate(Genre = plyr::revalue(Genre, c("tempsFemmeSeule" = "femmes", "tempsHommeSeul" = "hommes"))) %>%
  ggplot(aes(x = PCSMLT, y = tps/60)) +
  geom_col(aes(fill = Genre), position = "dodge") +
  xlab("PCS Ménage") + ylab("heures passées seul·e à domicile") +
  facet_wrap(~ageMinEnfant) +
  geom_text(aes(label = heureMinToHr(tps), group = Genre),
            position = position_dodge(width = 1), angle = 90, hjust=1.1) +
  labs(title = "Temps passé sans le/la conjoint·e au domicile (couples homme-femme)\nSelon le genre et l'âge du plus jeune enfant",
       caption = src_fig())
off()

sortie("Horaires/Temps seule femmes selon âge et enfants", taille = "carré")
MEN %>%
  filter(!PCSMLT %in% c("6", "7e", "7i") & !is.na(PCSMT) & !is.na(MenEnfants)) %>%
  left_join(summarise(group_by(PER, uid_MEN),
                      ageF = first(na.omit(ifelse(Lien %in% c("1", "2") & Genre == "F", Age, NA)))),
            by = "uid_MEN") %>%
  filter(!is.na(ageF) & ageF < 69) %>%
  mutate(ageF = etqAge(ageF, min = 20, max = 69, pas = 10)) %>%
  group_by(PCSMLT, ageF, MenEnfants) %>%
  summarise(tempsFemmeSeule = mean(tempsFemmeSeule, na.rm=T)) %>%
  ggplot(aes(x = PCSMLT, y = tempsFemmeSeule/60)) +
  geom_col(aes(fill = MenEnfants), position = "dodge") +
  xlab("PCS Ménage") + ylab("heures passées seule à domicile") +
  facet_wrap(~ageF) +
  scale_fill_manual(values=c("grey55", "grey85"), name = "Enfants\nà charge", labels = c("non", "oui")) +
  geom_text(aes(label = heureMinToHr(tempsFemmeSeule), group = MenEnfants),
            position = position_dodge(width = 1), angle = 90, hjust=1.1, size=2.3) +
  labs(title = ml("Temps sans son conjoint au domicile (femmes des couples hommes-femmes)",
                  "selon l'âge et la présence d'enfants de moins de 16 ans"),
       caption = src_fig())
off()

Taches = nv(noms = c("10", "30", "31", "32", "33", "34", "35", "41", "42", "43", "44", "51", "52",
                     "53", "54", "60", "70", "81", "82", "91", "99"),
            valeurs = c("1- lieu d'emploi",
                        rep("2- commerce", times = 6),
                        rep("3- service/soin", times = 4),
                        rep("4- lieu de loisirs", times = 3),
                        "5- visite",
                        rep("6- accompagnement", times = 2),
                        "1- lieu d'emploi", "2- commerce",
                        "8- autres",
                        "7- en déplacement"))

pal_Taches = c("skyblue", "salmon3", "indianred2", "tan2", "lightgoldenrod", "plum1", "thistle3", "gray")

tab1 = Menages %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_H = substr(Tache_H, 1, 2)) %>%
  filter(dom_femmeSeule, !Tache_H %in% c("02", as.character(c(21:29)))) %>%
  mutate(Tache_H = plyr::revalue(Tache_H, Taches)) %>%
  group_by(PCSMLT, Tache_H) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

Menages %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_H = substr(Tache_H, 1, 2)) %>%
  filter(dom_femmeSeule, !Tache_H %in% c("02", as.character(c(21:29)))) %>%
  mutate(Tache_H = plyr::revalue(Tache_H, Taches)) %>%
  group_by(Tache_H) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

Menages %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_F = substr(Tache_F, 1, 2)) %>%
  filter(dom_hommeSeul, !Tache_F %in% c("02", as.character(c(21:29)))) %>%
  mutate(Tache_F = plyr::revalue(Tache_F, Taches)) %>%
  group_by(Tache_F) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

tab2 = Menages %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_F = substr(Tache_F, 1, 2)) %>%
  filter(dom_hommeSeul, !Tache_F %in% c("02", as.character(c(21:29)))) %>%
  mutate(Tache_F = plyr::revalue(Tache_F, Taches)) %>%
  group_by(PCSMLT, Tache_F) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

sortie("Horaires/Activité conjoint·e quand autre seul·e à dom")
left_join(rename(tab1, Tache = Tache_H),
          rename(tab2, Tache = Tache_F), by=c("PCSMLT", "Tache"), suffix = c("_H", "_F")) %>%
  filter(!PCSMLT %in% c("6", "7i", "7e") & !is.na(PCSMLT)) %>%
  pivot_longer(cols = c("p_H", "p_F"), values_to = "p", names_to = "Genre") %>%
  mutate(Genre = case_when(Genre == "p_H" ~ "l'individu est une femme,\nle conjoint est un homme",
                           Genre == "p_F" ~ "l'individu est un homme,\nla conjointe est une femme")) %>%
  ggplot(aes(x = PCSMLT, y = p)) +
  geom_col(aes(fill = Tache)) + scale_fill_manual(values = pal_Taches, name = "Lieu d'activité") +
  facet_wrap(~Genre) +
  xlab("PCS Ménage") + ylab("Part du volume horaire total") +
  labs(title = "Lorsqu'une personne est chez elle sans son/sa conjoint·e,\noù se trouve celle ou celui-ci ?",
       subtitle = "Couples homme-femme biactifs,\ntravaillant tou·tes deux le jour d'enquête",
       caption = src_fig())
off()

# On refait la même chose, cette fois-ci en prenant en compte le fait que certaines personnes ne travaillent pas ?

Taches = nv(noms = c("01", "02", "30", "31", "32", "33", "34", "35", "41", "42", "43", "44", "51", "52",
                     "53", "54", "60", "70", "82", "91", "99"),
            valeurs = c("1- domicile",
                        "2- autre hébergement",
                        rep("3- commerce", times = 6),
                        rep("4- service/soin", times = 4),
                        rep("5- lieu de loisirs", times = 3),
                        "6- visite",
                        rep("7- accompagnement", times = 2),
                        "3- commerce",
                        "9- autres",
                        "8- en déplacement"))

pal_Taches = c("darkolivegreen2", "aquamarine3", "salmon", "indianred", "tan3", "lightgoldenrod", "plum", "thistle2", "gray")

Menages = left_join(Menages, select(MEN, uid_MEN, MenEnfants), by = "uid_MEN")

Menages %>%
  filter(!uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_F = substr(Tache_F, 1, 2), Tache_H = substr(Tache_H, 1, 2)) %>%
  filter(Tache_F == "10", !Tache_H %in% c(as.character(c(21:29)))) %>%
  mutate(Tache_H = plyr::revalue(Tache_H, Taches)) %>%
  group_by(Tache_H) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

tab1 = Menages %>%
  filter(!uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_F = substr(Tache_F, 1, 2), Tache_H = substr(Tache_H, 1, 2)) %>%
  filter(Tache_F == "10", !Tache_H %in% c(as.character(c(21:29)))) %>%
  mutate(Tache_H = plyr::revalue(Tache_H, Taches)) %>%
  group_by(MenEnfants, PCSMLT, Tache_H) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

Menages %>%
  filter(!uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_H = substr(Tache_H, 1, 2), Tache_F = substr(Tache_F, 1, 2)) %>%
  filter(Tache_H == "10", !Tache_F %in% c(as.character(c(21:29)))) %>%
  mutate(Tache_F = plyr::revalue(Tache_F, Taches)) %>%
  group_by(Tache_F) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

tab2 = Menages %>%
  filter(!uid_MEN %in% MenagesTrav$uid_MEN) %>%
  mutate(Tache_H = substr(Tache_H, 1, 2), Tache_F = substr(Tache_F, 1, 2)) %>%
  filter(Tache_H == "10", !Tache_F %in% c(as.character(c(21:29)))) %>%
  mutate(Tache_F = plyr::revalue(Tache_F, Taches)) %>%
  group_by(MenEnfants, PCSMLT, Tache_F) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n) * 100)

sortie("Horaires/Activité conjoint·e quand autre au travail")
left_join(rename(tab1, Tache = Tache_H),
          rename(tab2, Tache = Tache_F), by=c("PCSMLT", "Tache", "MenEnfants"), suffix = c("_H", "_F")) %>%
  filter(!PCSMLT %in% c("6", "7i", "7e") & !is.na(PCSMLT)) %>%
  pivot_longer(cols = c("p_H", "p_F"), values_to = "p", names_to = "Genre") %>%
  mutate(Genre = case_when(Genre == "p_H" ~ "l'individu est une femme,\nle conjoint est un homme",
                           Genre == "p_F" ~ "l'individu est un homme,\nla conjointe est une femme")) %>%
  ggplot(aes(x = PCSMLT, y = p)) +
  geom_col(aes(fill = Tache)) + scale_fill_manual(values = pal_Taches, name = "Lieu d'activité") +
  facet_wrap(~Genre) +
  xlab("PCS Ménage") + ylab("Part du volume horaire total") +
  labs(title = "Que fait une personne inoccupée ce jour-là\nlorsque son/sa conjoint·e est sur son lieu de travail ?",
       subtitle = "Couples homme-femme biactifs,\ndont l'un·e ne se rend pas sur son lieu de travail",
       caption = src_fig())
off()

# C'est pas très clair, il va falloir faire un modèle...

# Unité de base du modèle = individu faisant partie du couple de référence d'un ménage hétéro
PER_mod = PER %>%
  filter(uid_PER %in% unique(c(Menages$uid_mec, Menages$uid_meuf))) %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  filter(!PCSMLT %in% c("6", "7e","7i"), Age > 19 & Age < 70) %>%
  left_join(select(MEN, uid_MEN, ageMinEnfant), by = "uid_MEN") %>%
  mutate(ageMinEnfant = case_when(ageMinEnfant < 4                      ~ "plus jeune enfant : 00 à 3 ans",
                                  ageMinEnfant >=4  & ageMinEnfant < 10 ~ "plus jeune enfant : 04 à 9 ans",
                                  ageMinEnfant >=10 & ageMinEnfant < 15 ~ "plus jeune enfant : 10 à 14 ans",
                                  ageMinEnfant >=15 & ageMinEnfant < 19 ~ "plus jeune enfant : 15 à 18 ans",
                                  T ~ "pas d'enfant à charge"),
         ageMinEnfant = factor(ageMinEnfant, levels = c("pas d'enfant à charge",
                                                        "plus jeune enfant : 00 à 3 ans",
                                                        "plus jeune enfant : 04 à 9 ans",
                                                        "plus jeune enfant : 10 à 14 ans",
                                                        "plus jeune enfant : 15 à 18 ans"))) 

MSD_desag = MenagesSoloDom %>%
  pivot_longer(cols = starts_with("temps"), names_to = "Genre", values_to = "tpsSeule") %>%
  mutate(Genre = case_when(Genre == "tempsFemmeSeule" ~ "F", Genre == "tempsHommeSeul" ~ "H"),
         Genre = factor(Genre, levels = c("F", "H")))

PER_mod = PER_mod %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  left_join(select(MSD_desag, uid_MEN, Genre, tpsSeule), by=c("uid_MEN", "Genre"))

ggplot(PER_mod, aes(x = tpsSeule)) + geom_density()
ggplot(PER_mod, aes(x = log(tpsSeule))) + geom_density()

sortie("Horaires/Temps seul·e modèles de synthèse", taille = "carré")
regressionLog(base = valref(PER_mod),
              val = "tpsSeule",
              formule = "PCSMLT + ageMinEnfant + Age10 + ZoneDens",
              unite = "mn",
              colComparaison = "Genre")
off()

PER_mod2 = PER %>%
  filter(uid_PER %in% unique(c(Menages$uid_mec, Menages$uid_meuf))) %>%
  filter(uid_MEN %in% MenagesTrav$uid_MEN) %>%
  filter(!PCSMLT %in% c("6", "7e","7i"), Age > 19 & Age < 70) %>%
  left_join(select(MEN, uid_MEN, ageMinEnfant), by = "uid_MEN") %>%
  left_join(select(MSD_desag, uid_MEN, Genre, tpsSeule), by=c("uid_MEN", "Genre"))

regressionLog(base = valref(PER_mod2),
              val = "tpsSeule",
              formule = "PCSMLT + ageMinEnfant + Age + ZoneDens",
              unite = "mn",
              colComparaison = "Genre")

ggplot(PER_mod2, aes(x = log(tpsSeule))) + geom_density()


# Présence simultanée =====

rapport("Analyse des rythmes", prim = T)

# Est-ce que les gens qui se ressemblent sont plus présent·es "en même temps" que l'ensemble ?

# Trois façons de tester :
# - par ZT, voire par ZF
# - avec des secteurs imaginaires, tels que les transports
# - avec de la SIG et des rayons

# Tests statistiques possibles :
# - comparer la part de gens de sa propre catégorie dans le voisinage défini sur les 24 heures
#   par rapport à la part de gens de sa propre catégorie de la population générale, et à la part
#   de gens de sa propre catégorie dans son quartier de résidence par exemple
# - calculer ainsi un taux de surreprésentation moyen sur 24h, ou uniquement durant le temps de
#   travail, ou uniquement de non-présence à domicile
# - travailler à partir de la table "activites"

partAuTravail = activites %>%
  filter(substr(Tache, 1,1) == "1") %>%
  group_by(heure, PCS8) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n / sum(n))

partAuTravail %>%
  ggplot(aes(x = heure, y = p)) + geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part de chaque PCS sur les lieux de travail")

partAuTravail %>%
  ggplot(aes(x = heure, y = n)) + geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Stock de chaque PCS sur les lieux de travail")

principalesEnquêtes = table(PER$uid_ENQ) %>%
  as.data.frame() %>% tab_Tri(parCol = "Freq", rev = T) %>% head(n = 12) %>%
  .$Var1 %>% as.character()

partAuTravail = activites %>%
  mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
  filter(substr(Tache, 1,1) == "1", uid_ENQ %in% principalesEnquêtes) %>%
  group_by(uid_ENQ, heure, PCS8) %>% summarise(n = n()) %>%
  group_by(uid_ENQ, heure) %>% mutate(p = n / sum(n))

partAuTravail %>%
  ggplot(aes(x = heure, y = p)) + geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part de chaque PCS sur les lieux de travail selon l'enquête")+
  facet_wrap(~uid_ENQ)

partAuTravail = activites %>%
  left_join(select(PER, uid_PER, EnqDate_JS), by="uid_PER") %>%
  filter(substr(Tache, 1,1) == "1") %>%
  group_by(EnqDate_JS, heure, PCS8) %>% summarise(n = n()) %>%
  group_by(EnqDate_JS, heure) %>% mutate(p = n / sum(n))

partAuTravail %>%
  ggplot(aes(x = heure, y = p)) + geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part de chaque PCS sur les lieux de travail selon le jour de la semaine")+
  facet_wrap(~EnqDate_JS)

DEP %>%
  filter(substr(O_Motif,1,1) == "1" & substr(D_Motif,1,1) == "0") %>%
  mutate(heureA = trunc(heureHHMMtoM(D_Hr)/15)*15) %>%
  filter(heureA >= 240 & heureA <= 1440) %>%
  left_join(select(PER, uid_PER, PCS8), by="uid_PER") %>%
  group_by(PCS8, heureA) %>% summarise(n = n()) %>%
  group_by(heureA) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = heureA/60, y = p)) +
  geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part des trajets retours du travail au domicile, par PCS")

DEP %>%
  filter(substr(O_Motif,1,1) == "0" & substr(D_Motif,1,1) == "1") %>%
  mutate(heureA = trunc(heureHHMMtoM(D_Hr)/15)*15) %>%
  filter(heureA >= 240 & heureA <= 1440) %>%
  left_join(select(PER, uid_PER, PCS8), by="uid_PER") %>%
  group_by(PCS8, heureA) %>% summarise(n = n()) %>%
  group_by(heureA) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = heureA/60, y = p)) +
  geom_area(aes(fill = PCS8), position = "stack") +
  scale_fill_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part des trajets retours du travail au domicile, par PCS")

DEP %>%
  filter(substr(O_Motif,1,1) == "1" & substr(D_Motif,1,1) == "0" & uid_ENQ %in% principalesEnquêtes) %>%
  mutate(heureA = trunc(heureHHMMtoM(D_Hr)/15)*15) %>%
  filter(heureA >= 240 & heureA <= 1440) %>%
  left_join(select(PER, uid_PER, PCS8, CoeffEnq), by="uid_PER") %>%
  group_by(uid_ENQ, PCS8, heureA) %>% summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  group_by(uid_ENQ, heureA) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = heureA/60, y = p)) +
  geom_line(aes(color = PCS8)) +
  scale_color_manual(values = c(pal_PCS8, "grey")) +
  labs(title = "Part des trajets allers du domicile au travail, par PCS et par enquête") +
  facet_wrap(~uid_ENQ)

pal_PCS42S_c = c(pal_PCS8[1], "lightgoldenrod1", "lightgoldenrod2", "lightgoldenrod3",
                 "lightslateblue", "slateblue", "midnightblue",
                 "plum1", "plum2", "plum3", "plum4",
                 "pink", "pink1", "pink2", "pink3",
                 "firebrick2", "firebrick3", "firebrick4")

activites %>%
  filter(substr(Tache, 1,1) == "1" & !is.na(PCS42S)) %>%
  group_by(heure, PCS42S) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = heure, y = p)) + geom_area(aes(fill = PCS42S), position = "stack") +
  scale_fill_manual(values = c("gray", 
                               pal_PCS42S_c,
                               rep("gray",40))) +
  labs(title = "Part de chaque PCS (détaillée) sur les lieux de travail")

activites %>%
  filter(substr(Tache, 1,1) == "1" & !is.na(PCS42S)) %>%
  group_by(heure, PCS42S) %>% summarise(n = n()) %>%
  group_by(heure) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = heure, y = n)) + geom_area(aes(fill = PCS42S), position = "stack") +
  scale_fill_manual(values = c("gray", pal_PCS8[1],
                               "lightgoldenrod1", "lightgoldenrod2", "lightgoldenrod3",
                               "lightslateblue", "slateblue", "midnightblue",
                               "plum1", "plum2", "plum3", "plum4",
                               "pink", "pink1", "pink2", "pink3",
                               "firebrick2", "firebrick3", "firebrick4",
                               rep("gray",40))) +
  labs(title = "Stock de chaque PCS (détaillée) sur les lieux de travail") +
  coord_cartesian(ylim = c(0, 3000))

# Synchronisation et exposition ====

initMémoire(BasesCharger = "PER")
load("Data/activites.rds") ; gc()

PER_ff = init_PER_ff(PER) ; remove(PER)
PER_ff$NivDip = NivEtuVersNivDip(PER_ff$NivEtu)
gc()

activites = left_join(activites, select(PER_ff, uid_PER, PCS8, PCS42S, NivDip, Age10, DuTvl, pendule), by="uid_PER")
gc()

expositionPendule = function(var)
{
  rapport("Calculs de taux d'exposition simultanée avec la variable", var)
  colnames(activites)[colnames(activites) == var] = "var"
  
  activites = filter(activites, !is.na(var))
  
  tabRef = group_by(activites, heure, ZT.ACT, var) %>%
    summarise(nRef = sum(CoeffRecEnq, na.rm=T)) %>%
    group_by(heure, ZT.ACT) %>%
    mutate(pRef = nRef / sum(nRef))
  gc()
  
  activites = activites |>
    mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
           dsDom = factor(dsDom, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
           dsTvl = discretisation(dsTvl, methode = "quartiles"),
           dsTvl = factor(dsTvl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")))
  gc()
  
  actiTr = activites %>%
    filter(uid_PER %in% PER_ff$uid_PER) %>%
    left_join(tabRef, by=c("heure" = "heure", "ZT.ACT" = "ZT.ACT", "var" = "var")) %>%
    filter(!is.na(ZT.ACT)) %>%
    group_by(dsDom, dsTvl, var, uid_PER) %>% summarise(mTx = mean(pRef),
                                                       CoeffRecEnq = first(CoeffRecEnq)) %>%
    group_by(dsDom, dsTvl, var) %>% summarise(mTx = median(mTx),
                                              q1Tx = quantile(mTx, probs=.25),
                                              q3Tx = quantile(mTx, probs=.75))
  
  tabRefHorsDom = group_by(activites, heure, ZT.ACT, var) %>%
    filter(!substr(Tache,1,1) == "0") %>%
    summarise(nRef = sum(CoeffRecEnq, na.rm=T)) %>%
    group_by(heure, ZT.ACT) %>%
    mutate(pRef = nRef / sum(nRef))
  
  actiHorsDom = activites %>%
    filter(uid_PER %in% PER_ff$uid_PER) %>%
    filter(!substr(Tache,1,1) == "0") %>%
    left_join(tabRefHorsDom, by=c("heure" = "heure", "ZT.ACT" = "ZT.ACT", "var" = "var")) %>%
    filter(!is.na(ZT.ACT)) %>%
    group_by(dsDom, dsTvl, var, uid_PER) %>%
    summarise(mTx = mean(pRef), CoeffRecEnq = first(CoeffRecEnq)) %>%
    group_by(dsDom, dsTvl, var) %>% summarise(mTx_Ext = median(mTx), 
                                              q1Tx_Ext = quantile(mTx, probs=.25),
                                              q3Tx_Ext = quantile(mTx, probs=.75))
  
  actiRefMoy = activites %>%
    mutate(uid_ENQ = substr(uid_PER, 1, 7)) %>%
    group_by(dsDom, dsTvl, heure, uid_ENQ, var) %>% summarise(nRef = sum(CoeffRecEnq, na.rm=T)) %>%
    group_by(dsDom, dsTvl, heure, uid_ENQ) %>% mutate(pRef = nRef / sum(nRef)) %>%
    group_by(dsDom, dsTvl, uid_ENQ, var) %>% summarise(mRef = mean(pRef), n = sum(nRef)) %>%
    group_by(dsDom, dsTvl, var) %>% summarise(mRef_24h = median(mRef))
  
  colnames(PER_ff)[colnames(PER_ff) == var] = "var"
  
  tabRef_PER = PER_ff %>%
    group_by(ZT, var) %>% summarise(nRef = sum(CoeffRecEnq, na.rm=T)) %>%
    group_by(ZT) %>% mutate(pRef = nRef / sum(nRef))
  
  ref_PER = left_join(PER_ff, tabRef_PER, by=c("ZT" = "ZT", "var" = "var")) %>%
    filter(!is.na(CoeffRecEnq)) %>%
    group_by(var) %>% summarise(mRef_Dom = median(pRef),
                                q1Tx_Dom = quantile(pRef, probs=.25),
                                q3Tx_Dom = quantile(pRef, probs=.75))
  
  actiTr = left_join(actiTr, ref_PER, by="var") %>%
    mutate(surrep_Dom = (mTx/mRef_Dom - 1) * 100)
  actiTr = left_join(actiTr, actiRefMoy, by=c("dsDom", "dsTvl", "var")) %>%
    mutate(surrep_24h = (mTx/mRef_24h - 1) * 100)
  actiTr = left_join(actiTr, actiHorsDom, by= c("dsDom", "dsTvl", "var")) %>%
    mutate(surrep_Ext = (mTx_Ext/mRef_Dom - 1) * 100)
  
  #actiTr = select(actiTr, var, mTx, mTx_Ext, mRef_Dom, mRef_24h, surrep_24h, surrep_Dom, surrep_Ext)
  
  return(actiTr)
}

tabPCS8 = expositionPendule("PCS8") 

t = tabPCS8 %>%
  filter(var %in% c("02", "03", "04", "05", "06"), !is.na(dsDom), !is.na(dsTvl)) %>%
  mutate(var = etqPCS8(var, rev = T)) %>%
  rename(Dom = mRef_Dom, Ext = mTx_Ext) %>%
  pivot_longer(cols = c("Dom", "Ext"), names_to = "DomExt", values_to = "v_mTx") %>%
  rename(Dom = q1Tx_Dom, Ext = q1Tx_Ext) %>%
  pivot_longer(cols = c("Dom", "Ext"), names_to = "DomExt2", values_to = "v_q1") %>%
  rename(Dom = q3Tx_Dom, Ext = q3Tx_Ext) %>%
  pivot_longer(cols = c("Dom", "Ext"), names_to = "DomExt3", values_to = "v_q3") %>%
  filter(DomExt == DomExt2, DomExt == DomExt3) %>%
  select(-DomExt2, -DomExt3) %>%
  mutate(DomExt = factor(DomExt, levels = c("Ext", "Dom")),
         v_mTx = v_mTx * 100, v_q1 = v_q1 * 100, v_q3 = v_q3 * 100)

t$dsDom = factor(t$dsDom, labels = paste0("Sect. de résidence\n",
                                               c("1er", "2e", "3e", "4e"),
                                               " quartile de densité"))
t$dsTvl = factor(t$dsTvl, labels = paste0("Sect. de travail\n",
                                               c("1er", "2e", "3e", "4e"),
                                               " quartile de densité"))
g = ggplot(t, aes(x = v_mTx, y = var, group=DomExt)) +
  geom_linerange(aes(xmin = v_q1, xmax = v_q3, colour = var, linetype = "intervalle 1er quartile - 3e quartile"),
                 position = position_dodge(width = .6), key_glyph = "path") +
  geom_point(aes(colour = var, shape = DomExt), position = position_dodge(width = .6)) +
  scale_colour_manual(values = rev(pal_PCS8[2:6])) +
  facet_grid(dsTvl~dsDom) +
  xlab("part des personnes de même catégorie dans le secteur (%)") +
  ylab("catégorie socioprofessionnelle") +
  labs(title = ml("Part médiane des personnes de la même catégorie sociale",
                  "par quartier d'habitation, ou en moyenne sur le temps passé hors domicile"),
       subtitle = "en fonction du type de secteur de résidence et de travail",
       caption = src_fig(emp=F)) +
  guides(colour = "none") +
  theme(legend.position = "bottom") +
  scale_shape(name = "Mesure", labels = c("médiane sur temps\nhors domicile",
                                          "par rapport au secteur\nde domicile")) +
  scale_linetype(name = NULL)

sortie("Horaires/Exposition PCS8", taille = "carré")
print(g)
off()
remove(t, g)

seuil = 20

PER_ff |> mutate(dsDom = discretisation(dsDom, methode = "quartiles")) |> group_by(dsDom, var) |> summarise(n = sum(CoeffRecEnq, na.rm=T)) |> group_by(dsDom) |> mutate(p = n / sum(n)) |> filter(var == "03")


PER_ff |> mutate(dsDom = discretisation(dsDom, methode = "quartiles")) |> group_by(dsDom, var) |> summarise(n = sum(CoeffRecEnq, na.rm=T)) |> group_by(dsDom) |> mutate(p = n / sum(n)) |> filter(var == "06")

expositionPenduleHpH = function(var)
{
  colnames(activites)[colnames(activites) == var] = "var"
  
  rapport("[!!!] Démarrage du calcul de l'indice d'exposition, heure par heure, par catégorie")
  rapport("      Catégorie choisie :", var)
  rapport("[1/8] Calcul de l'indice d'exposition de référence, par enquête")
  tabRef = group_by(activites, heure, ZT.ACT, var) %>%
    summarise(nRef = sum(CoeffEnq, na.rm=T)) %>%
    group_by(heure, ZT.ACT) %>%
    mutate(pRef = nRef / sum(nRef))
  
  rapport("[2/8] Calcul de l'indice d'exposition principal")
  actiTr = activites %>%
    filter(DuTvl > 0 & Activ %in% c("10","11")) %>%
    left_join(tabRef, by=c("heure" = "heure", "ZT.ACT" = "ZT.ACT", "var" = "var")) %>%
    filter(!is.na(ZT.ACT)) %>%
    group_by(heure, ZoneDens_duo, var, uid_PER) %>%
    summarise(mTx = mean(pRef), CoeffEnq = first(CoeffEnq), .groups = "drop_last") %>%
    group_by(heure, ZoneDens_duo, var) %>%
    summarise(mTx = median(mTx),
              q1Tx = quantile(mTx, probs=.25),
              q3Tx = quantile(mTx, probs=.75), n = n(), .groups="drop")
  
  rapport("[3/8] Calcul de l'indice d'exposition de référence hors domicile")
  tabRefHorsDom = group_by(activites, heure, ZT.ACT, var) %>%
    filter(substr(Tache,1,1) == "1") %>%
    summarise(nRef = sum(CoeffEnq, na.rm=T)) %>%
    group_by(heure, ZT.ACT) %>%
    mutate(pRef = nRef / sum(nRef))
  
  rapport("[4/8] Calcul de l'indice d'exposition principal sur lieu de travail")
  actiHorsDom = activites %>%
    filter(DuTvl > 0 & Activ %in% c("10","11")) %>%
    filter(substr(Tache,1,1) == "1") %>%
    left_join(tabRefHorsDom, by=c("heure" = "heure", "ZT.ACT" = "ZT.ACT", "var" = "var")) %>%
    filter(!is.na(ZT.ACT)) %>%
    group_by(heure, ZoneDens_duo, var, uid_PER) %>% summarise(mTx = mean(pRef), CoeffEnq = first(CoeffEnq)) %>%
    group_by(heure, ZoneDens_duo, var) %>% summarise(mTx_Ext = median(mTx), 
                                                     q1Tx_Ext = quantile(mTx, probs=.25),
                                                     q3Tx_Ext = quantile(mTx, probs=.75),
                                                     n_Ext = n())
  
  colnames(PER)[colnames(PER) == var] = "var"
  
  rapport("[5/8] Calcul de l'indice d'exposition de référence à partir de la base PER")
  tabRef_PER = PER %>%
    group_by(ZT, var) %>% summarise(nRef = sum(CoeffEnq, na.rm=T)) %>%
    group_by(ZT) %>% mutate(pRef = nRef / sum(nRef))
  
  rapport("[6/8] Calcul de l'indice d'exposition principal à partir de la base PER")
  ref_PER = left_join(PER, tabRef_PER, by=c("ZT" = "ZT", "var" = "var")) %>%
    filter(!is.na(CoeffEnq)) %>%
    group_by(var) %>% summarise(mRef_Dom = median(pRef),
                                q1Tx_Dom = quantile(pRef, probs=.25),
                                q3Tx_Dom = quantile(pRef, probs=.75))
  
  rapport("[7/8] Jointure des résultats")
  actiTr = left_join(actiTr, ref_PER, by="var") %>%
    mutate(surrep_Dom = (mTx/mRef_Dom - 1) * 100)
  actiTr = left_join(actiTr, actiHorsDom, by= c("heure", "ZoneDens_duo", "var")) %>%
    mutate(surrep_Ext = (mTx_Ext/mRef_Dom - 1) * 100)
  
  rapport(paste0("[8/8] Filtrage par seuil (fixé à ", seuil, ")"))
  actiTr = filter(actiTr, n_Ext > seuil)
  
  #actiTr = select(actiTr, var, mTx, mTx_Ext, mRef_Dom, mRef_24h, surrep_24h, surrep_Dom, surrep_Ext)
  
  return(actiTr)
}


tabPCS8hph = expositionPenduleHpH("PCS8")

sortie("Horaires/Exposition heure par heure, PCS8", taille = "carré")
tabPCS8hph %>%
  filter(var %in% c("02", "03", "04", "05", "06"), !is.na(ZoneDens_duo)) %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  découperDuo() %>%
  ggplot(aes(x = heure/60, y = mTx_Ext)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[2:6]) +
  facet_grid(duoTvl~duoRes) +
  scale_x_continuous(breaks = c(3:12)*2)
off()

valIntervalleSur100 = 2
ticks = c(-50, 0, 100, 200, 300, 400)

sortie("Horaires/Exposition heure par heure, PCS8, surreprésentation, travail", taille = "carré")
tabPCS8hph %>%
  filter(var %in% c("02", "03", "04", "05", "06"), !is.na(ZoneDens_duo)) %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  découperDuo() %>%
  ggplot(aes(x = heure/60, y = surrep_Ext)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[2:6], name = "catégorie\nsocioprofessionnelle\nde la personne") +
  facet_grid(duoTvl~duoRes) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_x_continuous(breaks = c(3:12)*2) +
  xlab("heure") + ylab("surreprésentation des enquêté·es de même catégorie sur lieu d'emploi dans le secteur (%)") +
  labs(title = ml("Surreprésentation des enquêté·es appartenant à la même catégorie",
                  "dans le secteur où se trouvent les travailleur·ses lorsqu'elles/ils",
                  "sont sur leur lieu d'emploi, par rapport à leur lieu de domicile"),
       subtitle = "Mesuré par secteur, en fonction du type de commune de résidence et de travail",
       caption = src_fig(PER)) +
  scale_y_continuous(trans = trans_sur100, breaks=ticks, limits = c(ticks[1], ticks[5])) +
  theme(legend.position = "bottom")
off()

# Indice de Liebeson ====

expositionLieberson = function(activites, var, seuil = seuilSignifiant)
{
  colnames(activites)[colnames(activites) == var] = "var"
  colnames(PER_ff)[colnames(PER_ff) == var] = "var"
  rapport(" !!!  Calcul Indice de Lieberson pour la variable", var, "avec un seuil de", seuil)
  if (length(unique(substr(activites$uid_PER, 1, 7))) == 1)
  {
    rapport("      Restreint à l'enquête", unique(substr(activites$uid_PER, 1,7)), info=T)
  }
  
    # Subset d'activites pour les travailleur·ses
  rapport("[1/3] Filtrage de la table des activités (travail uniquement)")
  activitesTrav = activites %>%
    filter(substr(uid_PER, 1, 7) != "EMP2019",
           uid_PER %in% PER_ff$uid_PER,
           !is.na(CoeffRecEnq),
           substr(Tache,1,1) == "1")
  
  # Calcul de l'indice d'exposition de Lieberson, heure par heure,
  # par rapport au groupe auquel appartiennent les enquêté·es elleux-mêmes
  rapport("[2/3] Calcul de l'indice de Lieberson pour les personnes sur leur lieu de travail")
  popFiltre = activitesTrav %>%
    group_by(heure, ZT.ACT) %>%
    summarise(n = n())
  A = activitesTrav %>%
    left_join(popFiltre, by=c("heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(heure, var) %>%
    summarise(A = sum(CoeffRecEnq))
  ai = activitesTrav %>%
    left_join(popFiltre, by=c("heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(heure, ZT.ACT, var) %>%
    summarise(ai = sum(CoeffRecEnq))
  ti = activitesTrav %>%
    left_join(popFiltre, by=c("heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(heure, ZT.ACT) %>%
    summarise(ti = sum(CoeffRecEnq))
  ai_sur_A = ai %>% 
    left_join(A, by = c("heure", "var")) %>%
    mutate(ai_sur_A = ai/A)
  bi_sur_ti = ai %>% rename(bi = ai) %>%
    left_join(ti, by=c("heure", "ZT.ACT")) %>%
    mutate(bi_sur_ti = bi / ti)
  indice = ai_sur_A %>%
    left_join(bi_sur_ti, by=c("heure", "ZT.ACT", "var")) %>%
    mutate(indice = ai_sur_A * bi_sur_ti) %>%
    group_by(heure, var) %>% summarise(indice = sum(indice))
  
  # On va rapporter l'indice d'exposition à la part de la population à l'heure dite
  # Si Lieberson > part, ça veut dire que surexposition ; si Lieb < part, sous-exposition
  # en moyenne
  rapport("[3/3] Calcul de la composition sociale heure par heure")
  ref = activitesTrav %>%
    group_by(heure, var) %>% summarise(n = n()) %>%
    group_by(heure) %>% mutate(compoHoraire = n / sum(n))
  
  indice = left_join(indice, ref, by=c("heure" = "heure", "var" = "var")) %>%
    mutate(rapport = (indice/compoHoraire - 1) * 100) 
  
  return(indice)
}

Lieb = expositionLieberson(activites, "PCS8", seuil = 0)

g0 = Lieb %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "catégorie\nsocio-professionnelle") +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'isolement de type Lieberson, heure par heure") +
  xlab("heure") + theme(legend.position = "none")

Lieb = expositionLieberson(activites, "PCS8")

g1 = Lieb %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "catégorie\nsocio-professionnelle") +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'isolement de type Lieberson, heure par heure") +
  xlab("heure") + theme(legend.position = "none")

g2 = Lieb %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  ggplot(aes(x = heure/60, y = rapport)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "catégorie\nsocio-professionnelle") +
  scale_x_continuous(breaks = c(3:12)*2) +
  scale_y_continuous(trans = trans_sur100, breaks=c(-50, 0, 100, 200), limits = c(-50, 250)) +
  labs(title = "Indice d'isolement relatif aux effectifs des groupes\nau travail à cette heure-là",
       caption = src_fig(emp = F)) +
  ylab("rapport (%)") +
  theme(legend.position = "bottom") +
  xlab("heure")

sortie("Horaires/Lieberson", taille = "carré")
cowplot::plot_grid(g1, g2, align = "v", nrow = 2, rel_heights=c(.43,.57), labels=c("a", "b")) %>% plot()
off()

Lieb75 = expositionLieberson(filter(activites, substr(uid_PER, 1, 7) == "IDF2010"), "PCS8")
Lieb69 = expositionLieberson(filter(activites, substr(uid_PER, 1, 7) == "LYO2015"), "PCS8")
Lieb13 = expositionLieberson(filter(activites, substr(uid_PER, 1, 7) == "MAR2009"), "PCS8")
Lieb44 = expositionLieberson(filter(activites, substr(uid_PER, 1, 7) == "LOI2015"), "PCS8")

Lieb75$Enq = "Île-de-France, 2010"
Lieb69$Enq = "Région lyonnaise, 2015"
Lieb13$Enq = "Marseille, 2009"
Lieb44$Enq = "Loire-Atlantique, 2015"

rbind(Lieb75, Lieb69) %>% rbind(Lieb13) %>% rbind(Lieb44) %>%
  filter(var %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[2:6], name = "catégorie\nsocio-professionnelle") +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'exposition de Lieberson, heure par heure") +
  xlab("heure") + theme(legend.position = "none") +
  facet_wrap(~Enq)

expositionLiebersonDuo = function(activites, var, seuil = 20)
{
  colnames(activites)[colnames(activites) == var] = "var"
  colnames(PER)[colnames(PER) == var] = "var"
  rapport(" !!!  Calcul Indice de Lieberson Duo pour la variable", var, "avec un seuil de", seuil)
  if (length(unique(substr(activites$uid_PER, 1, 7))) == 1)
  {
    rapport("      Restreint à l'enquête", unique(substr(activites$uid_PER, 1,7)), info=T)
  }
  
  # Subset d'activites pour les travailleur·ses
  rapport("[1/3] Filtrage de la table des activités (travail uniquement)")
  activitesTrav = activites %>%
    filter(substr(uid_PER, 1, 7) != "EMP2019",
           DuTvl > 0 & Activ %in% c("10","11"),
           substr(Tache,1,1) == "1")
  
  # Calcul de l'indice d'exposition de Lieberson, heure par heure,
  # par rapport au groupe auquel appartiennent les enquêté·es elleux-mêmes
  rapport("[2/3] Calcul de l'indice de Lieberson pour les personnes sur leur lieu de travail")
  
  # À reprendre ici : il faut évidemment comparer à la population selon son type de com
  # de travail ? En tous cas, on ne peut pas compartimenter le taux ainsi puisque
  # les habitant⋅es ne sont comparé⋅es qu'à celleux qui ont la même trajectoire qu'elleux-mêmes
  popFiltre = activitesTrav %>%
    group_by(ZoneDens_duo, heure, ZT.ACT) %>%
    summarise(n = n())
  A = activitesTrav %>%
    left_join(popFiltre, by=c("ZoneDens_duo", "heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(ZoneDens_duo, heure, var) %>%
    summarise(A = sum(CoeffEnq))
  ai = activitesTrav %>%
    left_join(popFiltre, by=c("ZoneDens_duo", "heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(ZoneDens_duo, heure, ZT.ACT, var) %>%
    summarise(ai = sum(CoeffEnq))
  ti = activitesTrav %>%
    left_join(popFiltre, by=c("ZoneDens_duo", "heure", "ZT.ACT")) %>%
    filter(n>seuil) %>%
    group_by(ZoneDens_duo, heure, ZT.ACT) %>%
    summarise(ti = sum(CoeffEnq))
  ai_sur_A = ai %>% 
    left_join(A, by = c("ZoneDens_duo", "heure", "var")) %>%
    mutate(ai_sur_A = ai/A)
  bi_sur_ti = ai %>% rename(bi = ai) %>%
    left_join(ti, by=c("ZoneDens_duo", "heure", "ZT.ACT")) %>%
    mutate(bi_sur_ti = bi / ti)
  indice = ai_sur_A %>%
    left_join(bi_sur_ti, by=c("ZoneDens_duo", "heure", "ZT.ACT", "var")) %>%
    mutate(indice = ai_sur_A * bi_sur_ti) %>%
    group_by(ZoneDens_duo, heure, var) %>% summarise(indice = sum(indice))
  
  # On va rapporter l'indice d'exposition à la part de la population à l'heure dite
  # Si Lieberson > part, ça veut dire que surexposition ; si Lieb < part, sous-exposition
  # en moyenne
  rapport("[3/3] Calcul de la composition sociale heure par heure")
  ref = activitesTrav %>%
    group_by(ZoneDens_duo, heure, var) %>% summarise(n = n()) %>%
    group_by(ZoneDens_duo, heure) %>% mutate(compoHoraire = n / sum(n))
  
  indice = left_join(indice, ref, by=c("ZoneDens_duo", "heure", "var")) %>%
    mutate(rapport = (indice/compoHoraire - 1) * 100) 
  
  return(indice)
}

LiebDuo = expositionLiebersonDuo(activites, "PCS8")

LiebDuo %>%
  filter(var %in% c("02", "03", "04", "05", "06"), !is.na(ZoneDens_duo)) %>%
  découperDuo() %>%
  mutate(PCS8 = etqPCS8(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCS8, group = PCS8)) +
  scale_color_manual(values = pal_PCS8[2:6], name = "catégorie\nsocio-professionnelle") +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'exposition de Lieberson, heure par heure") +
  xlab("heure") + theme(legend.position = "none") +
  facet_grid(duoTvl~duoRes)

Lieb = expositionLieberson(activites, "NivDip")

Lieb %>%
  mutate(NivDip = etqNivDip(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = NivDip, group = NivDip)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'exposition de Lieberson : niveau de diplôme, heure par heure") +
  xlab("heure")

Lieb = expositionLieberson(activites, "Age10")

Lieb %>%
  mutate(NivDip = etqNivDip(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = NivDip, group = NivDip)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'exposition de Lieberson : âge, heure par heure") +
  xlab("heure")

Lieb = expositionLieberson(activites, "Genre")

Lieb %>%
  mutate(NivDip = etqNivDip(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = NivDip, group = NivDip)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  labs(title = "Indice d'exposition de Lieberson : genre, heure par heure") +
  xlab("heure")

Lieb = expositionLieberson(activites, "PCS42S")

Lieb %>%
  filter(var %in% as.character(c(10:69))) %>%
  mutate(PCS42S = etqPCS42S(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCS42S, group = PCS42S)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  scale_color_manual(values = pal_PCS42S_c) +
  labs(title = "Indice d'exposition de Lieberson : PCS détaillée, heure par heure") +
  xlab("heure")

Lieb = expositionLieberson(activites, "ZoneDens")

Lieb %>%
  mutate(ZoneDens = etqZoneDens(var, num = T)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = ZoneDens, group = ZoneDens)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  scale_color_manual(values = pal_ZoneDens) +
  labs(title = "Indice d'exposition de Lieberson : densité zone de résidence, heure par heure") +
  xlab("heure")

Lieb = expositionLieberson(activites, "PCSMT")

Lieb %>%
  mutate(PCSMT = etqPCSM(var)) %>%
  ggplot(aes(x = heure/60, y = indice)) +
  geom_line(aes(color = PCSMT, group = PCSMT)) +
  scale_x_continuous(breaks = c(3:12)*2) +
  scale_color_manual(values = pal_PCSMT) +
  labs(title = "Indice d'exposition de Lieberson : densité zone de résidence, heure par heure") +
  xlab("heure")

# Calculs carroyés ====

if (resetCart)
{
  rapport("Préparation des calculs carroyés", prim = T)
  enqCarro = c("IDF2010", "LOI2015")
  load("Data/PGT.rds")
  
  sortie("Calculs carroyés", format = "pdf", taille = "a4")
  
  PER_POSI = PER %>%
    filter(uid_ENQ %in% enqCarro, PCS8 %in% c("02", "03", "04", "05", "06")) %>%
    calculer_lieuTravail(ACT) %>%
    calculer_pointZF(shp_ZF, PGT, champZF = "ZF_travMax", champUid = "uid_PER")
  
  grille = filter(shp_ZF, uid_ENQ %in% enqCarro) %>%
    st_make_grid(cellsize = c(2000,2000), square = F)
  baseGrille = matrix(ncol=1, nrow=length(grille), data=c(1:length(grille))) %>%
    as_tibble() %>% cbind(grille) %>% st_as_sf() 
  remove(grille)
  
  # NB : piwot_wider buggue avec de la géométrie, je suis obligé de bypasser
  PER_POSI_PCS = PER_POSI %>%
    mutate(point = geometry) %>% st_drop_geometry() %>% mutate(poids = CoeffEnq) %>%
    pivot_wider(names_from = PCS8, values_from = CoeffEnq, names_prefix = "pcs_") %>%
    rename(geometry = point) %>% st_as_sf(crs = 2154) %>%
    select(uid_ENQ, ZF_travMax, pcs_02, pcs_03, pcs_04, pcs_05, pcs_06, poids, geometry) %>%
    st_join(y = baseGrille) %>% st_drop_geometry() %>%
    group_by(V1) %>% summarize(across(starts_with("pcs"), sum, na.rm=T),
                               total = sum(poids, na.rm=T), n = n(),
                               uid_ENQ = first(uid_ENQ)) %>%
    filter(n > seuilSignifiant)
  
  # On calcule à présent l'indice de déviation
  PER_POSI_PCS = PER_POSI_PCS %>%
    rowwise() %>%
    mutate(pPcs_02 = pcs_02/total,
           pPcs_03 = pcs_03/total, 
           pPcs_04 = pcs_04/total, 
           pPcs_05 = pcs_05/total, 
           pPcs_06 = pcs_06/total) %>%
    ungroup() %>% group_by(uid_ENQ) %>%
    mutate(ePcs_02 = pPcs_02/(sum(pcs_02)/sum(total)),
           ePcs_03 = pPcs_03/(sum(pcs_03)/sum(total)),
           ePcs_04 = pPcs_04/(sum(pcs_04)/sum(total)),
           ePcs_05 = pPcs_05/(sum(pcs_05)/sum(total)),
           ePcs_06 = pPcs_06/(sum(pcs_06)/sum(total))) %>% ungroup() %>%
    mutate(indice = abs(ePcs_02-1) + abs(ePcs_03-1) + abs(ePcs_04-1) +
             abs(ePcs_05-1) + abs(ePcs_06-1),
           indice = indice/5,
           indice2 = (pcs_03 / (pcs_05 + pcs_06)))
  
  PER_POSI_PCS = left_join(PER_POSI_PCS, baseGrille, by=c("V1" = "V1")) %>% st_as_sf()
  
  
  viz_enTete("Indice carroyé de diversité sociale", num="4.3")
  
  map_CarteParEnquete(PER_POSI_PCS, fdCarte = fdCarte,
                      pdf = NULL,
                      colVal = "indice", colVal2 = "total", paysage=T,
                      legende="Indice de\ndiversité sociale", nClasses = 5, stockSup = T)%>% print()
  
  # map_CarteParEnquete(PER_POSI_PCS, fdCarte = fdCarte,
  #                     pdf = "Sorties/Atlas Diversité sociale carroyée 2.pdf",
  #                     colVal = "indice", paysage=T,
  #                     legende="Indice de\ndiversité sociale", nClasses = 5, mClasses="quantile")
  
  viz_enTete("Rapport cadres/classes populaires", num="4.4")
  
  map_CarteParEnquete(PER_POSI_PCS, fdCarte = fdCarte,
                      pdf =  NULL,
                      colVal = "indice2", paysage=T,
                      legende="Rapport cadres/classes pops", nClasses = 5, mClasses = "quantile") %>% print()
  
  # Idée de Julie : montrer que dans le privé, environnement moins ségrégué que dans le public à
  # cause des relations internes à l'entreprise ? Un peu trop difficile à faire.
  PER_PRIVPUB = PER %>%
    filter(!is.na(PCS42S)) %>%
    mutate(privpub = case_when(PCS42S %in% c("10", "21", "22", "31") ~ "Indépendant⋅e",
                               PCS42S %in% c("32", "41", "51") ~ "Public",
                               PCS42S %in% c("23", "36", "46", "47", "48", "52", "54",
                                             "55", "56", "61", "66", "69") ~ "Privé"),
           privpub = as.factor(privpub)) 
  
  # Mais je peux recalculer l'indice en ne prenant en compte que le privé :
  PER_POSI_PCS_PRIV = PER_POSI %>%
    filter(!is.na(PCS42S) & PCS42S %in% c("23", "36", "46", "47", "48", "52", "54",
                                          "55", "56", "61", "66", "69")) %>%
    mutate(point = geometry) %>% st_drop_geometry() %>% mutate(poids = CoeffEnq) %>%
    pivot_wider(names_from = PCS8, values_from = CoeffEnq, names_prefix = "pcs_") %>%
    rename(geometry = point) %>% st_as_sf(crs = 2154) %>%
    select(uid_ENQ, ZF_travMax, pcs_02, pcs_03, pcs_04, pcs_05, pcs_06, poids, geometry) %>%
    st_join(y = baseGrille) %>% st_drop_geometry() %>%
    group_by(V1) %>% summarize(across(starts_with("pcs"), sum, na.rm=T),
                               total = sum(poids, na.rm=T), n = n(),
                               uid_ENQ = first(uid_ENQ)) %>%
    filter(n > seuilSignifiant) %>%
    rowwise() %>%
    mutate(pPcs_02 = pcs_02/total,
           pPcs_03 = pcs_03/total, 
           pPcs_04 = pcs_04/total, 
           pPcs_05 = pcs_05/total, 
           pPcs_06 = pcs_06/total) %>%
    ungroup() %>% group_by(uid_ENQ) %>%
    mutate(ePcs_02 = pPcs_02/(sum(pcs_02)/sum(total)),
           ePcs_03 = pPcs_03/(sum(pcs_03)/sum(total)),
           ePcs_04 = pPcs_04/(sum(pcs_04)/sum(total)),
           ePcs_05 = pPcs_05/(sum(pcs_05)/sum(total)),
           ePcs_06 = pPcs_06/(sum(pcs_06)/sum(total))) %>% ungroup() %>%
    mutate(indice = abs(ePcs_02-1) + abs(ePcs_03-1) + abs(ePcs_04-1) +
             abs(ePcs_05-1) + abs(ePcs_06-1),
           indice = indice/5,
           indice2 = (pcs_03 / (pcs_05 + pcs_06))) %>%
    left_join(baseGrille, by=c("V1" = "V1")) %>% st_as_sf()
  
  viz_enTete("Indice carroyé de diversité sociale, secteur privé", num="4.5")
  
  map_CarteParEnquete(PER_POSI_PCS_PRIV, fdCarte = fdCarte,
                      pdf = NULL,
                      colVal = "indice", paysage=T,
                      legende="Indice de\ndiversité sociale", nClasses = 5, mClasses="quantile")%>% print()
  
  
}
off()