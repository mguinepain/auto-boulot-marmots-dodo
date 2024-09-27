# FACTOINDIV V6FF
# Un script développé de juillet à septembre 2023
# en vue de la publication d'un article
# Maxime Guinepain

source("START.R", print.eval = T)

# seuilSignifiant = 20
# pasDeTemps = 60
# vite = T
# garder = c("initMémoire", "seuilSignifiant", "pasDeTemps", "vite", "garder")  

if (!dir.exists("Sorties/Typologies")) { dir.create("Sorties/Typologies") }

rm(list = ls()[!ls() %in% garder], pos = globalenv())
initMémoire(BasesCharger = c("PER", "MEN"), f_base = T)

# Chargement des bases ====

rm(list = ls()[!ls() %in% garder], pos = globalenv())
initMémoire(BasesCharger = c("PER", "MEN", "shp_COM", "shp_ZT", "shp_ZF", "fdCarte"))

# On charge le champ "doublon" pour les retirer
PER = left_join(PER, select(MEN, uid_MEN, Doublon), by="uid_MEN")
remove(MEN)

# On calcule PER_ff (utilisée ici) et PER_f (pour comparer)
PER_f = PER %>%
  filter(DuTvl > 0 & PCS8 %in% c("02", "03", "04", "05", "06") & Activ %in% c("10", "11")) %>%
  filter(!uid_ENQ %in% c("CAY2011", "MTQ2014", "REU2016", "SQY2010", "EMP2019") & !Doublon)
PER_ff = PER %>%
  filter(DuTvl > 0 & Dis < 160000 & # DuLsr < 180 & DuCom < 60 &  DuSvc < 60 & DuTax < 60 &  
           JoTvDeb > 4 & JoTvFin < 28 &
           PCS8 %in% c("02", "03", "04", "05", "06") & Activ %in% c("10", "11")) %>%
  filter(!uid_ENQ %in% c("CAY2011", "MTQ2014", "REU2016", "SQY2010", "EMP2019") & !Doublon)

# Quelques champs supplémentaires pour l'analyse
PER_ff = mutate(PER_ff,
                disTvlSurDis = Dis/Travail_Dis,
                DuTvl = DuTvl + DuEtu,
                DuCtt = DuCom + DuSvc + DuTax,
                NivDip = NivEtuVersNivDip(NivEtu)) %>%
  filter(!is.na(disTvlSurDis) & !is.infinite(disTvlSurDis))

rapport("nombre d'enquêté⋅es correspondant au filtre des journées standards :", nrow(PER_ff), info=T)

# Coefficients ====

rapport("Recoefficientage")
# On veut repondérer l'échantillon pour que ça colle à peu près avec la composition
# de la population active

# Fichier Population du recensement de 2020, avec les données de 2014.
structurePop = read_delim("Sources/base-cc-evol-struct-pop-2020.CSV", delim = ";")

# Jointure pour get la grille communale de densité.
densite = read_delim("Sources/grille_densite_2020_agrege.csv", delim = ";") %>%
  select(COM, DENS)

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
  left_join(aavs, by="AAV2020") %>%
  select(-AAV2020)

arrondissements = tibble(CODGEO = c(as.character(c(13201:13216)),
                                    as.character(c(69381:69389)),
                                    as.character(c(75101:75120))),
                         TAAV2017 = c(rep(4, times = 25), rep(5, times = 20)))

aavs_com = rbind(aavs_com, arrondissements)

structurePop = left_join(structurePop, aavs_com, by="CODGEO")

remove(aavs_com, arrondissements)

# Ready pour faire deux tableaux :
tabPop = structurePop %>%
  group_by(DENS, TAAV2017) %>% summarise(across(where(is.numeric), \(x) sum(x, na.rm=T))) %>%
  select(DENS, TAAV2017, C14_POP1524_CS2:C14_POP1524_CS6, C14_POP2554_CS2:C14_POP2554_CS6,
         C14_POP55P_CS2:C14_POP55P_CS6) %>%
  pivot_longer(cols = starts_with("C14"), values_to = "n", names_to = "CATEG") %>%
  mutate(PCS_insee = substr(CATEG, nchar(CATEG), nchar(CATEG))) %>%
  mutate(age_insee = substr(CATEG, 8,9),
         age_insee = plyr::revalue(age_insee, c("15" = "1524",
                                                "25" = "2554",
                                                "55" = "55P"))) %>% select(-CATEG) %>%
  rename(ZoneDens = DENS, ZoneRang = TAAV2017) %>%
  mutate(ZoneDens = as.factor(as.character(ZoneDens)),
         ZoneRang = as.factor(as.character(ZoneRang)))

PER_ff = PER_ff %>%
  mutate(age_insee = case_when(Age > 14 & Age < 25 ~ "1524",
                               Age > 24 & Age < 55 ~ "2554",
                               Age > 54            ~ "55P"),
         PCS_insee = case_when(!PCS8 %in% c("00", "09") ~ substr(PCS8, 2, 2)))

tabPopPER = PER_ff %>%
  filter(Age > 14) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneRang), !is.na(PCS_insee), !is.na(Age)) %>%
  group_by(ZoneDens, ZoneRang, age_insee, PCS_insee) %>%
  summarise(nPER = n(), nPER_enq = sum(ifelse(is.na(CoeffEnq) | CoeffEnq == 0, 0, 1)))

# On joint et on calcule les ratios pour chaque intersection :
tabPopPER = tabPopPER %>%
  left_join(tabPop, by=c("ZoneDens", "ZoneRang", "age_insee", "PCS_insee")) %>%
  mutate(CoeffRecSpec = n / nPER_enq)

# On reporte les ratios comme des coefficients de pondération :
PER_ff = mutate(PER_ff, CoeffEnqOk = !is.na(CoeffEnq) & CoeffEnq != 0)
tabPopPER$CoeffEnqOk = T
PER_ff = left_join(PER_ff, select(tabPopPER, ZoneDens, ZoneRang, age_insee, PCS_insee, CoeffEnqOk, CoeffRecSpec),
                by = c("ZoneDens", "ZoneRang", "age_insee", "PCS_insee", "CoeffEnqOk"))
PER_ff = select(PER_ff, -CoeffEnqOk)

# Filtrage de PER_ff par rapport à PER_f ====
# Bilan : quelle part de gens matchent par rapport à l'ancienne version PER_f ?
PER$filtre_A = PER$uid_PER %in% PER_f$uid_PER
PER$filtre_B = PER$uid_PER %in% PER_ff$uid_PER
PER_f = PER_f %>%
  mutate(filtre_activites = DuLsr < 180 & DuCom < 60 & DuSvc < 60 & DuTax < 60,
         filtre_distance  = Dis < 160000,
         filtre_horaires  = JoTvDeb > 4 & JoTvFin < 28)

rapport("Part de journées > 1h de loisirs :",
        round(nrow(filter(PER_f, DuLsr>60))/nrow(PER_f)*100,2), "%", info=T)
rapport("Part de journées > 1h d'achats :",
        round(nrow(filter(PER_f, DuCom>60))/nrow(PER_f)*100,2), "%", info=T)
rapport("Part de journées > 1h d'activités contraintes :",
        round(nrow(filter(PER_f, DuSvc>60))/nrow(PER_f)*100,2), "%", info=T)
rapport("Part de journées > 1h d'accompagnement :",
        round(nrow(filter(PER_f, DuTax>60))/nrow(PER_f)*100,2), "%", info=T)
rapport("Part de journées > 2h de loisirs :",
        round(nrow(filter(PER_f, DuLsr>120))/nrow(PER_f)*100,2),"%", info=T)
rapport("Part de journées > 3h de loisirs :",
        round(nrow(filter(PER_f, DuLsr>180))/nrow(PER_f)*100,2),"%", info=T)

# Quantile(filter(PER_f, !is.na(CoeffRecEnq))$Dis, w=filter(PER_f, !is.na(CoeffRecEnq))$CoeffRecEnq, probs=.95, na.rm=T)

nrow(filter(PER_f, JoTvDeb == 4 | JoTvFin == 28))/nrow(PER_f)

summary(PER_f$DuCom)
summary(PER_f$DuLsr)

# Quelques graphiques pour illustrer
PER %>%
  mutate(filtre_AetB = case_when(filtre_A & filtre_B ~ T,
                                 filtre_A & !filtre_B ~ F)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, filtre_AetB) %>% summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = filtre_AetB)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("orange", "lightgreen"),
                    name = "journées", labels = c("non-conformes", "conformes", "non concerné"))

PER %>%
  mutate(filtre_AetB = case_when(filtre_A & filtre_B ~ T,
                                 filtre_A & !filtre_B ~ F)) %>%
  group_by(Age5, filtre_AetB) %>% summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = Age5, y = p)) + geom_col(aes(fill = filtre_AetB)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("orange", "lightgreen"),
                    name = "journées", labels = c("non-conformes", "conformes", "non concerné"))

PER %>%
  mutate(filtre_AetB = case_when(filtre_A & filtre_B ~ T,
                                 filtre_A & !filtre_B ~ F)) %>%
  group_by(ZoneRang, ZoneDens, filtre_AetB) %>% summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>% mutate(ZoneDens = etqZoneDens(ZoneDens), ZoneRang = etqZoneRang(ZoneRang)) %>%
  filter(!is.na(ZoneDens) & !is.na(ZoneRang)) %>%
  ggplot(aes(x = ZoneDens, y = p)) + geom_col(aes(fill = filtre_AetB)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("orange", "lightgreen"),
                    name = "journées", labels = c("non-conformes", "conformes", "non concerné")) +
  facet_wrap(~ZoneRang)

g1 = PER_f %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8)) %>%
  group_by(Genre, PCS8, filtre_activites) %>% summarise(n = sum(CoeffRec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = filtre_activites)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Genre) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "activités\nhors lieu travail\net domicile",
                    labels = c("plus d'une heure", "moins d'une heure", "non concerné"))
g2 = PER_f %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8)) %>%
  group_by(Genre, PCS8, filtre_horaires) %>% summarise(n = sum(CoeffRec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = filtre_horaires)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Genre) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "travail de nuit\nintersectant\nla plage",
                    labels = c("oui", "non", "non concerné"))
g3 = PER_f %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8)) %>%
  group_by(Genre, PCS8, filtre_distance) %>% summarise(n = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = filtre_distance)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Genre) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "distance totale\nexcédant 160 km",
                    labels = c("oui", "non", "non concerné"))

cowplot::plot_grid(g1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
                   g2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
                   g3, rel_heights = c(2.5,2.5,4), nrow = 3, align = "v") %>% plot()

g1 = PER_f %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneDens, ZoneRang, filtre_activites) %>% summarise(n = sum(CoeffRec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>% filter(!is.na(ZoneDens) & !is.na(ZoneRang)) %>%
  ggplot(aes(x = ZoneRang, y = p)) + geom_col(aes(fill = filtre_activites)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(~ZoneDens) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "activités", labels = c("plus d'une heure", "moins d'une heure", "non concerné"))
g2 = PER_f %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneDens, ZoneRang, filtre_horaires) %>% summarise(n = sum(CoeffRec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>% filter(!is.na(ZoneDens) & !is.na(ZoneRang)) %>%
  ggplot(aes(x = ZoneRang, y = p)) + geom_col(aes(fill = filtre_horaires)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(~ZoneDens) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "travail de nuit\nintersectant\nla plage",
                    labels = c("oui", "non", "non concerné"))
g3 = PER_f %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneDens, ZoneRang, filtre_distance) %>% summarise(n = sum(CoeffRec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>% filter(!is.na(ZoneDens) & !is.na(ZoneRang)) %>%
  ggplot(aes(x = ZoneRang, y = p)) + geom_col(aes(fill = filtre_distance)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(~ZoneDens) +
  scale_fill_manual(values = c("indianred1", "lightgreen"),
                    name = "distance totale\nexcédant 80 km",
                    labels = c("oui", "non", "non concerné"))
cowplot::plot_grid(g1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
                   g2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
                   g3, rel_heights = c(2.5,2.5,4), nrow = 3, align = "v") %>% plot()

ggplot(mutate(PER_f, PCS8=etqPCS8(PCS8)), aes(x = DuLsr/60)) +
  geom_density(aes(colour = PCS8), key_glyph = "path") +
  scale_colour_manual(values = pal_PCS8[2:6]) +
  coord_cartesian(xlim = c(0,3)) %>% print()
ggplot(mutate(filter(PER_f, DuLsr!=0), PCS8=etqPCS8(PCS8)), aes(x = DuLsr/60)) +
  geom_density(aes(colour = PCS8), key_glyph = "path") +
  scale_colour_manual(values = pal_PCS8[2:6]) +
  coord_cartesian(xlim = c(0,3)) %>% print()

ggplot(mutate(filter(PER_f, DuCom!=0), PCS8=etqPCS8(PCS8)), aes(x = DuCom/60)) +
  geom_density(aes(colour = PCS8), key_glyph = "path") +
  scale_colour_manual(values = pal_PCS8[2:6]) +
  coord_cartesian(xlim = c(0,3)) %>% print()

ggplot(mutate(filter(PER_f, DuSvc!=0), PCS8=etqPCS8(PCS8)), aes(x = DuSvc/60)) +
  geom_density(aes(colour = PCS8), key_glyph = "path") +
  scale_colour_manual(values = pal_PCS8[2:6]) +
  coord_cartesian(xlim = c(0,3)) %>% print()

summary(filter(PER_f, DuLsr!=0)$DuLsr)

# On n'a plus besoin de PER_f ensuite
remove(PER_f)
remove(PER)
remove(structurePop)

# Typologie des navettes simple ====

sortie("Typologies/Typologie des navettes")
PER %>%
  mutate(plusieursLxTvl = case_when(nbLxTvl == 1 ~ "un seul",
                                    nbLxTvl == 2 ~ "deux",
                                    nbLxTvl > 2  ~ "plus de deux"),
         plusieursLxTvl = factor(plusieursLxTvl, levels = c("un seul", "deux", "plus de deux"))) %>%
  filter(!is.na(plusieursLxTvl)) %>%
  ggplot(aes(x = duTvlPlage/60, y = plusieursLxTvl)) +
  geom_violin(scale="area") +
  geom_vline(xintercept = c(2,6), colour="lightslateblue") +
  xlab("durée moyenne d'une plage horaire de travail (h)") + ylab("nombre de lieux de travail") +
  labs(title="Une typologie des journées de travail", caption=src_fig(date = "février 2023")) +
  theme_bw()
off()

# Analyse factorielle par individu ====

factoIndiv = analyseFacto(base = PER_ff,
                          colVar = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                                     "JoTvDeb", "JoTvFin"),
                          colSup = c("PCS8", "PCSMT", "Age10", "NivDip", "Activ",
                                     "ZoneDens", "Genre", "MenEnfants", "MenCouple"),
                          colUids = "uid_PER", colPoids = "CoeffRecSpec",
                          scaleW = T, desacFiltreNa=F)

af = analyseFacto(base = PER_ff,
                  colVar = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr", "JoTvDeb", "JoTvFin"),
                  colSup = c("PCS8", "PCSMT", "Age10", "NivDip", "Activ", "ZoneDens", "Genre", "MenEnfants", "MenCouple"),
                  colUids = "uid_PER", colPoids = "CoeffRecSpec", scaleW = T, desacFiltreNa=F, sortieBrute = T)

PER_ff = filter(PER_ff, !is.na(N),
                !is.na(Tps), !is.na(Dis),
                !is.na(DuLsr), !is.na(DuTvl), !is.na(DuCtt),
                !is.na(JoTvDeb), !is.na(JoTvFin), !is.na(disTvlSurDis),
                !is.na(CoeffRecSpec))

PER_ff = cbind(PER_ff, af$li)

PER_ff = rename(PER_ff, Dim.1 = Axis1, Dim.2 = Axis2, Dim.3 = Axis3,
                        Dim.4 = Axis4, Dim.5 = Axis5)

PCSs = PER_ff %>%
  filter(!is.na(ZoneDens)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, num = T, supprTrFaible = T)) %>%
  mutate(ZoneDens = paste0("Com. de résidence\n", ZoneDens)) %>%
  group_by(ZoneDens, Genre, PCS8) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS8 = etqPCS8(PCS8, genre = Genre))

PCSsDet = PER_ff %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
  group_by(ZoneDens, Genre, PCS42S) %>% summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS42S = etqPCS42S(PCS42S, genre = Genre))

tabco = af$co
tabco$var = rownames(tabco)
tabco$var = plyr::revalue(tabco$var, c("N" = "nb. de\ndéplacements",
                                       "Tps" = "temps en\ndéplacement",
                                       "Dis" = "distance\ntotale",
                                       "DuLsr" = "temps\nd'activités\nde loisirs",
                                       "DuTvl" = "temps de\nprésence au\nlieu d'emploi",
                                       "DuCtt" = "temps\nd'activités\ncontraintes",
                                       "JoTvDeb" = "début\nprésence\nlieu d'emploi",
                                       "JoTvFin" = "fin présence\nlieu d'emploi",
                                       "disTvlSurDis" = "rapport\ndistance tot.\n/dist. vol\nd'oiseau\nlieu d'emploi"))

tabfl = tabco
tabfl$Comp1 = 0 ; tabfl$Comp2 = 0 ; tabfl$Comp3 = 0
tabfl = rbind(tabco, tabfl)

eigs = af$eig
eigs = eigs/sum(af$eig) * 100

g1 = ggplot(tabco, aes(x = Comp1, y = Comp2)) + theme_minimal() +
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") +
  geom_path(data = tabfl, aes(group = var), colour = "gray40", alpha=.3) +
  geom_point() +
  geom_text(aes(label = var), size=2, vjust=1.25) +
  coord_cartesian(xlim = c(min(tabco$Comp1) - abs(.2*min(tabco$Comp1)), max(tabco$Comp1) + .2*max(tabco$Comp1)),
                  ylim = c(min(tabco$Comp2) - abs(.2*min(tabco$Comp2)), max(tabco$Comp2) + .2*max(tabco$Comp2))) +
  xlab(paste0("Axe 1 (", round(eigs[1]), " % de la variance)")) +
  ylab(paste0("Axe 2 (", round(eigs[2]), " % de la variance)")) +
  labs(title = "Axes 1 et 2 de l'analyse") +
  theme(axis.text = element_blank())

g2 = ggplot(tabco, aes(x = Comp1, y = Comp3)) + theme_minimal() +
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") +
  geom_path(data = tabfl, aes(group = var), colour = "gray40", alpha=.3) +
  geom_point() +
  geom_text(aes(label = var), size=2, vjust=1.25) +
  coord_cartesian(xlim = c(min(tabco$Comp1) - abs(.2*min(tabco$Comp1)), max(tabco$Comp1) + .2*max(tabco$Comp1)),
                  ylim = c(min(tabco$Comp3) - abs(.2*min(tabco$Comp3)), max(tabco$Comp3) + .2*max(tabco$Comp3))) +
  xlab(paste0("Axe 1 (", round(eigs[1]), " % de la variance)")) +
  ylab(paste0("Axe 3 (", round(eigs[3]), " % de la variance)")) +
  labs(title = "Axes 1 et 3 de l'analyse", caption = src_fig()) +
  theme(axis.text = element_blank())

sortie("Typologies/Champs factoriels", format = "svg")
cowplot::plot_grid(g1, g2, align = "h") %>%
  viz_Titre("Plans factoriels de l'analyse portant sur les journées de travail") %>% print()
off()

# Typologie individus par k-means ====

factoIndiv_cat = categ_kMeans(factoIndiv, nomColonne = "factoIndiv_v6f", nCateg = 6) 

PER_ff = left_join(PER_ff, factoIndiv_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = PER_ff,
          cols = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                   "JoTvDeb", "JoTvFin", "PCS8", "PCSMT", "ZoneDens", "Genre"), colCateg = "factoIndiv_v6f",
          colPoids = "Coeff", colUid = "uid_PER",
          varActives = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                         "JoTvDeb", "JoTvFin"), seuilAbsurde = 300)

# V1 (juillet 2023)
categLabs1 = c("direct-court", "direct-long", "indirect-contraint", "court-tôt",
               "indirect-loisirs", "court-tard")
# V2 (août 2023)
categLabs2 = c("directs-locaux", "indirects", "directs-étendus", "locaux-tôt", "indirects-loisirs",
               "locaux-tards")
# V3 (septembre 2023)
categLabs3 = c("locales-typiques", "indirectes-contraintes", "indirectes-loisirs",
              "étendues", "locales-tôt", "locales-tard")

# V4 (novembre 2023)
categLabs = c("locales-typiques", "étendues", "locales-tôt", "indirectes-loisirs",
              "locales-tard", "indirectes-contraintes")

pal = c("slateblue", "olivedrab2", "orange", "skyblue", "tomato", "pink")

ggplot(PER_ff, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = factoIndiv_v6f)) +
  scale_color_hue(labels = categLabs)

profilageCategories(PER = PER_ff, champCateg = "factoIndiv_v6f") %>% print()

# Répartition des profils selon les navettes

PER_ff %>%
  group_by(ZoneRang, ZoneDens, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  group_by(factoIndiv_v6f) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = ZoneDens, y=ZoneRang)) +
  scale_colour_gradient(low = "mediumpurple", high = "red") +
  geom_count(aes(size = p, colour = p)) + facet_wrap(~factoIndiv_v6f)

# réalisation épique grâce à
# https://stackoverflow.com/questions/11353287/how-do-you-add-a-general-label-to-facets-in-ggplot2

g = PER_ff %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T),
         ZoneDens_travMax = etqZoneDens(ZoneDens_travMax, supprTrFaible = T)) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax), !is.na(factoIndiv_v6f)) %>%
  group_by(ZoneDens, ZoneDens_travMax, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  group_by(ZoneDens, ZoneDens_travMax) %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(hjust = ifelse(p < (max(p)-min(p))/2, -.1, 1.1)) %>%
  ggplot(aes(x = factoIndiv_v6f, y = p)) + geom_col(fill = "gray") +
  geom_text(aes(label = paste0(round(p), " %"), hjust=hjust), size = 2) +
  scale_x_discrete(limits = c("6","5","4","3","2","1"), labels=rev(categLabs)) +
  ylab("part des enquêté·es (%)") + xlab("type de journée") +
  facet_grid(ZoneDens_travMax~ZoneDens) + coord_flip() +
  labs(title = "Position moyenne des types de la typologie selon\nle lieu de résidence et d'emploi des enquêté·es",
       caption=src_fig())
g = ggplotGrob(g)
g = gtable::gtable_add_cols(g, unit(g$widths[[10]], "cm"), 10)
g = gtable::gtable_add_grob(g, list(grid::rectGrob(gp = grid::gpar(col = "black", fill = gray(.7))),
                                    grid::textGrob("Commune de travail", rot = -90,
                                                   gp = grid::gpar(col = gray(0), fontsize=8))),
                            8, 11, 12, name = paste(runif(2)))
g = gtable::gtable_add_rows(g, unit(g$heights[[7]], "cm"), 5)
g = gtable::gtable_add_grob(g, list(grid::rectGrob(gp = grid::gpar(col = "black", fill = "gray70")),
                                    grid::textGrob("Commune de résidence",
                                                   gp = grid::gpar(col = "black", fontsize=8))),
                            t=5, b=6, l=5, r=9, name = paste(runif(2)))
g = gtable::gtable_add_cols(g, unit(1/4, "line"), 10)
g = gtable::gtable_add_rows(g, unit(1/4, "line"), 6)

sortie("Typologies/Répartition selon ZoneDens")
grid::grid.draw(g)
off()

# PER_ff %>%
#   group_by(factoIndiv_v6f, pendule) %>%
#   summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
#   mutate(p = n / sum(n))

PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(CadSurPop = sum(ifelse(PCS8 == "03",CoeffRecSpec,0), na.rm=T) /
              sum(ifelse(PCS8 %in% c("05","06"),CoeffRecSpec,0), na.rm=T),
            PopSurCad = sum(ifelse(PCS8 %in% c("05","06"),CoeffRecSpec,0), na.rm=T) /
              sum(ifelse(PCS8 == "03",CoeffRecSpec,0), na.rm=T))

PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(popPop = sum(ifelse(PCS8 %in% c("05","06"), CoeffRecSpec, 0), na.rm=T),
            popTot = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = popPop / popTot)

g1= PER_ff %>%
  filter(!is.na(ZoneDens), !is.na(factoIndiv_v6f)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  group_by(ZoneDens, factoIndiv_v6f) %>% summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(`val%` = n / sum(n)) %>%
  ggplot(aes(x = factoIndiv_v6f, y = `val%`)) + geom_col() +
  geom_label(aes(label = paste(round(`val%`*100,1), "%"))) +
  labs(title = "Répartition des individus dans les catégories de l'analyse") +
  xlab("Catégorie") + ylab("Part de la population pondérée") +
  scale_x_discrete(labels = categLabs) +
  facet_wrap(~ZoneDens) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)

g2= PER_ff %>%
  filter(!is.na(ZoneDens), !is.na(factoIndiv_v6f)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  group_by(ZoneDens, factoIndiv_v6f) %>%
  summarize(dis.V = weighted.mean(Dis.V, w = CoeffRecSpec, na.rm=T)/1000) %>%
  ggplot(aes(x = factoIndiv_v6f, y = dis.V)) + geom_col() +
  geom_label(aes(label = paste(round(dis.V,1), "km"))) +
  labs(title = "Distance moyenne totale par individu, par catégorie") +
  xlab("Catégorie") + ylab("Distance") + facet_wrap(~ZoneDens) +
  scale_x_discrete(labels = categLabs) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g2)

g9= PER_ff %>%
  filter(!is.na(ZoneDens), !is.na(factoIndiv_v6f)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  ggplot(aes(x = Dis/1000)) + geom_density(aes(color = factoIndiv_v6f)) +
  labs(title = "Répartition des journées par distance parcourue au sein de chaque catégorie") +
  xlab("distance (km)") + ylab("densité") + scale_color_hue(name = "catégorie", labels=categLabs) +
  coord_cartesian(xlim=c(0,80)) +
  facet_wrap(~ZoneDens)
sortie("Typologies/Distance parcourue selon le profil")
print(g9)
off()

g4= PER_ff %>%
  filter(!is.na(ZoneDens), !is.na(factoIndiv_v6f)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  group_by(ZoneDens, factoIndiv_v6f) %>%
  summarize(N = weighted.mean(N, w = CoeffRecSpec, na.rm=T)) %>%
  ggplot(aes(x = factoIndiv_v6f, y = N)) + geom_col() +
  labs(title = "Nombre de déplacements") +
  xlab("Catégorie") + ylab("Nombre de déplacements") +
  facet_wrap(~ZoneDens) +
  scale_x_discrete(labels = categLabs) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g4)

g5= PER_ff %>%
  mutate(JoDeb = heureHHMMtoM(JoDeb),
         JoFin = heureHHMMtoM(JoFin)) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax), !is.na(factoIndiv_v6f)) %>%
  mutate(ZoneDens = paste0("Domicile\n",etqZoneDens(ZoneDens, supprTrFaible = T, num = T)),
         ZoneDens_travMax = paste0("Travail\n",etqZoneDens(ZoneDens_travMax, supprTrFaible = T, num = T))) %>%
  group_by(ZoneDens, ZoneDens_travMax, factoIndiv_v6f) %>%
  summarize(JoDeb = weighted.mean(JoDeb, w = CoeffRecSpec, na.rm=T),
            JoFin = weighted.mean(JoFin, w = CoeffRecSpec, na.rm=T)) %>%
  mutate(factoIndiv_v6f = factor(factoIndiv_v6f, levels=rev(sort(unique(factoIndiv_v6f))))) %>%
  pivot_longer(cols=c("JoDeb", "JoFin"), names_to="Heure", values_to="h") %>%
  ggplot(aes(x = factoIndiv_v6f, y = h/60)) + geom_point(aes(color = Heure)) +
  labs(title = "Horaires moyens de départ et retour au domicile") +
  xlab("Catégorie") + ylab("Heure") +
  geom_label(aes(label = heureMinToHr(h, secondes=F)), nudge_x = -.25) +
  scale_color_hue(labels = c("...de départ", "...de retour")) +
  coord_flip() +
  facet_grid(ZoneDens_travMax~ZoneDens)
sortie("Typologies/Horaires selon profils", taille="page")
print(g5)
off()

# g8= PER_ff %>%
#   mutate(JoDeb = heureHHMMtoM(JoDeb),
#          JoFin = heureHHMMtoM(JoFin)) %>%
#   filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax), !is.na(factoIndiv_v6f)) %>%
#   mutate(ZoneDens = paste0("Domicile\n",etqZoneDens(ZoneDens, supprTrFaible = T, num = T)),
#          ZoneDens_travMax = paste0("Travail\n",etqZoneDens(ZoneDens_travMax, supprTrFaible = T, num = T))) %>%
#   mutate(pop = CoeffRecEnq) %>%
#   filter(!is.na(typoModes) & !is.na(factoIndiv_v6f)) %>%
#   pivot_wider(names_from = "typoModes", names_prefix = "typomodes_", values_from = CoeffRecEnq,
#               names_repair = "unique") %>%
#   group_by(ZoneDens, ZoneDens_travMax, factoIndiv_v6f) %>%
#   summarize(across(starts_with("typomodes_"), sum, na.rm=T), pop = sum(pop, na.rm=T)) %>%
#   mutate(across(starts_with("typomodes_"), ~./pop*100),
#          partVoiture = typomodes_voiture + `typomodes_voiture+tc` + `typomodes_voiture+train`,
#          partTC = `typomodes_voiture+tc` + typomodes_tc + `typomodes_voiture+train` + typomodes_train) %>%
#   pivot_longer(cols = starts_with("typomodes_"), names_to = "typoModes", values_to = "part") %>%
#   mutate(typoModes = substr(typoModes,11,nchar(typoModes)),
#          typoModes = factor(typoModes, levels = levels(PER$typoModes)),
#          factoIndiv_v6f = factor(factoIndiv_v6f, levels = rev(sort(unique(factoIndiv_v6f))))) %>%
#   ggplot(aes(x = factoIndiv_v6f, y=part)) + geom_col(aes(fill = typoModes)) +
#   labs(title = "Typologie des modes de transport utilisés\nselon le type de journée") +
#   xlab("Catégorie journée") + ylab("Part de la population (%)") +
#   scale_fill_manual(values = c(pal22_typoModes), name = "modes utilisés") +
#   geom_text(y=90, hjust=1, aes(x = factoIndiv_v6f,
#                                label = paste0("dont motorisé : ", round(partVoiture,1), " %")), fontface = "italic") +
#   geom_text(y=5, hjust=0, aes(x = factoIndiv_v6f,
#                               label = paste0("dont TC : ", round(partTC,1), " %")), fontface = "italic") +
#   coord_flip() + facet_grid(ZoneDens_travMax~ZoneDens) + scale_x_discrete(labels = rev(categLabs))
# g8 %>% print()

# sortie("Typo modes selon ZoneDens", portrait=F)
# PER_ff %>%
#   mutate(JoDeb = heureHHMMtoM(JoDeb),
#          JoFin = heureHHMMtoM(JoFin)) %>%
#   filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax), !is.na(factoIndiv_v6f)) %>%
#   mutate(ZoneDens = paste0("Domicile\n",etqZoneDens(ZoneDens, supprTrFaible = T, num = T)),
#          ZoneDens_travMax = paste0("Travail\n",etqZoneDens(ZoneDens_travMax, supprTrFaible = T, num = T))) %>%
#   mutate(pop = CoeffRecEnq) %>%
#   filter(!is.na(typoModes) & !is.na(factoIndiv_v6f)) %>%
#   pivot_wider(names_from = "typoModes", names_prefix = "typomodes_", values_from = CoeffRecEnq,
#               names_repair = "unique") %>%
#   group_by(ZoneDens, ZoneDens_travMax, factoIndiv_v6f) %>%
#   summarize(across(starts_with("typomodes_"), sum, na.rm=T), pop = sum(pop, na.rm=T)) %>%
#   mutate(across(starts_with("typomodes_"), ~./pop*100),
#          partVoiture = typomodes_voiture + `typomodes_voiture+tc` + `typomodes_voiture+train`,
#          partTC = `typomodes_voiture+tc` + typomodes_tc + `typomodes_voiture+train` + typomodes_train) %>%
#   pivot_longer(cols = starts_with("typomodes_"), names_to = "typoModes", values_to = "part") %>%
#   mutate(typoModes = substr(typoModes,11,nchar(typoModes)),
#          typoModes = factor(typoModes, levels = levels(PER$typoModes)),
#          factoIndiv_v6f = factor(factoIndiv_v6f, levels = rev(sort(unique(factoIndiv_v6f))))) %>%
#   ggplot(aes(x = factoIndiv_v6f, y=part)) + geom_col(aes(fill = typoModes)) +
#   labs(title = "Typologie des modes de transport utilisés\nselon le type de journée") +
#   xlab("Catégorie journée") + ylab("Part de l'échantillon (%)") +
#   scale_fill_manual(values = c(pal22_typoModes), name = "modes utilisés") +
#   coord_flip() + facet_grid(ZoneDens_travMax~ZoneDens) + scale_x_discrete(labels = rev(categLabs)) %>% print()


# Chiffres (à citer dans le papier) =====

# Analyses pour faire des chiffres

sum(filter(PER_ff, Genre=="F")$CoeffRecSpec) / sum(PER_ff$CoeffRecSpec) 

# Part des personnes qui prennent la voiture en zone dense
sum(filter(PER_ff, ZoneDens == "1" & modes_voiture == "oui")$CoeffRecSpec) / sum(filter(PER_ff, ZoneDens == "1")$CoeffRecSpec)

# Part des personnes qui prennent la voiture en zone peu dense
sum(filter(PER_ff, ZoneDens %in% c("3","4") & modes_voiture == "oui")$CoeffRecSpec) / sum(filter(PER_ff, ZoneDens  %in% c("3","4"))$CoeffRecSpec)

# Part des personnes qui prennent la voiture en zone dense d'IDF
sum(filter(PER_ff, uid_ENQ == "IDF2010" & ZoneDens == "1" & modes_voiture == "oui")$CoeffRecSpec) / sum(filter(PER_ff, uid_ENQ == "IDF2010" & ZoneDens == "1")$CoeffRecSpec)

# Distance moyenne par classe de densité
PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffRecSpec, na.rm=T))

# Distance moyenne par catégorie
PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffRecSpec, na.rm=T))

# Durée moyenne en déplacement par catégorie
PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Tps, CoeffRecSpec, na.rm=T))

# Distance moyenne de déplacement dans la catégorie 1 (locale typique)
PER_ff %>%
  group_by(factoIndiv_v6f, ZoneDens) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffRecSpec, na.rm=T)) %>%
  filter(factoIndiv_v6f == "1")

# Distance moyenne dans la catégorie 4 (étendue)
PER_ff %>%
  group_by(factoIndiv_v6f, ZoneDens) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffRecSpec, na.rm=T)) %>%
  filter(factoIndiv_v6f == "4")

# Distance moyenne dans la catégorie 1, par ordre croissant
PER_ff %>%
  group_by(factoIndiv_v6f, ZoneDens, ZoneDens_travMax) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffRecSpec, na.rm=T)) %>%
  filter(factoIndiv_v6f == "1") %>% tab_Tri(parCol = "disMoy",rev=T)

# Distance médiane dans la catégorie 1, par densité
PER_ff %>%
  group_by(factoIndiv_v6f, ZoneDens) %>%
  summarise(disMed = weighted.median(Dis, CoeffRecSpec, na.rm=T)) %>%
  filter(factoIndiv_v6f == "1")

# Distance moyenne par densité, EMP
PER_ff %>%
  filter(uid_ENQ == "EMP2019") %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T))

# Distance moyenne selon la densité de la commune de travail
PER_ff %>%
  filter(!is.na(CoeffRecSpec)) %>%
  group_by(ZoneDens_travMax) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T))

PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(Tps, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(DuTvl, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneDens_travMax) %>%
  summarise(disMoy = weighted.mean(JoTvDeb, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(JoTvFin, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneRang) %>%
  summarise(disMoy = weighted.mean(JoTvDeb, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneRang) %>%
  summarise(disMoy = weighted.mean(JoTvFin, CoeffRecSpec, na.rm=T))

PER_ff %>%
  filter(ZoneRang != "5") %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(Tps, CoeffRecSpec, na.rm=T))

weighted.mean(filter(PER_ff, uid_ENQ == "IDF2010")$Tps, filter(PER_ff, uid_ENQ == "IDF2010")$CoeffEnq, na.rm=T)

PER_ff %>%
  filter(ZoneRang == "5") %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(Tps, CoeffRecSpec, na.rm=T))

weighted.mean(filter(PER_ff, ZoneRang != "5")$Tps, filter(PER_ff, ZoneRang != "5")$CoeffRecSpec, na.rm=T)

PER_ff %>%
  group_by(ZoneDens_travMax) %>%
  summarise(disMoy = weighted.mean(Tps, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(disMoy = weighted.mean(N, CoeffRecSpec, na.rm=T))

PER_ff %>%
  group_by(ZoneDens) %>%
  summarise(n = sum(ifelse(modes_voiture == "oui", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>%
  filter(ZoneRang != "5") %>%
  group_by(ZoneDens) %>%
  summarise(n = sum(ifelse(modes_voiture == "oui", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>%
  filter(ZoneRang == "5") %>%
  group_by(ZoneDens) %>%
  summarise(n = sum(ifelse(modes_voiture == "oui", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>%
  group_by(uid_ENQ, ZoneDens) %>%
  summarise(n = sum(ifelse(modes_voiture == "oui", CoeffEnq, 0), na.rm=T),
            pop = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/pop) %>% filter(ZoneDens == "1") %>%
  tab_Tri("uid_ENQ", parCol = "p")


PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 1)

PER_ff %>%
  group_by(ZoneDens, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 1)

PER_ff %>%
  group_by(ZoneRang, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 1)

PER_ff %>%
  group_by(ZoneDens, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 1)

PER_ff %>%
  group_by(uid_ENQ, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 1) %>% tab_Tri("uid_ENQ", parCol = "p", rev=T)

PER_ff %>%
  group_by(ZoneDens, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 4)

PER_ff %>%
  group_by(ZoneDens_travMax, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 4)

PER_ff %>%
  group_by(uid_ENQ, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 4) %>% tab_Tri("uid_ENQ", parCol = "p", rev=F)

PER_ff %>%
  group_by(ZoneDens_travMax, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 5)

PER_ff %>%
  group_by(ZoneDens_travMax, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 6)

PER_ff %>%
  group_by(uid_ENQ, factoIndiv_v6f) %>%
  summarise(n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(p = n/sum(n)) %>%
  filter(factoIndiv_v6f == 5) %>% tab_Tri("uid_ENQ", parCol = "p", rev=T)

sum(filter(PER_ff, DuCom > 0)$CoeffRecSpec, na.rm=T) / sum(PER_ff$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuCom > 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)
sum(filter(PER_ff, DuCom > 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuSvc > 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)
sum(filter(PER_ff, DuSvc > 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuTax > 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)
sum(filter(PER_ff, DuTax > 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuLsr > 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)
sum(filter(PER_ff, DuLsr > 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuCtt == 0 & DuLsr == 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

sum(filter(PER_ff, DuCtt == 0 & DuLsr == 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)



sum(filter(PER_ff, DuCtt > 0, Genre == "F")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "F")$CoeffRecSpec, na.rm=T)
sum(filter(PER_ff, DuCtt > 0, Genre == "H")$CoeffRecSpec, na.rm=T) /
  sum(filter(PER_ff, Genre == "H")$CoeffRecSpec, na.rm=T)

PER_ff %>%
  filter(!is.na(CoeffRecSpec)) %>%
  group_by(Genre) %>%
  summarise(du = weighted.mean(DuTvl, w=CoeffRecSpec, na.rm=T)) %>%
  mutate(du = du/60)

PER_ff %>%
  filter(!is.na(CoeffRecSpec)) %>%
  group_by(Genre) %>%
  summarise(dis = weighted.mean(Travail_Dis, w=CoeffRecSpec, na.rm=T)) %>%
  mutate(dis = dis/1000)

PER_ff %>%
  filter(!is.na(CoeffRecSpec)) %>%
  group_by(ZoneDens, Genre) %>%
  summarise(dis = weighted.mean(Travail_Dis, w=CoeffRecSpec, na.rm=T)) %>%
  mutate(dis = dis/1000)

PER_ff %>%
  summarise(n = sum(ifelse(Genre == "F", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>%
  group_by(factoIndiv_v6f) %>%
  summarise(n = sum(ifelse(Genre == "F", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>% filter(ZoneDens == "1") %>%
  group_by(factoIndiv_v6f) %>%
  summarise(n = sum(ifelse(Genre == "F", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)
PER_ff %>% filter(ZoneDens == "3" | ZoneDens == "4") %>%
  group_by(factoIndiv_v6f) %>%
  summarise(n = sum(ifelse(Genre == "F", CoeffRecSpec, 0), na.rm=T),
            pop = sum(CoeffRecSpec, na.rm=T)) %>%
  mutate(p = n/pop)

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  summarise(disMoy = weighted.mean(Dis/1000, w=CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "disMoy")


PER_ff %>% filter(!is.na(CoeffRecSpec)) %>% group_by(PCS8) %>%
  summarise(disMoy = weighted.mean(Dis/1000, w=CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "disMoy")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>% 
  summarise(disMoy = weighted.mean(Dis/1000, w=CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "disMoy")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>% group_by(PCS8) %>%
  summarise(tpsMoy = weighted.mean(Tps/60, w=CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "tpsMoy")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>% 
  summarise(tpsMoy = weighted.mean(Tps/60, w=CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "tpsMoy")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>%
  summarise(p100 = sum(ifelse(Dis>80000, CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>%
  summarise(joTvDeb = weighted.mean(JoTvDeb, CoeffRecSpec, na.rm=T),
            joTvFin = weighted.mean(JoTvFin, CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "joTvFin")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>% filter(ZoneRang != "5") %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>%
  summarise(joTvDeb = weighted.mean(JoTvDeb, CoeffRecSpec, na.rm=T),
            joTvFin = weighted.mean(JoTvFin, CoeffRecSpec, na.rm=T)) %>%
  tab_Tri(parCol = "joTvFin")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(factoIndiv_v6f) %>%
  summarise(p100 = sum(ifelse(PCS8 == "06", CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(factoIndiv_v6f) %>%
  summarise(p100 = sum(ifelse(PCS8 == "05", CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>%
  summarise(p100 = sum(ifelse(factoIndiv_v6f == "6", CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  summarise(p100 = sum(ifelse(Activ == "11", CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")

PER_ff %>% filter(!is.na(CoeffRecSpec)) %>%
  group_by(factoIndiv_v6f) %>%
  summarise(p100 = sum(ifelse(Activ == "11", CoeffRecSpec, 0), na.rm=T),
            pTot = sum(CoeffRecSpec)) %>%
  mutate(p100 = p100/pTot) %>%
  tab_Tri(parCol = "p100")


# Corrélation densité/paramètres ====


PER_ff_test = left_join(PER_ff, select(shp_ZF, CODE_ZF, densite), by=c("ZF" = "CODE_ZF"))

cor.test(log(filter(PER_ff_test, Dis != 0)$densite), log(filter(PER_ff_test, Dis != 0)$Dis))
cor.test(log(PER_ff_test$densite), log(PER_ff_test$Tps))
cor.test(log(PER_ff_test$densite), log(PER_ff_test$N))
cor.test(log(filter(PER_ff_test, disTvlSurDis != 0)$densite), log(filter(PER_ff_test, disTvlSurDis != 0)$disTvlSurDis))

cor.test(log(PER_ff_test$densite), PER_ff_test$DuTvl)
cor.test(log(PER_ff_test$densite), PER_ff_test$DuCtt)
cor.test(log(PER_ff_test$densite), PER_ff_test$DuLsr)

cor.test(log(PER_ff_test$densite), PER_ff_test$JoTvDeb)
cor.test(log(PER_ff_test$densite), PER_ff_test$JoTvFin)

# Projection variables illustratives ====

# PER_ff = left_join(PER_ff, factoIndiv, by = c("uid_PER" = "colUids"))

g1 = PER_ff %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) + geom_density2d(aes(color = Genre)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  ggtitle("Répartition des enquêté⋅es selon leur genre") + theme(legend.position="none")
g2 = PER_ff %>%
  ggplot(aes(x = Dim.1, y = Dim.3)) + geom_density2d(aes(color = Genre)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  labs(caption = src_fig(PER_ff)) + theme(legend.position = "bottom")
print(cowplot::plot_grid(g1, g2, nrow=1, align = "h", axis="tb"))

g1 = PER_ff %>% filter(PCS8 %in% c("03", "04", "05", "06")) %>% mutate(PCS8 = etqPCS8(PCS8)) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) + geom_density2d(aes(color = PCS8)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_colour_manual(values = pal_PCS8[3:6]) +
  ggtitle("Répartition des enquêté⋅es selon leur PCS") + theme(legend.position="none")
g2 = PER_ff %>% filter(PCS8 %in% c("03", "04", "05", "06")) %>% mutate(PCS8 = etqPCS8(PCS8)) %>%
  ggplot(aes(x = Dim.1, y = Dim.3)) + geom_density2d(aes(color = PCS8)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_colour_manual(values = pal_PCS8[3:6]) +
  labs(caption = src_fig(PER_ff)) + theme(legend.position = "bottom")
print(cowplot::plot_grid(g1, g2, nrow=1, align = "h", axis="tb"))

g1 = PER_ff %>% mutate(ZoneDens = etqZoneDens(ZoneDens, num=T)) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) + geom_density2d(aes(color = ZoneDens)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_colour_manual(values = pal_ZoneDens) +
  ggtitle("Répartition des enquêté⋅es selon la densité de leur com de résidence") + theme(legend.position="none")
g2 = PER_ff %>%  mutate(ZoneDens = etqZoneDens(ZoneDens, num=T)) %>%
  ggplot(aes(x = Dim.1, y = Dim.3)) + geom_density2d(aes(color = ZoneDens)) +
  scale_colour_manual(values = pal_ZoneDens) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  labs(caption = src_fig(PER_ff)) + theme(legend.position = "bottom")
print(cowplot::plot_grid(g1, g2, nrow=1, align = "h", axis="tb"))

PCSs = PER_ff %>% group_by(Genre, PCS8) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS8 = etqPCS8(PCS8, genre = Genre))

PCSsDet = PER_ff %>%
  group_by(Genre, PCS42S) %>% summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS42S = etqPCS42S(PCS42S, genre = Genre))

g = PER_ff %>%
  mutate(Dim.1 = round(Dim.1*10)/10, Dim.2 = round(Dim.2*10)/10) %>%
  group_by(Dim.1,
           Dim.2) %>% summarise(n = n(), dis = mean(Dis, na.rm=T)/1000, nDep = mean(N, na.rm=T)) %>%
  filter(dis < 120, nDep < 10) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(size = n, color = dis)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_label(data = PCSsDet, aes(label = etiqPCS42S, fill = Genre), alpha = .8, label.size=0,
             size = 2) +
  geom_label(data = PCSs, aes(label = etiqPCS8, fill = Genre), alpha = .8, label.size=0,
             fontface="italic") +
  scale_fill_hue(name = "genre", labels = c("femmes","hommes")) +
  scale_color_gradient(low = "green", high = "blue", name = "distance\nmoyenne\n(en km)") +
  scale_size(name = "nombre\nd'enquêté⋅es\npar\ncoord.") +
  coord_cartesian(xlim = c(-.8,.8), ylim = c(-.8,.8)) +
  labs(caption = src_fig(emp=F)) %>% print()
sortie("Typologies/Plan factoriel pour Lyon")
print(g)
off()

PER_ff %>%
  mutate(Dim.1 = round(Dim.1*10)/10, Dim.2 = round(Dim.2*10)/10) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
  group_by(ZoneDens, Dim.1,
           Dim.2) %>% summarise(n = n(), dis = mean(Dis, na.rm=T)/1000, nDep = mean(N, na.rm=T)) %>%
  filter(dis < 120, nDep < 10) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(size = n, color = dis)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_label(data = PCSsDet, aes(label = etiqPCS42S, fill = Genre), alpha = .8, label.size=0,
             size = 2) +
  geom_label(data = PCSs, aes(label = etiqPCS8, fill = Genre), alpha = .8, label.size=0,
             fontface="italic") +
  scale_fill_hue(name = "genre", labels = c("femmes","hommes")) +
  scale_color_gradient(low = "green", high = "blue", name = "distance\nmoyenne\n(en km)") +
  scale_size(name = "nombre\nd'enquêté⋅es\npar\ncoord.") +
  coord_cartesian(xlim = c(-.8,.8), ylim = c(-.8,.8)) +
  labs(caption = src_fig()) + facet_wrap(~ZoneDens) %>% print()

sortie("Typologies/Plans factoriels par genre et zone")
PER_ff %>%
  filter(!is.na(ZoneDens)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, num = T, supprTrFaible = T)) %>%
  group_by(ZoneDens, Genre, PCS8) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS8 = etqPCS8(PCS8, genre = Genre)) %>%
  mutate(Genre = plyr::revalue(Genre, c("F"= "femmes", "H" = "hommes"))) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_path(aes(colour = PCS8), ) +
  geom_point(aes(colour = PCS8, shape = ZoneDens), size = 2.5) +
  scale_colour_hue(name = "catégories\nsocioprofessionnelles",
                   labels = c("indépendant·es", "cadres et\nprof. intel.",
                              "prof. intermédiaires", "employé·es", "ouvrier·es")) +
  scale_shape(name = "type de commune\nde résidence\n(grille densité Insee)",
              labels=c("dense", "intermédiaire", "peu dense")) +
  coord_cartesian(xlim = c(-.7,.7), ylim = c(-.7,.7)) +
  labs(title = "Une stratification constante d'un contexte à l'autre",
       subtitle = ml("Répartition des PCS par genre et type de commune de résidence",
                     "dans le champ factoriel (variables illustratives)"), caption = src_fig()) +
  xlab(paste0("Axe 1 de l'analyse (", round(af$eig[1]/sum(af$eig)*100), " % de la variance)")) +
  ylab(paste0("Axe 2 de l'analyse (", round(af$eig[2]/sum(af$eig)*100), " % de la variance)")) +
  facet_wrap(~Genre) %>% print()
off()

# Tests du khi-2 sur les catégories ====

chisq.test(x = PER_ff$ZoneRang, y = PER_ff$factoIndiv_v6f)
cramer.v(table(PER_ff$ZoneRang, PER_ff$factoIndiv_v6f))

PER_ff_test = PER_ff %>% mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T))

chisq.test(x = PER_ff$ZoneDens, y = PER_ff$factoIndiv_v6f)
cramer.v(table(PER_ff$ZoneDens, PER_ff$factoIndiv_v6f))

chisq.test(x = PER_ff$Genre, y = PER_ff$factoIndiv_v6f)
cramer.v(table(PER_ff$Genre, PER_ff$factoIndiv_v6f))

PER_ff_test = PER_ff %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = as.character(PCS8))
chisq.test(x = PER_ff_test$PCS8, y = PER_ff_test$factoIndiv_v6f)
cramer.v(table(PER_ff_test$PCS8, PER_ff_test$factoIndiv_v6f))

# Carte des profils par département ====

baseDep = PER_ff %>%
  mutate(Dep = substr(Com, 1,2), pop = CoeffRecSpec) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecSpec, names_prefix = "fi6f") %>%
  group_by(Dep) %>% summarise(across(starts_with("fi6f"), sum, na.rm=T),
                              pop = sum(pop), n = n()) %>%
  mutate(across(starts_with("fi6f"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("fi6f"), names_to = "factoIndiv_v6f", values_to = "p") %>%
  group_by(factoIndiv_v6f) %>% mutate(surrep = ((p / weighted.mean(p, pop, na.rm=T)) - 1) * 100) %>%
  filter(n>100) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f, c("fi6f1"     = categLabs[1],
                                                          "fi6f2"                       = categLabs[2],
                                                          "fi6f3"                       = categLabs[3],
                                                          "fi6f4"                       = categLabs[4],
                                                          "fi6f5"                       = categLabs[5],
                                                          "fi6f6"                       = categLabs[6]),
                                        warn_missing = F))

shp_Dep = read_sf("Sources/Fond Carte/DEPARTEMENT.shp") %>%
  mutate(INSEE_DEP = ifelse(INSEE_DEP %in% c("2A", "2B"), "02", INSEE_DEP)) %>%
  group_by(INSEE_DEP) %>% summarise() %>%
  st_simplify(preserveTopology = T, dTolerance = 1000)

shp_Dep = tibble(INSEE_DEP = sort(rep(shp_Dep$INSEE_DEP, times = length(unique(baseDep$factoIndiv_v6f)))),
                 factoIndiv_v6f = rep(unique(baseDep$factoIndiv_v6f), times = nrow(shp_Dep))) %>%
  left_join(shp_Dep, by = "INSEE_DEP") %>% st_as_sf() %>%
  left_join(baseDep, by=c("INSEE_DEP" = "Dep", "factoIndiv_v6f" = "factoIndiv_v6f"))

etendue = summarise(group_by(shp_Dep, tout = T))

shp_Dep = mutate(shp_Dep, surrepD = discretisation(surrep, verb = T, couper1pc = T))

g = ggplot(data = filter(shp_Dep, factoIndiv_v6f != "fi6fNA" | is.na(factoIndiv_v6f))) +
  geom_sf(aes(fill = surrepD), color = "white") +
  #  geom_sf(data = st_point_on_surface(summarise(group_by(shp_Dep, INSEE_DEP), n = first(n))),
  #          aes(size = n), color = "gray20", alpha=.3) +
  labs(title = "Surreprésentation de chaque profil par département",
       caption = src_fig(PER_ff)) + facet_wrap(~factoIndiv_v6f) +
  # scale_fill_gradient2 (low = "slateblue", mid="whitesmoke", high = "indianred", na.value = "gray85",
  #                    name = "surreprésentation\ndu profil (%)",
  #                    trans = trans_sur100, labels = transf_echelle_sur100_lab) +
  scale_fill_brewer(type = "div", palette = "RdBu", direction = -1,
                    name = "surreprésentation\ndu profil (%)", na.value = "gray90") #+
# scale_size(name = "nombre d'enquêté⋅es")
g = cartoFinish(g, etendue)
sortie("Typologies/Surreprésentation profils par département", taille="page")
print(g)
off()

g = ggplot(data = filter(mutate(shp_Dep, surrepD = discretisation(surrep, couper1pc = T)),
                         factoIndiv_v6f == "étendus")) +
  geom_sf(aes(fill = surrepD), color = "white") +
  #  geom_sf(data = st_point_on_surface(summarise(group_by(shp_Dep, INSEE_DEP), n = first(n))),
  #          aes(size = n), color = "gray20", alpha=.3) +
  labs(title = "Surreprésentation de chaque profil par département",
       caption = src_fig(PER_ff)) +
  # scale_fill_gradient2 (low = "slateblue", mid="whitesmoke", high = "indianred", na.value = "gray85",
  #                    name = "surreprésentation\ndu profil (%)",
  #                    trans = trans_sur100, labels = transf_echelle_sur100_lab) +
  scale_fill_brewer(type = "div", palette = "RdBu", direction = -1,
                    name = "surreprésentation\ndu profil (%)", na.value = "gray90") #+
# scale_size(name = "nombre d'enquêté⋅es")
g = cartoFinish(g, etendue)
print(g)

# Discrétisation de la densité ====

# On va créer des catégories arbitraires
# 0-50, 50-100, 100-200, 200-400, 400-800, 800-1600, 1600-3200, 3200-6400, 6400-Inf
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

load("Data/shp_COM.rds")

PER_ff_zt_zf = PER_ff %>%
  left_join(select(shp_ZF, CODE_ZF, densite), by=c("ZF" = "CODE_ZF")) %>%
  filter(!is.na(densite))

PER_ff_zt_com = PER_ff %>%
  left_join(select(shp_COM, insee, densite), by=c("Com" = "insee")) %>%
  filter(!ZF %in% PER_ff_zt_zf$ZF)

PER_ff_zt = rbind(PER_ff_zt_zf, PER_ff_zt_com) %>% 
  mutate(etiqLog = classesDensites(densite)) %>%
  filter(Dis>0)

remove(PER_ff_zt_zf, PER_ff_zt_com)

PER_ff_zt_zf = PER_ff %>%
  left_join(select(shp_ZF, CODE_ZF, densite), by=c("ZF_travMax" = "CODE_ZF")) %>%
  filter(!is.na(densite))

PER_ff_zt_com = PER_ff %>%
  left_join(select(shp_COM, insee, densite), by=c("Com_travMax" = "insee")) %>%
  filter(!ZF %in% PER_ff_zt_zf$ZF)

PER_ff_zt_trav = rbind(PER_ff_zt_zf, PER_ff_zt_com) %>%
  mutate(etiqLog = classesDensites(densite)) %>%
  filter(Dis>0)

remove(PER_ff_zt_zf, PER_ff_zt_com)
remove(shp_COM)

etiquettes = levels(PER_ff_zt$etiqLog)

# etiquettes = sort(unique(PER_ff_zt$etiqLog))[c(1, (1:((length(unique(PER_ff_zt$etiqLog)))/2))*2)]
# etiquettes = etiquettes[!is.na(etiquettes)]

# Pour tester la discrétisation
shp_ZF %>%
  mutate(densite = classesDensites(densite)) %>%
  filter(uid_ENQ == "LOI2015") %>%
  ggplot() + geom_sf(aes(fill = densite)) +
  scale_fill_brewer(palette = "YlOrRd")

shp_ZF %>%
  mutate(densite = classesDensites(densite)) %>%
  filter(uid_ENQ == "LYO2015") %>%
  ggplot() + geom_sf(aes(fill = densite)) +
  scale_fill_brewer(palette = "YlOrRd")

PER_ff_zt %>%
  group_by(etiqLog) %>% summarise(pop = sum(CoeffRecSpec)) %>%
  ggplot(aes(x = etiqLog, y = pop)) + geom_col()


# Modèle simple distance/densité et résidus ====
 PER_ff_zt_mod = filter(PER_ff_zt, Dis>0)
 mod = lm(data = PER_ff_zt_mod, formula = "log(Dis) ~ log(densite)")
# 
# simulation = tibble(densite = unique(sort(PER_ff_zt_mod$etiqLog)),
#                     Dis = rep(0, times=length(unique(PER_ff_zt_mod$etiqLog))))
# 
# simulation$disMoy = exp(predict(object = mod, newdata = simulation))
# simulation$etiqLog = simulation$densite
# 
# weighted.mean(PER_ff$Dis, PER_ff$CoeffRecSpec)
# weighted.median(PER_ff_zt$densite, PER_ff_zt$CoeffRecSpec)

# L'ajout du modèle est très intéressant, mais il m'entraîne vers une pente dangereuse.

# Courbes de densité =====

pal1 = c("slateblue", "skyblue", "pink", "olivedrab2", "orange", "tomato")

# pal = c("slateblue", "olivedrab2", "orange", "skyblue", "tomato", "pink")


# Composition

gph_profils = function(base, titre="Densité de population et profils de journée")
{
  g = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecSpec, names_prefix = "fIv6f") %>%
    summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
    filter(nEntites>100) %>%
    pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
    group_by(etiqLog) %>% mutate(p = n / sum(n)) %>%
    filter(fI != "fIv6fNA") %>%
    ggplot(aes(x = etiqLog, y = p * 100)) +
    geom_line(aes(colour = fI, group=fI), alpha=.3) +
    geom_point(aes(colour = fI)) +
    xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
    labs(title = titre,
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                        values = pal) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  return(g)
}
gph_profils(PER_ff_zt)

gph_disMoy = function(base, titre = "Distance parcourue en fonction de la densité")
{
  cbGen = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(disMoy = weighted.mean(Dis, w=CoeffRecSpec), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(disMoy = weighted.mean(Dis, w = CoeffRecSpec), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = disMoy/1000)) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    xlab("densité de population (hab/km²)") + ylab("distance moyenne (km)") +
    labs(title = titre,
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "portée de\nla mesure",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16,17), name = "distance (en km)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  return(g)
}
gph_disMoy(PER_ff_zt)
gph_disMoy(PER_ff_zt_trav)

# Temps en déplacement
gph_tpsMoy = function(base)
{
  cbGen = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(tpsMoy = weighted.mean(Tps, w=CoeffRecSpec), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(tpsMoy = weighted.mean(Tps, w = CoeffRecSpec), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = tpsMoy)) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("temps en déplacement (min)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16, 17), name = "portée de la\nmesure") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  return(g)
}
gph_tpsMoy(PER_ff_zt)

# Nombre de dep's
gph_nDep = function(base)
{
  cbGen = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(nDep = weighted.mean(N, w=CoeffRecSpec), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(nDep = weighted.mean(N, w = CoeffRecSpec), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = nDep)) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16, 17), name = "déplacements") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  return(g)
}
gph_nDep(PER_ff_zt)


# Part des PCS
gGen = PER_ff_zt %>%
  filter(!is.na(etiqLog)) %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecSpec, names_prefix = "PCSn") %>%
  group_by(etiqLog) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(PCS != "PCSnNA")

g1 = PER_ff_zt %>% filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecSpec, names_prefix = "PCSn") %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(factoIndiv_v6f, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(PCS != "PCSnNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) +
  geom_line(aes(colour = PCS, group = PCS), alpha=.4) +
  geom_point(aes(colour = PCS)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque PCS (%)") +
  labs(title = "Part de chaque PCS dans les profils\nen fonction de la densité autour du lieu de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle", labels = niv_PCS8[2:6],
                      values = pal_PCS8[2:6]) +
  facet_wrap(~factoIndiv_v6f) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2 = PER_ff_zt %>%  filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecSpec, names_prefix = "PCSn") %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(factoIndiv_v6f, etiqLog) %>% mutate(p = n / sum(n)) %>%
  left_join(gGen, by=c("etiqLog", "PCS"), suffix=c("", ".gen")) %>%
  mutate(surrep = p/p.gen) %>%
  filter(PCS != "PCSnNA") %>%
  ggplot(aes(x = etiqLog, y = (surrep-1) * 100)) +
  geom_line(aes(colour = PCS, group = PCS), alpha=.4) +
  geom_point(aes(colour = PCS)) +
  xlab("densité de population (hab/km²)") + ylab("surreprésentation de la PCS\npar rapport à la moyenne nationale") +
  labs(title = "Surreprésentation de chaque PCS dans les profils\nen fonction de la densité autour du domicile",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle", labels = niv_PCS8[2:6],
                      values = pal_PCS8[2:6]) +
  facet_wrap(~factoIndiv_v6f) +
  geom_hline(yintercept = 0, colour = "black", alpha=.5) +
  scale_y_continuous(trans = trans_sur100, breaks=transf_echelle_sur100_inverse(c(-3:3)),
                     labels=transf_echelle_sur100_lab(transf_echelle_sur100_inverse(c(-3:3)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cadre = cowplot::plot_grid(g1 + theme(legend.position = "none") + labs(caption = NULL) +
                             theme(axis.text.x = element_blank()) + xlab(NULL),
                           g2 + theme(legend.position = "none") + labs(caption = NULL),
                           cowplot::get_legend(g1 + theme(legend.position = "bottom") +
                                                 guides(colour = guide_legend(nrow = 2))),
                           nrow=3, rel_heights = c(4,5,1), align="v", axis="lr")
cadre = viz_Pied(cadre, src_fig(PER_ff_zt))

sortie("Typologies/Composition PCS par profil", taille = "carré")
print(cadre)
off()

# Distance moyenne par PCS
cbGen = PER_ff_zt %>% filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecSpec), nEntites = n()) %>%
  filter(nEntites>50)
gDisMoyPcs = PER_ff_zt %>%  filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecSpec), nEntités = n()) %>%
  left_join(rename(cbGen, disMoyMoy = disMoy), by=c("etiqLog", "factoIndiv_v6f")) %>%
  mutate(surrepDis = ((disMoy / disMoyMoy)-1)*100) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = surrepDis)) +
  geom_hline(yintercept=0, colour="black", alpha=.5) +
  geom_line(aes(colour = PCS8, group = PCS8), alpha=.3) +
  geom_point(aes(colour = PCS8), shape=17) +
  xlab("densité de population (hab/km²)") + ylab("écart à la moyenne (%)") +
  labs(title = "Écart à la moyenne des distances \nparcourues selon la PCS et la densité\nmesurée autour du domicile",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6]),
                      values = c(pal_PCS8[2:6])) +
  scale_y_continuous(trans = trans_sur100) +
  facet_wrap(~factoIndiv_v6f, ncol=1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(gDisMoyPcs)

# Durée moyenne par PCS
cbGen = PER_ff_zt %>% filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(duMoy = weighted.mean(DuTvl, w=CoeffRecSpec), nEntites = n()) %>%
  filter(nEntites>50)
gDutMoyPcs = PER_ff_zt %>%  filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(duMoy = weighted.mean(DuTvl, w = CoeffRecSpec), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duMoy/60)) +
  geom_point(aes(colour = PCS8, shape="par profil")) +
  geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
  xlab("densité de population (hab/km²)") + ylab("temps passé au lieu d'emploi (h)") +
  labs(title = "Temps passé sur le lieu d'emploi\nselon la PCS et la densité mesurée\nautour du domicile",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6], "tous profils"),
                      values = c(pal_PCS8[2:6], "black")) +
  scale_shape_manual(values = c(16, 17), name = "distance (en km)") +
  facet_wrap(~factoIndiv_v6f, nrow=6) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
print(gDutMoyPcs)

# Heure début et fin (selon lieu de travail)

cbGen = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecSpec),
            hDeb = weighted.mean(JoTvDeb, w=CoeffRecSpec), nEntites = n()) %>%
  pivot_longer(cols = c(hDeb, hFin), names_to = "hLaquelle", values_to = "h") %>%
  mutate(hLaquelle = plyr::revalue(hLaquelle, c("hDeb" = "heure d'arrivée sur\nle lieu de travail",
                                                "hFin" = "heure de départ du\nlieu de travail"))) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecSpec),
            hDeb = weighted.mean(JoTvDeb, w = CoeffRecSpec), nEntités = n()) %>%
  pivot_longer(cols = c(hDeb, hFin), names_to = "hLaquelle", values_to = "h") %>%
  mutate(hLaquelle = plyr::revalue(hLaquelle, c("hDeb" = "heure d'arrivée sur\nle lieu de travail",
                                                "hFin" = "heure de départ du\nlieu de travail"))) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = h)) +
  geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
  geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
  xlab("densité de population (hab/km²)") + ylab("heure (h)") +
  labs(title = "Horaires de travail en fonction\nde la densité autour du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt_trav, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_shape_manual(values = c(16,17), name = "portée de la\nmesure") +
  scale_y_reverse(breaks=c(3:10)*2) +
  facet_grid(rows = "hLaquelle", scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
sortie("Typologies/Horaires selon profils et densité")
print(g)
off()

gph_modeVoit = function(base)
{
  cbGen = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = pVoit*100)) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation de la voiture (%)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16, 17), name = "déplacements") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(g)
}

gph_modeTC = function(base)
{
  cbGen = PER_ff_zt %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = PER_ff_zt %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = pTC*100)) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation des TC (%)") +
    labs(title = "Utilisation des TC en fonction de la densité au secteur d'emploi",
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16, 17), name = "déplacements") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(g)
}

gph_modeDoux = function(base)
{
  cbGen = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog) %>%
    summarise(pTC = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pTC = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffRecSpec, 0), na.rm=T)/sum(CoeffRecSpec, na.rm=T), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = pTC*100)) +
    geom_point(aes(colour = factoIndiv_v6f, shape="par profil")) +
    geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation marche/vélo (%)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_shape_manual(values = c(16, 17), name = "déplacements") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(g)
}



# Heure début et fin, courbes par PCS
cbGen = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], 
                                          "2" = gsub("-", "\n-", categLabs[2]),
                                          "3" = categLabs[3],
                                          "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecSpec),
            hDeb = weighted.mean(JoTvDeb, w=CoeffRecSpec), nEntites = n()) %>%
  filter(nEntites>200) %>%
  pivot_longer(cols = c(hFin, hDeb), values_to = "h", names_to = "hLaquelle") %>%
  mutate(hLaquelle = plyr::revalue(hLaquelle, c("hDeb" = "heure d'arrivée sur\nle lieu de travail",
                                                "hFin" = "heure de départ du \nlieu de travail")))
g = PER_ff_zt_trav %>% filter(!is.na(etiqLog)) %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], 
                                          "2" = gsub("-", "\n-", categLabs[2]),
                                          "3" = categLabs[3],
                                          "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecSpec),
            hDeb = weighted.mean(JoTvDeb, w = CoeffRecSpec), nEntités = n()) %>%
  filter(nEntités>50) %>%
  pivot_longer(cols = c(hFin, hDeb), values_to = "h", names_to = "hLaquelle") %>%
  mutate(hLaquelle = plyr::revalue(hLaquelle, c("hDeb" = "heure d'arrivée sur\nle lieu de travail",
                                                "hFin" = "heure de départ du \nlieu de travail"))) %>%
  ggplot(aes(x = etiqLog, y = h)) +
  geom_point(aes(colour = PCS8, shape="par profil"), size=1) +
  geom_point(data = cbGen, aes(colour = "tous profils", shape = "en moyenne"), size=1) +
  xlab("densité de population (hab/km²)") + ylab("heure (h)") +
  labs(title = "Heures de travail en fonction de la PCS et du type de journée\net de la densité mesurée autour du lieu de travail",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(niv_PCS8[2:6], "tous profils"),
                      values = c(pal_PCS8[2:6], "black")) +
  scale_shape_manual(values = c(16,17), name = "portée de la courbe") +
  scale_linetype(name = "heure", labels=c("d'arrivée sur\nle lieu de travail",
                                          "de départ du\nlieu de travail")) +
  scale_y_reverse(breaks=c(6:21)) +
  facet_grid(hLaquelle~factoIndiv_v6f, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position="bottom")

sortie("Typologies/Heures de travail par PCS")
print(g)
off()

# Carte ====

gen = PER_ff_zt %>%
  filter(uid_ENQ == "LOI2015") %>%
  mutate(pop = CoeffRecSpec) %>%
  pivot_wider(names_from = "factoIndiv_v6f", values_from = "CoeffRecSpec", names_prefix = "MoyfIv6_") %>%
  summarise(across(starts_with("MoyfIv6_"), sum, na.rm=T),
                             popTot = sum(pop)) %>%
  mutate(across(starts_with("MoyfIv6_"), ~./popTot*100)) %>%
  select(-popTot) %>%
  mutate(jointure = T)

PER_ff_zt %>%
  filter(uid_ENQ == "LOI2015") %>%
  mutate(pop = CoeffRecSpec) %>%
  pivot_wider(names_from = "factoIndiv_v6f", values_from = "CoeffRecSpec", names_prefix = "fIv6_") %>%
  group_by(ZT) %>% summarise(across(starts_with("fIv6_"), sum, na.rm=T),
                             popTot = sum(pop)) %>%
  mutate(across(starts_with("fIv6_"), ~./popTot*100)) %>%
  mutate(jointure = T) %>%
  left_join(gen, by = "jointure") %>%
  mutate(fIv6_1 = (1-fIv6_1/MoyfIv6_1)*100, fIv6_2 = (1-fIv6_2/MoyfIv6_2)*100,
         fIv6_3 = (1-fIv6_3/MoyfIv6_3)*100, fIv6_4 = (1-fIv6_4/MoyfIv6_4)*100,
         fIv6_5 = (1-fIv6_5/MoyfIv6_5)*100, fIv6_5 = (1-fIv6_5/MoyfIv6_5)*100) %>%
  pivot_longer(cols = starts_with("fIv6_"), names_to = "factoIndiv_v6f", values_to = "p") %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f, c("fIv6_1" = categLabs[1],
                                                          "fIv6_2" = categLabs[2],
                                                          "fIv6_3" = categLabs[3],
                                                          "fIv6_4" = categLabs[4],
                                                          "fIv6_5" = categLabs[5],
                                                          "fIv6_6" = categLabs[6]))) %>%
  ggplot() + geom_sf(aes(fill = p)) + facet_wrap(~factoIndiv_v6f) +
  scale_fill_gradient2(trans = trans_sur100, limits = c(-70,300),
                       low = "blue", mid = "green", high = "red", midpoint = 0)

shp_ZT %>% left_join(PER_ff_zt_carte)


# Représentation du plan factoriel avec densité ====

sortie("Typologies/Plans factoriels par genre et zone")
PER_ff_zt_trav %>%
  mutate(etiqLog = factor(etiqLog, levels=etiquettes)) %>%
  filter(PCS8 != "02", !is.na(densite)) %>%
  group_by(etiqLog, Genre, PCS8) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T), n = n()) %>%
  filter(n>100) %>%
  mutate(etiqPCS8 = etqPCS8(PCS8, genre = Genre)) %>%
  mutate(Genre = plyr::revalue(Genre, c("F"= "femmes", "H" = "hommes"))) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_path(aes(group=PCS8, linetype = "variation au\nsein de la même\nCSP"), colour = "grey70", alpha=.6) +
  geom_point(aes(colour = etiqLog, shape = PCS8), size = 2) +
  scale_shape_manual(values = c(15:17,3),  name = "catégories\nsocioprofessionnelles",
                   labels = c("cadres et\nprof. intel.",
                              "prof. intermédiaires", "employé·es", "ouvrier·es")) +
  scale_colour_brewer(name = "densité com.\ndu lieu de travail",
                      palette = "Spectral") +
  scale_linetype(name = NULL) +
  coord_cartesian(xlim = c(-.3,.3), ylim = c(-.7,.7)) +
  labs(title = "Une stratification constante d'un contexte à l'autre",
       subtitle = ml("Position des PCS par genre et type de commune de résidence",
                     "dans le champ factoriel (variables illustratives)"), caption = src_fig()) +
  xlab(paste0("Axe 1 de l'analyse (", round(af$eig[1]/sum(af$eig)*100), " % de la variance)")) +
  ylab(paste0("Axe 2 de l'analyse (", round(af$eig[2]/sum(af$eig)*100), " % de la variance)")) +
  facet_wrap(~Genre) %>% print()
off()

# Figures =====

# Figure complexe : répartition des classes et distances selon dom et trav
g1 = gph_profils(PER_ff_zt, titre = "Répartition des profils selon la densité\nau lieu de domicile")
g2 = gph_profils(PER_ff_zt_trav, titre = "Répartition des profils selon la densité\nau lieu de travail")
g3 = gph_disMoy(PER_ff_zt, titre = "Distance moyenne parcourue selon\nla densité au lieu de domicile")
g4 = gph_disMoy(PER_ff_zt_trav, titre="Distance moyenne parcourue selon\nla densité au lieu de travail")


cadre = cowplot::plot_grid(g1 + theme(legend.position = "none") + labs(caption=NULL),
                           g2 + theme(legend.position = "none") + labs(caption=NULL),
                           g3 + theme(legend.position = "none") + labs(caption=NULL),
                           g4 + theme(legend.position = "none") + labs(caption=NULL),
                           nrow=2, align="h", labels = c("a", "b", "c", "d"))
cadre = cowplot::plot_grid(cadre, cowplot::get_legend(g4), rel_widths=c(8,2))
cadre = viz_Pied(cadre, src_fig(PER_ff_zt))

sortie("Typologies/Ensemble figures 1")
print(cadre)
off()

remove(g1,g2,g3,g4)

# Pour parler de l'encombrement en ville
g1 = gph_tpsMoy(PER_ff_zt) + labs(title = "Temps passé en déplacement selon\nla densité au lieu de domicile")
g2 = gph_tpsMoy(PER_ff_zt_trav) + labs(title = "Temps passé en déplacement selon\nla densité au lieu de travail")
g3 = gph_nDep(PER_ff_zt_trav) + labs(title = "Nombre de déplacements selon\nla densité au lieu de travail")
g4 = gph_modeVoit(PER_ff_zt_trav) + labs(title = "Recours à l'automobile selon\nla densité au lieu de travail")
g5 = gph_modeTC(PER_ff_zt_trav) + labs(title = "Recours aux T.C. selon\nla densité au lieu de travail")
g6 = gph_modeDoux(PER_ff_zt_trav) + labs(title = "Recours à la marche et au vélo selon\nla densité au lieu de travail")

cadre = cowplot::plot_grid(g1 + theme(legend.position = "none") + labs(caption=NULL),
                           g2 + theme(legend.position = "none") + labs(caption=NULL),
                           g3 + theme(legend.position = "none") + labs(caption=NULL),
                           g4 + theme(legend.position = "none") + labs(caption=NULL),
                           g5 + theme(legend.position = "none") + labs(caption=NULL),
                           g6 + theme(legend.position = "none") + labs(caption=NULL),
                           nrow=3, align="h", labels = c("a", "b", "c", "d", "e", "f"))
cadre = cowplot::plot_grid(cadre, cowplot::get_legend(g2), rel_widths=c(8,2))
cadre = viz_Pied(cadre, src_fig(PER_ff_zt))

sortie("Typologies/Ensemble figures 2", taille="carré")
print(cadre)
off()

remove(g1, g2, g3, g4, g5, g6)

cadre = cowplot::plot_grid(gDisMoyPcs + theme(legend.position = "none") + labs(caption=NULL),
                           gDutMoyPcs + theme(legend.position = "none") + labs(caption=NULL),
                           cowplot::get_legend(gDutMoyPcs), nrow=1, rel_widths=c(4,4,2))
cadre = viz_Pied(cadre, src_fig(PER_ff_zt))

sortie("Typologies/Ensemble figures 3", taille="page", portrait=T)
print(cadre)
off()

# Blablabla ====

# Concentration des lieux d'emploi et des lieux de domicile

parZF = PER %>% filter(!is.na(ZF_travMax)) %>% group_by(ZF) %>% summarise(n = sum(CoeffRecSpec, na.rm = T))
parZFTV = PER %>% group_by(ZF_travMax) %>% summarise(n = sum(CoeffRecSpec, na.rm=T))

parZF = tab_Tri(parZF, parCol = "n", rev=T) 
parZFTV = tab_Tri(parZFTV, parCol = "n", rev=T)  %>%
  filter(!is.na(ZF_travMax))

# Quantile(parZF$n, weights=parZF$n, probs=.5)
# Quantile(parZFTV$n, weights=parZFTV$n, probs=.5)

# Logiquement les lieux d'emploi devraient être plus concentrés que les lieux de résidence, mais
# ce n'est pas ce que je trouve. Bizarre....
# Mieux vaut laisser tomber ça pour l'instant...

parZF$num = c(1:nrow(parZF))
parZFTV$num = c(1:nrow(parZFTV))
ggplot(parZF, aes(x = num, y = n)) + geom_line() +
  geom_line(data=parZFTV, colour = "red") +
  coord_cartesian(xlim = c(0,10000), ylim = c(0,20000))

nrow(filter(parZFTV, n>10000)) / nrow(parZF)
nrow(filter(parZF, n>10000)) / nrow(parZF)

parZF = PER %>% filter(!is.na(ZF_travMax)) %>% group_by(ZF) %>% summarise(n = n())
parZFTV = PER %>% group_by(ZF_travMax) %>% summarise(n = n())

parZF = tab_Tri(parZF, parCol = "n", rev=T) 
parZFTV = tab_Tri(parZFTV, parCol = "n", rev=T)  %>%
  filter(!is.na(ZF_travMax))

parZF$num = c(1:nrow(parZF))
parZFTV$num = c(1:nrow(parZFTV))
ggplot(parZF, aes(x = num, y = n)) + geom_line() +
  geom_line(data=parZFTV, colour = "red")


nrow(filter(parZFTV, n>50)) / nrow(parZF)
nrow(filter(parZF, n>50)) / nrow(parZF)



# Mesure de la congestion : vitesse moyenne selon heure d'arrivée ? ====
remove(PER) ; remove(MEN) ; remove(af)

load("Data/DEP.rds")
DEP_ff = filter(DEP, uid_PER %in% PER_ff$uid_PER)
remove(DEP)

DEP_ff$veloc = (DEP_ff$Dis / 1000) / (DEP_ff$Duree / 60)
DEP_ff = filter(DEP_ff, veloc > 0 & !is.infinite(veloc))
DEP_ff$D_Hr = heureHHMMtoM(DEP_ff$D_Hr) / 60

DEP_ff = left_join(DEP_ff, select(PER_ff, uid_PER, ZF, ZF_travMax, CoeffRecSpec, factoIndiv_v6f), by="uid_PER") %>%
         left_join(select(PER_ff_zt, uid_PER, densite, etiqLog), by=c("uid_PER" = "uid_PER"))

# DEP_ff = DEP_ff %>%
#   mutate(paliersLog = paliers(log(densite), log(4))) %>%
#   mutate(etiqLog = round(exp(paliersLog))) %>%
#   filter(Dis > 0)

# etiquettes = sort(unique(DEP_ff$etiqLog))
# etiquettes = etiquettes[!is.na(etiquettes)]

DEP_ff$HrMed = (heureHHMMtoM(DEP_ff$O_Hr)/60) + ((DEP_ff$D_Hr - (heureHHMMtoM(DEP_ff$O_Hr)/60)) / 2)

DEP_ff %>%
  mutate(HrMed = paliers(HrMed, 1)) %>%
  group_by(HrMed) %>% summarise(velocMoy = weighted.mean(veloc, CoeffRecSpec), n=n()) %>%
  filter(n>200) %>%
  ggplot(aes(x = HrMed, y = velocMoy)) + geom_line()

DEP_ff %>%
  left_join(select(PER_ff, uid_PER, modes_voiture)) %>% filter(modes_voiture == "oui") %>%
  mutate(HrMed = paliers(HrMed, 1)) %>%
  group_by(HrMed) %>% summarise(velocMoy = weighted.mean(veloc, CoeffRecSpec), n=n()) %>%
  filter(n>200) %>%
  ggplot(aes(x = HrMed, y = velocMoy)) + geom_line()

DEP_ff %>%
  left_join(select(PER_ff, uid_PER, modes_voiture)) %>% filter(modes_voiture == "oui") %>%
  mutate(HrMed = paliers(HrMed, 1)) %>%
  group_by(etiqLog, HrMed) %>% summarise(velocMoy = weighted.mean(veloc, CoeffRecSpec), n=n()) %>%
  filter(n>200) %>%
  ggplot(aes(x = HrMed, y = velocMoy)) + geom_line(aes(colour = etiqLog, group=etiqLog)) +
  scale_colour_discrete(type = "viridis", breaks = etiquettes)


# etiquettes = sort(unique(DEP_ff$etiqLog))
# etiquettes = etiquettes[!is.na(etiquettes)]

# etiquettesBis = c("0 à 0,5 hab/km²",
#                   "0,5 à 2 hab/km²",
#                   "2 à 8 hab/km²",
#                   "8 à 32 hab/km²",
#                   "32 à 128 hab/km²",
#                   "128 à 512 hab/km²",
#                   "512 à 2048 hab/km²",
#                   "2048 à 8192 hab/km²",
#                   "8192 à 32768 hab/km²",
#                   "32768 à 131072 hab/km²")

g1 = DEP_ff %>%
  mutate(etiqLog = as.factor(etiqLog)) %>%
  filter(Duree>15) %>%
  filter(HrMed>6.5 & HrMed<20.5) %>%
  left_join(select(PER_ff, uid_PER, modes_voiture)) %>% filter(modes_voiture == "oui") %>%
  mutate(HrMed = paliers(HrMed, 1)) %>%
  group_by(etiqLog, HrMed) %>% summarise(velocMoy = weighted.mean(veloc, CoeffRecSpec), n=n()) %>%
  filter(etiqLog %in% etiquettes, n>150) %>%
  ggplot(aes(x = HrMed, y = velocMoy)) + geom_line(aes(colour = etiqLog, group=etiqLog)) +
  scale_colour_hue(name = "densité\n(hab./km²)\nau niveau du\nlieu d'emploi",
                      breaks=etiquettes, labels=etiquettes) +
  scale_x_continuous(breaks = c(3:11)*2, limits=c(4,27)) +
  xlab("heure médiane du déplacement") + ylab("vitesse moyenne du déplacement\n(km/h)")

g2 = DEP_ff %>%
  mutate(etiqLog = as.factor(etiqLog)) %>%
  filter(Duree>15) %>%
  mutate(HrMed = paliers(HrMed, 1)) %>%
  group_by(etiqLog, HrMed) %>% summarise(nombre = sum(CoeffRecSpec, na.rm=T)) %>%
  group_by(etiqLog) %>% mutate(prop = nombre/sum(nombre) * 100) %>%
  filter(etiqLog %in% etiquettes) %>%
  ggplot(aes(x = HrMed, y = prop)) + geom_line(aes(colour = etiqLog, group=etiqLog)) +
  scale_colour_hue(name = "densité\n(hab./km²)\nau niveau du\nlieu d'emploi",
                      breaks=etiquettes, labels=etiquettes) +
  scale_x_continuous(breaks = c(3:11)*2, limits=c(4,27)) +
  xlab("heure médiane du déplacement") + ylab("part des déplacements\nayant lieu à cette heure (%)")

cadre = plot_grid(g1 + theme(legend.position = "none"),
                  g2 + theme(legend.position = "none"), nrow = 2,
                  rel_heights = c(2,1),
                  align="v", axis="lr")
cadre = plot_grid(cadre, cowplot::get_legend(g2), nrow=1, rel_widths=c(8,2))
cadre = viz_Pied(cadre, src_fig(DEP_ff))

sortie("Typologies/Vitesse et heures de pointe")
print(cadre)
off()

remove(DEP_ff)

shp_Dep = read_sf("Sources/Fond Carte/DEPARTEMENT.shp") %>%
  mutate(INSEE_DEP = ifelse(INSEE_DEP %in% c("2A", "2B"), "02", INSEE_DEP)) %>%
  group_by(INSEE_DEP) %>% summarise() %>%
  st_simplify(preserveTopology = T, dTolerance = 1000)

depsData = PER_ff %>% group_by(DEP = substr(Com, 1,2)) %>%
  summarise(hDeb = weighted.mean(JoTvDeb, CoeffRecSpec, na.rm=T),
            hFin = weighted.mean(JoTvFin, CoeffRecSpec, na.rm=T), n = n()) %>%
  filter(n>25) %>%
  mutate(hDeb = discretisation(hDeb, nbClassesCible = 8),
         hFin = discretisation(hFin, nbClassesCible = 8)) 

shp_Dep = left_join(shp_Dep, depsData, by=c("INSEE_DEP" = "DEP"))

# shp_Dep = pivot_longer(shp_Dep, cols = c(hDeb, hFin), values_to = "h", names_to = "hWhat")

ggplot(shp_Dep) + geom_sf(aes(fill = hDeb)) + scale_fill_brewer(na.value = "grey")
ggplot(shp_Dep) + geom_sf(aes(fill = hFin)) + scale_fill_brewer(na.value = "grey")




# Densité autour du lieu de résidence

PER_ff_zt %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f, c("1" = categLabs[1],
                                                          "2" = categLabs[2],
                                                          "3" = categLabs[3],
                                                          "4" = categLabs[4],
                                                          "5" = categLabs[5],
                                                          "6" = categLabs[6]))) %>%
  group_by(factoIndiv_v6f) %>%
  summarise(dens = weighted.median(densite, w=CoeffRecSpec, na.rm=T))

PER_ff_zt_trav %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f, c("1" = categLabs[1],
                                                          "2" = categLabs[2],
                                                          "3" = categLabs[3],
                                                          "4" = categLabs[4],
                                                          "5" = categLabs[5],
                                                          "6" = categLabs[6]))) %>%
  group_by(factoIndiv_v6f) %>%
  summarise(dens = weighted.median(densite, w=CoeffRecSpec, na.rm=T))

# (Archives) Typologie de modes déclarés, abonnements, véhicules ====

colsFqcs = c("Fqc_Drm", "Fqc_Mch", "Fqc_Tco", "Fqc_Vco", "Fqc_Vel", "Fqc_Vpa")
PER$cat_Modes = NULL ; PER$cat_Modes_PT = NULL

cats = analyseFacto(base = PER, colVar = colsFqcs,
                    colSup = c("Activ", "Age10", "PCSMT", "PCS8", "ZoneDens"),
                    colPoids = "Coeff", colUids = "uid_PER") %>%
  categ_kMeans(nomColonne = "cat_Modes")
PER = left_join(PER, cats, by=c("uid_PER" = "uid"))
plotCateg(tab = PER, cols = colsFqcs, colCateg = "cat_Modes",
          colPoids = "Coeff", colUid = "uid_PER")

cats = analyseFacto(base = PER_trav, colVar = colsFqcs,
                    colSup = c("Activ", "Age10", "PCSMT", "PCS8", "ZoneDens"),
                    colPoids = "Coeff", colUids = "uid_PER") %>%
  categ_kMeans(nomColonne = "cat_Modes_PT")
PER = left_join(PER, cats, by=c("uid_PER" = "uid"))
plotCateg(tab = PER, cols = colsFqcs, colCateg = "cat_Modes_PT",
          colPoids = "Coeff", colUid = "uid_PER")


# (Archives) Typo FactoIndiv v3 ====

colActives = c("Dis.V", "Tps", "nAct", "Dis.M", "mDis_Tvl",
               "pDis_VOI", "pDis_TCO",
               "pDis_CTR", "pDis_LSR",
               "JoDeb", "JoFin", "DuDom", "DuTvl",
               "rapDomCtdDis")

factoIndiv = analyseFacto(base = PER_trav,
                          colVar = colActives,
                          colSup = c("PCS42S", "PCSMT", "Age10", "Activ", "ZoneDens", "Genre"),
                          colPoids = "CoeffEnq", colUids = "uid_PER", scaleW = T, desacFiltreNa=T)

factoIndiv_cat = categ_kMeans(factoIndiv, nomColonne = "factoIndiv_v3")                          


PER_trav = left_join(PER_trav, factoIndiv_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = PER_trav,
          cols = c(colActives, "PCS42S", "PCSMT", "ZoneDens", "Genre"), colCateg = "factoIndiv_v3",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives)

# on calcule la part d'individu de chaque categ par ZT
PER_trav_ZT = tab_partCategsZT(PER_trav, names_from = "factoIndiv_v3", values_from = "CoeffEnq",
                               prefixe=("fIndivV3_"), garderTotal = T)

# on fait une ACP de la répartition
PER_trav_ZT_CAH = analyseFacto(base = PER_trav_ZT, colVar = colnames(PER_trav_ZT)[2:ncol(PER_trav_ZT)-1],
                               colUids = "ZT", titre = "Analyse factorielle par résidence des types d'individus",
                               colPoids = "total") %>%
  categ_cah(colUids = "ZT", nCateg = 5, nomColonne = "cahTrav",
            titre = "CAH des ZT selon la composition de la population en emploi résidente") %>%
  rename(ZT = colUids)

plotCateg(left_join(PER_trav_ZT, select(PER_trav_ZT_CAH, ZT, cahTrav), by=c("ZT" = "ZT")),
          cols=colnames(PER_trav_ZT[2:ncol(PER_trav_ZT)]), colCateg = "cahTrav",
          colUid = "ZT",
          varActives = colnames(PER_trav_ZT[2:ncol(PER_trav_ZT)-1]))

# on calcule la part d'individu de chaque categ par ZT − sur le lieu de travail
load("Data/ACT.rds")
PER_trav_ZTlT = PER_trav %>% calculer_lieuTravail(ACT) %>%
  tab_partCategsZT(names_from = "factoIndiv_v3", values_from = "CoeffEnq",
                   prefixe=("fIndivV3lT_"), garderTotal = T, champZT = "ZT_travMax") %>%
  filter(total>500) # beaucoup d'effets liés aux faibles effectifs dans zones périphériques...

PER_trav_ZTlT_CAH = analyseFacto(base = PER_trav_ZTlT,
                                 colVar = colnames(PER_trav_ZTlT)[2:ncol(PER_trav_ZTlT)-1],
                                 colUids = "ZT_travMax",
                                 titre = "Analyse factorielle par lieu d'emploi des types d'individus",
                                 colPoids = "total") %>%
  categ_cah(colUids = "ZT_travMax", nCateg = 6, nomColonne = "cahTrav",
            titre = "CAH des ZT selon la composition de la population en emploi travaillant sur secteur") %>%
  rename(ZT_travMax = colUids)

plotCateg(left_join(PER_trav_ZTlT, select(PER_trav_ZTlT_CAH, ZT_travMax, cahTrav), by=c("ZT_travMax" = "ZT_travMax")),
          cols=colnames(PER_trav_ZTlT[2:ncol(PER_trav_ZTlT)]), colCateg = "cahTrav",
          colUid = "ZT_travMax",
          varActives = colnames(PER_trav_ZTlT[2:ncol(PER_trav_ZTlT)-1]))

# Cartographie des ZT par secteurs de travailleurs résident⋅es
PER_trav_ZT_CAH = left_join(shp_ZT, PER_trav_ZT_CAH, by=c("ZT" = "ZT")) %>%
  mutate(uid_ENQ = substr(ZT, 1, 7))

viz_enTete("Classif. des secteurs de tirage par profils de travailleur⋅ses résident⋅es", num="4.1")

map_CarteParEnquete(shp = PER_trav_ZT_CAH, colVal = "cahTrav",
                    pdf = NULL,
                    format = "a4", paysage = T,
                    titre = "Classif. des secteurs de tirage par profils de travailleurs résidents",
                    legende = "Catégorie de\nsecteur de tirage", fdCarte = fdCarte)


viz_enTete("Classif. des secteurs de tirage par profils de travailleur⋅ses par lieu d'emploi", num="4.2")

PER_trav_ZTlT_CAH = left_join(shp_ZT, PER_trav_ZTlT_CAH, by=c("ZT" = "ZT_travMax")) %>%
  mutate(uid_ENQ = substr(ZT, 1, 7))

map_CarteParEnquete(shp = PER_trav_ZTlT_CAH, colVal = "cahTrav",
                    pdf = NULL,
                    format = "a4", paysage = T,
                    titre = "Classif. des secteurs de tirage par profils de travailleurs par lieu d'emploi",
                    legende = "Catégorie de\nsecteur de tirage", fdCarte = fdCarte)

# (Archives) Typo FactoIndiv v4 ====
# On pourrait se servir de l'âge du véhicule, mais quid des gens qui n'en ont pas...
# Serait pertinent de faire une analyse avec que les voitures.

colActives = c("Dis.V", "Tps", "nAct", "Dis.M", "mDis_Tvl", "V", # V tient lieu d'indicateur modal
               "DuDom", "DuTvl", "DuExt",
               "JoDeb", "JoFin")

viz_enTete("Modélisation FactoIndiv_v4", num="3.3")

for (i in 1:length(colActives))
{
  g = ggplot(data = PER_trav, aes(x = PER_trav[[colActives[i]]])) + geom_density() + theme_bw() +
    labs(title=paste("Répartition de la variable", colActives[i]))
  print(g)
}

factoIndiv = analyseFacto(base = PER_trav,
                          colVar = colActives,
                          colSup = c("PCS42S", "PCSMT", "Age10", "Activ", "ZoneDens", "pendule", "Genre"),
                          colPoids = "CoeffEnq", colUids = "uid_PER", scaleW = T, desacFiltreNa=T)


factoIndiv_cat = categ_kMeans(factoIndiv, nomColonne = "factoIndiv_v4", nCateg = 8)                          

ggplot(data = left_join(factoIndiv, factoIndiv_cat, by=c("colUids" = "uid")),
       aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = factoIndiv_v4)) +
  scale_x_continuous(limits = c(-5,15)) + scale_y_continuous(limits = c(-10,10)) +
  scale_color_hue(name = "Catégorie") +
  labs(title = "Répartition des individus (journées)\nsur les plans 1 et 2 de l'analyse factorielle",
       caption = "Données EMD Cerema 2008-2018. Réalisation : Maxime Guinepain") +
  theme_bw() 

PER_trav = left_join(PER_trav, factoIndiv_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = PER_trav,
          cols = c(colActives, "PCS42S", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v4",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives, seuilAbsurde = 300, viewTab=T)

wtd.table(PER_trav$factoIndiv_v4, weights=PER_trav$CoeffEnq) %>% freq()

PER_trav %>% group_by(factoIndiv_v4) %>%
  summarize(dis.V = weighted.mean(Dis.V, w = CoeffEnq, na.rm=T)) %>%
  viz_Texte()
PER_trav %>% group_by(factoIndiv_v4) %>%
  summarize(Tps = weighted.mean(Tps, w = CoeffEnq, na.rm=T)) %>%
  viz_Texte()
PER_trav %>% group_by(factoIndiv_v4) %>%
  summarize(nAct = weighted.mean(nAct, w = CoeffEnq, na.rm=T)) %>%
  viz_Texte()

PER_trav %>% group_by(factoIndiv_v4) %>%
  summarize(JoDeb = weighted.mean(JoDeb, w = CoeffEnq, na.rm=T),
            JoFin = weighted.mean(JoFin, w = CoeffEnq, na.rm=T)) %>%
  mutate(JoDeb = JoDeb/60, JoFin = JoFin/60) %>%
  viz_Texte()


# (Archives) Typo FactoIndiv v5 ====

viz_enTete("Modélisation FactoIndiv_v5", num="3.3")

PER_actif = PER %>% filter(PCS8 %in% c("02", "03", "04", "05", "06") &
                             Activ %in% c("10", "11", "12") & typoJo == "TRAV")

g = PER_actif %>% ggplot(aes(x = Dis.V/1000, y = Tps/60)) + geom_point(aes(color = typoModes)) +
  coord_cartesian(xlim = c(0,250), ylim = c(0,8)) +
  labs(title = "Distance/temps par mode", caption = src_fig(bu = T, date = "nov. 2022")) +
  xlab("distance totale (km)") + ylab("temps en déplacement (h)") +
  theme_bw()
print(g)

g = PER_actif %>% ggplot(aes(x = Dis.V/1000)) + geom_density() +
  labs(title = "Distance totale dans la journée", caption=src_fig(bu=T,date="nov. 2022")) +
  scale_x_log10() 
print(g)

g = PER_actif %>% ggplot(aes(x = (Tps_MAR + Tps_VEL) / Tps * 100)) + geom_density() +
  labs(title = "Part de mode actif dans la journée", caption=src_fig(bu=T,date="nov. 2022")) 
print(g)

g = PER_actif %>% ggplot(aes(x = (Tps_VOI) / Tps * 100)) + geom_density() +
  labs(title = "Part de voiture dans la journée", caption=src_fig(bu=T,date="nov. 2022")) 
print(g)


PER_actif = mutate(PER_actif, Dis.V_log = log(Dis.V), Tps_log = log(Tps)) %>%
  mutate(JoDeb = heureHHMMtoM(JoDeb), JoFin = heureHHMMtoM(JoFin)) %>%
  mutate(ModeActif = (Tps_MAR + Tps_VEL))


colActives = c("Dis.V", "nAct",
               "V", "ModeActif",
               "JoDeb", "JoFin",
               "DuDom", "DuTvl", "DuExt")

PER_actif = filter(PER_actif, !is.na(Dis.V_log), !is.na(Tps_log),
                   !is.infinite(Dis.V_log), !is.infinite(Tps_log))

factoIndiv = analyseFacto(base = PER_actif,
                          colVar = colActives,
                          colSup = c("PCS8", "PCSMT", "Age10", "Activ", "ZoneDens", "pendule", "Genre"),
                          colPoids = "CoeffEnq", colUids = "uid_PER", scaleW = T, desacFiltreNa=F)


factoIndiv_cat = categ_kMeans(factoIndiv, nomColonne = "factoIndiv_v5")   
PER_actif = left_join(PER_actif, factoIndiv_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = PER_actif,
          cols = c(colActives, "PCS8", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v5",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives, seuilAbsurde = 300)

profilageCategories(PER = PER_actif, champCateg = "factoIndiv_v5")

# refaire une typo avec les gens qui prennent leur voiture pour inclure les caracs de l'âge du véhicule

# c) Typologie de journée ACP (pour tester)

# d) Typologie du fichier Opinion (ACM)

# (Archives) FactoIndiv v6 ####

PER_f = PER %>%
  filter(DuTvl > 0 & PCS8 %in% c("02", "03", "04", "05", "06") & Activ %in% c("10", "11"))

rapport("nombre d'enquêté⋅es s'étant déplacé⋅es pour se rendre au travail :", nrow(PER_f), info=T)

# PER_f = PER_f %>%
#     filter(!is.na(Dis), !is.na(Tps), !is.na(N), !is.na(Travail_Dis), !is.na(DuTvl))

PER_f = mutate(PER_f,
               disTvlSurDis = Dis/Travail_Dis,
               DuTvl = DuTvl + DuEtu,
               DuCtt = DuCom + DuSvc + DuTax,
               NivDip = NivEtuVersNivDip(NivEtu)) %>%
  filter(!is.na(disTvlSurDis) & !is.infinite(disTvlSurDis))

# PER_f = PER_f %>%
#     mutate(
#            PCSMT = etqPCSM(PCSMT),
#            NivDip = etqNivDip(NivDip),
#            Activ = etqActiv(Activ),
#            ZoneDens = etqZoneDens(ZoneDens))

nrow(PER_f)


# interstices = ACT %>%
#     mutate(hDeb = heureHHMMtoM(hDeb), hFin = heureHHMMtoM(hFin)) %>%
#     left_join(select(PER, uid_PER, JoTvDeb, JoTvFin), by="uid_PER") %>%
#     mutate(JoTvDeb = JoTvDeb * 60, JoTvFin = JoTvFin * 60) %>%
#     mutate(travail = ifelse(Tache %in% c("101", "102", "103", "104", "105", "106", "109", "810"), T, F),
#            interstice = ifelse(hDeb > JoTvDeb & hFin < JoTvFin & travail == F, T, F)) %>%
#     group_by(uid_PER) %>% summarise(intersticeTotal = sum(ifelse(interstice, du, 0), na.rm=T),
#                                     intersticeMax   = max(ifelse(interstice, du, 0), na.rm=T)) %>%
#     mutate(intersticeMax = ifelse(is.infinite(intersticeMax), 0, intersticeMax))

# Pas très utilisable l'interstice...

PER_f %>%
  select(Dis, Tps, N, disTvlSurDis, DuTvl, DuCtt, DuLsr, JoTvDeb, JoTvFin, CoeffRecEnq) %>%
  pivot_longer(cols = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt",
                        "DuLsr", "JoTvDeb", "JoTvFin"), names_to = "var", values_to = "x") %>%
  ggplot(aes(x =x)) + geom_density() + facet_wrap(~var, scales = "free_x")

PER_f %>%
  select(Dis, Tps, N, disTvlSurDis, DuTvl, DuCtt, DuLsr, JoTvDeb, JoTvFin, CoeffRecEnq) %>%
  filter(!is.na(CoeffRecEnq)) %>%
  mutate(across(c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt",
                  "DuLsr", "JoTvDeb", "JoTvFin"), scale_w, w = CoeffRecEnq)) %>%
  mutate(across(c("Dis", "disTvlSurDis"), log)) %>%
  pivot_longer(cols = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt",
                        "DuLsr", "JoTvDeb", "JoTvFin"), names_to = "var", values_to = "x") %>%
  ggplot(aes(x =x)) + geom_density() + facet_wrap(~var)



# PER_f = left_join(PER_f, interstices, by="uid_PER")

sortie("Typologies/FactoIndiv v6", format = "pdf", taille = "a4")
factoIndiv = analyseFacto(base = PER_f,
                          colVar = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                                     "JoTvDeb", "JoTvFin"),
                          colSup = c("PCS8", "PCSMT", "Age10", "NivDip", "Activ", "ZoneDens", "pendule", "Genre"),
                          colUids = "uid_PER", colPoids = "CoeffRecEnq",
                          scaleW = T, desacFiltreNa=F, sortieTexte = T)
off()

factoIndivLog = analyseFacto(base = mutate(filter(PER_f, !is.na(CoeffRecEnq), Dis != 0, disTvlSurDis != 0),
                                           across(c("Dis", "disTvlSurDis"), log)),
                             colVar = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                                        "JoTvDeb", "JoTvFin"),
                             colSup = c("PCS8", "PCSMT", "Age10", "NivDip", "Activ", "ZoneDens", "pendule", "Genre"),
                             colUids = "uid_PER", colPoids = "CoeffRecEnq",
                             scaleW = T, desacFiltreNa=F, sortieTexte = T, silence = T)


PER_f = left_join(PER_f, factoIndiv, by = c("uid_PER" = "colUids"))

PER_f %>% group_by(Age = round(Age/2)*2) %>% summarise(Dim.1 = mean(Dim.1, na.rm=T),
                                                       Dim.2 = mean(Dim.2, na.rm=T),
                                                       Dim.3 = mean(Dim.3, na.rm=T), n = n()) %>%
  filter(n > seuilSignifiant) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, z = Dim.3)) + geom_path(aes(color = Age)) +
  scale_color_gradientn(colors = c("red", "orange", "yellow", "green", "blue", "purple"))

PER_f %>% group_by(Age = round(Age/2)*2, Genre) %>% summarise(Dim.1 = mean(Dim.1, na.rm=T),
                                                              Dim.2 = mean(Dim.2, na.rm=T),
                                                              Dim.3 = mean(Dim.3, na.rm=T), n = n()) %>%
  filter(n > seuilSignifiant) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, z = Dim.3)) + geom_path(aes(color = Age)) +
  scale_color_gradientn(colors = c("red", "orange", "yellow", "green", "blue", "purple")) +
  facet_wrap(~Genre)

PCSs = PER_f %>% group_by(Genre, PCS8) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS8 = etqPCS8(PCS8, genre = Genre))

PCSsDet = PER_f %>%
  group_by(Genre, PCS42S) %>% summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  mutate(etiqPCS42S = etqPCS42S(PCS42S, genre = Genre))

sortie("Typologies/Champ factoriel FactoIndiv v6")
PER_f %>%
  mutate(Dim.1 = round(Dim.1*10)/10, Dim.2 = round(Dim.2*10)/10) %>%
  group_by(Dim.1,
           Dim.2) %>% summarise(n = n(), dis = mean(Dis, na.rm=T)/1000, nDep = mean(N, na.rm=T)) %>%
  filter(dis < 120, nDep < 10) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(size = n, color = dis)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_label(data = PCSsDet, aes(label = etiqPCS42S, fill = Genre), alpha = .8, label.size=0,
             size = 2) +
  geom_label(data = PCSs, aes(label = etiqPCS8, fill = Genre), alpha = .8, label.size=0,
             fontface="italic") +
  scale_fill_hue(name = "genre", labels = c("femmes","hommes")) +
  scale_color_gradient(low = "green", high = "blue", name = "distance\nmoyenne\n(en km)") +
  scale_size(name = "nombre\nd'enquêté⋅es\npar\ncoord.") +
  coord_cartesian(xlim = c(-.8,.8), ylim = c(-.8,.8)) +
  labs(caption = src_fig())
off()

PER_f %>%
  mutate(Dim.1 = round(Dim.1*10)/10, Dim.3 = round(Dim.3*10)/10) %>%
  group_by(Dim.1,
           Dim.3) %>% summarise(n = n(), dis = mean(Dis, na.rm=T)/1000, nDep = mean(N, na.rm=T)) %>%
  filter(dis < 120, nDep < 10) %>%
  ggplot(aes(x = Dim.1, y = Dim.3)) +
  geom_point(aes(size = n, color = dis)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
  geom_label(data = PCSsDet, aes(label = etiqPCS42S, fill = Genre), alpha = .8, label.size=0,
             size = 2) +
  geom_label(data = PCSs, aes(label = etiqPCS8, fill = Genre), alpha = .8, label.size=0,
             fontface="italic") +
  scale_fill_hue(name = "genre", labels = c("femmes","hommes")) +
  scale_color_gradient(low = "green", high = "blue", name = "distance\nmoyenne\n(en km)") +
  scale_size(name = "nombre\nd'enquêté⋅es\npar\ncoord.") +
  coord_cartesian(xlim = c(-.8,.8), ylim = c(-.8,.8)) +
  labs(caption = src_fig())

PER_f %>% left_join(z_Nomenclature) %>% group_by(Libelle_Long) %>%
  summarise(across(starts_with("Dim"), mean, na.rm=T)) %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) + geom_text(aes(label = Libelle_Long), check_overlap = T) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)

factoIndiv_cat = categ_kMeans(factoIndiv, nomColonne = "factoIndiv_v6")   

PER_f = left_join(PER_f, factoIndiv_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = PER_f,
          cols = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                   "JoTvDeb", "JoTvFin", "PCS8", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v6",
          colPoids = "Coeff", colUid = "uid_PER",
          varActives = c("Dis", "Tps", "N", "disTvlSurDis", "DuTvl", "DuCtt", "DuLsr",
                         "JoTvDeb", "JoTvFin"), seuilAbsurde = 300)

profilageCategories(PER = PER_f, champCateg = "factoIndiv_v6") %>% print()

# acti_f = ACT %>% filter(uid_PER %in% PER_f$uid_PER) %>%
#     left_join(select(PER_f, PCS8, PCSMT, Age10, NivDip, Activ, ZoneDens, pendule, Genre, uid_PER), by="uid_PER") %>%
#     mutate(Tache = substr(Tache, 1, 2)) %>%
#     pivot_wider(names_from = Tache, values_from = du, names_prefix = "codeActi") %>%
#     mutate(across(starts_with("codeActi"), ~ifelse(is.na(.), 0, .))) %>%
#     analyseFacto(colVar = starts_with("codeActi"), colSup = c("PCS8", "PCSMT", "Age10", "NivDip", "Activ",
#                                                           "ZoneDens", "pendule", "Genre"),
#                  colUids = "uid_PER", scaleW = T, sortieTexte = T, colPoids = "CoeffRecEnq")


# Let's go spatial now

espace = PER_f %>%
  pivot_wider(names_from = factoIndiv_v6, values_from = CoeffRecEnq, names_prefix = "categ_") %>%
  group_by(ZT) %>% summarise(across(starts_with("categ_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "n") %>%
  group_by(ZT) %>% mutate(p = n / sum(n), n = sum(n)) %>%
  pivot_wider(names_from = categ, values_from = p) %>%
  ungroup()

# à enlever quand sera résolu
espace$ZT = ifelse(substr(espace$ZT, 1, 2) == "DP", paste0("EMP2019", espace$ZT), espace$ZT)
espace$uid_ENQ = substr(espace$ZT, 1, 7)

espaceFacto = analyseFacto(espace, colVar = paste0("categ_", c(1:7)), colUids = "ZT", colPoids = "n")

# espace = left_join(espace, categ_kMeans(espaceFacto, nomColonne = "espaceFacto"), by=c("ZT" = "uid"))

categs = categ_cah(espaceFacto, nomColonne = "espaceFacto", nCateg = 7)
espace = espace %>% left_join(categs, by=c("ZT" = "colUids"))

plotCateg(tab = espace,
          cols = paste0("categ_", c(1:7)), colCateg = "espaceFacto",
          colPoids = "n", colUid = "ZT", varActives = paste0("categ_", c(1:7)))


espace = left_join(select(shp_ZT, -uid_ENQ), espace, by=c("ZT" = "ZT"))
espace = left_join(espace, espaceFacto, by = c("ZT" = "colUids"))

sortie("Typologies/Atlas catégos spatiales ZT", portrait = F, format = "pdf", taille = "a4")
map_CarteParEnquete(shp = espace, colVal = "espaceFacto",
                    pdf = NULL,
                    format = "a4", paysage = T,
                    titre = "Classif. des secteurs de tirage par profils de travailleurs par lieu d'emploi",
                    legende = "Catégorie de\nsecteur de tirage", fdCarte = fdCarte)
off()

ggCarteZT(uid = "IDF2010", shp = espace, var = "Dim.1", descr_leg = "Dim.1")

uids = unique(espace$uid_ENQ)
gs   = lapply(uids, ggCarteZT, shp = espace, var = "Dim.1", descr_leg = "Position sur\nl'axe 1\nde l'ACP",
              titre = "Position des secteurs sur l'axe 1 de l'ACP", degrMin = "blue", degrMax = "green",
              lims = c(min(espace$Dim.1, na.rm=T), max(espace$Dim.1, na.rm=T)))

sortie("Typologies/Atlas catégos spatiales ZT - Axe 1", portrait = F, format = "pdf", taille = "a4")
print(gs)
off()

gs   = lapply(uids, ggCarteZT, shp = espace, var = "Dim.2", descr_leg = "Position sur\nl'axe 2\nde l'ACP",
              titre = "Position des secteurs sur l'axe 2 de l'ACP", degrMin = "red", degrMax = "blue",
              lims = c(min(espace$Dim.2, na.rm=T), max(espace$Dim.2, na.rm=T)))

sortie("Typologies/Atlas catégos spatiales ZT - Axe 2", portrait = F, format = "pdf", taille = "a4")
print(gs)
off()

# (Archives) Typo Atlas Nantais v1 ====

# colActives = c("Dis.V", "Tps", "nAct", "Dis.M",
#                "pDis_VOI", "pDis_TCO",
#                "pDis_CTR", "pDis_LSR",
#                "JoDeb", "JoFin", "DuDom",
#                "rapDomCtdDis")
# 
# EMD44 = filter(PER, uid_ENQ == "LOI2015" & !is.na(N))
# 
# factoIndiv_44 = analyseFacto(base = EMD44,
#                              colVar = colActives,
#                              colSup = c("PCS42S", "PCSMT", "Age10", "Activ", "ZoneDens", "Genre"),
#                              colPoids = "CoeffEnq", colUids = "uid_PER",
#                              scaleW = T, desacFiltreNa=T)
# 
# factoIndiv_44_cat = categ_kMeans(factoIndiv_44, nomColonne = "factoIndiv_44", nCateg = 7)                          
# 
# EMD44 = left_join(EMD44, factoIndiv_44_cat, by=c("uid_PER" = "uid"))
# 
# plotCateg(tab = EMD44,
#           cols = c(colActives, "PCS8", "PCSMT", "ZoneDens", "Genre", "typoJo"), colCateg = "factoIndiv_44",
#           colPoids = "Coeff", colUid = "uid_PER", varActives = colActives)


EMD44 = filter(PER, uid_ENQ == "LOI2015" & !is.na(N) & typoJo == "TRAV") %>%
  mutate(JoDeb = heureHHMMtoM(JoDeb), JoFin = heureHHMMtoM(JoFin))


communes_nantes_m = read_sf("Sources/Mailles/communes-nantes-metropole.geojson") # %>%
#     st_buffer(dist=20) %>%
#     mutate(oui = "oui") %>% group_by(oui) %>% summarize() %>%
#     st_transform(crs = 2154)

# trop moche, faisons autrement
load("Data/shp_COM.rds")
communes_nantes_m = filter(shp_COM, insee %in% communes_nantes_m$id_insee) %>%
  group_by(oui = "oui") %>% summarize()

# quelques chiffres pour la planche
weighted.mean(EMD44$DisOk, w=EMD44$CoeffEnq)
weighted.mean(EMD44$Dis.V, w=EMD44$CoeffEnq)
weighted.median(EMD44$Dis.V, w=EMD44$CoeffEnq)
weighted.mean(EMD44$Tps, w=EMD44$CoeffEnq) %>% heureMinToHr()

sum(filter(EMD44, modes_voiture == "oui")$CoeffEnq, na.rm=T)/sum(filter(EMD44, !is.na(modes_voiture))$CoeffEnq)*100
sum(filter(EMD44, Com == "44109" & modes_voiture == "oui")$CoeffEnq, na.rm=T)/sum(filter(EMD44, Com == "44109" & !is.na(modes_voiture))$CoeffEnq)*100

sum(filter(PER, typoJo == "TRAV" & uid_ENQ == "IDF2010" & Voiture == "voiture" |
             typoJo == "TRAV" & uid_ENQ == "IDF2010" & Voiture == "multi")$CoeffEnq, na.rm=T) / 
  sum(filter(PER, typoJo == "TRAV" & uid_ENQ == "IDF2010" & !is.na(Voiture))$CoeffEnq)*100

DescTools::Quantile(EMD44$Dis.V,
                    weights= EMD44$CoeffEnq,
                    na.rm=T, probs=c(.25,.75))

DescTools::Quantile(filter(EMD44, Com == "44109")$Dis.V,
                    weights= filter(EMD44, Com == "44109")$CoeffEnq,
                    na.rm=T, probs=c(.25,.75))

weighted.mean(filter(EMD44, Com == "44109")$Dis.V, w=filter(EMD44, Com == "44109")$CoeffEnq)
weighted.median(filter(EMD44, Com == "44109")$Dis.V, w=filter(EMD44, Com == "44109")$CoeffEnq)
weighted.mean(filter(EMD44, Com == "44109")$Tps, w=filter(EMD44, Com == "44109")$CoeffEnq) %>% heureMinToHr()

colActives = c("Dis.V", "Tps", "nAct", "V", # V tient lieu d'indicateur modal
               "DuDom", "DuTvl", "DuExt",
               "JoTvDeb", "JoTvFin")

sortie("Typologies/Atlas nantais, v1", taille = "a4", format = "pdf")

# Expérimental (26 octobre) : passer au log les distances et temps

viz_enTete(titre = "Profils de journée par individus", soustitre = "Partie 1")

factoIndiv44 = analyseFacto(base = mutate(filter(EMD44, Dis.V != 0 & Activ %in% c("10","11")),
                                          Dis.V = log(Dis.V),),
                            colVar = colActives,
                            colSup = c("PCS42S", "PCSMT", "Age10", "Activ", "ZoneDens", "pendule", "Genre"),
                            colPoids = "CoeffEnq", colUids = "uid_PER", scaleW = T, desacFiltreNa=T)

factoIndiv44_cat = categ_kMeans(factoIndiv44, nomColonne = "factoIndiv_v4", seuilCateg = 1.1, nCateg=8)                          

g1 = ggplot(data = left_join(factoIndiv44, factoIndiv44_cat, by=c("colUids" = "uid")),
            aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = factoIndiv_v4)) +
  scale_x_continuous(limits = c(-5,15)) + scale_y_continuous(limits = c(-10,10)) +
  scale_color_hue(name = "Catégorie") +
  labs(title = "Répartition des individus (journées)\nsur les plans 1 et 2 de l'analyse factorielle",
       caption = "Données EMD Cerema 2008-2018. Réalisation : Maxime Guinepain") +
  theme_bw() 
g2 = ggplot(data = left_join(factoIndiv44, factoIndiv44_cat, by=c("colUids" = "uid")),
            aes(x = Dim.1, y = Dim.3)) +
  geom_point(aes(color = factoIndiv_v4)) +
  scale_x_continuous(limits = c(-5,15)) + scale_y_continuous(limits = c(-10,10)) +
  scale_color_hue(name = "Catégorie") +
  labs(title = "Répartition des individus (journées)\nsur les plans 1 et 3 de l'analyse factorielle",
       caption = "Données EMD Cerema 2008-2018. Réalisation : Maxime Guinepain") +
  theme_bw() 

print(g1)
print(g2)

EMD44 = left_join(EMD44, factoIndiv44_cat, by=c("uid_PER" = "uid"))

plotCateg(tab = EMD44, titre = "Typologie de journées (type 4 : ACP / k-Means)",
          cols = c(colActives, "PCS8", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v4",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives)

plotCateg(tab = EMD44, titre = "Typologie de journées (intervalle de visu. élargi)",
          cols = c(colActives, "PCS8", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v4",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives,
          valIntervalleSur100 = 1)

profilageCategories(PER = EMD44, champCateg = "factoIndiv_v4")



viz_enTete(titre = "Profils de secteurs par profils d'individus,\nagrégé⋅es au lieu d'emploi",
           soustitre = "Partie 2.1")

EMD44_ZT = EMD44 %>%
  mutate(pop = CoeffEnq) %>%
  filter(!is.na(factoIndiv_v4)) %>%
  pivot_wider(names_from = factoIndiv_v4, values_from = CoeffEnq, names_prefix = c("factoIndiv_v4_")) %>%
  group_by(ZT_travMax) %>%
  summarize(across(starts_with("factoIndiv_v4_"), sum, na.rm=T), total = sum(pop, na.rm=T), n = n()) %>%
  mutate(across(starts_with("factoIndiv_v4_"), ~./total*100))

facto_zt_44 = analyseFacto(base = filter(EMD44_ZT, n > seuil & !is.na(ZT_travMax)),
                           colVar = colnames(select(EMD44_ZT, -ZT_travMax, -total, -n)),
                           colPoids = "total", colUids = "ZT_travMax",
                           scaleW = T, desacFiltreNa=T) %>%
  categ_cah(nomColonne = "factoIndivZt_v4", nCateg=5)    

EMD44_ZT = left_join(EMD44_ZT, select(facto_zt_44, -starts_with("Dim.")), by=c("ZT_travMax" = "colUids"))

infosocio = EMD44 %>%
  mutate(pop = CoeffEnq, pop2 = CoeffEnq, pop3 = CoeffEnq, pop4 = CoeffEnq) %>%
  pivot_wider(names_from = "PCS8", values_from = "CoeffEnq", names_prefix = "PCS_") %>%
  pivot_wider(names_from = "ModeDoux", values_from = "pop2", names_prefix = "Mode_Doux_") %>%
  pivot_wider(names_from = "TranspCo", values_from = "pop3", names_prefix = "Mode_TrCo_") %>%
  pivot_wider(names_from = "Voiture", values_from  = "pop4", names_prefix = "Mode_Voit_") %>%
  group_by(ZT_travMax) %>%
  summarize(across(starts_with("PCS_"), sum, na.rm=T),
            across(starts_with("Mode_"), sum, na.rm=T),
            total = sum(pop, na.rm=T), ageMoy = weighted.mean(Age, w = pop)) %>%
  mutate(across(starts_with("PCS_"), ~./total*100), across(starts_with("Mode_"), ~./total*100)) %>%
  select(-total)

EMD44_ZT = left_join(EMD44_ZT, infosocio, by=c("ZT_travMax"))

plotCateg(tab = EMD44_ZT, titre = "Profils de secteurs par types de journées de travail et par lieu d'emploi",
          cols = colnames(select(EMD44_ZT, -ZT_travMax, -factoIndivZt_v4, -total, -n)),
          colCateg = "factoIndivZt_v4",
          colPoids = "total", colUid = "ZT_travMax",
          varActives = colnames(select(EMD44_ZT, -ZT_travMax, -factoIndivZt_v4, -total, -n,
                                       -starts_with("PCS_"), -starts_with("Mode_"), -ageMoy)),
          valIntervalleGuibard = .5)

EMD44_ZT = left_join(EMD44_ZT, select(shp_ZT, ZT, uid_ENQ, geometry), by=c("ZT_travMax" = "ZT")) %>%
  st_as_sf()




etq= c(ml("Journées plutôt locales"),
       ml("Mixte, nombreuses j.", "mobiles séquencées"),
       ml("Mixte, journées locales", "et mobiles longues"),
       ml("Journées plutôt mobiles"),
       ml("Mixte,","horaires décalés fréquents"))


pal= c("#88aadf", "#6bcfd9", "#9e8bd6",  "#dfce88", "#9adf88")

map_CarteParEnquete(shp = EMD44_ZT, colVal = "factoIndivZt_v4",
                    titre = "Typologie des secteurs de travail par type de journée de travail",
                    legende = "Catégorie\nde la typologie\n(secteurs d'emploi)", etiquettes = etq, palette = pal,
                    fdCarte = fdCarte, ajPolygone = communes_nantes_m)

map_CarteParEnquete(shp = EMD44_ZT, colVal = "factoIndivZt_v4", colVal2 = "total", stockSup = T,
                    titre = "Typologie des secteurs de travail par type de journée de travail (stocks)",
                    legende = "Catégorie\nde la typologie\n(secteurs d'emploi)", etiquettes = etq, palette = pal,
                    legende2 = "Nombre d'emplois",
                    fdCarte = fdCarte, montrerAAV = "008", ajPolygone = communes_nantes_m)

viz_enTete(titre = "Profils de secteurs par profil d'individus,\nagrégé⋅es au lieu de résidence",
           soustitre = "Partie 2.2")

EMD44_ZTDOM = EMD44 %>%
  mutate(pop = CoeffEnq) %>%
  filter(!is.na(factoIndiv_v4)) %>%
  pivot_wider(names_from = factoIndiv_v4, values_from = CoeffEnq, names_prefix = c("factoIndiv_v4_")) %>%
  group_by(ZT) %>%
  summarize(across(starts_with("factoIndiv_v4_"), sum, na.rm=T), total = sum(pop, na.rm=T), n = n()) %>%
  mutate(across(starts_with("factoIndiv_v4_"), ~./total*100)) %>%
  filter(n > seuilSignifiant & !is.na(ZT))

facto_ztdom_44 = analyseFacto(base = EMD44_ZTDOM,
                              colVar = colnames(select(EMD44_ZTDOM, starts_with("factoIndiv_v4_"))),
                              colPoids = "total", colUids = "ZT",
                              scaleW = T, desacFiltreNa=T) %>%
  categ_cah(nomColonne = "factoIndivZtDom_v4", nCateg = 4) 

EMD44_ZTDOM = left_join(EMD44_ZTDOM, select(facto_ztdom_44, -starts_with("Dim.")), by=c("ZT" = "colUids"))

infosocio = EMD44 %>%
  mutate(pop = CoeffEnq, pop2 = CoeffEnq, pop3 = CoeffEnq, pop4 = CoeffEnq) %>%
  pivot_wider(names_from = "PCSMT", values_from = "CoeffEnq", names_prefix = "PCSMT_") %>%
  pivot_wider(names_from = "ModeDoux", values_from = "pop2", names_prefix = "Mode_Doux_") %>%
  pivot_wider(names_from = "TranspCo", values_from = "pop3", names_prefix = "Mode_TrCo_") %>%
  pivot_wider(names_from = "Voiture", values_from  = "pop4", names_prefix = "Mode_Voit_") %>%
  group_by(ZT) %>%
  summarize(across(starts_with("PCSMT_"), sum, na.rm=T),
            across(starts_with("Mode_"), sum, na.rm=T),
            total = sum(pop, na.rm=T), ageMoy = weighted.mean(Age, w = pop)) %>%
  mutate(across(starts_with("PCSMT_"), ~./total*100), across(starts_with("Mode_"), ~./total*100)) %>%
  select(-total)

EMD44_ZTDOM = left_join(EMD44_ZTDOM, infosocio, by=c("ZT"))

plotCateg(tab = EMD44_ZTDOM,
          titre = "Profils de secteurs par types de journées de travail et par lieu d'habitation",
          cols = colnames(select(EMD44_ZTDOM, -ZT, -factoIndivZtDom_v4, -total, -n)),
          colCateg = "factoIndivZtDom_v4",
          colPoids = "total", colUid = "ZT",
          varActives = colnames(select(EMD44_ZTDOM, -ZT, -factoIndivZtDom_v4, -total, -n,
                                       -starts_with("PCSMT_"), -starts_with("Mode_"), -ageMoy)))

EMD44_ZTDOM = left_join(EMD44_ZTDOM, select(shp_ZT, ZT, uid_ENQ, geometry), by=c("ZT" = "ZT")) %>%
  st_as_sf()

etq = c(ml("Journées locales,","plutôt pluriactives"),
        ml("Journées locales,","plutôt courtes"),
        ml("Journées mobiles simples"),
        ml("Journées mobiles","et très mobiles"))

pal = c("#83b8ef","#b583ef", "#efea83", "#efca83")

map_CarteParEnquete(shp = EMD44_ZTDOM, colVal = "factoIndivZtDom_v4",
                    titre = "Typologie des secteurs de travail par type de journée de travail",
                    legende = "Catégorie\nde la typologie\n(secteur de résidence)", etiquettes=etq, palette=pal,
                    fdCarte = fdCarte, ajPolygone = communes_nantes_m)

viz_enTete(titre = "Comparaison des deux typologies par secteur",
           soustitre = "Partie 2.3")

# On compare les deux cartes
# en identifiant quels sont les profils "urbains" et les profils "ruraux"

mediane = weighted.median(EMD44$Dis.V, w= EMD44$CoeffEnq)

zt_trav = EMD44 %>% left_join(select(EMD44_ZT, ZT_travMax, factoIndivZt_v4), by="ZT_travMax") %>%
  group_by(factoIndivZt_v4) %>% summarize(medianeCat = weighted.median(Dis.V, w= CoeffEnq)) %>%
  mutate(urbainOuRural_trav = ifelse(medianeCat < mediane, "urbain", "rural"),
         urbainOuRural_trav = as.factor(urbainOuRural_trav)) %>%
  filter(!is.na(factoIndivZt_v4))

zt_dom = EMD44 %>% left_join(select(EMD44_ZTDOM, ZT, factoIndivZtDom_v4), by="ZT") %>%
  group_by(factoIndivZtDom_v4) %>% summarize(medianeCat = weighted.median(Dis.V, w= CoeffEnq)) %>%
  mutate(urbainOuRural_dom = ifelse(medianeCat < mediane, "urbain", "rural"),
         urbainOuRural_dom = as.factor(urbainOuRural_dom)) %>%
  filter(!is.na(factoIndivZtDom_v4))

EMD44_ZT =    left_join(EMD44_ZT,    zt_trav, by="factoIndivZt_v4")
EMD44_ZTDOM = left_join(EMD44_ZTDOM, zt_dom,  by="factoIndivZtDom_v4")

EMD44_ZT = EMD44_ZTDOM %>% left_join(select(st_drop_geometry(EMD44_ZT),
                                            ZT_travMax, urbainOuRural_trav, factoIndivZt_v4),
                                     by = c("ZT" = "ZT_travMax")) %>%
  mutate(urbainOuRural = case_when(is.na(urbainOuRural_trav) ~ "peu d'emplois",
                                   urbainOuRural_dom == "urbain" &
                                     urbainOuRural_trav == "urbain" ~ "urbain",
                                   urbainOuRural_dom == "rural" &
                                     urbainOuRural_trav == "rural" ~ "rural",
                                   urbainOuRural_dom == "urbain" &
                                     urbainOuRural_trav == "rural" ~ "dom. urbain, travail rural",
                                   urbainOuRural_dom == "rural" &
                                     urbainOuRural_trav == "urbain" ~ "dom. rural, travail urbain"),
         urbainOuRural = factor(urbainOuRural, levels = c("urbain", "dom. urbain, travail rural",
                                                          "dom. rural, travail urbain", "rural")))

etq = c("journées locales",
        ml("journées des habitant-es locales", "journées des travailleur-ses plutôt mobiles"),
        ml("journées des habitant-es mobiles", "journées des travailleur-ses plutôt locales"),
        "journées mobiles")

pal = c("#83b8ef", "#c583ef", "#83ef9c",  "#efe883")

map_CarteParEnquete(shp = EMD44_ZT, colVal = "urbainOuRural",
                    titre = "Comparaison des deux typologies",
                    legende = ml("Type local ou mobile", "par lieu de domicile ou",
                                 "de travail (par comparaison", "de la médiane de la somme",
                                 "des distances par type", "à la médiane de l'ensemble)"),
                    etiquettes=etq, palette=pal,
                    fdCarte = fdCarte, ajPolygone = communes_nantes_m)

off()


# a) par activités dans le secteur ; carto des flux distances, à inclure ?

# b) par catégorie sociale de la population présente à 2h et à 14h (cartes à mettre fàf)

# c) par typologies individuelles

# d) refaire la même chose, sur un maillage arbitraire ?

# e) par catégorie du secteur d'origine, mais à 14h (sorte de schéma avec des flèches)

# TODO: Typologie des zones, non pas à partir de la part des profils, mais à partir de
# moyennes pondérées des positionnements des individus sur les 5 axes de l'analyse factorielle
# ... et la dispersion

# (Archives) Typo Atlas nantais v2 =====

communes_nantes_m = read_sf("Sources/Mailles/communes-nantes-metropole.geojson")

EMD44 = filter(PER, uid_ENQ == "LOI2015" & !is.na(N) & DuTvl > 0 & Activ %in% c("10","11"))

rapport("nombre d'individus en Loire-Atlantique :", nrow(filter(PER, uid_ENQ == "LOI2015")))
rapport("nombre de travailleur·ses pris·es en compte :", nrow(EMD44))

colActives = c("Dis", "Tps", "nAct", "V", # V tient lieu d'indicateur modal
               "DuDom", "DuTvl", "DuExt", "JoTvDeb", "JoTvFin")

factoIndiv44 = analyseFacto(base = mutate(filter(EMD44, Dis != 0),
                                          Dis = log(Dis),),
                            colVar = colActives,
                            colSup = c("PCS42S", "PCSMT", "Age10", "Activ", "ZoneDens", "pendule", "Genre"),
                            colPoids = "CoeffEnq", colUids = "uid_PER", scaleW = T, desacFiltreNa=F)

factoIndiv44_cat = categ_kMeans(factoIndiv44, nomColonne = "factoIndiv_v5")

EMD44 = left_join(EMD44, factoIndiv44_cat, by=c("uid_PER" = "uid"))

EMD44 = mutate(EMD44, factoIndiv_v5 = plyr::revalue(factoIndiv_v5, c("2" = "1-1", "4" = "1-2", "6" = "1-3",
                                                                     "5" = "2-1", "3" = "2-2",
                                                                     "1" = "3-1", "7" = "3-2")))

plotCateg(tab = EMD44, titre = "Typologie de journées (type 5)",
          cols = c(colActives, "PCS8", "PCSMT", "ZoneDens", "Genre", "pendule"), colCateg = "factoIndiv_v5",
          colPoids = "Coeff", colUid = "uid_PER", varActives = colActives)

# Part des individus
EMD44 %>%
  group_by(factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  mutate(p = n / sum(n))

# Stats pour le tableau, en moyennes pondérées
EMD44 %>%
  group_by(factoIndiv_v5) %>%
  select(Dis, Tps, V, nAct, DuDom, DuTvl, DuExt, JoTvDeb, JoTvFin, CoeffEnq) %>%
  summarise(across(everything(), ~weighted.mean(., CoeffEnq, na.rm=T))) %>%
  mutate(JoTvDeb = heureMinToHr(JoTvDeb*60), JoTvFin = heureMinToHr(JoTvFin*60),
         DuDom = heureMinToHr(DuDom), DuTvl = heureMinToHr(DuTvl),
         DuExt = heureMinToHr(DuExt), Tps = heureMinToHr(Tps))

# Parts modales
EMD44 %>%
  group_by(factoIndiv_v5) %>%
  summarise(nVoit = sum(ifelse(modes_voiture == "oui", CoeffEnq, 0), na.rm=T),
            nTC   = sum(ifelse(modes_tc      == "oui", CoeffEnq, 0), na.rm=T),
            nDoux = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffEnq, 0), na.rm=T),
            nTrain =sum(ifelse(modes_tc_rail == "oui", CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq), na.rm=T) %>%
  mutate(pVoit = nVoit/n, pTC = nTC/n, pDoux = nDoux/n, pTrain = nTrain/n)

EMD44 %>%
  summarise(nVoit = sum(ifelse(modes_voiture == "oui", CoeffEnq, 0), na.rm=T),
            nTC   = sum(ifelse(modes_tc      == "oui", CoeffEnq, 0), na.rm=T),
            nDoux = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffEnq, 0), na.rm=T),
            nTrain =sum(ifelse(modes_tc_rail == "oui", CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq), na.rm=T) %>%
  mutate(pVoit = nVoit/n, pTC = nTC/n, pDoux = nDoux/n, pTrain = nTrain/n)

# Composition sociale
EMD44 %>%
  group_by(factoIndiv_v5) %>%
  summarise(nFem = sum(ifelse(Genre == "F", CoeffEnq, 0), na.rm=T),
            nCad = sum(ifelse(PCS8 == "03", CoeffEnq, 0), na.rm=T),
            nEmp = sum(ifelse(PCS8 == "05", CoeffEnq, 0), na.rm=T),
            nOuv = sum(ifelse(PCS8 == "06", CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq), na.rm=T) %>%
  mutate(pFem = nFem/n, pCad = nCad/n, pEmp = nEmp/n, pOuv = nOuv/n)

# Composition sociale du tout
EMD44 %>%
  group_by(tout=T) %>%
  summarise(nFem = sum(ifelse(Genre == "F", CoeffEnq, 0), na.rm=T),
            nCad = sum(ifelse(PCS8 == "03", CoeffEnq, 0), na.rm=T),
            nEmp = sum(ifelse(PCS8 == "05", CoeffEnq, 0), na.rm=T),
            nOuv = sum(ifelse(PCS8 == "06", CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq), na.rm=T) %>%
  mutate(pFem = nFem/n, pCad = nCad/n, pEmp = nEmp/n, pOuv = nOuv/n)

# Selon résident·es
EMD44 %>%
  group_by(factoIndiv_v5) %>%
  summarise(resMet = sum(ifelse(Com %in% communes_nantes_m$id_insee, CoeffEnq, 0), na.rm=T),
            resDst = sum(ifelse(ZoneDens %in% c("1","2"), CoeffEnq, 0), na.rm=T),
            resDst_i = sum(ifelse(ZoneDens %in% c("3","4"), CoeffEnq, 0), na.rm=T),
            tvlDst = sum(ifelse(ZoneDens_travMax %in% c("1","2"), CoeffEnq, 0), na.rm=T),
            tvlDst_i = sum(ifelse(ZoneDens_travMax %in% c("3","4"), CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(pResMet = resMet/n, pDens = resDst/n, pDens_i = resDst_i/n,
         pDensTrav = tvlDst/n, pDensTrav_i = tvlDst_i/n)

EMD44 %>%
  group_by(tout = T) %>%
  summarise(resMet = sum(ifelse(Com %in% communes_nantes_m$id_insee, CoeffEnq, 0), na.rm=T),
            resDst = sum(ifelse(ZoneDens %in% c("1","2"), CoeffEnq, 0), na.rm=T),
            n = sum(CoeffEnq, na.rm=T)) %>%
  mutate(pResMet = resMet/n, pDens = resDst/n)

EMD44 %>%
  filter(!is.na(factoIndiv_v5)) %>%
  group_by(ZT, factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  group_by(ZT) %>% mutate(p = n / sum(n) * 100) %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = p)) + facet_wrap(~factoIndiv_v5) + scale_fill_viridis_b(option = "E")

EMD44 %>%
  filter(!is.na(factoIndiv_v5)) %>%
  group_by(ZT_travMax, factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  group_by(ZT_travMax) %>% mutate(p = n / sum(n) * 100) %>%
  left_join(shp_ZT, by=c("ZT_travMax" = "ZT")) %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = p)) + facet_wrap(~factoIndiv_v5) + scale_fill_viridis_b(option = "E")

EMD44 %>%
  filter(!is.na(factoIndiv_v5)) %>%
  group_by(ZT, factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  group_by(ZT) %>% mutate(p = n / sum(n) * 100) %>%
  filter(!factoIndiv_v5 %in% c("1","2")) %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = p)) + facet_wrap(~factoIndiv_v5) + scale_fill_gradient(low = "linen", high="firebrick")


EMD44_ZT = EMD44 %>% filter(!is.na(factoIndiv_v5)) %>%
  group_by(ZT, factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  group_by(ZT) %>% mutate(p = n / sum(n) * 100, pop = sum(n)) %>% select(-n) %>%
  pivot_wider(names_from = factoIndiv_v5, names_prefix = "dom_", values_from = p)

temp = EMD44 %>% filter(!is.na(factoIndiv_v5)) %>%
  group_by(ZT_travMax, factoIndiv_v5) %>%
  summarise(n = sum(CoeffEnq)) %>%
  group_by(ZT_travMax) %>% mutate(p = n / sum(n) * 100, pop = sum(n)) %>% select(-n) %>%
  pivot_wider(names_from = factoIndiv_v5, names_prefix = "tvl_", values_from = p)

EMD44_ZT = left_join(EMD44_ZT, temp, by=c("ZT" = "ZT_travMax"))
remove(temp)

EMD44_ZT = EMD44_ZT %>% mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  ungroup()

EMD44_ZT_cah = categ_cah(select(EMD44_ZT, -pop.x, -pop.y), colUids = "ZT", nCateg = 4)
EMD44_ZT = left_join(EMD44_ZT_cah, select(EMD44_ZT, ZT, pop.x, pop.y), by = "ZT")

EMD44_ZT %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = cluster))

plotCateg(tab = EMD44_ZT, titre = "Typologie de typologie (type 5)",
          cols = colnames(select(EMD44_ZT, -ZT, -cluster)),
          colCateg = "cluster", colUid = "ZT", varActives = colnames(select(EMD44_ZT, -ZT, -cluster)),
          valIntervalleSur100 = 1)

EMD44_ZT %>%
  group_by(cluster) %>%
  summarise(across(starts_with("dom_"), ~sum(.))) %>%
  pivot_longer(cols = starts_with("dom_"), names_to = "journée", values_to = "n") %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = journée), position = "stack")

EMD44_ZT %>%
  group_by(cluster) %>%
  summarise(across(starts_with("tvl_"), ~sum(.))) %>%
  pivot_longer(cols = starts_with("tvl_"), names_to = "journée", values_to = "n") %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = journée), position = "stack")

EMD44 %>% 
  select(ZT, PCS8, uid_PER, CoeffEnq) %>%
  filter(PCS8 %in% c("01","02","03","04","05","06")) %>%
  pivot_wider(names_from = "PCS8", values_from = "CoeffEnq", names_prefix = "PCS") %>%
  group_by(ZT) %>% summarise(across(starts_with("PCS"), ~sum(., na.rm=T))) %>%
  left_join(EMD44_ZT, by="ZT") %>%
  group_by(cluster) %>% summarise(across(starts_with("PCS"), ~sum(., na.rm=T))) %>%
  pivot_longer(cols = starts_with("PCS"), names_to = "PCS", values_to = "n") %>%
  group_by(cluster) %>% mutate(p = n / sum (n)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = PCS), position = "stack") +
  scale_fill_manual(values = pal_PCS8[1:6])

EMD44 %>% 
  select(ZT_travMax, PCS8, uid_PER, CoeffEnq) %>%
  filter(PCS8 %in% c("01","02","03","04","05","06")) %>%
  pivot_wider(names_from = "PCS8", values_from = "CoeffEnq", names_prefix = "PCS") %>%
  group_by(ZT_travMax) %>% summarise(across(starts_with("PCS"), ~sum(., na.rm=T))) %>%
  rename(ZT = ZT_travMax) %>%
  left_join(EMD44_ZT, by="ZT") %>%
  group_by(cluster) %>% summarise(across(starts_with("PCS"), ~sum(., na.rm=T))) %>%
  pivot_longer(cols = starts_with("PCS"), names_to = "PCS", values_to = "n") %>%
  group_by(cluster) %>% mutate(p = n / sum (n)) %>%
  filter(!is.na(cluster)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = PCS), position = "stack") +
  scale_fill_manual(values = pal_PCS8[1:6])



EMD44 %>% 
  select(ZT, pendule, uid_PER, CoeffEnq) %>%
  pivot_wider(names_from = "pendule", values_from = "CoeffEnq", names_prefix = "pendule") %>%
  group_by(ZT) %>% summarise(across(starts_with("pendule"), ~sum(., na.rm=T))) %>%
  left_join(EMD44_ZT, by="ZT") %>%
  group_by(cluster) %>% summarise(across(starts_with("pendule"), ~sum(., na.rm=T))) %>%
  pivot_longer(cols = starts_with("pendule"), names_to = "pendule", values_to = "n") %>%
  group_by(cluster) %>% mutate(p = n / sum (n)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = pendule), position = "stack")
EMD44 %>% 
  select(ZT_travMax, pendule, uid_PER, CoeffEnq) %>%
  pivot_wider(names_from = "pendule", values_from = "CoeffEnq", names_prefix = "pendule") %>%
  group_by(ZT_travMax) %>% summarise(across(starts_with("pendule"), ~sum(., na.rm=T))) %>%
  rename(ZT = ZT_travMax) %>%
  left_join(EMD44_ZT, by="ZT") %>%
  group_by(cluster) %>% summarise(across(starts_with("pendule"), ~sum(., na.rm=T))) %>%
  pivot_longer(cols = starts_with("pendule"), names_to = "pendule", values_to = "n") %>%
  group_by(cluster) %>% mutate(p = n / sum (n)) %>%
  filter(!is.na(cluster)) %>%
  ggplot(aes(x = cluster, y = p)) + geom_col(aes(fill = pendule), position = "stack")

EMD44 %>% left_join(select(EMD44_ZT, ZT, cluster), by="ZT") %>%
  group_by(cluster) %>% summarise(disMed = weighted.median(x = Dis, w = CoeffEnq, na.rm=T))

EMD44 %>% left_join(select(EMD44_ZT, ZT, cluster), by=c("ZT_travMax"="ZT")) %>%
  group_by(cluster) %>% summarise(disMed = weighted.median(x = Dis, w = CoeffEnq, na.rm=T))


tab = EMD44_ZT %>% group_by(cluster) %>% summarise(pop = sum(pop.x), tvl = sum(pop.y),
                                                   across(everything(), mean),
                                                   n = n()) %>%
  select(-ZT, -pop.x, -pop.y) %>% mutate(pop = pop/sum(pop), tvl = tvl/sum(tvl)) %>%
  pivot_longer(cols = starts_with(c("dom_", "tvl_")),
               names_to = "categIndiv", values_to = "part") %>%
  mutate(champ = substr(categIndiv, 1, 3), categ = substr(categIndiv, 5, 7)) %>%
  mutate(categ = factor(categ, levels = sort(unique(categ), decreasing=T))) %>%
  mutate(cluster = plyr::revalue(cluster, c("1" = "1- mobilité plutôt locale",
                                            "2" = "2- mobilité locale / intermédiaire",
                                            "3" = "3- résident⋅es mobiles, emploi mixte",
                                            "4" = "4- mobilité plutôt grande"))) %>%
  mutate(cluster = paste0(as.character(cluster), "\n(", round(pop*100), " % de la population",
                          "\n", round(tvl * 100), " % des emplois)")) %>%
  mutate(champ = plyr::revalue(champ, c("dom" = "parmi les résident⋅es",
                                        "tvl" = "parmi les travailleur⋅ses")))

sortie("Typologies/Atlas nantais, légende")
ggplot(data= tab, aes(x = cluster, y = part)) + geom_col(aes(fill = categ)) +
  scale_fill_hue(breaks = c("1-1", "1-2", "1-3", "2-1", "2-2", "3-1", "3-2"),
                 labels = c("journées de mobilité locale",
                            "journées écourtées",
                            "journées décalées",
                            "journées biactives",
                            "journées pluriactives",
                            "journées de grande mobilité",
                            "journées de très grande mobilité"),
                 name = "Types de journée") +
  scale_x_discrete(limits = rev(unique(tab$cluster))) +
  facet_grid(~champ) + coord_flip() +
  xlab("type de secteur") + ylab("part (%)") +
  theme(legend.position = "bottom") +
  labs(title = ml("Typologie des secteurs en fonction des journées",
                  "des personnes qui y résident et qui y travaillent")) + 
  guides(fill = guide_legend(ncol = 2)) %>%
  print()

off()

sortie("Typologies/Atlas nantais, légende, vectoriel", format = "svg")
ggplot(data= tab, aes(x = cluster, y = part)) + geom_col(aes(fill = categ)) +
  scale_fill_hue(breaks = c("1-1", "1-2", "1-3", "2-1", "2-2", "3-1", "3-2"),
                 labels = c("journées de mobilité locale",
                            "journées écourtées",
                            "journées décalées",
                            "journées biactives",
                            "journées pluriactives",
                            "journées de grande mobilité",
                            "journées de très grande mobilité"),
                 name = "Types de journée") +
  scale_x_discrete(limits = rev(unique(tab$cluster))) +
  facet_grid(~champ) + coord_flip() +
  xlab("type de secteur") + ylab("part (%)") +
  theme(legend.position = "bottom") +
  labs(title = ml("Typologie des secteurs en fonction des journées",
                  "des personnes qui y résident et qui y travaillent")) + 
  guides(fill = guide_legend(ncol = 2)) +
  theme_void(base_size = 10) %>%
  print()
off()



etendue = st_as_sf(left_join(EMD44_ZT, shp_ZT, by="ZT"))

#communes_nantes_m = filter(shp_COM, insee %in% communes_nantes_m$id_insee) %>%
#    group_by(oui = "oui") %>% summarize()

g = EMD44_ZT %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = cluster), color = "gray85", size=.2) +
  scale_fill_manual(values = c("orange", "khaki", "cadetblue3", "plum"),
                    name = "type de secteur",
                    labels = c("mobilité plutôt\nlocale", "local-intermédiaire",
                               "résident·es mobiles,\nemploi mixte", "mobilité plutôt grande"))

g = cartoHydro(g = g, etendue = etendue)

g = g +
  geom_sf(data = summarise(group_by(st_buffer(communes_nantes_m, dist=10), oui="oui")),
          aes(color = "Métropole de Nantes"), fill = NA, size=1) +
  scale_color_manual(values = "slateblue", name = NULL)

g = cartoLib  (g = g, etendue = etendue, tailleTexte = 11)
g = cartoFinish(g, etendue)
g = g + labs(title = "Une géographie des types de journées qui\nrenvoie à la hiérarchie urbaine de la Loire-Atlantique",
             subtitle = ml("Analyse réalisée à partir des journées des résident·es et des travailleur·ses",
                           "de chaque secteur (définition Cerema)",
                           "Maille spatiale spécifique à l'enquête (environ 4.000 habitant⋅es par secteur)"),
             caption = src_fig(base = filter(PER, uid_ENQ == "LOI2015"), carto = T))

sortie("Typologies/Atlas Nantais, v2", taille = "page", portrait = F)
print(g)
off()

sortie("Typologies/Atlas Nantais, v2", format = "svg", taille = "page", portrait = F, pointsize = 10)
print(g + theme_bw(base_size = 10)) + ggRetirerAxeX + ggRetirerAxeY
off()

g = EMD44_ZT %>%
  left_join(shp_ZT, by="ZT") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(fill = cluster), color = "gray85", size=.2) +
  scale_fill_manual(values = c("orange", "khaki", "cadetblue3", "plum"),
                    name = "type de secteur",
                    labels = c("mobilité plutôt\nlocale", "local-intermédiaire",
                               "résident·es mobiles,\nemploi mixte", "mobilité plutôt grande"))

aav = filter(centroidesAAV(), LIBAAV2020 == "Nantes") %>%
  group_by(Com, LIBAAV2020) %>% summarise() %>%
  left_join(shp_COM, by = c("Com" = "insee")) %>% st_as_sf() %>%
  st_simplify(dTolerance = 200) %>%
  group_by(tout = T) %>%
  st_buffer(dist = 150) %>%
  summarise()

g = g +
  geom_sf(data = summarise(group_by(st_buffer(communes_nantes_m, dist=10), oui="oui")),
          aes(color = "Métropole de Nantes"), fill = NA, size=1) +
  geom_sf(data = aav,
          aes(color = "Aire d'attraction\nde Nantes"), fill = NA, size=1) +
  scale_color_manual(values = c("slategrey", "slateblue"), name = NULL)

g = cartoFinish(g, etendue)
g =  g + labs(title = "Une géographie des types de journées qui\nrenvoie à la hiérarchie urbaine de la Loire-Atlantique",
              subtitle = ml("Analyse réalisée à partir des journées des résident·es",
                            "et des travailleur·ses de chaque secteur",
                            "(secteurs ad hoc regroupant environ 4000 habitant⋅es)"),
              caption = src_fig(base = filter(PER, uid_ENQ == "LOI2015"), carto = T))

sortie("Typologies/Atlas Nantais, v2", portrait = F, taille = "page")
print(cartoLib(g + theme_void() + ggRetirerAxeX + ggRetirerAxeY, etendue = etendue, detail = 3))
off()

sortie("Typologies/Atlas Nantais, carte, vectoriel", format = "svg", taille = "page", portrait = F, pointsize = 10)
print(g + theme_void(base_size = 10)) + ggRetirerAxeX + ggRetirerAxeY
off()


# (Archives) Gradient centre-périphérie ====
# Quelques tests exploratoires de corrélation (sept. 2023)
# Abandonné en octobre 2023

# load("Data/PER.rds")
# centroAAVs = centroidesAAV()
# remove(PER)
# 
# PER_ff_test = left_join(PER_ff, centroAAVs, by=c("ZF" = "ZF")) %>%
#   mutate(disCentre = dis/1000)

cor.test(log(filter(PER_ff_test, Dis != 0)$disCentre), log(filter(PER_ff_test, Dis != 0)$Dis))
cor.test(log(PER_ff_test$disCentre), log(PER_ff_test$Tps))
cor.test(log(PER_ff_test$disCentre), log(PER_ff_test$N))
cor.test(log(filter(PER_ff_test, disTvlSurDis != 0)$disCentre), log(filter(PER_ff_test, disTvlSurDis != 0)$disTvlSurDis))

cor.test(log(PER_ff_test$disCentre), PER_ff_test$DuTvl)
cor.test(log(PER_ff_test$disCentre), PER_ff_test$DuCtt)
cor.test(log(PER_ff_test$disCentre), PER_ff_test$DuLsr)

cor.test(log(PER_ff_test$disCentre), PER_ff_test$JoTvDeb)
cor.test(log(PER_ff_test$disCentre), PER_ff_test$JoTvFin)

hist(PER_ff_test$disCentre)
hist(PER_ff_test$Dis)
hist(PER_ff_test$Tps)
hist(PER_ff_test$N)
hist(PER_ff_test$disTvlSurDis)
hist(log(PER_ff_test$disTvlSurDis))
hist(PER_ff_test$DuTvl)
hist(PER_ff_test$DuCtt)
hist(PER_ff_test$DuLsr)
hist(PER_ff_test$JoTvDeb)
hist(PER_ff_test$JoTvFin)

# En revanche toujours un petit test à faire
cor.test(log(PER_ff_zt$Dis), log(PER_ff_zt$densite))
cor.test(log(PER_ff_zt_trav$Dis), log(PER_ff_zt_trav$densite))

cor.test(log(PER_ff_zt$Tps), log(PER_ff_zt$densite))
cor.test(log(PER_ff_zt_trav$Tps), log(PER_ff_zt_trav$densite))

# (Archives) Typologie CAH espaces de résidence ====
# Abandonné en octobre 2023

espace = PER_ff %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "categ_") %>%
  group_by(ZT) %>% summarise(across(starts_with("categ_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "n") %>%
  group_by(ZT) %>% mutate(p = n / sum(n), n = sum(n)) %>%
  pivot_wider(names_from = categ, values_from = p) %>%
  ungroup()

# à enlever quand sera résolu
espace$ZT = ifelse(substr(espace$ZT, 1, 2) == "DP", paste0("EMP2019", espace$ZT), espace$ZT)
espace$uid_ENQ = substr(espace$ZT, 1, 7)

# en fait il vaut mieux virer l'EMP
espace = filter(espace, uid_ENQ != "EMP2019")

# Nouvelle analyse factorielle
espaceFacto = analyseFacto(espace, colVar = paste0("categ_", c(1:6)), colUids = "ZT", colPoids = "n")

# CAH
categs = categ_cah(select(espaceFacto, colUids, Dim.1, Dim.2, Dim.3), nomColonne = "espaceFacto",nCateg = 7)
espace = espace %>% left_join(categs, by=c("ZT" = "colUids"))
plotCateg(tab = espace,
          cols = paste0("categ_", c(1:6)), colCateg = "espaceFacto",
          colPoids = "n", colUid = "ZT", varActives = paste0("categ_", c(1:6)))
# NB : Pourquoi ne pas faire directement une CAH par part finalement ?

espace_shp = left_join(select(shp_ZT, -uid_ENQ), espace, by=c("ZT" = "ZT"))
espace_shp = left_join(espace_shp, espaceFacto, by = c("ZT" = "colUids"))

catPop = PER_ff %>%
  group_by(ZT) %>% summarise(pop = sum(CoeffRecEnq),
                             pop05 = sum(ifelse(PCS8 == "05", CoeffRecEnq, 0)),
                             pop06 = sum(ifelse(PCS8 == "06", CoeffRecEnq, 0))) %>%
  mutate(clPop = (pop05 + pop06)/pop * 100) %>%
  mutate(clPop = discretisation(clPop))

espace_shp = left_join(espace_shp, catPop, by = "ZT")

espace_shp$etiquette = plyr::revalue(espace_shp$espaceFacto,
                                     c("1" = "locales-typiques +",
                                       "2" = "indirectes +",
                                       "3" = "profil moyen",
                                       "4" = "décalées, locales +",
                                       "5" = "directes, étendues +",
                                       "6" = "étendues +",
                                       "7" = "décalées +"))


group_by(espace, espaceFacto) %>% summarise(n = n()) %>% mutate(p = n/sum(n))

tab = filter(PER_ff, ZT %in% filter(espace, espaceFacto == "1")$ZT, !is.na(CoeffRecEnq))
weighted.mean(x = tab$Dis, w = tab$CoeffRecEnq, na.rm=T)

mean(filter(espace, espaceFacto == "1")$categ_1)
mean(filter(espace, espaceFacto == "5")$categ_1)
mean(filter(espace, espaceFacto == "5")$categ_4)

tab = filter(PER_ff, ZT %in% filter(espace, espaceFacto == "5")$ZT, !is.na(CoeffRecEnq))
weighted.mean(x = tab$Dis, w = tab$CoeffRecEnq, na.rm=T)

# (Archives) Planche carto typologie CAH ====
# Abandonné en octobre 2023

couleurs = c("#ECE2F4", "#D6F6DE", "#DADDFC", "#ffcdd7", "#FEF9DA", "#FEE7B7", "#fbb598")
#D6F6DE
# gs   = lapply(unique(espace$uid_ENQ), ggCarteCat, shp = espace_shp, var = "espaceFacto",
#               descr_leg = "Surreprésentations\nremarquables",
#               titre = "Typologie de secteurs selon les journées des résident⋅es", couleurs = couleurs)
# print(gs)

g1 = ggCarteCat(uid = "CAL2011", shp = espace_shp, var = "etiquette",
                descr_leg = "journées\nsurreprésentées",
                titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                couleurs = couleurs, detailLabs = 3)

g1b = ggCarteCat(uid = "CAL2011", shp = espace_shp, var = "etiquette",
                 descr_leg = "journées\nsurreprésentées",
                 titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                 couleurs = couleurs, detailLabs = 3, filtreZT = paste0("CAL20110", c(101:134)),
                 champPop = "pop", legPop = "Population active\nen emploi",
                 champPart = "clPop", legPart = "Part d'ouvrier·es\net d'employé·es (%)")

g1 = g1 + geom_sf(data = cartoEtendue(g1b$data, enq = "CAL2011"), linetype=2, fill = NA) +
  coord_sf(xlim = c(st_bbox(g1$data)$xmin, st_bbox(g1$data)$xmax),
           ylim = c(st_bbox(g1$data)$ymin, st_bbox(g1$data)$ymax))

g2 = ggCarteCat(uid = "CLF2012", shp = espace_shp, var = "etiquette",
                descr_leg = "journées\nsurreprésentées",
                titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                couleurs = couleurs, detailLabs = 3)

g2b = ggCarteCat(uid = "CLF2012", shp = espace_shp, var = "etiquette",
                 descr_leg = "journées\nsurreprésentées",
                 titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                 couleurs = couleurs, detailLabs = 3, filtreZT = paste0("CLF20120", c(101:132)),
                 champPop = "pop", legPop = "Population active\nen emploi",
                 champPart = "clPop", legPart = "Part d'ouvrier·es\net d'employé·es (%)")

# g2 = g2 + geom_sf(data = cartoEtendue(g2b$data, enq = "CLF2012"), linetype=2, fill = NA) +
#     coord_sf(xlim = c(st_bbox(g2$data)$xmin, st_bbox(g2$data)$xmax),
#              ylim = c(st_bbox(g2$data)$ymin, st_bbox(g2$data)$ymax))

g3 = ggCarteCat(uid = "LOI2015", shp = espace_shp, var = "etiquette",
                descr_leg = "journées\nsurreprésentées",
                titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                couleurs = couleurs, detailLabs = 3)

g3b = ggCarteCat(uid = "LOI2015", shp = espace_shp, var = "etiquette",
                 descr_leg = "journées\nsurreprésentées",
                 titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                 couleurs = couleurs, detailLabs = 3, filtreZT = c(paste0("LOI2015000", c(1:9)),
                                                                   paste0("LOI201500", c(10:54))),
                 champPop = "pop", legPop = "Population active\nen emploi",
                 champPart = "clPop", legPart = "Part d'ouvrier·es\net d'employé·es (%)")

g3c = ggCarteCat(uid = "LOI2015", shp = espace_shp, var = "etiquette",
                 descr_leg = "journées\nsurreprésentées",
                 titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                 couleurs = couleurs, detailLabs = 3, filtreZT = c(paste0("LOI20150", c(110:121))),
                 champPop = "pop", legPop = "Population active\nen emploi",
                 champPart = "clPop", legPart = "Part d'ouvrier·es\net d'employé·es (%)")

g3 = g3 + geom_sf(data = cartoEtendue(g3b$data, enq = "LOI2015"), linetype=2, fill = NA) +
  coord_sf(xlim = c(st_bbox(g3$data)$xmin, st_bbox(g3$data)$xmax),
           ylim = c(st_bbox(g3$data)$ymin, st_bbox(g3$data)$ymax))

g4 = ggCarteCat(uid = "LYO2015", shp = espace_shp, var = "etiquette",
                descr_leg = "journées\nsurreprésentées",
                titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                couleurs = couleurs, detailLabs = 3)

g4b = ggCarteCat(uid = "LYO2015", shp = espace_shp, var = "etiquette",
                 descr_leg = "journées\nsurreprésentées",
                 titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
                 couleurs = couleurs, detailLabs = 3, filtreZT = c(paste0("LYO20150", c(101:245))),
                 champPop = "pop", legPop = "Population active\nen emploi",
                 champPart = "clPop", legPart = "Part d'ouvrier·es\net d'employé·es (%)")

g4 = g4 + geom_sf(data = cartoEtendue(g4b$data, enq = "LYO2015"), linetype=2, fill = NA) +
  coord_sf(xlim = c(st_bbox(g4$data)$xmin, st_bbox(g4$data)$xmax),
           ylim = c(st_bbox(g4$data)$ymin, st_bbox(g4$data)$ymax))

sortie("Typologies/Typo spatiale IDF")
ggCarteCat(uid = "IDF2010", shp = espace_shp, var = "etiquette",
           descr_leg = "journées\nsurreprésentées",
           titre = "Typologie des des secteurs selon les journées de travail des résident⋅es",
           couleurs = couleurs, detailLabs = 3) %>% print()
off()

# table présentant la composition de chaque catégorie
tab1 = espace_shp %>% filter(!is.na(etiquette)) %>%
  st_drop_geometry() %>%
  group_by(etiquette) %>%
  summarise(across(starts_with("categ_"), mean)) %>%
  select(etiquette, categ_1, categ_2, categ_3, categ_4, categ_5, categ_6)

tabMoy = espace_shp %>% filter(!is.na(etiquette)) %>%
  st_drop_geometry() %>%
  summarise(across(starts_with("categ_"), mean)) %>%
  select(categ_1, categ_2, categ_3, categ_4, categ_5, categ_6) %>%
  pivot_longer(cols=everything(), names_to = "categ", values_to="pMoy")

tabSd = espace_shp %>% filter(!is.na(etiquette)) %>%
  st_drop_geometry() %>%
  summarise(across(starts_with("categ_"), sd)) %>%
  select(categ_1, categ_2, categ_3, categ_4, categ_5, categ_6) %>%
  pivot_longer(cols=everything(), names_to = "categ", values_to="sd")

tab2 = tab1 %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "p") %>%
  left_join(tabMoy, by="categ") %>%
  mutate(sr = pMoy / p,
         sr = (1 - sr)*100) %>%
  select(-p, -pMoy) %>% pivot_wider(names_from = categ, values_from = sr)

tabtxt = mutate(tab1, across(starts_with("categ_"), ~paste0(round(.*100), " %")))
colnames(tabtxt) = c("journées\nsurreprésentées", gsub(pattern = "-", replacement = "-\n", categLabs))

taillePol = 8
t = ggpubr::ggtexttable(tabtxt, rows=NULL, theme=ggpubr::ttheme("classic", base_size = taillePol,
                                                                padding = unit(c(1.5,2), "mm")))
for (i in 1:length(levels(espace_shp$etiquette)))
{
  t = ggpubr::table_cell_bg(t, i+1, 1, fill = couleurs[i])
}
for (i in 1:length(unique(filter(PER_ff, !is.na(factoIndiv_v6f))$factoIndiv_v6f)))
{
  for (j in 1:length(levels(espace_shp$etiquette)))
  {
    val = tab1[[j, i+1]]
    moy = tabMoy$pMoy[i]
    std = tabSd$sd[i]
    
    if(val > moy + .5*std) { t = ggpubr::table_cell_font(t, row = j+1, col = i+1, color = "tomato", size=taillePol) }
    if(val < moy - .5*std) { t = ggpubr::table_cell_font(t, row = j+1, col = i+1, color = "steelblue3", size=taillePol)}
    if(val > moy + std) { t = ggpubr::table_cell_font(t, row = j+1, col = i+1, face = "bold", color = "tomato", size=taillePol) }
    if(val < moy - std) { t = ggpubr::table_cell_font(t, row = j+1, col = i+1, face = "bold", color = "steelblue3", size=taillePol) }
  }
}

duoTitre = cowplot::plot_grid(cowplot::ggdraw() + cowplot::draw_text(text = ml("Typologie des secteurs selon les profils",
                                                                               "des journées de travail des résident⋅es"),
                                                                     hjust = 0, x = .02, size = 11, fontface = "bold"),
                              cowplot::ggdraw() + cowplot::draw_text(hjust = 1, x = .97, size = 9, fontface="italic",
                                                                     text = paste0("Réalisation : Maxime Guinepain,\n", moisEnCours())),
                              nrow = 1, rel_widths = c(.7, .3))

triLegendes = cowplot::plot_grid(cowplot::get_legend(g4b + theme(legend.position = "bottom") +
                                                       guides(fill = "none", size = guide_legend(nrow = 2), colour = "none")),
                                 cowplot::get_legend(g4b + theme(legend.position = "bottom") +
                                                       guides(fill = "none", size = "none", colour = guide_legend(nrow = 3))),
                                 cowplot::ggdraw() +
                                   cowplot::draw_text(hjust = 1, x = .97, size = 9, face="italic",
                                                      c("Réalisation : Maxime Guinepain,\njuillet 2023.")),
                                 ncol = 1, nrow = 3)

g1plus = cowplot::plot_grid(g1 + labs(title = NULL) + theme(legend.position = "none"),
                            g1b + labs(title = NULL) + theme(legend.position = "none",
                                                             panel.border = element_blank(),
                                                             panel.grid = element_blank(),
                                                             plot.subtitle = element_text(hjust=.5, face = "italic"),
                                                             plot.caption = element_blank()), nrow=2)
g2plus = cowplot::plot_grid(g2 + labs(title = NULL) + theme(legend.position = "none"),
                            cowplot::ggdraw(), nrow=1)
g1g2 = cowplot::plot_grid(g1plus, g2 + theme(legend.position = "none") + labs(title = NULL), nrow=1)
g3plus = cowplot::plot_grid(g3 + labs(title = NULL) + theme(legend.position = "none"),
                            g3b + labs(title = NULL) + theme(legend.position = "none",
                                                             panel.border = element_blank(),
                                                             panel.grid = element_blank(),
                                                             plot.subtitle = element_text(hjust=.5, face = "italic"),
                                                             plot.caption = element_blank()), nrow=1)
g4plus = cowplot::plot_grid(g4 + labs(title = NULL) + theme(legend.position = "none"),
                            g4b + labs(title = NULL) + theme(legend.position = "none",
                                                             panel.border = element_blank(),
                                                             panel.grid = element_blank(),
                                                             plot.subtitle = element_text(hjust=.5, face = "italic"),
                                                             plot.caption = element_blank()), nrow=1)
legendes = cowplot::plot_grid(cowplot::get_legend(g4b + theme(legend.position = "bottom") +
                                                    guides(fill = "none", size = guide_legend(nrow = 2), colour = "none")),
                              cowplot::get_legend(g4b + theme(legend.position = "bottom") +
                                                    guides(fill = "none", size = "none", colour = guide_legend(nrow = 3))), nrow=1)

titreLegende = cowplot::ggdraw() + cowplot::draw_text(hjust = 0, x = .02, y = .5, size = 10, fontface="bold", "Légende")

tplus = cowplot::plot_grid(t, cowplot::ggdraw() +
                             cowplot::draw_text(ml("Part des journées de chaque","type dans chaque secteur", "",
                                                   "Valeurs en bleu : plus d'un demi", "écart type sous la moyenne",
                                                   "Valeurs en rouge : plus d'un demi", "écart-type au-dessus de la moyenne",
                                                   "Valeurs en gras : plus d'un écart", "type de différence"),
                                                size=8, fontface = "italic", x=.05, y=.95, vjust=1, hjust=0),
                           ncol = 2, rel_widths = c(7, 3))

page1 = cowplot::plot_grid(duoTitre, g1g2, titreLegende, tplus, legendes, ncol = 1, rel_heights=c(.75,8,.25,2,1))
page2 = cowplot::plot_grid(duoTitre, g3plus, g4plus, titreLegende, tplus, legendes, ncol = 1, rel_heights=c(.75,4,4,.25,2,1))

sortie("Typologies/Planche cartes 1", taille = "page", portrait = T)
plot(page1)
off()
sortie("Typologies/Planche cartes 2", taille = "page", portrait = T)
plot(page2)
off()

# (Archives) Composition sociale des secteurs ====

# Classement des secteurs en fonction des taux de cadres et d'ouvriers
load("Data/PER.rds")
partsZTdom = PER %>% filter(Activ %in% c("10", "11", "31"), PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  select(-PCS_insee) %>%
  pivot_wider(values_from = CoeffRec, names_from = PCS8, names_prefix = "PCS_") %>%
  group_by(ZT) %>% summarise(across(starts_with("PCS_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("PCS_"), names_to = "PCS8", values_to = "pop") %>%
  group_by(ZT) %>% mutate(p = pop / sum(pop)) %>%
  select(-pop) %>% pivot_wider(values_from = p, names_from = PCS8)

espace = left_join(espace, partsZTdom, by="ZT")

mean(espace$PCS_03)
mean(filter(espace, espaceFacto == "2")$PCS_03)
mean(filter(espace, espaceFacto == "3")$PCS_03)
mean(filter(espace, espaceFacto == "4")$PCS_03)

mean(espace$PCS_02)
mean(filter(espace, espaceFacto == "2")$PCS_02)
mean(filter(espace, espaceFacto == "3")$PCS_02)
mean(filter(espace, espaceFacto == "4")$PCS_02)

mean(espace$PCS_06)
mean(filter(espace, espaceFacto == "2")$PCS_06)
mean(filter(espace, espaceFacto == "3")$PCS_06)
mean(filter(espace, espaceFacto == "4")$PCS_06)

mean(espace$PCS_05)
mean(filter(espace, espaceFacto == "2")$PCS_05)
mean(filter(espace, espaceFacto == "3")$PCS_05)
mean(filter(espace, espaceFacto == "4")$PCS_05)

mean(filter(espace, espaceFacto == "7")$PCS_06)
mean(filter(espace, espaceFacto == "7")$PCS_05)

g = espace %>%
  pivot_longer(cols = starts_with("PCS_"), names_to = "PCS8", values_to = "p") %>%
  mutate(PCS8 = etqPCS8(substr(PCS8, 5,6))) %>%
  mutate(p = paliers(p*100, 5)) %>%
  group_by(p, PCS8) %>% summarise(across(starts_with("categ_"), mean, na.rm=T), n = n()) %>%
  filter(n>seuilSignifiant) %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "pMoy") %>%
  filter(categ != "categ_NA") %>%
  ggplot(aes(x = p, y = pMoy*100)) + geom_line(aes(color = categ)) + facet_wrap(~PCS8) +
  coord_cartesian(xlim = c(0, 100)) +
  xlab("part de la PCS dans le secteur (%)") + ylab("part du type de journée dans le secteur (%)") +
  scale_color_hue(labels = categLabs, name = "part du type\nde journée\npar rapport\naux résident⋅es")
sortie("Typologies/Type journée selon type secteur de résidence")
print(g)
off()

partsZTtvl = PER %>% filter(Activ %in% c("10", "11", "31"), PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  select(-PCS_insee) %>%
  pivot_wider(values_from = CoeffRec, names_from = PCS8, names_prefix = "travPCS_") %>%
  group_by(ZT_travMax) %>% summarise(across(starts_with("travPCS_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("travPCS_"), names_to = "PCS8", values_to = "pop") %>%
  group_by(ZT_travMax) %>% mutate(p = pop / sum(pop)) %>%
  select(-pop) %>% pivot_wider(values_from = p, names_from = PCS8)

espaceTvl = PER_ff %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "categ_") %>%
  group_by(ZT_travMax) %>% summarise(across(starts_with("categ_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "n") %>%
  group_by(ZT_travMax) %>% mutate(p = n / sum(n), n = sum(n)) %>%
  pivot_wider(names_from = categ, values_from = p) %>%
  ungroup()

espaceTvl = left_join(espaceTvl, partsZTtvl, by="ZT_travMax")

g= espaceTvl %>%
  pivot_longer(cols = starts_with("travPCS_"), names_to = "PCS8", values_to = "p") %>%
  mutate(PCS8 = etqPCS8(substr(PCS8, 9,10))) %>%
  mutate(p = paliers(p*100, 5)) %>%
  group_by(p, PCS8) %>% summarise(across(starts_with("categ_"), mean, na.rm=T), n = n()) %>%
  filter(n>10) %>%
  pivot_longer(cols = starts_with("categ_"), names_to = "categ", values_to = "pMoy") %>%
  filter(categ != "categ_NA") %>%
  ggplot(aes(x = p, y = pMoy*100)) + geom_line(aes(color = categ)) + facet_wrap(~PCS8) +
  coord_cartesian(xlim = c(0, 100)) +
  xlab("part de la PCS dans le secteur (%)") + ylab("part du type de journée dans le secteur (%)") +
  scale_color_hue(labels = categLabs, name = "part du type\nde journée\npar rapport\naux travailleur⋅ses")
sortie("Typologies/Type journée selon type secteur de travail")
print(g)
off()

# à enlever quand sera résolu
# espace$ZT = ifelse(substr(espace$ZT, 1, 2) == "DP", paste0("EMP2019", espace$ZT), espace$ZT)
# espace$uid_ENQ = substr(espace$ZT, 1, 7)
# espaceFacto = analyseFacto(espace, colVar = paste0("categ_", c(1:6)), colUids = "ZT", colPoids = "n")


# (Archives) Courbe profils selon disCentre (1) ====

ZFs = centroAAVs ; remove(centroAAVs)

PER_ff_zt = left_join(PER_ff, ZFs, by="ZF")

PER_ff_zt = mutate(PER_ff_zt, disCentre = paliers(dis, palier = 1000))

# pal = c("thistle1", "aquamarine2", "skyblue", "olivedrab2", "orange", "tomato")

g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>50) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(disCentre) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = disCentre/1000, y = p * 100)) + geom_area(aes(fill = fI)) +
  xlab("distance au centre (km)") + ylab("part de chaque profil (%)") +
  labs(title = "Une structure qui reste sensible à l'éloignement par rapport aux centres",
       subtitle = "Structure de la répartition des profils de journée en fonction\nde la distance du domicile au centroïde de la ville-centre de l'AAV",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,60)) +
  scale_fill_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                    values = pal) 

sortie("Typologies/Profils selon disCentre")
print(g)
off()

sum(filter(PER_ff_zt, factoIndiv_v6f == "4" & disCentre > 40000 & modes_tc_rail == "oui")$CoeffRecEnq) /
  sum(filter(PER_ff_zt, factoIndiv_v6f == "4" & disCentre > 40000)$CoeffRecEnq)

sum(filter(PER_ff_zt, disCentre > 40000 & modes_tc_rail == "oui")$CoeffRecEnq) /
  sum(filter(PER_ff_zt, disCentre > 40000)$CoeffRecEnq)

sum(filter(PER_ff_zt, ZoneRang == "0" & modes_tc_rail == "oui")$CoeffRecEnq) /
  sum(filter(PER_ff_zt, ZoneRang == "0")$CoeffRecEnq)

sum(filter(PER_ff_zt, modes_tc_rail == "oui")$CoeffRecEnq) /
  sum(PER_ff_zt$CoeffRecEnq, na.rm=T)

# (Archives) Courbe profils selon disCentre (2) ====

if (!"ZFs" %in% ls()) { ZFs = centroidesAAV() }

PER_ff_zt = left_join(PER_ff, ZFs, by="ZF")

PER_ff_zt = mutate(PER_ff_zt, disCentre = paliers(dis, palier = 1000))

# pal = c("thistle2", "aquamarine2", "skyblue", "olivedrab2", "orange", "tomato")

g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>50) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(disCentre) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = disCentre/1000, y = p * 100)) + geom_area(aes(fill = fI)) +
  xlab("distance au centre (km)") + ylab("part de chaque profil (%)") +
  labs(title = "Une structure qui reste sensible à l'éloignement par rapport aux centres",
       subtitle = "Structure de la répartition des profils de journée en fonction\nde la distance du domicile au centroïde de la ville-centre de l'AAV",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,60)) +
  scale_fill_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                    values = pal) 

sortie("Typologies/Profils selon disCentre, v2")
print(g)
off()


# (Archives) Courbes profils selon disCentre (3) ====

# Julie Vallée dit qu'il ne faut pas superposer les courbes.
# Je pense qu'on peut voir des choses intéressantes en décomposant les profils.
# Let's do just that.

PER_ff_zt = left_join(PER_ff, ZFs, by="ZF")

PER_ff_zt = mutate(PER_ff_zt, disCentre = paliers(dis, palier = 2000))

# pal = c("thistle2", "aquamarine2", "skyblue", "olivedrab2", "orange", "tomato")

g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>50) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(disCentre) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = disCentre/1000, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("distance domicile-centre (km)") + ylab("part de chaque profil (%)") +
  labs(title = "Une structure qui reste sensible à l'éloignement par rapport aux centres",
       subtitle = "Structure de la répartition des profils de journée en fonction\nde la distance du domicile au centroïde de la ville-centre de l'AAV",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,60)) +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) 

sortie("Typologies/disCentre - ZT - profils")
print(g)
off()

# Distance moyenne
cbGen = PER_ff_zt %>% group_by(disCentre) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq))
g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = disCentre/1000, y = disMoy/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("distance domicile-centre (km)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,45)) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)")
g

# Heure début et fin
cbGen = PER_ff_zt %>% group_by(disCentre) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq))
g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = disCentre/1000, y = hFin)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("distance domicile-centre (km)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,45)) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi")
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% group_by(disCentre) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq))
g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = disCentre/1000, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("distance domicile-centre (km)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,45)) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)")
g

# Nombre de dep's
cbGen = PER_ff_zt %>% group_by(disCentre) %>%
  summarise(nDep = weighted.mean(N, w=CoeffRecEnq))
g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = disCentre/1000, y = nDep)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("distance domicile-centre (km)") + ylab("nb de déplacements") +
  labs(title = "Nombre de déplacements en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,45)) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements")
g

cbGen = PER_ff_zt %>% group_by(disCentre) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T))
g = PER_ff_zt %>% group_by(disCentre, factoIndiv_v6f) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = disCentre/1000, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("distance domicile-centre (km)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(disCentre)))) +
  coord_cartesian(xlim = c(0,45)) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements")
g

# (Archives) Approche densité par maille (ne pas utiliser) ====

# Une autre approche toute bête pour calculer la densité, oups...
# MAIS n'est pas satisfaisante à cause des découpages qui isolent artificiellement
# les centre-bourgs des campagnes
PER_ff_zt =
  left_join(PER_ff, shp_ZF, by=c("ZF" = "CODE_ZF")) %>%
  left_join(select(st_drop_geometry(pop), ZF_dans_rayon, pop), by=c("ZF" = "ZF_dans_rayon")) %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(densite = pop / (st_area(geometry)/10^6))

PER_ff_zt = PER_ff_zt %>%
  mutate(densite = as.double(densite)) %>%
  mutate(paliersLog = paliers(log(densite), .5)) %>%
  mutate(etiqLog = round(exp(paliersLog)))

shp_ZF %>%
  left_join(select(st_drop_geometry(pop), ZF_dans_rayon, pop), by=c("CODE_ZF" = "ZF_dans_rayon")) %>%
  mutate(densite = pop / (st_area(geometry)/10^6)) %>%
  mutate(densite = as.double(densite)) %>%
  mutate(paliersLog = paliers(log(densite), .5)) %>%
  mutate(etiqLog = round(exp(paliersLog))) %>%
  left_join(points, by = "CODE_ZF") %>%
  filter(uid_ENQ == "LOI2015") %>%
  ggplot() + geom_sf(aes(fill = etiqLog), colour = NA) +
  scale_fill_gradient(low = "green", high = "red", trans = "log")

g = PER_ff_zt %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>1000) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
  labs(title = "Densité de population et profils de journée",
       caption = src_fig(filter(PER_ff_zt, !is.na(densite)))) +
  scale_x_continuous(trans = "log", breaks = sort(unique(PER_ff_zt$etiqLog))) +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) 


# (Archives) Ancien calcul de la densité ====

# Calcul de densité à présent ? Comment faire ?
# Plusieurs approches :
# - un lissage de la densité de personnes et d'emplois ?
# - un décompte dans un rayon ?

tailleRayon = 10 # la taille du rayon en km²

# Petite expérience, je serais curieux de voir ce que ça donnerait en fonction
# de la population incluse dans un rayon bcp plus grand

rayon = sqrt(tailleRayon * 10^6 / pi)
# On peut diviser par 10 pour obtenir la densité.

# Tracer un cercle de ce rayon autour de chaque point de shp_ZF.
# Puis intersecter avec shp_ZT condensé ne contenant que la population.
# Procéder avec la population totale, c'est-à-dire PER.
# Pour éviter les incohérences, je vais faire le choix contestable de ne prendre
# en compte que les unités dont le centroïde sont dans le cercle.
# Il me semble que c'est passable puisque nos points de comparaison sont eux
# mêmes des points.

# Pour construire pop à partir de PER
pop = PER %>% group_by(ZF) %>% summarise(pop = sum(CoeffRecEnq, na.rm=T)) %>%
  left_join(st_centroid(shp_ZF), by = c("ZF" = "CODE_ZF")) %>% st_as_sf() %>%
  rename(ZF_dans_rayon = ZF) %>%
  select(ZF_dans_rayon, pop, geometry)
points = shp_ZF %>% st_centroid() %>% st_buffer(dist = rayon) %>%
  st_join(pop, st_intersects) %>%
  st_drop_geometry() %>%
  group_by(CODE_ZF) %>% summarise(pop = sum(pop), n = n()) %>%
  mutate(densite = pop/tailleRayon)

# Pour construire pop à partir du recensement, technique un peu plus propre
popRec = read_delim("Sources/base-ic-evol-struct-pop-2019.CSV", delim = ";") %>%
  select(IRIS, COM, P19_POP)
iris = st_read("Sources/Mailles/Contours IRIS/CONTOURS-IRIS.shp")
popRec = left_join(popRec, iris, by=c("IRIS" = "CODE_IRIS"))
popRec = st_as_sf(popRec) %>% st_centroid()

points = shp_ZF %>% st_centroid() %>% st_buffer(dist = rayon) %>%
  st_join(popRec, st_intersects) %>%
  st_drop_geometry() %>%
  group_by(CODE_ZF) %>% summarise(pop = sum(P19_POP), n = n()) %>%
  mutate(densite = pop/tailleRayon)

# On voudrait compléter en attribuant une densité approximative aux ZF à partir
# de leur code commune quand rien d'autre n'est disponible
ZF_sans_geom = PER %>% group_by(ZF) %>% summarise(COM = mode(Com)) %>%
  filter(!ZF %in% points$CODE_ZF)
coms = st_read("Sources/Mailles/communes-20210101.shp")
coms = coms %>% st_transform(crs = 2154) %>% st_centroid()
ZF_sans_geom = left_join(ZF_sans_geom, coms, by=c("COM" = "insee"))
points2 = ZF_sans_geom %>% rename(CODE_ZF = ZF) %>%
  st_as_sf() %>% st_buffer(dist = rayon) %>%
  st_join(popRec, st_intersects) %>%
  st_drop_geometry() %>%
  group_by(CODE_ZF) %>% summarise(pop = sum(P19_POP), n = n()) %>%
  mutate(densite = pop/tailleRayon)

points = rbind(points, points2)
remove(points2, coms, iris, popRec, pop)



# Cette mesure contestable est-elle réaliste ou faut-il raisonner à partir d'aires ?
shp_ZF %>%
  left_join(points, by = "CODE_ZF") %>%
  filter(uid_ENQ == "IDF2010") %>%
  st_centroid() %>%
  ggplot() + geom_sf(aes(colour = densite)) +
  scale_colour_gradient(low = "green", high = "red", trans = "log")

shp_ZF %>%
  left_join(points, by = "CODE_ZF") %>%
  filter(uid_ENQ == "LOI2015") %>%
  ggplot() + geom_sf(aes(fill = densite), colour = NA) +
  scale_fill_gradient(low = "green", high = "red", trans = "log")

shp_ZF %>%
  left_join(points, by = "CODE_ZF") %>%
  filter(uid_ENQ == "LYO2015") %>%
  ggplot() + geom_sf(aes(fill = densite), colour = NA) +
  scale_fill_gradient(low = "green", high = "red")

shp_ZF %>%
  left_join(points, by = "CODE_ZF") %>%
  filter(uid_ENQ == "LYO2015") %>%
  ggplot() + geom_sf(aes(fill = densite), colour = NA) +
  scale_fill_gradient(low = "green", high = "red", trans = "log")

ZF_sans_geom %>%
  left_join(points, by = c("ZF" = "CODE_ZF")) %>%
  filter(substr(ZF, 1, 7) == "ROA2012") %>% st_as_sf() %>%
  ggplot() + geom_sf(aes(colour = densite)) +
  scale_colour_gradient(low = "green", high = "red", trans = "log")


# (Archives) Courbes profils selon densité ====

# Abandonné le 2 nov 2023 puisque la densité est maintenant un facteur

# pal = c("thistle2", "aquamarine2", "skyblue", "olivedrab2", "orange", "tomato")

g = PER_ff_zt %>%
  ggplot(aes(x = etiqLog)) + geom_density(aes(colour = factoIndiv_v6f)) +
  scale_x_continuous(trans = "log", breaks=etiquettes) +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) 

gph_profils = function(base, titre="Densité de population et profils de journée")
{
  g = base %>% group_by(etiqLog, factoIndiv_v6f) %>%
    pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
    summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
    filter(nEntites>100) %>%
    pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
    group_by(etiqLog) %>% mutate(p = n / sum(n)) %>%
    filter(fI != "fIv6fNA") %>%
    ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
    xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
    labs(title = titre,
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_x_continuous(trans = "log", breaks=etiquettes) +
    scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                        values = pal) 
  return(g)
}

gph_profils(PER_ff_zt)

# Distance moyenne
#+
# geom_line(data=simulation, linetype = 2)

gph_disMoy = function(base, titre = "Distance parcourue en fonction de la densité")
{
  cbGen = base %>% group_by(etiqLog) %>%
    summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = disMoy/1000)) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    xlab("densité de population (hab/km²)") + ylab("distance moyenne (km)") +
    labs(title = titre,
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "portée de\nla mesure",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "distance (en km)") +
    scale_x_continuous(trans = "log", breaks=etiquettes)
  return(g)
}

gph_disMoy(PER_ff_zt)

cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
gDisMoyTvl = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = disMoy/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt_trav, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)") +
  scale_x_continuous(trans = "log", breaks=etiquettes)

cowplot::plot_grid(gDisMoy, gDisMoyTvl)

cor.test(log(PER_ff_zt$densite), log(PER_ff_zt$Dis))




# Diff entre distance et navette
PER_ff_zt = PER_ff_zt %>%
  mutate(navetteTheorique = Travail_Dis * 2,
         navetteThDiff = Dis - navetteTheorique,
         navetteThPctg = ((Dis/navetteTheorique)-1)*100)

cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(navetteThDiff = weighted.mean(navetteThDiff, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)


g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(navetteThDiff = weighted.mean(navetteThDiff, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = navetteThDiff/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("différence (km)") +
  labs(title = "Différence entre la navette théorique et la navette attendue\nselon la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "différence (en km)") +
  scale_x_continuous(trans = "log", breaks = etiquettes) 

g

cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(navetteThPctg = weighted.mean(navetteThPctg, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(navetteThPctg = weighted.mean(navetteThPctg, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = navetteThPctg)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("écart moyen entre la distance dom/trav et la distance parcourue (%)") +
  labs(title = "Différence entre la navette théorique et la navette attendue\nselon la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = NULL) +
  scale_x_continuous(trans = "log", breaks = etiquettes)
g



# Temps en déplacement
gph_tpsMoy = function(base)
{
  cbGen = base %>% group_by(etiqLog) %>%
    summarise(tpsMoy = weighted.mean(Tps, w=CoeffRecEnq), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(tpsMoy = weighted.mean(Tps, w = CoeffRecEnq), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = tpsMoy)) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("temps en déplacement (min)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "portée de la\nmesure") +
    scale_x_continuous(trans = "log", breaks = etiquettes)
  return(g)
}

gph_tpsMoy(PER_ff_zt)

# Vélocité du déplacement
cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = vitMoy)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("rapport km parcours / temps en déplacement") +
  labs(title = "Vélocité du déplacement selon la densité de la zone de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "vitesse (en km/h)") +
  scale_x_continuous(trans = "log")
g

# Vélocité du déplacement selon mode ; c'est planté de toute évidence
cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(!is.na(typoModes)) %>% group_by(etiqLog, typoModes) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = vitMoy)) +
  geom_line(aes(colour = typoModes, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("rapport km parcours / temps en déplacement") +
  labs(title = "Vélocité du déplacement selon la densité de la zone de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  #scale_colour_manual(name = "profil de modes",
  # breaks = c(levels(PER_ff_zt$typoModes)[2:8], "tous profils"),
  #  values = c(pal22_typoModes[2:8], "black")) +
  scale_size_manual(values = c(2, .75), name = "vitesse (en km/h)") +
  scale_x_continuous(trans = "log")
g



# Heure début et fin
cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w=CoeffRecEnq),
            nEntites = n()) %>%
  filter(nEntites>200) %>%
  pivot_longer(cols = c("hFin", "hDeb"), names_to = "heure", values_to = "h")
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq),
            nEntités = n()) %>%
  filter(nEntités>50) %>%
  pivot_longer(cols = c("hFin", "hDeb"), names_to = "heure", values_to = "h") %>%
  ggplot(aes(x = etiqLog, y = h)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil", linetype=heure)) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne", linetype=heure), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log")
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log")
g

# Nombre de dep's
gph_nDep = function(base)
{
  cbGen = base %>% group_by(etiqLog) %>%
    summarise(nDep = weighted.mean(N, w=CoeffRecEnq), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = nDep)) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "déplacements") +
    scale_x_continuous(trans = "log", breaks=etiquettes)
  return(g)
}
gph_nDep(PER_ff_zt)


cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log")
g

cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pTC*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation des TC") +
  labs(title = "Utilisation des TC en fonction de la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log")
g

# Variante par genre

g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(Genre, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
  labs(title = "Densité de population et profils de journée",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log") +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) +
  facet_wrap(~Genre)

# Distance moyenne
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f, Genre) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = disMoy/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)") +
  scale_x_continuous(trans = "log") +
  facet_wrap(~Genre)
g

# Heure début et fin
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = hFin)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Nombre de dep's
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(nDep = weighted.mean(N, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = nDep)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Nombre de déplacements en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f, Genre) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pTC*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation des TC") +
  labs(title = "Utilisation des TC en fonction de la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Variante par PCS

g = PER_ff_zt %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  filter(PCS8 != "02") %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(PCS8, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
  labs(title = "Densité de population et profils de journée",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log") +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) +
  facet_wrap(~PCS8)
g



gGen = PER_ff_zt %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecEnq, names_prefix = "PCSn") %>%
  group_by(etiqLog) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(PCS != "PCSnNA")

g1 = PER_ff_zt %>% 
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecEnq, names_prefix = "PCSn") %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(factoIndiv_v6f, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(PCS != "PCSnNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = PCS)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque PCS (%)") +
  labs(title = "Part de chaque PCS dans les profils\nen fonction de la densité autour du lieu de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log", breaks=etiquettes) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle", labels = niv_PCS8[2:6],
                      values = pal_PCS8[2:6]) +
  facet_wrap(~factoIndiv_v6f)

g2 = PER_ff_zt %>% 
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  pivot_wider(names_from = PCS8, values_from = CoeffRecEnq, names_prefix = "PCSn") %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(across(starts_with("PCSn"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("PCSn"), names_to = "PCS", values_to = "n") %>%
  group_by(factoIndiv_v6f, etiqLog) %>% mutate(p = n / sum(n)) %>%
  left_join(gGen, by=c("etiqLog", "PCS"), suffix=c("", ".gen")) %>%
  mutate(surrep = p/p.gen) %>%
  filter(PCS != "PCSnNA") %>%
  ggplot(aes(x = etiqLog, y = (surrep-1) * 100)) + geom_line(aes(colour = PCS)) +
  xlab("densité de population (hab/km²)") + ylab("surreprésentation de la PCS\npar rapport à la moyenne nationale") +
  labs(title = "Surreprésentation de chaque PCS dans les profils\nen fonction de la densité autour du lieu de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log", breaks=etiquettes) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle", labels = niv_PCS8[2:6],
                      values = pal_PCS8[2:6]) +
  facet_wrap(~factoIndiv_v6f) +
  geom_hline(yintercept = 0, colour = "black", alpha=.5) +
  scale_y_continuous(trans = trans_sur100, breaks=transf_echelle_sur100_inverse(c(-3:3)/2),
                     labels=transf_echelle_sur100_lab(transf_echelle_sur100_inverse(c(-3:3)/2)))

cadre = cowplot::plot_grid(g1 + theme(legend.position = "none") + labs(caption = NULL),
                           g2 + theme(legend.position = "none") + labs(caption = NULL),
                           cowplot::get_legend(g1 + theme(legend.position = "bottom") +
                                                 guides(colour = guide_legend(nrow = 2))),
                           nrow=3, rel_heights = c(5,5,1), align="v", axis="lr")
cadre = viz_Pied(cadre, src_fig(PER_ff_zt))

sortie("Typologies/Composition PCS par profil", taille = "carré")
print(cadre)
off()


# Distance moyenne
cbGen = PER_ff_zt %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>50)
gDisMoyPcs = PER_ff_zt %>% 
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  left_join(rename(cbGen, disMoyMoy = disMoy), by=c("etiqLog", "factoIndiv_v6f")) %>%
  mutate(surrepDis = ((disMoy / disMoyMoy)-1)*100) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = surrepDis)) +
  geom_hline(yintercept=0, colour="black", alpha=.5) +
  geom_line(aes(colour = PCS8)) +
  xlab("densité de population (hab/km²)") + ylab("écart à la moyenne (%)") +
  labs(title = "Écart à la moyenne des distances parcourues\nselon la PCS et la densité mesurée\nautour du lieu de domicile",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6]),
                      values = c(pal_PCS8[2:6])) +
  scale_x_continuous(trans = "log", breaks = etiquettes) +
  scale_y_continuous(trans = trans_sur100) +
  facet_wrap(~factoIndiv_v6f, ncol=1)
gDisMoyPcs

g = PER_ff_zt %>% 
  mutate(Genre = plyr::revalue(Genre, c("F"="femmes", "H"="hommes"))) %>%
  group_by(etiqLog, PCS8, Genre) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = disMoy/1000)) +
  geom_line(aes(colour = PCS8)) +
  xlab("densité de population (hab/km²)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6]),
                      values = c(pal_PCS8[2:6])) +
  scale_x_continuous(trans = "log", breaks = etiquettes) + facet_wrap(~Genre)

g



# Durée moyenne
cbGen = PER_ff_zt %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(duMoy = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>50)
gDutMoyPcs = PER_ff_zt %>% 
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], "2" = categLabs[2],
                                          "3" = categLabs[3], "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(duMoy = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duMoy/60)) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  geom_line(aes(colour = PCS8, size="par profil")) +
  xlab("densité de population (hab/km²)") + ylab("temps passé au lieu d'emploi (h)") +
  labs(title = "Temps passé sur le lieu d'emploi\nselon la PCS et la densité mesurée\nautour du lieu de domicile",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6], "tous profils"),
                      values = c(pal_PCS8[2:6], "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)") +
  scale_x_continuous(trans = "log", breaks = etiquettes) +
  facet_wrap(~factoIndiv_v6f, nrow=6)





cbGen = PER_ff_zt_trav %>%
  mutate(Genre = plyr::revalue(Genre, c("F"="femmes", "H"="hommes"))) %>%
  group_by(etiqLog, Genre) %>% 
  summarise(duMoy = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100)
g = PER_ff_zt_trav %>% 
  mutate(Genre = plyr::revalue(Genre, c("F"="femmes", "H"="hommes"))) %>%
  group_by(etiqLog, Genre, PCS8) %>%
  summarise(duMoy = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = duMoy/60)) +
  geom_line(data = cbGen, aes(colour = "toutes PCS", size="toutes PCS"), alpha=.5) +
  geom_line(aes(colour = PCS8, size = "par PCS", group=PCS8)) +
  xlab("densité de population (hab/km²)") + ylab("temps passé sur lieu d'emploi") +
  labs(title = "Temps passé sur le lieu d'emploi par PCS et selon la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "catégorie\nsocioprofessionnelle",
                      labels = c(niv_PCS8[2:6], "toutes PCS"),
                      values = c(pal_PCS8[2:6], "black")) +
  scale_x_discrete(breaks = etiquettes) +
  scale_size_manual(values = c(.75, 2), name = "mesure") +
  facet_wrap(~Genre)
g

weighted.mean(filter(PER_ff, Genre == "H")$DuTvl, filter(PER_ff, Genre == "H")$CoeffRecEnq)/60
weighted.mean(filter(PER_ff, Genre == "F")$DuTvl, filter(PER_ff, Genre == "F")$CoeffRecEnq)/60

# Heure début et fin
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = hFin)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

# Nombre de dep's
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(nDep = weighted.mean(N, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = nDep)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Nombre de déplacements en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

cbGen = PER_ff_zt %>%  filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>%  filter(PCS8 != "02") %>% group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

cbGen = PER_ff_zt %>% group_by(etiqLog, PCS8) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pTC*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation des TC") +
  labs(title = "Utilisation des TC en fonction de la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g


# (Archives) Courbes profils selon densité du lieu de travail ====

# Abandonné puisque la densité est maintenant un facteur

# mesure de corrélation
PER_ff_zt_trav_ntm = PER_ff_zt_trav %>%
  filter(Dis>0) %>% filter(!is.na(densite)) %>% filter(!is.infinite(log(densite))) %>%
  filter(!is.infinite(log(Dis)))
cor.test(PER_ff_zt_trav_ntm$densite, PER_ff_zt_trav_ntm$Dis) ; remove(PER_ff_zt_trav_ntm)

densites = log(filter(PER_ff_zt_trav, !is.na(densite) & Dis > 0 & !is.na(Dis))$densite)
distances = log(filter(PER_ff_zt_trav, !is.na(densite) & Dis > 0 & !is.na(Dis))$Dis)

cor.test(densites, distances)


cor.test(log(PER_ff_zt$densite), log(PER_ff_zt$Tps))
cor.test(log(filter(PER_ff_zt_trav, !is.infinite(log(densite)) & !is.na(densite) & !is.infinite(log(Tps)) & !is.na(Tps))$densite), log(filter(PER_ff_zt_trav,  !is.infinite(log(densite)) & !is.na(densite) & !is.infinite(log(Tps) & !is.na(Tps)))$Tps))


# Vélocité du déplacement
cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = vitMoy)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("rapport km parcours / temps en déplacement") +
  labs(title = "Vélocité du déplacement selon la densité de la zone d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "vitesse (en km/h)") +
  scale_x_continuous(trans = "log")
g

# Vélocité du déplacement selon mode
cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(!is.na(typoModes)) %>% group_by(etiqLog, typoModes) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = vitMoy)) +
  geom_line(aes(colour = typoModes, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("rapport km parcours / temps en déplacement") +
  labs(title = "Vélocité du déplacement selon la densité de la zone d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de modes",
                      breaks = c(levels(PER_ff_zt$typoModes), "tous profils"),
                      values = c(pal22_typoModes, "black")) +
  scale_size_manual(values = c(2, .75), name = "vitesse (en km/h)") +
  scale_x_continuous(trans = "log")
g

# Heure début et fin

cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w=CoeffRecEnq), nEntites = n()) %>%
  pivot_longer(cols = c(hDeb, hFin), names_to = "hLaquelle", values_to = "h") %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq), nEntités = n()) %>%
  pivot_longer(cols = c(hDeb, hFin), names_to = "hLaquelle", values_to = "h") %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = h)) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne", linetype=hLaquelle), alpha=.5) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil", linetype=hLaquelle)) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Horaires de travail en fonction\nde la densité autour du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt_trav, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "portée de la\nmesure") +
  scale_linetype(name = "heure", labels=c("d'arrivée au\nlieu de travail", "de départ du\nlieu de travail")) +
  scale_x_continuous(trans = "log", breaks=etiquettes) +
  scale_y_continuous(breaks=c(3:10)*2)
sortie("Typologies/Horaires selon profils et densité")
print(g)
off()


cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(hDeb = weighted.mean(JoTvDeb, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = hDeb)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heure de début de journée en fonction de la densité du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt_trav, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log", breaks=etiquettes)
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la densité du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log")
g

# Nombre de dep's
gph_nDep(PER_ff_zt_trav)

weighted.mean(PER_ff$N, PER_ff$CoeffRecEnq)

cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(x = weighted.mean(DuCtt, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(x = weighted.mean(DuCtt, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = x)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Temps consacré à des activités contraintes en fonction de la densité du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log")
g

cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(x = weighted.mean(DuLsr, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(x = weighted.mean(DuLsr, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = x)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Temps consacré à des activités de loisirs en fonction de la densité du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log")
g

gph_modeVoit = function(base)
{
  cbGen = base %>% group_by(etiqLog) %>%
    summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = base %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = pVoit*100)) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation de la voiture (%)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "déplacements") +
    scale_x_continuous(trans = "log", breaks=etiquettes)
  return(g)
}

gph_modeTC = function(base)
{
  cbGen = PER_ff_zt %>% group_by(etiqLog) %>%
    summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
    filter(nEntités>50) %>%
    ggplot(aes(x = etiqLog, y = pTC*100)) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation des TC (%)") +
    labs(title = "Utilisation des TC en fonction de la densité au secteur d'emploi",
         caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "déplacements") +
    scale_x_continuous(trans = "log", breaks=etiquettes)
  return(g)
}



# Variante par genre

g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(Genre, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
  labs(title = "Densité de population (lieu d'emploi) et profils de journée",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log") +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) +
  facet_wrap(~Genre)

# Distance moyenne
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, factoIndiv_v6f, Genre) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = disMoy/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)") +
  scale_x_continuous(trans = "log") +
  facet_wrap(~Genre)
g

# Heure début et fin
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = hFin)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Nombre de dep's
cbGen = PER_ff_zt %>% group_by(etiqLog, Genre) %>%
  summarise(nDep = weighted.mean(N, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, Genre, factoIndiv_v6f) %>%
  summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = nDep)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Nombre de déplacements en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~Genre)
g

# Variante par PCS

g = PER_ff_zt %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  filter(PCS8 != "02") %>%
  pivot_wider(names_from = factoIndiv_v6f, values_from = CoeffRecEnq, names_prefix = "fIv6f") %>%
  summarise(across(starts_with("fIv6f"), sum, na.rm=T), nEntites = n()) %>%
  filter(nEntites>100) %>%
  pivot_longer(cols = starts_with("fIv6f"), names_to = "fI", values_to = "n") %>%
  group_by(PCS8, etiqLog) %>% mutate(p = n / sum(n)) %>%
  filter(fI != "fIv6fNA") %>%
  ggplot(aes(x = etiqLog, y = p * 100)) + geom_line(aes(colour = fI)) +
  xlab("densité de population (hab/km²)") + ylab("part de chaque profil (%)") +
  labs(title = "Densité de population et profils de journée",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_x_continuous(trans = "log") +
  scale_colour_manual(name = "part de chaque\nprofil de journées (%)", labels = categLabs,
                      values = pal) +
  facet_wrap(~PCS8)

# Distance moyenne
cbGen = PER_ff_zt %>%   filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>%   filter(PCS8 != "02") %>%
  group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(disMoy = weighted.mean(Dis, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = disMoy/1000)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("distance parcourue en moyenne (km)") +
  labs(title = "Augmentation des distances à mesure qu'on s'éloigne des centres",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "distance (en km)") +
  scale_x_continuous(trans = "log") +
  facet_wrap(~PCS8)
g

# Heure début et fin
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = hFin)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la densité du secteur d'emploi",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "heure de dernier\ndépart du lieu\nd'emploi") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

# Heure début et fin, courbes par PCS
cbGen = PER_ff_zt_trav %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], 
                                          "2" = gsub("-", "\n-", categLabs[2]),
                                          "3" = categLabs[3],
                                          "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w=CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200) %>%
  pivot_longer(cols = c(hFin, hDeb), values_to = "h", names_to = "hLaquelle")
g = PER_ff_zt_trav %>%
  mutate(factoIndiv_v6f = plyr::revalue(factoIndiv_v6f,
                                        c("1" = categLabs[1], 
                                          "2" = gsub("-", "\n-", categLabs[2]),
                                          "3" = categLabs[3],
                                          "4" = categLabs[4],
                                          "5" = categLabs[5], "6" = categLabs[6]))) %>%
  group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(hFin = weighted.mean(JoTvFin, w = CoeffRecEnq),
            hDeb = weighted.mean(JoTvDeb, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  pivot_longer(cols = c(hFin, hDeb), values_to = "h", names_to = "hLaquelle") %>%
  ggplot(aes(x = etiqLog, y = h)) +
  geom_line(aes(colour = PCS8, size="par profil", linetype = hLaquelle)) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne", linetype = hLaquelle), alpha=.4) +
  xlab("densité de population (hab/km²)") + ylab("heure de départ du lieu de travail") +
  labs(title = "Heures de travail en fonction de la PCS et du type de journée\net de la densité mesurée autour du lieu de travail",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(niv_PCS8[2:6], "tous profils"),
                      values = c(pal_PCS8[2:6], "black")) +
  scale_size_manual(values = c(2, .75), name = "portée de la courbe") +
  scale_linetype(name = "heure", labels=c("d'arrivée sur\nle lieu de travail",
                                          "de départ du\nlieu de travail")) +
  scale_x_discrete(breaks=etiquettes) + 
  scale_y_continuous(breaks=c(6:21)) +
  facet_wrap(~factoIndiv_v6f, nrow=1) +
  coord_cartesian(xlim = c(64, 16384)) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

sortie("Typologies/Heures de travail par PCS")
print(g)
off()




# Temps passé lieu d'emploi
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(duTvl = weighted.mean(DuTvl, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(duTvl = weighted.mean(DuTvl, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = duTvl/60)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("durée présence sur lieu d'emploi (h)") +
  labs(title = "Durée présence au lieu d'emploi en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "durée présence (h)") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

# Nombre de dep's
cbGen = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(nDep = weighted.mean(N, w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% filter(PCS8 != "02") %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(nDep = weighted.mean(N, w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = nDep)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("nb de déplacements") +
  labs(title = "Nombre de déplacements en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

cbGen = PER_ff_zt %>%  filter(PCS8 != "02") %>% group_by(etiqLog, PCS8) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>%  filter(PCS8 != "02") %>% group_by(etiqLog, factoIndiv_v6f, PCS8) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

cbGen = PER_ff_zt %>% group_by(etiqLog, PCS8) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt %>% group_by(etiqLog, PCS8, factoIndiv_v6f) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pTC*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation des TC") +
  labs(title = "Utilisation des TC en fonction de la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log") + facet_wrap(~PCS8)
g

# Rapport entre les deux
comparaison = PER_ff_zt_trav %>%
  select(uid_PER, factoIndiv_v6f, Dis, Tps, Genre, PCS8, densite, CoeffRecEnq) %>%
  rename(densiteTrav = densite) %>%
  left_join(select(PER_ff_zt, uid_PER, densite), by="uid_PER") %>%
  mutate(rapDomTrav = densiteTrav/densite,
         rapDomTravLog = log(rapDomTrav))

# Quantile(comparaison$rapDomTrav, w=comparaison$CoeffRecEnq, probs=c(.05, .95), na.rm=T)

# Quantile(filter(comparaison, !is.infinite(rapDomTravLog))$rapDomTravLog,
# w=filter(comparaison, !is.infinite(rapDomTravLog))$CoeffRecEnq,
# probs=c(.05, .95), na.rm=T)

filter(comparaison, rapDomTrav >= -20 & rapDomTrav <=20) %>%
  ggplot(aes(x = rapDomTrav)) + geom_density()

ggplot(data = comparaison, aes(x = rapDomTravLog)) + geom_density()

# Déjà, caracs sociales ?
comparaison = filter(comparaison, !is.na(rapDomTrav), !is.infinite(rapDomTrav))

weighted.median(comparaison$rapDomTrav, w=comparaison$CoeffRecEnq)
# Quantile(comparaison$rapDomTrav, w=comparaison$CoeffRecEnq, probs=c(.25, .75), na.rm=T)

weighted.median(filter(comparaison, Genre == "F")$rapDomTrav, w=filter(comparaison, Genre == "F")$CoeffRecEnq)
weighted.median(filter(comparaison, Genre == "H")$rapDomTrav, w=filter(comparaison, Genre == "H")$CoeffRecEnq)

weighted.median(filter(comparaison, PCS8 == "03")$rapDomTrav, w=filter(comparaison, PCS8 == "03")$CoeffRecEnq)
weighted.median(filter(comparaison, PCS8 == "04")$rapDomTrav, w=filter(comparaison, PCS8 == "04")$CoeffRecEnq)
weighted.median(filter(comparaison, PCS8 == "05")$rapDomTrav, w=filter(comparaison, PCS8 == "05")$CoeffRecEnq)
weighted.median(filter(comparaison, PCS8 == "06")$rapDomTrav, w=filter(comparaison, PCS8 == "06")$CoeffRecEnq)

ggplot(data = comparaison, aes(x = rapDomTravLog)) + geom_density(aes(color = Genre))
ggplot(data = comparaison, aes(x = rapDomTravLog)) + geom_density(aes(color = PCS8))

filter(comparaison, rapDomTrav >= -20 & rapDomTrav <=20) %>%
  mutate(rapDomTrav = paliers(rapDomTrav, palier = 1)) %>%
  group_by(rapDomTrav) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq) / 1000) %>%
  ggplot(aes(x = rapDomTrav, y = disMoy)) + geom_line()

filter(comparaison, rapDomTrav >= -20 & rapDomTrav <=20) %>%
  mutate(rapDomTrav = paliers(rapDomTrav, palier = 1)) %>%
  group_by(rapDomTrav) %>% summarise(disMed = weighted.median(Dis, w=CoeffRecEnq) / 1000) %>%
  ggplot(aes(x = rapDomTrav, y = disMed)) + geom_line()

filter(comparaison, rapDomTrav >= -20 & rapDomTrav <=20) %>%
  mutate(rapDomTrav = paliers(rapDomTrav, palier = 1)) %>%
  group_by(PCS8, rapDomTrav) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq) / 1000, n = n()) %>%
  filter(n>200) %>%
  ggplot(aes(x = rapDomTrav, y = disMoy)) + geom_line(aes(colour = PCS8))

filter(comparaison) %>%
  mutate(rapDomTrav = paliers(rapDomTrav, palier = 1)) %>%
  group_by(PCS8, rapDomTrav) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq) / 1000, n = n()) %>%
  filter(n>200) %>%
  ggplot(aes(x = rapDomTrav, y = disMoy)) + geom_line(aes(colour = PCS8))

compPaliers = comparaison %>%
  mutate(rapDomTravLog = paliers(rapDomTravLog, palier = log(2))) %>%
  filter(!is.infinite(rapDomTravLog))

bks = sort(unique(compPaliers$rapDomTravLog))
etq = paste0("x ", exp(bks))

compPaliers %>%
  group_by(PCS8, rapDomTravLog) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq) / 1000, n = n()) %>%
  filter(n>100) %>%
  ggplot(aes(x = rapDomTravLog, y = disMoy)) + geom_line(aes(colour = PCS8)) +
  scale_x_continuous(breaks = bks, labels=etq)

compPaliers %>%
  group_by(factoIndiv_v6f, rapDomTravLog) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq) / 1000, n = n()) %>%
  filter(n>100) %>%
  ggplot(aes(x = rapDomTravLog, y = disMoy)) + geom_line(aes(colour = factoIndiv_v6f)) +
  scale_x_continuous(breaks = bks, labels=etq)

compPaliers %>%
  group_by(PCS8, rapDomTravLog) %>% summarise(tpsMoy = weighted.mean(Tps, w=CoeffRecEnq) / 60, n = n()) %>%
  filter(n>100) %>%
  ggplot(aes(x = rapDomTravLog, y = tpsMoy)) + geom_line(aes(colour = PCS8)) +
  scale_x_continuous(breaks = bks, labels=etq)

mod = comparaison %>%
  filter(!is.na(Dis), !is.infinite(log(Dis)), !is.na(rapDomTravLog), !is.infinite(rapDomTravLog)) %>%
  lm(formula = "log(Dis) ~ abs(rapDomTravLog)")

mod = comparaison %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  filter(!is.na(Dis), !is.infinite(log(Dis)), !is.na(rapDomTravLog), !is.infinite(rapDomTravLog)) %>%
  lm(formula = "log(Dis) ~ abs(rapDomTravLog) + Genre * PCS8")
exp(mod$coefficients)


# Mais bon. Est-ce que c'est pas un peu tautologique finalement. Deux lieux proches ne pouvant avoir des densités
# proches. Du coup, tout ça ne vaut pas grand chose.






cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(pVoit = sum(ifelse(modes_voiture == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pVoit*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation de la voiture") +
  labs(title = "Utilisation de la voiture en fonction de la distance au centre",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log", breaks=etiquettes)
g

cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
  filter(nEntites>200)
g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(pTC = sum(ifelse(modes_tc == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
  filter(nEntités>50) %>%
  ggplot(aes(x = etiqLog, y = pTC*100)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("part d'utilisation des TC") +
  labs(title = "Utilisation des TC en fonction de la densité",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "déplacements") +
  scale_x_continuous(trans = "log", breaks=etiquettes)
g

gph_modeDoux = function(base)
{
  cbGen = PER_ff_zt_trav %>% group_by(etiqLog) %>%
    summarise(pTC = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntites = n()) %>%
    filter(nEntites>200)
  g = PER_ff_zt_trav %>% group_by(etiqLog, factoIndiv_v6f) %>%
    summarise(pTC = sum(ifelse(modes_marche == "oui" | modes_vélo == "oui", CoeffRecEnq, 0), na.rm=T)/sum(CoeffRecEnq, na.rm=T), nEntités = n()) %>%
    filter(nEntités>100) %>%
    ggplot(aes(x = etiqLog, y = pTC*100)) +
    geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
    geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
    xlab("densité de population (hab/km²)") + ylab("utilisation marche/vélo (%)") +
    labs(caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
    scale_colour_manual(name = "profil de journée",
                        labels = c(categLabs, "tous profils"),
                        values = c(pal, "black")) +
    scale_size_manual(values = c(2, .75), name = "déplacements") +
    scale_x_continuous(trans = "log", breaks=etiquettes)
  return(g)
}


# Vélocité du déplacement
cbGen = PER_ff_zt_trav %>% filter(modes_voiture == "oui") %>% group_by(etiqLog) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w=CoeffRecEnq), nEntites = n()) %>%
  filter(nEntites>300)
g = PER_ff_zt_trav %>% filter(modes_voiture == "oui") %>% group_by(etiqLog, factoIndiv_v6f) %>%
  summarise(vitMoy = weighted.mean((Dis/1000)/(Tps/60), w = CoeffRecEnq), nEntités = n()) %>%
  filter(nEntités>100) %>%
  ggplot(aes(x = etiqLog, y = vitMoy)) +
  geom_line(aes(colour = factoIndiv_v6f, size="par profil")) +
  geom_line(data = cbGen, aes(colour = "tous profils", size = "en moyenne"), alpha=.6) +
  xlab("densité de population (hab/km²)") + ylab("rapport km parcours / temps en déplacement") +
  labs(title = "Vélocité du déplacement selon la densité de la zone de résidence",
       caption = src_fig(filter(PER_ff_zt, !is.na(etiqLog)))) +
  scale_colour_manual(name = "profil de journée",
                      labels = c(categLabs, "tous profils"),
                      values = c(pal, "black")) +
  scale_size_manual(values = c(2, .75), name = "vitesse (en km/h)") +
  scale_x_continuous(trans = "log", breaks=etiquettes)
g