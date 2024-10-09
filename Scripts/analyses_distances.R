# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                                                                 #
#     E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                                   #
#                                                                                                 #
#     SCRIPTS DE TRAVAIL M. GUINEPAIN                                                             #
#     ANALYSES POUR LE CHAPITRE 3 : LA VALEUR SOCIALE DES KILOMÈTRES (THÈSE)                      #
#                                                                                                 #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# setwd("/home/maxime/Données/EMD")
# Pour exécuter ce script lui-même : > source("Scripts/analyses_distances.R", print.eval=T)

rm(list = ls()) ; gc()
source("START.R", print.eval=T)

initMémoire(BasesCharger = c("PER"))

# Répertoire des figures : "Distances"
if (!dir.exists("Sorties/Distances")) { dir.create("Sorties/Distances") }

# Base des AAVs nécessaire pour distances/disCentre et atlas
aavs = read_sf("Sources/Fond Carte/zMetro.shp")
aavs = rbind(aavs, read_sf("Sources/Fond Carte/zDOM.shp"))
aavs = st_transform(aavs, crs = 2154)
aavs = aavs %>% filter(AAV20 != "000") %>% group_by(AAV20) %>%
  summarise(LIBAAV2 = first(LIBAAV2)) %>%
  rename(LIBAAV2020 = LIBAAV2)

PER_ff = init_PER_ff(PER)

PER_f = PER %>%
  filter(DuTvl > 0,
         PCS8 %in% c("01", "02", "03", "04", "05", "06"),
         Activ %in% c("10", "11"),
         Age < 70,
         !is.na(CoeffRecEnq))

load("Data/MEN.rds")
PER_ff = left_join(PER_ff, select(MEN, uid_MEN, MenBebe), by = "uid_MEN")
remove(MEN)

load("Data/ACT.rds")

# Généralités ===================================================================

# Corr temps passé en dep / distance (non pondéré)
corr = filter(PER_ff, !is.na(Tps) & !is.na(Dis))
cor.test(corr$Dis, corr$Tps)

# Moyenne et médiane
weighted.mean(PER_f$Dis, PER_f$CoeffRecEnq, na.rm=T)
weighted.mean(PER_f$Tps, PER_f$CoeffRecEnq, na.rm=T)
weighted.median(PER_f$Dis, PER_f$CoeffRecEnq, na.rm=T)
weighted.median(PER_f$Tps, PER_f$CoeffRecEnq, na.rm=T)

# Écart type
weighted.sd(PER_f$Dis, PER_f$CoeffRecEnq, na.rm=T)
weighted.sd(PER_f$Dis, PER_f$CoeffRecEnq, na.rm=T) / weighted.mean(PER_f$Dis, PER_f$CoeffRecEnq, na.rm=T)
weighted.sd(PER_f$Tps, PER_f$CoeffRecEnq, na.rm=T) / weighted.mean(PER_f$Tps, PER_f$CoeffRecEnq, na.rm=T)

# Écart type par enquête
PER_f |>
  group_by(uid_ENQ) |>
  summarise(sd = weighted.sd(Dis, CoeffRecEnq, na.rm=T)) |>
  tab_Tri(parCol = "sd", rev = T) |>
  left_join(z_Nomenclature, by = "uid_ENQ")

# Part de deps pour trav
DEP |>
  left_join(select(PER, uid_PER, CoeffRecEnq), by="uid_PER") |>
  mutate(pourTravail = substr(D_Motif, 1, 1) == "1" | substr(O_Motif, 1, 1) == "1") |>
  group_by(pourTravail) |> summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n) * 100)
DEP |> filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER, uid_PER, CoeffRecEnq), by="uid_PER") |>
  mutate(pourTravail = substr(D_Motif, 1, 1) == "1" | substr(O_Motif, 1, 1) == "1") |>
  group_by(pourTravail) |> summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n) * 100)

# Journées avec dep pour trav
PER |>
  mutate(pourTravail = DuTvl > 0) |>
  group_by(pourTravail) |> summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n)*100)
PER |> filter(Age >= 16, Age <= 65) |>
  mutate(pourTravail = DuTvl > 0) |>
  group_by(pourTravail) |> summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n)*100)
PER |> filter(Activ %in% c("10", "11")) |>
  mutate(pourTravail = DuTvl > 0) |>
  group_by(pourTravail) |> summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n)*100)

# Distance Travail_DIS
weighted.median(PER_f$Travail_Dis, PER_f$CoeffRecEnq, na.rm=T)
weighted.mean(PER_f$Travail_Dis, PER_f$CoeffRecEnq, na.rm=T)

weighted.median(PER_f$Dis.V, PER_f$CoeffRecEnq, na.rm=T)
weighted.mean(PER_f$Dis.V, PER_f$CoeffRecEnq, na.rm=T)

# Selon navette
PER_ff |>
  group_by(schAct) |>
  summarise(n = sum(CoeffRecEnq, na.rm=T), nb = n()) |>
  mutate(p = n / sum(n) * 100) |>
  tab_Tri(parCol = "p", rev = T)


# Tableau distances médianes selon classe de densité
weighted.quantile(PER_ff$dsDom, PER_ff$CoeffRecEnqSansEMP, probs = c(0, .25, .5, .75, 1))
weighted.quantile(PER_ff$dsTvl, PER_ff$CoeffRecEnqSansEMP, probs = c(0, .25, .5, .75, 1))

PER_ff |> mutate(ds4dom = case_when(dsDom < 400 ~ "4",
                                   dsDom < 1200 ~ "3",
                                   dsDom < 4000 ~ "2",
                                   dsDom >= 4000 ~ "1"),
                ds4tvl = case_when(dsTvl < 400 ~ "4",
                                   dsTvl < 1200 ~ "3",
                                   dsTvl < 4000 ~ "2",
                                   dsTvl >= 4000 ~ "1")) |>
  group_by(ds4dom, ds4tvl) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP)) |>
  filter(!is.na(ds4tvl))

PER_ff |> mutate(ds4dom = case_when(dsDom < 400 ~ "4",
                                    dsDom < 1200 ~ "3",
                                    dsDom < 4000 ~ "2",
                                    dsDom >= 4000 ~ "1")) |>
  group_by(ds4dom) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP))

PER_ff |> mutate(ds4tvl = case_when(dsTvl < 400 ~ "4",
                                    dsTvl < 1200 ~ "3",
                                    dsTvl < 4000 ~ "2",
                                    dsTvl >= 4000 ~ "1")) |>
  group_by(ds4tvl) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP))

PER_ff |>   summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP))


PER_ff |> mutate(ds4dom = case_when(dsDom < 400 ~ "4",
                                    dsDom < 1200 ~ "3",
                                    dsDom < 4000 ~ "2",
                                    dsDom >= 4000 ~ "1"),
                 ds4tvl = case_when(dsTvl < 400 ~ "4",
                                    dsTvl < 1200 ~ "3",
                                    dsTvl < 4000 ~ "2",
                                    dsTvl >= 4000 ~ "1")) |>
  filter(schAct == "DOMICILE → TRAVAIL → DOMICILE") |>
  group_by(ds4dom, ds4tvl) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnq),
            tps = weighted.median(Tps, CoeffRecEnq)) |>
  filter(!is.na(ds4tvl))

PER_ff |> mutate(ds4dom = case_when(dsDom < 400 ~ "4",
                                    dsDom < 1200 ~ "3",
                                    dsDom < 4000 ~ "2",
                                    dsDom >= 4000 ~ "1")) |>
  filter(schAct == "DOMICILE → TRAVAIL → DOMICILE") |>
  group_by(ds4dom) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP))

PER_ff |> mutate(ds4tvl = case_when(dsTvl < 400 ~ "4",
                                    dsTvl < 1200 ~ "3",
                                    dsTvl < 4000 ~ "2",
                                    dsTvl >= 4000 ~ "1")) |>
  filter(schAct == "DOMICILE → TRAVAIL → DOMICILE") |>
  group_by(ds4tvl) |>
  summarise(n = n(),
            dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            tps = weighted.median(Tps, CoeffRecEnqSansEMP))

PER_ff |>   
  filter(schAct == "DOMICILE → TRAVAIL → DOMICILE") |>
  summarise(n = n(),
                      dis = weighted.median(Dis, CoeffRecEnqSansEMP),
                      tps = weighted.median(Tps, CoeffRecEnqSansEMP))

# Chiffres selon densité
weighted.median(filter(PER_ff, !is.na(CoeffRecEnqSansEMP))$dsDom, filter(PER_ff, !is.na(CoeffRecEnqSansEMP))$CoeffRecEnqSansEMP, na.rm=T)

PER_ff |>
  filter(dsDom > 400, dsDom < 4000, !is.na(CoeffRecEnq)) |>
  group_by(PCS8) |>
  summarise(moy_dis = weighted.mean(Dis, CoeffRecEnqSansEMP),
            med_dis = weighted.median(Dis, CoeffRecEnqSansEMP),
            moy_tps = weighted.mean(Tps, CoeffRecEnqSansEMP, na.rm=T),
            med_tps = weighted.median(Tps, CoeffRecEnqSansEMP))

# Gros temps de depl
PER_ff |>
  mutate(long = Tps > 120) |>
  group_by(long) |> summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  mutate(p = nb / sum(nb))

PER_ff |>
  mutate(long = Tps > 120) |>
  group_by(PCS8, long) |> summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS8) |> mutate(p = nb / sum(nb)) |> filter(long)

PER_ff |>
  filter(Dis_VOI == Dis) |>
  mutate(long = Tps > 120) |>
  group_by(PCS8, long) |> summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS8) |> mutate(p = nb / sum(nb)) |> filter(long)

# Part des cadres, des femmes etc
quartilesDs = weighted.quantile(PER_ff$dsDom, PER_ff$CoeffRecEnq,
                                probs = c(.25, .5, .75), na.rm=T)
PER_ff |>
  mutate(quartile = case_when(dsDom <  quartilesDs[1] ~ "1",
                              dsDom <  quartilesDs[2] ~ "2",
                              dsDom <  quartilesDs[3] ~ "3",
                              dsDom >= quartilesDs[3] ~ "4")) |>
  group_by(quartile, PCS8) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(quartile) |> mutate(p = nb / sum(nb) * 100) |>
  filter(PCS8 == "03")

PER_ff |>
  group_by(PCS8, Genre) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS8) |> mutate(p = nb / sum(nb) * 100) |>
  filter(Genre == "F")

PER_ff |>
  filter(!is.na(LogOcc)) |>
  group_by(LogOcc, PCS8) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS8) |> mutate(p = nb / sum(nb) * 100) |>
  filter(LogOcc == "10")

PER_ff |>
  mutate(quartile = case_when(dsDom <  quartilesDs[1] ~ "1",
                              dsDom <  quartilesDs[2] ~ "2",
                              dsDom <  quartilesDs[3] ~ "3",
                              dsDom >= quartilesDs[3] ~ "4")) |>
  filter(!is.na(LogOcc)) |>
  group_by(quartile, LogOcc) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(quartile) |> mutate(p = nb / sum(nb) * 100) |>
  filter(LogOcc == "10")

# Distances moyennes lieu de travail
PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  group_by(Genre) |>
  summarise(disMT = weighted.mean(Dis, w = CoeffRecEnq, na.rm=T))

PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  group_by(Genre) |>
  summarise(disMT = weighted.mean(Travail_Dis, w = CoeffRecEnq, na.rm=T))

# Analyse de la variance =========================================================================

rapport("Analyse de la variance", prim = T)

table(!is.na(filter(PER, CoeffEnq>0)$Dis), filter(PER, CoeffEnq>0)$uid_ENQ)

summary(PER$Dis)/1000
summary(filter(PER, Dis>0)$Dis)/1000

nrow(filter(PER, Dis>0))

PER %>% group_by(uid_ENQ) %>% summarize(Max = max(Dis, na.rm=T)) %>% tab_Tri(parCol = 2) %>% head()
PER %>% group_by(uid_ENQ) %>% summarize(Max = max(Dis, na.rm=T)) %>% tab_Tri(parCol = 2, rev=T) %>% head()
nrow(filter(PER, Dis>1000000))
nrow(filter(PER, Dis>500000))

summary(filter(PER, Dis>0 & typoJo == "TRAV")$Dis)/1000
summary(filter(PER, Dis>0 & typoJo == "TRAV")$Tps)

PER %>%
  filter(Dis>0) %>%
  group_by(uid_ENQ) %>% summarise(disMoy = weighted.mean(Dis, w=CoeffEnq)/1000) %>%
  tab_Tri(parCol = "disMoy") %>%
  ggplot(aes(x = uid_ENQ, y = disMoy)) + geom_col() + coord_flip() + theme_bw()

sortie(nom = "Distances/Courbe des distances", taille = "carré")
g3 = DEP %>%
  filter(Dis < quantile(DEP$Dis, .99, na.rm=T)) %>%
  mutate(Dis = round(Dis/1000)) %>%
  group_by(Dis) %>% summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = Dis, y = p * 100)) + geom_line() +
  labs(title="Distribution des déplacements\nselon leur longueur",
       subtitle="99% des déplacements les plus courts (en distance)") +
  xlab("Longueur (km)") + ylab("Part de la population (%)")
g4 = DEP %>%
  filter(Duree < quantile(DEP$Duree, .99, na.rm=T)) %>%
  mutate(Duree = round(Duree)) %>%
  group_by(Duree) %>% summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = Duree, y = p * 100)) + geom_line() +
  labs(title="Distribution des déplacements\nselon leur durée",
       subtitle="99% des déplacements les plus courts (en temps)") +
  xlab("Durée (mn)") + ylab("Part de la population (%)")
limite = quantile(filter(PER, Dis>0, typoJo == "TRAV")$Dis, .99)
g1 = filter(PER_f, Dis<=limite) %>%
  mutate(Dis = round(Dis/1000)) %>%
  group_by(Dis) %>% summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = Dis, y = p * 100)) + geom_line() +
  labs(title="Distribution des journées de travail\nselon la distance totale parcourue",
       subtitle="99% des journées les plus courtes (en distance)") +
  xlab("Distance totale (km)") + ylab("Part de la population (%)")
limite = quantile(filter(PER_f, Tps>0)$Tps, .99)
g2 = filter(PER_f, Tps < limite) %>%
  mutate(Tps = round(Tps)) %>%
  group_by(Tps) %>% summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = Tps/60, y = p * 100)) + geom_line() +
  labs(title="Distribution des journées de travail\nselon la durée de déplacement",
       subtitle="99% des journées les plus courtes (en temps)",
       caption=src_fig(PER)) +
  xlab("Temps en déplacement (h)") + ylab("Part de la population (%)")
cowplot::plot_grid(g3, g4, g1, g2, align="hv", axis = "tb", nrow=2) %>% print()
off()

sortie(nom = "Distances/Déplacements selon longueur")
filter(PER_f) %>% ggplot(aes(x = Dis/1000)) + geom_density() +
  labs(title="Répartition des déplacements selon leur longueur (km),\ntoutes enquêtes confondues",
       subtitle="Déplacements de 0 à 100 km", caption=src_fig(emp=T, date = "janvier 2023")) +
  xlab("Distance totale (km)") + ylab("Part de la population")
off()

sortie(nom = "Distances/Déplacements selon durée")
filter(PER_f, Tps<=240) %>% ggplot(aes(x = Tps)) + geom_density() +
  labs(title="Répartition des déplacements selon leur durée (mn),\ntoutes enquêtes confondues",
       subtitle="Déplacements de 0 à 4 heures", caption=src_fig(emp=T, date = "janvier 2023")) +
  xlab("Temps passé en déplacement (mn)") + ylab("Part de la population")
off()

summary(filter(PER, Dis>0)$Tps)

sortie(nom = "Distances/Durée selon enquêtes", portrait = T)
PER_f %>%
  filter(Dis>0) %>%
  group_by(uid_ENQ) %>% summarise(tpsMoy = weighted.mean(Tps, w=CoeffRecEnq, na.rm=T)) %>%
  tab_Tri(parCol = "tpsMoy", rev=T) %>%
  ggplot(aes(x = uid_ENQ, y = tpsMoy)) + geom_col() + coord_flip()
off()

sortie(nom = "Distances/Distribution des distances (basique)")
ggplot(filter(PER_f, Dis != 0), aes(x = Dis)) + geom_density() + theme_bw()
off()

sortie(nom = "Distances/Variance, analyse factorielle", format = "pdf", taille = "a4",
       portrait = F)

acpEnq = PER_f %>% group_by(uid_ENQ) %>%
  summarise(disMoy = weighted.mean(Dis, w=CoeffRecEnq, na.rm=T),
            disSd = sd  (Dis, na.rm=T),
            tpsMoy = weighted.mean(Tps, w=CoeffRecEnq, na.rm=T),
            tpsSd = sd  (Tps, na.rm=T),
            voiMoy = weighted.mean(Dis_VOI/Dis, w= CoeffRecEnq, na.rm=T),
            mdiMoy = weighted.mean(Dis.M, w=CoeffRecEnq, na.rm=T),
            mdiSd = sd  (Dis.M, na.rm=T))

acpEnq_r = analyseFacto(base = acpEnq, colVar = colnames(select(acpEnq, -uid_ENQ)),
                        colUids = "uid_ENQ", scaleW = T)

acpAnalyse = left_join(acpEnq, acpEnq_r, by = c("uid_ENQ" = "colUids"))
ggplot(acpAnalyse, aes(x= `Dim.1`, y=disMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=disSd)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=tpsMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=tpsSd)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=voiMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=mdiMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.1`, y=mdiSd)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=disMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=disSd)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=tpsMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=tpsSd)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=voiMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=mdiMoy)) + geom_point()
ggplot(acpAnalyse, aes(x= `Dim.2`, y=mdiSd)) + geom_point()

# Axe 1 : distances totales et par trajet plus longues et + variables vs. plus courtes et + homogènes
# Axe 2 : temps long et moins de voiture vs temps court et + voitures

summary(acpEnq_r)

acpEnq_r = left_join(acpEnq_r, z_Nomenclature, by = c("colUids" = "uid_ENQ"))

tailles = PER_f %>% group_by(uid_ENQ) %>% summarise(n = n())
acpEnq_r = left_join(acpEnq_r, tailles, by=c("colUids" = "uid_ENQ"))

acpEnq_r = mutate(acpEnq_r, hjust = case_when(Dim.1 < -2 ~ -.1,
                                              Dim.1 > 2  ~ 1.1,
                                              T          ~ .5))
off()

sortie("Distances/Comparaisons entre enquêtes", taille = "page", portrait=F)
ggplot(acpEnq_r, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = Methodo, size = n), alpha = .6) +
  geom_text(aes(label = Libelle_Long, hjust = hjust),
            check_overlap = T, color = "grey20", size=3) +
  geom_label(aes(x = 0, y = -4.5, label = "durées plus longues"),
             fill = "grey90", fontface="italic") +
  geom_label(aes(x = 2.5, y = 3.5, label = "durées plus courtes"),
             fill = "grey90", fontface="italic") +
  geom_label(aes(x = 3.2, y = .25, label = "distances plus courtes\net homogènes"),
             fill = "grey90", fontface="italic") +
  geom_label(aes(x = -4, y = -.5, label = "distances plus longues\net hétérogènes"),
             fill = "grey90", fontface="italic") +
  geom_label(aes(x = -2.5, y = 3.5, label = "plus fort recours\nà l'automobile"),
             fill = "grey90", fontface="italic") +
  xlab("dimension 1 (39%)") +
  ylab("dimension 2 (25%)") +
  labs(title = "Caractéristiques des déplacements par enquête", caption = src_fig(),
       subtitle = "Issues d'une analyse factorielle des déplacements") +
  scale_size(name = "nombre\nd'enquêté⋅es") +
  scale_color_hue(name = "type\nd'enquête")
off()

cahEnq = categ_cah(acpEnq_r, nCateg = 4)
acpEnq = left_join(acpEnq, cahEnq, by=c("uid_ENQ" = "colUids"))

plotCateg(acpEnq,
          colUid = "uid_ENQ",
          cols = c("disMoy", "disSd", "tpsMoy", "tpsSd", "voiMoy", "mdiMoy", "mdiSd"),
          colCateg = "cluster")

ggplot(acpEnq, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(colour=cluster, size = disSd), alpha=.8) +
  geom_text(aes(label = Libelle_Long, colour=cluster),
            nudge_y=-.25) +
  theme_bw()


remove(acpEnq, cahEnq, acpEnq_r)

# Dép travail vs dép général ===========================================

rapport("Statistiques générales sur les journées de travail", prim = T)
t = nrow(filter(DEP, O_Motif == "11" | D_Motif == "11")) / nrow(DEP)
rapport("Part de déplacements dédiés au travail :", t*100, "%", info = T)

DEP = left_join(DEP, select(PER, uid_PER, CoeffEnq), by="uid_PER")

t = (sum(filter(DEP, O_Motif == "11" | D_Motif == "11")$Dis, na.rm=T) *
       sum(filter(DEP, O_Motif == "11" | D_Motif == "11")$CoeffEnq, na.rm=T)) /
  (sum(DEP$Dis, na.rm=T) * sum(DEP$CoeffEnq, na.rm=T))
rapport("Part de la distance en déplacement dédiés au travail, pondérée :", t*100, "%", info = T)

t = (sum(filter(DEP, O_Motif == "11" | D_Motif == "11")$Duree, na.rm=T) *
       sum(filter(DEP, O_Motif == "11" | D_Motif == "11")$CoeffEnq, na.rm=T)) /
  (sum(DEP$Duree, na.rm=T) * sum(DEP$CoeffEnq, na.rm=T))
rapport("Part de la durée en déplacement dédiée au travail, pondérée :", t*100, "%", info = T)

t = sum(filter(PER, typoJo == "TRAV")$CoeffEnq, na.rm=T) / sum(PER$CoeffEnq, na.rm=T)
rapport("Part de journées incluant un déplacement au travail, pondérée :", t*100, "%", info = T)

t = sum(filter(PER, Age>15 & Age<66 & typoJo == "TRAV")$CoeffEnq, na.rm=T) /
  sum(filter(PER, Age>15 & Age<66)$CoeffEnq, na.rm=T)
rapport("Part de journées incluant un déplacement au travail pour les 15~66 ans, pondérée :",
        t*100, "%", info = T)

t = sum(filter(PER, Activ %in% c("10", "11", "12") & typoJo == "TRAV")$CoeffEnq, na.rm=T) /
  sum(filter(PER, Activ %in% c("10", "11", "12"))$CoeffEnq, na.rm=T)
rapport("Part de journées d'actif⋅ves incluant un déplacement au travail :", t*100, "%", info = T)

t = weighted.median(filter(PER, typoJo == "TRAV")$Dis, w=filter(PER, typoJo == "TRAV")$CoeffEnq,
                    na.rm=T)
rapport("Distance médiane parcourue durant une journée de travail :", t/1000, "km", info=T)

t = weighted.median(filter(PER, Dis>0)$Dis, w=filter(PER, Dis>0)$CoeffEnq, na.rm=T)
rapport("Distance médiane parcourue durant une journée quelconque (hors non-dep) :", t/1000, "km", info=T)

t = weighted.median(filter(PER, typoJo == "TRAV")$Tps, w=filter(PER, typoJo == "TRAV")$CoeffEnq, na.rm=T)
rapport("Temps médian de déplacement durant une journée de travail :", t/60, "h", info=T)
t = weighted.median(filter(PER, Dis>0)$Tps, w=filter(PER, Dis>0)$CoeffEnq, na.rm=T)
rapport("Temps médian de déplacement durant une journée quelconque (hors non-dep) :", t/60, "h", info=T)

t = mean(filter(PER, typoJo == "TRAV")$Dis, na.rm=T) / 1000
rapport("Distance parcourue moyenne durant une journée de travail :", t, "km", info=T)

t = mean(filter(PER, typoJo == "TRAV" & Genre == "H")$Dis, na.rm=T) 
rapport("Distance parcourue moyenne d'un homme s'étant déplacée pour travail :", round(t/1000,3), "km", info=T)
t = mean(filter(PER, typoJo == "TRAV" & Genre == "F")$Dis, na.rm=T)
rapport("Distance parcourue moyenne d'une femme s'étant déplacée pour travail :", round(t/1000,3), "km", info=T)

t = (1 - (mean(filter(PER, typoJo == "TRAV" & Genre == "F")$Dis, na.rm=T)  /
            mean(filter(PER, typoJo == "TRAV" & Genre == "H")$Dis, na.rm=T))) * 100
rapport("Les femmes parcourent", round(t, 2), "% de distance en moins", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "23" & ZoneDens == "1")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "23" & ZoneDens == "1")$Dis, na.rm=T) 
rapport("Les femmes cheffes d'entreprise vivant en commune dense parcourent", round(t*100,2),
        "% de distance en moins", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "36" & ZoneDens == "1")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "36" & ZoneDens == "1")$Dis, na.rm=T) 
rapport("Les femmes cadres d'entreprise vivant en commune dense parcourent", round(t*100,2),
        "% de distance en moins", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "46" & ZoneDens == "1")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "46" & ZoneDens == "1")$Dis, na.rm=T) 
rapport("Les femmes prof. interm. d'entreprise vivant en commune dense parcourent", round(t*100,2),
        "% de distance en moins", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "61" )$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "61" )$Dis, na.rm=T) 
rapport("Les femmes ouvrières qualifiées parcourent", round(t*100,2), "% de distance en moins", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "55" )$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "55" )$Dis, na.rm=T) 
rapport("Les femmes employées de commerce parcourent", round(t*100,2), "% de distance en moins", info=T)

t = nrow(filter(PER, typoJo == "TRAV" & PCS42S == "66" & Genre == "F")) / nrow(filter(PER, typoJo == "TRAV" & PCS42S == "66"))
rapport(round(t*100,2), "% des ouvier⋅es qualifié⋅es sont des femmes", info=T)
t = nrow(filter(PER, typoJo == "TRAV" & PCS42S == "61" & Genre == "F")) / nrow(filter(PER, typoJo == "TRAV" & PCS42S == "61"))
rapport(round(t*100,2), "% des ouvrier⋅es non qualifié⋅es sont des femmes", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & Genre == "F" & PCS42S == "61" & ZoneDens == "1")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & Genre == "H" & PCS42S == "61" & ZoneDens == "1")$Dis, na.rm=T) 
rapport("Les femmes ouvrières qualifiées habitant en commune dense parcourent", round(t*100,2),
        "% de distance en plus que les hommes", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & PCS42S == "41")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & PCS42S == "46")$Dis, na.rm=T)
rapport("Les prof. interm. du public parcourent", round(t*100,2), "% de distance en moins",
        "que celles du privé", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & PCS42S == "32")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & PCS42S == "36")$Dis, na.rm=T) 
rapport("Les cadres du public parcourent", round(t*100,2), "% de distance en moins",
        "que celleux du privé", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & PCS42S == "55")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & PCS42S == "54")$Dis, na.rm=T) 
rapport("Les employé⋅es de commerce parcourent", round(t*100,2), "% de distance en moins",
        "que celleux de bureau", info=T)

t = 1 - mean(filter(PER, typoJo == "TRAV" & PCS42S == "51")$Dis, na.rm=T)  /
  mean(filter(PER, typoJo == "TRAV" & PCS42S == "55")$Dis, na.rm=T) 
rapport("Les employé⋅es du public parcourent", round(t*100,2), "% de distance en moins",
        "que celleux de commerce", info=T)

t1 = weighted.mean(filter(PER, typoJo == "TRAV" & Genre=="H" & Age > 19 & Age < 61)$Dis,
                   w=filter(PER, typoJo == "TRAV" & Genre=="H" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T)
t2 = weighted.mean(filter(PER, typoJo == "TRAV" & Genre=="F" & Age > 19 & Age < 61)$Dis,
                   w=filter(PER, typoJo == "TRAV" & Genre=="F" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T)
rapport("Un jour de travail, un homme âgé de 19 à 61 ans parcourt en moyenne", round(t1/1000,2),
        "km, une femme", round(t2/1000,2), "km", info=T)

t1 = weighted.mean(filter(PER, typoJo == "CHOM" & Genre=="H" & Age > 19 & Age < 61)$Dis,
                   w=filter(PER, typoJo == "CHOM" & Genre=="H" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T)
t2 = weighted.mean(filter(PER, typoJo == "CHOM" & Genre=="F" & Age > 19 & Age < 61)$Dis,
                   w=filter(PER, typoJo == "CHOM" & Genre=="F" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T)
rapport("Un jour chômé, un homme âgé de 19 à 61 ans parcourt en moyenne", round(t1/1000,2),
        "km, une femme", round(t2/1000,2), "km", info=T)

t = 1 - weighted.mean(filter(PER, typoJo == "CHOM" & Genre=="F" & Age > 19 & Age < 61)$Dis,
                      w=filter(PER, typoJo == "CHOM" & Genre=="F" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T) /
  weighted.mean(filter(PER, typoJo == "CHOM" & Genre=="H" & Age > 19 & Age < 61)$Dis,
                w=filter(PER, typoJo == "CHOM" & Genre=="H" & Age > 19 & Age < 61)$CoeffEnq, na.rm=T)
rapport("Les femmes parcourent donc", round(t*100,2), "% de km en moins", info=T)

t = weighted.median(filter(PER, !is.na(Travail_Dis) & Travail_Dis > 0 & !is.na(CoeffEnq))$Travail_Dis,
                    w=filter(PER, !is.na(Travail_Dis)  & Travail_Dis > 0 & !is.na(CoeffEnq))$CoeffEnq) * 2
rapport("Distance médiane au lieu d'emploi déclarée :", round(t/1000,2), "km", info=T)
t = weighted.mean(filter(PER, !is.na(Travail_Dis) & Travail_Dis > 0 & !is.na(CoeffEnq))$Travail_Dis,
                  w=filter(PER, !is.na(Travail_Dis)  & Travail_Dis > 0 & !is.na(CoeffEnq))$CoeffEnq) * 2
rapport("Distance moyenne au lieu d'emploi déclarée :", round(t/1000,2), "km", info=T)
t = weighted.median(filter(PER, typoJo == "TRAV")$Dis.V, w=filter(PER, typoJo == "TRAV")$CoeffEnq, na.rm=T)
rapport("Somme des distances à vol d'oiseau médiane pour une journée de travail :", round(t/1000, 2), "km", info=T)
t = weighted.mean(filter(PER, typoJo == "TRAV")$Dis.V, w=filter(PER, typoJo == "TRAV")$CoeffEnq, na.rm=T)
rapport("Somme des distances à vol d'oiseau moyenne pour une journée de travail :", round(t/1000, 2), "km", info=T)

t = nrow(filter(PER, schAct == "DOMICILE → TRAVAIL → DOMICILE")) / nrow(filter(PER, typoJo == "TRAV"))
rapport("Part des journées de travail de type DOMICILE → TRAVAIL → DOMICILE :", round(t*100,2), "%", info=T)
t = nrow(filter(PER, schAct == "DOMICILE → TRAVAIL → DOMICILE → TRAVAIL → DOMICILE")) / nrow(filter(PER, typoJo == "TRAV"))
rapport("Part des journées de travail de type DOMICILE → TRAVAIL → DOMICILE → TRAVAIL → DOMICILE :",
        round(t*100,2), "%", info=T)

t = nrow(filter(PER, schAct == "DOMICILE → TRAVAIL → COMMERCE → DOMICILE")) / nrow(filter(PER, typoJo == "TRAV"))
rapport("Part des journées de travail de type DOMICILE → TRAVAIL → COMMERCE → DOMICILE :", round(t*100,2), info=T)
t = nrow(filter(PER, schAct == "DOMICILE → TRAVAIL → COMMERCE → DOMICILE" & Genre == "F")) / nrow(filter(PER, schAct == "DOMICILE → TRAVAIL → COMMERCE → DOMICILE"))
rapport("Part des journées de travail de type DOMICILE → TRAVAIL → COMMERCE → DOMICILE :",
        round(t*100,2), "%")

sortie(nom = "Distances/Part pendules selon densité commune de résidence")
PER_ff %>%
  mutate(ZoneDens = discretisation(dsDom, methode = "quartiles")) |>
  mutate(ZoneDens = factor(ZoneDens, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(ZoneDens)) |>
  group_by(ZoneDens, pendule) %>%
  summarize(pop = sum(CoeffRecEnqSansEMP, na.rm=T), n = n()) %>%
  group_by(ZoneDens) %>% mutate(part = pop/sum(pop)*100) %>%
  ggplot(aes(x = pendule, y = part)) + geom_col() + coord_flip() + facet_wrap(~ZoneDens) +
  labs(title = "Part des travailleur·es suivant des déplacements pendulaires selon\nla densité de la commune de résidence",
       caption = src_fig(bu = T, emp=F)) +
  ylab("part (%)") + xlab("type de mouvement pendulaire") +
  geom_text(aes(label = round(part,1)), hjust=-.15)
off()

sortie(nom = "Distances/Part pendules selon densité commune de résidence et PCS8", taille = "page",
       portrait = T)
PER_f %>% 
  mutate(ZoneDens = discretisation(dsDom, methode = "quartiles")) |>
  mutate(ZoneDens = factor(ZoneDens, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(ZoneDens)) |>
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(ZoneDens, PCS8, pendule) %>%
  summarize(pop = sum(CoeffRecEnqSansEMP, na.rm=T), n = n()) %>%
  group_by(ZoneDens, PCS8) %>% mutate(part = pop/sum(pop)*100) %>%
  ggplot(aes(x = pendule, y = part)) + geom_col() + coord_flip() + facet_grid(PCS8~ZoneDens) +
  labs(title = "Part des travailleur·es suivant des déplacements pendulaires selon\nla densité de la commune de résidence",
       caption = src_fig(bu = T, emp=T, date="janvier 2023")) +
  ylab("part (%)") + xlab("type de mouvement pendulaire") +
  geom_text(aes(label = round(part,1)), hjust=-.15)
off()


# Tris à plat (distance selon x) ======

rapport("Statistiques simples sur les mobilités", prim = T)

sortie("Distances/Distance par Age et Genre")
viz_DispersionSelonVariable(table = mutate(PER_f, Genre = etqGenre(Genre)),
                            champContinu = "Dis",
                            champDiscret = "Age5", champCateg = "Genre",
                            champPoids = "CoeffRecEnq",
                            legendeContinu = "Distance parcourue (estimée) dans la journée",
                            legendeDiscret = "Classe d'âge", facteurDiv = 1000) %>%
  viz_Titre(titre = "Somme des distances selon la classe d'âge et le genre", rel_heights = c(.05,.95)) %>%
  print()
off()

sortie("Distances/Distance selon classe d'âge")
viz_DispersionSelonVariable(table = mutate(PER_ff, Genre = etqGenre(Genre)),
                            champContinu = "Dis",
                            champDiscret = "Age5", champCateg = "Genre",
                            champPoids = "CoeffRecEnq",
                            legendeContinu = "Distance parcourue (estimée) dans la journée",
                            legendeDiscret = "Classe d'âge", facteurDiv = 1000) %>%
  viz_Titre(titre = "Somme des distances selon la classe\nd'âge et le genre (travailleur⋅ses)", rel_heights = c(.05,.95)) %>%
  print()
off()

sortie("Distances/Nb déplacements selon âge et genre")
viz_DispersionSelonVariable(table = PER_f,
                            champContinu = "N",
                            champDiscret = "Age5", champCateg = "Genre",
                            champPoids = "CoeffRecEnq",
                            legendeContinu = "Nombre de déplacements",
                            legendeDiscret = "Classe d'âge", méthodeCalcul = 1) %>%
  viz_Titre(titre = "Nombre de déplacements selon la classe d'âge et le genre") %>%
  print()
off()

sortie("Distances/Nb activités selon âge et genre")
viz_DispersionSelonVariable(table = PER_f,
                            champContinu = "nAct",
                            champDiscret = "Age5", champCateg = "Genre",
                            champPoids = "CoeffRecEnq",
                            legendeContinu = "Nombre d'activités hors domicile",
                            legendeDiscret = "Classe d'âge", méthodeCalcul = 1) %>%
  viz_Titre(titre = "Nombre d'activités hors domicile selon la classe d'âge et le genre") %>%
  print()
off()

sortie("Distances/Distance (hors estimation approximative) selon ménage et genre")
viz_DispersionSelonVariable(table = PER_f,
                            champContinu = "DisOk",
                            champDiscret = "PCSMLT", champCateg = "Genre",
                            champPoids = "CoeffRecEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000) %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS Ménage et le genre") %>%
  print()
off()

sortie("Distances/Distance (hors estimation approximative) selon PCSM et ZoneDens")
PER_f |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
  viz_DispersionSelonVariable(champContinu = "DisOk",
                            champDiscret = "PCSMLT", champCateg = "dsDomQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000) %>%
  viz_Titre(titre = ml("Distance \"pacourue\" selon la PCS Ménage et la",
                       "classe de densité de la commune de résidence")) %>%
  print()
off()

sortie("Distances/Distance (hors estimation approximative) selon PCSM et ZoneDens (travail)")
PER_ff |>
  mutate(dsTvlQtl = discretisation(dsTvl, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsTvlQtl)) |>
  viz_DispersionSelonVariable(champContinu = "DisOk",
                            champDiscret = "PCSMLT", champCateg = "dsTvlQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000) %>%
  viz_Titre(titre = ml("Distance \"pacourue\" selon la PCS Ménage et la",
                       "classe de densité de la commune de résidence")) %>%
  print()
off()

sortie("Distances/Distance selon ZDTravMax et PCSMLT")
PER_ff |>
  mutate(dsTvlQtl = discretisation(dsTvl, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsTvlQtl)) |>
  filter(!PCSMLT %in% c("4", "7i", "7e") & !is.na(PCSMLT)) |>
  mutate(PCSMLT = etqPCSM(PCSMLT, dét=T)) |>
  viz_DispersionSelonVariable(champContinu = "Dis",
                            champDiscret = "PCSMLT",
                            champContinu2 = "Tps",
                            champCateg = "dsTvlQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000, méthodeCalcul = 1) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS Ménage et la classe",
                       "de densité de la commune de travail")) %>% print()
off()

# approche antérieure à l'utilisation de discretisation()
quartilesDs = weighted.quantile(PER_ff$dsTvl, PER_ff$CoeffRecEnq, c(.25, .5, .75), na.rm=T)
sortie("Distances/Distance selon ZD et PCSMLT, PCS 5 et 6", taille = "carré")
PER_ff |>
  filter(!PCSMLT %in% c("1", "7i", "7e"),
         PCS8 %in% c("05", "06")) |>
  mutate(ZoneDens = case_when(dsTvl < quartilesDs[1] ~ "1er quartile\nde densité",
                              dsTvl < quartilesDs[2] ~ "2e quartile\nde densité",
                              dsTvl < quartilesDs[3] ~ "3e quartile\nde densité",
                              dsTvl >= quartilesDs[3] ~ "4e quartile\nde densité")) |>
  mutate(ZoneDens = as.factor(ZoneDens)) |>
  mutate(PCSMLT = etqPCSM(PCSMLT)) |>
  filter(!is.na(ZoneDens)) |>
  viz_DispersionSelonVariable(
                            champContinu = "Dis",
                            champDiscret = "PCSMLT",
                            champContinu2 = "Tps",
                            champCateg = "ZoneDens",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000, méthodeCalcul = 1) |>
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS Ménage et la",
                       "densité de la commune de travail, employé·es/ouvrier·es")) %>% print()
off()

sum(filter(PER, PCSMLT == "2" & PCS8 %in% c("05","06") & ZoneDens %in% c("3", "4") & Genre == "H")$CoeffEnq, na.rm=T) /
  sum(filter(PER, PCSMLT == "2" & PCS8 %in% c("05","06") & ZoneDens %in% c("3", "4"))$CoeffEnq, na.rm=T)

sum(filter(PER, PCSMLT == "2" & PCS8 %in% c("05","06") & ZoneDens %in% c("1") & Genre == "F")$CoeffEnq, na.rm=T) /
  sum(filter(PER, PCSMLT == "2" & PCS8 %in% c("05","06") & ZoneDens %in% c("1"))$CoeffEnq, na.rm=T)

sortie("Distances/Distance selon ZD et PCSMLT 5 et 6 et F")
PER_ff |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
  filter(PCS8 %in% c("05", "06"), Genre == "F", !is.na(PCS8), !is.na(PCSMLT)) |>
  mutate(PCSMLT = etqPCSM(PCSMLT, dét = T)) |>
  viz_DispersionSelonVariable(champContinu = "Dis",
                            champDiscret = "PCSMLT",
                            champContinu2 = "Tps",
                            champCateg = "dsDomQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000, méthodeCalcul = 1) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS Ménage et la classe",
                       "de densité de la commune de résidence, femmes employées/ouvrières")) %>% print()
off()

sortie("Distances/Distance selon ZD et PCSMLT et PCS5")
PER_ff |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
  filter(PCS8 == "05", !is.na(PCS8), !is.na(PCSMLT)) |>
  mutate(PCSMLT = etqPCSM(PCSMLT, dét = T)) |>
  viz_DispersionSelonVariable(champContinu = "Dis",
                            champDiscret = "PCSMLT",
                            champContinu2 = "Tps",
                            champCateg = "dsDomQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000,
                            méthodeCalcul = 1, verbose = T) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS Ménage et la classe",
                       "de densité de la commune de résidence, employé·es")) %>% print()
off()


sortie("Distances/Distance selon ZD et PCSMLT et PCS6")
PER_ff |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
  filter(PCS8 == "06", !is.na(PCS8), !is.na(PCSMLT)) |>
  mutate(PCSMLT = etqPCSM(PCSMLT, dét = T)) |>
  viz_DispersionSelonVariable(champContinu = "Dis",
                              champDiscret = "PCSMLT",
                              champContinu2 = "Tps",
                              champCateg = "dsDomQtl",
                              champPoids = "CoeffRecEnqSansEMP",
                              legendeContinu = "Somme des distances estimées (km)",
                              legendeContinu2 = "Temps passé en déplacement (min)",
                              legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000,
                              méthodeCalcul = 1) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS Ménage et la classe",
                       "de densité de la commune de résidence, ouvrier·es")) %>% print()
off()

sortie("Distances/Distance par ZDTravMax et PCS")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(PCS8),
                                                  !PCS8 %in% c("00", "01", "07", "08", "09")),
                                           ZoneDens_travMax = etqZoneDens(ZoneDens_travMax,
                                                                          supprTrFaible = T,
                                                                          num = T),
                                           PCS8 = etqPCS8(PCS8, num=T), uno = 1),
                            champContinu = "Dis",
                            champDiscret = "PCS8",
                            champContinu2 = "Tps",
                            champCateg = "ZoneDens_travMax",
                            champPoids = "uno",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000, méthodeCalcul = 1) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS individuelle et la classe",
                       "de densité de la commune de travail")) %>% print()
off()

median(filter(PER, typoJo == "TRAV", PCS8 == "06", ZoneDens == "1", ZoneDens_travMax == "1")$Tps, na.rm=T)
median(filter(PER, typoJo == "TRAV", PCS8 == "03", ZoneDens == "1", ZoneDens_travMax == "1")$Tps, na.rm=T)

sum(filter(PER, !is.na(PCS42S) & typoJo == "TRAV")$CoeffEnq, na.rm=T) / sum(filter(PER, typoJo == "TRAV")$CoeffEnq, na.rm=T)

sortie("Distances/Distance selon ZD et PCS")
PER_ff |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(PCS8),
                                                  !PCS8 %in% c("00", "01", "07", "08", "09")),
                                           ZoneDens = etqZoneDens(ZoneDens,
                                                                  supprTrFaible = T,
                                                                  num = T),
                                           PCS8 = etqPCS8(PCS8, num=T), uno = 1),
                            champContinu = "Dis",
                            champDiscret = "PCS8",
                            champContinu2 = "Tps",
                            champCateg = "ZoneDens",
                            champPoids = "uno",
                            legendeContinu = "Somme des distances estimées (km)",
                            legendeContinu2 = "Temps passé en déplacement (min)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000, méthodeCalcul = 1) %>%
  viz_Titre(titre = ml("Distribution des déplacements selon la PCS individuelle et la classe",
                       "de densité de la commune de résidence")) %>% print()
off()

# Même chose, mais sans pondérations, en ggplot classique
levTvl = paste0("Commune de travail\n", niv_ZoneDens[1:3])
levRes = paste0("Commune de résidence\n", niv_ZoneDens[1:3])

medDsDom = weighted.median(PER_ff$dsDom, PER_ff$CoeffRecEnq)
medDsTvl = weighted.median(PER_ff$dsTvl, PER_ff$CoeffRecEnq)

g1 = PER_f %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax),
         !is.na(Dis)) %>%
  filter(typoJo == "TRAV") %>%
  mutate(PCS8 = etqPCS8(PCS8),
         ZoneDens = case_when(dsDom > medDsDom ~ "dense (> médiane)",
                              dsDom <= medDsDom ~ "peu dense (< médiane)"),
         ZoneDens_travMax = case_when(dsTvl > medDsTvl ~ "dense (> médiane)",
                                      dsTvl < medDsTvl ~ "peu dense (< médiane)")) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax)) |>
  mutate(ZoneDens = paste0("Commune de résidence\n", ZoneDens),
         ZoneDens_travMax = paste0("Commune de travail\n", ZoneDens_travMax)) %>%
  group_by(ZoneDens, ZoneDens_travMax, PCS8) %>%
  mutate(Dis = Dis/1000) %>%
  summarise(med = median(Dis),
            q03 = quantile(Dis, .025), q25 = quantile(Dis, .25),
            q75 = quantile(Dis, .75), q98 = quantile(Dis, .975)) %>%
  ggplot(aes(x = PCS8, y = med)) +
  geom_linerange(aes(ymin = q98, ymax = q03, alpha = "95% des\nobservations"),
                 key_glyph = "vline") +
  geom_crossbar(aes(ymin = q25, y = med, ymax = q75, fill = PCS8,
                    color = "médiane et\nécart\ninterquartile")) +
  scale_fill_manual(values = pal_PCS8[1:6], name = "PCS") +
  scale_color_manual(values = "black", name = NULL) +
  scale_alpha_manual(values = 1, name = NULL) +
  facet_grid(ZoneDens_travMax~ZoneDens) +
  coord_cartesian(ylim = c(0, 250)) +
  ylab("Distance totale (km)") + xlab("PCS de l'enquêté·e") +
  labs(title = "Distance totale parcourue (estimée) selon la PCS de l'enquêté·e") +
  theme_bw() + theme(legend.position = "right") +
  ggRetirerAxeX

g2 = PER_f %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax),
         !is.na(Tps)) %>%
  filter(typoJo == "TRAV") %>%
  mutate(PCS8 = etqPCS8(PCS8),
         ZoneDens = case_when(dsDom > medDsDom ~ "dense (> médiane)",
                              dsDom <= medDsDom ~ "peu dense (< médiane)"),
         ZoneDens_travMax = case_when(dsTvl > medDsTvl ~ "dense (> médiane)",
                                      dsTvl < medDsTvl ~ "peu dense (< médiane)")) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax)) |>
  group_by(ZoneDens, ZoneDens_travMax, PCS8) %>%
  mutate(Tps = Tps/60) %>%
  summarise(med = median(Tps),
            q03 = quantile(Tps, .025), q25 = quantile(Tps, .25),
            q75 = quantile(Tps, .75), q98 = quantile(Tps, .975)) %>%
  ggplot(aes(x = PCS8, y = med)) +
  geom_linerange(aes(ymin = q98, ymax = q03, alpha = "95% des\nobservations"),
                 key_glyph = "vline") +
  geom_crossbar(aes(ymin = q25, y = med, ymax = q75, fill = PCS8,
                    color = "médiane et\nécart\ninterquartile")) +
  scale_fill_manual(values = pal_PCS8[1:6], name = "PCS") +
  scale_color_manual(values = "black", name = NULL) +
  scale_alpha_manual(values = 1, name = NULL) +
  facet_grid(ZoneDens_travMax~ZoneDens) +
  ylab("Temps total (h)") + xlab("PCS de l'enquêté·e") +
  labs(title = "Temps passé en déplacement selon la PCS de l'enquêté·e",
       caption = src_fig()) +
  theme_bw() + theme(legend.position = "none") +
  ggRetirerAxeX

sortie("Distances/Distance par ZDT ou D et PCS", taille = "page", portrait = T)
cowplot::plot_grid(g1, g2, nrow=2, rel_heights = c(4.75,5.25), align = "v", axis="lr")
off()

sortie("Distances/Temps en déplacement selon PCSM et ZD")
PER_ff |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile de ds."))) |>
  filter(!is.na(dsDomQtl)) |>
  viz_DispersionSelonVariable(champContinu = "Tps",
                            champDiscret = "PCSMLT", champCateg = "dsDomQtl",
                            champPoids = "CoeffRecEnqSansEMP",
                            legendeContinu = "Temps passé en déplacement (minutes)",
                            legendeDiscret = "PCS Ménage simplifiée") %>%
  viz_Titre(titre = ml("Temps de déplacement selon la PCS Ménage et la classe",
                       "de densité de la commune de résidence")) %>% print()
off()

sortie("Distances/Distance non approximative selon PCS8")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & ZoneDens != "4" & 
                                                    typoJo == "TRAV" & PCS8 != "09" & PCS8 != "00" & PCS8 != "08"),
                                           ZoneDens = etqZoneDens(ZoneDens), PCS8 = etqPCS8(PCS8)),
                            champContinu = "DisOk",
                            champDiscret = "PCS8", champCateg = "ZoneDens",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000) %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS-8") %>% print()
off()

sortie("Distances/Temps en dépalcement selon PCS8 et ZoneDens")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & ZoneDens != "4" & 
                                                    typoJo == "TRAV" & PCS8 != "09" & PCS8 != "00" & PCS8 != "08"),
                                           ZoneDens = etqZoneDens(ZoneDens), PCS8 = etqPCS8(PCS8)),
                            champContinu = "Tps",
                            champDiscret = "PCS8", champCateg = "ZoneDens",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Temps passé en déplacement (minutes)",
                            legendeDiscret = "PCS individuelle", méthodeCalcul = 1) %>%
  viz_Titre(titre = "Temps en déplacement selon la PCS-8") %>% print()
off()

sortie("Distances/Distance non approximative selon PCS42S et Genre")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & ZoneDens != "4" & 
                                                    typoJo == "TRAV" & PCS8 != "09" & PCS8 != "00" &
                                                    PCS8 != "08" & PCS8 != "07" & !is.na(PCS42S)),
                                           ZoneDens = etqZoneDens(ZoneDens), PCS42S = etqPCS42S(PCS42S)),
                            champContinu = "DisOk",
                            champDiscret = "PCS42S", champCateg = "Genre",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000) %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS-42S et le genre") %>% print()
off()

sortie("Distances/Distance et temps par PCS et Genre", taille = "page", portrait = T)
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & 
                                                    typoJo == "TRAV" &
                                                    !PCS8 %in% c("00", "01", "07", "08", "09") &
                                                    !is.na(PCS42S) &
                                                    Dis<500000),
                                           ZoneDens = etqZoneDens(ZoneDens),
                                           PCS42S = etqPCS42S(PCS42S, num = T),
                                           Genre = etqGenre(Genre)),
                            champContinu = "Dis",
                            champContinu2 = "Tps",
                            champDiscret = "PCS42S",
                            champCateg = "Genre",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance estimée (km)",
                            legendeContinu2 = "Temps passé en déplacement (mn)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000, méthodeCalcul = 1) %>%
  viz_Titre(titre = "Distribution de la distance estimée selon la PCS et le genre", rel_heights = c(.05, .95)) %>% print()
off()

sortie("Distances/Distance par PCS et Genre ZD1")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & 
                                                    ZoneDens == "1" &
                                                    typoJo == "TRAV" &
                                                    !PCS42S == "69" &
                                                    !PCS8 %in% c("00", "01", "07", "08", "09") &
                                                    !is.na(PCS42S)),
                                           ZoneDens = etqZoneDens(ZoneDens),
                                           PCS42S = etqPCS42S(PCS42S, num = T),
                                           Genre = etqGenre(Genre), poidsNul = 1),
                            champContinu = "Dis",
                            champDiscret = "PCS42S",
                            champCateg = "Genre",
                            champPoids = "poidsNul",
                            legendeContinu = "Distance estimée (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000,
                            verbose = T, view = T) %>%
  viz_Titre(titre = ml("Distribution de la distance estimée selon la PCS et le genre,",
                       "résident·es d'une commune de forte densité"), rel_heights = c(.05, .95)) %>% print()
off()

sortie("Distances/Distance par PCS et Genre ZD3")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & 
                                                    ZoneDens == "3" &
                                                    typoJo == "TRAV" &
                                                    !PCS8 %in% c("00", "01", "07", "08", "09") &
                                                    !PCS42S == "69" &
                                                    !is.na(PCS42S)),
                                           ZoneDens = etqZoneDens(ZoneDens),
                                           PCS42S = etqPCS42S(PCS42S, num = T),
                                           Genre = etqGenre(Genre), poidsNul = 1),
                            champContinu = "Dis",
                            champDiscret = "PCS42S",
                            champCateg = "Genre",
                            champPoids = "poidsNul",
                            legendeContinu = "Distance estimée (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000,
                            verbose = T, view = T) %>%
  viz_Titre(titre = ml("Distribution de la distance estimée selon la PCS et le genre,",
                       "résident·es d'une commune de faible densité"), rel_heights = c(.05, .95)) %>% print()
off()

# En ggplot classique :
levRes = paste0("Commune de résidence\n", niv_ZoneDens[1:3])

sortie("Distances/Distance ggplot par PCS et Genre", taille = "page", portrait=T)
g1 = PER_f %>%
  filter(!is.na(PCS42S), !PCS42S %in% c("00", "10", "69",
                                        "80", "81", "82", "83", "84", "85", "86", "87",
                                        "88", "89", "90"),
         !is.na(ZoneDens), !is.na(ZoneDens_travMax),
         !is.na(Dis)) %>%
  filter(typoJo == "TRAV") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T),
         Genre = etqGenre(Genre),
         ZoneDens = case_when(dsDom > medDsDom ~ "dense (> médiane)",
                              dsDom <= medDsDom ~ "peu dense (< médiane)"),
        ZoneDens_travMax = case_when(dsTvl > medDsTvl ~ "dense (> médiane)",
                                     dsTvl < medDsTvl ~ "peu dense (< médiane)")) %>%
  filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax)) |>
  group_by(ZoneDens, PCS42S, Genre) %>%
  mutate(Dis = Dis / 1000) %>%
  summarise(q03 = quantile(Dis, .025), q25 = quantile(Dis, .25), q50 = quantile(Dis, .5),
            q75 = quantile(Dis, .75), q98 = quantile(Dis, .975)) %>%
  ggplot(aes(y = PCS42S, x = q50)) +
  geom_linerange(aes(xmin = q03, xmax = q98, color = Genre,
                     linetype = "95% de la\ndistribution"), position = position_dodge(width = 1)) +
  geom_crossbar(aes(xmin = q25, x = q50, xmax = q75, fill = PCS42S, color = Genre,
                    alpha = "médiane\net nécart\ninter-\nquartile"), position = "dodge") +
  scale_fill_manual(values = rev(pal_PCS42S), name = "PCS", breaks=NULL) +
  scale_color_hue(name = "Genre") +
  scale_alpha_manual(values = 1, name = NULL) +
  scale_linetype_manual(values = 1, name = NULL) +
  facet_grid(~ZoneDens) + coord_cartesian(xlim = c(0,500)) +
  xlab("Distance totale (km)") + ylab("PCS de l'enquêté·e") +
  labs(title = "Distance totale parcourue (estimée)\nselon la PCS de l'enquêté·e",
       subtitle = "Ensemble de l'effectif") +
  theme_bw() + theme(legend.position = "none")

g2 = PER_f %>%
  filter(!is.na(PCS42S), !PCS42S %in% c("00", "10", "69",
                                        "80", "81", "82", "83", "84", "85", "86", "87",
                                        "88", "89", "90"),
         !is.na(ZoneDens), !is.na(ZoneDens_travMax),
         !is.na(Dis)) %>%
  filter(schAct == "DOMICILE → TRAVAIL → DOMICILE") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T),
         Genre = etqGenre(Genre),
         ZoneDens = case_when(dsDom > medDsDom ~ "dense (> médiane)",
                              dsDom <= medDsDom ~ "peu dense (< médiane)"),
         ZoneDens_travMax = case_when(dsTvl > medDsTvl ~ "dense (> médiane)",
                              dsTvl < medDsTvl ~ "peu dense (< médiane)")) %>%
         filter(!is.na(ZoneDens), !is.na(ZoneDens_travMax)) |>
  group_by(ZoneDens, PCS42S, Genre) %>%
  mutate(Dis = Dis / 1000) %>%
  summarise(q03 = quantile(Dis, .025), q25 = quantile(Dis, .25), q50 = quantile(Dis, .5),
            q75 = quantile(Dis, .75), q98 = quantile(Dis, .975), n = n()) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(y = PCS42S, x = q50)) +
  geom_linerange(aes(xmin = q03, xmax = q98, color = Genre,
                     linetype = "95% de la\ndistribution"), position = position_dodge(width = 1)) +
  geom_crossbar(aes(xmin = q25, x = q50, xmax = q75, fill = PCS42S, color = Genre,
                    alpha = "médiane\net écart\ninter-\nquartile"), position = "dodge") +
  scale_fill_manual(values = rev(pal_PCS42S), name = "PCS", breaks=NULL) +
  scale_color_hue(name = "Genre") +
  scale_alpha_manual(values = 1, name = NULL) +
  scale_linetype_manual(values = 1, name = NULL) +
  facet_grid(~ZoneDens) + coord_cartesian(xlim = c(0,500)) +
  xlab("Distance totale (km)") + ylab("PCS de l'enquêté·e") +
  labs(subtitle = "Journées comportant une seule navette",
       caption=src_fig()) +
  theme_bw() + theme(legend.position = "bottom")

print(cowplot::plot_grid(g1, g2, nrow = 2, rel_heights = c(.48, .52)))
off()

# Simple comparaison par genre
PER_f %>%
  filter(!is.na(PCS42S), !PCS42S %in% c("00", "10", "69",
                                        "80", "81", "82", "83", "84", "85", "86", "87",
                                        "88", "89", "90"),
         !is.na(ZoneDens), !is.na(ZoneDens_travMax),
         !is.na(Dis)) %>%
  filter(typoJo == "TRAV") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T),
         Genre = etqGenre(Genre)) %>%
  group_by(PCS42S, Genre) %>%
  mutate(Dis = Dis / 1000) %>%
  summarise(q03 = quantile(Dis, .025), q25 = quantile(Dis, .25), q50 = quantile(Dis, .5),
            q75 = quantile(Dis, .75), q98 = quantile(Dis, .975)) |>
  select(PCS42S, Genre, q50) |>
  pivot_wider(names_from = Genre, values_from = q50) |>
  mutate(ecart = 1-Femme/Homme) |>
  tab_Tri(parCol = "ecart", rev = T)
  

sortie("Distances/Distance non approximative selon PCS42S et Genre")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens) & ZoneDens == "2" & 
                                                    typoJo == "TRAV" & PCS8 != "09" & PCS8 != "00" &
                                                    PCS8 != "08" & PCS8 != "07" & !is.na(PCS42S)),
                                           ZoneDens = etqZoneDens(ZoneDens), PCS42S = etqPCS42S(PCS42S)),
                            champContinu = "DisOk",
                            champDiscret = "PCS42S", champCateg = "Genre",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000) %>%
  viz_Titre(titre = ml("Distance \"parcourue\n selon la PCS-16 et le genre",
                       "com. de résidc. de densité interm.")) %>% print()
off()

sortie("Distances/Distance non approximative par PCS, densité intermédiaire")
viz_DispersionSelonVariable(table = mutate(filter(PER, !is.na(ZoneDens_travMax) & ZoneDens_travMax == "2" & 
                                                    typoJo == "TRAV" & PCS8 != "09" & PCS8 != "00" &
                                                    PCS8 != "08" & PCS8 != "07" & !is.na(PCS42S)),
                                           ZoneDens = etqZoneDens(ZoneDens), PCS42S = etqPCS42S(PCS42S)),
                            champContinu = "DisOk",
                            champDiscret = "PCS42S", champCateg = "Genre",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS individuelle", facteurDiv = 1000) %>%
  viz_Titre(titre = ml("Distance \"parcourue\n selon la PCS-16 et le genre",
                       "com. de résidc. de densité interm.")) %>% print()
off()

sortie("Distances/Distance non approximative selon PCSM et LogType")
viz_DispersionSelonVariable(table = mutate(PER),
                            champContinu = "DisOk",
                            champDiscret = "PCSMLT", champCateg = "LogType",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000) %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS Ménage et le type de logement") %>% print()
off()

sortie("Distances/Distance non approximative selon PCSM et LogOcc")
viz_DispersionSelonVariable(table = mutate(filter(PER, !LogOcc %in% c("30", "40", "99", "22") & !is.na(LogOcc))),
                            champContinu = "DisOk",
                            champDiscret = "PCSMLT", champCateg = "LogOcc",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée", facteurDiv = 1000) %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS Ménage et le statut d'habitation") %>% print()
off()

sortie("Distances/Temps en déplacement selon PCSM et LogOcc")
viz_DispersionSelonVariable(table = mutate(filter(PER, !LogOcc %in% c("30", "40", "99", "22") & !is.na(LogOcc))),
                            champContinu = "Tps",
                            champDiscret = "PCSMLT", champCateg = "LogOcc",
                            champPoids = "CoeffEnq",
                            legendeContinu = "Distance parcourue estimée : vélo, DRM et automobile (km)",
                            legendeDiscret = "PCS Ménage simplifiée") %>%
  viz_Titre(titre = "Distance \"parcourue\" selon la PCS Ménage et le statut d'habitation") %>% print()
off()

# AOVs ====

base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e") &
                Age <65 & ZoneRang != "0") %>%
  mutate(poidsZero = ifelse(is.na(CoeffEnq), NA, 1))
base = left_join(base, centroidesAAV(), by=c("ZF" = "ZF")) %>%
  mutate(disCentre = dis/1000)

aovPcsDis = aov(data = filter(base, Dis != 0), log(Dis) ~ PCS8)
summary(aovPcsDis)
# On vérifie que les résidus sont normaux : oui
hist(aovPcsDis$residuals)

cor.test(base$Dis, base$disCentre)
cor.test(base$Dis, base$disCentre, method = "spearman")
cor.test(log(filter(base, Dis!=0)$Dis), filter(base, Dis!=0)$disCentre)
cor.test(log(filter(base, Dis!=0)$Dis), log(filter(base, Dis!=0)$disCentre))

cor.test(base$Dis, base$Tps)
cor.test(log(filter(base, Dis!=0)$Dis), filter(base, Dis!=0)$Tps)



aov(data = filter(base, Dis != 0), log(Dis) ~ PCSMT) %>% summary()
aov(data = filter(base, Dis != 0), log(Dis) ~ Genre) %>% summary()
aov(data = filter(base, Dis != 0), log(Dis) ~ Activ) %>% summary()
aov(data = filter(base, Dis != 0), log(Dis) ~ ZoneDens) %>% summary()
aov(data = filter(base, Dis != 0), log(Dis) ~ ZoneRang) %>% summary()


aov(data = filter(base, Dis != 0), log(Dis) ~ Activ + Genre) %>% summary()
aov(data = filter(base, Dis != 0), log(Dis) ~ Activ * Genre) %>% summary()

# Modèles LM ====

load("Data/shp_COM.rds") 
load("Data/shp_ZF.rds")

# → Tous les travailleurs, PCS individuelle
base = filter(PER_ff) %>%
  mutate(poidsZero = ifelse(is.na(CoeffEnq), NA, 1)) |>
  left_join(centroidesAAV(), by=c("ZF" = "ZF")) %>%
  mutate(disCentre = dis/1000) |>
  valref()

remove(shp_COM, shp_ZF) ; gc()

rapport("Part des femmes parmi les employé·es à temps partiel :",
        round(nrow(filter(base, Activ == "11" & Genre == "F")) / nrow(filter(base, Activ == "11")), 2),
        "%", info=T)
rapport("Part des femmes parmi les employé·es de service à la personne :",
        round(nrow(filter(base, PCS42S == "56" & Genre == "F")) / nrow(filter(base, PCS42S == "56")), 2),
        "%", info=T)

# ~ Modèles de 2022 ====
# Les vieilles versions du modèle (2022)
sortie("Distances/Distance parcourue (ancien modèle, vol d'oiseau)")
regressionLog(base = valref(PER), val = "Dis.V", formule = "PCSMLT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffRecEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Modélisation de la distance parcourue (à vol d'oiseau)", unite = "km",
              imprDistrib = F)
off()

sortie("Distances/Distance parcourue (ancien modèle, distance ok)")
regressionLog(base = valref(PER), val = "DisOk", formule = "PCSMLT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance parcourue (km)",
              titre = "Modélisation de la distance parcourue (en véhicule)", unite = "km",
              imprDistrib = F)
off()

sortie("Distances/Distance parcourue (ancien modèle, 16-70 ans)")
regressionLog(base = filter(valref(PER), Age>16 & Age<70), val = "Dis.V", formule = "PCSMLT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Modélisation de la distance parcourue (à vol d'oiseau, 16 à 70 ans)", unite = "km",
              imprDistrib = F)
off()

sortie("Distances/Distance parcourue (ancien modèle, travailleur⋅ses, par PCSM)")
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") &
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCSMT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref) 
off()

sortie("Distances/Distance parcourue (ancien modèle, travailleur⋅ses, par PCS8)")
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref) 
off()

sortie("Distances/Distance parcourue (ancien modèle, travailleur⋅ses, par PCS8)")
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 * Genre + Activ + ZoneDens + ZoneRang",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = ml("Modélisation de la somme des distances à vol d'oiseau",
                         "(journée comptant au moins un déplacement pour le travail)"),
              unite = "km", imprDistrib = F,
              refDescr = ml("femme de prof. interm. à temps plein, vivant dans le centre",
                            "d'une agglomération de plus de 200.000 hab."),
              caption=src_fig(bu = T)) 
off()

# Code non mis à jour
#    → Tous les travailleurs, PCS Ménage des intermédiaires
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 == "04" & 
                              !Activ %in% c("31", "32", "33", "21", "22") &
                              !PCSMT %in% c("7i", "7e", "3C", "4", "5", "6")),
              val = "Dis.V", formule = "PCSMT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), travailleur⋅ses de prof. intermédiaires", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Tous les travailleurs, PCS Ménage des employé⋅es
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 == "05" & 
                              !Activ %in% c("31", "32", "33", "21", "22") &
                              !PCSMT %in% c("7i", "7e", "1")),
              val = "Dis.V", formule = "PCSMT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), employé⋅es", unite = "km",
              imprDistrib = F, refDescr = ref)
#    → Tous les travailleurs, PCS Ménage des ouvrier⋅es
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 == "06" & 
                              !Activ %in% c("31", "32", "33", "21", "22") &
                              !PCSMT %in% c("7i", "7e", "1", "3C")),
              val = "Dis.V", formule = "PCSMT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), ouvrier⋅es", unite = "km",
              imprDistrib = F, refDescr = ref)
#    → Tous les travailleurs, PCS indiv des hommes
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & Genre == "H" & 
                              !Activ %in% c("31", "32", "33", "21", "22")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), hommes", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Tous les travailleurs, PCS indiv des femmes
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & Genre == "F" &
                              !Activ %in% c("31", "32", "33", "21", "22")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale (journée de travail), femmes", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Même chose, avec ou sans enfants
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre * MenEnfants",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale, selon la présence d'enfants dans le ménage", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Même chose, sans enfants
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e") &
                              MenEnfants==F, Age > 29 & Age < 50),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale, 30-49 ans sans enfants dans le ménage", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Même chose, avec enfants
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e") &
                              MenEnfants==T, Age > 29 & Age < 50),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale, 30-49 ans avec enfants dans le ménage", unite = "km",
              imprDistrib = F, refDescr = ref) 
#    → Tous les travailleurs, PCS individuelle, statut d'occupation
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & 
                              !Activ %in% c("31", "32", "33", "21", "22") &
                              !PCSMT %in% c("7i", "7e") & LogOcc %in% c("10", "20", "21")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre + LogOcc",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Distance totale, selon la PCS et le statut individuels", unite = "km",
              imprDistrib = F, refDescr = "(femme locataire, prof. interm.,\nvivant dans le centre d'une ville de plus de 200.00 hab.)") %>% summary()
#    → Pour comparaison : travpriv désactivé et activé
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS42S %in% as.character(20:69) & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Somme des distances, toutes catégories de PCS détaillée disponible", unite = "km",
              imprDistrib = F, refDescr = ref) %>% summary()
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS42S %in% PCS_privé & 
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Somme des distances, PCS détaillée indiquant le secteur privé", unite = "km",
              imprDistrib = F, refDescr = ref) %>% summary()
regressionLog(base = filter(valref(PER), typoJo == "TRAV" & PCS42S %in% c("32", "41", "51") &
                              !Activ %in% c("31", "32", "33", "21", "22") & !PCSMT %in% c("7i", "7e")),
              val = "Dis.V", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance à vol d'oiseau (km)",
              titre = "Somme des distances, PCS détaillée indiquant le secteur public", unite = "km",
              imprDistrib = F, refDescr = ref) %>% summary()

# ~ Tests individuels ====

# pourquoi ce modèle est-il utile ? parce qu'il faut contrôler les effets de composition, e.g.
PER %>% valref() %>%
  filter(typoJo == "TRAV", PCS8 %in% c("01", "02", "03", "04", "05", "06")) %>%
  group_by(PCS8, Genre) %>% summarize(n = n()) %>%
  group_by(PCS8) %>% mutate(part = n / sum(n) * 100)
PER %>% valref() %>%
  filter(typoJo == "TRAV", PCS8 %in% c("01", "02", "03", "04", "05", "06")) %>%
  group_by(ZoneDens, PCS8) %>% summarize(n = n()) %>%
  group_by(ZoneDens) %>% mutate(part = n / sum(n) * 100) 
PER %>% valref() %>%
  filter(typoJo == "TRAV", !is.na(LogOcc)) %>%
  group_by(ZoneDens, LogOcc) %>% summarize(n = n()) %>%
  group_by(ZoneDens) %>% mutate(part = n / sum(n) * 100)
PER %>% valref() %>%
  filter(typoJo == "TRAV", !is.na(LogOcc), PCS8 %in% c("01", "02", "03", "04", "05", "06")) %>%
  group_by(PCS8, LogOcc) %>% summarize(n = n()) %>%
  group_by(PCS8) %>% mutate(part = n / sum(n) * 100)

# avant de commencer, quelle est la valeur explicative de chaque variable à elle seule ?
vars = c("PCS8", "Activ", "dsDomEtq", "dsTvlEtq",
         "ZoneRang", "Genre", "Age10", "PCSMLT", "NivEtu", "LogOcc",
         "MenEnfants")

sortie("Distances/Distance parcourue et modèles variables individuelles",
       h = 7.5, l = 17, taille = "man")
tableModesSpl = tibble(vars = vars) %>%
  rowwise() %>%
  mutate(r2 = summary(lm(data = filter(PER_ff, Dis > 0),
                         formula = paste0("log(Dis) ~ ", vars)))$adj.r.squared,
         vars = nomColsLisibles(vars)) %>%
  tab_Tri(i = "vars", parCol = "r2")
ggplot(tableModesSpl, aes(x = vars, y = r2)) + geom_col() + coord_flip() + theme_bw() +
  labs(title = "Valeur explicative individuelle de chaque variable",
       subtitle = ml("R² du modèle testé avec chaque variable individuellement",
                     "pour la distance estimée parcourue lors d'une journée de travail"),
       catpion = src_fig()) +
  ylab("R² du modèle") + xlab("Variable testée") %>% print()
off()

# idée fun : associer les variables deux à deux
paires = paste(sort(rep(vars, times=length(vars))), "*", rep(vars,times=length(vars)))
tableMods =
  tibble(vars = paires) %>%
  rowwise() %>%
  mutate(r2 = summary(lm(data = filter(PER_ff, Dis>0),
                         formula = paste0("log(Dis) ~ ", vars)))$adj.r.squared,
         vars = paste(nomColsLisibles(unlist(strsplit(vars, " * ", fixed=T))), collapse = " * ")) %>%
  mutate(type = ifelse(unlist(strsplit(vars, " * ", fixed=T))[1] == unlist(strsplit(vars, " * ",
                                                                                    fixed=T))[2],
                       "une variable", "deux variables"),
         vars = ifelse(unlist(strsplit(vars, " * ", fixed=T))[1] == unlist(strsplit(vars, " * ",
                                                                                    fixed=T))[2],
                       unlist(strsplit(vars, " * ", fixed=T))[1], vars)) %>%
  tab_Tri(i = "vars", parCol = "r2", rev=T)

sortie("Distances/Distance parcourue et modèles 2à2")
ggplot(data = tab_Tri(t = tableMods[c(1:20*2-1),], parCol = "r2", rev=F), aes(x = vars, y = r2)) +
  geom_col(aes(fill = type)) +
  coord_flip() + theme_bw() +
  labs(title = "Valeur explicative individuelle de chaque paire de variables",
       subtitle = ml("R² du modèle testé avec chaque couple de variables",
                     "pour la distance estimée parcourue lors d'une journée de travail"),
       caption = src_fig(date = "janvier 2023")) +
  scale_fill_manual(values=c("darkslateblue", "slateblue"), name="Type de modèle") +
  ylab("R² du modèle") + xlab("Variable testée")
off()

# ~ Modèles de référence ====

ref = ml("femme, prof. interm., vivant et travaillant en commune urbaine à temps plein",
         "dans une AAV de plus de 700.000 hab., 40 à 49 ans")

# LE modèle de référence
sortie("Distances/Distance parcourue journée travail, modèle",
       taille = "man", h = 22, l = 16)
regressionLog(base = valref(PER_ff),
              val = "Dis", formule =  "PCS8 + Activ + dsDomEtq + Genre + Age10",
              poids="CoeffRecEnqSansEMP", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref,
              caption = src_fig(emp = F))
off()

regressionLog(base = valref(PER_ff),
              val = "Dis", formule =  "PCS8 + Activ + dsTvlEtq + Genre + Age10",
              poids="CoeffRecEnqSansEMP", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "juillet 2023"))

# Par genre
sortie("Distances/Distance parcourue journée travail, modèle, par genre", portrait = T, taille = "page")
regressionLog(base = valref(mutate(PER_ff, Genre = etqGenre(Genre))),
              val = "Dis", formule =  "PCS8 + Activ + dsDomEtq  + Age10", colComparaison = "Genre",
              poids="CoeffRecEnqSansEMP", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref,
              caption = src_fig(emp = F))
off()

sortie("Distances/Distance parcourue journée travail, modèle, par PCSDet", portrait = T, taille = "page")
regressionLog(base = valref(filter(PER_ff, !is.na(PCS42S))),
              val = "Dis", formule =  "PCS42S + dsDomEtq  + Age10", colComparaison = "Genre",
              poids="CoeffRecEnqSansEMP", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref,
              caption = src_fig(emp = F))
off()

# Pour les CS détaillées
niv_pg1 = niv_PCS42S[ 2: 7]
niv_pg2 = niv_PCS42S[ 8:13]
niv_pg3 = niv_PCS42S[14:19]

pg1 = PER_ff |>
  valref() |>
  mutate(PCS42S_lib = etqPCS42S(PCS42S),
         PCS42S = etqPCS42S(champ = PCS42S, num = T)) |>
  filter(PCS42S_lib %in% niv_pg1) |>
  regressionLog(val= "Dis", formule = "Genre + dsDomEtq + Age10 + Activ",
                colComparaison = "PCS42S", poids = "CoeffRecEnqSansEMP",
                retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
                titre = "Distance estimée (journée de travail)\nPar PCS détaillée (1/3)",
                unite = "km", imprDistrib = F, returnFig = T,
                caption = src_fig(emp=F))

pg2 = PER_ff |>
  valref() |>
  mutate(PCS42S_lib = etqPCS42S(PCS42S),
         PCS42S = etqPCS42S(champ = PCS42S, num = T)) |>
  filter(PCS42S_lib %in% niv_pg2) |>
  regressionLog(val= "Dis", formule = "Genre + dsDomEtq + Age10 + Activ",
                colComparaison = "PCS42S", poids = "CoeffRecEnqSansEMP",
                retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
                titre = "Distance estimée (journée de travail)\nPar PCS détaillée (2/3)",
                unite = "km", imprDistrib = F, returnFig = T,
                caption = src_fig(emp=F))

pg3 = PER_ff |>
  valref() |>
  mutate(PCS42S_lib = etqPCS42S(PCS42S),
         PCS42S = etqPCS42S(champ = PCS42S, num = T)) |>
  filter(PCS42S_lib %in% niv_pg3) |>
  regressionLog(val= "Dis", formule = "Genre + dsDomEtq + Age10 + Activ",
                colComparaison = "PCS42S", poids = "CoeffRecEnqSansEMP",
                retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
                titre = "Distance estimée (journée de travail)\nPar PCS détaillée (3/3)",
                unite = "km", imprDistrib = F, returnFig = T,
                caption = src_fig(emp=F))

sortie("Distances/Modèle Dis par PCS42S, pg1", taille = "page")
  print(pg1)
off()
sortie("Distances/Modèle Dis par PCS42S, pg2", taille = "page")
  print(pg2)
off()
sortie("Distances/Modèle Dis par PCS42S, pg3", taille = "page")
  print(pg3)
off()

image_read("Sorties/Distances/Modèle Dis par PCS42S, pg1.png") |>
  image_rotate(270) |>
  image_write("Sorties/Distances/Modèle Dis par PCS42S, pg1.png")
image_read("Sorties/Distances/Modèle Dis par PCS42S, pg2.png") |>
  image_rotate(270) |>
  image_write("Sorties/Distances/Modèle Dis par PCS42S, pg2.png")
image_read("Sorties/Distances/Modèle Dis par PCS42S, pg3.png") |>
  image_rotate(270) |>
  image_write("Sorties/Distances/Modèle Dis par PCS42S, pg3.png")

# Modèle de référence, en remplaçant ZoneDens par km par rapport au centre
sortie("Distances/Distance parcourue journée travail, modèle, km du centre")
regressionLog(base = base,
              val = "Dis", formule =  "PCS8 + Activ + disCentre + ZoneRang + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig())
off()

sortie("Distances/Distance parcourue journée travail, modèle, statut d'occupation", taille = "page")
regressionLog(base = filter(base, LogOcc %in% c("10", "20", "21")),
              val = "Dis", formule =  "Activ + dsDomEtq + Genre + Age10 + LogOcc",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(), colComparaison = "PCS8")
off()

image_read("Sorties/Distances/Distance parcourue journée travail, modèle, statut d'occupation.png") |>
  image_rotate(270) |>
  image_write("Sorties/Distances/Distance parcourue journée travail, modèle, statut d'occupation.png")

sortie("Distances/Distance parcourue journée travail, modèles alternatifs", format = "pdf",
       taille = "a4", portrait = T)
regressionLog(base = base,
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = base,
              val = "Dis", formule =  "ZoneDens + ZoneRang + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = base,
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = base,
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens_travMax + ZoneRang + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = filter(base, ZoneDens=="1"),
              val = "Dis", formule =  "PCS8 + Activ + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), travailleur⋅ses vivant en commune dense", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = filter(base, ZoneDens=="3"),
              val = "Dis", formule =  "PCS8 + Activ + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), travailleur⋅ses vivant en commune peu dense", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = base,
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens + uid_ENQ.f + Genre + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))

ref = ml("prof. interm., vivant et travaillant en commune urbaine à temps plein",
         "dans une AAV de plus de 700.000 hab., 40 à 49 ans")
regressionLog(base = mutate(base, Genre = etqGenre(Genre)), colComparaison = "Genre",
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = mutate(base, Genre = etqGenre(Genre)), colComparaison = "Genre",
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens + ZoneRang + Age10",
              poids="CoeffRecEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig())
regressionLog(base = mutate(base, PCS8 = etqPCS8(PCS8)), colComparaison = "PCS8",
              val = "Dis", formule =  "Activ + Genre + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = mutate(base, PCS42S = etqPCS42S(PCS42S)), colComparaison = "PCS42S",
              val = "Dis", formule =  "Activ + Genre + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = mutate(base, Genre = etqGenre(Genre)), colComparaison = "Genre",
              val = "Dis", formule =  "PCS42S + Activ + ZoneDens + ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), tou⋅tes les travailleur⋅ses", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = filter(base, Genre == "H"),
              val = "Dis", formule = "PCS8 + Activ + ZoneDens+ ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), hommes", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
regressionLog(base = filter(base, Genre == "F"),
              val = "Dis", formule =  "PCS8 + Activ + ZoneDens+ ZoneRang + Age10",
              poids="poidsZero", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
              titre = "Distance estimée (journée de travail), femmes", unite = "km",
              imprDistrib = F, refDescr = ref, caption = src_fig(date = "janvier 2023"))
remove(base)
off()

# ~ Boucle journée de travail ====

# Sous-ensemble de PER correspondant à ces PCS, un jour travaillé
PER_trav = filter(valref(PER_ff), PCS42S %in% PCS_privé)
PER_trav$JoDeb = heureHHMMtoM(PER_trav$JoDeb) ; PER_trav$JoFin = heureHHMMtoM(PER_trav$JoFin) 

sortie("Distances/Distance parcourrue journée de travail, PCS du privé", format = "pdf")
regressionLog(base = PER_trav,
              val = "DisOk", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance parcourue (km)",
              titre = "Modélisation de la distance \"parcourue\" en véhicule (travailleur⋅ses)", unite = "km",
              imprDistrib = F) %>% summary()
regressionLog(base = PER_trav,
              val = "DisOk", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance parcourue (km)",
              titre = "Modélisation de la distance \"parcourue\" en véhicule (travailleur⋅ses)", unite = "km",
              imprDistrib = F) %>% summary()
remove(PER_trav)
off()

# ~ Modèle prédictif (for the lols) ====

rapport("Modèle prédictif IDF")

perParMen = PER %>% filter(Age>16) %>% group_by(uid_MEN) %>% summarise(per16ParMen = n())

load("Data/MEN.rds")

MEN = left_join(MEN, perParMen, by="uid_MEN")

load("Data/shp_COM.rds") ; load("Data/shp_ZF.rds")

baseIdf = filter(valref(PER_ff), uid_ENQ == "IDF2010") %>%
  mutate(Age = (Age - 16)/10) %>%
  left_join(y = rename(centroidesAAV(), disCentre = dis), by = "ZF") %>%
  mutate(disCentre = disCentre/10000) %>%
  mutate(nivDip = NivEtuVersNivDip(NivEtu)) %>%
  left_join(select(MEN, uid_MEN, VehN, per16ParMen), by="uid_MEN") %>%
  mutate(voituresParAdulte = VehN/per16ParMen)

remove(shp_COM, shp_ZF, MEN)

modIdf = regressionLog(base = baseIdf,
                       val = "Dis", formule =  "PCS42S * Activ * Genre * nivDip + disCentre + Age + Permis * AboTC + MenEnfants + voituresParAdulte + EnqDate_JS",
                       poids="CoeffEnq", retirerZ = T, facteurDiv = 1000, legVal = "distance estimée (km)",
                       titre = "Distance estimée (journée de travail),\ntou⋅tes les travailleur⋅ses", unite = "km",
                       imprDistrib = F, refDescr = ref, caption = src_fig(baseIdf))

sujet = rbind(tibble(uid_PER = "MAXIME",
                     PCS42S = "32", # Profession intellecutelle supérieure
                     Activ = "10", # en emploi
                     disCentre = .57, # distance entre dom et centroïde Paris ne km/10
                     Genre = "H",
                     Age = 1.1, # années éoculées depuis ses 16 ans / 10
                     nivDip = "4", # dipl sup
                     Permis = "1", # a le permis
                     MenEnfants = "FALSE",
                     voituresParAdulte = 0, # n'a pas de voiture
                     AboTC = "4", # n'a pas d'abo tspt
                     EnqDate_JS = "ven"))
predict.lm(object = modIdf, newdata = sujet) %>% exp()

# Comparaison par genre =====

rapport("Analyse des mobilités par genre", prim = T)

PER %>%
  filter(PCS42S %in% as.character(c(20:68))) %>%
  group_by(Genre) %>% summarise(m = mean(Travail_Dis, na.rm=T))

PER %>%
  filter(PCS42S %in% as.character(c(20:68))) %>%
  group_by(ZoneDens, Genre) %>% summarise(m = mean(Travail_Dis, na.rm=T))

PER_ff |>
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq), !is.na(dsDom), !is.na(Travail_Dis)) |>
  mutate(dsDom = discretisation(dsDom, methode = "quartiles")) |>
  group_by(dsDom, Genre) |> summarise(m = weighted.mean(Travail_Dis, CoeffRecEnq)) |>
  pivot_wider(names_from = Genre, values_from = m, names_prefix = "Genre") |>
  mutate(ecart = 1 - GenreF/GenreH)

PER_ff |>
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq), !is.na(Travail_Dis)) |>
  group_by(PCS42S, Genre) |> summarise(m = weighted.mean(Travail_Dis, CoeffRecEnq)) |>
  pivot_wider(names_from = Genre, values_from = m, names_prefix = "Genre") |>
  mutate(ecart = 1 - GenreF/GenreH) |>
  tab_Tri(parCol = "ecart", rev = T) |>
  mutate(PCS42S = etqPCS42S(PCS42S))

PER_ff |>
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq), !is.na(Dis)) |>
  group_by(PCS42S, Genre) |> summarise(m = weighted.mean(Dis, CoeffRecEnq)) |>
  pivot_wider(names_from = Genre, values_from = m, names_prefix = "Genre") |>
  mutate(ecart = 1 - GenreF/GenreH) |>
  tab_Tri(parCol = "ecart", rev = T) |>
  mutate(PCS42S = etqPCS42S(PCS42S))

PER_ff |>
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq), !is.na(Dis)) |>
  group_by(PCS42S, Genre) |> summarise(nb = sum(CoeffRecEnq)) |>
  pivot_wider(names_from = Genre, values_from = nb, names_prefix = "Genre") |>
  mutate(pF = GenreF / sum(GenreH + GenreF)) |>
  tab_Tri(parCol = "pF", rev = T) |>
  mutate(PCS42S = etqPCS42S(PCS42S))

sortie("Distances/Distance par Genre et PCS", taille = "carré")
PER_ff %>%
 # filter(PCS42S %in% as.character(c(20:68))) %>%
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq)) |>
  mutate(Genre = etqGenre(Genre),
         PCS42S = etqPCS42S(PCS42S, rev = T),
         ZoneDens = discretisation(dsDom, methode = "quartiles"),
         PCS8 = etqPCS8(PCS8)) %>%
  filter(!is.na(ZoneDens)) |>
  group_by(ZoneDens, PCS42S, Genre) %>%
  summarise(disLTvl = weighted.mean(Travail_Dis, w = CoeffRecEnq, na.rm=T), n = n(),
            PCS8 = first(na.omit(PCS8))) %>%
  #filter(n>seuilSignifiant) %>%
  select(-n) %>%
  pivot_wider(names_from = "Genre", values_from = "disLTvl") %>%
  mutate(ecart = (Femme/Homme - 1) * 100) %>%
  pivot_longer(cols = c("Homme","Femme"), names_to = "Genre", values_to = "disLTvl") %>%
  ungroup() |>
  mutate(ZoneDens = factor(ZoneDens, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile\nde densité"))) |>
  ggplot(aes(x = PCS42S, y = disLTvl/1000)) +
  geom_col(aes(colour = Genre), position = "dodge", alpha=1, fill="grey") +
  scale_colour_hue(breaks=c("Homme","Femme"), name = "genre") +
  geom_label(aes(label = paste0(ifelse(ecart>0, "+", ""),
                                round(ecart,1), " %"), y = 40,
                 fill = ecart), hjust=1, size=2.5) +
  scale_fill_gradient2(name = "écart\nfemmes/\nhommes\n (en %)") +
  coord_flip(ylim = c(0,40)) +
  facet_grid(PCS8~ZoneDens, scales = "free", space = "free_y") +
  ylab("Distance moyenne au lieu de travail (km)") + xlab("PCS du / de la travailleuse") +
  labs(title = "Distance au lieu de travail selon la PCS et le genre",
       subtitle = "Travailleur⋅ses effectuant moins de 166 km/jour",
       caption = src_fig(bu = T, emp = T)) +
  theme(legend.position = "bottom")
off()

PER_ff %>%
  # filter(PCS42S %in% as.character(c(20:68))) %>%
  filter(!is.na(PCS42S), !is.na(CoeffRecEnq)) |>
  mutate(Genre = etqGenre(Genre),
         PCS42S = etqPCS42S(PCS42S, rev = T),
         ZoneDens = discretisation(dsDom, methode = "quartiles")) %>%
  filter(!is.na(ZoneDens)) |>
  group_by(ZoneDens, PCS42S, Genre) %>%
  summarise(disLTvl = weighted.mean(Dis, w = CoeffRecEnq, na.rm=T), n = n()) %>%
  filter(n>seuilSignifiant) %>%
  select(-n) %>%
  pivot_wider(names_from = "Genre", values_from = "disLTvl") %>%
  mutate(ecart = (Femme/Homme - 1) * 100) %>%
  pivot_longer(cols = c("Homme","Femme"), names_to = "Genre", values_to = "disLTvl") %>%
  ungroup() |>
  mutate(ZoneDens = factor(ZoneDens, labels = paste0(c("1er", "2e", "3e", "4"), " quartile\nde densité"))) |>
  ggplot(aes(x = PCS42S, y = disLTvl/1000)) +
  geom_col(aes(fill = PCS42S, color = Genre), position = "dodge", alpha=.8) +
  scale_fill_manual(values = rev(pal_PCS42S[1:18]), breaks=NULL) +
  scale_color_hue(breaks=c("Homme","Femme"), name = "genre") +
  new_scale_fill() +
  geom_label(aes(label = paste0(ifelse(ecart>0, "+", ""),
                                round(ecart,1), " %"), y = 40,
                 fill = ecart), hjust=1, size=2.5) +
  scale_fill_gradient2(name = "écart\nfemmes/\nhommes\n (en %)") +
  coord_flip() +
  facet_grid(~ZoneDens) +
  ylab("Distance moyenne parcourue (km)") + xlab("PCS du / de la travailleuse") +
  labs(title = "Distance parcourue selon la PCS et le genre",
       subtitle = "Travailleur⋅ses effectuant moins de 166 km/jour",
       caption = src_fig(bu = T, emp = T)) +
  theme(legend.position = "bottom")


# Duncan par genre, Com
t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  group_by(Travail_Com) %>%
  summarise(Tvl_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Tvl_n = n())
t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(Com) %>%
  summarise(Dom_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Dom_n = n()) %>%
  left_join(t1, by=c("Com" = "Travail_Com")) %>%
  filter(Dom_n>=seuilSignifiant & Tvl_n>=seuilSignifiant) %>%
  group_by(tout = T) %>% summarise(duncanTvl = indexDuncan(Tvl_nHo, Tvl_n),
                                   duncanDom = indexDuncan(Dom_nHo, Dom_n))

# Duncan par genre, ZF
t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  group_by(Travail_ZF) %>%
  summarise(Tvl_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Tvl_n = n())
t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_ZF)) %>%
  group_by(ZF) %>%
  summarise(Dom_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Dom_n = n()) %>%
  left_join(t1, by=c("ZF" = "Travail_ZF")) %>%
  filter(Dom_n>=seuilSignifiant/2 & Tvl_n>=seuilSignifiant/2) %>%
  group_by(tout = T) %>% summarise(duncanTvl = indexDuncan(Tvl_nHo, Tvl_n),
                                   duncanDom = indexDuncan(Dom_nHo, Dom_n))


# Duncan par genre, Com + PCS
t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  group_by(PCS8, Travail_Com) %>%
  summarise(Tvl_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Tvl_n = n())
t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(PCS8, Com) %>%
  summarise(Dom_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Dom_n = n()) %>%
  left_join(t1, by=c("Com" = "Travail_Com", "PCS8" = "PCS8")) %>%
  filter(Dom_n>=seuilSignifiant & Tvl_n>=seuilSignifiant) %>%
  group_by(PCS8) %>% summarise(duncanTvl = indexDuncan(Tvl_nHo, Tvl_n),
                               duncanDom = indexDuncan(Dom_nHo, Dom_n)) %>%
  tab_Tri("PCS8")


indexDuncan(populationA = t$nHo, populationTotale = t$n)

t = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  group_by(Travail_ZT) %>%
  summarise(nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            n = n()) %>%
  filter(n>=seuilSignifiant)


t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_ZT)) %>%
  group_by(PCS8, Travail_ZT) %>%
  summarise(nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            n = n()) %>%
  filter(n>=seuilSignifiant) %>%
  group_by(PCS8) %>% summarise(duncanTvl = indexDuncan(nHo, n)) %>%
  tab_Tri("PCS8")

t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_ZT)) %>%
  group_by(PCS8, ZT) %>%
  summarise(nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            n = n()) %>%
  filter(n>=seuilSignifiant) %>%
  group_by(PCS8) %>% summarise(duncanDom = indexDuncan(nHo, n)) %>%
  tab_Tri("PCS8") %>%
  left_join(t1, by="PCS8")

t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(PCS8, Travail_Com) %>%
  summarise(nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            n = n()) %>%
  filter(n>=seuilSignifiant) %>%
  group_by(PCS8) %>% summarise(duncanTvl = indexDuncan(nHo, n)) %>%
  tab_Tri("PCS8")

t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(PCS8, Com) %>%
  summarise(nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            n = n()) %>%
  filter(n>=seuilSignifiant) %>%
  group_by(PCS8) %>% summarise(duncanDom = indexDuncan(nHo, n)) %>%
  tab_Tri("PCS8") %>%
  left_join(t1, by="PCS8")

# nombre de gens et d'emplois par commune
t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(Travail_Com) %>%
  summarise(Tvl_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Tvl_nFe = sum(ifelse(Genre == "Femme", 1, 0)),
            Tvl_n = n())
t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(Com) %>%
  summarise(Dom_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Dom_nFe = sum(ifelse(Genre == "Femme", 1, 0)),
            Dom_n = n()) %>%
  left_join(t1, by=c("Com" = "Travail_Com")) %>%
  filter(Dom_n>=seuilSignifiant & Tvl_n>=seuilSignifiant) %>%
  mutate(txHo = Tvl_nHo / Dom_nHo, txFe = Tvl_nFe / Dom_nFe) %>%
  group_by(tout = T) %>%
  summarise(devHo = mean(abs(txHo / mean(txHo) - 1)),
            devFe = mean(abs(txFe / mean(txFe) - 1)))


t1 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(PCS8, Travail_Com) %>%
  summarise(Tvl_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Tvl_nFe = sum(ifelse(Genre == "Femme", 1, 0)),
            Tvl_n = n())
t2 = PER_ff %>%
  mutate(Genre = etqGenre(Genre)) %>%
  filter(!is.na(Travail_Com)) %>%
  group_by(PCS8, Com) %>%
  summarise(Dom_nHo = sum(ifelse(Genre == "Homme", 1, 0)),
            Dom_nFe = sum(ifelse(Genre == "Femme", 1, 0)),
            Dom_n = n()) %>%
  left_join(t1, by=c("Com" = "Travail_Com", "PCS8" = "PCS8")) %>%
  filter(Dom_n>=seuilSignifiant/2 & Tvl_n>=seuilSignifiant/2) %>%
  mutate(txHo = Tvl_nHo / Dom_nHo, txFe = Tvl_nFe / Dom_nFe) %>%
  filter(!is.infinite(txHo), !is.infinite(txFe),
         !is.na(txHo), !is.na(txFe)) %>%
  group_by(PCS8) %>%
  summarise(devHo = mean(abs(txHo / mean(txHo) - 1)),
            devFe = mean(abs(txFe / mean(txFe) - 1))) %>%
  tab_Tri("PCS8")


PER_ff %>% 
  filter(!is.na(Travail_Dis),
         Age>15 & Age <70,
         Activ %in% c("10", "11"),
         PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0",
                             MenEnfants == T & MenBebe == F ~ "1",
                             MenEnfants == T & MenBebe == T ~ "2"),
  ) %>%
  regressionLog(val = "Dis",
                formule = "Age10 + PCS8 + ZoneDens + Activ + MenType",
                colComparaison = "Genre",
                retirerZ = T,
                facteurDiv = 1000,
                titre = "Modélisation de la distance domicile-travail\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(date = "février 2023"))

PER_ff %>% 
  filter(!is.na(Travail_Dis),
         Age>15 & Age <70,
         Activ %in% c("10", "11"),
         PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0. Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "1. Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "2. Jeunes enfants"),
         MenType = as.factor(MenType)) %>%
  regressionLog(val = "Travail_Dis",
                formule = "Age10 + Genre + PCS8 + ZoneDens + Activ",
                colComparaison = "MenType",
                retirerZ = T,
                facteurDiv = 1000,
                titre = "Modélisation de la distance domicile-travail\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(date = "février 2023"))

sortie("Distances/Distance DT présence d'enfants", taille = "carré")
g = PER_ff %>% 
  valref() |>
  filter(!is.na(Dis)) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0. Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "1. Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "2. Jeunes enfants"),
         MenType = as.factor(MenType)) %>%
  regressionLog(val = "Travail_Dis",
                formule = "Age10 + Genre + PCS8 + dsDomEtq + Activ",
                colComparaison = "MenType", colCompNt = T,
                retirerZ = T,
                facteurDiv = 1000,
                titre = "Modélisation de la distance domicile-travail\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(emp = F), returnFig = T)

g = g + theme_bw(base_size = 8)
print(g)
off()

g = PER_ff %>% 
  valref() |>
  filter(!is.na(Dis)) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0. Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "1. Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "2. Jeunes enfants"),
         MenType = as.factor(MenType)) %>%
  regressionLog(val = "Travail_Dis",
                formule = "Age10 + Genre + PCSMT + dsDomEtq + Activ",
                colComparaison = "MenType", colCompNt = T,
                retirerZ = T,
                facteurDiv = 1000,
                titre = "Modélisation de la distance domicile-travail\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(emp = F), returnFig = T)

sortie("Distances/Distance selon présence d'enfants", taille = "carré")
PER_ff %>% 
  valref() |>
  filter(!is.na(Dis)) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0. Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "1. Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "2. Jeunes enfants"),
         MenType = as.factor(MenType)) %>%
  regressionLog(val = "Dis",
                formule = "Age10 + Genre + PCS8 + dsDomEtq + Activ",
                colComparaison = "MenType", colCompNt = T,
                retirerZ = T,
                facteurDiv = 1000,
                titre = "Modélisation de la distance domicile-travail\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(emp = F))
off()



sortie("Distances/Temps en déplacement selon présence d'enfants", taille = "carré")
PER_ff %>% 
  valref() |>
  filter(!is.na(Tps)) |>
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "0. Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "1. Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "2. Jeunes enfants"),
         MenType = as.factor(MenType)) %>%
  regressionLog(val = "Tps",
                formule = "Age10 + Genre + PCS8 + ZoneDens + Activ",
                colComparaison = "MenType", colCompNt = T,
                retirerZ = T,
                facteurDiv = 60,
                titre = "Modélisation du temps de navette\nselon la présence d'enfants",
                imprDistrib = T, caption = src_fig(emp = T))
off()

sortie("Distances/Distance selon présence d'enfants et commune", taille = "page", portrait = T)
PER_ff %>%
  mutate(dsDom = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
         dsTvl = discretisation(dsTvl, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile"))) |>
  filter(!is.na(PCS42S),
         !is.na(dsDom), !is.na(dsTvl),
         !is.na(Dis)) %>%
  filter(typoJo == "TRAV" & Genre == "F" & Age > 25 & Age < 45) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T)) %>%
  mutate(MenType = case_when(MenEnfants == F & MenBebe == F ~ "Pas d'enfants",
                             MenEnfants == T & MenBebe == F ~ "Enfants > 3 ans",
                             MenEnfants == T & MenBebe == T ~ "Jeunes enfants"),
         MenType = factor(MenType, levels=c("Pas d'enfants", "Enfants > 3 ans", "Jeunes enfants"))) %>%
  ggplot(aes(x = Dis/1000, y = PCS42S)) +
  geom_boxplot(aes(fill = PCS42S, colour = MenType), outlier.shape = NA, alpha=.8) +
  scale_fill_manual(values = rev(pal_PCS42S), name = "PCS", breaks=NULL) +
  scale_color_manual(values = c("firebrick", "paleturquoise", "slateblue"), name = "Présence d'enfants") +
  coord_cartesian(xlim = c(0, 200)) +
  facet_grid(~dsDom) +
  xlab("Distance totale (km)") + ylab("PCS de l'enquêté·e") +
  labs(title = "Distance totale parcourue (estimée) selon la PCS de l'enquêtée",
       caption=src_fig(emp = F)) +
  theme_bw() + theme(legend.position = "bottom")
off()

# Discrepancies au sein des ménages ============================================

rapport("Analyse des écarts femme/homme au sein des ménages", prim = T)
load("Data/MEN.rds")

# Création d'une table représentant les couples H/F et les caractéristiques H/F
seuilFiltre = 100
rapport("Seuil filtre :", seuilFiltre, info = T)

conjH = PER_ff %>% filter(Lien == "1" | Lien == "2") %>%
  filter(Genre == "H")
conjF = PER_ff %>% filter(Lien == "1" | Lien == "2") %>%
  filter(Genre == "F")

colnames(conjH) = paste0(colnames(conjH), ".H")
colnames(conjF) = paste0(colnames(conjF), ".F")

couples = left_join(select(MEN, uid_MEN), conjH, by = c("uid_MEN" = "uid_MEN.H")) %>%
  left_join(conjF, by = c("uid_MEN" = "uid_MEN.F")) %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  left_join(select(MEN, uid_MEN, MenEnfants), by=c("uid_MEN" = "uid_MEN")) %>%
  mutate(ActivCpl = paste("H", etqActiv(Activ.H, num=F), " & F", etqActiv(Activ.F, num=F)))

remove(conjH, conjF)

cor.test(couples$Dis.H,      couples$Dis.F)
cor.test(couples$Tps.H,      couples$Tps.F)
cor.test(couples$N.H,        couples$N.F)
cor.test(couples$mDis.H,     couples$mDis.F)
cor.test(couples$pDis_VOI.H, couples$pDis_VOI.F)
cor.test(couples$pDis_VEL.H, couples$pDis_VEL.F)

# Corrélations propriétés du couple

compCorVar = function(table, colonne)
{
  varTest = colnames(table)[colnames(table) == colonne]
  colnames(table)[colnames(table) == colonne] = "colonne"
  
  table = table %>% group_by(colonne) %>% filter(!is.na(Tps.H) & !is.na(Tps.F)) %>%
    mutate(correlDis  = cor(Dis.H, Dis.F), correlTps = cor(Tps.H, Tps.F),
           correlN = cor(N.H, N.F)) %>%
    summarize(corDis = first(correlDis), corTps = first(correlTps), corNbr = first(correlN),
              rapDis = weighted.mean(Dis.H, CoeffEnq.H)/weighted.mean(Dis.F, CoeffEnq.F) - 1,
              rapTps = weighted.mean(Tps.H, CoeffEnq.H)/weighted.mean(Tps.F, CoeffEnq.F) - 1,
              rapNbr = weighted.mean(N.H, CoeffEnq.H, na.rm=T)  /
                weighted.mean(N.F, CoeffEnq.F, na.rm=T)   - 1,
              n = n()) %>%
    filter(n > seuilFiltre) %>%
    pivot_longer(corDis:rapNbr, names_to = "variable", values_to = "valeurs") %>%
    mutate(msr = substr(variable, 1,3),
           var = substr(variable, 4,6)) %>% select(-variable) %>%
    pivot_wider(names_from = msr, values_from = valeurs)
  
  table$variable = plyr::revalue(table$var, c("Dis" = "Distance parcourue",
                                              "Tps" = "Durée de dépl.",
                                              "Nbr"   = "Nombre de dépl."))
  nTot = sum(filter(table, variable == "Distance parcourue")$n)
  
  couleurs = palListe[[varTest]]
  
  g = table %>% ggplot(aes(x = colonne, y = cor)) +
    geom_bar(aes(fill = rap), stat = "identity") +
    scale_fill_gradient2(low = "purple", high = "brown", mid = "grey", midpoint = 0,
                         name = "écart moyen\nentre H et F\nx 100%") +
    coord_flip() + scale_x_discrete(limits = rev(levels(table$colonne))) +
    facet_grid(~variable) +
    xlab(etq(varTest)) + ylab("Coeff. de corrélation (Pearson)") +
    theme_bw()
  
  return(g)
}

g1 = compCorVar(couples, "PCSMLT.F") + labs(title="selon la PCS Ménage")
g2 = compCorVar(couples, "Age5.F")  + labs(title="selon la classe d'âge de la femme")
g3 = compCorVar(couples, "ActivCpl") + labs(title="selon l'activité des membres du ménage")
g4 = compCorVar(couples, "ZoneDens.F") + labs(title="selon la densité de la com. de résidence")

g5 = compCorVar(filter(couples, Age.H %in% c(16:64) & Age.F %in% c(16:64)), "PCSMLT.F") +
  labs(title="selon la PCS Ménage, entre 16 et 64 ans")
g6 = compCorVar(filter(couples, Age.H %in% c(16:64) & Age.F %in% c(16:64)), "MenEnfants") +
  labs(title="selon la présence ou non d'enfants dans le ménage entre 16 et 64 ans")
g7 = compCorVar(filter(couples, MenEnfants == F), "Age5.F") +
  labs(title="selon la classe d'âge de la femme, dans des ménages sans enfants")

p1 = cowplot::plot_grid(g1, g2, ncol = 1, nrow = 2, align = "v", axis = "lr") %>%
  viz_Titre("Corrélation entre les déplacements des hommes et des femmes")
p2 = cowplot::plot_grid(g3, g4, ncol = 1, nrow = 2, align = "v", axis = "lr") %>%
  viz_Titre("Corrélation entre les déplacements des hommes et des femmes")
p3 = cowplot::plot_grid(g5, g6, g7, ncol = 1, nrow = 3, align = "v", axis = "lr",
                        rel_heights = c(.4,.2,.4)) %>%
  viz_Titre("Corrélation entre les déplacements des hommes et des femmes")

sortie("Distances/Ménages, comparaison des distances", format = "pdf", taille = "a4", portrait = T)
print(p1)
print(p2)
print(p3)
off()

sortie("Distances/Ménages, plus longue distance", taille = "carré", portrait = T)
PER_ff |>
  group_by(uid_MEN) %>%
  summarise(disHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", Dis, NA))),
            disFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", Dis, NA))),
            PCSHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", as.character(PCS8), NA))),
            PCSFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", as.character(PCS8), NA)))) %>%
  filter(!is.na(disHo) & !is.na(disFe)) %>%
  mutate (  PCSHo = etqPCS8(PCSHo, genre = "H"),
            PCSFe = etqPCS8(PCSFe, genre = "F"),
            PCSHo = paste0("Homme\n", PCSHo),
            PCSFe = paste0("Femme\n", PCSFe)) %>%
  mutate(compar = case_when(disHo > disFe ~ "l'homme",
                            disFe > disHo ~ "la femme",
                            disHo == disFe ~"aucun·e")) %>%
  group_by(PCSHo, PCSFe) %>% summarise(nHo = sum(ifelse(compar == "l'homme",1,0)),
                                       nFe = sum(ifelse(compar == "la femme",1,0)),
                                       nEg = sum(ifelse(compar == "aucun·e",1,0))) %>%
  pivot_longer(cols = c("nHo","nFe","nEg"), names_to = "qui", values_to = "n") %>%
  mutate(qui = plyr::revalue(qui, c("nEg" = "aucun·e", "nFe" = "la femme", "nHo" = "l'homme")),
         qui = factor(qui, levels = c("la femme", "aucun·e", "l'homme"))) %>%
  group_by(PCSHo, PCSFe) %>% mutate(p = n / sum(n) * 100) %>%
  mutate(PCSHo = factor(PCSHo, levels = paste0("Homme\n", unique(niv_PCS8_H))),
         PCSFe = factor(PCSFe, levels = paste0("Femme\n", unique(niv_PCS8_F)))) %>%
  ggplot(aes(x = 1, y = p)) +
  geom_col(aes(fill = qui), position="stack") +
  geom_text(aes(y = ifelse(qui == "la femme", 80, ifelse(qui == "l'homme", 20, NA)),
                label = paste0(round(p,1), " %"), x=1), hjust=.5, size=2.5) +
  geom_hline(yintercept = 50, linetype=3) +
  scale_fill_manual(values = c("coral", "grey90", "thistle"), name=NULL) +
  facet_grid(PCSHo~PCSFe) +
  ylab("part des enquêté·es") +
  labs(title = "Qui parcourt la plus longue distance dans le ménage ?",
       subtitle = ml("Ménages dont les personnes de référence forment un couple hétérosexuel",
                     "et dont les deux membres se sont rendu·es au travail"),
       caption = src_fig()) +
  theme_minimal(base_size = 8) + ggRetirerAxeX + theme(legend.position = "top") %>% print()
off()

sortie("Distances/Ménages, plus longue distance théorique", taille = "carré", portrait = T)
PER_ff |>
  group_by(uid_MEN) %>%
  summarise(disHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", Travail_Dis, NA))),
            disFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", Travail_Dis, NA))),
            PCSHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", as.character(PCS8), NA))),
            PCSFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", as.character(PCS8), NA)))) %>%
  filter(!is.na(disHo) & !is.na(disFe)) %>%
  mutate (  PCSHo = etqPCS8(PCSHo, genre = "H"),
            PCSFe = etqPCS8(PCSFe, genre = "F"),
            PCSHo = paste0("Homme\n", PCSHo),
            PCSFe = paste0("Femme\n", PCSFe)) %>%
  mutate(compar = case_when(disHo > disFe ~ "l'homme",
                            disFe > disHo ~ "la femme",
                            disHo == disFe ~"aucun·e")) %>%
  group_by(PCSHo, PCSFe) %>% summarise(nHo = sum(ifelse(compar == "l'homme",1,0)),
                                       nFe = sum(ifelse(compar == "la femme",1,0)),
                                       nEg = sum(ifelse(compar == "aucun·e",1,0))) %>%
  pivot_longer(cols = c("nHo","nFe","nEg"), names_to = "qui", values_to = "n") %>%
  mutate(qui = plyr::revalue(qui, c("nEg" = "aucun·e", "nFe" = "la femme", "nHo" = "l'homme")),
         qui = factor(qui, levels = c("la femme", "aucun·e", "l'homme"))) %>%
  group_by(PCSHo, PCSFe) %>% mutate(p = n / sum(n) * 100) %>%
  mutate(PCSHo = factor(PCSHo, levels = paste0("Homme\n", unique(niv_PCS8_H))),
         PCSFe = factor(PCSFe, levels = paste0("Femme\n", unique(niv_PCS8_F)))) %>%
  ggplot(aes(x = 1, y = p)) +
  geom_col(aes(fill = qui), position="stack") +
  geom_text(aes(y = ifelse(qui == "la femme", 80, ifelse(qui == "l'homme", 20, NA)),
                label = paste0(round(p,1), " %"), x=1), hjust=.5, size=2.5) +
  geom_hline(yintercept = 50, linetype=3) +
  scale_fill_manual(values = c("coral", "grey90", "thistle"), name=NULL) +
  facet_grid(PCSHo~PCSFe) +
  ylab("part des enquêté·es") +
  labs(title = "Qui vit le plus loin de son lieu de travail habituel ?",
       subtitle = ml("Ménages dont les personnes de référence forment un couple hétérosexuel"),
       caption = src_fig(date = "février 2023")) +
  theme_minimal(base_size = 8) + ggRetirerAxeX + theme(legend.position = "top")
off()

# mettre filtre pas d'agriculteurs sur PER_ff sinon c'est la D

g1 = PER %>%
  group_by(uid_MEN) %>%
  summarise(
    disHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", Dis, NA))),
    disFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", Dis, NA))),
    PCSHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", as.character(PCS8), NA))),
    PCSFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", as.character(PCS8), NA))),
    uidHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", uid_PER, NA))),
    uidFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", uid_PER, NA))),
    enfants = ifelse(min(Age) < 7, "oui","non")
  ) %>%
  filter(!is.na(disHo) & !is.na(disFe)) %>%
  filter(uidHo %in% filter(PER_ff, PCS8 != "01")$uid_PER,
         uidFe %in% filter(PER_ff, PCS8 != "01")$uid_PER) |>
  mutate (  PCSHo = etqPCS8(PCSHo, genre = "H"),
            PCSFe = etqPCS8(PCSFe, genre = "F"),
            PCSHo = paste0("Homme\n", PCSHo),
            PCSFe = paste0("Femme\n", PCSFe)) %>%
  mutate(compar = case_when(disHo > disFe ~ "l'homme",
                            disFe > disHo ~ "la femme",
                            disHo == disFe ~"aucun·e")) %>%
  group_by(enfants, PCSHo, PCSFe) %>%
  summarise(nHo = sum(ifelse(compar == "l'homme",1,0)),
            nFe = sum(ifelse(compar == "la femme",1,0)),
            nEg = sum(ifelse(compar == "aucun·e",1,0))) %>%
  pivot_longer(cols = c("nHo","nFe","nEg"), names_to = "qui", values_to = "n") %>%
  mutate(qui = plyr::revalue(qui, c("nEg" = "aucun·e", "nFe" = "la femme", "nHo" = "l'homme")),
         qui = factor(qui, levels = c("la femme", "aucun·e", "l'homme"))) %>%
  group_by(enfants, PCSHo, PCSFe) %>% mutate(p = n / sum(n) * 100) %>%
  mutate(PCSHo = factor(PCSHo, levels = paste0("Homme\n", unique(niv_PCS8_H))),
         PCSFe = factor(PCSFe, levels = paste0("Femme\n", unique(niv_PCS8_F)))) %>%
  ggplot(aes(x = enfants, y = p)) +
  geom_col(aes(fill = qui), position="stack") +
  geom_text(aes(y = ifelse(qui == "la femme", 80, ifelse(qui == "l'homme", 20, NA)),
                label = paste0(round(p,1), " %")), hjust=.5, size=2.5) +
  geom_hline(yintercept = 50, linetype=3) +
  scale_fill_manual(values = c("coral", "grey90", "thistle"), name=NULL) +
  facet_grid(PCSHo~PCSFe) +
  ylab("part des enquêté·es") + xlab("présence d'enfants de moins de 7 ans") +
  labs(title = "Qui parcourt la plus longue distance dans le ménage ?",
       subtitle = ml("Ménages dont les personnes de référence forment un couple hétérosexuel",
                     "et dont les deux membres se sont rendu·es au travail"),
       caption = src_fig()) +
  theme_minimal(base_size = 8) + theme(legend.position = "top")

g2 = PER %>%
  group_by(uid_MEN) %>%
  summarise(
    TpsHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", Tps, NA))),
    TpsFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", Tps, NA))),
    PCSHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", as.character(PCS8), NA))),
    PCSFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", as.character(PCS8), NA))),
    uidHo = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "H", uid_PER, NA))),
    uidFe = first(na.omit(ifelse(Lien %in% c("1","2") & Genre == "F", uid_PER, NA))),
    enfants = ifelse(min(Age) < 7, "oui","non")
  ) %>%
  filter(!is.na(TpsHo) & !is.na(TpsFe)) %>%
  filter(uidHo %in% filter(PER_ff, PCS8 != "01")$uid_PER,
         uidFe %in% filter(PER_ff, PCS8 != "01")$uid_PER) |>
  mutate (  PCSHo = etqPCS8(PCSHo, genre = "H"),
            PCSFe = etqPCS8(PCSFe, genre = "F"),
            PCSHo = paste0("Homme\n", PCSHo),
            PCSFe = paste0("Femme\n", PCSFe)) %>%
  mutate(compar = case_when(TpsHo > TpsFe ~ "l'homme",
                            TpsFe > TpsHo ~ "la femme",
                            TpsHo == TpsFe ~"aucun·e")) %>%
  group_by(enfants, PCSHo, PCSFe) %>%
  summarise(nHo = sum(ifelse(compar == "l'homme",1,0)),
            nFe = sum(ifelse(compar == "la femme",1,0)),
            nEg = sum(ifelse(compar == "aucun·e",1,0))) %>%
  pivot_longer(cols = c("nHo","nFe","nEg"), names_to = "qui", values_to = "n") %>%
  mutate(qui = plyr::revalue(qui, c("nEg" = "aucun·e", "nFe" = "la femme", "nHo" = "l'homme")),
         qui = factor(qui, levels = c("la femme", "aucun·e", "l'homme"))) %>%
  group_by(enfants, PCSHo, PCSFe) %>% mutate(p = n / sum(n) * 100) %>%
  mutate(PCSHo = factor(PCSHo, levels = paste0("Homme\n", unique(niv_PCS8_H))),
         PCSFe = factor(PCSFe, levels = paste0("Femme\n", unique(niv_PCS8_F)))) %>%
  ggplot(aes(x = enfants, y = p)) +
  geom_col(aes(fill = qui), position="stack") +
  geom_text(aes(y = ifelse(qui == "la femme", 80, ifelse(qui == "l'homme", 20, NA)),
                label = paste0(round(p,1), " %")), hjust=.5, size=2.5) +
  geom_hline(yintercept = 50, linetype=3) +
  scale_fill_manual(values = c("coral", "grey90", "thistle"), name=NULL) +
  facet_grid(PCSHo~PCSFe) +
  ylab("part des enquêté·es") + xlab("présence d'enfants de moins de 7 ans") +
  labs(title = "Qui passe le plus de temps en déplacement dans le ménage ?",
       subtitle = ml("Ménages dont les personnes de référence forment un couple hétérosexuel",
                     "et dont les deux membres se sont rendu·es au travail"),
       caption = src_fig()) +
  theme_minimal(base_size = 8) + theme(legend.position = "top")

page = cowplot::plot_grid(g1 + labs(caption = ""), g2 + theme(legend.position = "none"), nrow = 2)

sortie("Distances/Ménage, comparaison distances", taille = "page", portrait = T)
print(page)
off()

# Code à mettre à jour ... ci dessous

# Coefficients de corrélation
hommes = filter(PER_ff, Lien %in% c("1", "2") & Genre == "H" & DuTvl > 0)
colnames(hommes) = paste0(colnames(hommes), "_H")
femmes = filter(PER_ff, Lien %in% c("1", "2") & Genre == "F" & DuTvl > 0)
colnames(femmes) = paste0(colnames(femmes), "_F")

MEN %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "H" & DuTvl > 0), by="uid_MEN") %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "F" & DuTvl > 0), suffix = c(".H", ".F"), by="uid_MEN") %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  filter(PCS8.H %in% c("01", "02", "03", "04", "05", "06"),
         PCS8.F %in% c("01", "02", "03", "04", "05", "06")) %>%
  select(uid_MEN, Dis.H, Dis.F, PCS8.H, PCS8.F) %>%
  group_by(PCS8.H, PCS8.F) %>% summarise(corr = cor(Dis.H, Dis.F), pval = cor.test(Dis.H, Dis.F)$p.value * 100,
                                         n = n()) 

MEN %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "H" & DuTvl > 0), by="uid_MEN") %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "F" & DuTvl > 0), suffix = c(".H", ".F"), by="uid_MEN") %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  filter(PCS8.H %in% c("01", "02", "03", "04", "05", "06"),
         PCS8.F %in% c("01", "02", "03", "04", "05", "06")) %>%
  select(uid_MEN, Dis.H, Dis.F, PCS8.H, PCS8.F) %>%
  group_by(PCS8.H) %>% summarise(corr = cor(Dis.H, Dis.F), pval = cor.test(Dis.H, Dis.F)$p.value,
                                 n = n())

MEN %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "H" & DuTvl > 0), by="uid_MEN") %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "F" & DuTvl > 0), suffix = c(".H", ".F"), by="uid_MEN") %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  filter(PCS8.H %in% c("01", "02", "03", "04", "05", "06"),
         PCS8.F %in% c("01", "02", "03", "04", "05", "06")) %>%
  select(uid_MEN, Dis.H, Dis.F, PCS8.H, PCS8.F) %>%
  group_by(PCS8.F) %>% summarise(corr = cor(Dis.H, Dis.F), pval = cor.test(Dis.H, Dis.F)$p.value,
                                 n = n())

MEN %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "H" & DuTvl > 0), by="uid_MEN") %>%
  left_join(filter(PER_ff, Lien %in% c("1","2") & Genre == "F" & DuTvl > 0), suffix = c(".H", ".F"), by="uid_MEN") %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  filter(PCS8.H %in% c("01", "02", "03", "04", "05", "06"),
         PCS8.F %in% c("01", "02", "03", "04", "05", "06")) %>%
  select(uid_MEN, Dis.H, Dis.F, PCS8.H, PCS8.F) %>%
  summarise(corr = cor(Dis.H, Dis.F), pval = cor.test(Dis.H, Dis.F)$p.value,
            n = n())

# Enfants et ménage ====

# On va constituer une base d'enfants, et leur attribuer 2 colonnes correspondant aux 2 parents
# on veut connaître → le statut, la PCS du parent, la distance, le temps à domicile

rapport("Analyse comparée au sein des ménages avec les enfants", prim = T)

enfants  = PER %>% filter(Age < 19, Lien == "3", EnqDate_JS != "3")

parents  = PER %>% filter(Lien %in% c("1", "2")) %>%
  select(uid_MEN, Lien, Genre, Activ, PCS8, PCS42S, Dis.V, DuDom)

parents1 = parents %>% filter(Lien == "1") %>% select(-Lien, -Genre) %>%
  rename(Activ_p1 = Activ, PCS8_p1 = PCS8, PCS42S_p1 = PCS42S, Dis_p1 = Dis.V, DuDom_p1 = DuDom)
parents2 = parents %>% filter(Lien == "2") %>% select(-Lien, -Genre) %>%
  rename(Activ_p2 = Activ, PCS8_p2 = PCS8, PCS42S_p2 = PCS42S, Dis_p2 = Dis.V, DuDom_p2 = DuDom)

peres    = parents %>% filter(Genre == "H") %>% select(-Lien, -Genre) %>%
  rename(Activ_P = Activ, PCS8_P = PCS8, PCS42S_P = PCS42S, Dis_P = Dis.V, DuDom_P = DuDom)
meres    = parents %>% filter(Genre == "F") %>% select(-Lien, -Genre) %>%
  rename(Activ_M = Activ, PCS8_M = PCS8, PCS42S_M = PCS42S, Dis_M = Dis.V, DuDom_M = DuDom)

enfants = enfants %>%
  left_join(parents1, by="uid_MEN") %>%
  left_join(parents2, by="uid_MEN") %>%
  left_join(peres,    by="uid_MEN") %>%
  left_join(meres,    by="uid_MEN")

# NB : si couple homo, la ligne de l'enfant est doublée et la corrélation évaluée avec chacun⋅e
#      des parents (j'ai vérifié)

enfants$Dis_Max   = apply(select(enfants, Dis_p1,   Dis_p2),   1, max)
enfants$Dis_Min   = apply(select(enfants, Dis_p1,   Dis_p2),   1, min)
enfants$DuDom_Max = apply(select(enfants, DuDom_p1, DuDom_p2), 1, max)
enfants$DuDom_Min = apply(select(enfants, DuDom_p1, DuDom_p2), 1, min)

diMax = cor.test(enfants$Dis.V, enfants$Dis_Max)
diMin = cor.test(enfants$Dis.V, enfants$Dis_Min)
diPeres = cor.test(enfants$Dis.V, enfants$Dis_P)
diMeres = cor.test(enfants$Dis.V, enfants$Dis_M)

duMax = cor.test(enfants$DuDom, enfants$DuDom_Max)
duMin = cor.test(enfants$DuDom, enfants$DuDom_Min)
duPeres = cor.test(enfants$DuDom, enfants$DuDom_P)
duMeres = cor.test(enfants$DuDom, enfants$DuDom_M)

# au sein de la fratrie : trop compliqué

# On en fait un graphique

corTab = tibble(ind = c(rep("Distance", times=4), rep("Durée au domicile", times=4)),
                var = rep(c("val. maximale", "val. minimale", "père", "mère"), times=2),
                val = c(diMax$estimate, diMin$estimate, diPeres$estimate, diMeres$estimate,
                        duMax$estimate, duMin$estimate, duPeres$estimate, duMeres$estimate),
                cf1 = c(diMax$conf.int[1], diMin$conf.int[1], diPeres$conf.int[1], diMeres$conf.int[1],
                        duMax$conf.int[1], duMin$conf.int[1], duPeres$conf.int[1], duMeres$conf.int[1]),
                cf2 = c(diMax$conf.int[2], diMin$conf.int[2], diPeres$conf.int[2], diMeres$conf.int[2],
                        duMax$conf.int[2], duMin$conf.int[2], duPeres$conf.int[2], duMeres$conf.int[2]))

g1 = corTab %>%
  ggplot(aes(x = var, y = val)) +
  geom_pointrange(aes(min = cf1, max=cf2)) +
  facet_wrap(~ind) +
  labs(title = "Corrélations observées entre les journées\ndes enfants et des parents",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  ylab("Coefficient de corrélation (Pearson)") + xlab("Parent") +
  coord_flip(ylim=c(0,1)) +
  theme_bw()

sortie("Distances/Parents et enfants, corrélation distances")
print(g1)
off()

ages = c(5:18)
corTab = tibble(age = integer(), ind = character(), var = character(),
                val = double(), cf1 = double(), cf2 = double(), df = double())

barre = ui_ProgInit(max(ages)-min(ages))

for(a in ages)
{
  enfants_a = filter(enfants, Age == a)
  
  diMax = cor.test(enfants_a$Dis.V, enfants_a$Dis_Max)
  diMin = cor.test(enfants_a$Dis.V, enfants_a$Dis_Min)
  diPeres = cor.test(enfants_a$Dis.V, enfants_a$Dis_P)
  diMeres = cor.test(enfants_a$Dis.V, enfants_a$Dis_M)
  
  duMax = cor.test(enfants_a$DuDom, enfants_a$DuDom_Max)
  duMin = cor.test(enfants_a$DuDom, enfants_a$DuDom_Min)
  duPeres = cor.test(enfants_a$DuDom, enfants_a$DuDom_P)
  duMeres = cor.test(enfants_a$DuDom, enfants_a$DuDom_M)
  
  corTabAge = tibble(age = rep(a, times=8),
                     ind = c(rep("Distance", times=4), rep("Durée au domicile", times=4)),
                     var = rep(c("la val. maximale", "la val. minimale", "le père", "la mère"), times=2),
                     val = c(diMax$estimate, diMin$estimate, diPeres$estimate, diMeres$estimate,
                             duMax$estimate, duMin$estimate, duPeres$estimate, duMeres$estimate),
                     cf1 = c(diMax$conf.int[1], diMin$conf.int[1], diPeres$conf.int[1], diMeres$conf.int[1],
                             duMax$conf.int[1], duMin$conf.int[1], duPeres$conf.int[1], duMeres$conf.int[1]),
                     cf2 = c(diMax$conf.int[2], diMin$conf.int[2], diPeres$conf.int[2], diMeres$conf.int[2],
                             duMax$conf.int[2], duMin$conf.int[2], duPeres$conf.int[2], duMeres$conf.int[2]),
                     df = c(diMax$parameter, diMin$parameter, diPeres$parameter, diMeres$parameter,
                            duMax$parameter, duMin$parameter, duPeres$parameter, duMeres$parameter))
  
  corTab = rbind(corTab, corTabAge)
  ui_Prog(barre = barre, i = a - min(ages))
}

g2 = corTab %>%
  ggplot(aes(x = age, y = val)) +
  geom_ribbon(aes(ymin = cf1, ymax = cf2, fill = var), alpha=.1) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  facet_wrap(~ind) +
  labs(title = "Corrélations observées entre les journées\ndes enfants et des parents",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  ylab("Coefficient de corrélation (Pearson)") + xlab("Âge de l'enfant") +
  scale_color_hue(name = "Corrélation avec") +
  theme_bw() + guides(fill = "none")

sortie("Distances/Parents et enfants, corrélation distances selon âge")
print(g2)
off()

sortie("Distances/Parents et enfants, corrélation distances selon âge et densité")
g3 = enfants %>%
  filter(Age > 4, !is.na(PCSMLT), !PCSMLT %in% c("7e", "7i")) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  group_by(PCSMLT, ZoneDens, Age) %>%
  summarize(Dis.V = weighted.mean(Dis.V, CoeffEnq, na.rm=T)/1000) %>%
  ggplot(aes(x = Age, y = Dis.V)) +
  geom_line(aes(color = PCSMLT)) +
  labs(title = "Distance moyenne dans la journée d'un enfant\npar PCS Ménage modifiée",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  xlab("Âge de l'enfant") + ylab("Distance moyenne dans la journée") +
  scale_color_manual(values = pal_PCSMT, name = "PCS Ménage") +
  facet_wrap(~ZoneDens) +
  theme_bw()
print(g3)
off()

g4 = enfants %>%
  filter(Age > 4, !is.na(PCSMLT), !PCSMLT %in% c("7e", "7i")) %>%
  group_by(PCSMLT, Age) %>%
  summarize(DuDom = weighted.mean(DuDom, CoeffEnq, na.rm=T)/60) %>%
  ggplot(aes(x = Age, y = DuDom)) +
  geom_line(aes(color = PCSMLT)) +
  labs(title = "Temps moyen au domicile dans la journée d'un enfant\npar PCS Ménage modifiée",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  xlab("Âge de l'enfant") + ylab("Temps moyen au domicile (heures)") +
  scale_color_manual(values = pal_PCSMT, name = "PCS Ménage") +
  theme_bw()

sortie("Distances/Enfants, temps moyen au domicile selon âge")
print(g4)
off()

# On recommence, en ne gardant que les parents qui travaillent !


enfants  = PER %>% filter(Age < 19, Lien == "3", EnqDate_JS != "3")

parents  = PER_ff %>% filter(Lien %in% c("1", "2")) %>%
  select(uid_MEN, Lien, Genre, Activ, PCS8, PCS42S, Dis.V, DuDom, typoModes)

parents1 = parents %>% filter(Lien == "1") %>% select(-Lien, -Genre) %>%
  rename(Activ_p1 = Activ, PCS8_p1 = PCS8, PCS42S_p1 = PCS42S,
         Dis_p1 = Dis.V, DuDom_p1 = DuDom, typoModes_p1 = typoModes)
parents2 = parents %>% filter(Lien == "2") %>% select(-Lien, -Genre) %>%
  rename(Activ_p2 = Activ, PCS8_p2 = PCS8, PCS42S_p2 = PCS42S,
         Dis_p2 = Dis.V, DuDom_p2 = DuDom, typoModes_p2 = typoModes)

peres    = parents %>% filter(Genre == "H") %>% select(-Lien, -Genre) %>%
  rename(Activ_P = Activ, PCS8_P = PCS8, PCS42S_P = PCS42S,
         Dis_P = Dis.V, DuDom_P = DuDom, typoModes_P = typoModes)
meres    = parents %>% filter(Genre == "F") %>% select(-Lien, -Genre) %>%
  rename(Activ_M = Activ, PCS8_M = PCS8, PCS42S_M = PCS42S,
         Dis_M = Dis.V, DuDom_M = DuDom, typoModes_M = typoModes)

enfants = enfants %>%
  left_join(parents1, by="uid_MEN") %>%
  left_join(parents2, by="uid_MEN") %>%
  left_join(peres,    by="uid_MEN") %>%
  left_join(meres,    by="uid_MEN")

enfants$Dis_Max   = apply(select(enfants, Dis_p1,   Dis_p2),   1, max)
enfants$Dis_Min   = apply(select(enfants, Dis_p1,   Dis_p2),   1, min)
enfants$DuDom_Max = apply(select(enfants, DuDom_p1, DuDom_p2), 1, max)
enfants$DuDom_Min = apply(select(enfants, DuDom_p1, DuDom_p2), 1, min)


ages = c(5:18)
corTab = tibble(age = integer(), ind = character(), var = character(),
                val = double(), cf1 = double(), cf2 = double(), df = double())

barre = ui_ProgInit(max(ages)-min(ages))

for(a in ages)
{
  enfants_a = filter(enfants, Age == a)
  
  diMax = cor.test(enfants_a$Dis.V, enfants_a$Dis_Max)
  diMin = cor.test(enfants_a$Dis.V, enfants_a$Dis_Min)
  diPeres = cor.test(enfants_a$Dis.V, enfants_a$Dis_P)
  diMeres = cor.test(enfants_a$Dis.V, enfants_a$Dis_M)
  
  duMax = cor.test(enfants_a$DuDom, enfants_a$DuDom_Max)
  duMin = cor.test(enfants_a$DuDom, enfants_a$DuDom_Min)
  duPeres = cor.test(enfants_a$DuDom, enfants_a$DuDom_P)
  duMeres = cor.test(enfants_a$DuDom, enfants_a$DuDom_M)
  
  
  
  corTabAge = tibble(age = rep(a, times=8),
                     ind = c(rep("Distance", times=4), rep("Durée au domicile", times=4)),
                     var = rep(c("la val. maximale", "la val. minimale", "le père", "la mère"), times=2),
                     val = c(diMax$estimate, diMin$estimate, diPeres$estimate, diMeres$estimate,
                             duMax$estimate, duMin$estimate, duPeres$estimate, duMeres$estimate),
                     cf1 = c(diMax$conf.int[1], diMin$conf.int[1], diPeres$conf.int[1], diMeres$conf.int[1],
                             duMax$conf.int[1], duMin$conf.int[1], duPeres$conf.int[1], duMeres$conf.int[1]),
                     cf2 = c(diMax$conf.int[2], diMin$conf.int[2], diPeres$conf.int[2], diMeres$conf.int[2],
                             duMax$conf.int[2], duMin$conf.int[2], duPeres$conf.int[2], duMeres$conf.int[2]),
                     df = c(diMax$parameter, diMin$parameter, diPeres$parameter, diMeres$parameter,
                            duMax$parameter, duMin$parameter, duPeres$parameter, duMeres$parameter))
  
  corTab = rbind(corTab, corTabAge)
  ui_Prog(barre = barre, i = a - min(ages))
}

g5 = corTab %>%
  ggplot(aes(x = age, y = val)) +
  geom_ribbon(aes(ymin = cf1, ymax = cf2, fill = var), alpha=.1) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  facet_wrap(~ind) +
  labs(title = "Corrélations observées entre les journées\ndes enfants et des parents lorsqu'ils travaillent",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  ylab("Coefficient de corrélation (Pearson)") + xlab("Âge de l'enfant") +
  scale_color_hue(name = "Corrélation avec") +
  theme_bw() + guides(fill = "none")

sortie("Distances/Parents et enfants, corrélation distances travailleur·ses")
print(g5)
off()

# Modes de transport

enfants = PER %>% filter(Age < 19, Lien == "3", EnqDate_JS != "3")
parents = PER_ff %>% filter(Lien %in% c("1", "2")) %>%
  select(uid_MEN, Lien, Genre, Activ, PCS8, PCS42S, Dis.V, DuDom, typoModes)

enfants = left_join(enfants, parents, by="uid_MEN", suffix=c("", "_p"))

sortie("Distances/Parents et enfants, corrélation modes")
g = enfants %>%
  filter(!is.na(typoModes) & !is.na(typoModes_p)) %>%
  group_by(Genre, typoModes, typoModes_p) %>%
  summarize(n = sum(CoeffEnq, na.rm=T)) %>%
  group_by(Genre, typoModes_p) %>% mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = typoModes, y = typoModes_p)) +
  geom_point(aes(size = n)) +
  facet_wrap(~Genre) +
  labs(title = "Association entre modes de transport des enfants\net des parents lorsqu'ils travaillent",
       caption = src_fig(bu = T, date = "nov. 2022")) +
  xlab("Modes employés par l'enfant") +
  ylab("Modes employés par le parent et part des parents transportant ainsi chaque enfant (%)") +
  geom_text(aes(label = paste(round(p,2), "%")), nudge_y = -.25, size = 1.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
print(g)
off()

# TODO: Relier à la FactoIndiv

# Carto simple ====

source("START.R")
initMémoire(BasesCharger = "PER")
PER_ff = init_PER_ff(PER)
load("Data/shp_ZT.rds") ; load("Data/fdCarte.rds")
load("Data/shp_COM.rds") ; load("Data/shp_ZF.rds")
ZFs = centroidesAAV()
remove(shp_ZF)

aavs = read_sf("Sources/Fond Carte/zMetro.shp")
aavs = rbind(aavs, read_sf("Sources/Fond Carte/zDOM.shp"))
aavs = st_transform(aavs, crs = 2154)
aavs = aavs %>% filter(AAV20 != "000") %>% group_by(AAV20) %>%
  summarise(LIBAAV2 = first(LIBAAV2)) %>%
  rename(LIBAAV2020 = LIBAAV2)

# On reprend les travaux sur les effets de contexte sur la distance, en simplifiant cette fois. But =
# réaliser des cartes toutes simples et sans fioritures inutiles.

# ~ Tests sur IDF et 44 ====

sortie("Distances/Distance moyenne, IDF2010")

disZT = PER_ff %>%
  filter(!is.na(CoeffEnq)) |>
  group_by(ZT) %>% summarise(disMed = weighted.median(Dis, CoeffEnq, na.rm=T),
                             disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T),
                             n = n())

et = filter(shp_ZT, uid_ENQ == "IDF2010")

g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  filter(uid_ENQ == "IDF2010") %>% cartoSchematiser() %>%
  ggplot() + geom_sf(aes(fill = disMoy/1000), color = "gray80") +
  scale_fill_gradient(low = "paleturquoise", high = "lightsteelblue4",
                      name = "distance moyenne\n(en km)", guide = "legend")
g = cartoAxes(g, et)
g = cartoLib(g, et)
g = cartoFinish(g, et)
print(g)
off()

sortie("Distances/Distance moyenne, LOI2015")
et = filter(shp_ZT, uid_ENQ == "LOI2015")
g = shp_ZT %>% left_join(disZT, by="ZT") %>% cartoSchematiser() %>%
  filter(uid_ENQ == "LOI2015") %>%
  ggplot() + geom_sf(aes(fill = disMoy/1000), color = "gray80") +
  scale_fill_gradient(low = "paleturquoise", high = "lightsteelblue4",
                      name = "distance moyenne\n(en km)", guide = "legend")
g = cartoHydro(g, et)
g = cartoAxes(g, et)
g = cartoLib(g, et)
g = cartoFinish(g, et)
print(g)
off()

# ~ Atlas des distances ====

cartoEnq = function(enq)
{
  cat("\rCarto : ",  z_Nomenclature[z_Nomenclature$uid_ENQ == enq,]$Libelle_Long)
  et = filter(shp_ZT, uid_ENQ == enq)
  g = shp_ZT %>% left_join(disZT, by="ZT") %>% cartoSchematiser() %>%
    filter(uid_ENQ == enq) %>%
    ggplot() + geom_sf(aes(fill = disMoy/1000), color = "gray80") +
    scale_fill_gradient(low = "paleturquoise", high = "lightsteelblue4",
                        name = "distance moyenne\n(en km)", guide = "legend") +
    labs(title = z_Nomenclature[z_Nomenclature$uid_ENQ == enq,]$Libelle_Long,
         caption = src_fig(et, carto = T))
  g = cartoHydro(g, et)
  g = cartoAxes(g, et, det = F)
  g = cartoLib(g, et)
  g = cartoFinish(g, et)
}

sortie("Distances/Atlas Distances", format = "pdf", taille = "a4", portrait = F)
cat("Carto : ")
lapply(X = sort(unique(shp_ZT$uid_ENQ)), FUN = cartoEnq) %>% print()
off()

disZT = PER_ff %>%
  group_by(ZT, PCS8) %>% summarise(disMed = weighted.median(Dis, CoeffEnq, na.rm=T),
                                   disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T),
                                   n = n()) %>%
  mutate(disMoy = ifelse(n > seuilSignifiant/2, disMoy, NA))

cartoEnq4 = function(enq)
{
  cat("\rCarto : ",  z_Nomenclature[z_Nomenclature$uid_ENQ == enq,]$Libelle_Long,
      "                              ")
  et = filter(shp_ZT, uid_ENQ == enq)
  g = shp_ZT %>% left_join(disZT, by="ZT") %>% cartoSchematiser() %>%
    mutate(PCS8 = etqPCS8(PCS8, num = T)) %>%
    filter(uid_ENQ == enq) %>%
    ggplot() + geom_sf(aes(fill = disMoy/1000), color = "gray80") +
    scale_fill_gradient(low = "paleturquoise", high = "lightsteelblue4",
                        name = "distance moyenne\n(en km)", guide = "legend",
                        na.value = "grey70") +
    labs(title = z_Nomenclature[z_Nomenclature$uid_ENQ == enq,]$Libelle_Long,
         caption = src_fig(et, carto = T)) + facet_wrap(~PCS8)
  g = cartoHydro(g, et)
  g = cartoAxes(g, et, det = F)
  g = cartoLib(g, et)
  g = cartoFinish(g, et)
}

sortie("Distances/Atlas Distances parPCS", format = "pdf", taille = "a4", portrait = F)
cat("Carto : ")
lapply(X = sort(unique(shp_ZT$uid_ENQ)), FUN = cartoEnq4) %>% print()
off()

sortie("Distances/Atlas Distances 44", portrait = F)
et = filter(shp_ZT, uid_ENQ == "LOI2015")
aav = filter(ZFs, LIBAAV2020 == "Nantes") %>%
  group_by(Com, LIBAAV2020) %>% summarise() %>%
  left_join(shp_COM, by = c("Com" = "insee")) %>% st_as_sf() %>%
  st_simplify(dTolerance = 200) %>%
  group_by(tout = T) %>%
  st_buffer(dist = 150) %>%
  summarise()
cen = filter(shp_COM, nom == "Nantes") %>% st_centroid()
g = shp_ZT %>% left_join(disZT, by="ZT") %>% cartoSchematiser() %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "LOI2015") %>%
  ggplot() + geom_sf(aes(fill = disMoy), color = "gray80", size = .2) +
  geom_sf(data = cen, aes(shape = "centroïde\nde Nantes")) +
  geom_sf(data = aav, aes(linetype = "aire d'attraction\nde Nantes"),
          color = "brown", size=2, fill = NA) +
  scale_shape(name = NULL) + scale_linetype(name = NULL) +
  scale_fill_brewer(palette = "BuPu", na.value = "ghostwhite",
                      name = "distance moyenne\n(en km)", guide = "legend") +
  labs(title = z_Nomenclature[z_Nomenclature$uid_ENQ == "LOI2015",]$Libelle_Long,
       caption = src_fig(et, carto = T))
g = cartoHydro(g, et)
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et)
g = cartoFinish(g, et)
print(g)
off()

sortie("Distances/Atlas Distances 69", portrait = F, taille = "carré")
et = filter(shp_ZT, uid_ENQ == "LYO2015")
aav = filter(ZFs, LIBAAV2020 == "Lyon") %>%
  group_by(Com, LIBAAV2020) %>% summarise() %>%
  left_join(shp_COM, by = c("Com" = "insee")) %>% st_as_sf() %>%
  st_simplify(dTolerance = 200) %>%
  group_by(tout = T) %>%
  st_buffer(dist = 150) %>%
  summarise()
cen = filter(shp_COM, nom == "Lyon") %>% st_centroid()
g = shp_ZT %>% left_join(disZT, by="ZT") %>% cartoSchematiser() %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "LYO2015") %>%
  ggplot() + geom_sf(aes(fill = disMoy), color = "gray80", size = .2) +
  geom_sf(data = cen, aes(shape = "centroïde\nde Lyon")) +
  geom_sf(data = aav, aes(linetype = "aire d'attraction\nde Lyon"), color = "plum", size=1, fill = NA) +
  scale_shape(name = NULL) + scale_linetype(name = NULL) +
  scale_fill_brewer(palette="BuPu", na.value = "ghostwhite",
                      name = "distance moyenne\n(en km)", guide = "legend") +
  labs(title = z_Nomenclature[z_Nomenclature$uid_ENQ == "LYO2015",]$Libelle_Long,
       caption = src_fig(et, carto = T))
g = cartoHydro(g, et)
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et, detail = 3)
g = cartoFinish(g, et)
print(g)
off()

disZT = PER_ff %>%
  filter(!is.na(Dis), !is.na(CoeffEnq)) |>
  group_by(ZT) %>% summarise(disMed = weighted.median(Dis, CoeffEnq, na.rm=T),
                             disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T),
                             n = n())

aav = filter(aavs, LIBAAV2020 == "Paris")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "IDF2010"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "IDF2010") %>% cartoSchematiser() %>%
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nparisienne"),
          color = "black", linewidth=.25, fill = NA, key_glyph = "polygon") +
  scale_fill_brewer(palette="BuPu", na.value = "ghostwhite",
                      name = "Distance moyenne\n(en km)", guide = "legend") +
  scale_linetype_manual(values = 2, name = "Agglomération") +
  labs(title = "Distance moyenne parcourue par les travailleur·ses de l'aire parisienne",
       subtitle = "En fonction de leur lieu de résidence",
       caption = src_fig(filter(PER, uid_ENQ == "IDF2010")))
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et, detail = 4)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Carte IDF")
print(g)
off()

aav = filter(aavs, LIBAAV2020 == "Bordeaux")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "BOR2009"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  filter(uid_ENQ == "BOR2009") %>% 
  mutate(disMoy = discretisation(disMoy/1000)) |>
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  scale_fill_brewer(palette="BuPu",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "ghostwhite") +
  labs(title = "Distance moyenne parcourue par les habitant·es de l'aire bordelaise",
       subtitle = "En fonction de leur lieu de résidence",
       caption = src_fig(filter(PER, uid_ENQ == "BOR2009")))
g = cartoHydro(g, et) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nbordelaise (2020)"),
          color = "black", linewidth = .25, fill = NA, key_glyph = "polygon") +
  scale_linetype_manual(values = 2, name = "Agglomération")
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Carte Bordeaux")
print(g)
off()

aav = filter(aavs, LIBAAV2020 == "Angers")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "AGR2012"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "AGR2012") %>% 
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  scale_fill_brewer(palette="BuPu",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "grey70") +
  labs(title = "Distance moyenne parcourue par les habitant·es de l'aire angevine",
       subtitle = "En fonction de leur lieu de résidence",
       caption = src_fig(filter(PER, uid_ENQ == "AGR2012")))
g = cartoHydro(g, et) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nd'Angers (2020)"),
          color = "slateblue", size = .6, fill = NA, key_glyph = "polygon") +
  scale_linetype(name = "Agglomération")
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Carte Angers")
print(g)
off()

aav = filter(aavs, LIBAAV2020 == "Metz")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "MET2017"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "MET2017") %>% 
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  scale_fill_brewer(palette="BuPu",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "grey70") +
  labs(title = "Distance moyenne parcourue par les habitant·es de l'aire de Metz",
       subtitle = "En fonction de leur lieu de résidence",
       caption = src_fig(filter(PER, uid_ENQ == "MET2017")))
g = cartoHydro(g, et) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nde Metz (2020)"),
          color = "slateblue", size = .6, fill = NA, key_glyph = "polygon") +
  scale_linetype(name = "Agglomération")
g = cartoAxes(g, et, det = F)
g = cartoLib(g, et)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Carte Metz")
print(g)
off()

# Rien à voir ici

inverse = summarise(st_difference(aav, st_transform(summarise(filter(shp_ZT, uid_ENQ == "REU2016")), crs = 3857)),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = filter(shp_ZT, uid_ENQ == "REU2016") %>% st_transform(crs = 3857)
g = shp_ZT %>% left_join(disZT, by="ZT") %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  filter(uid_ENQ == "REU2016") %>% 
  st_transform(crs = 3857) %>%
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  scale_fill_brewer(palette="BuPu",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "grey70") +
  labs(title = "Distance moyenne parcourue par les habitant·es\nde la Réunion",
       subtitle = "En fonction de leur lieu de résidence",
       caption = src_fig(filter(PER, uid_ENQ == "REU2016")))
g = cartoAxes(g, et, proj = 3857)
g = cartoLib(g, et, proj = 3857)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Carte Reunion")
print(g)
off()

disZT = PER_ff %>%
  filter(!is.na(Dis), !is.na(CoeffRec)) |>
  group_by(ZT_travMax) %>% summarise(disMed = weighted.median(Dis, CoeffEnq, na.rm=T),
                             disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T),
                             n = n())

aav = filter(aavs, LIBAAV2020 == "Clermont-Ferrand")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "CLF2012"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by=c("ZT" = "ZT_travMax")) %>%
    filter(uid_ENQ == "CLF2012") %>% cartoSchematiser() %>%
  mutate(disMoy = discretisation(disMoy/1000)) |>
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy), color = "gray90", size=.2) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nde Clermont-Ferrand (2020)"),
          color = "brown", linewidth = 1, fill = NA, key_glyph = "polygon") +
  scale_fill_brewer(palette="GnBu",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "grey70") +
  scale_linetype(name = "Agglomération") +
  labs(title = "Distance moyenne parcourue par les habitant·es\nde l'aire de Clermont-Ferrand",
       subtitle = "En fonction de leur lieu de travail principal (de la journée)",
       caption = src_fig(filter(PER, uid_ENQ == "CLF2012")))
g = cartoAxes(g, et)
g = cartoLib(g, et, detail = 4)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Travail Carte Clermont")
print(g)
off()

# Distance parcourue et distance au centre ====

# On part de l'hypothèse simple que la distance augmente en s'éloignant du centre du territoire de l'enquête
# Partons du principe que le centre peut être désigné par le premier secteur de chaque enquête.
centroides = shp_ZT %>% tab_Tri("ZT", parCol = "ZT") %>%
  group_by(uid_ENQ) %>% summarise(idFirst = first(ZT), uid_ENQ = first(uid_ENQ)) %>%
  st_centroid()

load("Data/shp_ZF.rds") ; load("Data/PGT.rds")
# Calculons la distance entre chaque ZT/PGT et ce centroide.
zf_centre = st_centroid(shp_ZF) %>%
  left_join(st_drop_geometry(mutate(centroides, centro = geometry)), by = "uid_ENQ") %>%
  mutate(disCentre = as.double(st_distance(geometry, centro, by_element = T))) %>%
  mutate(ZF = CODE_ZF) %>%
  select(ZF, disCentre)
zf_centre_pgt = PGT %>% rename(uid_ENQ = ENQ) %>% st_transform(crs = 2154) %>%
  left_join(st_drop_geometry(mutate(centroides, centro = geometry)), by = "uid_ENQ") %>%
  mutate(disCentre = as.double(st_distance(geometry, centro, by_element = T))) %>%
  mutate(ZF = paste(uid_ENQ, ZF))%>%
  select(ZF, disCentre)
zf_centre = rbind(zf_centre, zf_centre_pgt) %>% st_drop_geometry() ; remove(zf_centre_pgt)
remove(PGT)

# Et maintenant, attribuons une distance au centre à chaque individu et voyons comment ça évolue
# à mesure qu'on s'éloigne
PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  ggplot(aes(x = disCentre/1000, y = Dis/1000)) + geom_bin2d() 

sortie("Distances/Distance selon la distance au centre (médiane)")
PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  mutate(Dis = Dis/1000, disCentre = disCentre/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                          q1 = quantile(Dis, .25, na.rm=T),
                                          q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = "grey75") +
  geom_path() + facet_wrap(~PCS8) +
  xlab("Distance médiane parcourue dans la journée (km)") +
  ylab("Distance entre le centre de la ville centre et le domicile de l'enquêté⋅e (km)") +
  labs(title = "Rapport entre la distance médiane parcourue\net la distance au centre (enquête)",
       caption = src_fig())
off()

enqPasVide = PER_ff |>
  left_join(zf_centre, by = "ZF") %>%
  mutate(Dis = Dis/1000, disCentre = disCentre/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(uid_ENQ, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                             q1 = quantile(Dis, .25, na.rm=T),
                                             q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) |>
  group_by(uid_ENQ) |> summarise(n = n()) |> filter(n > 1)
  
g = PER_ff |>
  filter(uid_ENQ %in% enqPasVide$uid_ENQ) |>
  left_join(zf_centre, by = "ZF") %>%
  left_join(z_Nomenclature, by="uid_ENQ") |>
  mutate(Dis = Dis/1000, disCentre = disCentre/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(Libelle_Long, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                             q1 = quantile(Dis, .25, na.rm=T),
                                             q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = "grey75") +
  geom_path() + facet_wrap(~Libelle_Long, ncol = 5) +
  xlab("distance au centre de l'AAV (km)") +
  ylab("distance médiane parcourue (km) + écart interquartile") +
  labs(title = "Distance médiane parcourue en fonction de la distance\nau centre de l'AAV",
       caption = src_fig(enqPasVide))

sortie("Distances/Gradient distance et disCentre par enq", taille = "page", portrait=T)
print(g)
off()


PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  group_by(uid_ENQ) %>% summarise(corPearson = cor(Dis, disCentre),
                                  corSpearman = cor(Dis, disCentre, method="spearman")) %>%
  tab_Tri(parCol = "corSpearman", rev = F)

PER_ff |>
  left_join(zf_centre, by = "ZF") %>%
  group_by(uid_ENQ) %>% summarise(corPearson = cor(Dis, disCentre),
                                  corSpearman = cor(Dis, disCentre, method="spearman")) %>%
  tab_Tri(parCol = "corSpearman", rev = T)

PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  filter(!is.na(Dis), !is.na(disCentre)) %>%
  group_by(ZoneRang) %>% summarise(corPearson = cor(Dis, disCentre),
                                   corSpearman = cor(Dis, disCentre, method="spearman"))

PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  filter(!is.na(Dis), !is.na(disCentre)) %>%
  group_by(tout = T) %>% summarise(corPearson = cor(Dis, disCentre),
                                   corSpearman = cor(Dis, disCentre, method="spearman"))

PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  group_by(uid_ENQ) %>% summarise(corPearson = cor(Dis, disCentre),
                                  corSpearman = cor(Dis, disCentre, method="spearman")) %>%
  filter(uid_ENQ %in% c("IDF2010", "LOI2015"))

# Nous avons maintenant un indice de centralité par enquête ; voyons par genre et PCS
PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  group_by(uid_ENQ, Genre) %>% summarise(corPearson = cor(Dis, disCentre),
                                         corSpearman = cor(Dis, disCentre, method="spearman")) %>%
  ggplot(aes(x = Genre, y = corSpearman)) + geom_col(aes(fill = Genre)) + facet_wrap(~uid_ENQ) +
  scale_fill_hue()

PER_ff %>%
  left_join(zf_centre, by = "ZF") %>%
  group_by(uid_ENQ, PCS8) %>% summarise(corPearson = cor(Dis, disCentre),
                                        corSpearman = cor(Dis, disCentre, method="spearman")) %>%
  ggplot(aes(x = PCS8, y = corSpearman)) + geom_col(aes(fill = PCS8)) + facet_wrap(~uid_ENQ) +
  scale_fill_manual(values = pal_PCS8[2:6])

# En partant cette fois du lieu de travail
PER_ff %>%
  left_join(zf_centre, by = c("ZF_travMax" = "ZF")) %>%
  filter(!is.na(disCentre)) %>%
  group_by(uid_ENQ, Genre) %>% summarise(corPearson = cor(Dis, disCentre),
                                         corSpearman = cor(Dis, disCentre, method="spearman"), n = n()) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = Genre, y = corSpearman)) + geom_col(aes(fill = Genre)) + facet_wrap(~uid_ENQ) +
  scale_fill_hue()

PER_ff %>%
  left_join(zf_centre, by = c("ZF_travMax" = "ZF")) %>%
  filter(!is.na(disCentre)) %>%
  group_by(uid_ENQ, PCS8) %>% summarise(corPearson = cor(Dis, disCentre),
                                        corSpearman = cor(Dis, disCentre, method="spearman"), n = n()) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = PCS8, y = corSpearman)) + geom_col(aes(fill = PCS8)) + facet_wrap(~uid_ENQ) +
  scale_fill_manual(values = pal_PCS8[2:6])

# Pas mal, pas mal
# A présent, faisons ça à partir des AAV, voir ce que ça donne

sortie("Distances/Distance centre et distance médiane")
g1 = PER_ff %>%
  left_join(ZFs, by = "ZF") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                    q1 = quantile(Dis, .25, na.rm=T),
                                    q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "1er quartile -\n3e quartile")) +
  geom_path(aes(color = "médiane")) +
  scale_fill_manual(values = "grey75", name = NULL) + scale_color_manual(values = "black", name = NULL) +
  xlab("Distance entre le domicile de l'enquêté⋅e et\nle centre de la ville-centre de l'AAV (km)") +
  ylab("Distance médiane parcourue dans la journée (km)") +
  labs(title=ml("Distance parcourue dans la journée en fonction de la position du domicile",
                "par rapport au centre de la ville centre de l'AAV (km)"),
       caption = src_fig(emp=F))
print(g1)
off()

sortie("Distances/Distance centre et distance médiane Nantes")
g0 = PER_ff %>%
  left_join(ZFs, by = "ZF") %>%
  filter(LIBAAV2020 == "Nantes") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                    q1 = quantile(Dis, .25, na.rm=T),
                                    q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "1er quartile -\n3e quartile")) +
  geom_path(aes(color = "médiane")) +
  scale_fill_manual(values = "grey75", name = NULL) + scale_color_manual(values = "black", name = NULL) +
  xlab("Distance entre le domicile de l'enquêté⋅e et\nle centre de la ville-centre de l'AAV (km)") +
  ylab("Distance médiane parcourue dans la journée (km)") +
  labs(title=ml("Distance parcourue dans la journée en fonction de la position du domicile",
                "par rapport au centre de Nantes (km)"),
       caption = src_fig(filter(PER, uid_ENQ == "LOI2015")))
print(g1)
off()


g2 = PER_ff %>%
  left_join(ZFs, by = "ZF") %>%
  filter(LIBAAV2020 == "Nantes") %>%
  mutate(Tps = Tps/60, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(disCentre) %>% summarise(TpsMed = median(Tps, na.rm=T), n = n(),
                                    q1 = quantile(Tps, .25, na.rm=T),
                                    q3 = quantile(Tps, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = TpsMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "1er quartile -\n3e quartile")) +
  geom_path(aes(color = "médiane")) +
  scale_fill_manual(values = "grey75", name = NULL) + scale_color_manual(values = "black", name = NULL) +
  xlab("Distance entre le domicile de l'enquêté⋅e et\nle centre de la ville-centre de l'AAV (km)") +
  ylab("Temps médian en déplacement dans la journée (min)") +
  labs(title=ml("Temps passé en déplacement dans la journée en fonction de la position du domicile",
                "par rapport au centre de Nantes (h)"),
       caption = src_fig(filter(PER, uid_ENQ == "LOI2015"))) %>% print()

p = plot_grid(g0, g2 + theme(legend.position="none"), align="v", nrow=2)

sortie("Distances/Distance et temps selon disCentre à Nantes", taille = "page")
print(p)
off()

PER_ff %>%
  left_join(ZFs, by = "ZF") %>%
  filter(!is.na(Dis), !is.na(dis)) %>%
  group_by(ZoneRang) %>% summarise(pearson = cor(Dis, dis))

g2 = PER_ff |>
  filter(!is.na(ZF_travMax)) %>%
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                    q1 = quantile(Dis, .25, na.rm=T),
                                    q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "1er quartile -\n3e quartile")) +
  geom_path(aes(color = "médiane")) +
  scale_fill_manual(values = "grey75", name = NULL) + scale_color_manual(values = "black", name = NULL) +
  xlab("Distance entre le lieu de travail de l'enquêté⋅e et\nle centre de la ville-centre de l'AAV (km)") +
  ylab("Distance médiane parcourue dans la journée (km)") +
  labs(title=ml("Distance parcourue dans la journée en fonction de la position du lieu de travail",
                "par rapport au centre de la ville centre de l'AAV (km)"),
       caption = src_fig()) +
  scale_x_continuous(limits = c(0,30))

sortie("Distances/Distance centre (duo) et distance médiane")
cowplot::plot_grid(g1 + theme(legend.position = "bottom") + scale_x_continuous(limits = c(0,30)) +
                     scale_y_continuous(limits = c(0,100)) +
                     labs(caption = NULL, title = "Distance centre AAV\net domicile") +
                     xlab("distance domicile - centre (km)"),
                   g2 + theme(legend.position = "none") + ylab(NULL) +
                     scale_y_continuous(limits = c(0,100)) +
                     labs(title = "Distance centre AAV\net lieu d'emploi") +
                     xlab("distance lieu d'emploi - centre (km)"),
                   nrow=1, axis = "tb", align = "h", rel_widths=c(.52, .48)) %>%
  viz_Titre(ml("Distance médiane parcourue selon la position",
               "du lieu de résidence ou d'emploi des enquêté⋅es",
               "et le centre de la ville centre de l'AAV"), rel_heights = c(.15, .85))
off()

# chelou
test = filter(left_join(PER, ZFs, by = c("ZF_travMax" = "ZF")), round(dis/1000) == 33)
# table(test$uid_ENQ) issus de l'enquête de Paris
test %>% filter(uid_ENQ == "IDF2010") %>%
  group_by(ZF) %>% summarise(med = median(Dis, na.rm=T), n = n()) %>%
  left_join(shp_ZF, by = c("ZF" = "CODE_ZF")) %>% st_as_sf() %>%
  st_centroid() %>%
  ggplot() +
  geom_sf(data = filter(shp_ZT, substr(ZT, 1, 7) == "IDF2010"), fill = "grey90") +
  geom_sf(aes(color = med, size = n))
filter(shp_ZF, CODE_ZF == unique(filter(test, uid_ENQ == "IDF2010")$ZF_travMax))


sortie("Distances/Distance centre et distance médiane selon PCS et Genre")
PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                                 q1 = quantile(Dis, .25, na.rm=T),
                                                 q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  scale_color_manual(values = pal_PCS8[2:6]) +
  geom_path(aes(color = PCS8)) + facet_wrap(~Genre) +
  scale_x_continuous(limits = c(0,25)) + scale_y_continuous(limits = c(0, 70)) +
  labs(title = "Relation entre la distance médiane parcourue et la\nposition du lieu de résidence par rapport au centre de l'AAV",
       subtitle = "Personnes en emploi s'étant déplacées",
       caption = src_fig()) +
  xlab("Distance entre le lieu de résidence de l'enquêté⋅e\net le centre de la ville-centre de l'AAV (km)") +
  ylab("Distance médiane parcourue dans la journée")
off()

sortie("Distances/Distance centre et distance médiane selon PCS et Genre, Nantes")
PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  filter(LIBAAV2020 == "Nantes") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre/4)*4) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                                 q1 = quantile(Dis, .25, na.rm=T),
                                                 q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_path(aes(color = PCS8, linetype = Genre)) +
  scale_color_manual(values = pal_PCS8[3:6]) +
  scale_x_continuous(limits = c(0,25)) + scale_y_continuous(limits = c(0, 70)) +
  labs(title = "Relation entre la distance médiane parcourue et la\nposition du lieu de résidence par rapport au centre de l'AAV",
       subtitle = "Personnes en emploi s'étant déplacées",
       caption = src_fig()) +
  xlab("Distance entre le lieu de résidence de l'enquêté⋅e\net le centre de Nantes (km)") +
  ylab("Distance médiane parcourue dans la journée") %>% print()
off()


sortie("Distances/Distance centre et distance médiane selon PCS et Genre, Lyon")
PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  filter(LIBAAV2020 == "Lyon") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre/2.5)*2.5) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                                 q1 = quantile(Dis, .25, na.rm=T),
                                                 q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_path(aes(color = PCS8, linetype = Genre)) +
  scale_color_manual(values = pal_PCS8[3:6]) +
  scale_x_continuous(limits = c(0,25)) + scale_y_continuous(limits = c(0, 70)) +
  labs(title = "Relation entre la distance médiane parcourue et la\nposition du lieu de résidence ZFs",
       subtitle = "Personnes en emploi s'étant déplacées",
       caption = src_fig()) +
  xlab("Distance entre le lieu de résidence de l'enquêté⋅e\net le centre de l'aire (km)") +
  ylab("Distance médiane parcourue dans la journée") %>% print()
off()

sortie("Distances/Distance centre et distance médiane selon PCS et Genre, Lyon, sup", h=3, taille = "man")
PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  filter(LIBAAV2020 == "Lyon") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre/2.5)*2.5) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, disCentre) %>% summarise(n = sum(CoeffEnq)) %>% 
  group_by(disCentre) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = disCentre, y = n)) +
  geom_col(aes(fill = PCS8)) +
  scale_fill_manual(values = pal_PCS8[3:6]) +
  scale_x_continuous(limits = c(-2,27)) + 
  xlab(NULL) +
  ylab("Part pop. (%)") %>% print()
off()

PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  mutate(Dis = Dis/1000, disCentre = dis/1000) %>%
  mutate(disCentre = round(disCentre)) %>%
  group_by(uid_ENQ, disCentre) %>% summarise(disMed = median(Dis, na.rm=T), n = n(),
                                             q1 = quantile(Dis, .25, na.rm=T),
                                             q3 = quantile(Dis, .75, na.rm=T)) %>%
  filter(n>seuilSignifiant) %>%
  ggplot(aes(x = disCentre, y = disMed)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = "grey75") + geom_path() + facet_wrap(~uid_ENQ)


PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  filter(!is.na(dis) & !is.na(Dis)) %>%
  group_by(uid_ENQ) %>% summarise(pearson = as.double(cor.test(Dis, dis)$estimate),
                                  pearsonMin = as.double(cor.test(Dis, dis)$conf.int[1]),
                                  pearsonMax = as.double(cor.test(Dis, dis)$conf.int[2]),
                                  p_value = as.double(cor.test(Dis, dis)$p.value)) %>%
  left_join(z_Nomenclature, by = "uid_ENQ") %>% mutate(uid_ENQ = Libelle_Long) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "non significative")) %>%
  tab_Tri(parCol = "pearson") %>%
  ggplot(aes(x = uid_ENQ, y = pearson)) + geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value)) +
  scale_alpha_manual(values = c(1, .4, .2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

sortie("Distances/Corrélation disCentre et Dis par agglomération (domiciles)")
PER_ff %>%
  left_join(ZFs, by = "ZF") %>%
  filter(!is.na(dis) & !is.na(Dis)) %>%
  group_by(LIBAAV2020) %>% summarise(n = n(),
                                     pearson =    ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$estimate), NA),
                                     pearsonMin = ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$conf.int[1]), NA),
                                     pearsonMax = ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$conf.int[2]), NA),
                                     p_value =    ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$p.value), NA)) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "n. s.")) %>%
  filter(!is.na(pearson), n > 500) %>%
  tab_Tri(parCol = "pearson") %>%
  ggplot(aes(x = LIBAAV2020, y = pearson)) + geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value)) +
  scale_alpha_manual(values = c(1, .4, .2)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) +
  xlab("Principales agglomérations (Aires d'Attraction des Villes)") +
  ylab("coefficient de Pearson") +
  labs(title = "Corrélation entre la distance parcourue et la distance\nentre le domicile et le centre de l'AAV",
       subtitle = "par agglomération (parmis celles comptant plus de 500 enquêté⋅es)",
       caption = src_fig(bu = T, emp = F)) + theme(legend.position = "bottom")
off()


sortie("Distances/Corrélation disCentre et Dis par agglomération (lieux de travail)")
# Et pour l'emploi :
PER_ff %>%
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  filter(!is.na(dis) & !is.na(Dis)) %>%
  group_by(LIBAAV2020) %>% summarise(n = n(),
                                     pearson =    ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$estimate), NA),
                                     pearsonMin = ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$conf.int[1]), NA),
                                     pearsonMax = ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$conf.int[2]), NA),
                                     p_value =    ifelse(n>seuilSignifiant, as.double(cor.test(Dis, dis)$p.value), NA)) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "non significative")) %>%
  filter(!is.na(pearson), n > 500) %>%
  tab_Tri(parCol = "pearson") %>%
  ggplot(aes(x = LIBAAV2020, y = pearson)) + geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value)) +
  scale_alpha_manual(values = c(1, .4, .2)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) +
  xlab("Principales agglomérations (Aires d'Attraction des Villes)") +
  ylab("coefficient de Pearson") +
  labs(title = "Corrélation entre la distance parcourue et la distance\nentre le lieu de travail principal et le centre de l'AAV",
       subtitle = "par agglomération (parmis celles comptant plus de 500 enquêté⋅es)",
       caption = src_fig(bu = T, emp = F)) + theme(legend.position = "bottom")
off()

disZT = PER_ff |>
  group_by(ZT_travMax) %>% summarise(disMed = weighted.median(Dis, CoeffEnq, na.rm=T),
                                     disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T),
                                     n = n())

aav = filter(aavs, LIBAAV2020 == "Paris")
inverse = summarise(st_difference(aav, summarise(filter(shp_ZT, uid_ENQ == "IDF2010"))),
                    ID_SEC = NA, ENQUETE = NA, EnqAnnee = NA, CODE_SEC = NA, LENGTH = NA,
                    AREA = NA, X_W84 = NA, Y_W84 = NA, CODE_INSEE_VC = NA, uid_ENQ = NA, 
                    ZT = NA, disMed = NA, disMoy = NA, n = NA)
et = aav
g = shp_ZT %>% left_join(disZT, by=c("ZT" = "ZT_travMax")) %>%
  filter(uid_ENQ == "IDF2010") %>% cartoSchematiser() %>%
  rbind(inverse) %>%
  ggplot() + 
  geom_sf(aes(fill = disMoy/1000), color = "gray90", size=.2) +
  geom_sf(data = aav, aes(linetype = "Limites de l'AAV\nparisienne"),
          color = "lightslateblue", size = .6, fill = NA, key_glyph = "polygon") +
  scale_fill_gradient(low = "paleturquoise", high = "lightsteelblue4",
                      name = "Distance moyenne\n(en km)", guide = "legend",
                      na.value = "grey90") +
  scale_linetype(name = "Agglomération") +
  labs(title = "Distance moyenne parcourue par les habitant·es de l'aire parisienne",
       subtitle = "En fonction de leur lieu de travail principal (de la journée)",
       caption = src_fig(filter(PER, uid_ENQ == "IDF2010")))
g = cartoAxes(g, et)
g = cartoLib(g, et, detail = 4)
g = cartoFinish(g, et)

sortie("Distances/Distance moyenne Travail Carte IDF")
print(g)
off()


sortie("Distances/Corrélation disCentre et dis par agglo et PCS")
PER_ff |>
  left_join(ZFs, by = "ZF") %>%
  filter(!is.na(dis) & !is.na(Dis)) %>%
  group_by(LIBAAV2020) %>% mutate(nEnqAAV = n()) %>%
  group_by(LIBAAV2020, PCS8) %>% summarise(n = n(),
                                           pearson =    ifelse(n>2, as.double(cor.test(Dis, dis)$estimate), NA),
                                           pearsonMin = ifelse(n>2, as.double(cor.test(Dis, dis)$conf.int[1]), NA),
                                           pearsonMax = ifelse(n>2, as.double(cor.test(Dis, dis)$conf.int[2]), NA),
                                           p_value =    ifelse(n>2, as.double(cor.test(Dis, dis)$p.value), NA)) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "non significative")) %>%
  filter(!is.na(pearson), n > 500) %>%
  tab_Tri(parCol = "pearson") %>%
  ggplot(aes(x = PCS8, y = pearson)) +
  geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value, color = PCS8), shape=4, size = .35) +
  scale_alpha_manual(values = c(1, .4, .2)) + scale_color_manual(values = pal_PCS8[3:6],
                                                                 name = "CSP", labels = niv_PCS8[3:6]) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) + facet_wrap(~LIBAAV2020) +
  xlab("catégorie socioprofessionnelle") + ylab("indice de Pearson") +
  labs(title = "Corrélation entre distance au centre et distance\nparcourue dans la journée selon la PCS",
       subtitle = "Par agglomération (AAV de 2020)", caption = src_fig(emp = F))
off()

PER_ff |>
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  filter(!is.na(dis) & !is.na(Dis)) %>%
  group_by(LIBAAV2020) %>% mutate(nEnqAAV = n()) %>%
  group_by(LIBAAV2020, PCS8) %>% summarise(n = n(),
                                           pearson =    ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, dis)$estimate), NA),
                                           pearsonMin = ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, dis)$conf.int[1]), NA),
                                           pearsonMax = ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, dis)$conf.int[2]), NA),
                                           p_value =    ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, dis)$p.value), NA)) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "non significative")) %>%
  filter(!is.na(pearson), n > 500) %>%
  tab_Tri(parCol = "pearson") %>%
  ggplot(aes(x = PCS8, y = pearson)) + geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value, color = PCS8)) +
  scale_alpha_manual(values = c(1, .4, .2)) + scale_color_manual(values = pal_PCS8[2:6]) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) + facet_wrap(~LIBAAV2020)


# Où sont les emplois ?
g1 = PER_ff |>
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  group_by(dis = round(dis/1000)) %>% summarise(n = n())
# %>%
#     ggplot(aes(x = dis, y = nEmplois)) + geom_path() + coord_cartesian(ylim = c(0,15000)) +
#     labs(title = "Lieux d'emploi",
#          caption = src_fig(emp=F)) +scale_x_continuous (limits = c(0,75)) +
#     xlab("distance au centre de l'AAV (km)") + ylab("nombre de lieux d'emploi")

sortie("Distances/Centralisation des lieux de résidence et d'emploi")
PER_ff %>%
  left_join(ZFs, by = c("ZF" = "ZF")) %>%
  group_by(dis = trunc(dis/1000)) %>% summarise(n = n()) %>%
  ggplot(aes(x = dis, y = n)) +
  geom_path(aes(color = "lieux de résidence")) +
  geom_path(data = g1, aes(color = "lieux d'emploi princ.")) +
  coord_cartesian(ylim = c(0,12000)) +
  labs(title = "Répartition des lieux de résidence et d'emploi par rapport aux centres des AAV",
       caption = src_fig(emp=F),
       subtitle = "Personnes en emploi s'étant rendues sur leur lieu de travail") +
  scale_x_continuous(limits= c(0,75)) +
  xlab("distance au centre de l'AAV (km)") + ylab("nombre d'enquêté⋅es") +
  scale_color_manual(values = c("goldenrod", "forestgreen"), name = "lieux comptabilisés")
off()


sortie("Distances/Distance entre lieux d'emploi et centre de l'AAV")
PER_ff |>
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  group_by(PCS8, dis = round(dis/1000)) %>% summarise(nEmplois = n()) %>%
  group_by(PCS8) %>% mutate(pEmplois = nEmplois/sum(nEmplois)*100) %>%
  ggplot(aes(x = dis, y = pEmplois)) + geom_path(aes(color = PCS8)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "PCS des enquêté⋅es",
                     labels = niv_PCS8[1:6]) +
  coord_cartesian(ylim = c(0,8), xlim = c(0,30)) +
  labs(title = "Part des lieux d'emplois principaux par\ntranche de distance au centre de l'AAV",
       caption = src_fig(emp=F)) +
  xlab("distance au centre de l'AAV (km)") +
  ylab("part des lieux d'emplois situés à cette distance (%)")
off()

PER_ff |>
  left_join(ZFs, by = c("ZF_travMax" = "ZF")) %>%
  group_by(Genre, dis = round(dis/1000)) %>% summarise(nEmplois = n()) %>%
  group_by(Genre) %>% mutate(pEmplois = nEmplois/sum(nEmplois)*100) %>%
  ggplot(aes(x = dis, y = pEmplois)) + geom_path(aes(color = Genre)) +
  coord_cartesian(ylim = c(0,8), xlim = c(0,50))

sortie("Distances/Corrélation distance parcourue et Travail_Dis")
PER_ff %>%
  filter(!is.na(Travail_Dis)) %>%
  mutate(Genre = paste0(etqGenre(Genre), "s")) %>%
  group_by(Genre, PCS8) %>% summarise(n = n(),
                                      pearson =    ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, Travail_Dis)$estimate), NA),
                                      pearsonMin = ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, Travail_Dis)$conf.int[1]), NA),
                                      pearsonMax = ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, Travail_Dis)$conf.int[2]), NA),
                                      p_value =    ifelse(n>seuilSignifiant/2, as.double(cor.test(Dis, Travail_Dis)$p.value), NA)) %>%
  mutate(p_value = case_when(p_value < .01 ~ "inf. à 1%",
                             p_value < .05 ~ "inf. à 5%",
                             p_value > .05 ~ "non significative")) %>%
  ggplot(aes(x = PCS8, y = pearson)) + geom_pointrange(aes(ymin = pearsonMin, ymax = pearsonMax, alpha=p_value, color = PCS8)) +
  scale_alpha_manual(values = c(1, .4, .2)) +
  scale_color_manual(values = pal_PCS8[1:6], name = "PCS", labels = niv_PCS8[1:6]) +
  facet_wrap(~Genre) +
  labs(title = ml("Corrélation entre la distance domicile travail à vol d'oiseau",
                  "et la distance parcourue dans la journée"),
       caption = src_fig(emp = F)) +
  xlab("catégorie socioprofessionnelle") + ylab("coefficient de Pearson")
off()


# Potentiels d'emploi et distance ====

# Calculons un potentiel d'emplois autour de chaque ZF

# # Etape 1 : prenons l'ensemble des ZF et établissons un cercle de 10 km
# ZF = shp_ZF %>% select(CODE_ZF, geometry) %>% rename(ZF = CODE_ZF)
# PG = PGT    %>% select(ENQ, ZF, geometry) %>% mutate(ZF = paste(ENQ, ZF)) %>% select(-ENQ)
# ZF = rbind(ZF, st_transform(PG, crs=2154)) ; remove(PG)
# ZF = st_centroid(ZF)
# ZF_10km = st_buffer(ZF, dist = 10000)
# 
# # Etape 2 : comptons combien il y a d'emplois dans les périmètres
# # 2.1 : comptons les emplois par ZF
# ZF_emplois = PER %>%
#     group_by(ZF_travMax, PCS8) %>% summarise(nEmplois = sum(CoeffEnq)) %>%
#     left_join(shp_ZF, by = c("ZF_travMax" = "CODE_ZF")) %>%
#     left_join(mutate(PGT, ZF = paste(ENQ, ZF)), by = c("ZF_travMax" = "ZF")) %>%
#     mutate(geometry = ifelse(st_is_empty(geometry.x), geometry.y, geometry.x)) %>%
#     st_as_sf()
# # 2.2 : intersectons et réunissons /!\ opération longue
# # ZF_10km = st_intersection(ZF_10km, ZF_emplois)

# Calculer, par ZT, le différentiel entre le nombre d'emplois et le nombre de résident·es ?
# Et trouver un moyen d'identifier où - ce différentiel est le plus homogène ; le plus hétérogène

tableauGens = PER_ff |>
  group_by(ZT) %>% summarise(nPop = sum(CoeffEnq, na.rm=T))
tableauEmplois = PER_ff %>%
  group_by(ZT_travMax) %>% summarise(nEmp = sum(CoeffEnq, na.rm=T), nCheck = n()) %>%
  rename(ZT = ZT_travMax)
rapPopEmp = left_join(shp_ZT, tableauGens, by = "ZT") %>%
  left_join(tableauEmplois, by = "ZT") %>%
  mutate(rapport = ifelse(nCheck > seuilSignifiant/2, transf_echelle_sur100((nEmp/nPop - 1)*100), NA))

rapPopEmp %>% group_by(uid_ENQ) %>% summarise(sd = sd(rapport, na.rm=T), moy = mean(rapport, na.rm=T)) %>%
  tab_Tri(i = "uid_ENQ", parCol = "sd")
rapPopEmp %>% group_by(uid_ENQ) %>% summarise(sd = sd(rapport, na.rm=T), moy = mean(rapport, na.rm=T)) %>%
  tab_Tri(i = "uid_ENQ", parCol = "sd", rev = T)

minimap_emploi = function(enq)
{
  zts = filter(shp_ZT, uid_ENQ == enq)
  et = summarise(zts)
  g = ggplot(filter(rapPopEmp, uid_ENQ == enq)) +
    geom_sf(aes(fill = rapport), color = "gray90", size=.2) +
    scale_fill_gradient2(low = "forestgreen", high = "gold",
                         mid = "grey95", midpoint = 0, na.value = "grey75",
                         name = "Nb. d'emplois /\nnb de travailleur·ses\nrésident·es",
                         labels = ~round(transf_echelle_sur100_inverse(.)/100 + 1, 2)) +
    labs(title = paste("Rapport emploi/résidence sur l'enquête de",
                       z_Nomenclature[z_Nomenclature$uid_ENQ == enq,]$Libelle_Long),
         subtitle = "Population active s'étant déplacée pour travailler uniquement",
         caption = src_fig(zts, carto = T))
  g = cartoHydro(g, et)
  g = cartoAxes(g, et)
  g = cartoLib(g, et, detail = 5)
  g = cartoFinish(g, et)
  return(g)
}

rapport("Atlas du rapport emplois sur population par ZT")
sortie("Distances/Atlas Rapport Emplois sur Population", format = "pdf", taille = "a4", portrait = F)
barre = ui_ProgInit(length(unique(rapPopEmp$uid_ENQ)))
i = 0
for (enq in unique(rapPopEmp$uid_ENQ))
{
  print(minimap_emploi(enq))
  i = i + 1 ; ui_Prog(barre = barre, i = i)
}
off()

sortie("Distances/Carte rapport emploi population Toulouse")
minimap_emploi("TLS2013")
off()

sortie("Distances/Carte rapport emploi population IDF")
minimap_emploi("IDF2010")
off()

# Portraits de travailleurs longue distance ======================================================

rapport("Zoom sur les travailleur·ses longue distance", prim = T)

PER_ld = filter(PER_f, Dis > 166000, modes_avion == "non")

# Généralités
PER_ld |> group_by(Genre) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  mutate(p = nb / sum(nb) * 100)

PER_ld |> group_by(PCS8, Genre) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS8) |> mutate(p = nb / sum(nb) * 100) |> filter(Genre == "F")

PER_ld |> group_by(PCS42S, Genre) |>
  summarise(n = n(), nb = sum(CoeffRecEnq, na.rm=T)) |>
  group_by(PCS42S) |> mutate(p = nb / sum(nb) * 100) |> filter(Genre == "F")

# Où sont iels en France ?
tDep = PER_f |>
  filter(modes_avion == "non") |>
  mutate(long = Dis > 166000) |>
  mutate(dep = comEnDep(Com)) |>
  group_by(dep, long) |> summarise(nb = sum(CoeffRecEnq, na.rm = T), n = n()) |>
  group_by(dep) |> mutate(p = nb / sum(nb) * 100, n = sum(n)) |>
  filter(long) |> filter(n>seuilSignifiant)

# Ça ne fonctionne pas en mutate :(
tDep$pDisc = discretisation(tDep$p)
nNiv = levels(tDep$pDisc) |> length()

# Échelle
echelle = scale_fill_brewer(palette = "BuPu", na.value = "ghostwhite",
                            name = ml("Part de",
                                      "travailleur⋅ses",
                                      "parcourant plus",
                                      "de 166 km",
                                      "au cours de la",
                                      "journée",
                                      "d'enquête (%)"))

# Carte finale
g = tDep |> viz_France(champRel = "pDisc", echelleRel = echelle)

g = viz_Titre(g, "Part d'enquêté⋅es se déplaçant sur de longues distances\nau cours d'une journée de travail")

sortie("Distances/France longues distances")
  print(g)
off()

# Identification des chef·fes d'entreprise et des professions intermédiaires quand c'est possible,
# ainsi que des 5/6 selon leur schéma de mobilité.

# filter(ACT, uid_PER %in% filter(PER_ld, PCS8 == "06")$uid_PER) %>%
#     filter(Tache == "106") %>%
#     group_by(uid_PER) %>% summarize(nTaTvl = n(), duTot = sum(du), duMax = max(du)) %>%
#     filter(duMax < 60, duTot > 60)

tourneesLongues =
  filter(DEP, O_Motif == "81") %>%
  group_by(uid_PER) %>% summarize(duTot = sum(Duree))
base_tournees = filter(PER_ld, PCS8 == "06" & uid_PER %in% tourneesLongues$uid_PER)
remove(tourneesLongues)

base_popSansTournees = filter(PER_ld, PCS8 %in% c("05", "06") & !uid_PER %in% base_tournees$uid_PER)

base_patrons = filter(PER_ld, PCS42S == "23")

base_comcx = filter(PER_ld, PCS42S == "46")

PER_ld = mutate(PER_ld, categ = case_when(PCS42S == "23" ~ "chef·fes d'entreprise",
                                          PCS42S == "46"   ~ "administration privée",
                                          PCS8 == "06" ~ "ouvrier·es avec tournées",
                                          PCS8 == "05" ~ "autres employé·es/ouvrier·es",
                                          T ~ "autres"))

sortie("Temps/Durée moyenne d'une plage de travail")
PER %>% filter(typoJo == "TRAV" & modes_avion == "non") %>%
  mutate(lgNav = case_when(Dis > 150000 ~ "plus de 150 km", Dis <= 150000 ~ "jusqu'à 150 km")) %>%
  filter(!is.na(lgNav)) %>%
  ggplot(aes(x = duTvlPlage/60)) + geom_density(aes(color = lgNav, fill=lgNav), alpha=.2) +
  coord_cartesian(xlim = c(0,15)) +
  scale_fill_hue(name = "longueur de la navette") +
  scale_color_hue(name = "longueur de la navette") +
  theme_bw(base_family = "mono") +
  theme(legend.position = "bottom") +
  labs(title = "Distribution de la durée moyenne d'une plage de travail",
       subtitle = "Selon la longueur de la navette") +
  xlab("durée moyenne d'une plage de travail (heures)") + ylab("répartition de l'effectif")
off()

sortie("Temps/Temps au lieu d'emploi sur lieu d'emploi principal")
PER %>% filter(typoJo == "TRAV" & modes_avion == "non") %>%
  mutate(lgNav = case_when(Dis > 150000 ~ "plus de 150 km", Dis <= 150000 ~ "jusqu'à 150 km")) %>%
  filter(!is.na(lgNav)) %>%
  ggplot(aes(x = duTvl_lPrinc)) + geom_density(aes(color = lgNav, fill=lgNav), alpha=.2) +
  scale_fill_hue(name = "longueur de la navette") +
  scale_color_hue(name = "longueur de la navette") +
  theme_bw(base_family = "mono") +
  theme(legend.position = "bottom") +
  labs(title = "Distribution de la part du temps passé sur le lieu de travail principal",
       subtitle = "Selon la longueur de la navette") +
  xlab("part du temps passé sur le lieu de travail principal (%)") + ylab("répartition de l'effectif")
off()

sortie("Distances/Nb de lieux de travail distincts")
PER %>% filter(typoJo == "TRAV" & modes_avion == "non") %>%
  mutate(lgNav = case_when(Dis > 150000 ~ "plus de 150 km", Dis <= 150000 ~ "jusqu'à 150 km")) %>%
  filter(!is.na(lgNav)) %>%
  ggplot(aes(x = nbLxTvl)) + geom_density(aes(color = lgNav, fill=lgNav), alpha=.2) +
  scale_fill_hue(name = "longueur de la navette") +
  scale_color_hue(name = "longueur de la navette") +
  theme_bw(base_family = "mono") +
  theme(legend.position = "bottom") +
  labs(title = "Distribution du nombre de lieux de travail distincts",
       subtitle = "Selon la longueur de la navette") +
  xlab("lieux de travail distincts") + ylab("répartition de l'effectif")
off()

sortie("Distances/Distance parcourue durant boucle de travail")
PER_f %>%
  mutate(lgNav = case_when(Dis > 166000 ~ "plus de 166 km", Dis <= 166000 ~ "jusqu'à 166 km")) %>%
  filter(!is.na(lgNav)) %>%
  ggplot(aes(x = Dis_pBclTvl)) + geom_density(aes(color = lgNav, fill=lgNav), alpha=.2) +
  scale_fill_hue(name = "longueur de la navette") +
  scale_color_hue(name = "longueur de la navette") +
  theme_bw(base_family = "mono") +
  theme(legend.position = "bottom") +
  labs(title = "Distribution de la part de la distance parcourue\ndurant une boucle de travail",
       subtitle = "Selon la longueur de la navette") +
  xlab("part distance correspondant à une boucle de travail (%)") + ylab("répartition de l'effectif")
off()


seuilPartBclTv = 50

PER_ld = mutate(PER_ld,
                typoPlages = case_when(duTvlPlage >= 360 &                    nbLxTvl == 1 ~ "intensif simple",
                                       duTvlPlage >= 120 & duTvlPlage < 360 & nbLxTvl == 1 ~ "intensif fractionné",
                                       duTvlPlage >= 120                    & nbLxTvl >  1 ~ "extensif séquentiel",
                                       duTvlPlage <  120                    & nbLxTvl >  1 ~ "extensif mobile",
                                       duTvlPlage <  120                    & nbLxTvl == 1 ~ "autres 1",
                                       duTvlPlage >= 360 &                    nbLxTvl >  1 ~ "autres 2",
                                       T                                                   ~ "autres"),
                typoPlages = factor(x = typoPlages,
                                    levels = c("intensif simple", "intensif fractionné", "extensif séquentiel",
                                               "extensif mobile", "autres 1", "autres 2", "autres")))

PER_ld |> group_by(typoPlages) |> summarise(n = n(), nb = sum(CoeffRecEnq)) |>
  mutate(p = nb / sum(nb) * 100)

PER_ld |> filter(Dis_pBclTvl <= 50) |>
  group_by(typoPlages) |> summarise(n = n(), nb = sum(CoeffRecEnq))

nrow(filter(PER_ld, duTvlPlage<120 & nbLxTvl == 1)) / nrow(PER_ld)
nrow(filter(PER_ld, duTvlPlage>=360 & nbLxTvl>1)) / nrow(PER_ld)

table(filter(PER_ld, Dis_pBclTvl <= 50)$typoPlages)

table(filter(PER_ld, Dis_pBclTvl > 50)$PCS42S, useNA = "always") %>% freq() %>% tab_Tri(parCol = "n", rev=T)
table(filter(PER_ld, Dis_pBclTvl > 50, !is.na(PCS42S))$PCS42S, useNA = "always") %>% freq() %>% tab_Tri(parCol = "n", rev=T)

table(PER_ld$uid_ENQ) %>% freq() %>% as.data.frame() %>% tab_Tri(parCol = "n", rev=T)

(sum(filter(PER_ld, PCS42S == "36")$CoeffRecEnq) + sum(filter(PER_ld, PCS42S == "61")$CoeffRecEnq))  / sum(filter(PER_ld, !is.na(PCS42S))$CoeffRecEnq)

PER_ld %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(typoPlages, Genre, PCS8) %>% summarise(n = n()) %>%
  group_by(typoPlages, Genre) %>% mutate(p = n/sum(n) *100) %>%
  ggplot(aes(x = typoPlages, y = n)) +
  geom_col(aes(fill = PCS8), position="dodge") +
  facet_wrap(~Genre) +
  scale_fill_manual(values = pal_PCS8[2:6]) +
  coord_flip() + ylab("part (%)") + xlab("type de navette") + 
  theme(legend.position = "bottom")

g1 = PER_ld %>%
  filter(!is.na(PCS8), typoPlages != "autres 1") |>
  filter(Dis_pBclTvl > 50) %>%
  mutate(PCS8 = etqPCS8(PCS8, rev=T), Genre = etqGenre(Genre)) %>%
  group_by(PCS8, Genre, typoPlages) %>% summarise(n = n()) %>%
  group_by(PCS8, Genre) %>% mutate(p = n/sum(n) * 100,
                                   sN = sum(n)) %>%
  ungroup() %>% mutate(p = ifelse(sN > 10, p, NA)) %>%
  ggplot(aes(x = PCS8, y = p)) +
  geom_col(aes(fill = typoPlages), position="stack") +
  facet_wrap(~Genre) +
  scale_fill_manual(values = c("darkseagreen4", "darkseagreen3", "steelblue2" , "steelblue4", "grey")) +
  coord_flip() + ylab("part des de travailleur·ses (%)") + xlab("PCS") + 
  labs(title = "Part des schémas de navettes par PCS") +
  theme(legend.position = "bottom")

g2 = PER_ld %>%
  filter(!is.na(PCS42S), typoPlages != "autres 1") |>
  filter(Dis_pBclTvl > 50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev=T), Genre = etqGenre(Genre)) %>%
  group_by(PCS42S, Genre, typoPlages) %>% summarise(n = n()) %>%
  group_by(PCS42S, Genre) %>% mutate(p = n/sum(n) * 100,
                                     sN = sum(n)) %>%
  ungroup() %>% mutate(p = ifelse(sN > 10, p, NA)) %>%
  ggplot(aes(x = PCS42S, y = n)) +
  geom_col(aes(fill = typoPlages), position="stack") +
  facet_wrap(~Genre) +
  scale_fill_manual(values = c("darkseagreen4",  "darkseagreen3", "steelblue2" , "steelblue4", "grey"),
                    name = "schéma") +
  coord_flip() + ylab("nombre de travailleur·ses") + xlab("PCS 16 postes") + 
  labs(title = "Nombre de travailleur·ses par schéma") +
  theme(legend.position = "bottom")

sortie("Distances/Typologie navettes longues distances", taille = "carré", portrait = T)
cowplot::plot_grid(g1 + theme(legend.position = "none"), g2,
                   labels = c("A","B"), nrow = 2, align="v", rel_heights = c(.4,.6))
off()

# PER_ld |>
#   filter(!is.na(PCS42S), typoPlages != "autres 1") |>
#   filter(Dis_pBclTvl > 50) %>%
#   group_by(PCS42S, Genre, typoPlages) %>% summarise(n = n()) %>%
#   group_by(PCS42S, Genre) %>% mutate(p = n/sum(n) * 100,
#                                      sN = sum(n)) %>% View()

PER_ld |>
    filter(!is.na(PCS42S), typoPlages != "autres 1") |>
    filter(Dis_pBclTvl > 50) %>%
    group_by(PCS42S, Genre, typoPlages) |>
    summarise(nb = sum(CoeffRecEnq), n = n()) |>
    filter(typoPlages == "intensif simple") |>
    filter(PCS42S == "36")

PER_ld |>
  filter(!is.na(PCS42S), typoPlages != "autres 1") |>
  filter(Dis_pBclTvl > 50) %>%
  group_by(PCS42S, Genre, typoPlages) |>
  summarise(nb = sum(CoeffRecEnq), n = n()) |>
  filter(typoPlages == "intensif simple") |>
  filter(PCS42S == "61")

PER_ld %>%
  filter(Dis_pBclTvl > 50) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06") & !is.na(PCS42S) & PCS42S != "69") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev=T), Genre = etqGenre(Genre)) %>%
  group_by(PCS42S, Genre, typoPlages) %>% summarise(n = n()) %>%
  group_by(PCS42S, Genre) %>% mutate(p = n/sum(n) * 100,
                                     sN = sum(n)) %>%
  ungroup() %>% mutate(p = ifelse(sN > 10, p, NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) +
  geom_col(aes(fill = typoPlages), position="stack") +
  facet_wrap(~Genre) +
  scale_fill_manual(values = c("darkseagreen4",  "darkseagreen3", "steelblue2" , "steelblue4", "grey"),
                    name = "schéma") +
  coord_flip() + ylab("%") + xlab("PCS 16 postes") + 
  labs(title = "Part de travailleur·ses par schéma") +
  theme_bw(base_family="mono") +
  theme(legend.position = "bottom")

PER_ld_cp = PER_ld %>% filter(PCS42S %in% c("36", "61") & typoPlages == "intensif simple")


sortie("Distances/Navettes longues distances, cadres vs ouvrier·es selon densité")
# Quel est le type d'espace du lieu de travail et du lieu de résidence ?
PER_ld_cp %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T),
         ZoneDens_travMax = etqZoneDens(ZoneDens_travMax, supprTrFaible = T),
         PCS42S = etqPCS42S(PCS42S)) %>%
  filter(!is.na(ZoneDens_travMax)) %>%
  group_by(ZoneDens, ZoneDens_travMax, PCS42S) %>%
  summarise(n = n()) %>%
  group_by(ZoneDens, PCS42S) %>% mutate(p = n/sum(n) * 100, nZoneDens = sum(n)) %>%
  ggplot(aes(x = ZoneDens, y = ZoneDens_travMax)) +
  geom_point(aes(size = n, alpha=p, color=PCS42S), shape = 15) +
  scale_size_binned_area(max_size = 10, name = "nombre\nd'enquêté·es") +
  scale_alpha(name = "part des\nenquêté·es (%)\npar type de com.\nde résidence") +
  scale_colour_manual(name = "PCS", values = c("navyblue", "brown")) +
  facet_grid(~PCS42S) +
  xlab("Classe de densité, commune de résidence") +
  ylab("Classe de densité, commune de travail") +
  labs(title = "Comparaison du type de commune de résidence et de travail",
       subtitle = "Ouvrier·es et cadres parcourant plus de 166 km/journée de travail,\ntype intensif simple",
       caption = src_fig())
off()

sortie("Distances/Navettes longues distances, cadres vs ouvrier·es selon position AAV")
# Quel est le type d'espace du lieu de travail et du lieu de résidence ?
PER_ld_cp %>%
  mutate(ZonePosi = etqZonePosi(ZonePosi, court=T),
         ZonePosi_travMax = etqZonePosi(ZonePosi_travMax),
         PCS42S = etqPCS42S(PCS42S)) %>%
  filter(!is.na(ZonePosi_travMax)) %>%
  group_by(ZonePosi, ZonePosi_travMax, PCS42S) %>%
  summarise(n = n()) %>%
  group_by(ZonePosi, PCS42S) %>% mutate(p = n/sum(n) * 100, nZonePosi = sum(n)) %>%
  ggplot(aes(x = ZonePosi, y = ZonePosi_travMax)) +
  geom_point(aes(size = n, alpha=p, color=PCS42S), shape = 15) +
  scale_size_binned_area(max_size = 10, name = "nombre\nd'enquêté·es") +
  scale_alpha(name = "part des\nenquêté·es (%)\npar type de com.\nde résidence") +
  scale_colour_manual(name = "PCS", values = c("navyblue", "brown")) +
  facet_grid(~PCS42S) +
  xlab("Position dans l'AAV, commune de résidence") +
  ylab("Position dans l'AAV, commune de travail") +
  labs(title = "Comparaison du type de commune de résidence et de travail",
       subtitle = "Ouvrier·es et cadres parcourant plus de 150 km/journée de travail,\ntype intensif simple",
       caption = src_fig()) +
  theme_bw()
off()

# Quelles activités sont pratiquées au cours de la journée, au-delà du travail ?
pop = PER_ff %>% group_by(PCS42S) %>% summarise(pop = n())

sortie("Distances/Navettes longues distances, cadres vs ouvrier·es selon activités")
ACT %>%
  left_join(select(PER, uid_PER, PCS42S), by="uid_PER") %>%
  filter(uid_PER %in% PER_ff$uid_PER, PCS42S %in% c("36", "61")) %>%
  filter(!substr(Tache,1,1) %in% c("0", "1", "8", "9")) %>%
  mutate(TacheDom = etqMotifActiv_Dom(substr(Tache,1,1))) %>%
  mutate(Tache = etqMotifActiv(substr(Tache,1,2))) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S, TacheDom, Tache, uid_PER) %>% summarise(n = n()) %>%
  group_by(PCS42S, TacheDom, Tache) %>% summarise(n = n()) %>%
  left_join(mutate(pop, PCS42S = etqPCS42S(PCS42S)), by="PCS42S") %>%
  mutate(p = n / pop * 100) %>%
  tab_Tri(i = "Tache", parCol = "p", rev=T) %>%
  ggplot(aes(x = Tache, y = p)) +
  geom_col(aes(fill = PCS42S)) +
  facet_grid(TacheDom~PCS42S, scales = "free", space = "free_y") +
  scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  ylab("Part de la population (%)") + xlab("Activité") +
  coord_flip(ylim = c(0, 50)) +
  geom_text(aes(label = paste0(round(p, 1), " %")), hjust=-.25) +
  labs(title = "Activités pratiquées selon la PCS, hors travail",
       subtitle = "Individus parcourant plus de 150 km en une journée",
       caption = src_fig(date = "janvier 2023")) +
  theme(legend.position = "bottom")
off()



# Quels sont les modes utilisés ?
pop = PER_ld_cp %>%
  mutate(dsQuartiles = discretisation(dsDom, methode = "quartiles")) |>
  group_by(dsQuartiles, PCS42S) %>% summarise(pop = n())


sortie("Distances/Navettes longues distances, cadres vs ouvrier·es selon modes", taille = "carré")
PER_ld_cp %>%
  mutate(dsQuartiles = discretisation(dsDom, methode = "quartiles")) |>
  pivot_longer(cols = starts_with("modes_"), names_to = "mode", values_to = "pMode") %>%
  filter(pMode != "non") %>%
  filter(mode != "modes_motor") %>%
  filter(mode != "modes_tc") %>%
  mutate(mode = plyr::revalue(mode, c("modes_voiture" = "voiture", "modes_marche" = "marche", 
                                      "modes_tc_rail" = "train", 
                                      "modes_tc_route" = "bus/car", "modes_tc_light" = "tramway",
                                      "modes_métro" = "métro", "modes_vélo" = "vélo",
                                      "modes_drm" = "moto/scooter", "modes_trott" = "trottinette",
                                      "modes_bateau" = "bateau"))) %>%
  filter(!is.na(dsQuartiles)) |>
  group_by(dsQuartiles, PCS42S, mode) %>% summarise(n = n()) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  left_join(mutate(pop, PCS42S = etqPCS42S(PCS42S)), by=c("PCS42S" = "PCS42S", "dsQuartiles" = "dsQuartiles")) %>%
  ungroup() |>
  mutate(dsQuartiles = factor(dsQuartiles, labels = paste0(levels(dsQuartiles),
                                                           "\n", c("1er", "2e", "3e", "4e"),
                                                           " quartile"))) |>
  group_by(dsQuartiles) %>%
  mutate(p = n / pop * 100) %>%
  tab_Tri(i = "mode", parCol = "p") %>%
  ggplot(aes(x = mode, y = p)) + geom_col(aes(fill = PCS42S)) +
  facet_grid(dsQuartiles~PCS42S) +
  scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  ylab("Part de la population (%)") + xlab("Mode de transport") +
  coord_flip(ylim = c(0, 100)) +
  geom_text(aes(label = paste0(round(p, 1), " %")), hjust=-.15, size=3) +
  labs(title = "Modes utilisés dans la journée par les navetteur·ses",
       subtitle = "Cadres du privé et ouvrier·es qualifié·es\nparcourant plus de 166 km en une journée",
       caption = src_fig(date = "janvier 2023")) +
  theme(legend.position = "bottom")
off()

# Quels sont les véhicules utilisés ?

sortie("Distances/Navettes longues distances, parc automobile")
PER_ld_cp %>%
  left_join(VEH, by = "uid_VEH", suffix = c("", "Veh")) %>%
  filter(AgeVeh<50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S, AgeVeh) %>% summarize(n = n()) %>%
  group_by(PCS42S) %>% mutate(p = n / sum(n)) %>%
  ggplot(aes(x = AgeVeh, y = p)) + geom_col(aes(fill = PCS42S, group=PCS42S), position = "dodge") +
  scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  xlab("Âge du véhicule") + ylab("Part du parc automobile") +
  labs(title="Comparaison du parc automobile des navetteur·ses",
       subtitle="Navetteur·ses sur plus de 150 km,\ncadres du privé et ouvrier·es qualifié·es",
       caption=src_fig(date = "janvier 2023")) +
  theme_bw() + theme(legend.position="bottom")
off()

sortie("Distances/Navettes longues distances, âge des véhicules")
PER_ld %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  left_join(VEH, by = "uid_VEH", suffix = c("", "Veh")) %>%
  filter(AgeVeh<50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>% summarise(AgeMoy = mean(AgeVeh)) %>%
  tab_Tri(i = "PCS42S", parCol = "AgeMoy") %>%
  ggplot(aes(x = PCS42S, y = AgeMoy)) + geom_col() +
  # scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  xlab("PCS") + ylab("Âge moyen du véhicule") +
  coord_flip() +
  labs(title="Âge moyen du véhicule des navetteur⋅ses longu⋅es",
       subtitle="Navetteur·ses sur plus de 150 km",
       caption=src_fig(date = "janvier 2023")) +
  theme_bw() + theme(legend.position="bottom")
off()

sortie("Distance/Part du temps sur lieu de travail")
PER %>%
  filter(typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & Tps>0 & DuTvl > 0) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  mutate(ratioTvlDep = Tps/(1440-DuDom)) %>%
  group_by(PCS42S) %>%
  summarise(mRatio = mean(ratioTvlDep, na.rm=T)) %>%
  tab_Tri("PCS42S", parCol = "mRatio") %>%
  ggplot(aes(x = PCS42S, y=mRatio)) +
  #facet_wrap(~ZoneDens) +
  geom_col() +
  coord_flip() +
  theme_bw()
off()

PER = mutate(PER,
             typoPlages = case_when(duTvlPlage >= 360 &                    nbLxTvl == 1 ~ "intensif simple",
                                    duTvlPlage >= 120 & duTvlPlage < 360 & nbLxTvl == 1 ~ "intensif fractionné",
                                    duTvlPlage >= 120                    & nbLxTvl >  1 ~ "extensif séquentiel",
                                    duTvlPlage <  120                    & nbLxTvl >  1 ~ "extensif mobile",
                                    T                                                   ~ "autres"),
             typoPlages = factor(x = typoPlages,
                                 levels = c("intensif simple", "intensif fractionné", "extensif séquentiel",
                                            "extensif mobile", "autres")))

table(PER$typoPlages)

sortie("Distance/Part du temps sur lieu de travail selon typologie")
PER %>%
  filter(typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06") & Tps>0 & DuTvl > 0) %>%
  filter(typoPlages == "intensif simple" & ZoneDens == "1") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  mutate(ratioTvlDep = Tps/(1440-DuDom)) %>%
  group_by(PCS42S) %>%
  summarise(mRatio = mean(ratioTvlDep, na.rm=T)) %>%
  tab_Tri("PCS42S", parCol = "mRatio") %>%
  ggplot(aes(x = PCS42S, y=mRatio)) +
  #facet_wrap(~ZoneDens) +
  geom_col() +
  coord_flip() +
  theme_bw()
off()

# Prise en charge employeur =====

rapport("Analyse prise en charge employeur", prim = T)

load("Data/VEH.rds")

# Depuis la BU Cerema, on a deux indicateurs de prise en charge :
# - qui est le propriétaire du véhicule
# - est-ce que l'abonnement TC est payé par l'employeur
# - on pourrait aussi compter éventuellement le parking mais bof

# Plusieurs approches sont possibles
# - état des lieux de la prise en charge par catégorie
# - modèles pour voir l'impact de la prise en charge sur la distance
# - (différences entre tc et voiture)
# - (traitements pour voir si prise en charge = meilleur véhicule)

# Commencer par créer champ prise en charge
# sachant qu'on n'a pas l'info sur la pec des frais d'essence

# Commencer par trouver les enquêtes qui ne sont pas fiables
PER_PEC_excl = PER_ff |>
  mutate(n = 1) %>%
  pivot_wider(names_from = AboTC, values_from = n, names_prefix = "AboTC") %>%
  group_by(uid_ENQ) %>% summarise(across(starts_with("AboTC"), sum, na.rm=T)) %>%
  filter((AboTC5 > 0  | AboTC6 > 0 | AboTCNA > 0) & AboTC3 == 0)

PER_PEC = PER_ff %>%
  left_join(VEH, by="uid_VEH", suffix=c("", "Veh")) %>%
  mutate(pec_tc = case_when(!uid_ENQ %in% PER_PEC_excl$uid_ENQ & AboTC == "2" ~ "oui",
                            !uid_ENQ %in% PER_PEC_excl$uid_ENQ & AboTC == "3" ~ "non",
                            !uid_ENQ %in% PER_PEC_excl$uid_ENQ & AboTC == "1" ~ "non"),
         pec_veh = case_when(Propr == "oui" ~ "non",
                             ProprAutre %in% c("empl., totale", "empl., limitée") ~ "oui"))

sortie("Distances/Prise en charge abonnement TC")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8, rev = T)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(PCS8) %>% summarise(nPec = sum(compterPec, na.rm=T),
                               n = sum(compterTout, na.rm=T)) %>%
  group_by(PCS8) %>% mutate(oui = (nPec / sum(n)) * 100,
                            non = 100-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport", subtitle = "Abonnements de transports en commun",
       caption = src_fig()) +
  xlab("part des enquêté⋅es (%)") + ylab("PCS des enquêté⋅es") +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC selon Genre")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS8) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                      n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS8) %>% mutate(oui = (nPec / sum(n)) * 100,
                                   non = 100-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport", subtitle = "Abonnements de transports en commun",
       caption = src_fig()) +
  xlab("part des enquêté⋅es (%)") + ylab("PCS des enquêté⋅es") +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC en IDF")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui") %>%
  filter(uid_ENQ == "IDF2010") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés, IDF (2010) uniquement"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC chez usager·es")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(typoModes, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                            n = sum(compterTout, na.rm=T)) %>%
  group_by(typoModes, PCS42S) %>% mutate(oui = sum(nPec),
                                         non = sum(n)-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "nb") %>%
  ggplot(aes(x = PCS42S, y = nb)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("nombre d'enquêté·es)") +
  facet_wrap(~typoModes) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC par enquête")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  filter(modes_tc == "oui") %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Libelle_Long, PCS8) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                             n = sum(compterTout, na.rm=T)) %>%
  group_by(Libelle_Long, PCS8) %>% mutate(oui = (nPec / sum(n)) * 100,
                                          non = 100-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = Libelle_Long, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté·es") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_grid(~PCS8) +
  theme(legend.position = "bottom") %>% print()
off()

# quel mode pour les gens qui viennent en tc mais avec un abonnement qu'iels ont payé ?
sortie("Distances/Prise en charge abonnement TC selon TC")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui") %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "mode", values_to = "pMode") %>%
  filter(pMode != "non") %>%
  filter(mode != "modes_motor") %>%
  filter(mode != "modes_tc") %>%
  mutate(mode = plyr::revalue(mode, c("modes_voiture" = "voiture", "modes_marche" = "marche", 
                                      "modes_tc_rail" = "train", 
                                      "modes_tc_route" = "bus/car", "modes_tc_light" = "tramway",
                                      "modes_métro" = "métro", "modes_vélo" = "vélo",
                                      "modes_drm" = "moto/scooter", "modes_trott" = "trottinette",
                                      "modes_bateau" = "bateau"))) %>%
  group_by(pec_tc, PCS8, mode) %>% summarise(n = n()) %>%
  group_by(pec_tc, PCS8) %>% mutate(p = n / sum(n) * 100) %>%
  tab_Tri(i = "mode", parCol = "p") %>%
  ggplot(aes(x = mode, y = p)) + geom_col(aes(fill = PCS8)) +
  facet_grid(pec_tc~PCS8) +
  scale_fill_manual(name = "PCS8", values = pal_PCS8[2:6]) +
  ylab("Part de la population (%)") + xlab("Mode de transport") +
  coord_flip(ylim = c(0, 100)) +
  geom_text(aes(label = paste0(round(p, 1), " %")), hjust=-.15, size=3) +
  labs(title = "Modes utilisés dans la journée par les navetteur·ses en transports en commun",
       subtitle = "Classé·es selon la prise en charge de leur abonnement de transports",
       caption = src_fig(date = "janvier 2023")) +
  theme_bw() + theme(legend.position = "bottom")
off()

# et si que gens qui prennent pas la voiture
sortie("Distances/Prise en charge abonnement TC chez usager·es exclusif·ves")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui" & modes_voiture == "non") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC selon PCS42S")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(AboTC == "2", 1, 0),
         compterTout = ifelse(AboTC %in% c("2","3"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC chez classes populaires")
PER_PEC %>%
  filter(PCS42S %in% c("55", "56", "61", "66")) %>%
  mutate(NivEtu = etqNivEtu(NivEtu), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(AboTC == "2", 1, 0),
         compterTout = ifelse(AboTC %in% c("2","3"), 1, 0)) %>%
  group_by(Genre, NivEtu) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, NivEtu) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = NivEtu, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC chez classes populaires selon âge")
PER_PEC %>%
  filter(PCS42S %in% c("55", "56", "61", "66")) %>%
  mutate(Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(AboTC == "2", 1, 0),
         compterTout = ifelse(AboTC %in% c("2","3"), 1, 0)) %>%
  group_by(Genre, Age10) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                       n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, Age10) %>% mutate(oui = (nPec / sum(n)) * 100,
                                    non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = Age10, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("Âge des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge abonnement TC selon type contrat")
PER_PEC %>%
  filter(PCS42S %in% c("55", "56", "61", "66")) %>%
  mutate(Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(AboTC == "2", 1, 0),
         compterTout = ifelse(AboTC %in% c("2","3"), 1, 0)) %>%
  group_by(Genre, Activ) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                       n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, Activ) %>% mutate(oui = (nPec / sum(n)) * 100,
                                    non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " %"), NA)) %>%
  ggplot(aes(x = Activ, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("Statut des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=-.15, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge TC")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_tc == "oui") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0(round(p,1), " % "), NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Abonnements de transports en commun pour les enquêté·es\nqui les ont empruntés"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("part des enquêté⋅es (%)") +
  geom_text(aes(label = ouiEtq), hjust=1, size=2.5) +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge Véhicule à disposition")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8, rev = T)) %>%
  mutate(compterPec = ifelse(pec_veh == "oui", 1, 0),
         compterTout = ifelse(pec_veh %in% c("oui","non"), 1, 0)) %>%
  group_by(PCS8) %>% summarise(nPec = sum(compterPec, na.rm=T),
                               n = sum(compterTout, na.rm=T)) %>%
  group_by(PCS8) %>% mutate(oui = (nPec / sum(n)) * 100,
                            non = 100-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  ggplot(aes(x = PCS8, y = p)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("véhicule à disposition")) +
  labs(title = "Prise en charge des frais de transport",
       subtitle = "Véhicule à disposition",
       caption = src_fig()) +
  xlab("part des enquêté⋅es (%)") + ylab("PCS des enquêté⋅es") +
  theme(legend.position = "bottom") %>% print()
off()

sortie("Distances/Prise en charge Véhicule de fonction")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_motor == "oui") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_veh == "oui", 1, 0),
         compterTout = ifelse(pec_veh %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = (nPec / sum(n)) * 100,
                                     non = 100-oui) %>%
  filter(n > 10) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "p") %>%
  filter(pec == "oui") %>%
  mutate(ouiEtq = ifelse(pec == "oui", paste0( round(p,1), " %"), NA)) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(position="stack", fill = "lightgreen") +
  coord_flip(ylim=c(0,100)) + theme_bw() +
  labs(subtitle = ml("Part des enquêté·es ayant utilisé majoritairement",
                     "un véhicule de fonction le jour d'enquête"),
       caption = src_fig(date="janvier 2023", emp = F)) +
  ylab("part des enquêté⋅es (%)") + xlab("PCS des enquêté⋅es") +
  geom_text(aes(label = ouiEtq), hjust=-.25, size=2.5) +
  facet_wrap(~Genre) %>% print()
off()

sortie("Distances/Prise en charge Véhicule de fonction, type")
PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_motor == "oui" & pec_veh == "oui") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compte = 1) %>%
  pivot_wider(names_from = Type, values_from = compte, names_prefix = "veh_") %>%
  group_by(Genre, PCS42S) %>% summarise(across(starts_with("veh_"), sum, na.rm=T)) %>%
  pivot_longer(cols = starts_with("veh_"), names_to = "Type", values_to = "compte") %>%
  group_by(Genre, PCS42S) %>% filter(sum(compte)>20) %>%
  mutate(p = compte / sum(compte) * 100) %>%
  mutate(Type = plyr::revalue(Type, c("veh_Sans permis" = "sans permis",
                                      "veh_Tourisme" = "tourisme", "veh_Utilitaire" = "utilitaire"))) %>%
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = Type), position = "stack") +
  coord_flip() + theme_bw() +
  labs(title="Type de véhicule de fonction mis à disposition",
       subtitle="Travailleur·ses bénéficiant d'un véhicule de fonction (si plus de 20 individus)",
       caption = src_fig(date = "février 2023", emp = F)) +
  ylab("part des enquêté·es (%)") + xlab("PCS des enquêté·es") +
  scale_fill_manual(values = c("orange", "lightgreen", "lightblue"), name = "type de véhicule") +
  facet_wrap(~Genre) %>% print()
off()

g1 = PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_motor == "oui" & pec_veh == "oui" & Type == "Tourisme") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(Puissance = ifelse(Puissance == 99, NA, as.integer(Puissance))) %>%
  group_by(Genre, PCS42S) %>% summarise(moy = median(Puissance, na.rm=T),
                                        q1 = weighted.quantile(Puissance, probs=.25, w=CoeffRecEnq),
                                        q3 = weighted.quantile(Puissance, probs=.75, w=CoeffRecEnq), n = n()) %>%
  filter(n>20) %>%
  ggplot(aes(x = PCS42S, y = moy)) + 
  geom_pointrange(aes(ymin = q1, ymax = q3)) +
  coord_flip() + theme_bw() +
  labs(title="Puissance du véhicule de fonction mis à disposition",
       subtitle="Travailleur·ses bénéficiant d'un véhicule de tourisme de fonction (si plus de 20 individus)",
       caption = src_fig(date = "février 2023", emp = F)) +
  ylab("puissance médiane du véhicule (chev. fiscaux)") + xlab("PCS des enquêté·es") +
  facet_wrap(~Genre)

g2 = PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  filter(modes_motor == "oui" & pec_veh == "oui" & Type == "Tourisme") %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  filter(AgeVeh < 50) %>%
  group_by(Genre, PCS42S) %>% summarise(moy = mean(AgeVeh, na.rm=T),
                                        ect = sd(AgeVeh, na.rm=T), n = n()) %>%
  filter(n>20) %>%
  ggplot(aes(x = PCS42S, y = moy)) + 
  geom_pointrange(aes(ymin = moy-ect, ymax = moy+ect)) +
  coord_flip() + theme_bw() +
  labs(title="Âge du véhicule de fonction mis à disposition",
       subtitle="Travailleur·ses bénéficiant d'un véhicule de tourisme de fonction (si plus de 20 individus)",
       caption = src_fig(date = "février 2023", emp = F)) +
  ylab("âge moyen du véhicule (années)") + xlab("PCS des enquêté·es") +
  facet_wrap(~Genre)

sortie("Distances/Prise en charge Propriétés véhicules", taille = "page", portrait = T)
cowplot::plot_grid(g1 + theme(legend.position="none"), nrow=2, g2, rel_heights = c(4.5,5.5), align = "v") %>%
  print()
off()


g1 = PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_tc == "oui", 1, 0),
         compterTout = ifelse(pec_tc %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = sum(nPec),
                                     non = sum(n)-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "nb") %>%
  ggplot(aes(x = PCS42S, y = nb)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge","abonnement TC")) +
  labs(title = "Prise en charge des frais de transport", subtitle = "Abonnements de transports en commun"
  ) +
  xlab("PCS des enquêté⋅es") + ylab("nombre d'enquêté⋅es") +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom")

g2 = PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T), Genre = etqGenre(Genre)) %>%
  mutate(compterPec = ifelse(pec_veh == "oui", 1, 0),
         compterTout = ifelse(pec_veh %in% c("oui","non"), 1, 0)) %>%
  group_by(Genre, PCS42S) %>% summarise(nPec = sum(compterPec, na.rm=T),
                                        n = sum(compterTout, na.rm=T)) %>%
  group_by(Genre, PCS42S) %>% mutate(oui = sum(nPec),
                                     non = sum(n)-oui) %>%
  pivot_longer(cols = c("oui", "non"), names_to = "pec", values_to = "nb") %>%
  ggplot(aes(x = PCS42S, y = nb)) + geom_col(aes(fill = pec), position="stack") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("orange", "lightgreen"), name=ml("prise en charge")) +
  labs(subtitle = "Véhicule à disposition",
       caption = src_fig(date="janvier 2023")) +
  ylab("nombre d'enquêté⋅es") + xlab("PCS des enquêté⋅es") +
  facet_wrap(~Genre) +
  theme(legend.position = "bottom")


sortie("Distances/Prise en charge Stock", taille = "page", portrait = T)
cowplot::plot_grid(g1 + theme(legend.position="none"), nrow=2, g2, rel_heights = c(4.5,5.5), align = "v") %>% print()
off()
remove(PER_PEC)

rapport("Modélisation de la probabilité d'avoir un véhicule de fonction")

modDep = filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                modes_voiture == "oui") %>%
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>%
  glm('pec_veh ~ PCS42S', data=.,
      family="binomial")
# rapport("pseudoR2 de McFadden, modèle véhicule de fonction selon PCS42S :", PseudoR2(modDep, which="McFadden"))


modDep = filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                modes_voiture == "oui") %>%
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>%
  glm('pec_veh ~ PCS42S + Genre', data=.,
      family="binomial")
#  rapport("pseudoR2 de McFadden, modèle véhicule de fonction selon PCS42S et genre :", PseudoR2(modDep, which="McFadden"))

modDep = filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                modes_voiture == "oui") %>%
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>%
  glm('pec_veh ~ Genre', data=.,
      family="binomial")
#  rapport("pseudoR2 de McFadden, modèle véhicule de fonction selon genre :", PseudoR2(modDep, which="McFadden"))

modDep = filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                typoJo == "TRAV", Activ %in% c("10", "11", "12"),
                modes_voiture == "oui", Age<70) %>%
  mutate(pec_veh = case_when(pec_veh == "oui" ~ 1, pec_veh == "non" ~ 0)) %>%
  glm('pec_veh ~ PCS42S + Genre + Age10 + Activ + ZoneDens + ZoneDens_travMax', data=.,
      family="binomial")
#  rapport("pseudoR2 de McFadden, modèle véhicule de fonction selon modèle complet :",PseudoR2(modDep, which="McFadden"))


modDep$AIC
efficaciteBinom(modDep, filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                               modes_voiture == "oui"), "pec_veh")


PER_PEC$PCS42S = relevel(PER_PEC$PCS42S, "54")

modDep = nnet::multinom("pec_veh ~ Age10 + Activ + Genre + PCS42S + ZoneDens + ZoneDens_travMax",
                        data= filter(PER_PEC, PCS8 %in% c("02", "03", "04", "05", "06"),
                                     modes_voiture == "oui"), weights=CoeffEnq)

modDep = PER_PEC %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06"), modes_voiture == "oui") %>%
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>% mutate(compte = 1) %>%
  filter(!is.na(pec_veh) & !is.na(Age10) & !is.na(Activ) & !is.na(Genre) & !is.na(PCS42S) &
           !is.na(ZoneDens) & !is.na(ZoneDens_travMax)) %>%
  glm("pec_veh ~ Age10 + Activ + Genre + PCS42S + ZoneDens + ZoneDens_travMax",
      data = .,
      family = "binomial")


summary(modDep)
confint(modDep)
exp(coef(modDep))

sortie("Distances/Prise en charge Véhicule de fonction, modèle logit", taille = "carré")
modDep = PER_PEC %>%
  valref() |>
  filter(modes_voiture == "oui", Type %in% c("Tourisme", "Utilitaire")) %>%
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  mutate(dsDom = factor(dsDom, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
         dsTvl = factor(dsTvl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile"))) |>
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>% mutate(compte = 1) %>%
  logit(val = "pec_veh", formule = "Age10 + Activ + Genre + PCS42S + dsDom + dsTvl", 
        valIntervalleSur100 = 5,
        titre = "Modèle logit portant sur la probabilité\nde disposer d'un véhicule de fonction",
        caption = src_fig(emp = F, date = "février 2023"),
        colComparaison = "Type", petit = T)
print(modDep)
off()

modDep = PER_PEC %>%
  valref() |>
  filter(Dis != 0) |>
  filter(modes_voiture == "oui", Type %in% c("Tourisme", "Utilitaire")) %>%
  mutate(dsDom = discretisation(dsDom, methode = "quartiles"),
         dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  mutate(dsDom = factor(dsDom, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile")),
         dsTvl = factor(dsTvl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile"))) |>
  mutate(Dis = log(Dis/10000)) |>
  mutate(pec_veh = ifelse(pec_veh == "oui", 1, 0)) %>% mutate(compte = 1) %>%
  logit(val = "pec_veh", formule = "Age10 + Activ + Genre + PCS42S + dsDom + dsTvl + Dis", 
        valIntervalleSur100 = 5,
        titre = "Modèle logit portant sur la probabilité\nde disposer d'un véhicule de fonction",
        caption = src_fig(emp = F, date = "février 2023"),
        colComparaison = "Type")

rapport(paste("AIC modèle :", modDep$AIC), info=T)

as.data.frame(exp(coef(modDep)))

# Carte de la distance potentielle =====

rapport("Distances potentielles", prim = T)

# Commençons avec la Loire Atlantique.
# Besoin d'une carte avec 1 point = 1 résident·e = une distance.

# head(shp_ZTS)

load("Data/shp_ZF.rds")
load("Data/shp_COM.rds")
library(potential)

# On charge une fois pour toutes le fond de carte
# Un problème complètement STUPIDE et irrésolu est causé par la conversion en 2154 de fdCarte dans
# certaines fonctions. < Fév 2023 >
# Solution temporaire = déplacer la conversion
fdCarte = map_initCarteParEnquete(simplifier = F, proj = 2154)

plot_inter(fun = "e", span = 10000, beta = 2, limit = 30000)

rapport("Analyse de la ressemblance selon la proximité des individus")

# Regardons si les points proches ont des valeurs qui se ressemblent
IDF2010 = PER %>%
  filter(uid_ENQ == "IDF2010") %>%
  filter(typoJo == "TRAV", PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  init_geomMen(shp_ZF, shp_COM) %>%
  st_transform(crs = 2154)
IDF2010 = filter(IDF2010, !st_is_empty(geometry))
rapport(nrow(IDF2010), "individus à traiter", info=T)


rapport("Analyse d'un échantillon de 1000 individus (pour accélérer le traitement)")
IDF2010 = IDF2010[sample(nrow(IDF2010), 1000),]

logCible = .1

moyennes = sapply(X = intervalles, FUN = intervallePointsRangeDis3, x = IDF2010, 
                  seuil = 10) %>%
  apply(MARGIN = 2, median, na.rm=T)

tab = tibble(range = intervalles, moyenne = moyennes)

rapport("Même opération depuis les lieux de travail", info=T)

IDF2010TRAV = IDF2010 %>%
  mutate(ZF = ZF_travMax, Com = Com_travMax, ZT = ZT_travMax) %>%
  st_drop_geometry() %>%
  init_geomMen(shp_ZF = shp_ZF, shp_COM = shp_COM)

nLieuTrav = nrow(filter(IDF2010TRAV, !st_is_empty(geometry)))

moyennesTrav = sapply(X = intervalles, FUN = intervallePointsRangeDis3, x = IDF2010TRAV, 
                      seuil = 10) %>%
  apply(MARGIN = 2, median, na.rm=T)

tab2 = tibble(range = intervalles, domicile = moyennes, travail = moyennesTrav) %>%
  pivot_longer(cols = c("domicile", "travail"), names_to = "quoi", values_to = "moyenne")

intervalle = c((exp(logCible * -1) - 1) * 100, (exp(logCible) - 1) * 100) %>%
  round(digits = 2)

sortie("Distances/Space matters, actually")
ggplot(data = tab2, aes(x = range / 1000, y = exp(moyenne))) + geom_line(aes(color = quoi)) +
  labs(title = ml("Différence logarithmique médiane entre les distances parcourues",
                  "par un·e enquêté·e et celle parcourue par les autres enquêté·es du voisinage,",
                  "défini par le lieu de résidence ou par le lieu d'emploi"),
       subtitle = "sous-échantillon aléatoire de 1000 individus",
       caption = src_fig(base = IDF2010)) +
  scale_color_manual(values = c("slateblue", "firebrick"), name = "distance\nprise en compte") +
  xlab("rayon des individus avec lesquelles la distance est comparée (km)") +
  ylab("différence logarithmique médiane")
off()

moyennesTps = sapply(X = intervalles, FUN = intervallePointsRangeTps, x = subsetIDF2010, logCible = logCible) %>%
  apply(MARGIN = 2, mean)

tabTps = tibble(range = intervalles, moyenne = moyennesTps)

intervalle = c((exp(logCible * -1) - 1) * 100, (exp(logCible) - 1) * 100) %>%
  round(digits = 2)


ggplot(data = tabTps, aes(x = range / 1000, y = moyenne * 100)) + geom_line() +
  labs(title = paste0("Part des individus dont le temps en déplacement est compris dans l'intervalle [", intervalle[1],
                      "% : +", intervalle[2], " %]\nselon la proximité de leurs lieux de résidence")) +
  xlab("rayon des individus avec lesquelles la distance est comparée (lieux de résidence, km)") +
  ylab("moyenne de la part des points inclus dans l'intervalle (%)") %>% print()


potential::plot_inter(fun = "p", span = 2500, beta=1, limit = 25000)

ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Région parisienne", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))


sortie("Distances/Distance moyenne potentielle - IDF")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Région parisienne", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))
off()

sortie("Distances/Distance moyenne potentielle - 44")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))
off()

sortie("Distances/Distance moyenne potentielle - Normandie")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Basse-Normandie", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))
off()

sortie("Distances/Distance moyenne potentielle - Lyonnais")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("BEB2017", "LYO2015"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Région lyonnaise", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))
off()

sortie("Distances/Distance moyenne potentielle - Marseille")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("MAR2009"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Bouches du Rhône", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))
off()

brks = c(15,30,45,60,75,90,105,120,135,150,200)
leg = ml("Temps passé","en déplacement","(en mn)")

carte1 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4", brks=brks,
                          titre = "Région parisienne", leg = leg,
                          credits = src_fig(emp = F))

carte2 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Loire-Atlantique", leg = leg,
                          credits = src_fig(emp = F))

carte3 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Basse-Normandie", leg = leg,
                          credits = src_fig(emp = F))

carte4 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("BEB2017", "LYO2015"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Région lyonnaise", leg = leg,
                          credits = src_fig(emp = F))

carte5 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("MAR2009"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "e", span = 5000, beta = 2, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Bouches du Rhône", leg = leg,
                          credits = src_fig(emp = F))

sortie("Distances/Temps passé en déplacement potentiel", taille = "a4", portrait = T)
cowplot::plot_grid(carte1 + theme(legend.position = "none"),
                   carte2 + theme(legend.position = "none"),
                   carte3 + theme(legend.position = "none"),
                   carte4 + theme(legend.position = "none"),
                   carte5 + theme(legend.position = "none"),
                   ggpubr::get_legend(carte1), nrow=3, ncol = 2) %>% print()
off()


brks = c(15,30,45,60,75,90,105,120,135,150,200)

ggCartePotentiel(PER = filter(PER, PCS8 == "03"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique : Cadres", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "04"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique : Prof. Interm.", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "05"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique : Employé·es", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "06"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique : Ouvrier·es", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "03"), shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Ile-de-France : Cadres", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "05"), shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Ile-de-France : Employé·es", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

ggCartePotentiel(PER = filter(PER, PCS8 == "06"), shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 7500, beta = 2, limit = 25000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Ile-de-France : Ouvrier·es", leg = "Distance\nmoyenne\npotentielle\n(en km)",
                 credits = src_fig(emp = F))

# En ajustant la fonction pour qu'elle ressemble plus à ce qu'on observe réellement :

# ce que je pensais quand j'étais bête
potential::plot_inter(fun = "p", span = 2500, beta = 2, limit = 25000)
# maintenant que je suis intelligent
plot_inter(fun = "e", span = 10000, beta = 2, limit = 30000)

brks = c(0:10) * 10

sortie("Distances/Distance potentielle IDF2010")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Région parisienne", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()

sortie("Distances/Distance potentielle LOI2015")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Loire-Atlantique", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()

sortie("Distances/Distance potentielle NORM")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Basse-Normandie", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()

sortie("Distances/Distance potentielle LYO2015")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("LYO2015"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Région lyonnaise", leg = "Distance\nmédiane\npotentielle\n(en km)", detailNoms = 4) %>% print()
off()

sortie("Distances/Distance potentielle MAR2009")
ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("MAR2009"), proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                 res_grille = 1000, facteurDiv = 1000, brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Bouches du Rhône", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()

sortie("Distances/Distance potentielle Nantes", width = 9, height = 5.5)
ggCartePotentiel(PER = filter(PER, substr(ZT,8,9) == "00"),
                 shp_ZT = filter(shp_ZT, substr(ZT,8,9) == "00"), enq = "LOI2015", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 1000, beta = 2, limit = 30000,
                 res_grille = 500, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Agglomération nantaise", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()

sortie("Distances/Distance potentielle Paris")
ggCartePotentiel(PER = filter(PER, substr(ZT,8,9) %in% c("75","92","93","94")),
                 shp_ZT = filter(shp_ZT, substr(ZT,8,9) %in% c("75","92","93","94")), enq = "IDF2010", proj = 2154,
                 var = "Dis", w = "CoeffEnq", fun = "e", span = 1000, beta = 2, limit = 30000,
                 res_grille = 500, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                 titre = "Paris et la petite couronne", leg = "Distance\nmédiane\npotentielle\n(en km)") %>% print()
off()



carte1 = ggCartePotentiel(PER = filter(PER, PCS8 == "03"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 30000,
                          res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                          titre = "Loire-Atlantique : Cadres", leg = "Distance\nmédiane\npotentielle\n(en km)",
                          credits = src_fig(emp = F), detailNoms = 3)

carte2 = ggCartePotentiel(PER = filter(PER, PCS8 == "04"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Dis", w = "CoeffEnq",fun = "e", span = 10000, beta = 2, limit = 25000,
                          res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                          titre = "Loire-Atlantique : Prof. Interm.", leg = "Distance\nmédiane\npotentielle\n(en km)",
                          credits = src_fig(emp = F), detailNoms = 3)

carte3 = ggCartePotentiel(PER = filter(PER, PCS8 == "05"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 25000,
                          res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                          titre = "Loire-Atlantique : Employé·es", leg = "Distance\nmédiane\npotentielle\n(en km)",
                          credits = src_fig(emp = F), detailNoms = 3)

carte4 = ggCartePotentiel(PER = filter(PER, PCS8 == "06"), shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Dis", w = "CoeffEnq", fun = "e", span = 10000, beta = 2, limit = 25000,
                          res_grille = 1000, facteurDiv = 1000,  brks=brks, coulBas = "lightcyan", coulHaut="turquoise4", 
                          titre = "Loire-Atlantique : Ouvrier·es", leg = "Distance\nmédiane\npotentielle\n(en km)",
                          credits = src_fig(emp = F), detailNoms = 3)

sortie("Distances/Planche DistancePCS", taille = "page", portrait = T)
cowplot::plot_grid(carte1 + theme(legend.position = "none") + labs(title = "Cadres", subtitle=NULL, caption=NULL),
                   carte2 + theme(legend.position = "none") + labs(title = "Prof. Interm.", subtitle=NULL, caption=NULL),
                   carte3 + theme(legend.position = "none") + labs(title = "Employé·es", subtitle=NULL, caption=NULL),
                   carte4 + theme(legend.position = "none") + labs(title = "Ouvrier·es", subtitle=NULL, caption=NULL),
                   ggpubr::get_legend(carte1 + theme(legend.position = "left") + guides(fill=FALSE)),
                   ggpubr::get_legend(carte1 + theme(legend.position = "bottom") + guides(color=FALSE)),
                   nrow=3, ncol = 2, rel_heights = c(2,2,1)) %>%
  viz_Titre("Distance médiane potentielle selon la PCS de l'enquêté·e\nFonction exponentielle, span = 10.000, beta = 2") %>% print()
off()

brks = c(5:15) * 10
leg = ml("Temps passé","en déplacement","(en mn)")

carte1 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "p", span = 2500, beta = 1, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4", brks=brks,
                          titre = "Région parisienne", leg = leg,
                          credits = src_fig(emp = F))

carte2 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154,
                          var = "Tps", w = "CoeffEnq",  fun = "p", span = 2500, beta = 1, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Loire-Atlantique", leg = leg,
                          credits = src_fig(emp = F))

carte3 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "p", span = 2500, beta = 1, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Basse-Normandie", leg = leg,
                          credits = src_fig(emp = F))

carte4 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("LYO2015", "BEB2017"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "p", span = 2500, beta = 1, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Région lyonnaise", leg = leg,
                          credits = src_fig(emp = F))

carte5 = ggCartePotentiel(PER = PER, shp_ZT = shp_ZT, enq = c("MAR2009"), proj = 2154,
                          var = "Tps", w = "CoeffEnq", fun = "p", span = 2500, beta = 1, limit = 25000,
                          res_grille = 1000, coulBas = "snow", coulHaut="tan4",  brks=brks,
                          titre = "Bouches du Rhône", leg = leg,
                          credits = src_fig(emp = F))

sortie("Distances/Temps en déplacement potentiel", taille = "a4", portrait = T)
cowplot::plot_grid(carte1 + theme(legend.position = "none"),
                   carte2 + theme(legend.position = "none"),
                   carte3 + theme(legend.position = "none"),
                   carte4 + theme(legend.position = "none"),
                   carte5 + theme(legend.position = "none"),
                   ggpubr::get_legend(carte1), nrow=3, ncol = 2) %>% print()
off()

# R&D Simulation des emplois potentiels =====

PER_emploi = PER %>%
  filter(typoJo == "TRAV" & PCS8 %in% c("02", "03", "04", "05", "06"))

realite = PER_emploi %>%
  mutate(classeDisEmploi = floor(Travail_Dis/1000)) %>%
  filter(classeDisEmploi<=100) %>%
  group_by(classeDisEmploi) %>% summarise(n = n()) %>%
  mutate(yReel = n / sum(n)) %>%
  rename(x = classeDisEmploi)

fp = function(x, beta, span)
{
  alpha <- (2^(1 / beta) - 1) / span
  x <- (1 + alpha * x)^(-beta)
  return(x)
}

theorie = tibble(x = c(1:100),
                 yTheo = fp(c(1:100), span = 2, beta = 1.5) / 5)

left_join(theorie, select(realite, x, yReel), by = "x") %>%
  pivot_longer(cols = c("yReel", "yTheo"), names_to = "modele", values_to = "y") %>%
  ggplot(aes(x = x, y = y)) + geom_line(aes(color = modele)) %>%
  print()

fmod = function(x)
{
  x = fp(x, span = 2, beta = 1.5) / 5
  return(x)
}

realitePCS = PER_emploi %>%
  mutate(classeDisEmploi = floor(Travail_Dis/1000)) %>%
  filter(classeDisEmploi<=50) %>%
  group_by(PCS8, classeDisEmploi) %>% summarise(n = n()) %>%
  group_by(PCS8) %>% mutate(yReel = n / sum(n)) %>%
  rename(x = classeDisEmploi)
ggplot(data = realitePCS, aes(x = x, y = yReel*100)) + geom_line(aes(color = PCS8)) +
  xlab("distance domicile/travail (km)") + ylab("part des enquêté·es (%)") +
  scale_color_manual(values = pal_PCS8[2:6], name = "PCS de l'enquêté·e") +
  labs(title = "Distance domicile-travail (en km) selon la PCS", caption = src_fig())

ggCartePotentielEmploi = function(PER, shp_ZT, enq, proj = 2154,
                                  w, fun, span, beta, limit,
                                  res_grille = 1000, brks= NA, facteurDiv=1,
                                  coulBas = "cadetblue3", coulZero = "snow1", coulHaut = "rosybrown",
                                  plotSemis = F, plotSemisNom = "Semis.svg",
                                  plotFonction = F, plotFonctionNom = "Fonction.svg",
                                  titre, leg, credits = src_fig(),
                                  carte = fdCarte, axes = T, detailNoms = 5,
                                  corrigerRapport = F)
{
  sortie = ifelse(length(enq) == 1, enq, paste0(length(enq), " enquêtes"))
  rapport("Carte de potentiel emploi/domicile :", sortie)
  
  fonction = paste0(ifelse(fun == "e", "fonction exponentielle ", ""),
                    ifelse(fun == "p", "fonction pareto ", ""),
                    "(span = ", span, ", beta = ", beta, ")")
  
  # Compte des stocks de population et d'emplois occupés par ZF
  domiciles = PER %>%
    filter(!is.na(ZF_travMax)) %>%
    group_by(Com, ZF) %>% summarise(nDom = sum(CoeffEnq, na.rm=T))
  
  travails = PER %>%
    filter(!is.na(ZF)) %>%
    group_by(Com_travMax, ZF_travMax) %>% summarise(nTvl = sum(CoeffEnq, na.rm=T))
  
  # Constitution d'un seul tableau, avec toutes les valeurs des unes et des autres
  tab = PER %>% group_by(Com,ZF) %>% summarise()
  tab = tab %>%
    left_join(domiciles, by=c("ZF" = "ZF", "Com" = "Com")) %>%
    left_join(travails, by=c("ZF" = "ZF_travMax", "Com" = "Com_travMax")) %>%
    mutate(nDom = ifelse(is.na(nDom), 0, nDom),
           nTvl = ifelse(is.na(nTvl), 0, nTvl))
  
  # Géométrie
  tab = init_geomMen(tab, shp_ZF, shp_COM) %>%
    st_transform(crs = proj)
  
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
  
  
  rapport("Calcul du potentiel", info=T)
  # Potentiel
  grille$potentielDom = mcpotential(x = tab, y = grille,
                                    var = "nDom",
                                    fun = fun, span = span, beta = beta, limit = limit,
                                    ncl = 2)
  grille$potentielTvl = mcpotential(x = tab, y = grille,
                                    var = "nTvl",
                                    fun = fun, span = span, beta = beta, limit = limit,
                                    ncl = 2)
  
  # Pour que le rapport ait du sens, il faudrait peut-être recalibrer le calcul pour simuler
  # autant de résident·es que de personnes en emploi ?
  # Cependant, rien ne prouve que cette déformation soit homogène dans l'espace, c'est là le hic
  grilleTest = st_intersection(grille, etendue)
  
  if (corrigerRapport)
  {
    popDom = sum(grilleTest$potentielDom)
    popTvl = sum(grilleTest$potentielTvl)
    
    grille$potentielTvl = grille$potentielTvl * (popDom/popTvl)
  }
  
  # Valeur positive si plus de résidences que d'emplois
  grille$potentiel = log(grille$potentielDom / grille$potentielTvl)
  grille$potentiel = ifelse(grille$potentiel == Inf, NA, grille$potentiel)
  print(summary(grille$potentiel))
  
  # Discrétisation
  if (is.na(brks))
  {
    rapport("Discrétisation optimale", info = T)
    
    grilleTest = st_intersection(grille, etendue)
    
    intervalle = max(grilleTest$potentiel, na.rm=T) - min(grilleTest$potentiel, na.rm=T)
    barresPosi = ifelse(max(grilleTest$potentiel, na.rm=T) > 0,
                        round(max(grilleTest$potentiel, na.rm=T) / intervalle * 10, 0),
                        0)
    barresNega = ifelse(min(grilleTest$potentiel, na.rm=T) < 0,
                        round(abs(min(grilleTest$potentiel, na.rm=T)) / intervalle * 10, 0),
                        0)
    
    bks <- c(seq(min(grilleTest$potentiel, na.rm=T), 0, length.out=barresNega),
             seq(0, max(grilleTest$potentiel, na.rm=T), length.out=barresPosi)) %>% unique()
    
  } else {bks = brks}
  
  cat("\nCategs :")
  print(bks)
  
  # Calcul des isozones
  grille_eq = equipotential(grille, var = "potentiel", breaks = bks, mask = etendue)
  
  rapport("Traçage de la carte", info=T)
  # Carte
  g = ggplot(data = grille_eq) +
    geom_sf(aes(fill = min), color = "grey70", size=.1) +
    scale_fill_gradient2(low = coulBas, mid = coulZero, high = coulHaut, midpoint = 0,
                         name = leg,
                         breaks = bks, limits = c(brks[1], brks[length(brks)]),
                         guide = "legend")
  
  # Calques lignes
  if (axes)
  {
    boite = st_bbox(etendue)
    diagonale = matrix(c(boite$xmin, boite$ymin, boite$xmax, boite$ymax), ncol=2, byrow=T) %>%
      list() %>% st_multilinestring() %>% st_length()
    
    eau = carte$shpEau %>%
      st_transform(crs = proj) %>%
      st_crop(y = st_bbox(st_buffer(etendue, dist = .5*diagonale)))
    g = g + geom_sf(data = eau, fill = "lavender", color = NA)
    
    ra = carte$shpRail %>%
      st_transform(crs = proj) %>%
      st_intersection(etendue) %>%
      st_simplify(dTolerance = 10000) %>%
      mutate(type = "voies ferrées") %>%
      select(type, geometry)
    r2 = carte$shpRoutes2 %>%
      st_transform(crs = proj) %>%
      st_intersection(etendue) %>%
      st_simplify(dTolerance = 10000) %>%
      mutate(type = "routes secondaires") %>%
      select(type, geometry)
    r1 = carte$shpRoutes1 %>%
      st_transform(crs = proj) %>%
      st_intersection(etendue) %>%
      st_simplify(dTolerance = 10000) %>%
      mutate(type = "routes majeures") %>%
      select(type, geometry)
    axes = rbind(ra, r2, r1)
    g = g + geom_sf(data = axes, aes(color = type), size=.4, alpha=.4) +
      scale_color_manual(values = c("brown", "grey40", "grey30"),
                         name = "Axes de transport") +
      guides(color = guide_legend(override.aes = list(fill = NA)))
    
    
    # Etiquetage avec un carroyage : but = afficher le lieu de peuplement le plus big de
    # chaque carreau
    grilleNoms = st_make_grid(x = etendue, n = c(detailNoms,detailNoms), square=F)
    grilleNoms = tibble(id = c(1:length(grilleNoms)), geometry = grilleNoms) %>% st_as_sf()
    noms = filter(carte$geonames, substr(feature_code,1,2) == "PP" & population > 0 | name == "Paris") %>%
      st_transform(crs = proj) %>%
      st_intersection(y = grilleNoms) %>%
      group_by(id) %>%
      mutate(jeSuisMax = ifelse(max(population) == population, T, F)) %>%
      filter(jeSuisMax == T) %>%
      st_intersection(y = etendue)
    
    g = g + geom_sf_text(data = noms, aes(label = name), size=3, check_overlap = T)
  }
  
  g = g + labs(title = titre, subtitle = fonction, caption = credits) +
    coord_sf(xlim = c(as.double(st_bbox(etendue)$xmin), as.double(st_bbox(etendue)$xmax)),
             ylim = c(as.double(st_bbox(etendue)$ymin), as.double(st_bbox(etendue)$ymax))) +
    ggspatial::annotation_scale(location="bl") +
    ggRetirerAxeX + ggRetirerAxeY
  
  return(g)
}

base = PER %>% filter(typoJo == "TRAV", PCS8 %in% c("02", "03", "04", "05", "06"))
leg = ml("Rapport logarithmique","entre le potentiel", "d'emploi et de résid.")

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 2000, beta = 1.1, limit = 25000,
                       titre = "Île de France", leg = leg)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 2000, beta = 1.1, limit = 25000,
                       titre = "Loire-Atlantique", leg = leg)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LYO2015", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 2000, beta = 1.1, limit = 25000,
                       titre = "Région lyonnaise", leg = leg)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"),
                       proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 2000, beta = 1.1, limit = 25000,
                       titre = "Haute-Normandie", leg = leg)



ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154, w = "CoeffEnq",
                       fun = "e", span = 2500, beta = 2, limit = 25000,
                       titre = "Île de France", leg = leg)


ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 2000, beta = 1.1, limit = 25000,
                       titre = "Île de France", leg = leg, corrigerRapport = T)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 5000, beta = 1.1, limit = 25000,
                       titre = "Île de France", leg = leg, corrigerRapport = T)

# En fait, les grands cratères correspondent probablement à la limite de l'indicateur (rayon 25 km)
# Ce n'est pas raisonnable d'utiliser cet indicateur, les villes pèsent trop lourd
# Elles déforment l'indice même à très grande distance
# Il faut utiliser une fonction exponentielle ici

potential::plot_inter(fun = "p", span = 5000, beta=1.1, limit = 25000)
potential::plot_inter(fun = "p", span = 2500, beta=2, limit = 25000)
potential::plot_inter(fun = "e", span = 2000, beta=2, limit = 25000)
potential::plot_inter(fun = "p", span = 5000, beta=100, limit = 100000)
potential::plot_inter(fun = "p", span = 2000, beta=1000, limit = 50000)

potEmpIdf = ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "IDF2010", proj = 2154, w = "CoeffEnq",
                                   fun = "p", span = 2000, beta=1000, limit = 50000,
                                   titre = "Île de France", leg = leg, corrigerRapport = T)

potEmp44 = ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                  fun = "p", span = 2000, beta=1000, limit = 50000,
                                  titre = "Loire-Atlantique", leg = leg, corrigerRapport = T)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 1000, beta=1000, limit = 50000,
                       titre = "Loire-Atlantique", leg = leg, corrigerRapport = T)

ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                       fun = "p", span = 4000, beta=10^10, limit = 50000,
                       titre = "Loire-Atlantique", leg = leg, corrigerRapport = T)

potEmpLyon = ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = "LYO2015", proj = 2154, w = "CoeffEnq",
                                    fun = "e", span = 2500, beta = 1000, limit = 50000,
                                    titre = "Région lyonnaise", leg = leg, corrigerRapport = T)

potEmpNorm = ggCartePotentielEmploi(PER = base, shp_ZT = shp_ZT, enq = c("ALE2018", "CHE2016", "CAL2011"),
                                    proj = 2154, w = "CoeffEnq",
                                    fun = "p", span = 2500, beta = 1000, limit = 50000,
                                    titre = "Haute-Normandie", leg = leg)



# Par categ :
brks = c(-2,-1.5,-1,-.5,0,.5,1,1.5,2,2.5,3,3.5,4,4.5)

carte1 = potEmp44_cadres = ggCartePotentielEmploi(PER = filter(base, PCS8 == "03"),
                                                  shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                                  fun = "p", span = 2000, beta=1000, limit = 50000, brks=brks,
                                                  titre = "Loire-Atlantique : Cadres", leg = leg, corrigerRapport = T)
carte2 = potEmp44_intm = ggCartePotentielEmploi(PER = filter(base, PCS8 == "04"),
                                                shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                                fun = "p", span = 2000, beta=1000, limit = 50000, brks=brks,
                                                titre = "Loire-Atlantique : Prof. Interm.", leg = leg, corrigerRapport = T)
carte3 = potEmp44_emp = ggCartePotentielEmploi(PER = filter(base, PCS8 == "05"),
                                               shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                               fun = "p", span = 2000, beta=1000, limit = 50000, brks=brks,
                                               titre = "Loire-Atlantique : Employé·es", leg = leg, corrigerRapport = T)
carte4 = potEmp44_ouv = ggCartePotentielEmploi(PER = filter(base, PCS8 == "06"),
                                               shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                               fun = "p", span = 2000, beta=1000, limit = 50000, brks=brks,
                                               titre = "Loire-Atlantique : Ouvrier·es", leg = leg, corrigerRapport = T)

sortie("Distances/Planche PotEmploi", taille = "page", portrait = T)
cowplot::plot_grid(carte1 + theme(legend.position = "none") + labs(title = "Cadres", subtitle=NULL, caption=NULL),
                   carte2 + theme(legend.position = "none") + labs(title = "Prof. Interm.", subtitle=NULL, caption=NULL),
                   carte3 + theme(legend.position = "none") + labs(title = "Employé·es", subtitle=NULL, caption=NULL),
                   carte4 + theme(legend.position = "none") + labs(title = "Ouvrier·es", subtitle=NULL, caption=NULL),
                   ggpubr::get_legend(carte1 + theme(legend.position = "bottom") + guides(fill=FALSE)),
                   ggpubr::get_legend(carte1 + theme(legend.position = "bottom") + guides(color=FALSE)),
                   nrow=3, ncol = 2, rel_heights = c(2,2,.5)) %>%
  viz_Titre("Irrégularité de répartition potentielle entre les emplois et les résidences\nFonction exponentielle, span = 10.000, beta = 2") %>% print()
off()


carte1 = ggCartePotentielEmploi(PER = filter(base, PCS8 == "03"),
                                shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                fun = "e", span = 5000, beta=2, limit = 50000, brks=brks,
                                titre = "Loire-Atlantique : Cadres", leg = leg, corrigerRapport = T)
carte2 = ggCartePotentielEmploi(PER = filter(base, PCS8 == "04"),
                                shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                fun = "e", span = 5000, beta=2, limit = 50000, brks=brks,
                                titre = "Loire-Atlantique : Prof. Interm.", leg = leg, corrigerRapport = T)
carte3 = ggCartePotentielEmploi(PER = filter(base, PCS8 == "05"),
                                shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                fun = "e", span = 5000, beta=2, limit = 50000, brks=brks,
                                titre = "Loire-Atlantique : Employé·es", leg = leg, corrigerRapport = T)
carte4 = ggCartePotentielEmploi(PER = filter(base, PCS8 == "06"),
                                shp_ZT = shp_ZT, enq = "LOI2015", proj = 2154, w = "CoeffEnq",
                                fun = "e", span = 5000, beta=2, limit = 50000, brks=brks,
                                titre = "Loire-Atlantique : Ouvrier·es", leg = leg, corrigerRapport = T)

sortie("Distances/Planche PotEmploi 44", taille = "page", portrait = T)
cowplot::plot_grid(carte1 + theme(legend.position = "none") + labs(title = "Cadres", subtitle=NULL, caption=NULL),
                   carte2 + theme(legend.position = "none") + labs(title = "Prof. Interm.", subtitle=NULL, caption=NULL),
                   carte3 + theme(legend.position = "none") + labs(title = "Employé·es", subtitle=NULL, caption=NULL),
                   carte4 + theme(legend.position = "none") + labs(title = "Ouvrier·es", subtitle=NULL, caption=NULL),
                   ggpubr::get_legend(carte1 + theme(legend.position = "bottom") + guides(fill=FALSE)),
                   ggpubr::get_legend(carte1 + theme(legend.position = "bottom") + guides(color=FALSE)),
                   nrow=3, ncol = 2, rel_heights = c(2,2,.5)) %>%
  viz_Titre("Irrégularité de répartition potentielle entre les emplois et les résidences\nFonction exponentielle, span = 10.000, beta = 2") %>% print()
off()

# Accompagnement ====

load("Data/DEP.rds") ; load("Data/MEN.rds")

sortie("Horaires/Accompagnement, proportion selon genre")
DEP %>%
  mutate(DisAcc = ifelse(D_Motif %in% as.character(c(61:79)), Dis, 0)) %>%
  group_by(uid_PER) %>% summarise(Dis = sum(Dis, na.rm= T),
                                  DisAcc = sum(DisAcc, na.rm=T)) %>%
  mutate(pAcc = DisAcc/Dis*100) %>% select(-Dis) %>%
  left_join(PER, by="uid_PER") %>%
  filter(typoJo == "TRAV") %>%
  filter(!is.na(Age10), !is.na(PCSMT), !is.na(Genre), !PCSMT %in% c("7i", "7e"), Age>16, Age<70) %>%
  mutate(PCSMT = etqPCSM(PCSMT, dét = T, retourChariot = T),
         Genre = etqGenre(Genre)) %>%
  group_by(Age10, PCSMT, Genre) %>%
  summarise(pAcc = mean(pAcc, na.rm=T)) %>%
  pivot_wider(names_from = Genre, values_from = pAcc) %>%
  mutate(ecGenre = (Femme / Homme - 1) * 100) %>%
  pivot_longer(cols = c("Femme", "Homme"), names_to = "Genre", values_to = "pAcc") %>%
  ggplot(aes(x = Genre, y = pAcc)) +
  geom_col(aes(fill = ecGenre)) +
  scale_fill_gradient2(high = "coral", mid = "grey", low = "thistle",
                       name = "rapport\npart allouée\nà l'accomp.\nchez les femmes\npar rapport à\nchez les hommes\n(en %)") +
  facet_grid(Age10~PCSMT) +
  coord_flip() %>% print()
off()

# Même graphique, selon âge du plus jeune enfant

sortie("Horaires/Accompagnement, proportion selon genre et âge enfant")
DEP %>%
  mutate(DisAcc = ifelse(D_Motif %in% as.character(c(61:79)), Dis, 0)) %>%
  group_by(uid_PER) %>% summarise(Dis = sum(Dis, na.rm= T),
                                  DisAcc = sum(DisAcc, na.rm=T),
                                  uid_MEN = first(uid_MEN)) %>%
  mutate(pAcc = DisAcc/Dis*100) %>% select(-Dis) %>%
  left_join(select(MEN, uid_MEN, enfPlusJeune), by="uid_MEN") %>%
  left_join(PER, by="uid_PER") %>%
  filter(typoJo == "TRAV") %>%
  filter(!is.na(enfPlusJeune), !is.na(PCSMT), !is.na(Genre), !PCSMT %in% c("7i", "7e"), Age>16, Age<70) %>%
  mutate(PCSMT = etqPCSM(PCSMT, dét = T, retourChariot = T),
         Genre = etqGenre(Genre)) %>%
  group_by(enfPlusJeune, PCSMT, Genre) %>%
  summarise(pAcc = mean(pAcc, na.rm=T)) %>%
  pivot_wider(names_from = Genre, values_from = pAcc) %>%
  mutate(ecGenre = (Femme / Homme - 1) * 100) %>%
  pivot_longer(cols = c("Femme", "Homme"), names_to = "Genre", values_to = "pAcc") %>%
  ggplot(aes(x = enfPlusJeune, y = pAcc)) +
  geom_col(aes(fill = ecGenre)) +
  scale_fill_gradient2(high = "coral", mid = "grey", low = "thistle",
                       name = "rapport\npart allouée\nà l'accomp.\nchez les femmes\npar rapport à\nchez les hommes\n(en %)") +
  facet_grid(Genre~PCSMT) +
  coord_flip() %>% print()
off()

# En croisant à présent la PCS8, ce qui sera somme toute plus large
pAcc = DEP %>%
  mutate(DisAcc = ifelse(D_Motif %in% as.character(c(61:79)) | O_Motif %in% as.character(c(61:79)), Dis, 0)) %>%
  group_by(uid_PER) %>% summarise(Dis = sum(Dis, na.rm= T),
                                  DisAcc = sum(DisAcc, na.rm=T)) %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, uid_MEN, uid_PER, CoeffRecEnq, Genre, Activ, PCS8, dsDom, dsDomEtq), by="uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(pAcc = DisAcc/Dis*100)

# Eviter les valeurs extrêmes ?
max(pAcc$DisAcc) / 1000

weighted.quantile(pAcc$DisAcc, probs = c(.5, .95, .99), w = pAcc$CoeffRecEnq) / 1000
seuil = weighted.quantile(pAcc$DisAcc, probs = .99, w = pAcc$CoeffRecEnq)
pAcc = filter(pAcc, DisAcc < seuil)

rapport("Moyenne distance accompagnement :", weighted.mean(pAcc$DisAcc, na.rm=T, w = pAcc$CoeffRecEnq)/1000, "km", info=T)
rapport("Médiane distance accompagnement (non nulle) :",
        median(filter(pAcc, DisAcc>0)$DisAcc, na.rm=T)/1000, "km", info=T)

s1= weighted.mean(pAcc$pAcc, w=pAcc$CoeffRecEnq, na.rm=T)

s2= weighted.mean(filter(pAcc, Genre == "H")$pAcc, w=filter(pAcc, Genre == "H")$CoeffRecEnq, na.rm=T)
s3= weighted.mean(filter(pAcc, Genre == "F")$pAcc, w=filter(pAcc, Genre == "F")$CoeffRecEnq, na.rm=T)

s4= weighted.mean(filter(pAcc, Genre == "H")$DisAcc, w=filter(pAcc, Genre == "H")$CoeffRecEnq, na.rm=T)
s5= weighted.mean(filter(pAcc, Genre == "F")$DisAcc, w=filter(pAcc, Genre == "F")$CoeffRecEnq, na.rm=T)

s6= weighted.mean(filter(pAcc, Activ %in% c("10", "11"), Genre == "H")$DisAcc,
                  w=filter(pAcc, Activ %in% c("10", "11"), Genre == "H")$CoeffRecEnq, na.rm=T)
s7= weighted.mean(filter(pAcc, Activ %in% c("10", "11"), Genre == "F")$DisAcc,
                  w=filter(pAcc, Activ %in% c("10", "11"), Genre == "F")$CoeffRecEnq, na.rm=T)

s8= weighted.mean(filter(pAcc, Genre == "H")$Dis,
                  w=filter(pAcc, Genre == "H")$CoeffRecEnq, na.rm=T)
s9= weighted.mean(filter(pAcc, Genre == "F")$Dis,
                  w=filter(pAcc, Genre == "F")$CoeffRecEnq, na.rm=T)

rapport("Part distance consacrée à l'accompagnement lors journée de travail :", round(s1, 2), "%", info=T)
rapport("Ce chiffre est de", round(s2,2), "% pour les hommes,", round(s3,2), "% pour les femmes", info = T)
rapport("Les hommes parcourent en moyenne", round(s4/1000,2), "km pour accompagnement", info = T)
rapport("Les femmes parcourent en moyenne", round(s5/1000,2), "km pour accompagnement", info = T)
rapport("Les hommes actifs qui ne travaillent pas ce jour là :", round(s6/1000,2), "km", info=T)
rapport("Les femmes actives qui travaillent bel et bien ce jour-là :", round(s7/1000,2), "km", info=T)
rapport("Pour rappel, les hommes qui travaillent ce jour là parcourent en moyenne", round(s8/1000,2),
        "km et les femmes", round(s9/1000,2), "km", info=T)

pAcc %>%
  group_by(dsDomEtq) %>% summarise(pAcc = weighted.mean(pAcc, w=CoeffRecEnq, na.rm=T))

pAcc %>%
  group_by(PCS8, Genre) %>% summarise(pAcc = mean(DisAcc/1000, na.rm=T)) %>% filter(PCS8 != "01")

pAcc %>%
  left_join(select(MEN, uid_MEN, enfPlusJeune), by="uid_MEN") %>%
  filter(enfPlusJeune<16) %>%
  group_by(Genre) %>% summarise(pAcc = mean(pAcc, na.rm=T))

pAcc %>%
  left_join(select(MEN, uid_MEN, enfPlusJeune), by="uid_MEN") %>%
  filter(enfPlusJeune<16) %>%
  group_by(PCS8) %>% summarise(pAcc = mean(pAcc, na.rm=T))

pAcc %>%
  left_join(select(MEN, uid_MEN, enfPlusJeune), by="uid_MEN") %>%
  filter(enfPlusJeune<16) %>%
  group_by(dsDomEtq) %>% summarise(pAcc = mean(pAcc, na.rm=T))

baseAcc = PER_ff %>%
  left_join(select(pAcc, uid_PER, DisAcc, pAcc), by="uid_PER") %>%
  mutate(PCS8_H = factor(
    case_when(Lien %in% c("1", "2") & Genre == "H" & Activ != "31" & !is.na(PCS8) ~ as.character(PCS8),
              Lien %in% c("1", "2") & Genre == "H" & Activ == "31" & !is.na(PCS8) ~ "08",
              Lien %in% c("1", "2") & Genre == "H" & is.na(Activ) & !is.na(PCS8) ~ "00",
              Lien %in% c("1", "2") & Genre == "H" & is.na(PCS8) ~ "00"),
    levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09")
  ),
  pAcc_H = case_when(Lien %in% c("1", "2") & Genre == "H" ~ pAcc),
  DisAcc_H = case_when(Lien %in% c("1", "2") & Genre == "H" ~ DisAcc),
  typo_H = case_when(Lien %in% c("1", "2") & Genre == "H" ~ typoJo),
  PCS8_F = factor(
    case_when(Lien %in% c("1", "2") & Genre == "F" & Activ != "31" & !is.na(PCS8) ~ as.character(PCS8),
              Lien %in% c("1", "2") & Genre == "F" & Activ == "31" & !is.na(PCS8) ~ "08",
              Lien %in% c("1", "2") & Genre == "F" & is.na(Activ) & !is.na(PCS8) ~ "00",
              Lien %in% c("1", "2") & Genre == "F" & is.na(PCS8) ~ "00"),
    levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09")
  ),
  pAcc_F = case_when(Lien %in% c("1", "2") & Genre == "F" ~ pAcc),
  DisAcc_F = case_when(Lien %in% c("1", "2") & Genre == "F" ~ DisAcc),
  typo_F = case_when(Lien %in% c("1", "2") & Genre == "F" ~ typoJo)) %>%
  group_by(uid_MEN) %>%
  summarise(PCS8_H = first(na.omit(PCS8_H)), pAcc_H = first(na.omit(pAcc_H)),
            typo_H = first(na.omit(typo_H)), DisAcc_H = first(na.omit(DisAcc_H)),
            PCS8_F = first(na.omit(PCS8_F)), pAcc_F = first(na.omit(pAcc_F)),
            typo_F = first(na.omit(typo_F)), DisAcc_F = first(na.omit(DisAcc_F))) %>%
  left_join(select(MEN, uid_MEN, enfPlusJeune, EnqMeth), by="uid_MEN")

# On peut mettre aes(alpha = n) pour obtenir une vision un peu de la représentativité de chaque croisement

baseAcc_0 = baseAcc %>%
  filter(EnqMeth == "F") %>%
  filter((PCS8_H %in% c("02", "03", "04", "05", "06") & PCS8_F %in% c("02", "03", "04", "05", "06")) |
           (PCS8_H %in% c("02", "03", "04", "05", "06") & is.na(PCS8_F)) |
           (PCS8_F %in% c("02", "03", "04", "05", "06") & is.na(PCS8_H))) %>%
  mutate(PCS8_H = paste0("Homme\n", etqPCS8(PCS8_H, genre = "H")),
         PCS8_F = paste0("Femme\n", etqPCS8(PCS8_F, genre = "F"))) %>%
  mutate(enfPlusJeune = case_when(enfPlusJeune >= 0 & enfPlusJeune <= 3 ~ "0 à 3 ans",
                                  enfPlusJeune >= 4 & enfPlusJeune <= 6 ~ "4 à 6 ans",
                                  enfPlusJeune >= 7 & enfPlusJeune <= 9 ~ "7 à 9 ans",
                                  enfPlusJeune >=10 & enfPlusJeune <=12 ~ "10 à 12 ans",
                                  enfPlusJeune >=13 & enfPlusJeune <=15 ~ "13 à 15 ans",
                                  T ~ "autres cas")) %>%
  filter((typo_H == "TRAV" & typo_F == "TRAV") |
           (typo_H == "TRAV" & PCS8_F == "Femme\nNA") |
           (typo_F == "TRAV" & PCS8_H == "Homme\nNA")) %>%
  group_by(PCS8_F, PCS8_H, enfPlusJeune) %>%
  summarise(pAcc_H = mean(pAcc_H, na.rm=T),
            pAcc_F = mean(pAcc_F, na.rm=T),
            DisAcc_H = mean(DisAcc_H, na.rm=T),
            DisAcc_F = mean(DisAcc_F, na.rm=T),
            n = n())

baseAcc_0p = baseAcc_0 %>%
  pivot_longer(cols = c("pAcc_H", "pAcc_F"), names_to = "Genre", values_to = "pAcc") %>%
  mutate(PCS8_H = factor(PCS8_H, levels = paste0("Homme\n", c(unique(niv_PCS8_H), "NA"))),
         PCS8_F = factor(PCS8_F, levels = paste0("Femme\n", c(unique(niv_PCS8_F), "NA")))) %>%
  mutate(pAcc = ifelse(n < seuilSignifiant, NA, pAcc)) %>% 
  mutate(enfPlusJeune = factor(as.character(enfPlusJeune),
                               levels = rev(c("0 à 3 ans", "4 à 6 ans", "7 à 9 ans",
                                              "10 à 12 ans", "13 à 15 ans", "autres cas")))) %>%
  mutate(PCS8_H = plyr::revalue(PCS8_H, c("Homme\nNA" = "< Femme seule >")),
         PCS8_F = plyr::revalue(PCS8_F, c("Femme\nNA" = "< Homme seul >"))) %>%
  mutate(Genre = plyr::revalue(Genre, c("pAcc_F" = "les femmes", "pAcc_H" = "les hommes")))

g1 = ggplot(data = baseAcc_0p, aes(x = enfPlusJeune, y = pAcc)) +
  geom_col(aes(fill = Genre), position = "dodge") +
  scale_fill_manual(values = c("thistle", "coral"),
                    name = "Part de leurs\ntrajets\nconsacrée à\nl'accompagnement\npar (en %)",
                    breaks=c("les hommes", "les femmes")) +
  xlab("Âge du plus jeune enfant") +
  ylab("Part de la distance parcourue dans la journée en trajet d'accompagnement (%)") +
  labs(title = "Comparaison de la part des trajets dédiée à l'accompagnement\npar les travailleur·ses du couple de référence du ménage",
       subtitle = paste0("Ménages ayant des enfants et dont les référents se sont déplacé·es pour le travail,",
                         "\npour lesquels l'information est disponible (n = ",
                         sum(baseAcc_0$n, na.rm=T), ")\n",
                         "Valeurs manquantes : moins de 20 ménages concernés"),
       caption = src_fig(date = "février 2023")) +
  coord_flip() +
  facet_grid(PCS8_H~PCS8_F) 

sortie("Distances/Accompagnement selon PCS des conjoint·es", taille = "page")
print(g1)
off()

baseAcc_0s = baseAcc_0 %>%
  pivot_longer(cols = c("DisAcc_H", "DisAcc_F"), names_to = "Genre", values_to = "DisAcc") %>%
  mutate(PCS8_H = factor(PCS8_H, levels = paste0("Homme\n", c(unique(niv_PCS8_H), "NA"))),
         PCS8_F = factor(PCS8_F, levels = paste0("Femme\n", c(unique(niv_PCS8_F), "NA")))) %>%
  mutate(DisAcc = ifelse(n < seuilSignifiant, NA, DisAcc)) %>% 
  mutate(enfPlusJeune = factor(as.character(enfPlusJeune),
                               levels = rev(c("0 à 3 ans", "4 à 6 ans", "7 à 9 ans",
                                              "10 à 12 ans", "13 à 15 ans", "autres cas")))) %>%
  mutate(PCS8_H = plyr::revalue(PCS8_H, c("Homme\nNA" = "< Femme seule >")),
         PCS8_F = plyr::revalue(PCS8_F, c("Femme\nNA" = "< Homme seul >"))) %>%
  mutate(Genre = plyr::revalue(Genre, c("DisAcc_F" = "les femmes", "DisAcc_H" = "les hommes")))

g1s = ggplot(data = baseAcc_0s, aes(x = enfPlusJeune, y = DisAcc/1000)) +
  geom_col(aes(fill = Genre), position = "dodge") +
  scale_fill_manual(values = c("thistle", "coral"),
                    name = "distance moyenne parcourue par",
                    breaks=c("les hommes", "les femmes")) +
  xlab("Âge du plus jeune enfant") +
  ylab("Distance (moyenne) parcourue dans la journée en trajet d'accompagnement (km)") +
  labs(title = "Comparaison de la distance dédiée aux trajets d'accompagnement\npar les travailleur·ses du couple de référence du ménage",
       subtitle = paste0("Ménages ayant des enfants et dont les référents se sont déplacé·es pour le travail,",
                         "\npour lesquels l'information est disponible (n = ",
                         sum(baseAcc_0$n, na.rm=T), ")\n",
                         "Valeurs manquantes : moins de 20 ménages concernés"),
       caption = src_fig(date = "février 2023")) +
  coord_flip() +
  facet_grid(PCS8_H~PCS8_F) + 
  theme_minimal(base_size = 8)

sortie("Distances/Accompagnement", taille = "page", portrait = T)
print(g1s + theme(legend.position="bottom"))
off()

# Modèles spécifiques : accompagnement

activites %>%
  filter(substr(Tache, 1, 1) %in% c("6", "7")) %>%
  group_by(heure) %>% summarise(n = n()) %>%
  ggplot(aes(x = heure/60, y = n)) + geom_line() +
  scale_x_continuous(breaks = c(6:22))

activites %>%
  left_join(select(PER, uid_PER, EnqDate_JS), by="uid_PER") %>%
  filter(substr(Tache, 1, 1) %in% c("6", "7")) %>%
  group_by(EnqDate_JS, heure) %>% summarise(n = n()) %>%
  ggplot(aes(x = heure/60, y = n)) + geom_line() +
  scale_x_continuous(breaks = c(6:22)) + facet_wrap(~EnqDate_JS)

activites %>%
  left_join(select(PER, uid_PER, DuTvl, MenEnfants), by="uid_PER") %>%
  filter(!is.na(MenEnfants)) %>%
  filter(DuTvl > 0) %>%
  filter(substr(Tache, 1, 1) %in% c("6", "7")) %>%
  group_by(MenEnfants, heure) %>% summarise(n = n()) %>%
  ggplot(aes(x = heure/60, y = n)) + geom_line() +
  scale_x_continuous(breaks = c(6:22)) + facet_wrap(~MenEnfants)

activites %>%
  left_join(select(PER, uid_PER, DuTvl, MenEnfants), by="uid_PER") %>%
  filter(!is.na(MenEnfants)) %>%
  filter(DuTvl > 0) %>%
  group_by(Genre, MenEnfants, heure) %>%
  summarise(nAcc = sum(ifelse(substr(Tache, 1, 1) %in% c("6", "7"), 1, 0)),
            n    = n()) %>%
  mutate(pAcc = nAcc / n) %>%
  ggplot(aes(x = heure/60, y = n)) + geom_line(aes(color = Genre)) +
  scale_x_continuous(breaks = c(6:22)) + facet_wrap(~MenEnfants)

sortie("Horaires/Accompagnement selon heure et genre")
activites %>%
  left_join(select(PER, uid_PER, DuTvl, MenEnfants), by="uid_PER") %>%
  filter(!is.na(MenEnfants)) %>%
  filter(DuTvl > 0) %>%
  mutate(enfants = case_when(MenEnfants == T ~ "Ménage avec enfants",
                             MenEnfants == F ~ "Ménage sans enfants")) %>%
  group_by(Genre, enfants, heure) %>%
  summarise(nAcc = sum(ifelse(substr(Tache, 1, 1) %in% c("6", "7"), 1, 0)),
            n    = n()) %>%
  mutate(pAcc = nAcc / n) %>%
  ggplot(aes(x = heure/60, y = pAcc * 100)) + geom_line(aes(color = Genre)) +
  scale_x_continuous(breaks = c(6:22)) + facet_wrap(~enfants) +
  scale_color_hue(name = "Genre\nenquêté·e", labels = c("femmes", "hommes")) +
  xlab("heure") + ylab("part des enquêté·es en train d'accompagner (%)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Part des enquêté·es occupé·es à accompagner quelqu'un",
       caption = src_fig())
off()

libEcoles = c("garderie", "école", "collège", "lycée", "supérieur", "travail")
ecole = nv(noms = as.character(c(21:25, 11)),
           valeurs = libEcoles)

rpctMotifs = nv(as.character(c(12:14, 26:29)), as.character(c(rep(11, 3), 22:25)))

statutsAct = c("actif·ve présent·e au\nlieu d'emploi (temps plein)",
               "actif·ve présent·e au\nlieu d'emploi (temps partiel)",
               "actif·ve absent·e du\nlieu d'emploi",
               "retraité·e", "sans emploi", "élève ou étudiant·e")

effectifsDEP = DEP %>%
  left_join(select(PER, uid_PER, Activ, DuTvl, Genre), by = "uid_PER") %>%
  mutate(act = case_when(Activ == "10" & DuTvl > 0 ~ statutsAct[1],
                         Activ == "11" & DuTvl > 0 ~ statutsAct[2],
                         Activ %in% c("10", "11") & DuTvl == 0 ~ statutsAct[3],
                         Activ == "32" ~ statutsAct[4],
                         Activ %in% c("31", "33") ~ statutsAct[5],
                         Activ %in% c("21", "22") ~ statutsAct[6])) %>%
  group_by(act) %>% summarise(nTot = n())

sortie("Horaires/Accompagnement selon genre")
DEP %>%
  left_join(select(PER, uid_PER, Activ, DuTvl, Genre), by = "uid_PER") %>%
  mutate(act = case_when(Activ == "10" & DuTvl > 0             ~ statutsAct[1],
                         Activ == "11" & DuTvl > 0             ~ statutsAct[2],
                         Activ %in% c("10", "11") & DuTvl == 0 ~ statutsAct[3],
                         Activ == "32"                         ~ statutsAct[4],
                         Activ %in% c("31", "33")              ~ statutsAct[5],
                         Activ %in% c("21", "22")              ~ statutsAct[6]),
         act = factor(act, levels = statutsAct)) %>%
  filter(!is.na(Genre), !is.na(act)) %>%
  mutate(O_MotAc = plyr::revalue(O_MotAc, rpctMotifs),
         D_MotAc = plyr::revalue(D_MotAc, rpctMotifs)) %>%
  filter(O_MotAc %in% as.character(c(11, 21:25)) | D_MotAc %in% as.character(c(11, 21:25))) %>%
  mutate(Motif = case_when(O_MotAc %in% as.character(c(11, 21:25)) ~ O_MotAc,
                           D_MotAc %in% as.character(c(11, 21:25)) ~ D_MotAc)) %>%
  mutate(Motif = plyr::revalue(Motif, ecole), Motif = factor(Motif, levels = libEcoles)) %>%
  group_by(Motif, act, Genre) %>% summarise (n = n()) %>%
  left_join(effectifsDEP, by = "act") %>%
  mutate(p = n / nTot * 100) %>%
  ggplot(aes(x = Motif, y = p)) +
  geom_col(aes(fill = Genre), position = "dodge") + facet_wrap(~act) +
  scale_fill_hue(name = "Genre de\nl'accompagnant·e", labels=c("femme", "homme")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("motif du déplacement") + ylab("part des déplacements (%)") +
  labs(title = "Part des déplacements consacrés à l'accompagnement",
       caption = src_fig())
off()

effectifsDEP = DEP %>%
  left_join(select(MEN, uid_MEN, PCSMLT), by="uid_MEN") %>%
  mutate(PCSMLT = etqPCSM(PCSMLT)) %>%
  group_by(PCSMLT) %>% summarise(nTot = n())

DEP %>%
  left_join(select(PER, uid_PER, PCSMLT, DuTvl, Genre), by = "uid_PER") %>%
  mutate(PCSMLT = etqPCSM(PCSMLT)) %>%
  filter(!is.na(Genre), !is.na(PCSMLT)) %>%
  mutate(O_MotAc = plyr::revalue(O_MotAc, rpctMotifs),
         D_MotAc = plyr::revalue(D_MotAc, rpctMotifs)) %>%
  filter(O_MotAc %in% as.character(c(11, 21:25)) | D_MotAc %in% as.character(c(11, 21:25))) %>%
  mutate(Motif = case_when(O_MotAc %in% as.character(c(11, 21:25)) ~ O_MotAc,
                           D_MotAc %in% as.character(c(11, 21:25)) ~ D_MotAc)) %>%
  mutate(Motif = plyr::revalue(Motif, ecole), Motif = factor(Motif, levels = libEcoles)) %>%
  group_by(Motif, PCSMLT, Genre) %>% summarise (n = n()) %>%
  left_join(effectifsDEP, by = "PCSMLT") %>%
  mutate(p = n / nTot * 100) %>%
  ggplot(aes(x = Motif, y = p)) +
  geom_col(aes(fill = Genre), position = "dodge") + facet_wrap(~PCSMLT) +
  scale_fill_hue(name = "Genre de\nl'accompagnant·e", labels=c("femme", "homme")) +
  xlab("motif du déplacement") + ylab("part des déplacements (%)")


DEP %>%
  left_join(select(PER, uid_PER, PCSMLT, DuTvl, Genre, ZoneDens), by = "uid_PER") %>%
  filter(!is.na(Genre), !is.na(PCSMLT), !PCSMLT %in% c("7i", "7e")) %>%
  mutate(PCSMLT = etqPCSM(PCSMLT), ZoneDens = etqZoneDens(ZoneDens, supprTrFaible = T)) %>%
  mutate(O_MotAc = plyr::revalue(O_MotAc, rpctMotifs),
         D_MotAc = plyr::revalue(D_MotAc, rpctMotifs)) %>%
  filter(O_MotAc %in% as.character(c(11, 21:25)) | D_MotAc %in% as.character(c(11, 21:25))) %>%
  mutate(Motif = case_when(O_MotAc %in% as.character(c(11, 21:25)) ~ O_MotAc,
                           D_MotAc %in% as.character(c(11, 21:25)) ~ D_MotAc)) %>%
  mutate(Motif = plyr::revalue(Motif, ecole), Motif = factor(Motif, levels = libEcoles)) %>%
  group_by(uid_PER, Motif, PCSMLT, Genre, ZoneDens) %>% summarise (Dis = sum(Dis, na.rm=T)) %>%
  group_by(Motif, PCSMLT, Genre, ZoneDens) %>% summarise (Dis = mean(Dis, na.rm=T)) %>%
  ggplot(aes(x = PCSMLT, y = Dis/1000)) +
  geom_col(aes(fill = Genre), position = "dodge") + facet_grid(Motif~ZoneDens) +
  scale_fill_hue(name = "Genre de\nl'accompagnant·e", labels=c("femme", "homme")) +
  xlab("motif du déplacement") + ylab("distance moyenne consacrée à l'accompagnement (km)")



sortie("Horaires/Accompagnement selon genre et motif école")
DEP %>%
  left_join(select(PER, uid_PER, Activ, DuTvl, Genre), by = "uid_PER") %>%
  filter(!is.na(Genre)) %>%
  mutate(O_MotAc = plyr::revalue(O_MotAc, rpctMotifs),
         D_MotAc = plyr::revalue(D_MotAc, rpctMotifs)) %>%
  filter(O_MotAc %in% as.character(c(11, 21:25)) | D_MotAc %in% as.character(c(11, 21:25))) %>%
  mutate(Motif = case_when(O_MotAc %in% as.character(c(11, 21:25)) ~ O_MotAc,
                           D_MotAc %in% as.character(c(11, 21:25)) ~ D_MotAc)) %>%
  mutate(Motif = plyr::revalue(Motif, ecole), Motif = factor(Motif, levels = libEcoles)) %>%
  group_by(Motif, Genre) %>% summarise (n = n()) %>%
  group_by(Motif) %>% mutate(p = n / sum(n) * 100) %>%
  filter(!is.na(Genre)) %>%
  ggplot(aes(x = Motif, y = p)) + geom_col(aes(fill = Genre)) +
  xlab("Motif de déplacement de la personne accompagnée") + ylab("part des accompagnant·es (%)") +
  scale_fill_hue(name = "Genre de\nl'accompagnant·e", labels = c("femmes", "hommes")) +
  labs(title = "Part des trajets d'accompagnement assurés\npar les accompagnant·es selon leur genre",
       caption = src_fig())
off()
