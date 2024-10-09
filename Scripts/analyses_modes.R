# Recherches générales
# Thèse de Maxime Guinepain
# Chapitre 6

# setwd("/home/maxime/Données/EMD")

# [>>] Initialisation ====

rm(list = ls()) ; gc()
source("START.R")
initMémoire(BasesCharger = c("MEN", "PER", "DEP", "OPI"))

rapport("Script Chap6", prim=T)

# On ne travaille que sur les actif⋅ves ayant des paramètres raisonnables

if(!dir.exists("Sorties/Modes")) { dir.create("Sorties/Modes") }

PER_ff = init_PER_ff(PER)
# Quelques champs supplémentaires pour l'analyse
PER_ff = mutate(PER_ff,
                disTvlSurDis = Dis/Travail_Dis,
                DuTvl = DuTvl + DuEtu,
                DuCtt = DuCom + DuSvc + DuTax,
                NivDip = NivEtuVersNivDip(NivEtu)) %>%
  filter(!is.na(disTvlSurDis) & !is.infinite(disTvlSurDis))

PER_ff = PER_ff %>%
  mutate(across(starts_with("modes_"), ~(. == "oui")))


# Recours modal général ==========================================================

# reprendre : les barres de parts modales (genre + âge, âge + densité, pcsm + densité,
# pcs + dis, pcsm + statutocc)

rapport("Modes/Recours modal", prim = T)

sortie("Recours modal", format = "pdf", taille = "page")
PER %>% group_by(Age5, Genre, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "Age5", var3 = "Genre", stackdim=T,
             couleurs = pal22_typoModes,
             xlab = "Classe d'âge", ylab = "Part de la population (%)",
             titre = "Recours modal selon l'âge et le genre au cours de la journée observée",
             vCramer=T) %>% print()

PER %>% group_by(Age5, Genre, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "Genre", var3 = "Age5",facetdim=T,
             couleurs = pal22_typoModes,
             xlab = "Part de la population (%)", ylab = "Classe d'âge",
             titre = "Recours modal selon l'âge et le genre au cours de la journée observée (vue détaillée)",
             etiqueterBarres = T, vCramer=T) %>% print()

PER %>%
  filter(!is.na(ZoneDens)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
  group_by(Age5, ZoneDens, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "Age5", var3 = "ZoneDens", stackdim=T,
             couleurs = pal22_typoModes,
             xlab = "Classe d'âge", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée",
             vCramer=T) %>% print()

PER %>%
  filter(!is.na(ZoneDens) & !is.na(PCSMLT)) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), PCSMLT = etqPCSM(PCSMLT)) %>%
  group_by(PCSMLT, ZoneDens, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "PCSMLT", var3 = "ZoneDens", stackdim=T,
             couleurs = pal22_typoModes,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée",
             vCramer=T) %>% print()

PER %>%
  filter(ZoneDens=="1" & !is.na(LogOcc) & LogOcc %in% c("10", "20", "21")) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), LogOcc = etqLogOcc(LogOcc)) %>%
  group_by(PCSMLT, LogOcc, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "PCSMLT", var3 = "LogOcc", facetdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nCommunes de forte densité uniquement",
             vCramer=T) %>% print()

PER %>%
  filter(ZoneDens=="3" & !is.na(LogOcc) & LogOcc %in% c("10", "20", "21")) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens), LogOcc = etqLogOcc(LogOcc)) %>%
  group_by(PCSMLT, LogOcc, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "PCSMLT", var3 = "LogOcc", facetdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nCommunes de faible densité uniquement",
             vCramer=T) %>% print()
PER %>%
  filter(!is.na(ZoneDens) & !is.na(clDis) & typoJo == "TRAV") %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
  group_by(ZoneDens, clDis, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "clDis", var3 = "ZoneDens", stackdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nJournées de travail uniquement",
             vCramer=T) %>% print()

PER %>%
  filter(!is.na(clDis) & typoJo == "TRAV" & ZoneDens == "1" & PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, clDis, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "clDis", var3 = "PCS8", facetdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nJournées de travail, communes de résidence denses uniquement",
             vCramer=T) %>% print()

PER %>%
  filter(!is.na(clDis) & typoJo == "TRAV" & ZoneDens == "2" & PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, clDis, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "clDis", var3 = "PCS8", facetdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nJournées de travail, communes de résidence de densité intermédiaire uniquement",
             vCramer=T) %>% print()

PER %>%
  filter(!is.na(clDis) & typoJo == "TRAV" & ZoneDens == "3" & PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, clDis, typoModes) %>%
  summarize(pop = sum(CoeffEnq), .groups="drop_last") %>%
  mutate(popTot = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(pop = pop/popTot*100) %>%
  viz_Barres(valeurs = "pop", var1 = "typoModes", var2 = "clDis", var3 = "PCS8", facetdim=T,
             couleurs = pal22_typoModes, etiqueterBarres = T,
             xlab = "PCS Ménage", ylab = "Part de la population",
             titre = "Recours modal selon l'âge et le type de secteur résidentiel au cours de la journée observée\nJournées de travail, communes de résidence peu denses uniquement",
             vCramer=T) %>% print()
off()


# Chiffres simples ====

sum(filter(PER_ff, !is.na(Fqc_Vco), !is.na(Fqc_Vpa), !is.na(CoeffRec),
           Fqc_Vco == "4" & Fqc_Vpa == "4")$CoeffRec) /
  sum(filter(PER_ff, !is.na(Fqc_Vco), !is.na(Fqc_Vpa), !is.na(CoeffRec))$CoeffRec)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI == 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI == 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Dis_VOI > 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Handi == "2")$CoeffRecEnq) / sum(filter(PER_ff, !is.na(Handi) & !is.na(CoeffRecEnq))$CoeffRecEnq)

medDsDom = weighted.median(PER_ff$dsDom, PER_ff$CoeffRecEnqSansEMP)

sum(filter(PER_ff, !is.na(CoeffRecEnq), Permis == "1", dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(Permis), !is.na(CoeffRecEnq), dsDom>1200)$CoeffRecEnq)
sum(filter(PER_ff, !is.na(CoeffRecEnq), Permis == "1", dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(Permis), !is.na(CoeffRecEnq), dsDom<1200)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Permis == "1" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & Permis == "1" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & Permis == "1" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & Permis == "1" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & Permis == "1" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Permis == "2" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & Permis == "2" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & Permis == "2" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & Permis == "2" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & Permis == "2" & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Permis == "1" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & Permis == "1" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & Permis == "1" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & Permis == "1" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & Permis == "1" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "1" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & Permis == "2" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & Permis == "2" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & Permis == "2" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & Permis == "2" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & Permis == "2" & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, Permis == "2" & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)

PER_ff = left_join(PER_ff, select(MEN, uid_MEN, VehN), by="uid_MEN")

sum(filter(PER_ff, !is.na(CoeffRecEnq), VehN > 0, dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq), dsDom>1200)$CoeffRecEnq)
sum(filter(PER_ff, !is.na(CoeffRecEnq), VehN > 0, dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq), dsDom<1200)$CoeffRecEnq)

nrow(filter(PER_ff, !is.na(CoeffRecEnq), VehN == 0, dsDom < 1200)) 

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN > 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN > 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN > 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN > 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN > 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN > 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 0 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_ff, VehN == 0 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)




couples = PER_ff |>
  mutate(n=1) |>
  pivot_wider(names_from = "Lien", values_from = "n", names_prefix = "ménage_") |>
  select(uid_MEN, ménage_1, ménage_2) |>
  group_by(uid_MEN) |> summarise(ménage_1 = sum(ménage_1, na.rm=T),
                                 ménage_2 = sum(ménage_2, na.rm=T)) |>
  filter(ménage_1 == 1, ménage_2 == 1)

PER_fff = filter(PER_ff, uid_MEN %in% couples$uid_MEN)
sum(PER_fff$CoeffRecEnq, na.rm=T) / sum(PER_ff$CoeffRecEnq, na.rm=T)

sum(filter(PER_fff, VehN == 1)$CoeffRecEnq, na.rm=T) / sum(PER_fff$CoeffRecEnq, na.rm=T)

sum(filter(PER_fff, VehN == 1, dsDom > 1200)$CoeffRecEnq, na.rm=T) / sum(filter(PER_fff, dsDom > 1200)$CoeffRecEnq, na.rm=T)
sum(filter(PER_fff, VehN == 1, dsDom < 1200)$CoeffRecEnq, na.rm=T) / sum(filter(PER_fff, dsDom < 1200)$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN > 1 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN > 1 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN > 1 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN > 1 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN > 1 & dsDom > 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 1 & dsDom > 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 1 & dsDom > 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 1 & dsDom > 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 1 & dsDom > 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 1 & dsDom > 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 1 & dsDom > 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 1 & dsDom > 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 1 & dsDom > 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 1 & dsDom > 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 1 & dsDom > 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom > 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN > 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN > 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN > 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN > 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN > 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN > 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 1 & dsDom < 1200)$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0)$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 1 & dsDom < 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 1 & dsDom < 1200  & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 1 & dsDom < 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 1 & dsDom < 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 1 & dsDom < 1200 & Genre == "H")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "H")$CoeffRecEnq)

sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_BUS + Dis_TRN > 0 & VehN == 1 & dsDom < 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VOI > 0 & VehN == 1 & dsDom < 1200  & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_MAR > 0 & VehN == 1 & dsDom < 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & VehN == 1 & dsDom < 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)
sum(filter(PER_fff,!is.na(CoeffRecEnq) & Dis_DRM > 0 & VehN == 1 & dsDom < 1200 & Genre == "F")$CoeffRecEnq) / sum(filter(PER_fff, VehN == 1 & !is.na(CoeffRecEnq) & dsDom < 1200 & Dis>0 & Genre == "F")$CoeffRecEnq)






sum(filter(PER_ff, dsDom > 1200)$CoeffRecEnq, na.rm=T) / sum(PER_ff$CoeffRecEnq, na.rm=T)

quantile(PER_ff$dsDom, na.rm=T)


PER_ff %>%
  filter(!is.na(CoeffRecEnq)) %>%
  mutate(ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneRang) %>%
  summarise(nVoi = sum(ifelse(Dis_VOI > 0, CoeffRecEnq, 0)),
            nTco = sum(ifelse(Dis_BUS + Dis_TRN > 0, CoeffRecEnq, 0)),
            tot = sum(CoeffRecEnq)) %>%
  mutate(pVoi = nVoi / tot, pTco = nTco / tot)

PER_ff %>%
  filter(!is.na(CoeffRecEnq), ZoneRang %in% c( "4", "5")) %>%
  group_by(uid_ENQ) %>%
  summarise(nVoi = sum(ifelse(Dis_VOI > 0, CoeffRecEnq, 0)),
            nTco = sum(ifelse(Dis_BUS + Dis_TRN > 0, CoeffRecEnq, 0)),
            tot = sum(CoeffRecEnq)) %>%
  mutate(pVoi = nVoi / tot, pTco = nTco / tot) %>%
  tab_Tri(parCol = "pTco", rev = T)

PER_ff %>%
  filter(!is.na(CoeffRecEnq), ZoneRang %in% c( "1", "2")) %>%
  group_by(uid_ENQ) %>%
  summarise(nVoi = sum(ifelse(Dis_VOI > 0, CoeffRecEnq, 0)),
            nTco = sum(ifelse(Dis_BUS + Dis_TRN > 0, CoeffRecEnq, 0)),
            tot = sum(CoeffRecEnq)) %>%
  mutate(pVoi = nVoi / tot, pTco = nTco / tot) %>%
  tab_Tri(parCol = "pTco", rev = T)



sum(PER_ff$Dis_VOI, na.rm=T) / sum(PER_ff$Dis, na.rm=T)
sum(PER_ff$Dis_MAR, na.rm=T) / sum(PER_ff$Dis, na.rm=T)
sum(PER_ff$Dis_BUS + PER_ff$Dis_TRN, na.rm=T) / sum(PER_ff$Dis, na.rm=T)
  
sum(filter(PER, !is.na(Fqc_Vco), !is.na(Fqc_Vpa), !is.na(CoeffRec),
           Fqc_Vco == "4" & Fqc_Vpa == "4")$CoeffRec) /
  sum(filter(PER, !is.na(Fqc_Vco), !is.na(Fqc_Vpa), !is.na(CoeffRec))$CoeffRec)

sum(filter(PER,!is.na(CoeffRecEnq) & Dis_VOI == 0)$CoeffRecEnq) / sum(filter(PER, !is.na(CoeffRecEnq))$CoeffRecEnq)

sum(filter(PER,!is.na(CoeffRecEnq) & Dis == 0)$CoeffRecEnq) / sum(filter(PER, !is.na(CoeffRecEnq))$CoeffRecEnq)

sum(PER$Dis_VOI, na.rm=T) / sum(PER$Dis, na.rm=T)

sum(filter(mutate(PER_ff, DisNonVoi = Dis - Dis_VOI), DisNonVoi > 0)$CoeffRecEnq, na.rm=T) /
  sum(PER_ff$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, Travail_Parc == "2")$CoeffRec, na.rm=T) / sum(filter(PER_ff, !is.na(Travail_Parc))$CoeffRec, na.rm=T)

sum(filter(PER_ff, Travail_Parc %in% c("2", "3", "4"))$CoeffRec, na.rm=T) / sum(filter(PER_ff, !is.na(Travail_Parc))$CoeffRec, na.rm=T)


sum(filter(PER_ff, Travail_Parc == "2" & ZoneDens_travMax == "1")$CoeffRec, na.rm=T) / sum(filter(PER_ff, !is.na(Travail_Parc), !is.na(ZoneDens_travMax))$CoeffRec, na.rm=T)

sum(filter(PER_ff, Travail_Parc %in% c("2", "3", "4") & ZoneDens_travMax == "1")$CoeffRec, na.rm=T) / sum(filter(PER_ff, !is.na(Travail_Parc), !is.na(ZoneDens_travMax))$CoeffRec, na.rm=T)

DEP %>%
  filter(uid_PER %in% PER_ff$uid_PER) %>%
  filter(Dis < 80000) %>%
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by="uid_PER") %>%
  mutate(Dis = discretisation(Dis, nbClassesCible = 10, passerLog = F),
         ModeP = etqMode(ModeP, simple=T), pop = CoeffRecEnq) %>%
  filter(ModeP != "modes_NA") %>%
  pivot_wider(names_from = "ModeP", values_from = "CoeffRecEnq", names_prefix = "mode_") %>%
  group_by(Dis) %>% summarise(across(starts_with("mode_"), \(x) sum(x, na.rm=T)), pop = sum(pop, na.rm=T)) %>%
  pivot_longer(cols = starts_with("mode_"), names_to = "Mode", values_to = "n") %>%
  mutate(p = n / pop) %>%
  ggplot(aes(x = Dis, y = p)) + geom_col(aes(fill = Mode), position = "stack")

g = DEP %>%
  mutate(Dis = Dis/1000) %>%
  filter(Dis < 1000, !is.na(Dis)) %>%
  filter(uid_PER %in% PER_ff$uid_PER) %>%
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by="uid_PER") %>%
  mutate(Dis = discretisation(Dis, methode = "déciles"),
         ModeP = etqMode(ModeP, simple=T), pop = CoeffRecEnq) %>%
  filter(ModeP != "NA", ModeP != "autres", ModeP != "taxi") %>%
  filter(!is.na(Dis)) %>%
  pivot_wider(names_from = "ModeP", values_from = "CoeffRecEnq", names_prefix = "mode_") %>%
  group_by(Dis) %>% summarise(across(starts_with("mode_"), \(x) sum(x, na.rm=T)), pop = sum(pop, na.rm=T)) %>%
  pivot_longer(cols = starts_with("mode_"), names_to = "Mode", values_to = "n") %>%
  mutate(p = n / pop * 100) %>%
  ggplot(aes(x = Dis, y = p)) +
  geom_line(aes(colour = Mode, group = Mode), lty=2, alpha=.6) +
  geom_point(aes(colour = Mode)) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  ylab("part des déplacements effectués avec le mode (%)") +
  xlab("longueur du trajet (km)") +
  labs(title = "Mode principal du déplacement\nen fonction de sa longueur (km)",
       subtitle = "Pour les travailleur⋅ses en emploi",
       caption = src_fig(PER_ff)) +
  scale_colour_manual(values = c("slateblue", "purple", "tan", "deepskyblue",
                                 "palevioletred", "lightgreen", "brown2"),
                      labels=c("liaison\ninterurbaine", "deux roues",
                               "marche", "TC local", "train", "vélo", "voiture")) +
  scale_y_continuous(breaks=c(0:9)*10)

sortie("Modes/Modes selon distance dep")
  print(g)
off()


# Permis
sum(filter(PER_ff, Permis == "1")$CoeffRecEnq, na.rm=T) / sum(PER_ff$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, Permis == "1", Age<25)$CoeffRecEnq, na.rm=T) / sum(filter(PER_ff, Age<25)$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, Permis == "1", Age<20)$CoeffRecEnq, na.rm=T) / sum(filter(PER_ff, Age<20)$CoeffRecEnq, na.rm=T)

# Retours avec courses ou sans courses
DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by="uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  filter(substr(O_Motif, 1,1) == "1", substr(D_Motif, 1,1) == "0") |>
  group_by(ModeP) |> summarise(n = sum(CoeffRecEnq)) |>
  mutate(p = n/sum(n), ModeP = etqMode(ModeP)) |>
  tab_Tri(parCol = "p", rev = T)

DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by="uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  filter(substr(O_Motif, 1,1) == "3", substr(D_Motif, 1,1) == "0") |>
  group_by(ModeP) |> summarise(n = sum(CoeffRecEnq)) |>
  mutate(p = n/sum(n), ModeP = etqMode(ModeP)) |>
  tab_Tri(parCol = "p", rev = T)


# Part des travailleur⋅ses possédant 1+ vélo
PER_ff |>
  left_join(select(MEN, uid_MEN, VelN), by="uid_MEN") |>
  filter(!is.na(VelN)) |>
  mutate(vels = ifelse(VelN == 0, "VelNon", "VelOui")) |>
  pivot_wider(names_from = vels, values_from = CoeffRecEnq) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T))

MEN |>
  filter(uid_MEN %in% PER_ff$uid_MEN) |>
  mutate(vels = ifelse(VelN == 0, "VelNon", "VelOui")) |>
  pivot_wider(names_from = vels, values_from = Coeff) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T))

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & Dis>0)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & dsDom < 800)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & Dis>0 & dsDom < 800)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & dsDom > 12800)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & Dis>0 & dsDom > 12800)$CoeffRecEnq)

sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & dsTvl < 800)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & Dis>0 & dsTvl < 800)$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_VEL > 0 & dsTvl > 12800)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq) & Dis>0 & dsTvl > 12800)$CoeffRecEnq)

PER_ff |>
  filter(PCS8 == "04") |>
  left_join(select(MEN, uid_MEN, VelN), by="uid_MEN") |>
  filter(!is.na(VelN)) |>
  mutate(vels = ifelse(VelN == 0, "VelNon", "VelOui")) |>
  pivot_wider(names_from = vels, values_from = CoeffRecEnq) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T)) |>
  pivot_longer(cols = c("nVelNon", "nVelOui"), names_to = "Vel", values_to = "n") |>
  mutate(p = n / sum(n))

PER_ff |>
  filter(!is.na(Dis), !is.na(CoeffRecEnq)) |>
  summarise(Dis = weighted.mean(Dis, w = CoeffRecEnq))

PER_ff |>
  mutate(vel = ifelse(Dis > 0 & Dis_VEL > 0, "VelOui", "VelNon")) |>
  pivot_wider(names_from = vel, values_from = CoeffRecEnq) |>
  group_by(PCS8) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T)) |>
  pivot_longer(cols = c("nVelNon", "nVelOui"), names_to = "Vel", values_to = "n") |>
  group_by(PCS8) |> mutate(p = n/sum(n)) |> filter(Vel == "nVelOui")

PER_ff |>
  mutate(vel = ifelse(Dis > 0 & Dis_VEL > 0, "VelOui", "VelNon")) |>
  pivot_wider(names_from = vel, values_from = CoeffRecEnq) |>
  mutate(PCS42S = etqPCS42S(PCS42S)) |> group_by(PCS42S) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T)) |>
  pivot_longer(cols = c("nVelNon", "nVelOui"), names_to = "Vel", values_to = "n") |>
  group_by(PCS42S) |> mutate(p = n/sum(n)) |> filter(Vel == "nVelOui") |>
  tab_Tri(parCol = "p")

t_vel = PER_ff |>
  mutate(vel = ifelse(Dis > 0 & Dis_VEL > 0, "VelOui", "VelNon")) |>
  pivot_wider(names_from = vel, values_from = CoeffRecEnq) |>
  filter(ZoneDens == 1) |>
 group_by(uid_ENQ) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T)) |>
  pivot_longer(cols = c("nVelNon", "nVelOui"), names_to = "Vel", values_to = "n") |>
  group_by(uid_ENQ) |> mutate(p = n/sum(n)) |> filter(Vel == "nVelOui") |>
  tab_Tri(parCol = "p")

t_trie = PER_ff |>
  mutate(vel = ifelse(Dis > 0 & Dis_VEL > 0, "VelOui", "VelNon")) |>
  pivot_wider(names_from = vel, values_from = CoeffRecEnq) |>
  filter(ZoneDens == 1) |>
  group_by(uid_ENQ, Genre) |>
  summarise(nVelNon = sum(VelNon, na.rm=T),
            nVelOui = sum(VelOui, na.rm=T)) |>
  pivot_longer(cols = c("nVelNon", "nVelOui"), names_to = "Vel", values_to = "n") |>
  group_by(uid_ENQ, Genre) |> mutate(p = n/sum(n)) |> filter(Vel == "nVelOui") |>
  select(-n) |>
  pivot_wider(names_from = Genre, values_from = p, names_prefix = "p") |>
  mutate(rapport = pH/pF) |>
  tab_Tri(parCol = "rapport") |>
  left_join(t_vel, by = c("uid_ENQ", "Vel")) |>
  filter(!is.na(p), !is.na(rapport), !is.infinite(rapport))

cor.test(t_trie$p, t_trie$rapport)

# Part des trajets à vélo dans l'EMP
sum(filter(left_join(DEP, select(PER, uid_PER, CoeffRecEnq), by="uid_PER"),
           uid_ENQ == "EMP2019" & ModeP %in% mode_VEL)$CoeffRecEnq, na.rm=T) / 
  sum(filter(left_join(DEP, select(PER, uid_PER, CoeffRecEnq), by="uid_PER"),
             uid_ENQ == "EMP2019")$CoeffRecEnq, na.rm=T) 

nrow(filter(left_join(DEP, select(PER, uid_PER, CoeffRecEnq), by="uid_PER"),
           uid_ENQ == "EMP2019" & ModeP %in% mode_VEL)) / 
  nrow(filter(left_join(DEP, select(PER, uid_PER, CoeffRecEnq), by="uid_PER"),
             uid_ENQ == "EMP2019")) 

PER_ff |>
  pivot_wider(names_from = modes_vélo, values_from = CoeffRecEnq, names_prefix = "Vel_") |>
  mutate(Annee = substr(uid_ENQ, 4, 8)) |>
  group_by(Annee) |>
  summarise(across(starts_with("Vel_"), ~sum(., na.rm = T))) |>
  pivot_longer(cols = starts_with("Vel_"),
               names_to = "Vel", values_to = "n") |>
  filter(Vel != "Vel_NA") |>
  group_by(Annee) |>
  mutate(p = n / sum(n)) |>
  ggplot(aes(x = Annee, y = p, fill = Vel)) +
  geom_col() +
  scale_fill_hue() +
  labs(title="Part des usager⋅es ayant recours au vélo (travailleur⋅ses en emploi)",
       caption=src_fig(filter(PER_ff, !is.null(modes_vélo))))

# Utilisation du vélo

PER %>%
  filter(Activ %in% c("10", "11", "12") & DuTvl > 0 & !is.na(PCS42S)) %>%
  filter(PCS42S %in% as.character(c(10:69))) %>%
  mutate(nvel = ifelse(modes_vélo == "oui", CoeffEnq, 0), PCS42S = etqPCS42S(PCS42S)) %>%
  mutate(ZoneDens = plyr::revalue(ZoneDens, c("1" = "dense",
                                              "2" = "intermédiaire ou\npeu dense",
                                              "3" = "intermédiaire ou\npeu dense",
                                              "4" = "intermédiaire ou\npeu dense"))) %>%
  group_by(ZoneDens, PCS42S, Genre) %>%
  summarise(nvel = sum(nvel, na.rm=T), pop = sum(CoeffEnq, na.rm=T), n = n()) %>%
  mutate(pvel = nvel / pop * 100) %>%
  ggplot(aes(x = PCS42S, y = pvel)) + geom_col(aes(fill = Genre), position = "dodge") +
  scale_fill_hue(limits = c("H", "F"), labels = c("hommes", "femmes")) +
  coord_flip() + facet_wrap(~ZoneDens) +
  theme(legend.position = "bottom") +
  labs(title = "Usage du vélo lors d'une journée de travail")


PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  mutate(nvel = ifelse(modes_vélo == "oui", CoeffEnq, 0)) %>%
  group_by(PCS42S) %>%
  summarise(nvel = sum(nvel, na.rm=T), pop = sum(CoeffEnq, na.rm=T)) %>%
  mutate(pvel = nvel / pop * 100) 

PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  mutate(nvel = ifelse(modes_vélo == "oui", 1, 0)) %>%
  group_by(PCS42S) %>%
  summarise(nvel = sum(nvel, na.rm=T), pop = n()) %>%
  mutate(pvel = nvel / pop * 100) 


# Étude des choix modaux ====

logit(tab = valref(PER_ff),
      val = "modes_voiture",
      formule = "Dis",
      titre = "Modèle Logit Usage de l'automobile (Simple)", valIntervalleSur100 = 8)

logit(tab = valref(PER_ff),
      val = "modes_voiture",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10",
      titre = "Modèle Logit Usage de l'automobile (Simple)", valIntervalleSur100 = 8)

logit(tab = valref(PER_ff),
      val = "modes_voiture",
      formule = "Activ + Genre + PCSMT + dsDomEtq",
      titre = "Modèle Logit Usage de l'automobile (PCS Ménage)", valIntervalleSur100 = 8)

logit(tab = valref(PER_ff),
      val = "modes_voiture",
      formule = "Activ + Genre + PCS42S + dsDomEtq",
      titre = "Modèle Logit Usage de l'automobile (PCS détaillée)", valIntervalleSur100 = 8)

# Le R2 monte et l'impact de la densité baisse avec PCSMT et PCS42S, ce qui pourrait
# être dû à la modification du périmètre...

# Don't know what we're looking at if we include the permis...
logit(tab = valref(PER_ff),
      val = "modes_voiture",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Permis",
      titre = "Modèle Logit Usage de l'automobile (Permis)", valIntervalleSur100 = 8)

# Quelle diff entre densité et distance ?
logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_voiture",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10",
      titre = "Modèle Logit Usage de l'automobile (Distance)",
      valIntervalleSur100 = 8, )

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_voiture == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_voiture == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F")$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_drm == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_drm == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F")$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_vélo == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_vélo == T)$CoeffRecEnq, na.rm=T) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F")$CoeffRecEnq, na.rm=T)

(sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_motor == T)$CoeffRecEnq, na.rm=T) -
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_voiture  == T)$CoeffRecEnq, na.rm=T))  /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H")$CoeffRecEnq, na.rm=T)

(sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_motor == T)$CoeffRecEnq, na.rm=T) -
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_voiture == T)$CoeffRecEnq, na.rm=T)) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F")$CoeffRecEnq, na.rm=T)

(sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_motor == T & PCS8 == "01")$CoeffRecEnq, na.rm=T) -
    sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & modes_voiture  == T & PCS8 == "01")$CoeffRecEnq, na.rm=T))  /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "H" & PCS8 == "01")$CoeffRecEnq, na.rm=T)

(sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_motor == T & PCS8 == "01")$CoeffRecEnq, na.rm=T) -
    sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & modes_voiture == T & PCS8 == "01")$CoeffRecEnq, na.rm=T)) /
  sum(filter(PER_ff, dsDomEtq == "< 200" & Genre == "F" & PCS8 == "01")$CoeffRecEnq, na.rm=T)

#TODO: ajuster la légende ici, c'est par 10 km et pas par 1 !!
gMdVoi = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
      val = "modes_voiture",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis10",
      titre = "Utilisation d'une voiture",
      valIntervalleSur100 = 9, returnFig = T,
      caption = src_fig(emp = F)) +
  theme(legend.position = "bottom")

# Ne fonctionne pas du tout quand on le pondère...
# PER_ff |>
#   filter(!is.na(CoeffRecEnqSansEMP)) |>
#   mutate(Dis = Dis/1000) |>
#   valref() |>
#   logit(val = "modes_voiture", poids = "CoeffRecEnqSansEMP",
#         formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis",
#         titre = "Utilisation d'une voiture",
#         valIntervalleSur100 = 9, returnFig = T)

sortie("Modes/Modèles logit voiture densité", taille = "page")
g = logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_voiture", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage de l'automobile (Par Densité)",
      valIntervalleSur100 = 3)
g = g + theme(axis.text.x = element_text(angle=270, hjust=0))
print(g)
off()

image_read("Sorties/Modes/Modèles logit voiture densité.png") |>
  image_rotate(degrees = 270) |>
  image_write("Sorties/Modes/Modèles logit voiture densité.png")

# Figure propre
# On récupère la liste de modèles
mliste = logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
          val = "modes_voiture", colComparaison = "dsDomEtq",
          formule = "Activ + Genre + PCS8 + Dis + Age10",
          titre = "Modèle Logit Usage de l'automobile (Par Densité)",
          valIntervalleSur100 = 3, returnFig = F)

# Maintenant il nous faut une table avec
# - la classe de densité
# - les coeffs pour PCS1, 2, 3, 5, 6
# - la marge d'erreur

tableau = tibble(variante = double(),
                 var = character(),
                 Estimate = double(),
                 `Std. Error` = double(),
                 `z value` = double(),
                 `Pr(>|z|)` = double())

for(i in 1:length(mliste))
{
  entree = summary(mliste[[i]])$coefficients |>
    as.data.frame() |>
    rownames_to_column("var") |>
    as_tibble() |>
    filter(substr(var, 1, 3) == "PCS") |>
    mutate(variante = i)
  
  tableau = add_row(tableau, entree)
}
tableau$variante = as.factor(tableau$variante)
levels(tableau$variante) = levels(PER_ff$dsDomEtq)

valIntervalleSur100 = 2
ticks = c(-1 * valIntervalleSur100 * 2, -1 * valIntervalleSur100, 0,
          valIntervalleSur100, valIntervalleSur100 * 2) %>%
  transf_echelle_sur100_inverse() %>% round(0)

g = tableau |>
  mutate(Estimate = (exp(Estimate) - 1) * 100) |>
  filter(`Pr(>|z|)` < .05) |>
  mutate(confiance = case_when(`Pr(>|z|)` < .001 ~ "< 1%",
                               `Pr(>|z|)` < .01  ~ "1%",
                               `Pr(>|z|)` < .05  ~ "5%")) |>
  ggplot(aes(x = variante, y = Estimate)) +
#  geom_line(aes(colour = var, group = var)) +
  geom_point(aes(colour = var, shape = confiance), size=2) +
  geom_hline(aes(yintercept = 0, colour = "PCS804"), linetype = 2) +
  scale_y_continuous(trans = trans_sur100, breaks = ticks,
                     labels = transf_echelle_sur100_lab(ticks)) +
  scale_shape_manual(values = c(16, 1, 4)) +
  scale_colour_manual(name = "PCS",
                   labels = c(niv_PCS8[1:3],
                              "Prof. interm. (référence)",
                              niv_PCS8[5:6]),
                   values = pal_PCS8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Variante du modèle, restreint aux résident⋅es\nde chaque classe de densité") +
  ylab("Coefficient (probabilité d'un recours à l'automobile\npar rapport aux individus de PCS intermédiaire)") +
  labs(title = ml("Coefficients associés aux catégories socioprofessionnelles",
                  "dans des modèles logit portant sur l'usage de la voiture,",
                  "stratifiés par densité du secteur de résidence"),
       caption = src_fig(emp=F))

sortie("Modes/Modèles logit - voiture, facteur CSP selon résid")
  print(g)
off()
  

#TODO : marginal effects ? intervalles de confiance ?


gMdTco = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
      val = "modes_tc",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis10 + Age10",
      titre = "Utilisation des transports en commun",
      valIntervalleSur100 = 9, returnFig = T,
      caption = src_fig(emp = F))

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_tc", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage des TC (Par Densité)", valIntervalleSur100 = 3)

gMdDrm = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
      val = "modes_drm",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis10 + Age10",
      titre = "Utilisation d'un deux roues motorisé",
      valIntervalleSur100 = 9, returnFig = T,
      caption=src_fig(emp=F))

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_drm", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage des deux roues (Par Densité)", valIntervalleSur100 = 3)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_drm", colComparaison = "dsTvlEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage des deux roues (Par Densité)", valIntervalleSur100 = 3)

sum(filter(PER, Genre == "H" & modes_drm == "oui")$CoeffRecEnq, na.rm=T) /
  sum(filter(PER, Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER, Genre == "F" & modes_drm == "oui")$CoeffRecEnq, na.rm=T) /
  sum(filter(PER, Genre == "F")$CoeffRecEnq, na.rm=T)

gMdVel = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
      val = "modes_vélo",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis10 + Age10",
      titre = "Utilisation d'un vélo",
      valIntervalleSur100 = 9, returnFig = T,
      caption = src_fig(emp = F))

# Avec les années
logit(tab = valref(mutate(PER_ff, Dis=Dis/10000, Annee = as.integer(substr(uid_ENQ, 4, 8)))),
      val = "modes_vélo",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10 + Annee",
      titre = "Utilisation d'un vélo",
      valIntervalleSur100 = 9, returnFig = T)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_vélo", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage du vélo (Par Densité)", valIntervalleSur100 = 3)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_tc_rail",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10",
      titre = "Modèle Logit Usage du train (Distance)", valIntervalleSur100 = 8)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_tc_rail", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Usage du train (Par Densité)", valIntervalleSur100 = 3)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_marche",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10",
      titre = "Modèle Logit Recours à la marche (Distance)", valIntervalleSur100 = 8)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_marche", colComparaison = "dsDomEtq",
      formule = "Activ + Genre + PCS8 + Dis + Age10",
      titre = "Modèle Logit Recours à la marche (Par Densité)", valIntervalleSur100 = 3)

# get_legend ne fonctionne plus avec la legend en bottom, ici on bricole...
leg = get_plot_component(gMdVoi + theme(legend.position = "top"), "guide-box", return_all = T)[[4]]
page = cowplot::plot_grid(gMdVoi + theme(legend.position = "none") + xlab(NULL) + labs(caption = ""),
                          gMdTco + theme(legend.position = "none") + xlab(NULL) + labs(caption = "", title = "Utilisation des t. en commun"),
                          gMdDrm + theme(legend.position = "none") + xlab(NULL) + labs(caption = "", title = "Utilisation d'un deux roues mot."),
                          gMdVel + theme(legend.position = "none") + xlab(NULL)+ labs(caption = ""),
                          nrow = 2, align = "hv", axis = "tblr", labels = "auto") 
page = cowplot::plot_grid(page, leg,
                          rel_heights = c(9.5,.5), nrow=2)
page = viz_Pied(page, rel_heights = c(9.5, .5), pied = src_fig(emp=F))
page = viz_Titre(page, titre = "Modélisation de la probabilité de recours\naux modes de transport (régr. logistiques)", rel_heights = c(.5, 9.5))
sortie(nom = "Modes/Modèles logit modes", taille = "page", portrait = T)
  print(page)
off()

logit(tab = mutate(PER_ff, Dis=Dis/1000, Fqc_Vco = relevel(Fqc_Vco, "1")),
               val = "modes_voiture",
               formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Vco",
               titre = "Utilisation d'une voiture",
               valIntervalleSur100 = 20, returnFig = T)

logit(tab = mutate(PER_ff, Dis=Dis/1000, Fqc_Tco = relevel(Fqc_Tco, "1")),
      val = "modes_tc",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Tco",
      titre = "Utilisation des TC",
      valIntervalleSur100 = 150, returnFig = T)

logit(tab = mutate(PER_ff, Dis=Dis/1000, Fqc_Drm = relevel(Fqc_Drm, "1")),
      val = "modes_drm",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Drm",
      titre = "Utilisation des DRM",
      valIntervalleSur100 = 650, returnFig = T)

logit(tab = mutate(PER_ff, Dis=log(Dis/1000), Fqc_Vel = relevel(Fqc_Vel, "1")),
      val = "modes_vélo",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Vel",
      titre = "Utilisation des TC",
      valIntervalleSur100 = 150, returnFig = T)

logit(tab = filter(mutate(PER_ff, Dis=Dis/1000, Fqc_Vel = relevel(Fqc_Vel, "1")), Dis < 10),
      val = "modes_vélo",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Fqc_Vel",
      titre = "Utilisation du vélo parmi jours <10km",
      valIntervalleSur100 = 150, returnFig = T)

nrow(filter(MEN, VelN > 0)) / nrow(MEN)
nrow(filter(MEN, DrmN > 0)) / nrow(MEN)

mutate(PER_ff, Dis=Dis/1000, Fqc_Tco = relevel(Fqc_Drm, "1")) |>
  left_join(select(MEN, uid_MEN, VelN), by="uid_MEN") |>
  filter(VelN>0) |>
  logit(val = "modes_drm",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Drm",
      titre = "Utilisation des DRM",
      valIntervalleSur100 = 150, returnFig = T)

logit(tab = mutate(PER_ff, Dis=Dis/1000, Fqc_Tco = relevel(Fqc_Vel, "1")),
      val = "modes_vélo",
      formule = "Activ + Genre + PCS8 + dsDomEtq + Age10 + Dis + Fqc_Vel",
      titre = "Utilisation des TC",
      valIntervalleSur100 = 150, returnFig = T)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_voiture",
      formule = "Fqc_Vco",
      titre = "Utilisation d'une voiture",
      valIntervalleSur100 = 20, returnFig = T)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_tc",
      formule = "Fqc_Tco",
      titre = "",
      valIntervalleSur100 = 20, returnFig = T)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_drm",
      formule = "Fqc_Drm",
      titre = "",
      valIntervalleSur100 = 20, returnFig = T)

logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
      val = "modes_vélo",
      formule = "Fqc_Vel",
      titre = "",
      valIntervalleSur100 = 20, returnFig = T)


# Modèle basé sur les déplacements

DEP_ff = DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, uid_PER, CoeffRecEnq), by="uid_PER")

DEP_ff = mutate(DEP_ff, Vel = ModeP %in% mode_VEL)

DEP_ff = left_join(DEP_ff,
                   select(PER_ff, Activ, Genre, PCS8, dsDomEtq,
                          Age10, uid_PER), by = "uid_PER")

gMdVel = logit(tab = valref(mutate(DEP_ff, Dist = Dis/1000)),
               val = "Vel",
               formule = "Activ + Genre + PCS8 + dsDomEtq + Dist",
               titre = "Utilisation d'un vélo",
               valIntervalleSur100 = 9, returnFig = T)

# On va filtrer pour ne garder que les motifs les plus fréquents
motifs_o = DEP_ff |> group_by(O_Motif) |>
  summarise(n = sum(CoeffRecEnq, na.rm = T)) |>
  tab_Tri(parCol = "n", rev = T) |>
  head(10)
motifs_o = sort(motifs_o$O_Motif)

motifs_d = DEP_ff |> group_by(D_Motif) |>
  summarise(n = sum(CoeffRecEnq, na.rm = T)) |>
  tab_Tri(parCol = "n", rev = T) |>
  head(10)
motifs_d = sort(motifs_d$D_Motif)

motifs = c(motifs_o, motifs_d) |> etqMotifActiv() |> unique() |> sort()
motifs = motifs[motifs != "Télétravail"]

gMdVelMotif = DEP_ff |>
  mutate(Dist = Dis / 1000) |>
  valref() |>
  mutate(O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif),
         O_Motif = relevel(O_Motif, "Domicile"),
         D_Motif = relevel(D_Motif, "Domicile")) |>
  filter(O_Motif %in% motifs, D_Motif %in% motifs) |>
  logit(val = "Vel", formule = "Activ + Genre + PCS8 + dsDomEtq + Dist + O_Motif + D_Motif",
               titre = "Utilisation d'un vélo",
               valIntervalleSur100 = 9, returnFig = T)

# Choix modaux les uns selon les autres ====

iSur100 = 100

gVoi = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
               val = "modes_voiture",
               formule = "Dis10 + Fqc_Vco + Fqc_Vpa + Fqc_Mch + Fqc_Drm + Fqc_Tco + Fqc_Vel",
               titre = "Automobile",
               valIntervalleSur100 = iSur100, returnFig = T,
               caption = NULL)

gTco = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
             val = "modes_tc",
             formule = "Dis10 + Fqc_Vco + Fqc_Vpa + Fqc_Mch + Fqc_Drm + Fqc_Tco + Fqc_Vel",
             titre = "Transports en commun",
             valIntervalleSur100 = iSur100, returnFig = T,
             caption = NULL)

gDrm = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
             val = "modes_drm",
             formule = "Dis10 + Fqc_Vco + Fqc_Vpa + Fqc_Mch + Fqc_Drm + Fqc_Tco + Fqc_Vel",
             titre = "Deux-roues motorisés",
             valIntervalleSur100 = iSur100, returnFig = T,
             caption = NULL)

gVel = logit(tab = valref(mutate(PER_ff, Dis10=Dis/10000)),
             val = "modes_vélo",
             formule = "Dis10 + Fqc_Vco + Fqc_Vpa + Fqc_Mch + Fqc_Drm + Fqc_Tco + Fqc_Vel",
             titre = "Vélo",
             valIntervalleSur100 = iSur100, returnFig = T,
             caption = NULL)

page = plot_grid(gVoi + theme(legend.position = "none"),
                 gTco + theme(legend.position = "none",
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank()),
                 get_legend(gVoi),
                 gDrm + theme(legend.position = "none"),
                 gVel + theme(legend.position = "none",
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank()),
                 NULL,
                 ncol = 3, rel_widths = c(1.5,1,.3))

print(page)

# Figure des parts modales selon densité et distance ====

PER_ff %>%
  filter(!is.na(dsDomEtq), !is.na(typoModes)) %>%
  pivot_wider(names_from = dsDomEtq, values_from = CoeffRecEnq, names_prefix = "dsDomEtq") %>%
  group_by(Genre, typoModes) %>%
  summarise(across(starts_with("dsDomEtq"), ~sum(., na.rm=T))) %>%
  mutate(across(starts_with("dsDomEtq"), ~./sum(., na.rm=T))) %>%
  pivot_longer(cols = starts_with("dsDomEtq"), names_to = "dsDomEtq", values_to = "p") %>%
  mutate(dsDomEtq = substr(dsDomEtq, 9, nchar(dsDomEtq)),
         dsDomEtq = factor(dsDomEtq, levels = niv_ds)) %>%
  ggplot(aes(x = dsDomEtq, y  = p)) +
  geom_col(aes(fill = typoModes), position = "stack", stat = "identity") +
  facet_grid(rows = "Genre")

PER_ff %>%
  filter(!is.na(dsDomEtq)) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.==T, CoeffRecEnq, 0))) %>%
  group_by(Genre, dsDomEtq) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = dsDomEtq, y = p)) +
  geom_line(aes(color = modes, group = modes)) +
  geom_point(aes(color = modes)) +
  facet_grid(rows="Genre")

# la classe supérieure de densité concerne très peu de personnes, mieux vaut la retirer ici

modes_d = PER_ff %>%
  filter(!is.na(dsDomEtq) & dsDom < 25000) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.==T, CoeffRecEnq, 0))) %>%
  group_by(dsDomEtq) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = dsDomEtq, y = p)) +
  geom_line(aes(color = modes, group = modes), linetype = 2) +
  geom_point(aes(color = modes)) +
  scale_color_manual(values = palModes(rev(c("voiture", "vélo", "car interurbain", "train",
                                "t. c. non précisé", "marche", "deux roues"))),
                     labels = c("deux-roues\nmotorisés", "marche", "transports\nen commun",
                                "train", "bus et cars", "vélo", "automobile")) +
  xlab("Densité du secteur de résidence (hab/km²)") +
  ylab("Part des travailleur⋅ses s'étant déplacé⋅es") +
  labs(title = "Utilisation des modes de transport selon la densité du secteur de résidence",
       caption = src_fig(filter(PER_ff, !is.null(dsDomEtq))))

modes_d2 = PER_ff %>%
  filter(!is.na(dsTvlEtq) & dsTvl < 25000) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.==T, CoeffRecEnq, 0))) %>%
  group_by(dsTvlEtq) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = dsTvlEtq, y = p)) +
  geom_line(aes(color = modes, group = modes), linetype=2) +
  geom_point(aes(color = modes)) +
  scale_color_manual(values = palModes(rev(c("voiture", "vélo", "car interurbain", "train",
                                             "t. c. non précisé", "marche", "deux roues"))),
                     labels = c("deux-roues\nmotorisés", "marche", "transports\nen commun",
                                "train", "bus et cars", "vélo", "automobile")) +
  xlab("Densité du secteur de résidence (hab/km²)") +
  ylab("Part des travailleur⋅ses s'étant déplacé⋅es") +
  labs(title = "Utilisation des modes de transport selon la densité du secteur de travail",
       caption = src_fig(emp = F))

sortie("Modes/Modes selon densité")
cowplot::plot_grid(modes_d  + labs(caption = NULL) + xlab(NULL) + theme(axis.text.x = element_blank()),
                   modes_d2 + theme(legend.position = "none") + theme(axis.text.x = element_text(hjust=1, angle=45)),
                   nrow = 2, axis = "lr", align = "v", rel_heights=c(.4, .6))
off()

sum(PER_ff$Dis_MAR, na.rm=T) / sum(PER_ff$Dis, na.rm=T)
sum(PER_ff$Dis_VOI, na.rm=T) / sum(PER_ff$Dis, na.rm=T)

mdDsDom = weighted.median(PER_ff$dsDom, PER_ff$CoeffRecEnqSansEMP)
mean(filter(MEN, uid_MEN %in% filter(PER_ff, dsDom < mdDsDom)$uid_MEN)$VelN, na.rm=T)
mean(filter(MEN, uid_MEN %in% filter(PER_ff, dsDom > mdDsDom)$uid_MEN)$VelN, na.rm=T)

sum(filter(PER_ff, dsDom <= 1600)$CoeffRecEnqSansEMP, na.rm=T) / sum(PER_ff$CoeffRecEnqSansEMP, na.rm=T)
sum(filter(PER_ff, dsTvl <= 1600)$CoeffRecEnqSansEMP, na.rm=T) / sum(PER_ff$CoeffRecEnqSansEMP, na.rm=T)

sum(filter(PER_ff, dsTvl > 25000)$CoeffRecEnqSansEMP, na.rm=T) / sum(PER_ff$CoeffRecEnqSansEMP, na.rm=T)

sum(filter(PER_ff, dsDomEtq == "< 200")$Dis_MAR, na.rm=T) / sum(filter(PER_ff, dsDomEtq == "< 200")$Dis, na.rm=T)
sum(filter(PER_ff, dsDomEtq == "> 25000")$Dis_MAR, na.rm=T) / sum(filter(PER_ff, dsDomEtq == "> 25000")$Dis, na.rm=T)
sum(filter(PER_ff, dsTvlEtq == "> 25000")$Dis_MAR, na.rm=T) / sum(filter(PER_ff, dsTvlEtq == "> 25000")$Dis, na.rm=T)

modes_etu = PER %>% filter(dsDom < 25000) |>
  filter(Activ %in% c("21", "22"), Age < 18) %>%
  left_join(select(shp_ZF, CODE_ZF, densite), by=c("ZF" = "CODE_ZF")) %>%
  filter(!is.na(densite)) %>%
  mutate(etiqLog = classesDensites(densite)) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.=="oui", CoeffRecEnq, 0))) %>%
  group_by(etiqLog) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = etiqLog, y = p)) +
  geom_line(aes(color = modes, group = modes), linetype=2) +
  geom_point(aes(color = modes)) +
  scale_color_manual(values = palModes(rev(c("voiture", "vélo", "car interurbain", "train",
                                             "t. c. non précisé", "marche", "deux roues"))),
                     labels = c("deux-roues\nmotorisés", "marche", "transports\nen commun",
                                "train", "bus et cars", "vélo", "automobile")) +
  xlab("Densité du secteur de résidence (hab/km²)") +
  ylab("Part des enfants/jeunes s'étant déplacé⋅es") +
  labs(title = "Utilisation des modes de transport selon la densité du secteur de résidence",
       subtitle = "Élèves et étudiant⋅es de moins de 18 ans uniquement",
       caption = src_fig(emp = F))

sortie(nom = "Modes/Modes élèves", taille = "man", h = 8, l = 17)
print(modes_etu)
off()

class(PER)
PERparMEN = PER %>%
  group_by(uid_MEN) %>% summarise(nPER = n())

left_join(PER_ff, select(MEN, uid_MEN, VelN), by="uid_MEN") %>%
  left_join(PERparMEN, by="uid_MEN") %>%
  filter(!is.na(dsDomEtq), !is.na(VelN)) %>%
  mutate(VelN = VelN / nPER) %>%
  group_by(dsDomEtq) %>% summarise(VelN = weighted.mean(VelN, w = CoeffRec))

AdulteParMEN = PER %>%
  group_by(uid_MEN) %>% filter(Age>17) %>% summarise(nPER = n())

left_join(PER_ff, select(MEN, uid_MEN, VehN), by="uid_MEN") %>%
  left_join(AdulteParMEN, by="uid_MEN") %>%
  filter(!is.na(VehN), !is.na(CoeffRec), !is.na(nPER)) %>%
  mutate(VehN = VehN / nPER) %>%
  group_by(PCS8) %>% summarise(VehN = weighted.mean(VehN, w = CoeffRec))

left_join(PER_ff, select(MEN, uid_MEN, VehN), by="uid_MEN") %>%
  left_join(PERparMEN, by="uid_MEN") %>%
  filter(!is.na(dsDomEtq), !is.na(VehN)) %>%
  mutate(VehN = VehN / nPER) %>%
  group_by(PCS8, dsDomEtq) %>% summarise(VehN = weighted.mean(VehN, w = CoeffRec)) %>%
  filter(PCS8 %in% c("03", "06"))

PER_ff %>%
  mutate(Dis = Dis/1000) %>%
  filter(Dis <= 100) %>%
  mutate(Dis = discretisation(Dis, nbClassesCible = 12)) %>%
  filter(!is.na(Dis)) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.==T, CoeffRecEnq, 0))) %>%
  group_by(Genre, Dis) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = Dis, y = p)) +
  geom_line(aes(color = modes, group = modes)) +
  geom_point(aes(color = modes)) +
  facet_grid(rows="Genre")

PER_ff %>%
  mutate(Dis = Dis/1000) %>%
  filter(Dis <= 10) %>%
  mutate(Dis = discretisation(Dis, nbClassesCible = 12)) %>%
  filter(!is.na(Dis)) %>%
  mutate(across(starts_with("modes_"), ~ifelse(.==T, CoeffRecEnq, 0))) %>%
  group_by(Genre, Dis) %>%
  summarise(across(starts_with("modes_"), ~sum(., na.rm=T)),
            pop = sum(CoeffRecEnq, na.rm=T)) %>%
  mutate(across(starts_with("modes_"), ~./pop * 100)) %>%
  pivot_longer(cols = starts_with("modes_"), names_to = "modes", values_to = "p") %>%
  mutate(modes = substr(modes, 7, nchar(modes))) %>%
  filter(!modes %in% c("avion", "bateau", "métro", "motor", "tc_light", "tgv", "trott")) %>%
  ggplot(aes(x = Dis, y = p)) +
  geom_line(aes(color = modes, group = modes)) +
  geom_point(aes(color = modes)) +
  facet_grid(rows="Genre")



# Calcul de profils d'utilisateur⋅rices ====

PER %>%
  select(uid_PER, Genre, Age10, PCS8, ZoneDens, starts_with("Fqc_"), CoeffRec) %>%
  filter(!is.na(CoeffRec)) %>%
  pivot_longer(cols=starts_with("Fqc_"), names_to = "mode", values_to = "freqMode") %>%
  pivot_wider(names_from="mode", values_from = CoeffRec, names_prefix = "mode") %>%
  group_by(freqMode) %>% summarise(across(starts_with("mode"), ~sum(., na.rm=T))) %>%
  filter(!is.na(freqMode)) %>%
  mutate(across(starts_with("modeFqc_"), ~./sum(.)*100)) %>%
  mutate(freqMode = factor(freqMode, levels=c("1","2","3","4"),
                           labels=c("Quotid.","Mensuel","Parfois","Jamais"))) %>%
  pivot_longer(starts_with("modeFqc_"), names_to = "mode", values_to = "p") %>%
  mutate(mode = substr(mode, 9, nchar(mode))) %>%
  mutate(mode = plyr::revalue(mode, c("Mch" = "marche", "Vel" = "vélo",
                                      "Drm" = "deux-roues", "Vco" = "voiture comme\nconducteur⋅rice",
                                      "Vpa" = "voiture comme\npassager⋅e", "Tco" = "T.C."))) %>%
  ggplot(aes(x = p, y = mode, fill = freqMode)) + geom_col(position="stack") +
  scale_fill_manual(values=c("indianred", "rosybrown", "mistyrose3", "lightgray"))

sortie("Modes/Profils par mode", format = "pdf", taille = "a4", portrait = F)
ACM_modes = PER %>%
  select(uid_PER, Genre, Age10, PCS8, ZoneDens, starts_with("Fqc_"), CoeffRec) %>%
  filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~ifelse(. %in% c("1","2"), "fréquent", "peu fréquent")),
         across(starts_with("Fqc_"), ~factor(., levels=c("fréquent", "peu fréquent")))) %>%
  mutate(Genre = etqGenre(Genre), PCS8 = etqPCS8(PCS8), ZoneDens=etqZoneDens(ZoneDens)) %>%
  analyseFacto(colVar = colnames(PER)[substr(colnames(PER),1,4) == "Fqc_"],
               colSup = c("Genre", "Age10", "PCS8", "ZoneDens"),
               colPoids = "CoeffRec", colUids = "uid_PER",
               titre = "Analyse factorielle des profils d'usage des modes")
off()

# cah = categ_cah(coords = filter(ACM_modes, substr(colUids,1,7) == "LOI2015"),
#                 colUids = "colUids", titre = "CAH des profils d'usager⋅es des transports\nen Loire-Atlantique")

# Trop instable pour servir
# kM = categ_kMeans(coords = ACM_modes, colUids = "colUids", titre = "CAH des profils d'usager⋅es des transports", nCateg = 4)
# 
# kM %>% 
#   left_join(rename(ACM_modes, uid = colUids), by="uid") %>%
#   ggplot(aes(x = `Dim.1`, y=`Dim.2`, colour=cluster)) + geom_point()

# PER$categUsg = NULL
# PER = left_join(PER, rename(kM, categUsg = cluster, uid_PER = uid), by="uid_PER")

# PER %>%
#   select(uid_PER, Genre, Age10, PCS8, ZoneDens, starts_with("Fqc_"), CoeffRec, categUsg) %>%
#   filter(!is.na(CoeffRec)) %>%
#   pivot_longer(cols=starts_with("Fqc_"), names_to = "mode", values_to = "freqMode") %>%
#   pivot_wider(names_from="mode", values_from = CoeffRec, names_prefix = "mode") %>%
#   group_by(categUsg, freqMode) %>% summarise(across(starts_with("mode"), ~sum(., na.rm=T))) %>%
#   filter(!is.na(freqMode)) %>%
#   mutate(across(starts_with("modeFqc_"), ~./sum(.)*100)) %>%
#   mutate(freqMode = factor(freqMode, levels=c("1","2","3","4"),
#                            labels=c("Quotid.","Mensuel","Parfois","Jamais"))) %>%
#   pivot_longer(starts_with("modeFqc_"), names_to = "mode", values_to = "p") %>%
#   mutate(mode = substr(mode, 9, nchar(mode))) %>%
#   mutate(mode = plyr::revalue(mode, c("Mch" = "marche", "Vel" = "vélo",
#                                       "Drm" = "deux-roues", "Vco" = "voiture comme\nconducteur⋅rice",
#                                       "Vpa" = "voiture comme\npassager⋅e", "Tco" = "T.C."))) %>%
#   ggplot(aes(x = p, y = mode, fill = freqMode)) + geom_col(position="stack") +
#   scale_fill_manual(values=c("indianred", "rosybrown", "mistyrose3", "lightgray")) +
#   facet_grid(~categUsg)

# PER = mutate(PER, categUsg = plyr::revalue(categUsg, c("1" = "Conducteur⋅rices",
#                                                        "2" = "Passager⋅es",
#                                                        "3" = "Automobilistes exclusif⋅ves",
#                                                        "4" = "Usager⋅es de deux-roues")))
# 
# PER_ff$categUsg = NULL
# PER_ff = left_join(PER_ff, select(PER, uid_PER, categUsg), by="uid_PER")
# PER_ff = mutate(PER_ff, categUsg = relevel(as.factor(categUsg), "Conducteur⋅rices"))

# logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
#       val = "modes_voiture",
#       formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10 + categUsg",
#       titre = "Modèle Logit Usage de l'automobile (selon profil modal)", valIntervalleSur100 = 6)
# 
# logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
#       val = "modes_voiture",
#       formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10", colComparaison="categUsg",
#       titre = "Modèle Logit Usage de l'automobile (par profil modal)", valIntervalleSur100 = 6)
# 
# logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
#       val = "modes_tc",
#       formule = "Activ + Genre + PCS8 + dsDomEtq + Dis + Age10 + categUsg",
#       titre = "Modèle Logit Usage des TC (selon profil modal)", valIntervalleSur100 = 8)
# 
# logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
#       val = "modes_tc", colComparaison = "dsDomEtq",
#       formule = "Activ + Genre + PCS8 + Dis + Age10",
#       titre = "Modèle Logit Usage des TC (Par Densité)", valIntervalleSur100 = 3)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~ifelse(. %in% c("1","2"), "fréquent", "peu fréquent")),
         across(starts_with("Fqc_"), ~factor(., levels=c("peu fréquent", "fréquent")))) %>%
  valref() %>%
  logit(val = "modes_voiture", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage de l'automobile (selon usages)", valIntervalleSur100 = 7)

# Juxtaposer sur une grande figure

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_voiture", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage de l'automobile (selon usages déclarés)", valIntervalleSur100 = 7)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_tc", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage des TC (selon usages déclarés)", valIntervalleSur100 = 60)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_vélo", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage de l'automobile (selon usages déclarés)", valIntervalleSur100 = 699)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "1"))) %>%
  valref() %>%
  logit(val = "modes_vélo", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage du vélo (selon usages déclarés)", valIntervalleSur100 = 699)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_drm", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage de DRM (selon usages déclarés)", valIntervalleSur100 = 400)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_marche", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        titre = "Modèle Logit Usage de la marche (selon usages déclarés)", valIntervalleSur100 = 2)

# Voyons même chose selon genre

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_voiture", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="Genre",
        titre = "Modèle Logit Usage de l'automobile (selon usages déclarés + Genre)", valIntervalleSur100 = 10)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_tc", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="Genre",
        titre = "Modèle Logit Usage des TC (selon usages déclarés + genre)", valIntervalleSur100 = 60)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_vélo", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="Genre",
        titre = "Modèle Logit Usage du vélo (selon usages déclarés + genre)", valIntervalleSur100 = 1500)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_drm", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="Genre",
        titre = "Modèle Logit Usage de DRM (selon usages déclarés + genre)", valIntervalleSur100 = 400)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_marche", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="Genre",
        titre = "Modèle Logit Usage de la marche (selon usages déclarés + genre)", valIntervalleSur100 = 2)

# Voyons même chose selon PCS

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_voiture", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="PCS8",
        titre = "Modèle Logit Usage de l'automobile (selon usages déclarés + PCS8)", valIntervalleSur100 = 10)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_tc", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="PCS8",
        titre = "Modèle Logit Usage des TC (selon usages déclarés + PCS8)", valIntervalleSur100 = 60)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_vélo", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="PCS8",
        titre = "Modèle Logit Usage du vélo (selon usages déclarés + PCS8)", valIntervalleSur100 = 1500)

# PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
#   mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
#   valref() %>%
#   logit(val = "modes_drm", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
#         colComparaison="PCS8",
#         titre = "Modèle Logit Usage de DRM (selon usages déclarés + PCS8)", valIntervalleSur100 = 2000)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_marche", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="PCS8",
        titre = "Modèle Logit Usage de la marche (selon usages déclarés + PCS8)", valIntervalleSur100 = 2)

# Voyons même chose selon densité

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_voiture", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="ZoneDens",
        titre = "Modèle Logit Usage de l'automobile (selon usages déclarés + ZoneDens)", valIntervalleSur100 = 10)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_tc", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="ZoneDens",
        titre = "Modèle Logit Usage des TC (selon usages déclarés + ZoneDens)", valIntervalleSur100 = 60)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_vélo", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="ZoneDens",
        titre = "Modèle Logit Usage du vélo (selon usages déclarés + ZoneDens)", valIntervalleSur100 = 1500)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_drm", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="ZoneDens",
        titre = "Modèle Logit Usage de DRM (selon usages déclarés + ZoneDens)", valIntervalleSur100 = 400)

PER_ff %>% filter(!is.na(CoeffRec)) %>% filter(if_all(starts_with("Fqc_"), .fns = ~!is.na(.))) %>%
  mutate(across(starts_with("Fqc_"), ~relevel(., "4"))) %>%
  valref() %>%
  logit(val = "modes_marche", formule="Fqc_Mch + Fqc_Vel + Fqc_Drm + Fqc_Tco + Fqc_Vco + Fqc_Vpa",
        colComparaison="ZoneDens",
        titre = "Modèle Logit Usage de la marche (selon usages déclarés + ZoneDens)", valIntervalleSur100 = 2)

# Associations de moyens de transport ====

colnames(PER)[substr(colnames(PER), 1, 5) == "modes"]

# On commence par une analyse détaillée un peu, pour rigoler !
modes = c("marche", "voiture", "vélo", "drm", "tc_route", "tc_light", "tc_rail",
          "trott", "avion", "bateau", "tgv", "métro")

sortie("Modes/Analyse facto transports jours d'enquête (détaillée)", format = "pdf", portrait = F)
af = PER %>%
  select(uid_PER, CoeffRecEnq, paste0("modes_", modes), Genre, PCS8, Age10, Activ, ZoneDens) %>%
  analyseFacto(colVar = starts_with("modes_"), colSup = c("Genre", "PCS8", "Age10", "Activ", "ZoneDens"),
               colPoids = "CoeffRecEnq", colUids = "uid_PER",
               titre = "Analyse des associations de modes de transport au cours du jour d'enquête")
off()


sortie("Modes/Analyse facto transports jours d'enquête (travail only)", format = "pdf", portrait = F)
af = PER_ff %>%
  mutate(across(starts_with("modes_"), ~ifelse(. == T, "oui", "non")),
         across(starts_with("modes_"), ~factor(., levels = c("oui", "non")))) %>%
  select(uid_PER, CoeffRecEnq, paste0("modes_", modes), Genre, PCS8, Age10, Activ, ZoneDens) %>%
  analyseFacto(colVar = starts_with("modes_"), colSup = c("Genre", "PCS8", "Age10", "Activ", "ZoneDens"),
               colPoids = "CoeffRecEnq", colUids = "uid_PER",
               titre = "Analyse des associations de modes de transport au cours du jour d'enquête")
off()

# État des lieux des véhicules ==============================================================
# montrer différences liées aux véhicules

rapport("Comparaison des véhicules par type de ménage", prim = T)
load("Data/VEH.rds")

sortie("Modes/Véhicules, exploration", format = "pdf", taille = "a4")

VEH %>% filter(Age<50) %>% ggplot(aes(x= Age)) + geom_bar() + theme_bw() +
  xlab("Âge du véhicule") + ylab("Nombre de véhicules (non pondéré)") +
  labs(title="Âge des véhicules recensés dans les EMD", caption=src_fig(bu = T, date = "sept. 2022")) %>%
  print()


VEH %>% filter(!is.na(PCSMLT)) %>%
  group_by(PCSMLT) %>%
  summarize(AgeMoy = weighted.mean(Age, Coeff, na.rm = T)) %>%
  ggplot(aes(x = PCSMLT, y=AgeMoy)) + geom_col() + coord_flip() + theme_bw() +
  labs(title="Âge moyen des voitures selon la PCS Ménage",
       caption=src_fig(bu = T, date = "sept. 2022")) + ylab("Âge moyen (années)") %>%
  print()

sortie("Âge des véhicules")
VEH %>% filter(Age<50) %>%
  mutate(uid_MEN = substr(uid_VEH, 1, 22)) %>%
  select(-PCSMLT) %>% left_join(select(MEN, uid_MEN, PCSMLT), by="uid_MEN") %>%
  filter(!PCSMLT %in% c("7e", "7i") & !is.na(PCSMLT)) %>%
  mutate(PCSMLT = etqPCSM(PCSMLT, dét=T)) %>%
  ggplot(aes(x= Age)) +
  geom_density(aes(color = PCSMLT), size=1.1, alpha=.6, key_glyph = "path") +
  xlab("Âge du véhicule") + ylab("répartition des véhicules par années d'âge") +
  scale_color_manual(name = "PCS Ménage", values = pal_PCSMT) +
  theme(legend.position = "bottom") +
  labs(title="Âge des véhicules recensés dans les EMD",
       caption=src_fig(bu = T, date = "sept. 2022")) %>%
  print()
off()

VEH %>% filter(Age<50 & Age >=0) %>% filter(!PCSMLT %in% c("IV", "VII-Ét.", "VII-In.") & !is.na(PCSMLT)) %>%
  group_by(Age, PCSMLT) %>% summarize(n = sum(Coeff)) %>%
  group_by(PCSMLT) %>% mutate(nTot = sum(n)) %>%
  mutate(part = n/nTot*100) %>%
  ggplot(aes(x= Age, y=part)) +
  geom_line(aes(color = PCSMLT), size=1.1, alpha=.9) +
  geom_area(aes(fill = PCSMLT), alpha=.2, position="identity") + scale_fill_hue(guide = "none") +
  theme_bw() +
  xlab("Âge du véhicule (années)") + ylab("Part du parc concerné dans chaque PCS Ménage (%)") +
  scale_color_hue(name = "PCSMLT") +
  labs(title="Âge des véhicules recensés dans les EMD",
       caption=src_fig(bu = T, date = "sept. 2022")) %>% print()

VEH %>% filter(!is.na(PCSMLT)) %>%
  group_by(PCSMLT, Energie) %>%
  summarize(n = sum(Coeff)) %>% mutate(n = n/sum(n) * 100) %>%
  viz_Barres(valeurs = "n", var1 = "Energie", var2 = "PCSMLT", etiqueterBarres = T,
             titre = "Type de motorisation par PCS Ménage") %>% print()

# Version de PER associée aux véhicules
PER_veh = left_join(valref(PER), rename(select(VEH, -PCSMLT, -ZoneDens), veh_Age = Age),
                    by=c("uid_VEH" = "uid_VEH")) %>% filter(typoJo == "TRAV", Activ %in% c("10", "11", "12"))

regressionLog(base = filter(PER_veh, !PCS8 %in% c("00", "08", "09")),
              val = "veh_Age", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, legVal = "distance à vol d'oiseau (km)",
              titre = "Âge du véhicule utilisé ce jour-là (travailleur⋅ses)", unite = "années",
              imprDistrib = F, refDescr = ref) %>% summary()

regressionLog(base = filter(PER_veh, PCS42S %in% PCS_privé & Activ %in% c("10", "11", "12")),
              val = "veh_Age", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, legVal = "distance à vol d'oiseau (km)",
              titre = "Âge du véhicule utilisé ce jour-là (travailleur⋅ses du privé)", unite = "années",
              imprDistrib = F, refDescr = ref) %>% summary()

regressionLog(base = filter(PER_veh, PCS42S %in% PCS_privé & Activ %in% c("10", "11", "12")),
              val = "veh_Age", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, legVal = "distance à vol d'oiseau (km)",
              titre = "Âge du véhicule utilisé ce jour-là (travailleur⋅ses du privé)", unite = "années",
              imprDistrib = F, refDescr = ref) %>% summary()

regressionLog(base = filter(PER_veh, PCS42S %in% PCS_privé & Activ %in% c("10", "11", "12")),
              val = "veh_Age", formule = "PCSMLT + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, legVal = "distance à vol d'oiseau (km)",
              titre = "Âge du véhicule utilisé ce jour-là (travailleur⋅ses du privé)", unite = "années",
              imprDistrib = F, refDescr = ref) %>% summary()
off()

PER_veh = left_join(valref(PER_ff), rename(select(VEH, -PCSMLT, -ZoneDens), veh_Age = Age),
                    by=c("uid_VEH" = "uid_VEH"))

g1 = regressionLog(base = filter(PER_veh, !PCS8 %in% c("00", "08", "09")),
              val = "veh_Age", formule = "PCS8 + Activ + dsDomEtq + Genre + Age10",
              poids="CoeffEnq", retirerZ = T, legVal = "distance à vol d'oiseau (km)",
              titre = "Âge du véhicule utilisé ce jour-là (travailleur⋅ses)", unite = "années",
              imprDistrib = F, returnFig = T)

g2 = PER_veh %>%
  mutate(Dis = Dis/1000) |>
  filter(!is.na(Energie)) %>%
  mutate(vehElec = Energie %in% c("Hybride", "Électrique")) %>%
  logit(val = "vehElec", formule = "PCS8 + Activ + Genre + Age10 + dsDomEtq")

g = cowplot::plot_grid(g1 + labs(title = "Modèle linéaire portant\nsur l'âge du véhicule", caption = NULL) + theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust=1)),
                       g2 + labs(title = "Modèle logit portant sur \nla motorisation électrique/hybride", caption = src_fig(emp = F)) + theme(axis.text.x = element_text(angle=45, hjust=1)),
                       nrow = 1, labels = c("A", "M"), align = "h", rel_widths = c(45,55))

sortie("Modes/Modèles véhicules")
  print(g)
off()

# Modèle pour estimer ce qui augmente la proba de disposer d'un véhicule électrique



PER_veh %>%
  mutate(Dis = Dis * 10) %>%
  filter(PCS8 %in% c("01", "02", "03", "04", "05", "06")) %>%
  filter(Activ != "12", ZoneDens != "4", Age < 70) %>%
  filter(!is.na(Energie)) %>%
  mutate(vehElec = Energie %in% c("Hybride", "Électrique")) %>%
  group_by(PCS8, vehElec) %>% summarise(tot = sum(Coeff.y)) %>%
  mutate(pElec = tot / sum(tot)) %>%
  filter(vehElec) %>%
  ggplot(aes(x = PCS8, y = pElec * 100)) + geom_col()

sortie("Modes/Voitures électriques", portrait = T)
PER_veh %>%
  mutate(Dis = Dis * 10) %>%
  filter(PCS42S %in% as.character(c(10:69))) %>%
  filter(Activ != "12", ZoneDens != "4", Age < 70) %>%
  filter(!is.na(Energie)) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T)) %>%
  mutate(vehElec = Energie %in% c("Hybride", "Électrique")) %>%
  group_by(PCS42S, vehElec) %>% summarise(tot = sum(Coeff.y)) %>%
  mutate(pElec = tot / sum(tot)) %>%
  filter(vehElec) %>%
  ggplot(aes(x = PCS42S, y = pElec * 100)) + geom_col() +
  coord_flip() +
  xlab("PCS du/de la principal⋅e conducteur⋅rice le jour d'enquête") +
  ylab("part de véhicules électriques (%)") +
  labs(title = "Électrification du parc automobile\nselon la PCS du/de la conducteur⋅rice",
       caption = src_fig())
off()

remove(PER_veh)

sortie("Modes/Véhicule, âge selon PCS")
g1 = VEH |>
  filter(uid_VEH %in% PER_ff$uid_VEH, !is.na(PCSMT)) |>
  filter(Age <50, !PCSMT %in% c("VII-Ét.", "VII-In.")) |>
  mutate(Age = discretisation(Age, methode = "déciles")) |>
  mutate(Age = factor(Age, levels = rev(levels(Age)))) |>
  group_by(PCSMT, Age) |> summarise(n = sum(Coeff)) |>
  group_by(PCSMT) |> mutate(p = n / sum(n) * 100) |>
  ggplot(aes(x = PCSMT, y = p)) +
  geom_col(aes(fill = Age), position = "stack") +
  scale_fill_brewer(palette = "Spectral", name = "âge du véhicule\nen années") +
  coord_flip() +
  ylab("Part de l'effectif de véhicules utilisé⋅es\npar des travailleur⋅ses (%)") +
  xlab("PCS Ménage du ménage auquel le véhicule est rattaché") +
  labs(title = "Âge du véhicule en fonction\nde la PCS Ménage du ménage",
       subtitle = ml("Véhicules conduits par des", "travailleur⋅ses de l'échantillon"),
       caption = src_fig(PER_ff, date = "août 2024"))

g2 = PER_ff %>%
  left_join(VEH, by = "uid_VEH", suffix = c("", "Veh")) %>%
  filter(AgeVeh<50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>% summarise(AgeMoy = mean(AgeVeh)) %>%
  tab_Tri(i = "PCS42S", parCol = "AgeMoy", rev=T) %>%
  ggplot(aes(x = PCS42S, y = AgeMoy)) + geom_col() +
  # scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  xlab("PCS du/de la conducteur⋅rice principal⋅e") + ylab("Âge moyen du véhicule") +
  coord_flip() +
  labs(title="Âge moyen du véhicule par\nprofession",
       subtitle=ml("Personnes s'étant déplacées pour", "travailler le jour de l'enquête"),
       caption=src_fig(date = "janvier 2023")) +
  theme(legend.position="bottom")

g = cowplot::plot_grid(g1 + theme(legend.position = "left"), g2,
                       labels = c("A", "B"))

print(g)

off()

# sortie("Modes/Véhicule, conso selon PCS42S")
PER_ff |>
  left_join(VEH, by = "uid_VEH", suffix = c("", "Veh")) %>%
  filter(Conso100<50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>% summarise(ConsoMoy = mean(Conso100, na.rm=T)) %>%
  tab_Tri(i = "PCS42S", parCol = "ConsoMoy", rev=F) %>%
  ggplot(aes(x = PCS42S, y = ConsoMoy)) + geom_col() +
  # scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  xlab("PCS") + ylab("Consommation moyenne du véhicule (L/100 km)") +
  coord_flip() +
  labs(title="Consommation moyenne du véhicule",
       subtitle="Ensemble de la population active",
       caption=src_fig(bu = F, date = "janvier 2023")) +
  theme_bw() + theme(legend.position="bottom")
# off()

sortie("Modes/Véhicule, conso selon PCS42S et genre")
PER_ff %>%
  left_join(VEH, by = "uid_VEH", suffix = c("", "Veh")) %>%
  filter(Conso100<50) %>%
  mutate(PCS42S = etqPCS42S(PCS42S), Genre=etqGenre(Genre)) %>%
  group_by(PCS42S,Genre) %>% summarise(ConsoMoy = mean(Conso100, na.rm=T)) %>%
  tab_Tri(i = "PCS42S", parCol = "ConsoMoy", rev=F) %>%
  ggplot(aes(x = PCS42S, y = ConsoMoy)) + geom_col(aes(fill=Genre), position="dodge") +
  # scale_fill_manual(name = "PCS", values = c("navyblue", "brown")) +
  xlab("PCS") + ylab("Consommation moyenne du véhicule (L/100 km)") +
  coord_flip() +
  labs(title="Consommation moyenne du véhicule",
       subtitle="Ensemble de la population active",
       caption=src_fig(bu = F, date = "janvier 2023")) +
  theme(legend.position="bottom")
off()

# Est-ce que les riches sont de gros pollueurs ?
# Quel suspense

PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  left_join(select(VEH, uid_VEH, Conso100), by="uid_VEH") %>%
  mutate(nok = ifelse(!is.na(Conso100), 1, 0)) %>%
  filter(!is.na(Conso100) & !is.na(CoeffEnq) & Conso100 < 100) %>%
  group_by(PCS8, Genre) %>%
  summarise(nok = sum(nok),
            medConso100 = weighted.median(Conso100, w=CoeffEnq, na.rm=T),
            moyConso100 = weighted.mean(Conso100, w=CoeffEnq, na.rm=T))

PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  left_join(select(VEH, uid_VEH, Conso100), by="uid_VEH") %>%
  mutate(nok = ifelse(!is.na(Conso100), 1, 0)) %>%
  filter(!is.na(Conso100) & !is.na(CoeffEnq) & Conso100 < 100) %>%
  group_by(ZoneDens) %>%
  summarise(nok = sum(nok),
            medConso100 = weighted.median(Conso100, w=CoeffEnq, na.rm=T),
            moyConso100 = weighted.mean(Conso100, w=CoeffEnq, na.rm=T))

PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  left_join(select(VEH, uid_VEH, Conso100), by="uid_VEH") %>%
  mutate(nok = ifelse(!is.na(Conso100), 1, 0)) %>%
  filter(!is.na(Conso100) & !is.na(CoeffEnq) & Conso100 < 100) %>%
  group_by(clDis) %>%
  summarise(nok = sum(nok),
            medConso100 = weighted.median(Conso100, w=CoeffEnq, na.rm=T),
            moyConso100 = weighted.mean(Conso100, w=CoeffEnq, na.rm=T))

PER %>%
  filter(uid_ENQ == "EMP2019") %>%
  left_join(select(VEH, uid_VEH, Conso100), by="uid_VEH") %>%
  filter(Activ %in% c("10", "11")) %>%
  mutate(nok = ifelse(!is.na(Conso100), 1, 0)) %>%
  filter(!is.na(Conso100) & !is.na(CoeffEnq) & Conso100 < 100) %>%
  group_by(PCS42S) %>%
  summarise(nok = sum(nok),
            medConso100 = weighted.median(Conso100, w=CoeffEnq, na.rm=T),
            moyConso100 = weighted.mean(Conso100, w=CoeffEnq, na.rm=T))

# Différences de véhicules au sein du ménage ====


# Différences modales

rapport("Base comparée des véhicules des hommes et des femmes au sein des couples")

seuilFiltre = 100
rapport("Seuil filtre :", seuilFiltre, info = T)

conjH = PER %>% filter(Lien == "1" | Lien == "2") %>%
  filter(Genre == "H")
conjF = PER %>% filter(Lien == "1" | Lien == "2") %>%
  filter(Genre == "F")

colnames(conjH) = paste0(colnames(conjH), ".H")
colnames(conjF) = paste0(colnames(conjF), ".F")

couples = left_join(select(MEN, uid_MEN), conjH, by = c("uid_MEN" = "uid_MEN.H")) %>%
  left_join(conjF, by = c("uid_MEN" = "uid_MEN.F")) %>%
  filter(!is.na(Dis.H) & !is.na(Dis.F)) %>%
  left_join(select(MEN, uid_MEN, MenEnfants), by=c("uid_MEN" = "uid_MEN")) %>%
  mutate(ActivCpl = paste("H", etqActiv(Activ.H, num=F), " & F", etqActiv(Activ.F, num=F)))

remove(conjH, conjF)

Voitures = select(couples, uid_MEN, uid_PER.H, uid_PER.F)
Voiture = left_join(Voitures, select(MEN, EnqAnnee, uid_MEN, 
                                     VehN:Veh4_Pty, Coeff), by=c("uid_MEN" = "uid_MEN"))

load("Data/TRJ.rds")
Voiture_b = TRJ %>%
  filter(as.integer(as.character(IdVeh)) %in% c(1:4)) %>%
  pivot_wider(names_from = IdVeh, values_from = Dis, names_prefix = "veh_", values_fn = sum) %>%
  group_by(uid_PER) %>% summarize(across(starts_with("veh_"), \(x) sum(x, na.rm=T)))

Voiture = left_join(Voiture, Voiture_b, by=c("uid_PER.H" = "uid_PER"), suffix = c("", ".H")) %>%
  left_join(         Voiture_b, by=c("uid_PER.F"="uid_PER"), suffix = c("", ".F"))

Voiture = Voiture %>%
  mutate(across(starts_with("veh_"), ~ifelse(is.na(.), 0, .))) %>%
  mutate(Veh1.Usg = "IND", Veh2.Usg = "IND", Veh3.Usg = "IND", Veh4.Usg = "IND",
         Veh1.Usg = ifelse(veh_01 > veh_01.F + veh_01.F*.1, "H", Veh1.Usg),
         Veh2.Usg = ifelse(veh_02 > veh_02.F + veh_02.F*.1, "H", Veh2.Usg),
         Veh3.Usg = ifelse(veh_03 > veh_03.F + veh_03.F*.1, "H", Veh3.Usg),
         Veh4.Usg = ifelse(veh_04 > veh_04.F + veh_04.F*.1, "H", Veh4.Usg),
         Veh1.Usg = ifelse(veh_01.F > veh_01 + veh_01  *.1, "F", Veh1.Usg),
         Veh2.Usg = ifelse(veh_02.F > veh_02 + veh_02  *.1, "F", Veh2.Usg),
         Veh3.Usg = ifelse(veh_03.F > veh_03 + veh_03  *.1, "F", Veh3.Usg),
         Veh4.Usg = ifelse(veh_04.F > veh_04 + veh_04  *.1, "F", Veh4.Usg),
         Veh1.Usg = ifelse(veh_01 == 0 & veh_01.F == 0,      NA, Veh1.Usg),
         Veh2.Usg = ifelse(veh_02 == 0 & veh_02.F == 0,      NA, Veh2.Usg),
         Veh3.Usg = ifelse(veh_03 == 0 & veh_03.F == 0,      NA, Veh3.Usg),
         Veh4.Usg = ifelse(veh_04 == 0 & veh_04.F == 0,      NA, Veh4.Usg))

Voiture_c = filter(Voiture, is.na(Veh3.Usg) & is.na(Veh4.Usg))

Voiture_c = mutate(Voiture_c,
                   VehH_Ann = case_when(Veh1.Usg == "H" ~ Veh1_Ann, Veh2.Usg == "H" ~ Veh2_Ann),
                   VehH_Psc = case_when(Veh1.Usg == "H" ~ Veh1_Psc, Veh2.Usg == "H" ~ Veh2_Psc),
                   VehH_Eng = case_when(Veh1.Usg == "H" ~ Veh1_Eng, Veh2.Usg == "H" ~ Veh2_Eng),
                   VehF_Ann = case_when(Veh1.Usg == "F" ~ Veh1_Ann, Veh2.Usg == "F" ~ Veh2_Ann),
                   VehF_Psc = case_when(Veh1.Usg == "F" ~ Veh1_Psc, Veh2.Usg == "F" ~ Veh2_Psc),
                   VehF_Eng = case_when(Veh1.Usg == "F" ~ Veh1_Eng, Veh2.Usg == "F" ~ Veh2_Eng)) %>%
  mutate(VehH_Age = as.integer(EnqAnnee) - VehH_Ann, VehF_Age = as.integer(EnqAnnee) - VehF_Ann) %>%
  filter(VehH_Age < 100 & VehF_Age < 100 & VehH_Age >= 0 & VehF_Age >= 0)

weighted.mean(Voiture_c$VehH_Age, Voiture_c$Coeff, na.rm=T)
weighted.mean(Voiture_c$VehF_Age, Voiture_c$Coeff, na.rm=T)

weighted.mean(Voiture_c$VehH_Psc, Voiture_c$Coeff, na.rm=T)
weighted.mean(Voiture_c$VehF_Psc, Voiture_c$Coeff, na.rm=T)

Voiture_c = Voiture_c %>%
  mutate(voitAncienne = case_when(VehH_Age > VehF_Age ~ "homme",
                                  VehF_Age > VehH_Age ~ "femme", T ~ "équivalent"),
         voitPuissante= case_when(VehH_Psc > VehF_Psc ~ "homme",
                                  VehF_Psc > VehH_Psc ~ "femme", T ~ "équivalent"))

Voiture_cf = Voiture_c |>
  filter(uid_PER.H %in% PER_ff$uid_PER & uid_PER.F %in% PER_ff$uid_PER)

g8 = Voiture_cf %>% pivot_wider(names_from = voitAncienne, values_from = Coeff,
                               names_prefix = "voitAncienne_") %>%
  group_by(tout = T) %>% summarize(across(starts_with("voitAncienne_"), sum, na.rm=T)) %>%
  pivot_longer(starts_with("voitAncienne_"), names_to = "voitAncienne", values_to = "N") %>%
  mutate(voitAncienne = plyr::revalue(voitAncienne, c("voitAncienne_équivalent" = "ni l'un·e ni l'autre",
                                                      "voitAncienne_homme"      = "l'homme",
                                                      "voitAncienne_femme"      = "la femme"))) %>%
  ggplot(aes(x = voitAncienne, y = N)) + geom_col() + xlab("") + ylab("nb ménages") +
  labs(title = "Qui conduit la voiture la plus ancienne ?")

g9 = Voiture_cf %>% pivot_wider(names_from = voitPuissante, values_from = Coeff,
                               names_prefix = "voitPuissante_") %>%
  group_by(tout = T) %>% summarize(across(starts_with("voitPuissante_"), sum, na.rm=T)) %>%
  pivot_longer(starts_with("voitPuissante_"), names_to = "voitPuissante", values_to = "N") %>%
  mutate(voitPuissante = plyr::revalue(voitPuissante, c("voitPuissante_équivalent" = "ni l'un·e ni l'autre",
                                                        "voitPuissante_homme"      = "l'homme",
                                                        "voitPuissante_femme"      = "la femme"))) %>%
  ggplot(aes(x = voitPuissante, y = N)) + geom_col() + xlab("") + ylab("nb ménages") +
  labs(title = "Qui conduit la voiture la plus puissante ?")

# Est-ce différent selon les milieux ?
g3 = Voiture_cf %>%
  left_join(select(MEN, uid_MEN, PCSMT, PCSMLT), by = "uid_MEN") |>
  pivot_wider(names_from = voitPuissante, values_from = Coeff,
                                names_prefix = "voitPuissante_") %>%
  group_by(PCSMLT) %>% summarize(across(starts_with("voitPuissante_"), sum, na.rm=T)) %>%
  pivot_longer(starts_with("voitPuissante_"), names_to = "voitPuissante", values_to = "N") %>%
  mutate(voitPuissante = plyr::revalue(voitPuissante, c("voitPuissante_équivalent" = "ni l'un·e ni l'autre",
                                                        "voitPuissante_homme"      = "l'homme",
                                                        "voitPuissante_femme"      = "la femme"))) %>%
  mutate(voitPuissante = factor(voitPuissante, levels = c("l'homme", "ni l'un·e ni l'autre", "la femme"))) |>
  group_by(PCSMLT) |> mutate(p = N / sum(N) * 100) |>
  mutate(PCSMLT = etqPCSM(PCSMLT)) |>
  ggplot(aes(x = PCSMLT, y = p)) +
  geom_col(aes(fill = voitPuissante), position = "stack") +
  scale_fill_hue(name = NULL) +
  xlab("PCS Ménage") + ylab("part des ménages (%)") +
  geom_hline(yintercept = 50, linetype = 2) +
  labs(title = "Qui conduit la voiture la plus puissante\ndu ménage, selon la PCS Ménage") +
  theme(legend.position = "bottom")

g4 = Voiture_cf %>%
  left_join(select(PER_ff, uid_PER, dsDomEtq), by = c("uid_PER.F" = "uid_PER")) |>
  filter(!is.na(dsDomEtq)) |>
  pivot_wider(names_from = voitPuissante, values_from = Coeff,
              names_prefix = "voitPuissante_") %>%
  group_by(dsDomEtq) %>% summarize(across(starts_with("voitPuissante_"), sum, na.rm=T)) %>%
  pivot_longer(starts_with("voitPuissante_"), names_to = "voitPuissante", values_to = "N") %>%
  mutate(voitPuissante = plyr::revalue(voitPuissante, c("voitPuissante_équivalent" = "ni l'un·e ni l'autre",
                                                        "voitPuissante_homme"      = "l'homme",
                                                        "voitPuissante_femme"      = "la femme"))) %>%
  mutate(voitPuissante = factor(voitPuissante, levels = c("l'homme", "ni l'un·e ni l'autre", "la femme"))) |>
  group_by(dsDomEtq) |> mutate(p = N / sum(N) * 100) |>
  ggplot(aes(x = dsDomEtq, y = p)) +
  geom_col(aes(fill = voitPuissante), position = "stack") +
  scale_fill_hue(name = NULL) +
  xlab("Densité au secteur de résidence") + ylab("part des ménages (%)") +
  geom_hline(yintercept = 50, linetype = 2) +
  labs(title = "Qui conduit la voiture la plus puissante\ndu ménage, selon la densité du secteur de rés.") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p4 = cowplot::plot_grid(g8, g9, g3, g4, ncol = 2, nrow = 2, align = "v", axis = "lr",
                        rel_heights = c(3, 7), labels = c("A", "B", "C", "D")) %>%
  viz_Titre("Utilisation des voitures dans les ménages bimotorisés\n(couples hétérosexuels de travailleur⋅ses)") |>
  viz_Pied(src_fig(mutate(Voiture_cf, uid_ENQ = substr(uid_MEN, 1, 7)), date = "août 2024"))

sortie("Modes/Véhicules H et F au sein du ménage")
print(p4)
off()

remove(VEH)

# Exploitation d'OPI =====

rapport("Exploitation des enquêtes Opinion", prim = T)

sum(filter(OPI, Age>16 & !is.na(polTspUrb_plusTco) & polTspUrb_plusTco == "1")$CoeffOpi, na.rm=T) /
  sum(filter(OPI, Age>16 & !is.na(polTspUrb_plusTco))$CoeffOpi, na.rm=T)

sum(filter(OPI, Age>16 & !is.na(polTspUrb_plusTco) & polTspUrb_plusTco == "2")$CoeffOpi, na.rm=T) /
  sum(filter(OPI, Age>16 & !is.na(polTspUrb_plusTco))$CoeffOpi, na.rm=T)

sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "1" & polTspUrb_plusTco == "2")$CoeffOpi, na.rm=T) /
  sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "1")$CoeffOpi, na.rm=T)

sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "4" & polTspUrb_plusTco == "2")$CoeffOpi, na.rm=T) /
  sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "4")$CoeffOpi, na.rm=T)

sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "1" & polTspUrb_plusTco == "1")$CoeffOpi, na.rm=T) /
  sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "1")$CoeffOpi, na.rm=T)

sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "4" & polTspUrb_plusTco == "1")$CoeffOpi, na.rm=T) /
  sum(filter(left_join(OPI, select(PER, uid_PER, Fqc_Vco), by="uid_PER"), Age>16 & !is.na(polTspUrb_plusTco) & Fqc_Vco == "4")$CoeffOpi, na.rm=T)


sortie("Transports en commun selon âge")
OPI %>%
  filter(Age>16 & !is.na(polTspUrb_plusTco)) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 85)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       caption=src_fig(bu = T, date = "2022")) + xlab("Âge") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() + facet_wrap(~Genre) %>% print()
off()

sortie("Opinion/Transports en commun selon PCSM")
OPI %>%
  filter(!is.na(polTspUrb_plusTco)) %>%
  mutate(Coeff = CoeffOpi, PCSMLT = etqPCSM(PCSMLT)) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCSMLT) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCSMLT, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS Ménage") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip()  %>% print()
off()

sortie("Opinion/Transports en commun selon PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusTco) & !is.na(PCS8)) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCS8) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCS8, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip()  %>% print()
off()

sortie("Opinion/Transports en commun chez usager·es selon PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusTco) & !is.na(PCS8) & Fqc_Tco %in% c("1", "2")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réactions à l'affirmation",
                "« Il faut continuer à développer les transports",
                "en commun, même si on est obligé pour cela de",
                "gêner les automobilistes »"),
       subtitle=ml("Parmi les personnes qui prennent",
                   "les transports en commun au moins 1x/sem"),
       caption=src_fig(emp=F, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() %>% print()
off()

sortie("Opinion/Transports en commun chez non-usager·es selon PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusTco) & !is.na(PCS8) & Fqc_Tco %in% c("4")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCS8) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCS8, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réactions à l'affirmation",
                "« Il faut continuer à développer les transports",
                "en commun, même si on est obligé pour cela de",
                "gêner les automobilistes »"),
       subtitle=ml("Parmi les personnes qui ne prennent",
                   "jamais les transports en commun"),
       caption=src_fig(emp=F)) + xlab("PCS personnelle") + ylab("Part de pop. enquêtée (%)") +
  theme_bw() + coord_flip() %>% print()

g1 = OPI %>%
  filter(Activ %in% c("10", "11", "31"), !is.na(polTspUrb_plusTco), Age<70) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 85), Genre = etqGenre(Genre)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       subtitle = "Selon l'âge",
       caption=src_fig(bu=T, emp=F)) + xlab(NULL) + ylab("Part pop. enquêtée (%)") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() + facet_wrap(~Genre)
g2 = OPI %>%
  filter(Activ %in% c("10", "11", "31"),
         PCS8 %in% c("01", "02", "03", "04", "05", "06", "09"),
         !is.na(polTspUrb_plusTco), Age<70) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,PCS8) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCS8, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       subtitle = "Selon la PCS",
       caption=src_fig(bu=T, emp=F)) + xlab(NULL) + ylab("Part pop. enquêtée (%)") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() + facet_wrap(~Genre)

fig = cowplot::plot_grid(g1 + theme(legend.position = "none") + labs(title = NULL, caption = NULL),
                   g2 + theme(legend.position = "none") + labs(title = NULL, caption=NULL),
                   nrow=1, align="v", axis = "tb")
fig = viz_Pied(fig, src_fig(bu = T, emp = F), rel_heights = c(.9, .1))
fig = cowplot::plot_grid(fig, cowplot::get_legend(g1), nrow=2, rel_heights=c(.9, .1))
fig = viz_Titre(fig, ml("Réponses à la question (population active)",
                        "« Il faut continuer à développer les transports en commun",
                        "même si on est obligé pour cela de gêner les automobilistes »"), rel_heights = c(.15, .85))

sortie("Modes/Question Tco Vs Auto")
  print(fig)
off()

# Variante de contrôle : étudiant⋅es
OPI %>%
  filter(Activ %in% c("22"), !is.na(polTspUrb_plusTco), Age<30) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 85), Genre = etqGenre(Genre)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       subtitle = "Selon l'âge",
       caption=src_fig(bu=T, emp=F)) + xlab(NULL) + ylab("Part pop. enquêtée (%)") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() + facet_wrap(~Genre)


OPI %>%
  filter(Activ %in% c("10", "11", "31"), !is.na(polTspUrb_plusTco), Age<70) %>%
  mutate(ZoneDens = etqZoneDens(ZoneDens)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,ZoneDens) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = ZoneDens, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réponses à la question",
                "« Il faut continuer à développer les transports en commun",
                "même si on est obligé pour cela de gêner les automobilistes »"),
       subtitle = "Parmi les travailleur⋅ses en emploi",
       caption=src_fig(filter(PER, uid_PER %in% OPI))) + xlab("Âge") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() + facet_wrap(~Genre) %>% print()


base = OPI %>% filter(Activ %in% c("10", "11", "12", "31"), PCS8 %in% c("01", "02", "03", "04", "05", "06"), Age<70) %>%
  left_join(select(PER, uid_PER, ZF, Fqc_Vco), by="uid_PER") %>%
  left_join(select(st_drop_geometry(shp_ZF), CODE_ZF, densite), by=c("ZF" = "CODE_ZF")) |>
  mutate(dsDomEtq = classesDensites(densite)) |>
  mutate(Age = etqAge(Age, pas = 10, min = 16, max = 70, forceMin = T)) %>%
  mutate(Age = relevel(Age, "30 à 39 ans")) %>%
  filter(polTspUrb_plusTco != "3") %>%
  mutate(plusTco = polTspUrb_plusTco == "1")

sortie("Modes/Modèle TCO", portrait=T)
logit(tab = valref(base), val = "plusTco", formule = "Age + Genre + PCS8 + dsDomEtq + Fqc_Tco + Fqc_Vco",
      titre = ml("« Il faut continuer à développer les transports",
                 "en commun, même si on est obligé pour cela",
                 "de gêner les automobilistes »"), caption = src_fig(filter(PER, uid_PER %in% OPI$uid_PER)))
off()

logit(tab = mutate(base, Fqc_Tco = etqFqc(Fqc_Tco), Fqc_Tco = factor(Fqc_Tco, levels = c("Jamais", "Parfois", "Mensuel", "Quotid."))),
      val = "plusTco", formule = "Age + Genre + PCS8 + ZoneDens + Fqc_Vco",
      titre = ml("« Il faut continuer à développer les transports",
                 "en commun, même si on est obligé pour cela de",
                 "gêner les automobilistes »"), colComparaison = "Fqc_Tco")

sortie("Opinion/Vélo chez usager·es selon PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("1","2")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réaction à la déclaration",
                "« Le vélo en ville, c'est l'avenir »"),
       subtitle="Parmi les personnes qui prennent le vélo au moins une fois/semaine",
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() %>% print()
off()

sortie("Opinion/Vélo chez non-usager·es selon PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("4")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réaction à la déclaration",
                "« Le vélo en ville, c'est l'avenir »"),
       subtitle="Parmi les personnes qui ne prennent jamais le vélo",
       caption=src_fig(emp=F)) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() %>% print()
off()

base = OPI %>% filter(Activ %in% c("10", "11", "12"), PCS8 %in% c("01", "02", "03", "04", "05", "06"), Age<70) %>%
  left_join(select(PER, uid_PER, Fqc_Vco), by="uid_PER") %>%
  mutate(Age = etqAge(Age, pas = 10, min = 16, max = 70, forceMin = T)) %>%
  mutate(Age = relevel(Age, "30 à 39 ans")) %>%
  filter(polTspUrb_plusVel != "3") %>%
  mutate(plusVel = polTspUrb_plusVel == "1")

logit(tab = base, val = "plusVel", formule = "Age + Genre + PCS8 + ZoneDens + Fqc_Vel + Fqc_Vco",
      titre = ml("« Le vélo en ville, c'est l'avenir »"), valIntervalleSur100 = 1.5)

logit(tab = mutate(base, velo = factor(ifelse(Fqc_Vel %in% c("1", "2", "3"), "cycliste", "non-cycliste"))),
      val = "plusVel", formule = "Age + Genre + PCS8 + ZoneDens + Fqc_Vco",
      titre = ml("« Le vélo en ville, c'est l'avenir »"), colComparaison = "velo")

# OPI %>%
#     filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("4")) %>%
#     mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T), Genre=etqGenre(Genre)) %>%
#     pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
#     group_by(tout = T) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
#     mutate(across(starts_with("réponse"), ~./n*100)) %>%
#     pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
#     mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
#     mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
#                                               "réponse2" = "pas d'accord",
#                                               "réponse3" = "sans opinion"))) %>% print()
# 
# OPI %>%
#     filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("4")) %>%
#     mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T), Genre=etqGenre(Genre)) %>%
#     pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
#     group_by(Genre) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
#     mutate(across(starts_with("réponse"), ~./n*100)) %>%
#     pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
#     mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
#     mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
#                                               "réponse2" = "pas d'accord",
#                                               "réponse3" = "sans opinion"))) %>% print()

sortie("Opinion/Vélo chez non-usager·es selon genre et PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("4")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T), Genre=etqGenre(Genre)) %>%
  pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = Age10, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réaction à la déclaration",
                "« Le vélo en ville, c'est l'avenir »"),
       subtitle="Parmi les personnes qui ne prennent jamais le vélo",
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() + facet_wrap(~Genre) + geom_hline(yintercept = 61.8, lty=2) %>% print()
off()

sortie("Opinion/Vélo chez usager·es selon genre et PCS")
OPI %>%
  filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("1","2")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCS8) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCS8, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réaction à la déclaration",
                "« Le vélo en ville, c'est l'avenir »"),
       subtitle="Parmi les personnes qui prennent le vélo au moins une fois/semaine",
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip()  %>% print()
off()

sortie("Opinion/Vélo chez non-usager·es selon PCS (v2)")
OPI %>%
  filter(!is.na(polTspUrb_plusVel) & !is.na(PCS8) & Fqc_Vel %in% c("4")) %>%
  mutate(Coeff = CoeffOpi, PCS8 = etqPCS8(PCS8, num=T)) %>%
  pivot_wider(names_from = polTspUrb_plusVel, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCS8) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = PCS8, y=pop)) + geom_col(aes(fill = réponse)) +
  scale_fill_manual(values = c("grey", "orange", "lightgreen")) +
  labs(title=ml("Réaction à la déclaration",
                "« Le vélo en ville, c'est l'avenir »"),
       subtitle="Parmi les personnes qui ne prennent jamais le vélo",
       caption=src_fig(bu = T, date = "2022")) + xlab("PCS personnelle") + ylab("Part de la population enquêtée (%)") +
  theme_bw() + coord_flip() %>% print()
off()

sortie("Opinion/Transports en commun selon genre")
OPI %>%
  filter(Age>16 & !is.na(polTspUrb_plusTco)) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 85)) %>%
  mutate(Coeff = CoeffOpi) %>%
  pivot_wider(names_from = polTspUrb_plusTco, values_from = CoeffOpi, names_prefix = "réponse") %>%
  group_by(PCS8,Genre,Age10) %>% summarize(across(starts_with("réponse"), sum, na.rm=T), n = sum(Coeff, na.rm=T)) %>%
  mutate(across(starts_with("réponse"), ~./n*100)) %>%
  pivot_longer(cols = starts_with("réponse"), names_to = "réponse", values_to = "pop") %>%
  mutate(réponse = factor(réponse, levels = rev(sort(unique(réponse))))) %>%
  mutate(réponse = plyr::revalue(réponse, c("réponse1" = "d'accord",
                                            "réponse2" = "pas d'accord",
                                            "réponse3" = "sans opinion"))) %>%
  ggplot(aes(x = réponse, y=pop)) + geom_col(aes(fill = Genre), position="dodge") +
  theme_bw() + coord_flip() + facet_grid(Age10~PCS8) +
  labs(caption = src_fig(bu = T, date = "2022")) %>% print()
off()

# Logits sur adjectifs OPI ====

# Voyons si 2/3 adjectifs positifs
adjs = OPI %>%
  select(uid_PER, starts_with("adj"), PCS8, ZoneDens, Activ, Age10, Genre) %>%
  mutate(across(starts_with("adj"), ~substr(.,1,1))) %>%
  mutate(across(starts_with("adj"), ~plyr::revalue(as.factor(.), c("n" = "négatif",
                                                                  "p" = "positif",
                                                                  "z" = "n/a",
                                                                  "2" = "n/a")))) %>%
  left_join(select(PER, uid_PER, CoeffRec), by="uid_PER") %>%
  rowwise() %>%
  mutate(adjsVoi = mode(c(adjVoi_1, adjVoi_2, adjVoi_3)),
         adjsTco = mode(c(adjTco_1, adjTco_2, adjTco_3)),
         adjsVel = mode(c(adjVel_1, adjVel_2, adjVel_3))) %>%
  mutate(across(starts_with("adjs"), ~ifelse(!. %in% c("positif", "négatif"), NA, as.character(.)))) %>%
  mutate(across(starts_with("adjs"), ~factor(., levels=c("positif", "négatif"))))

# Je ne sais pas pourquoi, il y a un souci avec des duplicats !
duplicats = adjs %>%
  dplyr::group_by(uid_PER, adjVoi_1, adjVoi_2, adjVoi_3, adjTco_1, adjTco_2, adjTco_3, adjVel_1, adjVel_2, adjVel_3, PCS8, ZoneDens, Activ, Age10, Genre,
                  adjsTco, adjsVel, adjsVoi) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)
adjs = filter(adjs, !uid_PER %in% duplicats$uid_PER)
rm(duplicats)

adjs %>%
  pivot_wider(names_from = adjsVoi, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  group_by(adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col() +
  labs(title = "Appréhension de la voiture", subtitle = "Échantillon général") +
  ylab("part (%)")

adjs %>%
  filter(Age10 != "0 à 9 ans", PCS8 != "00", PCS8 != "09") %>%
  filter(Activ %in% c("10", "11", "12", "31")) %>%
  filter(!is.na(adjsVoi),
         !is.na(PCS8), !is.na(ZoneDens), !is.na(Activ), !is.na(Age10), !is.na(Genre)) %>%
  mutate(adjsVoi = adjsVoi == "positif") %>%
  valref() %>%
  logit(val = "adjsVoi", formule = "PCS8 + ZoneDens + Activ + Age10 + Genre",
        titre = "Appréciation de la voiture")

sortie("Modes/Modèle appréciation voiture", portrait=T)
adjs %>%
  left_join(select(PER, uid_PER, Fqc_Vco, Age, dsDomEtq), by="uid_PER") %>%
  filter(Activ %in% c("10", "11", "31"),
         Age > 15, Age < 70, PCS8 %in% paste0("0", c(1:6))) |>
  mutate(Age10 = plyr::revalue(Age10, c("10 à 19 ans" = "16 à 19 ans"))) %>%
  filter(!is.na(adjsVoi),
         !is.na(PCS8), !is.na(Activ), !is.na(Age10), !is.na(Genre), !is.na(dsDomEtq)) %>%
  mutate(adjsVoi = adjsVoi == "positif") %>%
  valref() %>%
  logit(val = "adjsVoi", formule = "PCS8 + dsDomEtq + Activ + Age10 + Genre + Fqc_Vco",
        titre = "Appréciation de la voiture\nselon les enquêté⋅es (actif⋅ves)",
        caption = src_fig(emp = F))
off()

sortie("Modes/Voiture polluante", portrait=T)
OPI %>%
  filter(Activ %in% c("10", "11", "31"),
         Age > 15, Age < 70, PCS8 %in% paste0("0", c(1:6))) |>
  mutate(Age10 = plyr::revalue(Age10, c("10 à 19 ans" = "16 à 19 ans"))) %>%
  left_join(select(PER, uid_PER, dsDomEtq, Fqc_Vco), by="uid_PER") %>%
  mutate(polluant = adjVoi_1 == "n_polluant" | adjVoi_2 == "n_polluant" | adjVoi_3 == "n_polluant") %>%
  valref() %>%
  logit(val = "polluant", formule = "PCS8 + dsDomEtq + Activ + Age10 + Genre + Fqc_Vco",
        titre = "Probabilité de citer l'adjectif «polluant»\npour désigner le mode automobile\nselon les enquêté⋅es (actif⋅ves)",
        caption = src_fig(emp = F))
off()

adjs %>%
  pivot_wider(names_from = adjsTco, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  group_by(adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col() +
  labs(title = "Appréhension des TC") +
  ylab("part (%)")

adjs %>%
  pivot_wider(names_from = adjsVel, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  group_by(adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col() +
  labs(title = "Appréhension du vélo") +
  ylab("part (%)")


g1 = adjs %>%
  pivot_wider(names_from = adjsVoi, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  left_join(select(PER, uid_PER, Fqc_Vco), by="uid_PER") %>%
  filter(!is.na(Fqc_Vco)) %>%
  mutate(Fqc_Vco = factor(Fqc_Vco, levels=c("1","2","3","4"),
                          labels = c("conduit quotidiennement",
                                     "conduit 1x/sem", "conduit 1x/mois",
                                     "ne conduit pas"))) %>%
  group_by(Fqc_Vco, adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col(aes(fill = adjectif)) +
  labs(title = "Appréhension de la voiture", subtitle = "Selon le rapport à la conduite") +
  ylab("part (%)") + facet_wrap(~Fqc_Vco, nrow = 1) +
  scale_fill_manual(values = c("grey", "tomato", "olivedrab3")) +
  theme(legend.position = "none")

g2 = adjs %>%
  pivot_wider(names_from = adjsTco, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  left_join(select(PER, uid_PER, Fqc_Tco), by="uid_PER") %>%
  filter(!is.na(Fqc_Tco)) %>%
  mutate(Fqc_Tco = factor(Fqc_Tco, levels=c("1","2","3","4"),
                          labels = c("utilise quotidiennement",
                                     "utilise 1x/sem", "utilise 1x/mois",
                                     "n'utilise pas"))) %>%
  group_by(Fqc_Tco, adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col(aes(fill = adjectif)) +
  labs(title = "Appréhension des transports en commun (TC)", subtitle = "Selon l'usage des TC") +
  ylab("part (%)") + facet_wrap(~Fqc_Tco, nrow = 1) +
  scale_fill_manual(values = c("grey", "tomato", "olivedrab3")) +
  theme(legend.position = "none")

g3 = adjs %>%
  pivot_wider(names_from = adjsVel, values_from = CoeffRec, names_prefix = "adj_") %>%
  pivot_longer(cols = starts_with("adj_"), names_to = "adjectif", values_to = "n") %>%
  left_join(select(PER, uid_PER, Fqc_Vel), by="uid_PER") %>%
  filter(!is.na(Fqc_Vel)) %>%
  mutate(Fqc_Vel = factor(Fqc_Vel, levels=c("1","2","3","4"),
                          labels = c("utilise quotidiennement",
                                     "utilise 1x/sem", "utilise 1x/mois",
                                     "n'utilise pas"))) %>%
  group_by(Fqc_Vel, adjectif) %>% summarise(n = sum(n, na.rm=T)) %>%
  mutate(p = n / sum(n) * 100) %>% ggplot(aes(x = adjectif, y = p)) + geom_col(aes(fill = adjectif)) +
  labs(title = "Appréhension du vélo", subtitle = "Selon l'usage du vélo") +
  ylab("part (%)") + facet_wrap(~Fqc_Vel, nrow = 1) +
  scale_fill_manual(values = c("grey", "tomato", "olivedrab3")) +
  theme(legend.position = "none")

p = plot_grid(g1, g2, g3, ncol=1) |>
  viz_Pied(src_fig(emp=F))

sortie("Modes/Corrélation usage et opinion", taille = "page", portrait=T)
print(p)
off()

velOpiZT = adjs %>%
  pivot_wider(names_from = adjsVel, values_from = CoeffRec, names_prefix = "adj_") %>%
  left_join(select(PER, uid_PER, ZT), by="uid_PER") %>%
  group_by(ZT) %>% summarise(across(starts_with("adj_"), sum, na.rm=T)) %>%
  mutate(partPositif = adj_positif / (adj_positif + adj_négatif + adj_NA) * 100)

load("Data/shp_ZT.rds")

map_CarteParEnquete(shp = left_join(shp_ZT, velOpiZT, by="ZT"),
                    colUidEnq = "uid_ENQ", colVal = "partPositif",
                    pdf = "Appréhension Vélo.pdf",
                    format = "A4", paysage = T, titre = "Appréhension du vélo",
                    legende = "Part de répondant⋅es\njugeant le vélo\npositivement", unite = "%",
                    nClasses=6, mClasses="quantile")

tcoOpiZT = adjs %>%
  pivot_wider(names_from = adjsTco, values_from = CoeffRec, names_prefix = "adj_") %>%
  left_join(select(PER, uid_PER, ZT), by="uid_PER") %>%
  group_by(ZT) %>% summarise(across(starts_with("adj_"), sum, na.rm=T)) %>%
  mutate(partPositif = adj_positif / (adj_positif + adj_négatif + adj_NA) * 100)

load("Data/shp_ZT.rds")

map_CarteParEnquete(shp = left_join(shp_ZT, velOpiZT, by="ZT"),
                    colUidEnq = "uid_ENQ", colVal = "partPositif",
                    pdf = "Sorties/Modes/Appréhension Vélo.pdf",
                    format = "A4", paysage = T, titre = "Appréhension du vélo",
                    legende = "Part de répondant⋅es\njugeant le vélo\npositivement", unite = "%",
                    nClasses=6, mClasses="quantile")

map_CarteParEnquete(shp = left_join(shp_ZT, tcoOpiZT, by="ZT"),
                    colUidEnq = "uid_ENQ", colVal = "partPositif",
                    pdf = "Sorties/Modes/Appréhension TC.pdf",
                    format = "A4", paysage = T, titre = "Appréhension des TC",
                    legende = "Part de répondant⋅es\njugeant les TC\npositivement", unite = "%",
                    nClasses=6, mClasses="quantile")


# Introduisons des modèles logit simples
# Qui n'aime pas la voiture ?
adjs %>%
  mutate(adjNeg = adjsVoi == "négatif") %>%
  valref() %>%
  logit(val = "adjNeg", formule = "Genre + Age10 + PCS8 + ZoneDens",
        titre = "Qui n'aime pas la voiture ?")

adjs %>%
  mutate(adjNeg = adjsVel == "négatif") %>%
  valref() %>%
  logit(val = "adjNeg", formule = "Genre + Age10 + PCS8 + ZoneDens",
        titre = "Qui n'aime pas le vélo ?")

adjs %>%
  mutate(adjPos = adjsVel == "positif") %>%
  valref() %>%
  logit(val = "adjPos", formule = "Genre + Age10 + PCS8 + ZoneDens",
        titre = "Qui aime pas le vélo ?")

levels(OPI$adjVoi_1)

# Calcul des résidus moyens par secteur ====

modVoit = logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
                val = "modes_voiture",
                formule = "Activ + Genre + PCS8 + dsDomEtq + Dis",
                titre = "Modèle Logit Usage de l'automobile (Distance)",
                valIntervalleSur100 = 8, returnFig = F)
modVelo = logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
                val = "modes_vélo",
                formule = "Activ + Genre + PCS8 + dsDomEtq + Dis",
                titre = "Modèle Logit Usage du vélo (Distance)",
                valIntervalleSur100 = 8, returnFig = F)
modTC = logit(tab = valref(mutate(PER_ff, Dis=Dis/1000)),
                val = "modes_tc",
                formule = "Activ + Genre + PCS8 + dsDomEtq + Dis",
                titre = "Modèle Logit Usage des TC (Distance)",
                valIntervalleSur100 = 8, returnFig = F)

# Réassocions le résultat
resVoit = tibble(uid_PER = filter(PER_ff, !is.na(Activ) & !is.na(Genre) &
                                    !is.na(PCS8) & !is.na(dsDomEtq) &
                                    !is.na(Dis) & !is.na(modes_voiture))$uid_PER,
                 residuModVoit = (modVoit$residuals - 1)*100)
resVelo = tibble(uid_PER = filter(PER_ff, !is.na(Activ) & !is.na(Genre) &
                                    !is.na(PCS8) & !is.na(dsDomEtq) &
                                    !is.na(Dis) & !is.na(modes_vélo))$uid_PER,
                 residuModVelo = (modVelo$residuals - 1)*100)
resTC = tibble(uid_PER = filter(PER_ff, !is.na(Activ) & !is.na(Genre) & 
                                  !is.na(PCS8) & !is.na(dsDomEtq) &
                                  !is.na(Dis) & !is.na(modes_tc))$uid_PER,
                 residuModTC = (modTC$residuals - 1) * 100)

PER_ff = left_join(PER_ff, resVoit, by="uid_PER") %>%
         left_join(        resVelo, by="uid_PER") %>%
         left_join(        resTC,   by="uid_PER")

# Calculons la moyenne par ZT
ZT_Res_Moy = PER_ff %>%
  group_by(ZT) %>% summarise(across(starts_with("residu"), ~weighted.mean(., CoeffRecEnq, na.rm=T)))

load("Data/shp_ZT.rds")

map_CarteParEnquete(shp = left_join(shp_ZT, ZT_Res_Moy, by="ZT"),
                    colUidEnq = "uid_ENQ", colVal = "residuModVoit",
                    pdf = "Résidus Recours Automobile.pdf",
                    format = "A4", paysage = T, titre = "Résidus Choix Modal Automobile",
                    legende = "Résidus par\nrapport au\nmodèle de\nchoix modal", unite = "%",
                    nClasses=6, mClasses="jenks")

# Trajets IDF ====

# Idéalement, on utiliserait TRJ. Mais on ne peut pas, parce que les trajets ne sont pas horodatés.
# Ça craint. Que faire d'autre que de se contenter de DEP ?

# Tous les quarts d'heure par heure de pointe, repérer la part des gens en déplacement selon PCS

heures = seq(from = 6, to = 21, by = 1/2)

tab_DEP_parH = function(heures, enq = NULL, modes = NULL){
  tabPartCategs = tibble(h = heures,
                         hLab = rep(NA, length(heures)),
                         pPCS01 = rep(NA, length(heures)),
                         pPCS02 = rep(NA, length(heures)),
                         pPCS03 = rep(NA, length(heures)),
                         pPCS04 = rep(NA, length(heures)),
                         pPCS05 = rep(NA, length(heures)),
                         pPCS06 = rep(NA, length(heures)),
                         pPCSinac = rep(NA, length(heures)),
                         n = rep(NA, length(heures)))
  
  b = ui_ProgInit(length(heures)-1)
  
  for (i in 1:(length(heures)-1)) {
    h = tabPartCategs[[i,1]]
    h1= tabPartCategs[[i+1,1]]
    
    trajets = filter(DEP,
                     heureHHMMtoM(O_Hr)/60 >= h,
                     heureHHMMtoM(D_Hr)/60 <  h1)
    
    if (!is.null(enq)) { trajets = filter(trajets, uid_ENQ %in% enq) }
    
    if (!is.null(modes)) { trajets = filter(trajets, ModeP %in% modes) }
    
    # pop = trajets |>
    #   left_join(select(PER, uid_PER, PCS8, CoeffRecEnq), by="uid_PER") |>
    #   filter(!is.na(PCS8)) |>
    #   mutate(PCS8 = ifelse(!uid_PER %in% PER_ff$uid_PER, "inac", as.character(PCS8))) |>
    #   filter(PCS8 %in% c("01", "02", "03", "04", "05", "06", "inac"))
    # pop = sum(pop$CoeffRecEnq,na.rm=T)
    
    parts = trajets |>
      left_join(select(PER, uid_PER, PCS8, CoeffRecEnq), by="uid_PER") |>
      filter(!is.na(PCS8)) |>
      group_by(uid_PER) |> summarise(PCS8 = first(PCS8), CoeffRecEnq = first(CoeffRecEnq)) |>
      mutate(PCS8 = ifelse(!uid_PER %in% PER_ff$uid_PER, "inac", as.character(PCS8))) |>
      filter(PCS8 %in% c("01", "02", "03", "04", "05", "06", "inac")) |>
      pivot_wider(names_from = PCS8, values_from = CoeffRecEnq, names_prefix = "pPCS", names_sort = T) |>
      summarise(across(starts_with("pPCS"), \(x) sum(x, na.rm=T)), n = n()) # |>
    # rowwise() |>
    # mutate(across(starts_with("pPCS"), ~./pop))
    
    tabPartCategs[i, 3] = ifelse(is.null(parts$pPCS01), 0, parts$pPCS01)
    tabPartCategs[i, 4] = ifelse(is.null(parts$pPCS02), 0, parts$pPCS02)
    tabPartCategs[i, 5] = ifelse(is.null(parts$pPCS03), 0, parts$pPCS03)
    tabPartCategs[i, 6] = ifelse(is.null(parts$pPCS04), 0, parts$pPCS04)
    tabPartCategs[i, 7] = ifelse(is.null(parts$pPCS05), 0, parts$pPCS05)
    tabPartCategs[i, 8] = ifelse(is.null(parts$pPCS06), 0, parts$pPCS06)
    tabPartCategs[i, 9] = ifelse(is.null(parts$pPCSinac), 0, parts$pPCSinac)
    tabPartCategs[i,10] = ifelse(is.null(parts$n), 0, parts$n)
    
    ui_Prog(b, i)
  }
  
  for (i in 1:(length(heures)-1)) {
    h = tabPartCategs[[i,1]]
    h1= tabPartCategs[[i+1,1]]
    
    tabPartCategs[i,2] = paste0("de ", heureMinToHr(h*60), " à ", heureMinToHr(h*60))
  }
  
  return(tabPartCategs)
}


# TODO: deux colonnes n... impossible d'exécuter le code...
tabPartCategs = tab_DEP_parH(heures = heures, enq = "IDF2010")

tabPartCategs |>
  pivot_longer(cols = starts_with("pPCS"), names_to = "PCS", values_to = "n") |>
  ggplot(aes(x = h, y = n)) + geom_line(aes(colour = PCS, group  = PCS)) # |>
  scale_colour_manual(values = pal_PCS8[c(1:6, 8)])

tabPartCategs |>
  rowwise() |>
  mutate(popTot = sum(across(stat_metrts_with("pPCS")))) |>
  mutate(across(starts_with("pPCS"), ~. / popTot)) |>
  pivot_longer(cols = starts_with("pPCS"), names_to = "PCS", values_to = "p") |>
  ggplot(aes(x = h, y = p, fill = PCS)) + geom_col(position = "stack")


gphSrMode = function(tabPartCategs) {
  # Un graphique de la sur ou sous-représentation serait peut-être plus clair
  partsPCS = PER_ff |>
    group_by(PCS8) |> summarise(n = sum(CoeffRecEnq, na.rm=T)) |> mutate(p = n / sum(n)) |>
    select(-n) |>
    pivot_wider(names_from = PCS8, names_prefix = "pmoyPCS", values_from = p)
  
  g = tabPartCategs |>
    filter(n > seuilSignifiant) |>
    select(-pPCSinac) |>
    rowwise() |>
    mutate(popTot = sum(across(starts_with("pPCS")))) |>
    mutate(across(starts_with("pPCS"), ~. / popTot)) |>
    mutate(jointure = T) |>
    left_join(mutate(partsPCS, jointure = T), by="jointure") |>
    mutate(sr03 = ((pPCS03 / pmoyPCS03) - 1) * 100,
           sr04 = ((pPCS04 / pmoyPCS04) - 1) * 100,
           sr05 = ((pPCS05 / pmoyPCS05) - 1) * 100,
           sr06 = ((pPCS06 / pmoyPCS06) - 1) * 100) |>
    pivot_longer(cols = starts_with("sr"), names_to = "PCS", values_to = "sr") |>
    ggplot(aes(x = h, y = sr, colour = PCS)) + geom_line() +
    scale_colour_manual(values = pal_PCS8[3:6], labels = niv_PCS8[3:6]) +
    scale_y_continuous(trans = trans_sur100,
                       breaks = transf_echelle_sur100_inverse(c(-3, -2, -1, 0, 1, 2)),
                       labels = transf_echelle_sur100_lab(transf_echelle_sur100_inverse(c(-3, -2, -1, 0, 1, 2)))) +
    scale_x_continuous(breaks = c(5:21)) +
    xlab("heure") + ylab("surreprésentation") +
    geom_hline(yintercept = 0, lty = 2) 
  
  return(g)
}

gphSrMode(tabPartCategs)

# À refaire pour train, tram, bus, métro

heures=c(4:24)

t_met = tab_DEP_parH(heures = heures, modes = "33")
g_met = gphSrMode(t_met)

t_bus = tab_DEP_parH(heures = heures, modes = c("31"))
g_bus = gphSrMode(t_bus)

t_trn = tab_DEP_parH(heures = heures, modes = c("41", "42", "43", "44"))
g_trn = gphSrMode(t_trn)

t_trm = tab_DEP_parH(heures = heures, modes = "32")
g_trm = gphSrMode(t_trm)

fig = cowplot::plot_grid(g_met + coord_cartesian(ylim = c(-80, 300)) +
                           theme(legend.position = "none") +
                           ggtitle("Métro"),
                         g_trm + coord_cartesian(ylim = c(-80, 300)) +
                           ggtitle("Tramway"),
                         g_bus + coord_cartesian(ylim = c(-80, 300)) +
                           theme(legend.position = "none") +
                           ggtitle("Bus urbain"),
                         ncol = 1, align = "hv", axis = "tblr")
fig = fig |> viz_Titre("Surreprésentation des travailleur⋅ses dans les transports\npar rapport à la part qu'elles et ils représentent\ndans la population active en emploi") |>
  viz_Pied(src_fig(bu=T, emp=T))

sortie("Modes/Surrep trav TC", taille = "carré")
  print(fig)
off()

# Figure toutes enquêtes

tabPartCategs = tab_DEP_parH(heures = heures, modes = mode_TCO)
tabPartCategs |>
  pivot_longer(cols = starts_with("pPCS"), names_to = "PCS", values_to = "nb") |>
  ggplot(aes(x = h, y = nb)) + geom_line(aes(colour = PCS, group  = PCS))
fig = tabPartCategs |>
  pivot_longer(cols = starts_with("pPCS"), names_to = "PCS", values_to = "nb") |>
  mutate(PCS = factor(PCS, levels=c("pPCSinac", "pPCS01", "pPCS02", "pPCS03",
                                    "pPCS04", "pPCS05", "pPCS06"))) |>
  ggplot(aes(x = h, y = nb, fill = PCS)) + geom_col(position = "stack") +
  scale_fill_manual(values = c(pal_PCS8[8], pal_PCS8[1:6]),
                    labels = c("Travailleur⋅ses\nne travaillant pas\nce jour-là et\nreste de la\npopulation",
                               niv_PCS8[1:6])) +
  scale_x_continuous(breaks=c(2:11)*2) +
  scale_y_continuous(labels = ~ format(.x, scientific = F)) + # https://stackoverflow.com/questions/52758313/avoid-scientific-notation-x-axis-ggplot
  labs(title = "Nombre de personnes utilisant les transports en commun\npar heure de la journée, France entière",
       caption = src_fig(bu = T, emp = T)) +
  xlab("heure de la journée") +
  ylab("nombre de personnes (pondéré)")

sortie("Modes/Nb TC")
  print(fig)
off()

# On vérifie que le chiffre est réaliste
sum(filter(PER, modes_tc == "oui")$CoeffEnq) / sum(filter(PER, Dis>0)$CoeffEnq)
sum(filter(PER, modes_tc == "oui")$CoeffEnq)
sum(PER$CoeffRecEnq, na.rm=T)
sum(filter(PER, modes_tc == "oui")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Dis>0)$CoeffRecEnq, na.rm=T)


# Vélo ====

initMémoire(BasesCharger = c("PER", "DEP"))
PER_ff = init_PER_ff(PER)

rapport("Import des densités par secteur dans DEP")

# On charge les tableaux qui, en principe, comportent la densité
load("Data/shp_ZF.rds")
load("Data/shp_COM.rds")

DEP = filter(DEP, uid_PER %in% PER_ff$uid_PER)

# On joint la densité au lieu de résidence…
# Si disponible au niveau ZF, on l'utilise
DEP_zt_zf = DEP %>%
  left_join(select(shp_ZF, CODE_ZF, densite), by=c("O_ZF" = "CODE_ZF")) %>%
  filter(!is.na(densite))
# Sinon, on utilise la densité communale
DEP_zt_com = DEP %>%
  left_join(select(shp_COM, insee, densite), by=c("O_Com" = "insee")) %>%
  filter(!O_ZF %in% DEP_zt_zf$O_ZF)
# On joint le tout
DEP_zt = rbind(DEP_zt_zf, DEP_zt_com) %>% 
  mutate(etiqLog = classesDensites(densite)) %>%
  filter(Dis>0)

# On renomme
DEP_zt = rename(DEP_zt, dsO = densite, dsO_Etq = etiqLog)

# On joint ensemble
DEP = left_join(DEP, select(DEP_zt, uid_PER, dsO, dsO_Etq), by="uid_PER")
# Il y a quelque-chose d'anormal ici à vérifier
# Joiture many-to-many détectée

# On prend soin de la mémoire
remove(shp_ZF, shp_COM, DEP_zt, DEP_zt_zf, DEP_zt_com) ; gc()


# Maintenant, on lance les modèles
extrPER = select(PER_ff, uid_PER, Genre, PCS8, Activ, CoeffRecEnq)
remove(PER_ff) # mémoire surchargée
gc()

g = DEP |>
  left_join(extrPER, by = "uid_PER") |>
  mutate(Dist = Dis/1000) |>
  mutate(MdVelo = ModeP %in% mode_VEL) |>
  valref() |>
  logit(val = "MdVelo", formule = "Dist + Genre + PCS8 + Activ + dsO_Etq",
        titre = "Probabilité d'utiliser un vélo\npour réaliser un déplacement",
        caption = src_fig(emp = F))

sortie("Modes/Modèle déplacement vélo simple", portrait=T)
print(g)
off()

# A tester selon le motif de trajet, en reprenant la méthode développée
# pour la marche, ci-dessous

tabGen = DEP |>
  left_join(extrPER, by = "uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(ModeP = etqMode(ModeP, simple = T),
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-")) |>
  group_by(Motif) |>
  summarise(nb = sum(CoeffRecEnq)) |>
  ungroup() |>
  mutate(p = nb/sum(nb) * 100) |>
  mutate(Motif = ifelse(p < 2, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  group_by(Motif) |>
  summarise(p = sum(p))

g = DEP |>
  left_join(extrPER, by = "uid_PER") |>
  mutate(Dist = Dis/1000,
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-"),
         Motif = ifelse(!Motif %in% tabGen$Motif, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  mutate(MdVelo = ModeP %in% mode_VEL) |>
  valref() |>
  logit(val = "MdVelo", formule = "Dist + Genre + PCS8 + Activ + dsO_Etq + Motif",
        titre = "Probabilité d'utiliser un vélo\npour réaliser un déplacement\n(motifs détaillés)",
        caption = src_fig(emp = F))


# Marche =====

initMémoire(BasesCharger = c("PER", "DEP"))
PER_ff = init_PER_ff(PER)
remove(PER)
PER_ff = densitesZversPER(PER_ff)

# Part < 1km et < 3km.
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis < 3000)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis < 1000)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)



sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR == Dis)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & Dis_MAR > 0)$CoeffRecEnq) / sum(filter(PER_ff, !is.na(CoeffRecEnq))$CoeffRecEnq)

PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(QueMarche = Dis_MAR == Dis) |>
  group_by(Genre, QueMarche) |>
  summarise(nPop = sum(CoeffRecEnq)) |>
  group_by(Genre) |>
  mutate(p = nPop / sum(nPop))

PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(QueMarche = Dis_MAR == Dis) |>
  group_by(PCS8, QueMarche) |>
  summarise(nPop = sum(CoeffRecEnq)) |>
  group_by(PCS8) |>
  mutate(p = nPop / sum(nPop)) |>
  filter(QueMarche)

PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(QueMarche = Dis_MAR == Dis) |>
  mutate(DisClasse = discretisation(Dis/1000)) |>
  group_by(DisClasse, QueMarche) |>
  summarise(nPop = sum(CoeffRecEnq)) |>
  group_by(DisClasse) |>
  mutate(p = nPop / sum(nPop)) |>
  filter(QueMarche)

PER_ff |>
  filter(!is.na(CoeffRecEnq), Dis < 25000) |>
  mutate(QueMarche = Dis_MAR == Dis) |>
  mutate(DisClasse = discretisation(Dis/1000)) |>
  group_by(DisClasse, QueMarche) |>
  summarise(nPop = sum(CoeffRecEnq)) |>
  group_by(DisClasse) |>
  mutate(p = nPop / sum(nPop)) |>
  filter(QueMarche)

# et avec un paramètre urbain/rural ?
medDs = weighted.median(PER_ff$dsDom, PER_ff$CoeffRecEnqSansEMP)
medDsTv = weighted.median(PER_ff$dsTvl, PER_ff$CoeffRecEnqSansEMP)

g = PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP), Dis <= 10000, !is.na(dsDom)) |>
  mutate(QueMarche = Dis_MAR == Dis, Rural = dsDom < medDs) |>
  mutate(DisClasse = discretisation(Dis/1000, nbClassesCible = 12)) |>
  filter(Dis < 10000) |>
  group_by(DisClasse, QueMarche, Rural) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(DisClasse, Rural) |>
  mutate(p = nPop / sum(nPop) * 100) |>
  filter(QueMarche) |>
  ggplot(aes(x = DisClasse, y = p)) +
  geom_line(aes(colour = Rural, group = Rural)) +
  geom_point(aes(colour = Rural)) +
  scale_colour_manual(values = c("skyblue3", "goldenrod"),
                      name = "Densité secteur\nde domicile",
                      labels = c("supérieure\nà la médiane",
                                 "inférieure\nà la médiane")) +
  xlab("Distance parcourue dans la journée (en km)") +
  ylab("Part des travailleur⋅ses (%)") +
  labs(title = "Journées dont 100% des déplacements sont effectués à pied",
       caption = src_fig(emp=F))

sortie("Modes/Journées à pied")
print(g)
off()

PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP), Dis <= 10000, !is.na(dsTvl)) |>
  mutate(QueMarche = Dis_MAR == Dis, Rural = dsTvl < medDs) |>
  mutate(DisClasse = discretisation(Dis/1000, nbClassesCible = 12)) |>
  filter(Dis < 10000) |>
  group_by(DisClasse, QueMarche, Rural) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(DisClasse, Rural) |>
  mutate(p = nPop / sum(nPop) * 100) |>
  filter(QueMarche) |>
  ggplot(aes(x = DisClasse, y = p)) +
  geom_line(aes(colour = Rural, group = Rural)) +
  geom_point(aes(colour = Rural)) +
  scale_colour_manual(values = c("skyblue3", "goldenrod"),
                      name = "Densité secteur\ndu lieu d'emploi\nprincipal",
                      labels = c("supérieure\nà la médiane",
                                 "inférieure\nà la médiane")) +
  xlab("Distance parcourue dans la journée (en km)") +
  ylab("Part des travailleur⋅ses (%)") +
  labs(title = "Journées dont 100% des déplacements sont effectués à pied",
       caption = src_fig(PER_ff))


PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP), Dis <= 10000, !is.na(dsDom)) |>
  mutate(QueMarche = Dis_MAR == Dis, Rural = dsDom < medDs) |>
  mutate(DisClasse = discretisation(Dis/1000, nbClassesCible = 12)) |>
  filter(Dis < 10000) |>
  group_by(DisClasse, QueMarche, Rural, PCS8) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(DisClasse, Rural, PCS8) |>
  mutate(p = nPop / sum(nPop) * 100) |>
  filter(QueMarche) |>
  ggplot(aes(x = DisClasse, y = p)) +
  geom_line(aes(colour = Rural, group = Rural)) +
  geom_point(aes(colour = Rural)) +
  facet_wrap(~PCS8)


PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  mutate(QueMarche = Dis_MAR == Dis) |>
  mutate(Dis = round(Dis/1000)) |>
  filter(Dis <= 20) |>
  group_by(Genre, PCS8, Dis, QueMarche) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(Genre, PCS8, Dis) |>
  mutate(p = nPop / sum(nPop)) |>
  filter(QueMarche) |>
  ggplot(aes(x = Dis, y = p)) +
  geom_line(aes(colour = PCS8, group = PCS8)) +
  geom_point(aes(colour = PCS8)) +
  facet_wrap(~Genre)

load("Data/TRJ.rds")
# head(TRJ)

# Qui a un trj disponible ?

nrow(filter(PER_ff, uid_PER %in% TRJ$uid_PER)) / nrow(PER_ff)

# Calculons un temps de marche alternatif
# Sachant que pour une vitesse de marche moyenne de 5 km/h, il faut 0,012
# minutes pour parcourir un mètre

mchParPer = TRJ |>
  mutate(tempsMarche = O_Mch + D_Mch) |>
  group_by(uid_PER) |> summarise(tempsMarche = sum(tempsMarche))
PER_ff = left_join(PER_ff, mchParPer, by = "uid_PER")
PER_ff = mutate(PER_ff, tempsMarche = tempsMarche + Tps_MAR)

# Maintenant, on peut jouer

sum(filter(PER_ff,!is.na(CoeffRecEnqSansEMP) & tempsMarche > 0)$CoeffRecEnqSansEMP) / sum(filter(PER_ff, !is.na(CoeffRecEnqSansEMP) & !is.na(tempsMarche))$CoeffRecEnqSansEMP)

PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP), !is.na(tempsMarche)) |>
  mutate(Marche = tempsMarche > 0) |>
  group_by(Genre, Marche) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(Genre) |>
  mutate(p = nPop / sum(nPop))

PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP), !is.na(tempsMarche)) |>
  mutate(Marche = tempsMarche > 0) |>
  group_by(PCS8, Marche) |>
  summarise(nPop = sum(CoeffRecEnqSansEMP)) |>
  group_by(PCS8) |>
  mutate(p = nPop / sum(nPop)) |>
  filter(Marche)


sum(filter(PER_ff,!is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VEL > 0)$CoeffRecEnq) / sum(filter(PER_ff, Tps_VEL > 0 & !is.na(CoeffRecEnq) & !is.na(tempsMarche))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_DRM > 0)$CoeffRecEnq) / sum(filter(PER_ff, Tps_DRM > 0 & !is.na(CoeffRecEnq) & !is.na(tempsMarche))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VOI > 0)$CoeffRecEnq) / sum(filter(PER_ff, Tps_VOI > 0 & !is.na(CoeffRecEnq) & !is.na(tempsMarche))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_BUS > 0)$CoeffRecEnq) / sum(filter(PER_ff, Tps_BUS > 0 & !is.na(CoeffRecEnq) & !is.na(tempsMarche))$CoeffRecEnq)
sum(filter(PER_ff,!is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_TRN > 0)$CoeffRecEnq) / sum(filter(PER_ff, Tps_TRN > 0 & !is.na(CoeffRecEnq) & !is.na(tempsMarche))$CoeffRecEnq)

weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_MAR == Tps)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_MAR == Tps)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_BUS > 0)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_BUS > 0)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_TRN > 0)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_TRN > 0)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VOI > 0)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VOI > 0)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_DRM > 0)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_DRM > 0)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VEL > 0)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VEL > 0)$CoeffRecEnq)

weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VOI == Tps)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VOI == Tps)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_DRM == Tps)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_DRM == Tps)$CoeffRecEnq)
weighted.mean(x = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VEL == Tps)$tempsMarche, w = filter(PER_ff, !is.na(CoeffRecEnq) & tempsMarche > 0 & Tps_VEL == Tps)$CoeffRecEnq)

# Temps moyen avant et après chaque mode

tab = TRJ |>
  left_join(select(PER_ff, CoeffRecEnq, uid_PER), by = "uid_PER") |>
  mutate(tempsMarche = D_Mch + O_Mch, Mode = etqMode(Mode)) |>
  filter(!is.na(tempsMarche), !is.na(CoeffRecEnq))

g = tab |>
  group_by(Mode) |> summarise(tpsMarMoyen = weighted.mean(x = tempsMarche, y = CoeffRecEnq), n = n()) |>
  filter(n > 1000) |>
  tab_Tri(parCol = "tpsMarMoyen") |>
  ggplot(aes(x = Mode, y = tpsMarMoyen)) + geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Temps de marche moyen avant et\naprès l'usage d'un mode de transport",
       subtitle = "Base Trajets", caption =src_fig(tab)) +
  xlab("Principaux modes de transport (> 1000 occurrences)") +
  ylab("Temps de marche moyen avant et après (min.)")

sortie("Modes/Marche Avant Après transports")
print(g)
off()

TRJ |>
  left_join(select(PER_ff, CoeffRecEnq, uid_PER), by = "uid_PER") |>
  mutate(tempsMarche = D_Mch + O_Mch, Mode = etqMode(Mode, simple=T)) |>
  filter(!is.na(tempsMarche), !is.na(CoeffRecEnq)) |>
  group_by(Mode) |> summarise(tpsMarMoyen = weighted.mean(x = tempsMarche, y = CoeffRecEnq), n = n()) |>
  tab_Tri(parCol = "tpsMarMoyen") |>
  ggplot(aes(x = Mode, y = tpsMarMoyen)) + geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Trajets en cours de journée ?
# 2 façons : soit regarder le recours modal en fonction de l'heure,
# soit regarder le recours modal en fonction lieu d'activité...
# Ou bien : rechercher des séquences à pied ?

# Bien sûr autre possibilité = comaprer motifs destination des déplacements mostly à pied

tabGen = DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, CoeffRecEnq, uid_PER), by = "uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(ModeP = etqMode(ModeP, simple = T),
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-")) |>
  group_by(Motif) |>
  summarise(nb = sum(CoeffRecEnq)) |>
  ungroup() |>
  mutate(p = nb/sum(nb) * 100) |>
  mutate(Motif = ifelse(p < 2, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  group_by(Motif) |>
  summarise(p = sum(p))

tab = DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, CoeffRecEnq, uid_PER), by = "uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(ModeP = etqMode(ModeP, simple = T),
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-")) |>
  group_by(ModeP, Motif) |>
  summarise(nb = sum(CoeffRecEnq)) |>
  filter(!is.na(ModeP) & ModeP != "autres") |>
  group_by(ModeP) |>
  mutate(p = nb/sum(nb) * 100) |>
  mutate(Motif = ifelse(!Motif %in% tabGen$Motif, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  group_by(ModeP, Motif) |>
  summarise(p = sum(p), nb = sum(nb))

# tabModes = tab |> group_by(ModeP) |> summarise(nbMode = sum(nb))
# tab = left_join(tab, tabModes, by = "ModeP")

gBarres = ggplot(tab, aes(x = Motif, y = p)) +
  geom_col() +
  facet_wrap(~ModeP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

nivMotifs = c("Domicile-Accompagnement",
              "Domicile-Achats en petit commerce",
              "Domicile-Achats en supermarché",
              "Domicile-Loisirs sportifs, culturels ou assoc.",
              "Domicile-Visite d'un·e proche",
              "Domicile-Travail",
              "Travail-Travail",
              "Restaurant ou Bar-Travail",
              "Travail-Achats en petit commerce",
              "Travail-Accompagnement",
              "Autres")

tab = tab |>
  mutate(Motif = factor(as.character(Motif),
                        levels = nivMotifs))

# Il faut réordonner...

nivModes = levels(tab$ModeP)
nivModes = nivModes[nivModes %in% tab$ModeP]

# Motif = factor(Motif, levels = nivMotifs)) |>
tab2 = tibble(ModeP = lapply(nivModes, rep, times = length(levels(tab$Motif))) |> unlist(),
              Motif = rep(levels(tab$Motif), times = length(nivModes))) |>
  mutate(ModeP = factor(ModeP, levels = nivModes)) |> 
  left_join(tab, by = c("ModeP", "Motif"))

echelleMotifs = scale_fill_manual(breaks = rev(nivMotifs),
                                  values = rev(c("violetred",
                                                 "violet",
                                                 "pink",
                                                 "salmon",
                                                 "indianred",
                                                 "lightblue",
                                                 "lightblue3",
                                                 "skyblue3",
                                                 "purple3",
                                                 "slateblue",
                                                 "lightgrey")))

gAbs = ggplot(tab2, aes(x = ModeP, y = nb)) +
  geom_col(aes(fill = Motif, group = ModeP), position = "stack") +
  scale_y_continuous(labels = scales::label_number()) +
  xlab("Mode principal du déplacement") +
  ylab("Nb de déplacements\n(pondéré)") +
  echelleMotifs

gRel = ggplot(tab2, aes(x = ModeP, y = p)) +
  geom_col(aes(fill = Motif, group = ModeP), position = "stack") +
  xlab("Mode principal du déplacement") +
  ylab("Part des déplacements (%)\npar mode") +
  echelleMotifs

g = cowplot::plot_grid(gAbs + theme(legend.position = "none",
                                    axis.title.x = element_blank()) +
                         labs(title = ml("Motifs du déplacement au départ et à l'arrivée",
                                         "selon le mode de transport"),
                              subtitle = "En valeur absolue"),
                       gRel + labs(subtitle = "En valeur relative",
                                   caption = src_fig(emp = F)),
                       ncol = 1, labels = c("A", "B"), align = "v",
                       rel_heights=c(3, 7))

sortie("Modes/Motifs déplacements selon mode")
print(g)
off()

# nb déps en voiture domtrav / total
sum(filter(tab2, ModeP == "voiture" & Motif == "Domicile-Travail")$nb) / sum(tab2$nb, na.rm=T)

# Et si on recommençait, en prenant les 25% de secteurs les moins denses et les 25% les plus denses ?

tabDs = DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, CoeffRecEnqSansEMP, dsDom, dsTvl, uid_PER), by = "uid_PER") |>
  mutate(ds = case_when(dsDom < medDs & dsTvl < medDsTv ~ "Résid. peu dense, Travail peu dense",
                        dsDom > medDs & dsTvl > medDsTv ~ "Résid. dense, Travail dense",
                        dsDom < medDs & dsTvl > medDsTv ~ "Résid. peu dense, Travail dense",
                        dsDom > medDs & dsTvl < medDsTv ~ "Résid. dense, Travail peu dense")) |>
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  mutate(ModeP = etqMode(ModeP, simple = T),
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-")) |>
  group_by(ds, ModeP, Motif) |>
  summarise(nb = sum(CoeffRecEnqSansEMP)) |>
  filter(!is.na(ModeP) & ModeP != "autres") |>
  group_by(ds, ModeP) |>
  mutate(p = nb/sum(nb) * 100) |>
  mutate(Motif = ifelse(!Motif %in% tabGen$Motif, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  group_by(ds, ModeP, Motif) |>
  summarise(p = sum(p), nb = sum(nb))

# Motif = factor(Motif, levels = nivMotifs)) |>
tab2 = tibble(ModeP = lapply(nivModes, rep, times = length(levels(tab$Motif))) |> unlist(),
              Motif = rep(levels(tab$Motif), times = length(nivModes)))

tab2_1 = mutate(tab2, ds = unique(tabDs$ds)[1])
tab2_2 = mutate(tab2, ds = unique(tabDs$ds)[2])
tab2_3 = mutate(tab2, ds = unique(tabDs$ds)[3])
tab2_4 = mutate(tab2, ds = unique(tabDs$ds)[4])
tab2 = rbind(tab2_1, tab2_2, tab2_3, tab2_4)

tab2 = tab2 |> mutate(ModeP = factor(ModeP, levels = nivModes)) |> 
  left_join(tabDs, by = c("ds", "ModeP", "Motif"))


gRelDs = ggplot(tab2, aes(x = ModeP, y = p)) +
  geom_col(aes(fill = Motif, group = ModeP), position = "stack") +
  xlab("Mode principal du déplacement") +
  ylab("Part des déplacements (%)\npar mode") +
  echelleMotifs +
  facet_wrap(~ds) +
  labs(title = ml("Motif du déplacement au départ et à l'arrivée",
                  "selon le mode et la densité des secteurs de résidence et d'emploi"),
       caption = src_fig(emp = F))

sortie("Modes/Motifs déplacements selon mode et ds")
print(gRelDs + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
off()

tabGr = DEP |>
  filter(uid_PER %in% PER_ff$uid_PER) |>
  left_join(select(PER_ff, CoeffRecEnq, Genre, uid_PER), by = "uid_PER") |>
  filter(!is.na(CoeffRecEnq)) |>
  mutate(ModeP = etqMode(ModeP, simple = T),
         O_Motif = etqMotifActiv(O_Motif),
         D_Motif = etqMotifActiv(D_Motif)) |>
  rowwise() |>
  mutate(Motif = paste(sort(c(O_Motif, D_Motif)), collapse = "-")) |>
  group_by(Genre, ModeP, Motif) |>
  summarise(nb = sum(CoeffRecEnq)) |>
  filter(!is.na(ModeP) & ModeP != "autres") |>
  group_by(Genre, ModeP) |>
  mutate(p = nb/sum(nb) * 100) |>
  mutate(Motif = ifelse(!Motif %in% tabGen$Motif, "Autres", as.character(Motif)),
         Motif = as.factor(Motif)) |>
  group_by(Genre, ModeP, Motif) |>
  summarise(p = sum(p), nb = sum(nb))

# Motif = factor(Motif, levels = nivMotifs)) |>
tab2 = tibble(ModeP = lapply(nivModes, rep, times = length(levels(tab$Motif))) |> unlist(),
              Motif = rep(levels(tab$Motif), times = length(nivModes)))

tab2_1 = mutate(tab2, Genre = "H")
tab2_2 = mutate(tab2, Genre = "F")
tab2 = rbind(tab2_1, tab2_2)

tab2 = tab2 |> mutate(ModeP = factor(ModeP, levels = nivModes)) |> 
  left_join(tabGr, by = c("Genre", "ModeP", "Motif"))


gRelGr = ggplot(tab2, aes(x = ModeP, y = p)) +
  geom_col(aes(fill = Motif, group = ModeP), position = "stack") +
  xlab("Mode principal du déplacement") +
  ylab("Part des déplacements (%)\npar mode") +
  echelleMotifs +
  facet_wrap(~Genre) +
  labs(title = ml("Motif du déplacement au départ et à l'arrivée",
                  "selon le mode et le genre des travailleur⋅ses"),
       caption = src_fig(PER_ff))

sortie("Modes/Motifs déplacements selon mode et genre")
print(gRelGr + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
off()

gMdMar = PER_ff |>
  mutate(Dis10 = Dis/10000) |>
  valref() |>
  logit(       val = "modes_marche",
               formule = "Activ + Genre + PCS8 + dsDomEtq + Dis10 + Age10",
               titre = "Recours à la marche",
               valIntervalleSur100 = 55, returnFig = T,
               caption = src_fig(emp=T), petit=T)
g2 = PER_ff |>
  mutate(Dis10 = Dis/10000) |>
  valref() |>
  logit(       val = "modes_marche",
               formule = "Activ + Genre + PCS8 + dsTvlEtq + Dis + Age10",
               titre = "Recours à la marche",
               valIntervalleSur100 = 55, returnFig = T)


gMdMarAdapt = PER_ff |>
  mutate(sorties = ifelse(DuSvc > 0 | DuLsr > 0 | DuCom > 0, "TRUE", "FALSE"),
         sorties = as.factor(sorties),
         tc = ifelse(Tps_BUS > 0 | Tps_TRN > 0, "TRUE", "FALSE"),
         tc = as.factor(tc)) |>
  mutate(Dis = log(Dis)) |> valref() |>
  logit(       val = "modes_marche",
               formule = "Genre + PCS8 + dsDomEtq + Dis + sorties + tc",
               titre = "Recours à la marche",
               valIntervalleSur100 = 30, returnFig = T, petit = T,
               caption = src_fig(emp=F))

captionG = ggdraw() + draw_label(label = src_fig(emp=F), size = 8, x = .99, hjust = 1)

g = cowplot::plot_grid(gMdMar + labs(title="Modèle générique", caption=NULL) + theme(legend.position = "none"),
                       gMdMarAdapt + labs(title="Modèle adapté", caption=NULL) + theme(legend.position = "none"),
                       cowplot::get_legend(gMdMarAdapt + theme(legend.direction = "horizontal")),
                       captionG,
                       nrow = 2, labels = c("A", "B"), rel_heights = c(9, 1)) |>
  viz_Titre("Modèles logit estimant la probabilité d'un recours\nà la marche au cours de la journée")

sortie("Modes/Mod marche")
print(g)
off()
# tab |>
#   select(ModeP, Motif, nb) |>
#   pivot_wider(names_from = "Motif", values_from = "nb") |>
#   hist()


# Cartes ====

# Carte nationale du recours au TC
# - par département (EMP comprise)
# - par AAV
# - au sein de 4 AAV de différentes classes discrétisées

source("START.R") ; gc()
initMémoire(BasesCharger = "PER")

rapport("Carte du recours modal, France")
PER_ff = init_PER_ff(PER)

load("Data/shp_COM.rds") ; load("Data/shp_ZF.rds")
tab_aav = centroidesAAV()
remove(shp_COM, shp_ZF)
load("Data/fdCarte.rds")

dep = PER_ff |>
  pivot_wider(names_from = "modes_tc", names_prefix = "modes_tc_", values_from = "CoeffRecEnq") |>
  select(uid_PER, Com, modes_tc_non, modes_tc_oui) |>
  mutate(dep = ifelse(substr(Com, 1, 2) != "97", substr(Com, 1, 2), substr(Com, 1, 3))) |>
  group_by(dep) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pTc = modes_tc_oui / (modes_tc_non + modes_tc_oui))

dep$pTc = discretisation(dep$pTc*100, verb = T)

g1 = viz_France(base = dep, champRel = "pTc", echelleRel = scale_fill_brewer(palette = "Oranges", na.value = "ghostwhite",
                                                                            name = "Part de\ntravailleur⋅ses\nutilisant les\nTC (%)"))
aav = PER_ff |>
  pivot_wider(names_from = "modes_tc", names_prefix = "modes_tc_", values_from = "CoeffRecEnq") |>
  select(uid_PER, ZF, modes_tc_non, modes_tc_oui) |>
  left_join(select(tab_aav, ZF, AAV2020), by="ZF") |>
  group_by(AAV2020) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pTc = modes_tc_oui / (modes_tc_non + modes_tc_oui))

aav$pTc = discretisation(aav$pTc*100, verb = T)

g2 = viz_France(base = aav, methode = "aav", champRel = "pTc",
               echelleRel = scale_fill_brewer(palette = "YlOrBr",
                                              na.value = "ghostwhite",
                                              name = "Part de\ntravailleur⋅ses\nutilisant les\nTC (%)"))

gg = plot_grid(g1 + labs(subtitle = "Par département"),
               g2 + labs(subtitle = "Par aire d'attraction des villes (2020)"),
               nrow = 2)

gg = viz_Titre(gg, "Recours aux T.C. parmi les travailleur⋅ses en France\n(durant le jour d'enquête)")

sortie("Modes/Carte France TC", taille = "page", portrait = T)
  print(gg)
off()

rapport("Carte du recours au vélo, France")
PER_ff = init_PER_ff(PER)

dep = PER_ff |>
  pivot_wider(names_from = "modes_vélo", names_prefix = "modes_vélo_", values_from = "CoeffRecEnq") |>
  select(uid_PER, Com, modes_vélo_non, modes_vélo_oui) |>
  mutate(dep = ifelse(substr(Com, 1, 2) != "97", substr(Com, 1, 2), substr(Com, 1, 3))) |>
  group_by(dep) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pVel = modes_vélo_oui / (modes_vélo_non + modes_vélo_oui))

dep$pVel = discretisation(dep$pVel*100, verb = T)

g1 = viz_France(base = dep, champRel = "pVel", echelleRel = scale_fill_brewer(palette = "BuGn", na.value = "ghostwhite",
                                                                             name = "Part de\ntravailleur⋅ses\nutilisant\nle vélo (%)"))
aav = PER_ff |>
  pivot_wider(names_from = "modes_vélo", names_prefix = "modes_vélo_", values_from = "CoeffRecEnq") |>
  select(uid_PER, ZF, modes_vélo_non, modes_vélo_oui) |>
  left_join(select(tab_aav, ZF, AAV2020), by="ZF") |>
  group_by(AAV2020) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pVel = modes_vélo_oui / (modes_vélo_non + modes_vélo_oui))

aav$pVel = discretisation(aav$pVel*100, verb = T)

g2 = viz_France(base = aav, methode = "aav", champRel = "pVel",
                echelleRel = scale_fill_brewer(palette = "BuGn",
                                               na.value = "ghostwhite",
                                               name = "Part de\ntravailleur⋅ses\nutilisant\nle vélo (%)"))

gg = plot_grid(g1 + labs(subtitle = "Par département"),
               g2 + labs(subtitle = "Par aire d'attraction des villes (2020)"),
               nrow = 2)

gg = viz_Titre(gg, "Recours au vélo parmi les travailleur⋅ses en France\n(durant le jour d'enquête)")

sortie("Modes/Carte France Vélo", taille = "page", portrait = T)
print(gg)
off()

rapport("Carte du recours à l'auto, France")
PER_ff = init_PER_ff(PER)

dep = PER_ff |>
  pivot_wider(names_from = "modes_voiture", names_prefix = "modes_voiture_", values_from = "CoeffRecEnq") |>
  select(uid_PER, Com, modes_voiture_non, modes_voiture_oui) |>
  mutate(dep = ifelse(substr(Com, 1, 2) != "97", substr(Com, 1, 2), substr(Com, 1, 3))) |>
  group_by(dep) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pVoi = modes_voiture_oui / (modes_voiture_non + modes_voiture_oui))

dep$pVoi = discretisation(dep$pVoi*100, verb = T)

g1 = viz_France(base = dep, champRel = "pVoi", echelleRel = scale_fill_brewer(palette = "BuPu", na.value = "ghostwhite",
                                                                              name = "Part de\ntravailleur⋅ses\nutilisant\nla voiture (%)"))
aav = PER_ff |>
  pivot_wider(names_from = "modes_voiture", names_prefix = "modes_voiture_", values_from = "CoeffRecEnq") |>
  select(uid_PER, ZF, modes_voiture_non, modes_voiture_oui) |>
  left_join(select(tab_aav, ZF, AAV2020), by="ZF") |>
  group_by(AAV2020) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)), n = n()) |>
  filter(n > 100) |>
  mutate(pVoi = modes_voiture_oui / (modes_voiture_non + modes_voiture_oui))

aav$pVoi = discretisation(aav$pVoi*100, verb = T)

g2 = viz_France(base = aav, methode = "aav", champRel = "pVoi",
                echelleRel = scale_fill_brewer(palette = "Purples",
                                               na.value = "ghostwhite",
                                               name = "Part de\ntravailleur⋅ses\nutilisant\nle vélo (%)"))

gg = plot_grid(g1 + labs(subtitle = "Par département"),
               g2 + labs(subtitle = "Par aire d'attraction des villes (2020)"),
               nrow = 2)

gg = viz_Titre(gg, "Recours à la voiture parmi les travailleur⋅ses en France\n(durant le jour d'enquête)")

sortie("Modes/Carte France Voiture", taille = "page", portrait = T)
print(gg)
off()
