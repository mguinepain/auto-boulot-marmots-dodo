# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                                                                 #
#     E N Q U Ê T E S   M É N A G E S - D E P L A C E M E N T S                                   #
#                                                                                                 #
#     SCRIPTS DE TRAVAIL M. GUINEPAIN                                                             #
#     ANALYSES POUR LE CHAPITRE 4 : 24H POUR TOU.TES ?                                            #
#                                                                                                 #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #

# setwd("/home/maxime/Données/EMD")
# Pour exécuter ce script lui-même : > source("Scripts/analyses_temps.R", print.eval=T)

rm(list = ls()) ; gc()
source("START.R", print.eval=T)

initMémoire(BasesCharger = c("PER", "ACT"))

PER = densitesZversPER(PER)
PER_ff = init_PER_ff(PER)

# Répertoire des figures : "Temps"
if (!dir.exists("Sorties/Temps")) { dir.create("Sorties/Temps") }

# Temps hors domicile ====

rapport("Analyses temporelles", prim = T)

subPER = filter(valref(PER), uid_ENQ == "EMP2019")

# ~ Tris à plat ====

sortie("Temps/Temps hors dom gen", taille = "man", h = 11, l = 17)
filter(PER, uid_ENQ == "EMP2019") %>%
  group_by(Age5) %>%
  filter(!is.na(CoeffRecEnq)) %>%
  summarise(Tps_horsDom = 24 - weighted.mean(DuDom, CoeffRecEnq, na.rm=T)/60,
            Tps_Tvl =          weighted.mean(DuTvl + DuEtu, CoeffRecEnq, na.rm=T)/60,
            TpsHorsDomQ1 = 24 - (weighted.quantile(DuDom, w=CoeffRecEnq, na.rm = T, probs=.75)/60),
            TpsHorsDomQ3 = 24 - (weighted.quantile(DuDom, w=CoeffRecEnq, na.rm = T, probs=.25)/60)) %>%
  pivot_longer(cols = starts_with("Tps_"), names_to = "variable", values_to = "y") %>%
  ggplot(aes(x = Age5, y = y)) +
  geom_ribbon(aes(ymin = TpsHorsDomQ1, ymax=TpsHorsDomQ3,
                  fill=ml("Temps passé", "hors domicile","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype=variable, group=variable)) +
  labs(title = "Temps moyen passé hors du domicile par les enquêté·es de l'EMP", caption=src_fig(base=subPER)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  scale_linetype(name = "Temps moyen passé", labels = c("hors domicile",
                                                        ml("sur lieu de travail", "ou d'études"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Classe d'âge") + ylab("temps (en heures)")
off()

sortie("Temps/Temps hors dom jour de la semaine")
subPER %>%
  filter(!is.na(EnqDate_JS), Activ %in% c("10", "11", "12")) %>%
  group_by(EnqDate_JS) %>%
  summarise(p = sum(ifelse(DuTvl>0, CoeffEnq, 0), na.rm=T) / sum(ifelse(DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100,
            n = sum(ifelse(DuTvl>0, CoeffEnq, 0), na.rm=T)) %>%
  ggplot(aes(x = EnqDate_JS, y = p)) + geom_col() +
  labs(title="Part des enquêté·es s'étant déplacé·es pour travailler\nselon le jour de la semaine",
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("part (%)")
off()

sortie("Temps/Temps hors dom jour de la semaine et genre")
subPER %>%
  filter(!is.na(EnqDate_JS), Activ %in% c("10", "11", "12")) %>%
  group_by(Genre, EnqDate_JS) %>%
  summarise(p = sum(ifelse(DuTvl>0, CoeffEnq, 0), na.rm=T) / sum(ifelse(DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100) %>%
  ggplot(aes(x = EnqDate_JS, y = p)) +
  geom_col(aes(fill = Genre), width=.5, position="dodge") +
  labs(title="Part des enquêté·es en emploi s'étant déplacé·es pour travailler\nselon le jour de la semaine",
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("part (%)") +
  scale_fill_manual(values = c("coral", "thistle"), breaks = c("F", "H"), labels=c("Femmes", "Hommes"))
off()

sortie("Distances/Distance selon jour semaine, actif⋅ves")
subPER %>%
  filter(!is.na(EnqDate_JS), Activ %in% c("10", "11", "12")) %>%
  group_by(Genre, EnqDate_JS) %>%
  summarise(disMoy = weighted.mean(Dis, CoeffEnq, na.rm=T)/1000, n=n()) %>%
  ggplot(aes(x = EnqDate_JS, y = disMoy)) +
  geom_col(aes(fill = Genre), width=.5, position="dodge") +
  labs(title="Distance moyenne parcourue par les enquêté·es en emploi s'étant déplacé·es pour travailler\nselon le jour de la semaine",
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("distance (km)") +
  scale_fill_manual(values = c("coral", "thistle"), breaks = c("F", "H"), labels=c("Femmes", "Hommes"))
off()

sortie("Temps/Temps en déplacement selon jour semaine, actif⋅ves")
subPER %>%
  filter(!is.na(EnqDate_JS), Activ %in% c("10", "11", "12")) %>%
  group_by(Genre, EnqDate_JS) %>%
  summarise(tpsMoy = weighted.mean(Tps, CoeffEnq, na.rm=T)/60, n=n()) %>%
  ggplot(aes(x = EnqDate_JS, y = tpsMoy)) +
  geom_col(aes(fill = Genre), width=.5, position="dodge") +
  labs(title=ml("Temps en déplacement moyen pour les enquêté·es en emploi s'étant déplacé·es pour travailler",
                "selon le jour de la semaine"),
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("temps en déplacement (heures)") +
  scale_fill_manual(values = c("coral", "thistle"), breaks = c("F", "H"), labels=c("Femmes", "Hommes"))
off()

sortie("Temps/Temps sur lieu d'emploi selon jour de la semaine")
subPER %>%
  filter(!is.na(EnqDate_JS), Activ %in% c("10", "11", "12")) %>%
  group_by(PCS42, EnqDate_JS) %>%
  filter(DuTvl > 0) %>%
  summarise(DuTvl = weighted.median(DuTvl, CoeffEnq, na.rm=T)/60, n=n()) %>%
  filter(n > 5) %>%
  ggplot(aes(x = EnqDate_JS, y = DuTvl)) +
  geom_col(width=.5) +
  labs(title="Temps médian passé sur le lieu de travail\nselon le jour de la semaine",
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("distance (km)") +
  facet_wrap(~PCS42)
off()

sortie("Temps/Travail samedi et dimanche, selon PCS42S")
subPER %>%
  filter(EnqDate_JS %in% c("sam", "dim")) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, rev = T)) %>%
  group_by(PCS42S) %>%
  summarise(samedi = sum(ifelse(EnqDate_JS == "sam" & DuTvl>0, CoeffEnq, 0), na.rm=T) /
              sum(ifelse(EnqDate_JS == "sam" & DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100,
            dimanche = sum(ifelse(EnqDate_JS == "dim" & DuTvl>0, CoeffEnq, 0), na.rm=T) /
              sum(ifelse(EnqDate_JS == "dim" & DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100,
            n = n()) %>%
  filter(!n < seuilSignifiant) %>%
  pivot_longer(cols = c("samedi", "dimanche"), values_to = "p", names_to = "jour") %>% 
  ggplot(aes(x = PCS42S, y = p)) + geom_col(aes(fill = jour), position="dodge") +
  coord_flip() +
  labs(title="Part des enquêté·es s'étant déplacé·es pour travailler\nselon le jour de la semaine",
       caption = src_fig(subPER)) + xlab("jour de la semaine") + ylab("part (%)") +
  scale_fill_manual(values=c("lightblue", "tan"), breaks = c("samedi", "dimanche"),
                    name="Jour d'enquête")
off()

subPER %>%
  filter(EnqDate_JS %in% c("sam", "dim")) %>%
  group_by(PCS42) %>%
  summarise(samedi = sum(ifelse(EnqDate_JS == "sam" & DuTvl>0, CoeffEnq, 0), na.rm=T) /
              sum(ifelse(EnqDate_JS == "sam" & DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100,
            dimanche = sum(ifelse(EnqDate_JS == "dim" & DuTvl>0, CoeffEnq, 0), na.rm=T) /
              sum(ifelse(EnqDate_JS == "dim" & DuTvl>=0, CoeffEnq, 0), na.rm=T) * 100,
            n = n()) %>%
  filter(!n < seuilSignifiant) %>%
  tab_Tri(i = "PCS42", parCol = "samedi", rev = T)

subPER %>%
  filter(EnqDate_JS == "sam") %>%
  group_by(PCS42) %>%
  summarise(DuTvl = weighted.mean(DuTvl, CoeffEnq, na.rm=T)) %>%
  tab_Tri(i = "PCS42", parCol = "DuTvl", rev = T)

cor.test(PER_ff$DuTvl, PER_ff$DuDom)
cor.test(filter(PER_ff, Genre == "F")$DuTvl, filter(PER_ff, Genre == "F")$DuDom)

cor.test(PER_ff$Tps, PER_ff$DuTvl, method = "spearman")

weighted.mean(filter(PER_ff, Genre == "F", !is.na(CoeffRecEnq))$DuTvl,
              w = filter(PER_ff, Genre == "F", !is.na(CoeffRecEnq))$CoeffRecEnq) /60
weighted.mean(filter(PER_ff, Genre == "H", !is.na(CoeffRecEnq))$DuTvl,
              w = filter(PER_ff,Genre == "H", !is.na(CoeffRecEnq))$CoeffRecEnq) /60

weighted.mean(filter(PER_ff, Activ == "11", Genre == "F", !is.na(CoeffRecEnq))$DuTvl,
              w = filter(PER_ff, Activ == "11", Genre == "F", !is.na(CoeffRecEnq))$CoeffRecEnq) /60
weighted.mean(filter(PER_ff, Activ == "11", Genre == "H", !is.na(CoeffRecEnq))$DuTvl,
              w = filter(PER_ff, Activ == "11", Genre == "H", !is.na(CoeffRecEnq))$CoeffRecEnq) /60

remove(subPER)

PER_ff |>
  group_by(PCS8) |>
  summarise(pearson = cor(DuTvl, DuDom))

g = PER_ff |>
  mutate(PCS42S = etqPCS42S(PCS42S)) |>
  group_by(PCS42S) |>
  summarise(pearson = cor(DuTvl, DuDom)) |> tab_Tri(parCol = "pearson") |>
  ggplot(aes(x = pearson, y = PCS42S)) +
  geom_col(aes(fill = "Coefficient\nde Pearson")) +
  xlab(NULL) + ylab("PCS détaillée") +
  labs(title="Corrélation entre présence au domicile\net présence au lieu de travail",
       caption=src_fig(filter(PER_ff, !is.na(PCS42S)))) +
  scale_fill_manual(values = "grey", name = NULL)

sortie("Temps/Corrél DuDom DuTvl", taille = "mini")
print(g)
off()

  # ~ Corrélations simples entre temps travail et domicile ====

sortie("Temps/Corrélations temps de travail et temps domicile", format = "pdf",
       taille = "a4", portrait = T)

PER_ff %>%
  group_by(uid_ENQ) %>%
  summarise(pearson = cor(DuTvl, DuDom, method = "pearson")) %>%
  tab_Tri("uid_ENQ", parCol = "pearson") %>%
  ggplot(aes(x = uid_ENQ, y = pearson)) + geom_col() + coord_flip()

PER_ff |>
  group_by(Genre) %>%
  summarise(pearson = cor(DuTvl, DuDom, method = "pearson")) %>%
  tab_Tri("Genre", parCol = "pearson")

PER_ff %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  group_by(PCS42S) %>%
  summarise(pearson = cor(DuTvl, DuDom, method = "pearson")) %>%
  tab_Tri("PCS42S", parCol = "pearson", rev=T)

PER %>%
  filter(DuTvl>0) %>%
  group_by(ZoneDens) %>%
  summarise(pearson = cor(DuTvl, DuDom, method = "pearson")) %>%
  tab_Tri("ZoneDens", parCol = "pearson", rev=T)

PER %>%
  filter(DuTvl>0) %>%
  group_by(ZoneDens, ZoneDens_travMax) %>%
  summarise(pearson = cor(DuTvl, DuDom, method = "pearson"), n = n()) %>%
  tab_Tri("ZoneDens", parCol = "pearson")

off()

filter(PER, DuTvl>0) %>% nrow()
table(filter(PER, DuTvl>0 & DuEtu>0)$Age5)

# Temps lieu d'emploi ====

PER_ff |>
  filter(!is.na(DuTvl), !is.na(CoeffRecEnq)) |>
  summarise(m = weighted.median(DuTvl, CoeffRecEnq) / 60)

# ~ Tests de corrélations ====

sortie("Temps/Temps lieu d'emploi selon Dis")
PER_ff %>%
  filter(!is.na(CoeffRecEnq)) |>
  mutate(Activ = etqActiv(Activ)) %>%
  mutate(DisFloor = floor(Dis/1000)) %>%
  mutate(DisFloor = round(DisFloor/5) * 5) |>
  filter(DisFloor<=166) %>%
  group_by(Activ, DisFloor) %>%
  summarise(DuTvlMed = weighted.median(DuTvl,  w = CoeffRecEnq)/60,
            DuTvlMoy = weighted.mean  (DuTvl,  w = CoeffRecEnq)/60,
            DuTvlQ1 = weighted.quantile(DuTvl, w=CoeffRecEnq, .25)/60,
            DuTvlQ3 = weighted.quantile(DuTvl, w=CoeffRecEnq, .75)/60,
            n = n()) %>%
  pivot_longer(cols = c("DuTvlMed", "DuTvlMoy"), names_to = "type", values_to = "x") %>%
  filter(n > 100) %>%
  ggplot(aes(x = DisFloor, y = x)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "hors domicile","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype = type)) +
  scale_linetype(labels = c("médiane","moyenne"), name = NULL) +
  labs(title = "Temps moyen passé sur le lieu d'emploi en fonction\nde la distance parcourue dans la journée", caption=src_fig(base=PER)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("distance parcourue dans la journée (estimation, en km)") +
  ylab("temps sur lieu d'emploi (en h)") +
  facet_wrap(~Activ)
off()

sortie("Temps/Temps au lieu d'emploi selon DuDep")
PER_ff %>%
  filter(!is.na(CoeffRecEnq)) |>
  mutate(Activ = etqActiv(Activ)) %>%
  mutate(DuDep = round(DuDep/5)*5) %>%
  filter(DuDep<=240) %>%
  group_by(Activ, DuDep) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w=CoeffRecEnq)/60,
            DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnq)/60,
            DuTvlQ1 = weighted.quantile(DuTvl, w=CoeffRecEnq, .25)/60,
            DuTvlQ3 = weighted.quantile(DuTvl, w=CoeffRecEnq, .75)/60,
            n = n()) %>%
  pivot_longer(cols = c("DuTvlMed", "DuTvlMoy"), names_to = "type", values_to = "x") %>%
  filter(n > 100) %>%
  ggplot(aes(x = DuDep, y = x)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "hors domicile","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype = type)) +
  scale_linetype(labels = c("médiane","moyenne"), name = NULL) +
  labs(title = "Temps moyen passé sur le lieu d'emploi en fonction\ndu temps passé en déplacement dans la journée",
       caption=src_fig(base=PER_ff)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("temps passé en déplacement (en mn)") +
  ylab("temps sur lieu d'emploi (en h)") +
  facet_wrap(~Activ)
off()

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0)
cor.test(base10$DuTvl, base10$Dis, method = "pearson")

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0)
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0 & Activ == "10")
cor.test(base10$DuTvl, base10$Dis)
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0 & Genre == "F")
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

base10 = filter(PER_ff, Activ == "11")
cor.test(base10$DuTvl, base10$Dis)
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0 & Activ == "10")
cor.test(base10$DuDep, base10$DuTvl)
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

base10 = filter(PER, DuTvl > 0 & DuEtu == 0 & DuDom > 0 & Activ == "11")
cor.test(base10$DuDep, base10$DuTvl)
cor.test(base10$DuTvl, base10$Dis, method = "spearman")

filter(PER_ff, Dis < 10000, !is.na(CoeffRecEnq), Activ == "11") |>
  group_by(PCS8) |> summarise(nb = sum(CoeffRecEnq)) |>
  mutate(p = nb / sum(nb) * 100)

filter(PER_ff, !is.na(CoeffRecEnq), Activ == "11") |>
  group_by(PCS8) |> summarise(nb = sum(CoeffRecEnq)) |>
  mutate(p = nb / sum(nb) * 100)

filter(PER_ff, !is.na(CoeffRecEnq), Activ == "11") |>
  group_by(Genre) |> summarise(nb = sum(CoeffRecEnq)) |>
  mutate(p = nb / sum(nb) * 100)

filter(PER_ff, Dis < 10000, !is.na(CoeffRecEnq)) |>
  group_by(PCS8) |> summarise(nb = sum(CoeffRecEnq)) |>
  mutate(p = nb / sum(nb) * 100)

sortie("Temps/Corrélation temps lieu d'emploi et distance selon emploi, densité",
       taille = "page", portrait=T)
PER_ff %>%
  mutate(Activ = etqActiv(Activ),
         dsDomQtl = discretisation(dsDom, methode = "quartiles"),
         dsDomQtl = factor(dsDomQtl, labels = paste0(c("1er", "2e", "3e", "4e"), " quartile\nde densité (dom.)"))) %>%
  filter(!is.na(dsDomQtl), !is.na(CoeffRecEnqSansEMP)) |>
  mutate(DisFloor = floor(Dis/1000)) %>%
  filter(DisFloor<=100) %>%
  group_by(Activ, dsDomQtl, DisFloor) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w=CoeffRecEnqSansEMP)/60,
            DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnqSansEMP)/60,
            DuTvlQ1 = weighted.quantile(DuTvl, w=CoeffRecEnqSansEMP, .25)/60,
            DuTvlQ3 = weighted.quantile(DuTvl, w=CoeffRecEnqSansEMP, .75)/60,
            n = n()) %>%
  pivot_longer(cols = c("DuTvlMed", "DuTvlMoy"), names_to = "type", values_to = "x") %>%
  filter(n > 50) %>%
  ggplot(aes(x = DisFloor, y = x)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "hors domicile","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype = type)) +
  scale_linetype(labels = c("médiane","moyenne"), name = NULL) +
  labs(title = "Temps moyen passé sur le lieu d'emploi en fonction\nde la distance parcourue dans la journée",
       caption=src_fig(emp=F)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("distance parcourue dans la journée (estimation, en km)") +
  ylab("temps médian sur lieu d'emploi (en h)") +
  facet_grid(dsDomQtl~Activ)
off()

sortie("Temps/Temps passé sur lieu d'emploi selon distance (10km max)")
PER_ff |>
  mutate(DisFloor = discretisation(Dis/1000)) %>%
  filter(!is.na(DisFloor)) |>
  group_by(DisFloor) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w=CoeffRecEnq)/60,
            DuTvlQ1 = weighted.quantile(DuTvl, w=CoeffRecEnq, .25)/60,
            DuTvlQ3 = weighted.quantile(DuTvl, w=CoeffRecEnq, .75)/60) %>%
  ggplot(aes(x = DisFloor, y = DuTvlMed)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "sur lieu d'emploi","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(group = 2, colour="Temps médian\npassé sur lieu\nd'emploi")) +
  labs(title = "Temps médian passé sur le lieu d'emploi en fonction\nde la distance parcourue dans la journée",
       caption=src_fig(base=PER)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  scale_colour_manual(values = "black", name = "Médiane") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("distance parcourue dans la journée (estimation, en km)") +
  ylab("temps médian sur lieu d'emploi (en h)")
off()

niveaux = c(paste0("de ", c(0:13), " à ", c(1:14), " h"))

base = PER_ff

base2 = base %>%
  filter(!is.na(CoeffRecEnq)) |>
  group_by(N) %>%
  summarise(DuTvlMed = weighted.median(DuTvl,  w=CoeffRecEnq)/60,
            DuTvlMoy = weighted.mean  (DuTvl,  w=CoeffRecEnq)/60,
            DuTvlQ1 =  weighted.quantile(DuTvl,w=CoeffRecEnq, .25)/60,
            DuTvlQ3 =  weighted.quantile(DuTvl,w=CoeffRecEnq, .75)/60,
            n = n()) %>%
  pivot_longer(cols = c("DuTvlMed", "DuTvlMoy"), names_to = "type", values_to = "x") %>%
  filter(n > 100)


g = ggplot(data = base2, aes(x = N, y = x)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "sur lieu d'emploi","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype = type)) +
  scale_linetype(labels = c("médiane","moyenne"), name = NULL) +
  labs(title = "Temps moyen passé sur le lieu d'emploi en fonction\ndu nombre de déplacements dans la journée",
       subtitle = paste0("Coefficient de Pearson : ",
                         round(cor(filter(base, !is.na(N))$N, filter(base, !is.na(N))$DuTvl),3)),
       caption=src_fig(base=PER)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("nombre de déplacements au sens du Cerema") +
  ylab("temps médian sur lieu d'emploi (en h)")

cor.test(filter(base, !is.na(N))$N, filter(base, !is.na(N))$DuTvl)
cor.test(filter(base, !is.na(N))$N, filter(base, !is.na(N))$DuTvl, method = "spearman")

ggplot(data = base, aes(x = N, y = DuTvl/60)) +
  geom_bin2d(binwidth=c(1,1)) +
  scale_fill_gradient(low = "gainsboro", high="brown") +
  facet_wrap(~Genre)

sortie("Temps/Temps passé sur lieu d'emploi selon nb déplacements")
print(g)
off()

base2 = base %>%
  filter(!is.na(CoeffRecEnq)) |>
  mutate(plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  group_by(plusLieuxTravail, N) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w=CoeffRecEnq)/60,
            DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnq)/60,
            DuTvlQ1 =  weighted.quantile(DuTvl, w=CoeffRecEnq, .25)/60,
            DuTvlQ3 = weighted.quantile(DuTvl, w=CoeffRecEnq, .75)/60,
            n = n()) %>%
  pivot_longer(cols = c("DuTvlMed", "DuTvlMoy"), names_to = "type", values_to = "x") %>%
  filter(n > 100)

g = ggplot(data = base2, aes(x = N, y = x)) +
  geom_ribbon(aes(ymin = DuTvlQ1, ymax=DuTvlQ3,
                  fill=ml("Temps passé", "hors domicile","par 50%", "de la pop."), group=1), alpha=.25) +
  geom_line(aes(linetype = type)) +
  scale_linetype(labels = c("médiane","moyenne"), name = NULL) +
  labs(title = "Temps moyen passé sur le lieu d'emploi en fonction\nde la distance parcourue dans la journée",
       caption=src_fig(base=PER)) +
  scale_fill_manual(values = "grey", name = "Écart interquartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("nombre de déplacements au sens du Cerema") +
  ylab("temps médian sur lieu d'emploi (en h)") + facet_wrap(~plusLieuxTravail)

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  group_by(PCS42S = etqPCS42S(PCS42S), ZoneDens_travMax) %>% summarise(m = heureMinToHr(mean(DuTvl), sec=F)) %>% View()

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  filter(!PCS42S %in% c("00", as.character(81:99))) %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>% summarise(m = heureMinToHr(mean(DuTvl), sec=F))

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  group_by(PCS42S = etqPCS42S(PCS42S), ZoneDens) %>% summarise(m = heureMinToHr(mean(DuTvl), sec=F)) %>% View()

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  group_by(PCS8 = etqPCS8(PCS8), ZoneDens_travMax) %>% summarise(m = mean(nbLxTvl)) %>% View()

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  group_by(PCS42S = etqPCS42S(PCS42S), ZoneDens_travMax) %>% summarise(m = mean(nbLxTvl)) %>% View()

#TODO: mettre à jour calcul de la densité
tabPCS8 = PER_ff %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(ZoneDens         = paste0("Com. de résidence\n", etqZoneDens(ZoneDens, supprTrFaible = T)),
         ZoneDens_travMax = paste0("Com. de travail\n",   etqZoneDens(ZoneDens_travMax, supprTrFaible = T))) %>%
  mutate(ZoneDens = factor(ZoneDens, levels = paste0("Com. de résidence\n", niv_ZoneDens[1:3])),
         ZoneDens_travMax = factor(ZoneDens_travMax, levels = paste0("Com. de travail\n", niv_ZoneDens[1:3]))) %>%
  group_by(ZoneDens, ZoneDens_travMax, PCS8) %>%
  summarise(DuTvl = mean(DuTvl)/60) %>% rename(PCS = PCS8)

tabPCS42S = PER %>%
  filter(DuTvl > 0, Activ %in% c("10", "11"), PCS8 %in% c("01", "02", "03", "04", "05", "06"),
         !is.na(ZoneDens_travMax)) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  mutate(ZoneDens         = paste0("Com. de résidence\n", etqZoneDens(ZoneDens, supprTrFaible = T)),
         ZoneDens_travMax = paste0("Com. de travail\n",   etqZoneDens(ZoneDens_travMax, supprTrFaible = T))) %>%
  mutate(ZoneDens = factor(ZoneDens, levels = paste0("Com. de résidence\n", niv_ZoneDens[1:3])),
         ZoneDens_travMax = factor(ZoneDens_travMax, levels = paste0("Com. de travail\n", niv_ZoneDens[1:3]))) %>%
  group_by(ZoneDens, ZoneDens_travMax, PCS42S) %>%
  summarise(DuTvl = mean(DuTvl)/60)


g1 = PER_ff %>%
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  mutate(dsDomQtl = discretisation(dsDom, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile ds. dom.")),
         dsTvlQtl = discretisation(dsTvl, methode = "quartiles") |>
           factor(labels = paste0(c("1er", "2e", "3e", "4e"), " quartile ds. tvl."))) |>
  filter(!is.na(dsDomQtl), !is.na(dsTvlQtl)) |>
  group_by(dsDomQtl, dsTvlQtl, PCS8) |> summarise(DuTvl = weighted.mean(DuTvl/60, w = CoeffRecEnqSansEMP)) |>
  ggplot(aes(x = PCS8, y = DuTvl)) + geom_col(aes(fill = PCS8)) +
  scale_fill_manual(values = pal_PCS8) +
  facet_grid(dsTvlQtl~dsDomQtl) +
  ggRetirerAxeX + ylab("heures passées sur le lieu de travail") +
  labs(title = "Heures passées sur le lieu de travail")

# g2 = PER %>%
#   filter(DuTvl > 0, Activ %in% c("10", "11"), PCS8 %in% c("01", "02", "03", "04", "05", "06"),
#          !is.na(ZoneDens_travMax)) %>%
#   mutate(ZoneDens         = paste0("Com. de résidence\n", etqZoneDens(ZoneDens, supprTrFaible = T)),
#          ZoneDens_travMax = paste0("Com. de travail\n",   etqZoneDens(ZoneDens_travMax, supprTrFaible = T))) %>%
#   mutate(ZoneDens = factor(ZoneDens, levels = paste0("Com. de résidence\n", niv_ZoneDens[1:3])),
#          ZoneDens_travMax = factor(ZoneDens_travMax, levels = paste0("Com. de travail\n", niv_ZoneDens[1:3]))) %>%
#   group_by(ZoneDens, ZoneDens_travMax, PCS8) %>%
#   summarise(nbLxTvl = mean(nbLxTvl)) %>%
#   ggplot(aes(x = PCS8, y = nbLxTvl)) + geom_col(aes(fill = PCS8)) +
#   scale_fill_manual(values = pal_PCS8) +
#   facet_grid(ZoneDens_travMax~ZoneDens) +
#   ggRetirerAxeX + ylab("nombre de lieux de travail distincts") +
#   theme(legend.position = "none") +
#   labs(title = "Nombre de lieux de travail distincts")

# cowplot::plot_grid(g1 + theme(legend.position = "none"), NULL, g2, get_legend(g1),
#                    nrow = 2, rel_widths = c(8,2)) %>%
#   viz_Titre("Présence sur le lieu d'emploi\nselon les contextes géographiques") %>%
#   viz_Pied(src_fig()) %>% print()

PER %>%
  filter(DuTvl > 0 & DuTvl > 0) %>%
  filter(!PCS42S %in% c("00", as.character(81:99))) %>%
  group_by(PCS42S = etqPCS42S(PCS42S)) %>% summarise(m = heureMinToHr(mean(duTvlPlage), sec=F))

# ~ Histogrammes H et F ====

moyennes = PER_ff %>%
  filter(DuTvl < 14*60) %>%
  mutate(DuTvl = DuTvl/60) %>%
  mutate(Genre = etqGenre(Genre)) %>%
  group_by(Genre) %>% summarise(moy = mean(DuTvl, na.rm=T))

sortie("Temps/Temps au lieu d'emploi gen")
PER_ff %>%
  filter(DuTvl < 14*60) %>%
  mutate(DuTvl = trunc(DuTvl / 60),
         DuTvl = paste0("de ", DuTvl, " à ", DuTvl + 1, " h"),
         DuTvl = factor(DuTvl, levels = niveaux)) %>%
  group_by(Genre = etqGenre(Genre), DuTvl) %>% summarise(n = n()) %>%
  mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = DuTvl, y = p)) + geom_col(aes(fill = Genre)) +
  geom_text(aes(label = paste0(round(p,1), " %")), size=2.5, angle=90, hjust=-0.1) +
  coord_cartesian(ylim = c(0,30)) +
  labs(title = "Temps de présence sur le lieu de travail",
       subtitle = "Personnes s'étant déplacées sur leur lieu de travail le jour d'enquête (journées >14h exclues)",
       caption = src_fig(PER)) +
  xlab("Présence sur le lieu de travail") + ylab("Part de la population s'étant déplacée (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_wrap(~Genre) +
  geom_vline(data = moyennes, aes(xintercept = moy), linetype=2, alpha = .33) +
  geom_text(data = moyennes, aes(label = paste0("Moyenne : ", round(moy, 2), " h"), x = moy),
            y = 30, hjust = 1.1, size = 2, fontface = "italic") +
  scale_fill_manual(values = c("coral", "thistle")) +
  theme(legend.position = "none")
off()

# ~ Modèles LM temps au travail ====

regressionLog(base = filter(PER_trav, DuTvl>60),
              val = "DuTvl", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "temps au domicile (h)",
              titre = "Modélisation du temps passé au travail (un jour travaillé)", unite = "h",
              imprDistrib = T) %>% summary()

# Modèle (linéaire) le plus simple, historique, avec zonedens et zonerang
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "54")) %>%
  regression(val = "DuTvl", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)", unite = "min", imprDistrib = T)

# Modèle enrichi (mars 2023), avec valeurs continue : temps de déplacement, temps passé à faire d'autres activités
# Proxy position sociale = PCS42S
sortie("Temps/Temps au lieu d'emploi, modèle", portrait=T)
PER_ff %>% 
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "51"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + PCS42S + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1A", unite = "min", imprDistrib = F)
off()

# Même que le précédent
# Proxy position sociale = niveau de diplôme (champ regroupé inventé pour l'occasion)
sortie("Temps/Temps au lieu d'emploi, avec nivDip", portrait=T)
base_mod1b = PER_ff |>
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(as.factor(NivDip), "2"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"))
mod1b = base_mod1b %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + NivDip + Age10 + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1B", unite = "min", imprDistrib = T)
off()

sortie("Temps/Temps au lieu d'emploi, modèle 1C")
# Avec les PCS8, pour une application sur une base géographique plus large...
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(Activ %in% c("10", "11", "12"), Age>15, Age<70) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06")) %>%
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         PCS8 = relevel(PCS8, "04"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans")) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + PCS8 + Age10 + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1C", unite = "min", imprDistrib = T)
off()

# Même que le précédent, mais on va différencier par PCS
sortie("Temps/Temps au lieu d'emploi, modèles 2", taille = "man",
       h = 15, l = 22.5)
PER_ff |>
  mutate(PCS8 = relevel(PCS8, "04"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         PCS8 = etqPCS8(PCS8, num = T),
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (Modèles 2)", unite = "min",
             colComparaison = "PCS8")
off()

# Même que le précédent, mais on va différencier par secteur public/privé pour les comparer,
# tout en gardant les PCS 3, 4 et 5 en variables de contrôle
sortie("Temps/Temps au lieu d'emploi, modèles 3")
PER_ff %>%
  mutate(PCS8 = relevel(PCS8, "04"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public"),
         secteur = as.factor(secteur)) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + PCS8 + Tps + plusLieuxTravail + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (Modèles 3)", unite = "min",
             colComparaison = "secteur")
off()

# Série de modèles n°3, en utilisant cette fois le niveau de diplôme
sortie("Temps/Temps au lieu d'emploi, modèles 3 par nivDip")
PER_ff |>
  mutate(NivDip = NivEtuVersNivDip(NivEtu),
         NivDip = relevel(NivDip, "2"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public"),
         secteur = as.factor(secteur)) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + NivDip + Tps + plusLieuxTravail + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)", unite = "min",
             colComparaison = "secteur")
off()

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "51"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  regression(val = "DuTvl", formule = "DuDom + Tps + plusLieuxTravail + PCS42S + Activ + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1A", unite = "min", imprDistrib = T)


PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "51"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  regression(val = "DuTvl", formule = "DuDom + DuCom + DuSvc + DuLsr + DuTax + DuDep",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1A", unite = "min", imprDistrib = T)

# à tester : est-ce qu'il est possible de mettre en relation temps travail et dis d'une autre façon ?
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & PCS8 %in% c("02","03","04","05","06") & Activ %in% c("10","11","12")) %>%
  mutate(Dis = Dis / 1000,
         DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         typoModes = relevel(typoModes, "voiture"),
         PCS8 = relevel(PCS8, "04")) %>%
  regression(val = "DuTvl", formule = "Activ + Genre + PCS8 + ZoneDens + Dis + plusLieuxTravail + DuTvlInv + modes_voiture + modes_marche + modes_vélo + modes_tc_route + modes_tc_light + modes_tc_rail",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail et distance quotidienne\nModèle 4", unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & PCS8 %in% c("02","03","04","05","06") & Activ %in% c("10","11","12")) %>%
  mutate(Dis = Dis / 1000,
         DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         typoModes = relevel(typoModes, "voiture"),
         PCS8 = relevel(PCS8, "04")) %>%
  regression(val = "DuTvl", formule = "Activ + Genre + PCS8 + ZoneDens + Dis + plusLieuxTravail + DuTvlInv",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail et distance quotidienne\nModèle 4", unite = "min", imprDistrib = T,
             colComparaison = "typoModes")

sortie("Temps/Modèle 1A par classe de distance", taille = "page", rotation = T)
PER_ff %>%
  mutate(clDis = discretisation(Dis/1000, methode="quartiles"),
         DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         typoModes = relevel(typoModes, "voiture"),
         PCS42S = relevel(PCS42S, "51")) %>%
  regression(val = "DuTvl", formule = "Activ + Genre + PCS42S + plusLieuxTravail + DuTvlInv",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail et distance quotidienne\nModèle 1A par distance pc.", unite = "min", imprDistrib = T,
             colComparaison = "clDis") |>
  print()
off()

img = image_read(path = "Sorties/Temps/Modèle 1A par classe de distance.png")
img = image_rotate(img, 270)
image_write(img, path = "Sorties/Temps/Modèle 1A par classe de distance.png")


# Sur plutôt des critères de genre à présent

sortie("Temps/Temps au lieu d'emploi, modèle par Genre")
PER_ff |>
  valref() |>
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         Genre = etqGenre(Genre)) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + PCS42S + Activ + MenEnfants",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 4", unite = "min", imprDistrib = T,
             colComparaison = "Genre")
off()

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12"), Age>15, Age<70) %>%
  mutate(PCS42S = relevel(PCS42S, "41"), DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         NivDip = NivEtuVersNivDip(NivEtu), NivDip = relevel(as.factor(NivDip), "2"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans")) %>%
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + plusLieuxTravail + NivDip + Age10 + Activ + MenEnfants",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1B", unite = "min", imprDistrib = T,
             colComparaison = "Genre")

# ~ Cartographie des résidus ====

load("Data/shp_ZT.rds")
load("Data/MEN.rds")
load("Data/shp_ZTS.rds")

# On va utiliser les résidus de ce modèle pour essayer de cartographier ça
length(mod1b$residuals)

baseResid = base_mod1b %>% filter(!is.na(DuTvlInv) & !is.na(Tps) & !is.na(plusLieuxTravail) & !is.na(NivDip) &
                              !is.na(Age10) & !is.na(Activ) & !is.na(Genre) & !is.na(DuTvl))
baseResid$residuals = mod1b$residuals

baseCarte = baseResid %>% group_by(ZT) %>%
  summarise(residuMoy = weighted.mean(residuals, w=CoeffEnq)) %>%
  left_join(shp_ZT, by="ZT") %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry)) %>%
  select(ZT, residuMoy, uid_ENQ)

baseCarteZTS = baseResid %>%
  left_join(select(MEN, uid_MEN, ZTS), by="uid_MEN") %>%
  group_by(ZTS) %>%
  summarise(residuMoy = weighted.mean(residuals, w=CoeffEnq)) %>%
  left_join(shp_ZTS, by="ZTS") %>%
  rename(ZT = ZTS) %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry)) %>%
  select(ZT, residuMoy, uid_ENQ)

remove(MEN, shp_ZTS)

baseCarte = rbind(baseCarte, st_transform(baseCarteZTS, crs=2154))

carte_mod1b = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = residuMoy), color = "white", size=.1) +
    scale_fill_gradient2(low = "turquoise", high = "indianred", mid = "whitesmoke", midpoint = 0, 
                         name = "résidus moyens\ndu modèle\n(en minutes)") +
    labs(title = "Résidus du modèle 1B (par secteur de travail)",
         subtitle = ifelse(is.null(sousTitre),
                           z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

load("Data/fdCarte.rds")

uids = unique(baseCarte$uid_ENQ)
gs   = lapply(uids, carte_mod1b, shp = baseCarte)

gs[[length(gs)+1]] = carte_mod1b("IDF2010",
                                 filter(baseCarte, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                                 "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] = carte_mod1b("LOI2015",
                                 filter(baseCarte, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                                 "Agglomération nantaise")

gs[[length(gs)+1]] = carte_mod1b("LYO2015",
                                 filter(baseCarte, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                                 "Agglomération lyonnaise")

sortie("Temps/Modèle DuTvl - Atlas Résidus 1B", format = "pdf", taille = "a4")
print(gs)
off()

baseCarte = baseResid %>% group_by(ZT_travMax) %>%
  summarise(residuMoy = weighted.mean(residuals, w=CoeffEnq), n = n()) %>%
  filter(n>20) %>%
  rename(ZT = ZT_travMax) %>%
  left_join(shp_ZT, by="ZT") %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry))

carte_mod1b_travMax = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = residuMoy), color = "white", size=.1) +
    scale_fill_gradient2(low = "turquoise", high = "indianred", mid = "whitesmoke", midpoint = 0, 
                         name = "résidus moyens\ndu modèle\n(en minutes)") +
    labs(title = "Résidus du modèle 1B (par principal secteur de travail)",
         subtitle = ifelse(is.null(sousTitre),
                           z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(baseCarte$uid_ENQ)
gs   = lapply(uids, carte_mod1b_travMax, shp = baseCarte)

gs[[length(gs)+1]] = carte_mod1b_travMax("IDF2010",
                                         filter(baseCarte, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                                         "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =carte_mod1b_travMax("LOI2015",
                                        filter(baseCarte, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                                        "Agglomération nantaise")

gs[[length(gs)+1]] =carte_mod1b_travMax("LYO2015",
                                        filter(baseCarte, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                                        "Agglomération lyonnaise")

sortie("Temps/Modèle DuTvl - Atlas Résidus 1B travMax.pdf", format = "pdf", taille = "a4")
print(gs)
off()

# plus court que prévu : outremer, petites villes, rural
baseResid %>% group_by(uid_ENQ) %>%
  summarise(residuMoy = weighted.mean(residuals, w=CoeffEnq)) %>%
  tab_Tri(parCol = "residuMoy") %>% head(n=20) |>
  left_join(z_Nomenclature, by = "uid_ENQ")

# plus long que prévu : enquêtes frontalières, agglos c^ Grenoble, Lyon, Toulouse, Rennes...
baseResid %>% group_by(uid_ENQ) %>%
  summarise(residuMoy = weighted.mean(residuals, w=CoeffEnq)) %>%
  tab_Tri(parCol = "residuMoy", rev=T) %>% head(n=20) |>
  left_join(z_Nomenclature, by = "uid_ENQ")

# Modèle 3

# Analyse des résidus
base3a = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in%  c("36", "46", "47", "48", "54", "55", "56"), 
         Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         PCS8 = relevel(PCS8, "04")) %>%
  filter(!is.na(DuTvlInv), !is.na(PCS8), !is.na(Tps), !is.na(plusLieuxTravail),
         !is.na(Activ), !is.na(Genre))

sortie("Temps au lieu d'emploi, modèle 3A privé")
mod3a = regression(base3a, val = "DuTvl",
                   formule = "DuTvlInv + PCS8 + Tps + plusLieuxTravail + Activ + Genre",
                   poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
                   titre = "Temps passé au lieu de travail (Modèle 3A, Privé)", unite = "min")
base3a$res = mod3a$residuals
off()

base3a = base3a %>% left_join(select(MEN, uid_MEN, ZTS), by="uid_MEN")

base3a_ZT = base3a %>%
  group_by(ZT) %>% summarise(resMoy = weighted.mean(res, w = CoeffEnq))

base3a_ZTS = base3a %>%
  group_by(ZTS) %>% summarise(resMoy = weighted.mean(res, w = CoeffEnq))

base3a_shp_zt = shp_ZT %>%
  left_join(base3a_ZT, by="ZT" ) %>%
  select(resMoy, ZT, uid_ENQ)
base3a_shp_zts = shp_ZTS %>%
  left_join(base3a_ZTS,by="ZTS") %>%
  select(resMoy, ZTS, uid_ENQ) %>%
  rename(ZT = ZTS)

base3a_shp = rbind(st_transform(base3a_shp_zt, crs=2154),
                   st_transform(base3a_shp_zts,crs=2154)) %>%
  filter(!is.na(resMoy))

carte_mod3a = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = resMoy), color = "white", size=.1) +
    scale_fill_gradient2(low = "turquoise", high = "indianred", mid = "whitesmoke", midpoint = 0, 
                         name = "résidus moyens\ndu modèle\n(en minutes)") +
    labs(title = "Résidus du modèle 3A (secteur privé)",
         subtitle = ifelse(is.null(sousTitre),
                           z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base3a_shp$uid_ENQ[base3a_shp$uid_ENQ != "EMP2019"])
gs   = lapply(uids, carte_mod3a, shp = base3a_shp)

gs[[length(gs)+1]] = carte_mod3a("IDF2010",
                                 filter(baseCarte, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                                 "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =carte_mod3a("LOI2015",
                                filter(baseCarte, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                                "Agglomération nantaise")

gs[[length(gs)+1]] =carte_mod3a("LYO2015",
                                filter(baseCarte, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                                "Agglomération lyonnaise")

sortie("Temps / Modèle DuTvl - Atlas Résidus 3A.pdf", format = "pdf", taille = "a4")
print(gs)
off()

summary(base3a$res)

base3a %>%
  group_by(uid_ENQ) %>% summarise(resMoy = weighted.mean(res, w = CoeffEnq)) %>%
  tab_Tri(parCol = "resMoy") %>% head(n=20)

base3a %>%
  group_by(uid_ENQ) %>% summarise(resMoy = weighted.mean(res, w = CoeffEnq)) %>%
  tab_Tri(parCol = "resMoy", rev=T) %>% head(n=20)

# Idem, par travMax
base3a_ZT = base3a %>%
  group_by(ZT_travMax) %>% summarise(resMoy = weighted.mean(res, w = CoeffEnq)) %>%
  rename(ZT = ZT_travMax)
base3a_shp_zt = shp_ZT %>%
  left_join(base3a_ZT, by="ZT" ) %>%
  select(resMoy, ZT, uid_ENQ)

summary(base3a_ZT$resMoy)



carte_mod3a_travMax = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = resMoy), color = "white", size=.1) +
    scale_fill_gradient2(low = "turquoise", high = "indianred", mid = "whitesmoke", midpoint = 0, 
                         name = "résidus moyens\ndu modèle\n(en minutes)") +
    labs(title = "Résidus du modèle 3A (par secteur du lieu d'emploi, secteur privé)",
         subtitle = ifelse(is.null(sousTitre),
                           z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp))
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base3a_shp_zt$uid_ENQ)
gs   = lapply(uids, carte_mod3a_travMax, shp = base3a_shp_zt)

gs[[length(gs)+1]] = carte_mod3a_travMax("IDF2010",
                                         filter(base3a_shp_zt, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                                         "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =carte_mod3a_travMax("LOI2015",
                                        filter(base3a_shp_zt, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                                        "Agglomération nantaise")

gs[[length(gs)+1]] =carte_mod3a_travMax("LYO2015",
                                        filter(base3a_shp_zt, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                                        "Agglomération lyonnaise")

sortie("Temps / Modèle DuTvl - Atlas Résidus 3A travMax.pdf", format = "pdf", taille = "a4")
print(gs)
off()

remove(gs) ; remove(fdCarte) ; remove(base) ; remove(base_mod1b) ; gc()

# ~ Comparaisons au sein du couple ====

load("Data/MEN.rds")

MEN %>% select(uid_MEN) %>%
  left_join(select(filter(PER, Lien == "1"), uid_MEN, Genre), by="uid_MEN") %>%
  rename(Genre1 = Genre) %>%
  left_join(select(filter(PER, Lien == "2"), uid_MEN, Genre), by="uid_MEN") %>%
  rename(Genre2 = Genre) %>%
  group_by(Genre1, Genre2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(p = n / sum(n) * 100)

tpsMen = MEN %>% select(uid_MEN) %>%
  left_join(select(filter(PER, Lien == "1"), uid_MEN, DuTvl), by="uid_MEN") %>%
  rename(DuTvl1 = DuTvl) %>%
  left_join(select(filter(PER, Lien == "2"), uid_MEN, DuTvl), by="uid_MEN") %>%
  rename(DuTvl2 = DuTvl)

# Base de comparaison des membres du ménage (à restreindre en termes de genre)
PER_DuTvl_Conj = PER_ff %>% filter(Lien %in% c("1", "2"), PCS8 != "01") %>%
  # valref() |>
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail")) %>%
  # Système de jointures pour récupérer le temps de trajet du/de la conjointe
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER_ff, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) |>
  filter(PCS8.conj != "01")

PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0) %>% 
  regression(val = "DuTvl", formule = "DuTvlInv + Tps + DuTvl.conj + plusLieuxTravail + PCS42S + Activ + MenEnfants",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail\nModèle 1A", unite = "min", imprDistrib = T,
             colComparaison = "Genre")

sortie("Temps/Temps passé au lieu d'emploi, modèle Par_Couple.svg", taille = "page")
PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(couple = paste0(etqPCS8(PCS8), " + ", etqPCS8(PCS8.conj)),
         couple = as.factor(couple), couple = relevel(couple, "Employé·e + Employé·e")) %>%
  regressionLog(val = "gap", formule = "DuTvl + couple + MenEnfants + ZoneDens + ZoneRang",
                poids = "CoeffEnq", retirerZ = T, 
                titre = "Modélisation du rapport entre le temps\npassé au travail par les deux\nmembres du couple", unite = "%", imprDistrib = T,
                colComparaison = "Genre", verbose=T, valIntervalleSur100 = .25)
off()

PER_DuTvl_Conj %>%
  filter(ZoneRang != "6") %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(couple = paste0(etqPCS8(PCS8), " + ", etqPCS8(PCS8.conj)),
         couple = as.factor(couple), couple = relevel(couple, "Employé·e + Employé·e")) %>%
  regressionLog(val = "gap", formule = "couple + MenEnfants + ZoneDens + ZoneRang",
                poids = "CoeffEnq", retirerZ = T, 
                titre = "Modélisation du rapport entre le temps\npassé au travail par les deux\nmembres du couple", unite = "%", imprDistrib = T,
                colComparaison = "Genre", verbose=T, valIntervalleSur100 = .25)

PER_DuTvl_Conj %>%
  filter(ZoneRang != "6") %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(couple = paste0(etqPCS8(PCS8), " + ", etqPCS8(PCS8.conj)),
         couple = as.factor(couple), couple = relevel(couple, "Employé·e + Employé·e")) %>%
  mutate(coupleDip = paste0(NivEtuVersNivDip(NivEtu), " + ", NivEtuVersNivDip(NivEtu.conj))) %>%
  regressionLog(val = "gap", formule = "coupleDip",
                poids = "CoeffEnq", retirerZ = T, 
                titre = "Modélisation du rapport entre le temps\npassé au travail par les deux\nmembres du couple", unite = "%", imprDistrib = T,
                colComparaison = "Genre", verbose=T, valIntervalleSur100 = .25)

sortie("Temps/Temps sur lieu d'emploi, rapport hommes et femmes")
PER_DuTvl_Conj %>%
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  filter(Genre.conj == "H") %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8, genre = "F"), PCS8.conj = etqPCS8(PCS8.conj, genre = "H")) %>%
  group_by(Genre, PCS8, PCS8.conj) %>% summarise(n = n(),
                                                 gap = weighted.median(gap, w=CoeffRecEnqSansEMP)) %>%
  filter(Genre == "F") %>%
  mutate(gap_txt = gsub(x = as.character(round(gap,1)),
                        pattern = ".", replacement = ",", fixed=T)) |>
  ggplot(aes(x = PCS8, y = PCS8.conj)) + geom_point(aes(size = n, fill = gap), shape=22) +
  geom_text(aes(label = gap_txt), nudge_y=-.35) +
  scale_fill_gradient2(midpoint = 100, name = "rapport\ntps lieu travail/\ntps lieu travail\ndu conjoint\n(en %)") +
  scale_size(range = c(2,12), name = "effectif\n(nombre de\nménages)") +
  xlab("PCS de la femme") + ylab("PCS du conjoint") +
  labs(title = "Médiane du rapport entre le temps passé au lieu d'emploi par la femme\net l'homme des couples H/F",
       subtitle = "Selon la PCS des conjoint·es, hors agriculteur⋅rices",
       caption = src_fig(bu = T, emp = F)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
off()

PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, Genre.conj == "H", PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuDom/DuDom.conj * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8, genre = "F"), PCS8.conj = etqPCS8(PCS8.conj, genre = "H")) %>%
  group_by(Genre, PCS8, PCS8.conj) %>% summarise(n = n(), gap = median(gap)) %>%
  filter(Genre == "F") %>%
  ggplot(aes(x = PCS8, y = PCS8.conj)) + geom_point(aes(size = n, fill = gap), shape=22) +
  geom_text(aes(label = round(gap,1)), nudge_y=-.25) +
  scale_fill_gradient2(midpoint = 100, name = "rapport\ntps domicile /\ntps domicile\ndu conjoint\n(en %)") +
  scale_size(range = c(2,12), name = "effectif\n(nombre de\nménages)") +
  xlab("PCS de la femme") + ylab("PCS du conjoint") +
  labs(title = "Médiane du rapport entre le temps passé au domicile par la femme\net l'homme des couples H/F",
       subtitle = "Selon la PCS des conjoint·es",
       caption = src_fig(PER)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8, genre = "F"), PCS8.conj = etqPCS8(PCS8.conj, genre = "H")) %>%
  group_by(MenEnfants, Genre, PCS8, PCS8.conj) %>% summarise(n = n(), gap = median(gap)) %>%
  filter(Genre == "Femme") %>%
  ggplot(aes(x = PCS8, y = PCS8.conj)) +
  geom_point(aes(size = n, fill = gap), shape=22) +
  geom_text(aes(label = round(gap,1)), nudge_y=-.25) +
  scale_size(range=c(2,12)) +
  scale_fill_gradient2(midpoint = 100) + facet_wrap(~MenEnfants)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & Lien %in% c("1", "2")) %>%
  filter(PCS42S %in% as.character(c(21:69)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvlInv = DuCom + DuSvc + DuLsr + DuTax,
         plusLieuxTravail = ifelse(nbLxTvl == 1, "un lieu de travail", "plusieurs lieux de travail"),
         plusLieuxTravail = as.factor(plusLieuxTravail),
         plusLieuxTravail = relevel(plusLieuxTravail, "un lieu de travail"),
         ZoneDens = relevel(ZoneDens, "1"), ZoneRang = relevel(ZoneRang, "4"),
         Genre = etqGenre(Genre)) %>%
  filter(ZoneRang != "6") %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER, uid_MEN, Lien, PCS42S, DuTvl, DuEtu, DuDom), by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS42S.conj %in% as.character(c(21:69))) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(PCS42S = etqPCS42S(PCS42S, genre = "F"), PCS42S.conj = etqPCS42S(PCS42S.conj, genre = "H")) %>%
  group_by(Genre, PCS42S, PCS42S.conj) %>% summarise(n = n(), gap = median(gap)) %>%
  filter(Genre == "Femme", n > 50) %>%
  ggplot(aes(x = PCS42S, y = PCS42S.conj)) + geom_point(aes(size = n, fill = gap), shape=22) +
  scale_fill_gradient2(midpoint = 100) + scale_size(range = c(2,12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

sortie("Temps/Temps au lieu d'emploi, par Conjoints _NivDip")
PER_ff %>% 
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  mutate(Genre = etqGenre(Genre),
         NivDip = NivEtuVersNivDip(NivEtu)) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(mutate(PER_ff, NivDip = NivEtuVersNivDip(NivEtu)),
                   uid_MEN, Lien, NivDip, DuTvl, DuEtu, DuDom),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  filter(!is.na(NivDip.conj)) |>
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(NivDip = etqNivDip(NivDip), NivDip.conj = etqNivDip(NivDip.conj)) %>%
  group_by(Genre, NivDip, NivDip.conj) %>%
  summarise(n = n(),
            gap = weighted.median(gap, w=CoeffRecEnqSansEMP)) %>%
  filter(Genre == "Femme", n > 50) %>%
  filter(!is.na(NivDip), !is.na(NivDip.conj)) |>
  mutate(gap_txt = gsub(x = as.character(round(gap, 1)),
                        pattern = ".", replacement = ",", fixed = T)) |>
  ggplot(aes(x = NivDip, y = NivDip.conj)) + geom_point(aes(size = n, fill = gap), shape=22) +
  geom_text(aes(label = gap_txt), nudge_y=-.35) +
  scale_size(range = c(2,12), name = "effectif\n(nombre de\nménages)") +
  scale_fill_gradient2(midpoint = 100, name = "rapport\ntps lieu travail/\ntps lieu travail\ndu conjoint\n(en %)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Niveau de diplôme de la femme") + ylab("Niveau de diplôme du conjoint") +
  labs(title = "Médiane du rapport entre le temps passé au lieu d'emploi\npar la femme et l'homme des couples H/F",
       subtitle = "Selon le niveau de diplôme des conjoint·es, hors agriculteur⋅rices",
       caption = src_fig(bu = T, emp = F))
off()

PER_ff |>
  mutate(Genre = etqGenre(Genre),
         NivDip = NivEtuVersNivDip(NivEtu)) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(mutate(PER, NivDip = NivEtuVersNivDip(NivEtu)),
                   uid_MEN, Lien, NivDip, DuTvl, DuEtu, DuDom),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0) %>% 
  mutate(gap = DuDom/DuDom.conj * 100) %>%
  mutate(NivDip = etqNivDip(NivDip), NivDip.conj = etqNivDip(NivDip.conj)) %>%
  group_by(Genre, NivDip, NivDip.conj) %>% summarise(n = n(), gap = median(gap)) %>%
  filter(Genre == "Femme", n > 50) %>%
  ggplot(aes(x = NivDip, y = NivDip.conj)) + geom_point(aes(size = n, fill = gap), shape=22) +
  geom_text(aes(label = round(gap,1)), nudge_y=-.25) +
  scale_fill_gradient2(midpoint = 100, name = "rapport\ntps domicile / \ntps domicile\ndu conjoint\n(en %)") +
  scale_size(range = c(2,12), name = "effectif\n(nombre de\nménages)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Niveau de diplôme de la femme") + ylab("Niveau de diplôme du conjoint") +
  labs(title = "Médiane du rapport entre le temps passé au domicile par la femme\net l'homme des couples H/F",
       subtitle = "Selon le niveau de diplôme des conjoint·es",
       caption = src_fig(PER))

PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  mutate(couple = paste0(etqPCS8(PCS8), " + ", etqPCS8(PCS8.conj)),
         couple = as.factor(couple), couple = relevel(couple, "Employé·e + Employé·e")) %>%
  regressionLog(val = "gap", formule = "couple + MenEnfants + ZoneDens + ZoneRang",
                poids = "CoeffEnq", retirerZ = T, 
                titre = "Modélisation du rapport entre le temps\npassé au travail par les deux\nmembres du couple", unite = "%", imprDistrib = T,
                colComparaison = "Genre", verbose=T, valIntervalleSur100 = .25)

test =  PER_DuTvl_Conj %>% 
  filter(PCS8.conj %in% c("02", "03", "04", "05", "06")) %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100)

testF = PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06"), Genre == "F" & Genre.conj == "H") %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100)
testH = PER_DuTvl_Conj %>%
  filter(DuTvl.conj>0, DuDom.conj >0, DuEtu.conj == 0, PCS8.conj %in% c("02", "03", "04", "05", "06"), Genre == "H" & Genre.conj == "F") %>% 
  mutate(gap = DuTvl/DuTvl.conj * 100)

nrow(filter(testF, gap>100)) / nrow(testF)
nrow(filter(testF, gap>125)) / nrow(testF)
nrow(filter(testH, gap>125)) / nrow(testH)

summary(test$gap)

remove(test, testF, testH)

PER_ff %>%   filter(Lien %in% c("1", "2")) |>
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(mutate(PER_ff, NivDip = NivEtuVersNivDip(NivEtu)),
                   uid_MEN, Lien, NivDip, DuTvl, DuEtu, DuDom),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  group_by(Genre) |>
  summarise(n = n(), gap = median(gap, na.rm=T)) %>%
  filter(Genre == "F", n > 50)

PER_ff %>%   filter(Lien %in% c("1", "2")) |>
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(mutate(PER_ff, NivDip = NivEtuVersNivDip(NivEtu)),
                   uid_MEN, Lien, NivDip, DuTvl, DuEtu, DuDom),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  group_by(dsDomEtq, Genre) |>
  summarise(n = n(), gap = median(gap, na.rm=T)) %>%
  filter(Genre == "F", n > 50)

PER_ff %>%  filter(Lien %in% c("1", "2")) |>
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(mutate(PER_ff, NivDip = NivEtuVersNivDip(NivEtu)),
                   uid_MEN, Lien, NivDip, DuTvl, DuEtu, DuDom),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(gap = DuTvl/DuTvl.conj * 100) %>%
  group_by(uid_ENQ, Genre) |>
  summarise(n = n(), gap = median(gap, na.rm=T)) %>%
  filter(Genre == "F", n > 50) |>
  tab_Tri(parCol = "gap") |> left_join(z_Nomenclature, by="uid_ENQ")


# ~ Durée au travail médiane par département ====

PER_ff %>%
  filter(!is.na(CoeffRecEnq), !is.na(DuTvl)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsTvl) %>%
  summarise(DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnq)/60, n = sum(CoeffEnq)) |>
  mutate(DuTvlMoy = heureMinToHr(DuTvlMoy*60))

PER_ff %>%
  filter(!is.na(CoeffRecEnq), !is.na(DuTvl)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(PCS8, dsTvl) %>%
  summarise(DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnq)/60, n = sum(CoeffEnq)) |>
  mutate(DuTvlMoy = heureMinToHr(DuTvlMoy*60)) |>
  select(-n) |>
  pivot_wider(names_from = PCS8, values_from = DuTvlMoy) |> t()

PER_ff %>%
  filter(!is.na(CoeffRecEnq), !is.na(DuTvl)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(PCS42S, dsTvl) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) |>
  summarise(DuTvlMoy = weighted.mean(DuTvl, w=CoeffRecEnq)/60, n = sum(CoeffEnq)) |>
  mutate(DuTvlMoy = heureMinToHr(DuTvlMoy*60)) |>
  select(-n) |>
  pivot_wider(names_from = PCS42S, values_from = DuTvlMoy) |> t()

PER_ff %>%
  filter(!is.na(CoeffRecEnq), !is.na(DuTvl)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(PCS8, dsTvl) %>%
  summarise(nMoy = round(weighted.mean(nbLxTvl, w=CoeffRecEnq), 1), n = sum(CoeffEnq)) |>
  select(-n) |>
  pivot_wider(names_from = PCS8, values_from = nMoy) |> t()

PER_ff %>%
  filter(!is.na(CoeffRecEnq), !is.na(DuTvl)) |>
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(PCS42S, dsTvl) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) |>
  summarise(nMoy = round(weighted.mean(nbLxTvl, w=CoeffRecEnq), 1), n = sum(CoeffEnq)) |>
  select(-n) |>
  pivot_wider(names_from = PCS42S, values_from = nMoy) |> t()


  PER %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(ZoneDens) %>% summarise(DuTvlMed = median(DuTvl)/60,
                                   n = sum(CoeffEnq))
PER_ff %>%
  mutate(dsTvl = discretisation(dsTvl, methode = "quartiles")) |>
  group_by(dsTvl) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w=CoeffRecEnq)/60, n = sum(CoeffEnq))

PER %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(ZoneRang) %>% summarise(DuTvlMed = median(DuTvl)/60,
                                   n = sum(CoeffEnq))

PER %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(uid_ENQ) %>% summarise(DuTvlMed = weighted.median(DuTvl, weights = CoeffEnq)/60,
                                  n = sum(CoeffEnq)) %>% 
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  tab_Tri(parCol = "DuTvlMed") %>% head(n = 20)

PER %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(uid_ENQ) %>% summarise(DuTvlMed = weighted.median(DuTvl, weights = CoeffEnq)/60,
                                  n = sum(CoeffEnq)) %>% 
  left_join(z_Nomenclature, by="uid_ENQ") %>%
  tab_Tri(parCol = "DuTvlMed", rev=T)  %>% head(n = 20)

baseDep = PER_ff %>%
  filter(uid_ENQ == "EMP2019") %>%
  mutate(Dep = substr(Com, 1,2)) %>%
  group_by(Dep) %>% summarise(DuTvlMed = weighted.median(DuTvl, w = CoeffEnq)/60,
                              pop = sum(CoeffEnq), n = n()) %>%
  mutate(DuTvlMed = discretisation(DuTvlMed)) %>%
  filter(n>10)

# Unification de la Corse
shp_Dep = read_sf("Sources/Fond Carte/DEPARTEMENT.shp") %>%
  mutate(INSEE_DEP = ifelse(INSEE_DEP %in% c("2A", "2B"), "02", INSEE_DEP)) %>%
  group_by(INSEE_DEP) %>% summarise() %>%
  left_join(baseDep, by=c("INSEE_DEP" = "Dep")) %>%
  st_simplify(preserveTopology = T, dTolerance = 1000)

etendue = summarise(shp_Dep)
load("Data/fdCarte.rds")

g = ggplot(data = shp_Dep) +
  geom_sf(aes(fill = DuTvlMed), color = "white", size=.25, ) +
  labs(title = "Durée de travail médiane par département (Hexagone)",
       caption = src_fig(bu = F, emp = T, carto=T)) +
  scale_fill_brewer (palette = 2, 
                     name = "durée de travail\nmédiane (heures)", na.value = "grey") +
  geom_sf(data = st_point_on_surface(shp_Dep), aes(size = n),
          shape = 21, color = "gray20", alpha = .4) +
  scale_size(name = "échantillon\n(nb. enquêté·es)")
g = cartoLib(g, etendue, detail = 4)
g = cartoFinish(g, etendue)

sortie("Temps/Temps au lieu d'emploi par département", taille = "man",
       h = 14)
print(g)
off()

filter(PER, uid_ENQ == "EMP2019" & Com == "78000" & PCS8 == "03") %>% nrow()
filter(PER, uid_ENQ == "EMP2019" & Com == "78000") %>% nrow()

filter(PER, uid_ENQ == "EMP2019" & Com == "92000" & PCS8 == "03") %>% nrow() /
  filter(PER, uid_ENQ == "EMP2019" & Com == "92000") %>% nrow()

filter(PER, uid_ENQ == "EMP2019" & PCS8 == "03") %>% nrow()
filter(PER, uid_ENQ == "EMP2019") %>% nrow()

# ~ Cartographie détaillée du temps au travail ====

base = PER_ff |>
  filter(!is.na(CoeffRecEnqSansEMP)) |>
  group_by(PCS8, ZT) %>%
  summarise(DuTvlMed = weighted.median(DuTvl, w = CoeffRecEnqSansEMP)/60,
                                   n = sum(CoeffRecEnqSansEMP)) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  mutate(PCS8 = etqPCS8(PCS8))

summary(base$DuTvlMed)
quantile(base$DuTvlMed, probs=c(.05, .95))

carte_duTvlMed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = DuTvlMed), color = "white", size=.1) +
    scale_fill_gradient (low = "khaki", high = "turquoise", limits=c(6,10),
                         name = "durée de travail\nmédiane (heures)") +
    labs(title = "Durée de travail médiane par PCS",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp)) +
    geom_sf(data = st_point_on_surface(b), aes(size = n),
            shape=21, color = "gray20", alpha=.25) +
    scale_size(range = c(2,12), name = "heures passées\nsur le lieu de travail\n(total, en h)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base$uid_ENQ)
gs   = lapply(uids, carte_duTvlMed, shp = base)

gs[[length(gs)+1]] = carte_duTvlMed("IDF2010",
                                    filter(base, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                                    "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =carte_duTvlMed("LOI2015",
                                   filter(base, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                                   "Agglomération nantaise")

gs[[length(gs)+1]] =carte_duTvlMed("LYO2015",
                                   filter(base, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                                   "Agglomération lyonnaise")

sortie("Temps/Atlas durée journée travail", format = "pdf", taille = "a4")
print(gs)
off()

# Durée de travail médiane, par ZT_travMax

base = PER %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(PCS8, ZT_travMax) %>% summarise(DuTvlMed = weighted.median(DuTvl, weights = CoeffEnq)/60,
                                           n = sum(CoeffEnq)) %>%
  rename(ZT = ZT_travMax) %>%
  filter(n>20) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  mutate(PCS8 = etqPCS8(PCS8))

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = DuTvlMed), color = "white", size=.1) +
    scale_fill_gradient (low = "khaki", high = "turquoise",
                         name = "durée de travail\nmédiane (heures)") +
    labs(title = "Durée de travail médiane par PCS",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp)) +
    geom_sf(data = st_point_on_surface(b), aes(size = n),
            shape=21, color = "gray20", alpha=.25) +
    scale_size(range = c(2,12), name = "heures passées\nsur le lieu de travail\n(total, en h)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base$uid_ENQ)
gs   = lapply(uids, proceed, shp = base)

gs[[length(gs)+1]] = proceed("IDF2010",
                             filter(base, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                             "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =proceed("LOI2015",
                            filter(base, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                            "Agglomération nantaise")

gs[[length(gs)+1]] =proceed("LYO2015",
                            filter(base, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                            "Agglomération lyonnaise")

sortie("Temps/Atlas durée journée travail parZT_travMax.pdf", taille = "a4", format = "pdf")
print(gs)
off()

base = PER %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(Activ %in% c("10","11")) %>%
  group_by(PCS8, ZT) %>% summarise(nTpsPart = sum(ifelse(Activ == "11", CoeffEnq, 0)),
                                   n = sum(CoeffEnq)) %>%
  mutate(pTpsPart = nTpsPart/n * 100) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  mutate(PCS8 = etqPCS8(PCS8))

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  g = ggplot(data = b) +
    geom_sf(aes(fill = pTpsPart), color = "white", size=.1) +
    scale_fill_gradient (low = "aliceblue", high = "turquoise4", limits=c(0,100),
                         name = "recours au\ntemps partiel\n(% des travailleur·ses\ns'étant déplacé·es)") +
    labs(title = "Durée de travail médiane par PCS",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(b), aes(size = n),
            shape=21, color = "gray20", alpha=.25) +
    scale_size(range = c(2,12), name = "heures passées\nsur le lieu de travail\n(total, en h)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base$uid_ENQ)
gs   = lapply(uids, proceed, shp = base)

gs[[length(gs)+1]] = proceed("IDF2010",
                             filter(base, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                             "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =proceed("LOI2015",
                            filter(base, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                            "Agglomération nantaise")

gs[[length(gs)+1]] =proceed("LYO2015",
                            filter(base, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                            "Agglomération lyonnaise")

sortie("Temps/Atlas recours temps partiel.pdf", format = "pdf", taille = "a4")
print(gs)
off()

# ~ Volumes horaires comparés 2 à 2 ====

## Rapport PCS3/PCS5

rapport("Volumes horaires PCS3 vs PCS5")
seuil = 10

duParZT3 = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(PCS8 == "03") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps_3 = sum(du * CoeffEnq)/60, n_3 = n())

duParZT5 = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(PCS8 == "05") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps_5 = sum(du * CoeffEnq)/60, n_5 = n())

duParZT = left_join(duParZT3, shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  left_join(duParZT5, by="ZT") %>%
  mutate(rapport = ifelse(n_3 > seuil & n_5 > seuil, (temps_5/temps_3 - 1) * 100, NA))

rapport("Avec un seuil de", seuil, ":", nrow(filter(duParZT, is.na(rapport))), "secteurs retirés")

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

valIntervalleSur100 = 2
ticks = c(-80, -50, 0, 100, 300)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT, uid_ENQ == uid)) +
    geom_sf(aes(fill = rapport), color = "white", size=.1) +
    scale_fill_gradient2(low = pal_PCS8[3], mid = "gainsboro", high = pal_PCS8[5],
                         midpoint = 0,
                         trans = trans_sur100, breaks = ticks,
                         labels = transf_echelle_sur100_lab(ticks, deuxLignes = F),
                         name = "rapport heures d'ac.\nemployé⋅es/cadres\n(en %)") +
    labs(title = "Heures d'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT, uid_ENQ == uid)), aes(size = temps_5+temps_3),
            shape=21, color = "gray20", alpha=.5) +
    scale_size(range = c(2,12), name = "temps d'activité\nhors domicile\n(employé⋅es +\ncadres, en h)")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

## Rapport Genre
rapport("Volumes horaires h vs f")
seuil = 10

duParZT_F = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, Genre), by="uid_PER") %>%
  filter(Genre == "F") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps_F = sum(du * CoeffEnq)/60, n_F = n())

duParZT_H = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, Genre), by="uid_PER") %>%
  filter(Genre == "H") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps_H = sum(du * CoeffEnq)/60, n_H = n())

duParZT = left_join(duParZT_F, shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  left_join(duParZT_H, by="ZT") %>%
  mutate(rapport = ifelse(n_F > seuil & n_H > seuil, (temps_F/temps_H - 1) * 100, NA))

rapport("Avec un seuil de", seuil, ":", nrow(filter(duParZT, is.na(rapport))), "secteurs retirés")

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

valIntervalleSur100 = 2
ticks = c(-80, -50, 0, 100, 300)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT, uid_ENQ == uid)) +
    geom_sf(aes(fill = rapport), color = "white", size=.1) +
    scale_fill_gradient2(low = "thistle", mid = "gainsboro", high = "brown",
                         midpoint = 0,
                         trans = trans_sur100, breaks = ticks,
                         labels = transf_echelle_sur100_lab(ticks, deuxLignes = F),
                         name = "rapport heures d'ac.\nfemmes/hommes\n(en %)") +
    labs(title = "Heures d'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT, uid_ENQ == uid)), aes(size = temps_H+temps_F),
            shape=21, color = "gray20", alpha=.5) +
    scale_size(range = c(2,12), name = "temps d'activité\nhors domicile\n(total, en h)")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

# Sur / sous rep : 2 approches
# - écart entre le volume horaire moyen par ressortissant de la catégorie et le volume horaire
#   "dépensé" dans la ZT en fonction du nombre de personnes de la catégorie ?
# - résidus d'un modèle LM ? (risque d'overkill)

duParZT = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(PCS8, ZT) %>% summarise(temps = sum(du)/60)

popParZT = PER %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(PCS8, ZT) %>% summarise(pop = sum(CoeffEnq, na.rm=T))

duParZT = left_join(duParZT, popParZT, by = c("ZT", "PCS8")) %>%
  mutate(tempsParPers = temps / pop) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  mutate(PCS8 = etqPCS8(PCS8))

uids = unique(duParZT$uid_ENQ)

rapportParEnq = ACT %>%
  left_join(select(PER, uid_ENQ, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(PCS8, uid_ENQ) %>% summarise(temps = sum(du)/60, pop = sum(CoeffEnq), tempsParPop = temps/pop) %>%
  mutate(PCS8 = etqPCS8(PCS8))

duParZT = duParZT %>%
  left_join(rapportParEnq, by=c("PCS8", "uid_ENQ"), suffixes=c("", ".enq")) %>%
  mutate(rapportMoyEnq = (tempsParPers/tempsParPop - 1) * 100) %>%
  mutate(rapportMoyEnq = ifelse(rapportMoyEnq < -90 | rapportMoyEnq > 900, NA, rapportMoyEnq)) %>%
  rename(temps = temps.x)

ticks = c(-90,-60,-30,0,300,600,900)

proceed_tempsRelatif = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  g = ggplot(data = filter(shp, uid_ENQ == uid)) +
    geom_sf(aes(fill = rapportMoyEnq), color = "white", size=.1) +
    scale_fill_gradient2(low = "turquoise", high = "darkorange", mid = "gainsboro", midpoint = 0, 
                         trans = trans_sur100, breaks = ticks,
                         labels = transf_echelle_sur100_lab(ticks, deuxLignes = F),
                         name = "surreprésentation/\nratio heures/hab.\nde l'enquête") +
    labs(title = "Heures d'activité des cadres par rapport à leur répartition résidentielle",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(shp, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray20", alpha=.5) +
    scale_size(range = c(2,12), name = "temps d'activité\nhors domicile\n(total, en h)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

gs = lapply(uids, proceed_tempsRelatif, shp = duParZT)

sortie("Temps/Surreprésentation cadres par temps d'activité en IDF")
proceed_tempsRelatif("IDF2010",
                     filter(duParZT, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                     "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")  %>% print()
off()

# Approche à adopter : plutôt identifier la surreprésentation de la part de chaque PCS dans le volume
# d'heures d'activité de chaque ZT par rapport à la part qu'elles représentent dans le volume total
# d'heures d'activité pour l'enquête

# Juste part du temps d'activité par ZT ?

duParZT = ACT %>%
  left_join(select(PER, uid_ENQ, uid_PER, DuTvl, DuEtu, DuDom, PCS8, CoeffRecEnqSansEMP), by="uid_PER") %>%
  filter(uid_PER %in% PER_ff$uid_PER) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  mutate(du = du * CoeffRecEnqSansEMP) %>%
  ztzf(colZF = "l") %>%
  group_by(PCS8, uid_ENQ, ZT) %>% summarise(temps = sum(du)/60) %>%
  group_by(PCS8, uid_ENQ) %>% mutate(pTempsTot = temps/sum(temps) * 100) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  left_join(select(shp_ZT, -uid_ENQ), by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  g = ggplot(data = filter(shp, uid_ENQ == uid)) +
    geom_sf(aes(fill = pTempsTot), color = "white", size=.1) +
    scale_fill_gradient (low = "honeydew", high = "darkolivegreen",
                         name = "part des heures\nhors domicile\npassées dans la ZT\n(en %)") +
    labs(title = "Répartition des heures d'activités par zone de tirage",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(duParZT))
  g = cartoLib(g, etendue = etendue, detail = 1, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(duParZT$uid_ENQ)
gs   = lapply(uids, proceed, shp = duParZT)

sortie("Temps/Temps d'activité en IDF")
proceed("IDF2010",
        filter(duParZT, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
        "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)") %>% print()
off()

sortie("Temps/Temps d'activité en 44")
proceed("LOI2015",
        filter(duParZT, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
        "Agglomération nantaise") %>% print()
off()

# Part du temps passé dans la ZT qui est du travail

# Juste part du temps d'activité par ZT ?

duParZT_trav = ACT %>%
  left_join(select(PER, uid_ENQ, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(substr(Tache, 1, 2) == "10") %>%
  mutate(duTrav = du * CoeffEnq) %>% ztzf(colZF = "l") %>%
  group_by(PCS8, ZT) %>% summarise(duTrav = sum(duTrav) / 60, .groups = "drop")

duParZT = ACT %>%
  left_join(select(PER, uid_ENQ, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(uid_PER %in% PER_ff$uid_PER) |>
  filter(!substr(Tache, 1, 2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  mutate(du = du * CoeffEnq) %>% ztzf(colZF = "l") %>%
  group_by(PCS8, ZT) %>% summarise(du = sum(du) / 60, .groups = "drop") %>%
  left_join(duParZT_trav, by=c("PCS8", "ZT")) %>% mutate(pTrav = duTrav/du * 100)

duParZT = duParZT %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  g = ggplot(data = filter(shp, uid_ENQ == uid)) +
    geom_sf(aes(fill = pTrav), color = "white", size=.1) +
    scale_fill_gradient2(low = "palegreen", high = "darkorange", mid = "gainsboro", midpoint = 50,
                         name = "part des heures\nhors domicile\npassées au travailT\n(en %)") +
    labs(title = "Profil des heures d'activité hors domicile",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendue, proj = st_crs(duParZT))
  g = cartoLib(g, etendue = etendue, detail = 1, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(duParZT$uid_ENQ)
gs   = lapply(uids, proceed, shp = duParZT)

sortie("Temps/Part des heures d'activité au travail, IDF")
proceed("IDF2010",
        filter(duParZT, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
        "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)") %>% print()
off()

sortie("Temps/Part des heures d'activité au travail, 44")
proceed("LOI2015",
        filter(duParZT, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
        "Agglomération nantaise") %>% print()
off()

# Autre approche : diff temps au travail par categ et lieu de rés / de travail ?

base = PER %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(PCS8, ZT) %>% summarise(DuTvlMed = weighted.median(DuTvl, weights = CoeffEnq)) %>%
  pivot_wider(names_from = PCS8, values_from = DuTvlMed, names_prefix = "DuTvl.") %>%
  mutate(Rap0305 = (DuTvl.03/DuTvl.05 - 1) * 100) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

ticks = c(-25, -10, 0, 10, 50)

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid) ; min = min(b$Rap0305, na.rm=T) ; max = max(b$Rap0305, na.rm=T)
  g = ggplot(data = b) +
    geom_sf(aes(fill = Rap0305), color = "white", size=.1) +
    scale_fill_gradient2(low = pal_PCS8[5], high = pal_PCS8[3], mid = "gainsboro", midpoint = 0, 
                         trans = trans_sur100, breaks = sort(c(min, ticks, max)),
                         labels = transf_echelle_sur100_lab(sort(c(min, ticks, max)), deuxLignes = F),
                         name = "différence de temps\nde présence au\nlieu de travail\ncadres/employé·es") +
    labs(title = "Présence des cadres et employé·es au lieu de travail",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(b), aes(size = DuTvl.03+DuTvl.05),
            shape=21, color = "gray20", alpha=.5) +
    scale_size(range = c(2,12), name = "temps d'activité\nhors domicile\n(total, en h)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base$uid_ENQ)
gs   = lapply(uids, proceed, shp = base)

sortie("Temps/Ecart temps de présence au lieu de travail PCS3 vs PCS5")
proceed("IDF2010",
        filter(base, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
        "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)") %>% print()
off()

# Diffs H/F au sein des ménages ?

base0 = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & Lien %in% c("1", "2")) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(rapport = case_when(Genre == "F" & Genre.conj == "H" ~ (DuTvl.conj/DuTvl - 1) * 100,
                             Genre == "H" & Genre.conj == "F" ~ (DuTvl/DuTvl.conj - 1) * 100)) %>%
  group_by(tout = T) %>% summarise(rapMed = median(rapport, na.rm=T),
                                   du = sum(DuTvl + DuTvl.conj, na.rm=T), n = n())

# Premier test à faire : par enquête
base0 = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & Lien %in% c("1", "2")) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(rapport = case_when(Genre == "F" & Genre.conj == "H" ~ (DuTvl.conj/DuTvl - 1) * 100,
                             Genre == "H" & Genre.conj == "F" ~ (DuTvl/DuTvl.conj - 1) * 100)) %>%
  group_by(ZoneDens) %>% summarise(rapMed = median(rapport, na.rm=T),
                                   du = sum(DuTvl + DuTvl.conj, na.rm=T), n = n())

base0 = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & Lien %in% c("1", "2")) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(rapport = case_when(Genre == "F" & Genre.conj == "H" ~ (DuTvl.conj/DuTvl - 1) * 100,
                             Genre == "H" & Genre.conj == "F" ~ (DuTvl/DuTvl.conj - 1) * 100)) %>%
  mutate(ZoneRang = etqZoneRang(ZoneRang)) %>%
  group_by(ZoneRang) %>% summarise(rapMed = median(rapport, na.rm=T),
                                   du = sum(DuTvl + DuTvl.conj, na.rm=T), n = n())

base1 = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & Lien %in% c("1", "2")) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(rapport = case_when(Genre == "F" & Genre.conj == "H" ~ (DuTvl.conj/DuTvl - 1) * 100,
                             Genre == "H" & Genre.conj == "F" ~ (DuTvl/DuTvl.conj - 1) * 100)) %>%
  group_by(uid_ENQ) %>% summarise(rapMed = weighted.median(rapport, w = CoeffEnq, na.rm=T),
                                  du = sum(DuTvl + DuTvl.conj, na.rm=T), n = n())

PER %>% group_by(uid_ENQ) %>%
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  summarise(pPop = sum(ifelse(PCS8 %in% c("05", "06"), CoeffEnq, 0), na.rm=T) / sum(CoeffEnq, na.rm=T)) %>%
  tab_Tri(parCol = "pPop")

base = PER_ff %>% filter(Lien %in% c("1", "2")) %>%
  mutate(LienConj = ifelse(Lien == "1", "2", "1")) %>%
  left_join(select(PER_ff, uid_MEN, Lien, DuTvl, DuEtu, DuDom, PCS8, Genre, NivEtu),
            by=c("uid_MEN" = "uid_MEN", "LienConj" = "Lien"),
            suffix = c("", ".conj")) %>%
  mutate(rapport = case_when(Genre == "F" & Genre.conj == "H" ~ (DuTvl.conj/DuTvl - 1) * 100,
                             Genre == "H" & Genre.conj == "F" ~ (DuTvl/DuTvl.conj - 1) * 100)) %>%
  group_by(ZT) %>%
  summarise(rapMed = median(rapport, na.rm=T),
                             du = sum(DuTvl + DuTvl.conj, na.rm=T), n = n())
#rename(ZT = ZT_travMax)

base = base %>% left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

ticks = transf_echelle_sur100_inverse(c(-2, -1, -.5, -.1, .1, .5, 1, 2))

base = filter(base, rapMed > ticks[1] & rapMed < ticks[length(ticks)])

proceed = function(uid, shp, sousTitre = NULL) {
  etendue = cartoEtendue(shp, enq=uid, proj = 2154, df = F, unExemplairePar = "ZT")
  b = filter(shp, uid_ENQ == uid)
  ticksInside = ticks[ticks > min(b$rapMed, na.rm=T) & ticks < max(b$rapMed, na.rm=T)]
  ticksInside = c(min(b$rapMed, na.rm=T), ticksInside, max(b$rapMed, na.rm=T))
  g = ggplot(data = b) +
    geom_sf(aes(fill = rapMed), color = "white", size=.1) +
    scale_fill_steps2(low = "brown", high = "plum", mid = "gainsboro", midpoint = 0, 
                      trans = trans_sur100, breaks = ticksInside,
                      labels = transf_echelle_sur100_lab,
                      name = "temps de présence\nmédian des hommes\n(vivant avec une femme)\nau lieu de travail\npar rapport à celui\nde leur conjointe") +
    labs(title = "Rapport entre la durée de présence au travail des hommes et femmes\nau sein des couples hétérosexuels en emploi",
         subtitle = ifelse(is.null(sousTitre), z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long, sousTitre),
         caption = src_fig(base = filter(shp, uid_ENQ == uid), carto=T))
  g = cartoHydro(g, etendue = etendue, proj = st_crs(shp)) +
    geom_sf(data = st_point_on_surface(b), aes(size = du),
            shape=21, color = "gray20", alpha=.5) +
    scale_size(range = c(2,12), name = "heures passées sur\nle lieu de travail\n(total, couples H/F)")
  g = cartoLib(g, etendue = etendue, detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendue)
  return(g)
}

uids = unique(base$uid_ENQ)
gs   = lapply(uids, proceed, shp = base)

gs[[length(gs)+1]] = proceed("IDF2010",
                             filter(base, uid_ENQ == "IDF2010", substr(ZT,8,9) %in% c("75","92","93","94")),
                             "Petite couronne (Paris, Hauts-de-Seine, Seine-Saint-Denis et Val-de-Marne)")

gs[[length(gs)+1]] =proceed("LOI2015",
                            filter(base, uid_ENQ == "LOI2015", substr(ZT,8,9) == "00"),
                            "Agglomération nantaise")

gs[[length(gs)+1]] =proceed("LYO2015",
                            filter(base, uid_ENQ == "LYO2015", substr(ZT,8,9) %in% c("01", "02")),
                            "Agglomération lyonnaise")

sortie("Temps/Atlas durée prez travail hommes et femmes", format = "pdf", taille = "a4")
print(gs)
off()

sortie("Temps/Carte rapport hf temps au lieu de travail - bas-rhin")
print(gs[[13]])
off()

sortie("Temps/Carte rapport hf temps au lieu de travail - calvados")
print(gs[[14]])
off()

# Volumes horaires par territoire ====

# ~ Densité horaire ====

# Principe : on compte dans chaque ZT le temps non-résidentiel. Dans un premier temps, on peut
# cartographier ça !

duParZT = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps = sum(du)/60)

pop = PER %>% filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  group_by(ZT) %>% summarise(pop = sum(CoeffEnq, na.rm=T))

popDuDom =ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(substr(Tache, 1,2) %in% c("01", "02")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(duDom = sum(du)/60)

duParZT = left_join(duParZT, shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  left_join(popDuDom, by="ZT")

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

# basemap = tibble(uid_ENQ = uids, geometry = etendues) %>% st_as_sf()

valIntervalleSur100 = 2
ticks = c(-80, -50, 0, 100, 200)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT, uid_ENQ == uid)) +
    geom_sf(aes(fill = (temps/duDom - 1) * 100), color = "white", size=.1) +
    scale_fill_gradient2(low = "green4", high = "goldenrod", mid = "gainsboro",
                         midpoint = 0,
                         trans = trans_sur100, breaks = ticks,
                         labels = transf_echelle_sur100_lab(ticks, deuxLignes = F),
                         name = "rapport heures d'ac.\n/temps à domicile") +
    labs(title = "Rapport temps d'activité / temps de résidence",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray", stroke = 1.5) +
    scale_size(range = c(2,12), name = "heures d'activité\nhors domicile")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

# Densité horaire /km²

duParZT = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps = sum(du)/60)

duParZT = left_join(duParZT, shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  mutate(surface = st_area(.) / 10^6)

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT, uid_ENQ == uid)) +
    geom_sf(aes(fill = temps/as.double(surface)), color = "white", size=.1) +
    scale_fill_gradient (low = "gainsboro", high = "goldenrod",
                         name = "heures d'activité\nhors domicile\npar km²", trans = "log10") +
    labs(title = "Densité de l'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray80", stroke = 1.5, alpha=.5) +
    scale_size(range = c(2,12), name = "heures d'activité\nhors domicile")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

# Densité horaire /km² /PCS

duParZT = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0 & PCS8 %in% c("03", "04", "05", "06")) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq)

duParZT.gr = duParZT %>%
  group_by(PCS8, ZT) %>% summarise(temps = sum(du)/60) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  ungroup() %>%
  mutate(surface = as.double(st_area(.) / 10^6)) %>%
  mutate(PCS8 = etqPCS8(PCS8))

duParZT = duParZT %>%
  group_by(ZT) %>% summarise() %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT.gr, uid_ENQ == uid)) +
    geom_sf(aes(fill = temps/surface), color = "white", size=.1) +
    scale_fill_gradient (low = "gainsboro", high = "goldenrod",
                         name = "heures d'activité\nhors domicile\npar km²", trans = "log10") +
    labs(title = "Densité de l'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT.gr, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray80", stroke = 1.5, alpha=.5) +
    scale_size(range = c(2,12), name = "heures d'activité\nhors domicile")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

# ~ Heures dues à travailleur⋅ses hors ZT ====
## 2e possibilité : caculer la part d'heures dues à des gens qui viennent hors ZT

duParZT = ACT %>%
  left_join(select(PER, ZT, uid_PER, DuTvl, DuEtu, DuDom), by="uid_PER") %>%
  rename(ZT.res = ZT) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  filter(ZT != ZT.res) %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(temps = sum(du)/60)

duParZTtot = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(ZT) %>% summarise(tempsTot = sum(du)/60)

duParZT = left_join(duParZT, shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  left_join(duParZTtot, by="ZT")

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT, uid_ENQ == uid)) +
    geom_sf(aes(fill = temps/tempsTot * 100), color = "white", size=.1) +
    scale_fill_gradient2(low = "green4", mid = "gainsboro", high = "darkgoldenrod",
                         midpoint = 50,
                         name = "rapport heures d'ac.\nnon-résident⋅es/résident⋅es\n(en %)") +
    labs(title = "Heures d'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid)))
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray", stroke = 1.5) +
    scale_size(range = c(2,12), name = "heures d'activité\nhors domicile")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

## 3e possibilité : caculer la part d'heures dues à des gens qui viennent hors ZT et par PCS8

duParZT = ACT %>%
  left_join(select(PER, ZT, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  rename(ZT.res = ZT) %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  filter(ZT != ZT.res) %>%
  mutate(du = du * CoeffEnq) 

duParZTtot = ACT %>%
  left_join(select(PER, uid_PER, DuTvl, DuEtu, DuDom, PCS8), by="uid_PER") %>%
  filter(DuTvl > 0 & DuEtu == 0 & DuDom > 0) %>%
  filter(PCS8 %in% c("03", "04", "05", "06")) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  filter(!substr(Tache, 1,2) %in% c("01", "02", "60", "70", "81", "82", "91", "99")) %>%
  ztzf(colZF = "l") %>%
  mutate(du = du * CoeffEnq) %>%
  group_by(PCS8, ZT) %>% summarise(tempsTot = sum(du)/60)

duParZT.gr = duParZT %>%
  group_by(PCS8, ZT) %>% summarise(temps = sum(du)/60) %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>%
  left_join(duParZTtot, by=c("PCS8", "ZT"))

duParZT = duParZT %>%
  group_by(ZT) %>% summarise() %>%
  left_join(shp_ZT, by="ZT") %>%
  filter(!st_is_empty(geometry)) %>%
  st_as_sf()

uids = unique(duParZT$uid_ENQ)
etendues = sapply(uids, cartoEtendue, shp = duParZT, proj = 2154, df=F)

gs = lapply(uids, function(uid)
{ 
  g = ggplot(data = filter(duParZT.gr, uid_ENQ == uid)) +
    geom_sf(aes(fill = temps/tempsTot * 100), color = "white", size=.1) +
    scale_fill_gradient2(low = "green4", mid = "gainsboro", high = "darkgoldenrod",
                         midpoint = 50,
                         name = "rapport heures d'ac.\nnon-résident⋅es/résident⋅es\n(en %)") +
    labs(title = "Heures d'activité hors domicile par secteur",
         subtitle = z_Nomenclature[z_Nomenclature$uid_ENQ == uid,]$Libelle_Long,
         caption = src_fig(base = filter(duParZT, uid_ENQ == uid))) +
    facet_wrap(~PCS8)
  g = cartoHydro(g, etendue = etendues[[uid]], proj = st_crs(duParZT)) +
    geom_sf(data = st_point_on_surface(filter(duParZT.gr, uid_ENQ == uid)), aes(size = temps),
            shape=21, color = "gray20", stroke = 1.5, alpha=.5) +
    scale_size(range = c(2,12), name = "heures d'activité\nhors domicile")
  g = cartoLib(g, etendue = etendues[[uid]], detail = 5, overrideEtendue = T)
  g = cartoFinish(g, etendue = etendues[[uid]])
  return(g)
} )

# Activités dans la journée =====
rapport("Analyse des activités", prim = T)

# ~ Analyse factorielle activités ====

sortie("Temps/Activités analyses factorielles", format = "pdf", taille = "a4")

nbs = c("1er", "2e", "3e", "4e", "5e", "6e", "7e", "8e", "9e", "10e")
etq = paste0(nbs, " décile densité")

acpActivites = PER_ff %>%
  mutate(Genre = etqGenre(Genre),
         PCS42S = etqPCS42S(PCS42S),
         ZoneDens = etqZoneDens(ZoneDens)) %>%
  mutate(decileDs = discretisation(dsDom, methode = "déciles")) |>
  mutate(decileDs = factor(decileDs, labels = etq)) |>
  rename(Domicile = DuDom,
         `Lieu d'emploi` = DuTvl,
         Commerce = DuCom,
         `Services\nnécessaires` = DuSvc,
         Loisirs = DuLsr,
         Accompagnement = DuTax,
         `Déplacement` = DuDep) %>%
  analyseFacto(colVar = c("Domicile", "Lieu d'emploi", "Commerce", "Services\nnécessaires",
                          "Loisirs", "Accompagnement", "Déplacement"),
               colSup = c("Genre", "PCS42S", "decileDs"),
               colUids = "uid_PER",
               scaleW = F, titre = "Durées consacrées aux activités hors domicile", sortieBrute = T)

library(factoextra)
summary(acpActivites)

ade4::s.corcircle(acpActivites$co, xax = 1, yax = 2, fullcircle = T, box=T)

g1 = fviz_pca_var(acpActivites, axes=c(1,2), repel = T) +
  labs(title = "ACP : activités hors domicile",
       subtitle = "Axes 1 et 2",
       caption = src_fig(PER_ff))
g2 = fviz_pca_var(acpActivites, axes=c(1,3), repel = T) +
  labs(title = "ACP : activités hors domicile",
       subtitle = "Axes 1 et 3",
       caption = src_fig(PER_ff))

g1 = PER_ff |>
  cbind(acpActivites$li) |>
  filter(!is.na(CoeffRecEnq)) |>
  group_by(Genre, PCS8) |>
  summarise(coords1 = weighted.mean(Axis1, CoeffRecEnq),
            coords2 = weighted.mean(Axis2, CoeffRecEnq)) |>
  mutate(lab = paste0(etqPCS8(PCS8, genre = Genre), " (", Genre, ")")) |>
  ggplot(aes(x = coords1, y = coords2)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(aes(colour = PCS8)) +
  geom_line(aes(group = PCS8), colour = "grey", linetype = 2) +
  geom_text(aes(label = lab), vjust = -1) +
  scale_colour_manual(values = pal_PCS8) +
  xlab("Axe 1") + ylab("Axe 2") +
  labs(title = "PCS et Genre dans l'analyse factorielle des temps d'activité",
       subtitle = "Variables supplémentaires",
       caption = src_fig(PER_ff)) +
  coord_cartesian(xlim = c(-.85, 1.5), ylim = c(-.25, .50)) +
  theme(legend.position = "none")

sortie("Temps/ACP Activités - Genre et PCS", format = "svg")
print(g1)
off()

sortie("Temps/ACP Activités - Cercle 1-2", taille = "man", h = 8, l = 8)
  print(g1) + theme_minimal(base_size = 9)
off()
sortie("Temps/ACP Activités - Cercle 1-3", taille = "man", h = 8, l = 8)
  print(g2) + theme_minimal(base_size = 9)
off()

nbs = c("1er", "2e", "3e", "4e")
etq = paste0(nbs, " quartile")

g1 = PER_ff |>
  cbind(acpActivites$li) |>
  filter(!is.na(CoeffRecEnq), !is.na(dsDom), PCS8 %in% c("03", "04", "05", "06")) |>
  mutate(decileDs = discretisation(dsDom, methode = "quartiles")) |>
  mutate(decileDs = factor(decileDs, labels = etq)) |>
  filter(!is.na(decileDs)) |>
  group_by(decileDs, PCS8) |>
  summarise(coords1 = weighted.mean(Axis1, CoeffRecEnq),
            coords2 = weighted.mean(Axis2, CoeffRecEnq)) |>
  ggplot(aes(x = coords1, y = coords2)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_line(aes(group = PCS8), colour = "grey") +
  geom_point(aes(shape = PCS8, colour = decileDs)) +
  xlab("Axe 1") + ylab("Axe 2") +
  labs(title = "PCS et densité secteur de résidence\ndans l'analyse factorielle des temps d'activité",
       subtitle = "Variables supplémentaires",
       caption = src_fig(PER_ff)) +
  coord_cartesian() +
  scale_shape_manual(values = c(15, 16, 17, 18),
                     labels = niv_PCS8[3:6],
                     name = "PCS") +
  scale_color_brewer(name = "Densité secteur\nde domicile",
                     palette = "RdYlGn", direction = -1)

sortie("Temps/ACP Activités - Dens et PCS")
  print(g1)
off()

acpActivites = PER %>%
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0 & DuNA == 0) %>%
  rename(Domicile = DuDom,
         `Lieu d'emploi` = DuTvl,
         Commerce = DuCom,
         `Services\nnécessaires` = DuSvc,
         Loisirs = DuLsr,
         Accompagnement = DuTax,
         `Déplacement` = DuDep) %>%
  analyseFacto(colVar = c("Domicile", "Lieu d'emploi", "Commerce", "Services\nnécessaires",
                          "Loisirs", "Accompagnement", "Déplacement"),
               colSup = c("Age5", "MenEnfants"),
               colUids = "uid_PER",
               scaleW = F, titre = "Durées consacrées aux activités hors domicile")

summary(acpActivites)
off()

PER_ff %>%
  group_by(PCS42S) %>%
  filter(!PCS42S %in% c("00", as.character(c(80:99)))) %>%
  mutate(PCS42S = etqPCS42S(PCS42S)) %>%
  summarise(moyTpsLsr = mean(DuLsr)) %>%
  tab_Tri("PCS42S", parCol = "moyTpsLsr", rev=T)

# Liste de PCS du secteur privé, utilisée comme population de référence dans la thèse
PCS_privé = c("23", "36", "46", "47", "48", "54", "55", "61", "66")

PER_trav = filter(PER, typoJo == "TRAV" & !Activ %in% c("31", "32", "33", "21", "22") &
                    PCS42S %in% PCS_privé)

# ~ Modèles LM temps au domicile ====

sortie("Temps/Temps passé au domicile, modèles exploratoires", format = "pdf", taille = "a4",
       portrait = T)

regressionLog(base = filter(PER_trav, DuDom>360),
              val = "DuDom", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
              poids="CoeffEnq", retirerZ = T, facteurDiv = 60, legVal = "temps au domicile (h)",
              titre = "Modélisation du temps passé au domicile (un jour travaillé)", unite = "h",
              imprDistrib = T) %>% summary()

PER %>%
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  ggplot(aes(x = DuDom)) + geom_density()

PER %>%
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  ggplot(aes(x = log(DuDom))) + geom_density()


PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "54")) %>%
  regression(val = "DuDom", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au domicile (régression linéaire)", unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "54")) %>%
  regression(val = "DuTvl", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)", unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuDom", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au domicile (régression linéaire)", unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS8 %in% c("02", "03", "04", "05", "06"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuTvl", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)", unite = "min", imprDistrib = T)

# Deux approches complémentaires :
# - comparer privé et public pour 3, 4 et 5
# - comparer 23, 36, 46~8, 54-55

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  mutate(secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public")) %>%
  filter(PCS8 %in% c("03", "04", "05"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuDom", formule = "PCS8 + secteur + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au domicile (régression linéaire)",
             refDescr = "Salarié·es uniquement, hors ouvrier·es",
             unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  mutate(secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public"),
         secteur = as.factor(secteur)) %>%
  filter(PCS8 %in% c("03", "04", "05"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuTvl", formule = "PCS8 + secteur + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)",
             refDescr = "Salarié·es uniquement, hors ouvrier·es",
             unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  mutate(secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public"),
         secteur = as.factor(secteur)) %>%
  filter(PCS8 %in% c("03", "04", "05"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuDom", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au domicile (régression linéaire)",
             refDescr = "Salarié·es uniquement, hors ouvrier·es",
             unite = "min", imprDistrib = T, colComparaison = "secteur", verbose=T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  mutate(secteur = case_when(PCS42S %in% c("36", "46", "47", "48", "54", "55", "56") ~ "privé",
                             PCS42S %in% c("32", "41", "51") ~ "public"),
         secteur = as.factor(secteur)) %>%
  filter(PCS8 %in% c("03", "04", "05"), Activ %in% c("10", "11", "12")) %>%
  mutate(PCS8 = relevel(PCS8, "05")) %>%
  regression(val = "DuTvl", formule = "PCS8 + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)",
             refDescr = "Salarié·es uniquement, hors ouvrier·es",
             unite = "min", imprDistrib = T, colComparaison = "secteur", verbose=T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% PCS_privé, Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "54")) %>%
  regression(val = "DuDom", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au domicile (régression linéaire)", unite = "min", imprDistrib = T)

PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% PCS_privé, Activ %in% c("10", "11", "12")) %>%
  mutate(PCS42S = relevel(PCS42S, "54")) %>%
  regression(val = "DuTvl", formule = "PCS42S + Activ + ZoneDens + ZoneRang + Genre",
             poids = "CoeffEnq", retirerZ = T, legVal = "temps au domicile (min)",
             titre = "Temps passé au lieu de travail (régression linéaire)", unite = "min", imprDistrib = T)

off()

# ~ Activités selon PCS ====

pop = PER_ff%>%
  group_by(PCS8) %>% summarise(pop = n())

sortie(nom = "Temps/Activités selon PCS", taille = "page", portrait = T)
ACT %>%
  filter(uid_PER %in% PER_ff$uid_PER) %>%
  filter(!substr(Tache,1,1) %in% c("0", "1", "8", "9")) %>%
  filter(!Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280")) %>%
  left_join(select(PER, uid_PER, PCS8), by="uid_PER") %>%
  mutate(TacheDom = etqMotifActiv_Dom(substr(Tache,1,1))) %>%
  mutate(Tache = etqMotifActiv(substr(Tache,1,2))) %>%
  mutate(PCS8 = etqPCS8(PCS8)) %>%
  group_by(PCS8, TacheDom, Tache, uid_PER) %>% summarise(n = n()) %>%
  group_by(PCS8, TacheDom, Tache) %>% summarise(n = n()) %>%
  left_join(mutate(pop, PCS8 = etqPCS8(PCS8)), by="PCS8") %>%
  mutate(p = n / pop * 100) %>%
  tab_Tri(i = "Tache", parCol = "p", rev=T) %>%
  ggplot(aes(x = Tache, y = p)) +
  geom_col(aes(fill = PCS8)) +
  facet_grid(TacheDom~PCS8, scales = "free", space = "free_y") +
  scale_fill_manual(name = "PCS", values = c(pal_PCS8[1:6])) +
  ylab("Part de la population (%)") + xlab("Activité") +
  coord_flip(ylim = c(0, 50)) +
  geom_text(aes(label = paste0(round(p, 1), " %")), hjust=-.25) +
  labs(title = "Activités pratiquées selon la PCS, hors travail",
       caption = src_fig(date = "février 2023")) +
  theme_bw() + theme(legend.position = "bottom")
off()

# Même chose, en distinguant hommes et femmes
pop = PER_ff |>
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8) %>% summarise(pop = n())

sortie(nom = "Distances/Activités selon PCS+Genre", taille = "carré", portrait = T)
ACT %>%
  filter(uid_PER %in% PER_ff$uid_PER) %>%
  filter(!substr(Tache,1,1) %in% c("0", "1", "8", "9")) %>%
  filter(!Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280", "440")) %>%
  left_join(select(PER, uid_PER, PCS8, Genre), by="uid_PER") %>%
  mutate(TacheDom = etqMotifActiv_Dom(substr(Tache,1,1))) %>%
  mutate(Tache = etqMotifActiv(substr(Tache,1,2))) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8, TacheDom, Tache, uid_PER) %>% summarise(n = n()) %>%
  group_by(Genre, PCS8, TacheDom, Tache) %>% summarise(n = n()) %>%
  left_join(mutate(pop, PCS8 = etqPCS8(PCS8)), by=c("PCS8" = "PCS8", "Genre" = "Genre")) %>%
  mutate(vjust = ifelse(Genre == "Homme", -.4, 1.4)) %>%
  mutate(p = n / pop * 100) %>%
  tab_Tri(i = "Tache", parCol = "p", rev=T) %>%
  ggplot(aes(x = Tache, y = p)) +
  geom_col(aes(fill = PCS8, group = Genre), position = "dodge") +
  facet_grid(TacheDom~PCS8, scales = "free", space = "free_y") +
  scale_fill_manual(name = "PCS", values = c(pal_PCS8[1:6]), breaks=NULL) +
  ylab("Part de la population (%)") + xlab("Activité") +
  coord_flip(ylim = c(0, 50)) +
  geom_text(aes(label = paste0(round(p, 1), " %"), color=Genre, vjust=vjust),
            hjust=-.1, size=2.8) +
  labs(title = "Activités pratiquées selon la PCS, hors travail",
       caption = src_fig(date = "février 2023")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
off()

sortie(nom = "Temps/Activités selon PCS+Genre, hors jo travail", taille = "page", portrait = T)
ACT %>%
  filter(uid_PER %in% filter(PER, typoJo == "CHOM", Age>15 & Age<70,
                             PCS8 %in% c("03", "04", "05", "06"))$uid_PER) %>%
  filter(!substr(Tache,1,1) %in% c("0", "1", "8", "9")) %>%
  filter(!Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280", "440")) %>%
  left_join(select(PER, uid_PER, PCS8, Genre), by="uid_PER") %>%
  mutate(TacheDom = etqMotifActiv_Dom(substr(Tache,1,1))) %>%
  mutate(Tache = etqMotifActiv(substr(Tache,1,2))) %>%
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) %>%
  group_by(Genre, PCS8, TacheDom, Tache, uid_PER) %>% summarise(n = n()) %>%
  group_by(Genre, PCS8, TacheDom, Tache) %>% summarise(n = n()) %>%
  left_join(mutate(pop, PCS8 = etqPCS8(PCS8)), by=c("PCS8" = "PCS8", "Genre" = "Genre")) %>%
  mutate(vjust = ifelse(Genre == "Homme", -.4, 1.4)) %>%
  mutate(p = n / pop * 100) %>%
  tab_Tri(i = "Tache", parCol = "p", rev=T) %>%
  ggplot(aes(x = Tache, y = p)) +
  geom_col(aes(fill = PCS8, group = Genre), position = "dodge") +
  facet_grid(TacheDom~PCS8, scales = "free", space = "free_y") +
  scale_fill_manual(name = "PCS", values = c(pal_PCS8[3:6]), breaks=NULL) +
  ylab("Part de la population (%)") + xlab("Activité") +
  coord_flip(ylim = c(0, 100)) +
  geom_text(aes(label = paste0(round(p, 1), " %"), color=Genre, vjust=vjust),
            hjust=-.25, size=2.8) +
  labs(title = "Activités pratiquées selon la PCS, hors travail",
       caption = src_fig(date = "février 2023")) +
  theme_bw() + theme(legend.position = "bottom")
off()

# Quelques tableaux à tester

test = ACT %>%
  filter(uid_PER %in% filter(PER, typoJo == "TRAV",
                             PCS8 %in% c("03", "04", "05", "06"))$uid_PER) %>%
  mutate(activ = ifelse(substr(Tache,1,1) %in% c("0", "1", "8", "9") |
                          Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280"),
                        0, 1)) %>%
  group_by(uid_PER) %>%
  summarise(nOui = sum(activ), nTot = n()) %>%
  mutate(pOui = nOui/nTot)
nrow(filter(test, pOui>0))/nrow(test)

ACT %>%
  filter(uid_PER %in% filter(PER, typoJo == "TRAV",
                             PCS8 %in% c("02", "03", "04", "05", "06"))$uid_PER) %>%
  left_join(select(PER, uid_PER, PCS8), by="uid_PER") %>%
  mutate(activ = ifelse(substr(Tache,1,1) %in% c("0", "1", "8", "9") |
                          Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280"),
                        0, 1)) %>%
  group_by(PCS8, uid_PER) %>%
  summarise(nOui = sum(activ), nTot = n()) %>%
  mutate(pOui = nOui/nTot) %>%
  group_by(PCS8) %>% summarise(activ = sum(ifelse(pOui>0,1,0))/n())

ACT %>%
  filter(uid_PER %in% filter(PER, typoJo == "TRAV",
                             PCS8 %in% c("02", "03", "04", "05", "06"))$uid_PER) %>%
  left_join(select(PER, uid_PER, Genre), by="uid_PER") %>%
  mutate(activ = ifelse(substr(Tache,1,1) %in% c("0", "1", "8", "9") |
                          Tache %in% c("2x0", "210", "220", "230", "240", "260", "270", "280"),
                        0, 1)) %>%
  group_by(Genre, uid_PER) %>%
  summarise(nOui = sum(activ), nTot = n()) %>%
  mutate(pOui = nOui/nTot) %>%
  group_by(Genre) %>% summarise(activ = sum(ifelse(pOui>0,1,0))/n())


# ~ Modèles logit selon PCS ==== 

PER$com30=NULL; PER$com31=NULL; PER$com32=NULL
PER$com33=NULL; PER$com34=NULL; PER$com35=NULL
PER$lsr51=NULL; PER$lsr52=NULL; PER$lsr53=NULL; PER$lsr54=NULL

detAct = ACT %>%
  group_by(uid_PER) %>%
  summarise(com30 = "300" %in% Tache,
            com31 = "310" %in% Tache,
            com32 = "320" %in% Tache,
            com33 = "330" %in% Tache,
            com34 = "340" %in% Tache,
            com35 = "350" %in% Tache,
            lsr51 = "510" %in% Tache,
            lsr52 = "52"  %in% substr(Tache,1,2),
            lsr53 = "530" %in% Tache,
            lsr54 = "540" %in% Tache)
#remove(ACT) ; gc()

g32t = PER %>%
  filter(!is.na(CoeffRecEnq)) |>
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(10:67)), Activ %in% c("10", "11")) %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T)) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com32", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Supermarché, jour de travail", # poids = "CoeffRecEnq",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g32c = PER %>%
  filter(!is.na(CoeffRecEnq)) |>
  filter(DuTvl == 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(10:67)), Activ %in% c("10", "11")) %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T)) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com32", formule = "Genre + PCS8 + dsDomEtq + Age10",
        titre = "Supermarché, jour chômé", # poids = "CoeffRecEnq",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g33t = PER %>%
  filter(!is.na(CoeffRecEnq)) |>
  filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(10:67)), Activ %in% c("10", "11")) %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T)) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com33", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Petit commerce, jour de travail", # poids = "CoeffRecEnq",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g33c = PER %>%
  filter(!is.na(CoeffRecEnq)) |>
  filter(DuTvl == 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(10:67)), Activ %in% c("10", "11")) %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T)) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com33", formule = "Genre + PCS8 + dsDomEtq + Age10",
        titre = "Petit commerce, jour chômé", # poids = "CoeffRecEnq",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

page0 = cowplot::plot_grid(g32t + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g32c + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g33t + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g33c + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           axis = "b", nrow = 2, ncol = 2)

page = cowplot::plot_grid(page0, ggpubr::get_legend(g32t), nrow=1, ncol=2, rel_widths=c(.9, .1))
page = cowplot::plot_grid(page,  ggpubr::text_grob(src_fig(emp=F), x=.98, y=0.5, hjust=1, vjust=.5, size=10),
                          nrow=2, ncol=1, rel_heights=c(.95, .05))
page = viz_Titre(page, "Modèles logit portant sur la probabilité de se rendre dans un commerce", rel_heights = c(5, 95))

sortie("Temps/Commerces", taille = "man", h = 21, l = 18)
print(page)
off()

# Carte des résidus ?

mdl32 = PER_ff |>
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T)) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com32", formule = "Genre + PCS8 + Age10 + DuTvlHr",
        titre = "Supermarché, jour de travail",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75,
        returnFig = F)
PER_ff$residuals_mdl32 = mdl32$residuals
remove(mdl32)

load("Data/shp_ZT.rds")
load("Data/fdCarte.rds")

carte_resid = PER_ff |>
  filter(!is.na(CoeffRecEnq)) |>
  group_by(ZT) |>
  summarise(resid = weighted.mean(residuals_mdl32, CoeffRecEnq)) |>
  left_join(shp_ZT, by="ZT") |>
  st_as_sf() |>
  ggCarteZT(uid = "LOI2015", var = "resid",
            descr_leg = "Résidus du modèle Centres commerciaux", lims = c(-5,5))

sortie("Temps/Restos", taille = "page", portrait = T)

g32t = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com32", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl",
        titre = "Supermarché, jour de travail",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g32c = PER %>% filter(DuTvl == 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com32", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10",
        titre = "Supermarché, jour chômé",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g33t = PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com33", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl",
        titre = "Petit commerce, jour de travail",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

g33c = PER %>% filter(DuTvl == 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com33", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl",
        titre = "Petit commerce, jour chômé",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .75)

page0 = cowplot::plot_grid(g32t + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g32c + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g33t + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g33c + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           axis = "b", nrow = 2, ncol = 2)

page = cowplot::plot_grid(page0, ggpubr::get_legend(g32t), nrow=1, ncol=2, rel_widths=c(.8, .2))
page = cowplot::plot_grid(page,  ggpubr::text_grob(src_fig(), x=.98, y=0.5, hjust=1, vjust=.5, size=10),
                          nrow=2, ncol=1, rel_heights=c(.95, .05))
page = viz_Titre(page, "Modèles logit portant sur la probabilité de se rendre dans un commerce")
print(page)
off()

sortie("Temps/Commerces - Logit marché", portrait = T, taille="page")
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "com34", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl + DuDep",
        titre = "Modèle logit : fréquentation d'un marché",
        legVal = "Fréquentation d'un supermarché", valIntervalleSur100 = .5)
off()


iSur100 = 1.5

g51 = PER_ff %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr51", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Sport, Culture, Associatif",
        valIntervalleSur100 = iSur100, petit = T)

g52 = PER_ff %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr52", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Promenade",
        valIntervalleSur100 = iSur100, petit = T)

g53 = PER_ff %>% 
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr53", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Restaurant",
        valIntervalleSur100 = iSur100, petit = T)

g54 = PER_ff %>%
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr54", formule = "Genre + PCS8 + dsDomEtq + Age10 + DuTvlHr",
        titre = "Visite",
        valIntervalleSur100 = iSur100, petit = T)

page0 = cowplot::plot_grid(g51 + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g52 + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g53 + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           g54 + theme(legend.position = "none") + labs(caption = NULL) + xlab(NULL),
                           axis = "b", nrow = 2, ncol = 2)

page = cowplot::plot_grid(page0, ggpubr::get_legend(g32t), nrow=1, ncol=2, rel_widths=c(.9, .1))
page = cowplot::plot_grid(page,  ggpubr::text_grob(src_fig(emp=F), x=.98, y=0.5, hjust=1, vjust=.5, size=10),
                          nrow=2, ncol=1, rel_heights=c(.95, .05))
page = viz_Titre(page, ml("Modèles logit portant sur la probabilité de se déplacer pour loisirs",
                          "un jour de travail"), rel_heights = c(.05, .95))

sortie("Temps/Loisirs", taille = "man", h = 21.5, l = 17)
print(page)
off()

sortie("Temps/Loisirs - Logit asso, sportif, culturel", portrait = T)
PER_ff |>
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  mutate(PCS42S = relevel(PCS42S, "46")) |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr51", formule = "Genre + PCS42S + dsDomEtq + Age10 + DuTvlHr",
        titre = "Modèle logit : fréquentation d'un lieu de loisir asso/sportif/culturel",
        legVal = "", valIntervalleSur100 = 1)
off()

PER_ff |>
  mutate(DuTvlHr = DuTvl/60, DuDep = DuDep/60) %>%
  valref() |>
  mutate(PCS42S = relevel(PCS42S, "51")) |>
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr53", formule = "Genre + PCS42S + dsTvlEtq + Age10 + DuTvlHr",
        titre = "Modèle logit : fréquentation d'un lieu de loisir asso/sportif/culturel",
        legVal = "", valIntervalleSur100 = 1)


sortie("Temps/Loisirs - Logit Promenades", portrait = T, taille="page")
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr52", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl + DuDep",
        titre = "Modèle logit : promenades",
        legVal = "", valIntervalleSur100 = 1)
off()

sortie("Temps/Loisirs - Logit Restaurant", portrait = T, taille="page")
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr53", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl + DuDep",
        titre = "Modèle logit : restaurants",
        legVal = "", valIntervalleSur100 = 1)
off()

sortie("Temps/Loisirs - Logit visites", portrait = T, taille="page")
PER %>% filter(DuTvl > 0 & DuDom > 0 & DuEtu == 0) %>%
  filter(PCS42S %in% as.character(c(21:67)), Activ %in% c("10", "11", "12")) %>%
  mutate(DuTvl = DuTvl/60, DuDep = DuDep/60) %>%
  mutate(PCS42S = relevel(PCS42S, "41"),
         Age10 = etqAge(Age, pas = 10, min = 16, max = 69, forceMin = T),
         Age10 = relevel(Age10, "30 à 39 ans"),
         ZoneRang = relevel(ZoneRang, "4")) %>%
  left_join(detAct, by="uid_PER") %>%
  logit(val = "lsr54", formule = "Genre + PCS42S + ZoneDens + ZoneRang + Age10 + DuTvl + DuDep",
        titre = "Modèle logit : visites",
        valIntervalleSur100 = 1)
off()