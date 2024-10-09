# Quelques scripts généraux utilisés pour des figures dans la thèse
# Été 2024 - Maxime Guinepain

source("START.R")
initMémoire(BasesCharger = c("MEN", "PER"))

# Petite carte du nombre d'enquêté⋅es par département ====

PER_Dep = PER |>
  mutate(dep = case_when(substr(Com, 1, 2) == "97" ~ substr(Com, 1, 3),
                         substr(Com, 1, 2) != "97" ~ substr(Com, 1, 2))) |>
  group_by(dep) |> summarise(pop = sum(CoeffRecEnq), n = n())

coms_pop = read_delim("Sources/base-cc-evol-struct-pop-2020.CSV", delim = ";",
                      locale = locale(encoding = "Windows-1252"))
coms_pop = select(coms_pop, CODGEO, P14_POP)

deps_pop = coms_pop |>
  mutate(dep = case_when(substr(CODGEO, 1, 2) == "97" ~ substr(CODGEO, 1, 3),
                         substr(CODGEO, 1, 2) != "97" ~ substr(CODGEO, 1, 2))) |>
  group_by(dep) |> summarise(P14_POP = sum(P14_POP, na.rm=T))

PER_Dep = left_join(PER_Dep, deps_pop, by = "dep")

PER_Dep = PER_Dep |> mutate(pP14 = P14_POP / sum(P14_POP, na.rm=T),
                      pPop = n / sum(n, na.rm=T),
                      rapport = pPop/pP14,
                      rapportVecto = case_when(rapport < .1 ~ "Dépt. très peu représenté (<10%)",
                                               rapport < .8 ~ "Dépt. sous-représenté (<80%)",
                                               rapport >= .8 & rapport <= 1.2 ~ "Représentation équivalente",
                                               rapport > 1.2 ~ "Dépt. sur-représenté (>120%)"))

echelleCol =  scale_colour_manual(breaks = c("Dépt. très peu représenté (<10%)",
                                             "Dépt. sous-représenté (<80%)",
                                             "Représentation équivalente",
                                             "Dépt. sur-représenté (>120%)"),
                                  values = c("slateblue",
                                             "cyan2",
                                             "green",
                                             "orange"),
                                  name = "Part des enquêté⋅es par rapport\nà la part de la population")

g = viz_France(base = PER_Dep,
           champAbs = "n",
           champRel = "rapportVecto",
           echelleAbs = scale_size(breaks = c(100, 1000, 10000, 40000),
                                   name = "Nombre enquêté⋅es\n(base unifiée Cerema + EMP)"),
           echelleRel = echelleCol)

sortie("Enquêté⋅es par département")
  print(viz_Titre(g, "Répartition des enquêté⋅es par département"))
off()

coms = read_sf("Sources/Mailles/communes-20210101.shp")
coms$dep = substr(coms$insee, 1, 2)
coms$dep = ifelse(coms$dep == "97", substr(coms$insee, 1, 3), coms$dep)



coms = left_join(coms, coms_pop, by = c("insee" = "CODGEO")) |> st_transform(crs = 2154)

deps = coms |> st_simplify(preserveTopology = T, dTolerance = 100) |>
  st_buffer(dist = 100) |>
  group_by(dep) |> summarise(P14_POP = sum(P14_POP, na.rm=T)) |> st_transform(crs = 3857)

deps = left_join(deps, PER_Dep, by = "dep")



reg_points = deps |> maillageDepVersReg(champDep = "dep") |> st_centroid()

dep_points = st_centroid(deps)
dep_points = filter(dep_points, !is.na(rapport))

deps_hex = deps |> group_by(dep) |> summarise()

g=ggplot(data = dep_points) +
  geom_sf(data = deps_hex, fill = "grey95", colour = "grey80") +
  geom_sf(aes(size = n, colour = rapportVecto), alpha=.8) +
  



g = g + echelleCol

g = g + coord_sf(xlim = c(-550000,1100000), ylim = c(5000000,6600000))

# ggplot(data = reg_points) +
#   geom_sf(data = maillageDepVersReg(deps_hex, champDep = "dep"), fill = "grey95", colour = "grey80") +
#   geom_sf(aes(size = n), alpha = .8) +
#   scale_size(breaks = c(100, 1000, 10000, 40000), name = "Nombre enquêté⋅es") +
#   coord_sf(xlim = c(-550000,1100000), ylim = c(5000000,6600000))

g = g + ggRetirerAxeX + ggRetirerAxeY

# Tentative : calculer la bbox de la guyane
# et appliquer les mêmes valeurs autour des points centraux marti/réu
bbox973 = st_bbox(filter(deps, dep == "973"))

ctr972 = st_centroid(filter(deps, dep == "972")) |> st_bbox()
ctr974 = st_centroid(filter(deps, dep == "974")) |> st_bbox()

xinterv = as.double(bbox973$xmax - bbox973$xmin)
yinterv = as.double(bbox973$ymax - bbox973$ymin)

pts972 = data.frame(
  lon = c(ctr972$xmax - (xinterv / 2), ctr972$xmax + (xinterv / 2), ctr972$xmax + (xinterv / 2), ctr972$xmax - (xinterv / 2)) |> as.double(),
  lat = c(ctr972$ymax + (yinterv / 2), ctr972$ymax + (yinterv / 2), ctr972$ymax - (yinterv / 2), ctr972$ymax - (yinterv / 2)) |> as.double()
)
bbox972 = st_as_sf(pts972, coords = c("lon", "lat"), crs = st_crs(bbox973)) |>
  summarise(geometry = st_combine(geometry)) |> st_cast("POLYGON") 
box972 = st_bbox(bbox972)

pts974 = data.frame(
  lon = c(ctr974$xmax - (xinterv / 2), ctr974$xmax + (xinterv / 2), ctr974$xmax + (xinterv / 2), ctr974$xmax - (xinterv / 2)) |> as.double(),
  lat = c(ctr974$ymax + (yinterv / 2), ctr974$ymax + (yinterv / 2), ctr974$ymax - (yinterv / 2), ctr974$ymax - (yinterv / 2)) |> as.double()
)
bbox974 = st_as_sf(pts974, coords = c("lon", "lat"), crs = st_crs(bbox973)) |>
  summarise(geometry = st_combine(geometry)) |> st_cast("POLYGON")
box974 = st_bbox(bbox974)

g972 = g + coord_sf(xlim = c(box972$xmin, box972$xmax), ylim = c(box972$ymin, box972$ymax))
g973 = g + coord_sf(xlim = c(box973$xmin, box973$xmax), ylim = c(box973$ymin, box973$ymax))
g974 = g + coord_sf(xlim = c(box974$xmin, box974$xmax), ylim = c(box974$ymin, box974$ymax))

g972 = g972 + theme(axis.text = element_blank(), axis.ticks = element_blank())
g973 = g973 + theme(axis.text = element_blank(), axis.ticks = element_blank())
g974 = g974 + theme(axis.text = element_blank(), axis.ticks = element_blank())

g972 = g972 + echelleCol
g973 = g973 + echelleCol
g974 = g974 + echelleCol

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

g = g + ggspatial::annotation_scale(location="bl", bar_cols = c("gray60", "gray95"),
                                line_col = "gray70", height = unit(.1, "cm"))

gg = cowplot::plot_grid(gOM, g, nrow = 1, rel_widths = c(1,5))

gg = viz_Titre(gg, "Répartition des enquêté⋅es par département")
gg = viz_Pied(gg, src_fig(bu = T, emp = T, carto = T, date = "août 2024"))

sortie("Enquêté⋅es par département")
  print(gg)
off()

# Part des trajets renseignés ===================================================

initMémoire(BasesCharger = c("PER", "DEP", "TRJ"))

PER_ff = init_PER_ff(PER)

trjOk = DEP |>
  mutate(TRJ_ok = uid_DEP %in% TRJ$uid_DEP) |>
  left_join(select(PER, uid_PER, CoeffEnq), by="uid_PER") |>
  rename(poids = CoeffEnq) |>
  mutate(poidsSiTrj = ifelse(TRJ_ok, poids, 0)) |>
  group_by(uid_ENQ) |> summarise(n_trjOk = sum(poidsSiTrj, na.rm=T),
                                 n_tout  = sum(poids, na.rm=T)) |>
  mutate(pTrjOk = n_trjOk/n_tout)

# Part des PCS ==================================================================

rec14 = read_delim("Sources/base-cc-evol-struct-pop-2020.CSV", delim = ";")

# On va structurer pour obtenir la part des PCS

rec14 |>
  select("CODGEO", paste0("C14_POP15P_CS", c(1:6))) |>
  pivot_longer(cols = paste0("C14_POP15P_CS", c(1:6)),
               names_to = "PCS8", values_to = "N") |>
  group_by(PCS8) |> summarise(N = sum(N, na.rm=T)) |>
  mutate(p = N/sum(N))

PER |>
  filter(Age>15) |> filter(PCS8 %in% paste0("0", c(1:6))) |>
  filter(Activ != "32") |>
  group_by(PCS8) |> summarise(n = n()) |>
  mutate(p = n / sum(n))

PER |>
  filter(!is.na(CoeffRecEnq)) |>
  filter(Age>15) |> filter(PCS8 %in% paste0("0", c(1:6))) |>
  filter(Activ != "32") |>
  group_by(PCS8) |> summarise(n = sum(CoeffRecEnq)) |>
  mutate(p = n / sum(n))

# Caractéristiques des zones ====================================================

codesZF = c("LOI2015 000031010", "LOI2015 000031003",
            "LOI2015 000038012", "LOI2015 000039009",
            "LOI2015 000039011", "LOI2015 000039008",
            "LOI2015 000038011", "LOI2015 000039010")

sum(PER$CoeffRecEnq, na.rm=T)
sum(filter(PER, uid_ENQ == "LOI2015")$CoeffRecEnq, na.rm=T)
sum(filter(PER, ZT == "LOI20150031")$CoeffRecEnq, na.rm=T)
sum(filter(PER, ZT == "LOI20150039")$CoeffRecEnq, na.rm=T)
sum(filter(PER, ZT == "LOI20150038")$CoeffRecEnq, na.rm=T)
sum(filter(PER, ZF %in% codesZF)$CoeffRecEnq, na.rm=T)

sum(PER_ff$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, uid_ENQ == "LOI2015")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT == "LOI20150031")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT == "LOI20150039")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT == "LOI20150038")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZF %in% codesZF)$CoeffRecEnq, na.rm=T)

sum(filter(PER_ff, !is.na(ZT_travMax))$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq, na.rm=T)
sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq, na.rm=T)

nrow(filter(PER_ff, !is.na(ZT_travMax)))
nrow(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015"))
nrow(filter(PER_ff, ZT_travMax == "LOI20150031"))
nrow(filter(PER_ff, ZT_travMax == "LOI20150039"))
nrow(filter(PER_ff, ZT_travMax == "LOI20150038"))
nrow(filter(PER_ff, ZF_travMax %in% codesZF))

weighted.mean(filter(PER_ff, !is.na(ZT_travMax) & !is.na(CoeffRecEnq))$Age,
              filter(PER_ff, !is.na(ZT_travMax) & !is.na(CoeffRecEnq))$CoeffRecEnq)
weighted.mean(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$Age,
              filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
weighted.mean(filter(PER_ff, ZT_travMax == "LOI20150031")$Age,
              filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
weighted.mean(filter(PER_ff, ZT_travMax == "LOI20150039")$Age,
              filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
weighted.mean(filter(PER_ff, ZT_travMax == "LOI20150038")$Age,
              filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
weighted.mean(filter(PER_ff, ZF_travMax %in% codesZF)$Age,
              filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), Genre == "F" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & Genre == "F")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & Genre == "F")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & Genre == "F")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & Genre == "F")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & Genre == "F")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), PCS8 == "02" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & PCS8 == "02")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & PCS8 == "02")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & PCS8 == "02")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & PCS8 == "02")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & PCS8 == "02")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), PCS8 == "03" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & PCS8 == "03")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & PCS8 == "03")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & PCS8 == "03")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & PCS8 == "03")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & PCS8 == "03")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), PCS8 == "05" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & PCS8 == "05")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & PCS8 == "05")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & PCS8 == "05")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & PCS8 == "05")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & PCS8 == "05")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), PCS8 == "04" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & PCS8 == "04")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & PCS8 == "04")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & PCS8 == "04")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & PCS8 == "04")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & PCS8 == "04")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

PER_ff$NivDip = NivEtuVersNivDip(PER_ff$NivEtu)

sum(filter(PER_ff, !is.na(ZT_travMax), NivDip == "4" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & NivDip == "4")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & NivDip == "4")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & NivDip == "4")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & NivDip == "4")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & NivDip == "4")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

sum(filter(PER_ff, !is.na(ZT_travMax), PCS8 == "06" & !is.na(CoeffRecEnq))$CoeffRecEnq) /
  sum(filter(PER_ff, !is.na(ZT_travMax), !is.na(CoeffRecEnq))$CoeffRecEnq)
sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015" & PCS8 == "06")$CoeffRecEnq) /
  sum(filter(PER_ff, substr(ZT_travMax, 1, 7) == "LOI2015")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150031" & PCS8 == "06")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150031")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150039" & PCS8 == "06")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150039")$CoeffRecEnq)
sum(filter(PER_ff, ZT_travMax == "LOI20150038" & PCS8 == "06")$CoeffRecEnq) /
  sum(filter(PER_ff, ZT_travMax == "LOI20150038")$CoeffRecEnq)
sum(filter(PER_ff, ZF_travMax %in% codesZF & PCS8 == "06")$CoeffRecEnq) /
  sum(filter(PER_ff, ZF_travMax %in% codesZF)$CoeffRecEnq)

# Carte des secteurs
load("Data/shp_ZT.rds")
load("Data/fdCarte.rds")

s44 = filter(shp_ZT, uid_ENQ == "LOI2015")
bbox = filter(shp_ZT,
                substr(ZT, 1, 10) == "LOI2015000" |
                substr(ZT, 1, 10) == "LOI2015001" |
                substr(ZT, 1, 10) == "LOI2015002" |
                substr(ZT, 1, 10) == "LOI2015003")

s44$terrain = ifelse(s44$ZT %in% c("LOI20150031", "LOI20150038", "LOI20150039"),
                     "Terrain", "Autres secteurs")

load("Data/shp_ZF.rds")
s44zf = filter(shp_ZF, uid_ENQ == "LOI2015")
s44zf$terrain = ifelse(s44zf$CODE_ZF %in% codesZF, "Terrain", "Autres zones")

g = ggplot(s44) +
  geom_sf(data = s44zf, aes(fill = terrain, colour = "ZF")) +
  scale_fill_manual(values = c("white", "gold"),
                    name = "Terrain (2022)",
                    labels = c("ZF non enquêtées",
                               "ZF enquêtées lors\ndu terrain")) 
g = cartoHydro(g, etendue = bbox) +
  geom_sf(data = s44zf, fill="transparent", colour = "grey75") +
  geom_sf(aes(colour = "ST"), fill = NA) +
  scale_colour_manual(values = c("grey35", "grey75"),
                      name = "Secteurs et zones",
                      labels = c("Secteurs de\ntirage",
                                 "Zones fines"))
g = cartoLib(g, etendue = bbox, detail = 8)
g = cartoFinish(g, etendue = bbox)
g = g + labs(title = "Localisation des zones enquêtées",
             caption = src_fig(bu = F, emp = F, carto = T, date = "août 2024"))

sortie("Carte zones enquêtées")
  print(g)
off()
