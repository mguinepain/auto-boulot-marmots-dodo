# Script Télétravail (pour l'appoint au chapitre 5)
# Rassemblé le 30 juillet 2024, VLM

# setwd("Données/EMD")

source("START.R")
initMémoire(BasesCharger = c("MEN", "PER"))
dir.create("Sorties/Télétravail")

PER = densitesZversPER(PER)

# Absence de déplacement ====

rapport("Statistiques sur l'absence de déplacement", prim = T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"))$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), Dis == 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"))$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Dis == 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Dis == 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"))$CoeffRecEnq, na.rm=T)


sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuCom > 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuSvc > 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuLsr > 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0)$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Dis == 0 & Genre == "F")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0, Genre == "F")$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Dis == 0 & Genre == "H")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0, Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuCom > 0 & Genre == "F")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "F")$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuCom > 0 & Genre == "H")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "F")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "F")$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "H")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuLsr > 0 & Genre == "F")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "F")$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuLsr > 0 & Genre == "H")$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "H")$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "F" & MenEnfants == T)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "F" & MenEnfants == T)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "F" & MenEnfants == F)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "F" & MenEnfants == F)$CoeffRecEnq, na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "H" & MenEnfants == T)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "H" & MenEnfants == T)$CoeffRecEnq, na.rm=T)
sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & DuTax > 0 & Genre == "H" & MenEnfants == F)$CoeffRecEnq, na.rm=T) / sum(filter(PER, Activ %in% c("10", "11", "12"), DuTvl == 0 & Genre == "H" & MenEnfants == F)$CoeffRecEnq, na.rm=T)


PER |>
  filter(Activ %in% c("10", "11", "12"), DuTvl == 0) |>
  mutate()


sortie("Télétravail/Part absent-es", taille = "mini")
comprPCSNonDep = PER %>% filter(uid_ENQ != "EMP2019") |>
  mutate(pop = CoeffRecEnqSansEMP) %>% filter(Activ %in% c("10", "11")) %>%
  filter(PCS8 %in% c("00", "01", "02", "03", "04", "05", "06")) %>%
  pivot_wider(names_from = "PCS8", values_from = "CoeffRecEnqSansEMP", names_prefix = "pcs__",
              names_sort = T) %>%
  group_by(VeilleDepl) %>% summarise(across(starts_with("pcs__"), sum, na.rm=T),
                                     total = sum(pop, na.rm=T)) %>%
  pivot_longer(cols = starts_with("pcs__"),
                               names_to = "PCS", values_to = "n") |>
  select(-total) |>
  pivot_wider(values_from = "n", names_from = "VeilleDepl",
              names_prefix = "veilleDepl") |>
  mutate(p = veilleDepl2 / (veilleDepl1 + veilleDepl2) * 100) |>
  filter(PCS != "pcs__00") |>
  tab_Tri(parCol = "p", rev = T) |>
  mutate(PCS = etqPCS8(substr(PCS, 6, 7), num = F)) |>
  ggplot(aes(x = PCS, y = p)) + geom_col() + coord_flip() +
  xlab("PCS individuelle") +
  ylab("Part (%)") +
  labs(title = ml("Part des actif⋅ves ne s'étant pas déplacé⋅es",
                  "la veille du jour d'enquête"),
       caption = src_fig(emp = F))
print(comprPCSNonDep)
off()


# Essayons de tracer ce schéma par PCS des deux membres... :P

g1 = PER |>
  filter(Activ %in% c("10", "11"), !is.na(PCS8), uid_ENQ != "EMP2019") |>
  mutate(poidsH = ifelse(Lien %in% c("1", "2") & Genre == "H", CoeffRecEnqSansEMP, 0),
         poidsF = ifelse(Lien %in% c("1", "2") & Genre == "F", CoeffRecEnqSansEMP, 0),
         immoH  = ifelse(Lien %in% c("1", "2") & Genre == "H" & VeilleDepl == "2", CoeffRecEnqSansEMP, 0),
         immoF  = ifelse(Lien %in% c("1", "2") & Genre == "F" & VeilleDepl == "2", CoeffRecEnqSansEMP, 0),
         pcsH =   ifelse(Lien %in% c("1", "2") & Genre == "H", as.character(etqPCS8(PCS8, genre = "H")), NA),
         pcsF =   ifelse(Lien %in% c("1", "2") & Genre == "F", as.character(etqPCS8(PCS8, genre = "F")), NA),
         activH = ifelse(Lien %in% c("1", "2") & Genre == "H", as.character(Activ), NA),
         activF = ifelse(Lien %in% c("1", "2") & Genre == "F", as.character(Activ), NA),
         nH =     ifelse(Lien %in% c("1", "2") & Genre == "H", 1, 0),
         nF =     ifelse(Lien %in% c("1", "2") & Genre == "F", 1, 0)) |>
  group_by(uid_MEN) |>
  summarise(nH = sum(nH), nF = sum(nF),
            poidsH = sum(poidsH), poidsF = sum(poidsF),
            immoH = sum(immoH), immoF = sum(immoF),
            pcsH = as.factor(first(na.omit(as.character(pcsH)))),
            pcsF = as.factor(first(na.omit(as.character(pcsF)))),
            activH = as.factor(first(na.omit(as.character(activH)))),
            activF = as.factor(first(na.omit(as.character(activF))))) |>
  filter(nF < 2 & nH < 2) |> # on ne prend que les couples hétéro
  filter(activH %in% c("10","11"), activF %in% c("10","11")) |> # ménages biactifs
  filter(pcsF != "En études", pcsH != "En études",
         pcsF != "PCS inconnue", pcsH != "PCS inconnue") |>
  group_by(pcsH, pcsF) |> summarise(pImmoH = sum(immoH, na.rm=T) / sum(poidsH, na.rm=T) * 100,
                                    pImmoF = sum(immoF, na.rm=T) / sum(poidsF, na.rm=T) * 100) |>
  pivot_longer(cols = starts_with("pImmo"), names_to = "Genre", values_to = "pImmo") |>
  mutate(Genre = substr(Genre, 6,6))  |>
  ggplot(aes(x = pcsH, y = pcsF, size= pImmo)) +
  geom_count(aes(hjust = Genre, colour = Genre), shape = 21, alpha = .5) +
  scale_size(range = c(1, 10), guide = "bins") +
  labs(title = "Journées sans déplacement pour le travail selon le genre des conjoint⋅es",
       subtitle = "parmi les couples biactifs homme-femme",
       caption = src_fig(emp = F)) +
  xlab("PCS homme") + ylab("PCS femme") +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))


g2 = PER |>
  filter(!is.na(PCS8), uid_ENQ != "EMP2019") |>
  mutate(poidsH = ifelse(Lien %in% c("1", "2") & Genre == "H", CoeffRecEnqSansEMP, 0),
         poidsF = ifelse(Lien %in% c("1", "2") & Genre == "F", CoeffRecEnqSansEMP, 0),
         immoH  = ifelse(Lien %in% c("1", "2") & Genre == "H" & VeilleDepl == "2", CoeffRecEnqSansEMP, 0),
         immoF  = ifelse(Lien %in% c("1", "2") & Genre == "F" & VeilleDepl == "2", CoeffRecEnqSansEMP, 0),
         pcsH =   ifelse(Lien %in% c("1", "2") & Genre == "H", as.character(etqPCS8(PCS8, genre = "H")), NA),
         pcsF =   ifelse(Lien %in% c("1", "2") & Genre == "F", as.character(etqPCS8(PCS8, genre = "F")), NA),
         activH = ifelse(Lien %in% c("1", "2") & Genre == "H", as.character(Activ), NA),
         activF = ifelse(Lien %in% c("1", "2") & Genre == "F", as.character(Activ), NA),
         nH =     ifelse(Lien %in% c("1", "2") & Genre == "H", 1, 0),
         nF =     ifelse(Lien %in% c("1", "2") & Genre == "F", 1, 0)) |>
  group_by(uid_MEN) |>
  summarise(nH = sum(nH), nF = sum(nF),
            poidsH = sum(poidsH), poidsF = sum(poidsF),
            immoH = sum(immoH), immoF = sum(immoF),
            pcsH = as.factor(first(na.omit(as.character(pcsH)))),
            pcsF = as.factor(first(na.omit(as.character(pcsF)))),
            activH = as.factor(first(na.omit(as.character(activH)))),
            activF = as.factor(first(na.omit(as.character(activF))))) |>
  filter(nF < 2 & nH < 2) |> # on ne prend que les couples hétéro
  filter(activH %in% c("10","11", "12") | activF %in% c("10", "11", "12")) |> # ménages biactifs
  filter(pcsF != "PCS inconnue", pcsH != "PCS inconnue") |>
  mutate(pcsH = ifelse(activH %in% c("21", "22"), "En études", as.character(pcsH)),
         pcsH = ifelse(activH %in% c("31", "32", "33"), "Inactif", as.character(pcsH)),
         pcsF = ifelse(activF %in% c("21", "22"), "En études", as.character(pcsF)),
         pcsF = ifelse(activF %in% c("31", "32", "33"), "Inactive", as.character(pcsF))) |>
  group_by(pcsH, pcsF) |> summarise(pImmoH = sum(immoH, na.rm=T) / sum(poidsH, na.rm=T) * 100,
                                    pImmoF = sum(immoF, na.rm=T) / sum(poidsF, na.rm=T) * 100,
                                    n = n()) |>
  filter(n > seuilSignifiant) |> # éviter les croisements improbables
  pivot_longer(cols = starts_with("pImmo"), names_to = "Genre", values_to = "pImmo") |>
  mutate(Genre = substr(Genre, 6,6))  |>
  mutate(pcsH = factor(pcsH, levels = unique(niv_PCS8_H)),
         pcsF = factor(pcsF, levels = unique(niv_PCS8_F))) |>
  ggplot(aes(x = pcsH, y = pcsF, size= pImmo)) +
  geom_count(aes(hjust = Genre, colour = Genre), shape = 21, alpha = .5) +
  scale_size(range = c(1, 10), name = "part (%)\nsans dépl.") +
  labs(title = "Journées sans déplacement selon le genre des conjoint⋅es",
       subtitle = "Couples comportant au moins un⋅e actif⋅ve",
       caption = ml(src_fig(emp = F),
                    "Les personnes sans emploi apparaissent",
                    "ici comme inactives.")) +
  xlab("Emploi homme") + ylab("Emploi femme") +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

sortie("Télétravail/Pas de sortie couples actif-ves")
print(g2)
off()

# comprPCSNonDep = PER %>%
#   mutate(pop = CoeffEnq) %>% filter(Activ %in% c("10", "11")) %>%
#   mutate(GenreEtActiv = paste0(etqGenre(Genre), " et ", etqActiv(Activ))) %>%
#   pivot_wider(names_from = "GenreEtActiv", values_from = "CoeffEnq", names_prefix = "pcs_",
#               names_sort = T) %>%
#   group_by(VeilleDepl) %>% summarise(across(starts_with("pcs_"), sum, na.rm=T),
#                                      total = sum(pop, na.rm=T)) %>%
#   mutate(across(starts_with("pcs_"), ~./total * 100))
# 
# comprPCSNonDep = t((filter(comprPCSNonDep, VeilleDepl == "2") /
#                       filter(comprPCSNonDep, VeilleDepl == "1") - 1)*100) %>%
#   as.data.frame()
# comprPCSNonDep[,2] = rownames(comprPCSNonDep)
# colnames(comprPCSNonDep) = c("p", "cat")
# comprPCSNonDep = filter(comprPCSNonDep, substr(cat,1,4) == "pcs_") %>%
#   tab_Tri(i = 2) %>% mutate(cat = substr(cat,5,nchar(as.character(cat))))
# 
# g3 = ggplot(comprPCSNonDep, aes(x = cat, y = p)) + geom_col() + coord_flip() +
#   theme_bw() +xlab("Statut d'emploi et genre") + ylab("Surreprésentation (%)") +
#   labs(title = "Surreprésentation de l'immobilité (au sens des EMD)\nselon le statut d'emploi")


# table(PER$VeilleTrav) ; nrow(filter(PER, !is.na(VeilleTrav)))
# table(PER$uid_ENQ, PER$VeilleTrav)
# 
# comprPCSNonDep = PER %>%
#   mutate(pop = CoeffEnq) %>% filter(Activ %in% c("10", "11")) %>%
#   filter(substr(PCS42S,1,1) %in% c("1", "2", "3", "4", "5", "6")) %>%
#   filter(VeilleTrav %in% c("1", "3")) %>%
#   mutate(télétrav = ifelse(VeilleTrav == "1", "non", "oui"),
#          télétrav = as.factor(télétrav)) %>%
#   pivot_wider(names_from = "PCS42S", values_from = "CoeffEnq", names_prefix = "pcs_",
#               names_sort = T) %>%
#   group_by(télétrav) %>% summarise(across(starts_with("pcs_"), sum, na.rm=T),
#                                    total = sum(pop, na.rm=T)) %>%
#   mutate(across(starts_with("pcs_"), ~./total * 100))
# 
# comprPCSNonDep = t((filter(comprPCSNonDep, télétrav == "oui") /
#                       filter(comprPCSNonDep, télétrav == "non") - 1)*100) %>%
#   as.data.frame()
# comprPCSNonDep[,2] = rownames(comprPCSNonDep)
# colnames(comprPCSNonDep) = c("p", "cat")
# comprPCSNonDep = filter(comprPCSNonDep, substr(cat,1,4) == "pcs_") %>%
#   tab_Tri(i = 2) %>% mutate(cat = etqPCS42S(substr(cat,5,6), num=T))
# 
# g5 = ggplot(comprPCSNonDep, aes(x = cat, y = p)) + geom_col() + coord_flip() +
#   theme_bw() +xlab("PCS individuelle") + ylab("Surreprésentation (%)") +
#   labs(title = "Surreprésentation du télétravail selon la PCS individuelle",
#        caption = src_fig(bu = T, date = "septembre 2022"))
# 
# sortie("Temps/Immobilité.pdf", format = "pdf", taille = "a4", portrait = T)
# gridExtra::arrangeGrob(g1, g2, g3, g5, nrow=2, ncol=2) %>% gridExtra::grid.arrange() %>% print()
# off() 

# Télétravail déclaré =====

# Petites stats
weighted.mean(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3")$DuTvl,
              filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3")$CoeffRecEnqSansEMP,
              na.rm=T)


weighted.mean(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3")$Dis,
              filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3")$CoeffRecEnqSansEMP,
              na.rm=T)

weighted.mean(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav %in% c("1", "2", "4", "5"))$Dis,
              filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav %in% c("1", "2", "4", "5"))$CoeffRecEnqSansEMP,
              na.rm=T)

sum(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3", Dis == 0)$CoeffRecEnqSansEMP) /
  sum(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav == "3")$CoeffRecEnqSansEMP)

sum(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav != "3", Dis == 0)$CoeffRecEnqSansEMP) /
  sum(filter(PER, Activ %in% c("10", "11", "12"), !is.na(CoeffRecEnqSansEMP), VeilleTrav != "3")$CoeffRecEnqSansEMP)


# Calculons le même genre de choses pour le télétravail, mais cela nécessite
# d'exclure les enquêtes qui ne posent pas la question

enqVeilleTrav = PER |>
  mutate(n = ifelse(!is.na(VeilleTrav), 1, 0)) |>
  group_by(uid_ENQ) |> summarise(n = sum(n)) |>
  filter(n > 0)
enqVeilleTrav = enqVeilleTrav$uid_ENQ
rapport(length(enqVeilleTrav), "enquêtes ont une variable VeilleTrav")

# Part télétrav genre et PCS

g = PER |> filter(Activ %in% c("10", "11"),
              PCS8 %in% c("01", "02", "03", "04", "05", "06"),
              uid_ENQ %in% enqVeilleTrav) |>
  filter(VeilleTrav %in% c("1", "2", "3")) |>
  pivot_wider(names_from = "VeilleTrav", values_from = "CoeffEnq",
              names_prefix = "VeilleTrav") |>
  group_by(PCS8, Genre) |>
  summarise(across(starts_with("VeilleTrav"), ~sum(., na.rm=T))) |>
  pivot_longer(cols = starts_with("VeilleTrav"), names_to = "VeilleTrav", values_to = "n") |>
  group_by(PCS8, Genre) |> mutate(p = n / sum(n) * 100) |>
  filter(VeilleTrav != "VeilleTrav1") |>
  mutate(VeilleTrav = case_when(VeilleTrav == "VeilleTrav1" ~ "S'est déplacé⋅e\npour travailler",
                                VeilleTrav == "VeilleTrav2" ~ "travaillé à\ndomicile",
                                VeilleTrav == "VeilleTrav3" ~ "télétravaillé")) |>
  mutate(PCS8 = etqPCS8(PCS8), Genre = etqGenre(Genre)) |>
  ggplot(aes(x = p, y = PCS8)) +
  geom_col(aes(fill = VeilleTrav), position = "dodge") +
  facet_wrap(~Genre) +
  xlab("Part des journées (%)") + ylab("PCS") +
  scale_fill_hue(name = "Part des\nactif⋅ves ayant...") +
  labs(title = "Journées à domicile et en télétravail",
       subtitle = "parmi les actif⋅ves en emploi",
       caption = src_fig(tibble(uid_ENQ = enqVeilleTrav)))

sortie("Télétravail/Part télétravail brute")
print(g)
off()

g = PER |> filter(Activ %in% c("10", "11"),
                 !is.na(dsDomEtq),
                 uid_ENQ %in% enqVeilleTrav) |>
  filter(VeilleTrav %in% c("1", "2", "3")) |>
  pivot_wider(names_from = "VeilleTrav", values_from = "CoeffEnq",
              names_prefix = "VeilleTrav") |>
  group_by(dsDomEtq, Genre) |>
  summarise(across(starts_with("VeilleTrav"), ~sum(., na.rm=T))) |>
  pivot_longer(cols = starts_with("VeilleTrav"), names_to = "VeilleTrav", values_to = "n") |>
  group_by(dsDomEtq, Genre) |> mutate(p = n / sum(n) * 100) |>
  filter(VeilleTrav != "VeilleTrav1") |>
  mutate(VeilleTrav = case_when(VeilleTrav == "VeilleTrav1" ~ "S'est déplacé⋅e\npour travailler",
                                VeilleTrav == "VeilleTrav2" ~ "travaillé à\ndomicile",
                                VeilleTrav == "VeilleTrav3" ~ "télétravaillé")) |>
  mutate(Genre = etqGenre(Genre)) |>
  ggplot(aes(x = dsDomEtq, y = p)) +
  geom_line(aes(colour = VeilleTrav, group = VeilleTrav), linetype = 2) +
  geom_point(aes(colour = VeilleTrav)) +
  facet_grid(rows = "Genre") +
  ylab("Part des journées en emploi (%)") + xlab("Densité dans le secteur (hab/km²)") +
  scale_colour_hue(name = "Part des\nactif⋅ves ayant...") +
  labs(title = "Journées à domicile et en télétravail",
       subtitle = "parmi les actif⋅ves en emploi",
       caption = src_fig(tibble(uid_ENQ = enqVeilleTrav))) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

sortie("Télétravail/Part télétravail selon densité")
print(g)
off()


# Par dép

tabDep = PER |>
  mutate(dep = case_when(substr(Com, 1, 2) == "97" ~ substr(Com, 1, 3),
                         substr(Com, 1, 2) != "97" ~ substr(Com, 1, 2))) |>
  filter(Activ %in% c("10", "11"),
         !is.na(dep),
         uid_ENQ %in% enqVeilleTrav) |>
  filter(VeilleTrav %in% c("1", "2", "3")) |>
  pivot_wider(names_from = "VeilleTrav", values_from = "CoeffEnq",
              names_prefix = "VeilleTrav") |>
  group_by(dep) |>
  summarise(across(starts_with("VeilleTrav"), ~sum(., na.rm=T)), nEnq = n()) |>
  pivot_longer(cols = starts_with("VeilleTrav"), names_to = "VeilleTrav", values_to = "n") |>
  group_by(dep) |> mutate(p = n / sum(n) * 100) |>
  filter(VeilleTrav == "VeilleTrav3")

deps = read_sf("Sources/Mailles/contour-des-departements.geojson")
deps = left_join(deps, tabDep, by = c("code" = "dep"))

deps_pt = st_point_on_surface(deps)

g = deps |>
  mutate(pEtq = discretisation(p, nbClassesCible = 6, couper1pc = F)) |>
  mutate(pEtq = as.factor(ifelse(p > 2.5, "[2.5:3[", as.character(pEtq)))) |>
  ggplot() +
  geom_sf(aes(fill = pEtq)) +
  geom_sf(data = deps_pt, aes(size = n), shape = 21, colour = "orange") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(5, "Blues"), "grey"),
                    name = "Part de journées\nen télétravail\nen %") +
  scale_size(name = "Nombre total\nd'enquêté⋅es") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Part de journées en télétravail par département",
       caption = src_fig(tibble(uid_ENQ = enqVeilleTrav)))

sortie("Télétravail/Carte déps")
print(g)
off()

# Children ?

# Croisement

# Densité

# Carte Fr / Carte LoireA