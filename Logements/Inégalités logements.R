library(readxl)
library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(stringr)
library(survey)

# Importation et préparation des données
revenus <- read_excel("Niveau de vie par commune pour R.xlsx")
dpe <- read_delim(
  "Logements/DPE logements existants.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  clean_names()
dpe <- dpe %>%
    mutate(code_postal_ban = as.character(code_postal_ban) %>%
               str_trim())  # Supprime les espaces

# Les informations sur les DPE utilisent l’adresse avec le code postal pour identifier les logements, mais celles sur les revenus utilisent les codes de l’INSEE. Pour permettre l’attribution de ces deux bases de données, la Base Adresses Nationale a été téléchargée et est importée.
BAN <- read_delim(
  "Logements/Base Adresse Nationale.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

densite <- read_excel("Logements/Densité des communes.xlsx")

# Ecarter de la base de données des adresses les informations superflues pour accélérer le traitement
Attribution_codes_postaux <- BAN %>%
  mutate(
    code_postal = as.character(code_postal) %>% str_trim(),
    nom_voie = as.character(nom_voie) %>% str_trim() %>% str_to_lower()
  ) %>%
  distinct(code_postal, nom_voie, .keep_all = TRUE) %>%
  select(code_postal, nom_voie, code_insee)

dpe <- dpe %>%
  mutate(
    code_postal_ban = as.character(code_postal_ban) %>% str_trim(),
    nom_rue_ban = as.character(nom_rue_ban) %>% str_trim() %>% str_to_lower() %>%
      ifelse(. == "", NA_character_, .)  # Remplacer les chaînes vides par NA
  )

codes_postaux_partages <- Attribution_codes_postaux %>%
  count(code_postal) %>%
  filter(n > 1) %>%
  pull(code_postal)

# Pour l’attribution des DPE aux informations sur les revenus des habitant·es des communes qui utilisent les codes INSEE des communes, distinguer trois cas de figure des informations sur les DPE :
#    a- Le DPE utilise un code postal uniquement utilisé par une commune (attribution directe)
#    b- Le DPE utilise un code postal utilisé par plusieurs communes et le nom de la voie du bâtiment est renseigné (attribution par voies des communes couvertes par le code postal)
#    c- Le DPE utilise un code postal utilisé par plusieurs communes, mais le nom de la voie n’est pas renseigné (attribution à la première commune identifiée pour ce code postal de façon arbitraire). Ce cas de figure concerne 422 400 des 15 millions de DPE réalisés, soit 2,8 %
dpe_uniques <- dpe %>% filter(!code_postal_ban %in% codes_postaux_partages)
dpe_partages_avec_voie <- dpe %>% filter(
  code_postal_ban %in% codes_postaux_partages & !is.na(nom_rue_ban)
)
dpe_partages_sans_voie <- dpe %>% filter(
  code_postal_ban %in% codes_postaux_partages & is.na(nom_rue_ban)
)

#    a- Attribution des DPE dont le code postal correspond à un seul code INSEE existe
ban_uniques <- Attribution_codes_postaux %>%
  filter(!code_postal %in% codes_postaux_partages) %>%
  select(code_postal, code_insee)

dpe_codes_uniques <- dpe_uniques %>%
  left_join(ban_uniques, by = c("code_postal_ban" = "code_postal"))

#    b- Attribution des DPE pour lesquels plusieurs codes de l’INSEE existent grâce au nom de la voie
ban_partages <- Attribution_codes_postaux %>%
  filter(code_postal %in% codes_postaux_partages) %>%
  select(code_postal, nom_voie, code_insee)

dpe_codes_partages_avec_voie <- dpe_partages_avec_voie %>%
  left_join(ban_partages, by = c("code_postal_ban" = "code_postal", "nom_rue_ban" = "nom_voie"))

#    c- Attribution des DPE pour lesquels plusieurs codes de l’INSEE existent sans que l’information du nom de voie permette de les attribuer clairement (attribution à la 1ère commune du code postal)
ban_default <- Attribution_codes_postaux %>%
  group_by(code_postal) %>%
  slice(1) %>%
  ungroup() %>%
  select(code_postal, code_insee)

dpe_codes_partages_sans_voie <- dpe_partages_sans_voie %>%
  left_join(ban_default, by = c("code_postal_ban" = "code_postal"))

dpe_codes <- bind_rows(
  dpe_codes_uniques,
  dpe_codes_partages_avec_voie,
  dpe_codes_partages_sans_voie
)

# Après l’attribution des trois groupes, vérifier le taux d'attribution
taux_attribution <- mean(!is.na(dpe_codes$code_insee)) * 100
cat("Taux d'attribution :", round(taux_attribution, 2), "%\n")

# Compter les DPE par commune et catégorie
DPE_commune_total <- dpe_codes %>%
  filter(!is.na(code_insee)) %>%
  count(code_insee, etiquette_dpe, name = "compteur") %>%
  group_by(code_insee) %>%
  mutate(total = sum(compteur)) %>%
  ungroup()

# A partir des chiffres absolus, établir la part de chaque catégorie des DPE pour chaque commune
DPE_commune_part <- DPE_commune_total %>%
  pivot_wider(
    names_from = etiquette_dpe,
    values_from = compteur,
    values_fill = 0  # Pour les communes ne comptant aucun DPE d'une catégorie donnée, retenir la valeur 0
  ) %>%
  mutate(across(c(A, B, C, D, E, F, G), ~ . / total))

# Calculer la part de passoires par commune et en moyenne pour tous les bâtiments existants
DPE_commune_part$part_passoires <- DPE_commune_part$F + DPE_commune_part$G
# Pour les DPE établis pour les bâtiments existants, calculer la moyenne de la part des passoires classés F ou G
dpe_passoires <- dpe %>%
+     filter(etiquette_dpe %in% c("F", "G"))
Passoires_moyenne <- (nrow(dpe_passoires)/nrow(dpe))

### Vérifier si la part des passoires thermiques d'une commune est influencée par le revenu médian de la population communale

passoires_revenu <- DPE_commune_part %>%
  rename(Commune_code = code_insee) %>%
  left_join(
    revenus %>% select(Commune_code, Median),
    by = "Commune_code"
  ) %>%
  mutate(Median = as.numeric(Median))

# Calculer la moyenne des passoires thermiques pour les 10 % des communes dont le revenu médian est le plus faible

passoires_revenu <- passoires_revenu[order(passoires_revenu$Median), ] # trie des données des communes en fonction du revenu médian de leur population
dixieme_communes <- round(nrow(passoires_revenu) * 0.10)
Passoires_moyenne_10_faible <- mean(passoires_revenu[1: dixieme_communes, "part_passoires"][[1]], na.rm = TRUE)

cat("Moyenne des passoires thermiques :", Passoires_moyenne, "\n")
cat("Moyenne dans les 10 % des communes avec le niveau de revenu médian le plus faible :", Passoires_moyenne_10_faible, "\n")

# Réaliser une régression linéaire entre le revenu médian des communes et leur part de passoires thermiques

passoires_revenu <- na.omit(passoires_revenu)
modele <- lm(part_passoires ~ Median, data = passoires_revenu)
summary(modele)

plot(
  passoires_revenu$Median,
  passoires_revenu$part_passoires,
  xlab = "Revenu médian",
  ylab = "Part des passoires thermiques",
  main = "Revenu médian et part de passoires thermiques par commune"
)
abline(modele, col = "red")

## Répéter l’analyse du lien entre la part des passoires et le revenu médian de la commune, mais cette fois-ci en pondérant les données en fonction du nombre de DPE réalisés dans chaque commune.

# Calculer le poids des DPE réalisés dans chaque commune. Le résultat est normalisé pour éviter des numéros trop petits
passoires_revenu <- passoires_revenu %>%
  mutate(poids = total / sum(total))

# Utilisation de la bibliothèque « survey » pour une analyse pondérée sans cluster
Ponderation_passoires <- svydesign(
  id = ~1,
  weights = ~poids,
  data = passoires_revenu
)
modele_svyglm <- svyglm(
  part_passoires ~ Median,
  design = Ponderation_passoires,
  family = gaussian()
)
summary(modele_svyglm)

# Visualisation graphique de la répartition pondérée
ggplot(passoires_revenu, aes(x = Median, y = part_passoires)) +
  geom_point(aes(size = total), alpha = 0.6, color = "blue") +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "red",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Revenu médian (€)",
    y = "Part des passoires thermiques (F + G)",
    size = "Nombre de DPE",
    title = "Relation entre revenu médian et passoires thermiques (pondéré par le nombre de DPE)"
  ) +
  theme_minimal()

## -> L’analyse montre que, dans les grandes communes, le revenu médian est plutôt faible, tout comme la part des passoires thermiques. L’augmentation du revenu médian ne va pas de pair avec une baisse de la part des passoires. Pour compléter l’analyse, celle-ci est répétée pour différencier entre les communes rurales, urbaines intermédiaires et urbaines en utilisant la classification de l’INSEE

passoires_revenu <- passoires_revenu %>%
left_join(
densite %>% select(CODGEO, DENS, LIBDENS),
by = c("Commune_code" = "CODGEO")
)

# Communes rurales (DENS = 3)
rural <- passoires_revenu %>%
  filter(DENS == 3) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))  # Pondération des communes en racine carrée

Ponderation_rural <- svydesign(id = ~1, weights = ~poids, data = rural)
modele_rural <- svyglm(part_passoires ~ Median, design = Ponderation_rural, family = gaussian())
summary(modele_rural)

# Communes urbaines intermédiaires (DENS = 2)
intermediaire <- passoires_revenu %>%
  filter(DENS == 2) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))

Ponderation_intermediaire <- svydesign(id = ~1, weights = ~poids, data = intermediaire)
modele_intermediaire <- svyglm(part_passoires ~ Median, design = Ponderation_intermediaire, family = gaussian())
summary(modele_intermediaire)

# Communes urbaines (DENS = 1)
urbain <- passoires_revenu %>%
  filter(DENS == 1) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))

Ponderation_urbain <- svydesign(id = ~1, weights = ~poids, data = urbain)
modele_urbain <- svyglm(part_passoires ~ Median, design = Ponderation_urbain, family = gaussian())
summary(modele_urbain)

# Visualisation des trois catégories de communes 
pred_rural <- data.frame(
  Median = seq(min(rural$Median, na.rm = TRUE), max(rural$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_rural, newdata = data.frame(Median = seq(min(rural$Median, na.rm = TRUE), max(rural$Median, na.rm = TRUE), length.out = 100)))),
  type = "Rural"
)
pred_intermediaire <- data.frame(
  Median = seq(min(intermediaire$Median, na.rm = TRUE), max(intermediaire$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_intermediaire, newdata = data.frame(Median = seq(min(intermediaire$Median, na.rm = TRUE), max(intermediaire$Median, na.rm = TRUE), length.out = 100)))),
  type = "Urbain intermédiaire"
)
pred_urbain <- data.frame(
  Median = seq(min(urbain$Median, na.rm = TRUE), max(urbain$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_urbain, newdata = data.frame(Median = seq(min(urbain$Median, na.rm = TRUE), max(urbain$Median, na.rm = TRUE), length.out = 100)))),
  type = "Urbain dense"
)
pred_df <- bind_rows(pred_rural, pred_intermediaire, pred_urbain)
ggplot(passoires_revenu, aes(x = Median, y = part_passoires)) +
    geom_point(aes(color = LIBDENS, size = total), alpha = 0.5) +
    geom_line(data = pred_df, aes(x = Median, y = pred, color = type), linewidth = 1) +
    labs(
        x = "Revenu médian (€)",
        y = "Part des passoires thermiques (F & G)",
        color = "Type de commune",
        size = "DPE réalisés",
        title = "Revenu médian et passoires thermiques par type de commune"
    ) +
    theme_minimal()

### Vérifier s'il existe un lien entre le revenu médian et la part des logements très bien isolés (classement A ou B) au niveau des communes

# Calculer la part de logements très bien isolés par commune et en moyenne pour tous les bâtiments existants
DPE_commune_part$part_bonne_isolation <- DPE_commune_part$A + DPE_commune_part$B
DPE_moyen_bonne_isolation <- mean(DPE_commune_part$part_bonne_isolation, na.rm = TRUE)

isolation_revenu <- DPE_commune_part %>%
  rename(Commune_code = code_insee) %>%
  left_join(
    revenus %>% select(Commune_code, Median),
    by = "Commune_code"
  ) %>%
  mutate(Median = as.numeric(Median))

# Calculer la moyenne des logements bénéficiant d'une bonne isolation pour les 10 % des communes dont le revenu médian est le plus élevé
isolation_revenu <- isolation_revenu[order(isolation_revenu$Median, decreasing = TRUE), ] # trie des données en fonction des communes en fonction du revenu médian de leur population
dixieme_communes <- round(nrow(isolation_revenu) * 0.10)
isolation_moyenne_10_riches <- mean(isolation_revenu[1: dixieme_communes, "part_bonne_isolation"][[1]], na.rm = TRUE)

cat("Moyenne des logements très bien isolés :", DPE_moyen_bonne_isolation, "\n")
cat("Part des logements très bien isolés dans les 10 % des communes avec le niveau de revenu médian le plus élevé :", isolation_moyenne_10_riches, "\n")

# Régression linéaire et visualisation graphique

isolation_revenu <- na.omit(isolation_revenu)
modele <- lm(part_bonne_isolation ~ Median, data = isolation_revenu)
summary(modele)

plot(
  isolation_revenu$Median,
  isolation_revenu$part_bonne_isolation,
  xlab = "Revenu médian",
  ylab = "Part des logements très bien isolés (A & B)",
  main = "Revenu médian et part de logements très bien isolés par commune"
)
abline(modele, col = "red")

## Répéter l’analyse du lien entre la part des logements très bien isolés et le revenu médian de la commune, mais cette fois-ci en pondérant les données en fonction du nombre de DPE réalisés dans chaque commune.

# Calculer le poids des DPE réalisés dans chaque commune. Le résultat est normalisé pour éviter des numéros trop petits
isolation_revenu <-isolation_revenu %>%
  mutate(poids = total / sum(total))

# Utilisation de la bibliothèque « survey » pour une analyse pondérée sans cluster
Ponderation_bonne_isolation <- svydesign(
  id = ~1,
  weights = ~poids,
  data = isolation_revenu
)
modele_svyglm <- svyglm(
  part_bonne_isolation ~ Median,
  design = Ponderation_bonne_isolation,
  family = gaussian()
)
summary(modele_svyglm)

# Visualisation graphique de la répartition pondérée
ggplot(isolation_revenu, aes(x = Median, y = part_bonne_isolation)) +
  geom_point(aes(size = total), alpha = 0.6, color = "blue") +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "red",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Revenu médian (€)",
    y = "Part des logements très bien isolés (A & B)",
    size = "Nombre de DPE",
    title = "Revenu médian et logements très bien isolés (pondéré par le nombre de DPE)"
  ) +
  theme_minimal()

## Analyse supplémentaire en différenciant entre les communes rurales, urbaines intermédiaires et urbaines en utilisant la classification de l’INSEE

isolation_revenu <- isolation_revenu %>%
left_join(
densite %>% select(CODGEO, DENS, LIBDENS),
by = c("Commune_code" = "CODGEO")
)

# Communes rurales (DENS = 3)
rural <- isolation_revenu %>%
  filter(DENS == 3) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))  # Pondération des communes en racine carrée

Ponderation_rural <- svydesign(id = ~1, weights = ~poids, data = rural)
modele_rural <- svyglm(part_bonne_isolation ~ Median, design = Ponderation_rural, family = gaussian())
summary(modele_rural)

# Communes urbaines intermédiaires (DENS = 2)
intermediaire <- isolation_revenu %>%
  filter(DENS == 2) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))

Ponderation_intermediaire <- svydesign(id = ~1, weights = ~poids, data = intermediaire)
modele_intermediaire <- svyglm(part_bonne_isolation ~ Median, design = Ponderation_intermediaire, family = gaussian())
summary(modele_intermediaire)

# Communes urbaines (DENS = 1)
urbain <- isolation_revenu %>%
  filter(DENS == 1) %>%
  mutate(poids = sqrt(total) / sum(sqrt(total)))

Ponderation_urbain <- svydesign(id = ~1, weights = ~poids, data = urbain)
modele_urbain <- svyglm(part_bonne_isolation ~ Median, design = Ponderation_urbain, family = gaussian())
summary(modele_urbain)

# Visualisation des trois catégories de communes 
pred_rural <- data.frame(
  Median = seq(min(rural$Median, na.rm = TRUE), max(rural$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_rural, newdata = data.frame(Median = seq(min(rural$Median, na.rm = TRUE), max(rural$Median, na.rm = TRUE), length.out = 100)))),
  type = "Rural"
)
pred_intermediaire <- data.frame(
  Median = seq(min(intermediaire$Median, na.rm = TRUE), max(intermediaire$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_intermediaire, newdata = data.frame(Median = seq(min(intermediaire$Median, na.rm = TRUE), max(intermediaire$Median, na.rm = TRUE), length.out = 100)))),
  type = "Urbain intermédiaire"
)
pred_urbain <- data.frame(
  Median = seq(min(urbain$Median, na.rm = TRUE), max(urbain$Median, na.rm = TRUE), length.out = 100),
  pred = as.numeric(predict(modele_urbain, newdata = data.frame(Median = seq(min(urbain$Median, na.rm = TRUE), max(urbain$Median, na.rm = TRUE), length.out = 100)))),
  type = "Urbain dense"
)
pred_df <- bind_rows(pred_rural, pred_intermediaire, pred_urbain)
ggplot(isolation_revenu, aes(x = Median, y = part_bonne_isolation)) +
    geom_point(aes(color = LIBDENS, size = total), alpha = 0.5) +
    geom_line(data = pred_df, aes(x = Median, y = pred, color = type), linewidth = 1) +
    labs(
        x = "Revenu médian (€)",
        y = "Part des logements très bien isolés (A & B)",
        color = "Type de commune",
        size = "DPE réalisés",
        title = "Revenu médian et passoires thermiques par type de commune"
    ) +
    theme_minimal()

### Vérifier si la part des passoires thermiques est plus faible dans les 10 % des communes avec le revenu médian le plus élevé par rapport à la moyenne nationale
passoires_revenu <- passoires_revenu[order(passoires_revenu$Median, decreasing = TRUE), ]

# Retenir les 10 % des communes avec le revenu médian le plus élevé
seuil_10_pourcent_riches <- round(nrow(passoires_revenu) * 0.10)
groupes_riches <- passoires_revenu[1:seuil_10_pourcent_riches, "part_passoires"][[1]]

# Calcul de la moyenne des passoires thermiques dans ces 10 % des communes les plus riches
Passoires_moyenne_10_riches <- mean(groupes_riches, na.rm = TRUE)

cat("Moyenne globale des passoires thermiques :", Passoires_moyenne, "\n")
cat("Moyenne pour les 10 % les plus riches :", Passoires_moyenne_10_riches, "\n")

resultat_test_riches <- t.test(
  groupes_riches,
  passoires_revenu$part_passoires,
  alternative = "less"
)
print(resultat_test_riches)
