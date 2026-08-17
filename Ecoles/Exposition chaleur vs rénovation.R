library(readxl)
library(dplyr)

# Importation des données
Vigilance_rouge <- read_excel("Départements en vigilance rouge juin 2026.xlsx") # Ce fichier a été créé manuellement à partir des archives de vigilance de Météo France pour identifier tous les départements ayant connu un épisode de vigilance rouge canicule au cours du mois de juin 2026
Renovations_subventionnees <- read_excel("Ecoles subventionnées Fonds vert.xlsx")
Etablissements_scolaires <- read_excel("Annuaire établissements scolaires.xlsx")
depts_rouge <- Vigilance_rouge %>%
  filter(`Vigilance rouge canicule` == "Oui") %>%
  pull(GEO)

# Extraire du numéro SIRET le numéro SIREN (correspondant aux neuf premiers chiffres du SIRET)
Etablissements_scolaires <- Etablissements_scolaires %>%
  mutate(
    SIREN_SIRET = as.character(SIREN_SIRET),
    SIREN_9 = substr(SIREN_SIRET, 1, 9)  # 9 premiers caractères
  )

siren_subventionnes <- as.character(Renovations_subventionnees$SIREN)
siren_subventionnes_9 <- substr(siren_subventionnes, 1, 9)

# Identifier, parmi les établissements scolaires, ceux qui ont bénéficié d’une subvention par le Fonds vert grâce à leur numéro SIREN
Etablissements_scolaires <- Etablissements_scolaires %>%
  mutate(subvention = SIREN_SIRET %in% siren_subventionnes)

# Pour certains établissements, les données du Fonds vert ont renseigné non pas le SIREN, mais le SIRET. Continuer l’attribution avec ce numéro
Etablissements_scolaires <- Etablissements_scolaires %>%
  mutate(subvention = ifelse(subvention, TRUE, SIREN_9 %in% siren_subventionnes_9))

# Identification des établissements subventionnés qui n’ont pas trouvés ni par le SIREN, ni le SIRET
subventionnes_non_trouves <- Renovations_subventionnees %>%
  mutate(
    SIREN = as.character(SIREN),
    SIREN_9 = substr(SIREN, 1, 9)
  ) %>%
  filter(!SIREN %in% Etablissements_scolaires$SIREN_SIRET &
         !SIREN_9 %in% Etablissements_scolaires$SIREN_9)
print((nrow(subventionnes_non_trouves)))

# Identifier les établissements situés dans des départements qui ont été concernés par un épisode de canicule rouge en juin 2026
total_etablissements <- nrow(Etablissements_scolaires)
etablissements_rouge <- Etablissements_scolaires %>%
  filter(as.character(Code_departement) %in% depts_rouge) %>%
  nrow()

part_rouge <- (etablissements_rouge / total_etablissements) * 100
print(part_rouge)
print(etablissements_rouge)
print(total_etablissements)

# Identifier, parmi les établissements situés dans des départements qui ont été concernés par un épisode de canicule rouge en juin 2026, les seules écoles
ecoles_rouge <- Etablissements_scolaires %>%
  filter(Type_etablissement == "Ecole" & as.character(Code_departement) %in% depts_rouge)

total_ecoles <- Etablissements_scolaires %>%
  filter(Type_etablissement == "Ecole")

ecoles_rouges_subventionnees <- ecoles_rouge %>% filter(subvention)
print(nrow(ecoles_rouges_subventionnees))
print(nrow(ecoles_rouge))
print(nrow(total_ecoles)) # Nombre d’écoles

# Corriger l’analyse concernant les écoles en tenant compte du fait que certaines subventions à des établissements n’ont pas pu être attribués à des établissements. Pour cela assumer que l’ensemble des projets de rénovation non attribués concernent des écoles concernées par un épisode de vigilance rouge.
taux_ecoles_rouges_subventionnees <- (nrow(ecoles_rouges_subventionnees) + nrow(subventionnes_non_trouves)) / nrow(total_ecoles)
