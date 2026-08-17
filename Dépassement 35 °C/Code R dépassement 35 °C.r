###
# Ces commandes pour le logiciel R permettent de calculer l'évolution des journées supérieures à 35 °C à Paris en comparant leur nombre pour l'année en cours avec celui des années 2000. Il peut être adapté pour d'autres villes en remplaçant les noms des fichiers et en adaptant, au besoin, les noms des variables.
###

library(readr)
library(tidyverse)

Paris_hist <- read_delim("Météo/Paris 2000-2024.csv", delim = ";")
Paris_2026 <- read_delim("Météo/Paris 2026.csv", delim = ";")

# Les données météorologiques étant journalières, seule l’année est extraite vu que l’analyse vise uniquement à compter les dépassements par jour
Extraction_année <- function(AAAAMMJJ)as.integer(substr(as.character(AAAAMMJJ), 1, 4))

# Convertir la variable avec les données de températures maximales en variable numérique
Température_maximale <- function(tx) {
    as.numeric(tx)
}

# Compter le nombre de journées avec des températures supérieures à 35 °C au cours des années 2000
Paris_2000 <- Paris_hist %>%
    mutate(
        Année = Extraction_année(AAAAMMJJ),
        TX = Température_maximale(TX)
    ) %>%
    filter(Année >= 2000 & Année <= 2009 & !is.na(TX))

Dépassements_annuels <- Paris_2000 %>%
    group_by(Année) %>%
    summarise(Journées_35 = sum(TX > 35, na.rm = TRUE)) %>%
    ungroup()

# Calculer la moyenne annuelle pour les dépassements du seuil de 35 °C pour les années 2000
Paris_2000_moyenne <- mean(Dépassements_annuels$Journées_35, na.rm = TRUE)

# Pour 2026, compter également le nombre de journées avec des températures supérieurs à 35 °C
Paris_2026 <- Paris_2026 %>%
    mutate(TX = Température_maximale(TX))

Dépassements_2026 <- sum(Paris_2026$TX > 35, na.rm = TRUE)

print(Paris_2000_moyenne)
print(Dépassements_2026)
