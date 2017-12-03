library("purrr")
library("readr")
library("dplyr")
library("ggplot2")
library("ggthemes")
library('tricky')
## Aperçu 

table_75 <- read_csv2(
  file = "RPLS 2017 - Donnees detaillees au logement/rpls2017_detail_ile_de_france_dep75.csv"
  )
table_75 %>% 
  glimpse()

## Liste des CSV
list_files <- dir(path = "RPLS 2017 - Donnees detaillees au logement/", pattern = "*.csv")

## Fonction d'extraction
extract_rpls <- function(file) {
  read_csv2(
    file = file, 
    col_types = cols_only(
      DEPCOM_red = col_character(), 
      nbpiece_red = col_integer(), 
      surfhab_red = col_integer(), 
      construct_red = col_integer(), 
      DPEENERGIE_red = col_character(), 
      DPESERRE_red = col_character()
      )
    )
}
extract_rpls(file = "RPLS 2017 - Donnees detaillees au logement/rpls2017_detail_auvergne_rhone_alpes.csv")

## Extraction des informations pertinentes et consolidation
table_rpls <- map(
  .x = paste0("RPLS 2017 - Donnees detaillees au logement/", list_files), 
  .f = extract_rpls) %>%
  bind_rows()

table_rpls %>% 
  glimpse()

## Nombre de logements sociaux
table_rpls %>%
  count() %>% 
  mutate(n = french_formatting(n))

## Nombre de pièces

table_rpls %>% 
  count(nbpiece_red)

png(filename = "nbpieces.png", width = 400, height = 320)
table_rpls %>% 
  count(nbpiece_red) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = as.factor(nbpiece_red), y = n)
      ) + 
  scale_x_discrete() + 
  scale_y_continuous(labels = french_formatting) + 
  ggthemes::theme_fivethirtyeight() + 
  labs(title = "Répartition du nombre de pièces \n par logement")
dev.off()

table_rpls %>% 
  summarise(min(surfhab_red), 
            mean(surfhab_red),
            max(surfhab_red)
            )

png(filename = "histogram_surface.png", width = 400, height = 320)
table_rpls %>%
  ggplot() + 
  geom_histogram(
    mapping = aes(x = surfhab_red),
    color = "white"
      ) +
  theme_fivethirtyeight() + 
  scale_y_continuous(labels = french_formatting) + 
  labs(title = "Répartition des logements sociaux \n 
       par surface habitable")
dev.off()


## Département

png(filename = "departement.png", width = 400, height = 320)
table_rpls %>%
  mutate(dep = substr(x = DEPCOM_red, start = 1, stop = 2)) %>%
  count(dep, sort = TRUE) %>%
  slice(1:5) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(dep, n), y = n)
    ) +
  coord_flip() + 
  labs(title = "Nombre de logements sociaux \n par département") + 
  theme_fivethirtyeight() + 
  scale_y_continuous(labels = function(x) {format(x, scientific = FALSE, big.mark = " ")})
dev.off()

table_rpls %>% 
  filter(is.na(DPEENERGIE_red) == FALSE) %>%
  count(DPEENERGIE_red) %>%
  mutate(share = 100 * n / sum(n))


png(filename = "dpeenergie.png", width = 400, height = 320)
table_rpls %>% 
  filter(is.na(DPEENERGIE_red) == FALSE) %>%
  count(DPEENERGIE_red) %>%
  mutate(share = 100 * n / sum(n)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = DPEENERGIE_red,y = share)) + 
  theme_fivethirtyeight() + 
  labs(title = "Consommation énergétique", 
       subtitle = "diagnostic de performance énergétique") + 
  scale_y_continuous(labels = french_formatting)
dev.off()


png(filename = "dpeserre.png", width = 400, height = 320)
table_rpls %>% 
  filter(is.na(DPESERRE_red) == FALSE) %>%
  count(DPESERRE_red) %>%
  mutate(share = 100 * n / sum(n)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = DPESERRE_red,y = share)) + 
  theme_fivethirtyeight() + 
  labs(title = "Émission de gaz à effet de serre", 
       subtitle = "diagnostic de performance énergétique"
       ) + 
  scale_y_continuous(labels = french_formatting)
dev.off()



## Année de construction

table_rpls$construct_red %>% class()

table_rpls %>% 
  count(construct_red) %>%
  View()




