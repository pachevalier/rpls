
count_rpls <- function(file) {
  read_csv2(
    file = file,
    col_types = cols_only(dep = col_character())
  ) %>% 
    group_by(dep) %>%
    count() 
}
count_rpls(file = "RPLS 2017 - Donnees detaillees au logement/rpls2017_detail_auvergne_rhone_alpes.csv")

count_rpls_idf <- function(file) {
  read_csv2(
    file = file,
    col_types = cols_only(DROIT_red = col_character())
  ) %>% 
    count() %>%
    mutate(dep = sub(x = file, pattern = ".*dep([[:digit:]]{2})\\.csv$", replacement = "\\1")) %>%
    select(dep, n)
}
count_rpls_idf(file = "RPLS 2017 - Donnees detaillees au logement/rpls2017_detail_ile_de_france_dep75.csv")

table_depts_1 <- map(
  .x = paste0("RPLS 2017 - Donnees detaillees au logement/", 
              list_files %>% keep(.p = grepl(pattern = ".*dep[[:digit:]]{2}\\.csv$", x = .) == FALSE)), 
  .f = count_rpls
) %>%
  bind_rows()

table_depts_2 <- map(
  .x = paste0("RPLS 2017 - Donnees detaillees au logement/", 
              list_files %>% keep(.p = grepl(pattern = ".*dep[[:digit:]]{2}\\.csv$", x = .) == TRUE)), 
  .f = count_rpls_idf
) %>%
  bind_rows()

table_depts <- bind_rows(table_depts_1, table_depts_2) %>%
  group_by(dep) %>%
  summarise(n = sum(n))
table_depts %>% arrange(n)

# library(rgdal)
# library(hexmapr)
# library(sf)
# sp_depts <- readOGR(dsn = ".", layer = "departements-20140306-100m")
# plot(sp_depts)
# 
# polygons <- read_polygons(file = "departements-20140306-100m.shp")
# plot(polygons)
# original_details <- get_shape_details(original_shapes)