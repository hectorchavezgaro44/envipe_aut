

poner_edades <- function(bd){
  bd <- bd %>%
    mutate(rango = case_when(edad >= 18 & edad<26 ~ "De 18 a 25 años",
                             edad >= 26 & edad<36 ~ "De 26 a 35 años",
                             edad >= 36 & edad<46 ~ "De 36 a 45 años",
                             edad >= 46 & edad<60 ~ "De 46 a 60 años",
                             TRUE ~ "Más de 60 años"))

  return(bd)
}


poner_estados <- function(bd){
  bd <- bd %>%
  mutate(nom_ent=case_when(ent=="08"~ "Chihuahua",
                           ent=="05" ~ "Coahuila",
                           ent=="13" ~ "Hidalgo",
                           ent=="19" ~ "Nuevo Leon",
                           ent=="18" ~ "Nayarit",
                           ent=="24" ~ "SLP",
                           ent=="26" ~ "Sonora",
                           ent=="32" ~ "Zacatecas"))

  return(bd)
}
