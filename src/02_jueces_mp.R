rm(list=ls())
require(pacman)
p_load(sf, tidyverse, foreign, survey,  readxl, here, janitor)



# ministerios publicos ----------------------------------------------------

options(survey.lonely.psu="adjust")

inp <- "~/Documents/otros/inp"

df <- foreign::read.dbf(paste(inp, "TPer_Vic1.dbf", sep="/"), as.is = T) %>%
  clean_names() %>%
  select(upm:aream, est_dis, upm_dis, ap4_1:fac_ele_am) %>%
  summarise(across(upm:upm_dis, ~as.character(.x)),
            across(ap4_1:fac_ele_am, ~as.numeric(.x)))

pe <- df %>%
  mutate(ap5_4_07 = factor(ap5_4_07, levels = c(1,2,3,4,9),
                           labels = c("Mucha confianza", "Algo de confianza","Algo de desconfianza",
                                      "Mucha desconfianza", "Ns/Nc - confianza")),
         ap5_5_07=factor(ap5_5_07, levels = c(1,2,9),
                         labels = c("Corrupto", "No corrupto","Ns/Nc - corrupto")),
         ap5_6_07=factor(ap5_6_07, levels = c(1,2,3,4,9),
                         labels = c("Muy efectivo", "Algo efectivo","Poco efectivo", "Nada efectivo", "Ns/Nc -efectivo")),
         sexo=factor(sexo, levels = c(1,2),
                     labels = c("Hombre", "Mujer")),
         ent=substr(upm,1,2)) %>%
  filter(!is.na(ap5_4_07))


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe)

# ENT = substr(pe$upm, 1, 2) #Entidad
# SEX <- pe$sexo
mean_edo_confianza = svyby(~ap5_4_07, ~pe$ent+~pe$sexo, design=design, svymean) %>%
                       as.tibble() %>%
                       gather(variable, prom, `ap5_4_07Mucha confianza`:`se.ap5_4_07Ns/Nc - confianza`) %>%
                       mutate(variable=gsub("ap5_4_07", "", variable)) %>%
                       spread(variable, prom) %>%
                       rename("entidad"="pe$ent", "sexo"="pe$sexo")

mean_edo_corrupto = svyby(~ap5_5_07, ~pe$ent+pe$sexo, design=design, svymean) %>%
  as.tibble() %>%
  gather(variable, prom, `ap5_5_07Corrupto`:`se.ap5_5_07Ns/Nc - corrupto`) %>%
  mutate(variable=gsub("ap5_5_07", "", variable)) %>%
  spread(variable, prom) %>%
  rename("entidad"="pe$ent", "sexo"="pe$sexo")

mean_edo_efectivo = svyby(~ap5_6_07, ~pe$ent+pe$sexo, design=design, svymean) %>%
  as.tibble() %>%
  gather(variable, prom, `ap5_6_07Muy efectivo`:`se.ap5_6_07Ns/Nc -efectivo`) %>%
  mutate(variable=gsub("ap5_6_07", "", variable)) %>%
  spread(variable, prom) %>%
  rename("entidad"="pe$ent", "sexo"="pe$sexo")

mp <- left_join(mean_edo_confianza,mean_edo_corrupto , by=c("entidad","sexo"))
mp <- left_join(mp, mean_edo_efectivo , by=c("entidad","sexo"))

save(mp, file="mp.rda")
# Jueces ------------------------------------------------------------------

pe <- df %>%
  mutate(ap5_4_11 = factor(ap5_4_11, levels = c(1,2,3,4,9),
                           labels = c("Mucha confianza", "Algo de confianza","Algo de desconfianza",
                                      "Mucha desconfianza", "Ns/Nc - confianza")),
         ap5_5_11=factor(ap5_5_11, levels = c(1,2,9),
                         labels = c("Corrupto", "No corrupto","Ns/Nc - corrupto")),
         ap5_6_11=factor(ap5_6_11, levels = c(1,2,3,4,9),
                         labels = c("Muy efectivo", "Algo efectivo","Poco efectivo", "Nada efectivo", "Ns/Nc -efectivo")),
         sexo=factor(sexo, levels = c(1,2),
                     labels = c("Hombre", "Mujer")),
         ent=substr(upm,1,2)) %>%
  filter(!is.na(ap5_4_11))


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe, nest=F)

# ENT = substr(pe$upm, 1, 2) #Entidad
# SEX <- pe$sexo
mean_edo_confianza = svyby(~ap5_4_11,~pe$ent+~pe$sexo, design=design, svymean) %>%
  as.tibble() %>%
  gather(variable, prom, `ap5_4_11Mucha confianza`:`se.ap5_4_11Ns/Nc - confianza`) %>%
  mutate(variable=gsub("ap5_4_11", "", variable)) %>%
  spread(variable, prom) %>%
  rename("entidad"="pe$ent", "sexo"="pe$sexo")

mean_edo_corrupto = svyby(~ap5_5_11, ~pe$ent+pe$sexo, design=design, svymean) %>%
  as.tibble() %>%
  gather(variable, prom, `ap5_5_11Corrupto`:`se.ap5_5_11Ns/Nc - corrupto`) %>%
  mutate(variable=gsub("ap5_5_11", "", variable)) %>%
  spread(variable, prom) %>%
  rename("entidad"="pe$ent", "sexo"="pe$sexo")

mean_edo_efectivo = svyby(~ap5_6_11, ~pe$ent+pe$sexo, design=design, svymean)%>%
  as.tibble() %>%
  gather(variable, prom, `ap5_6_11Muy efectivo`:`se.ap5_6_11Ns/Nc -efectivo`) %>%
  mutate(variable=gsub("ap5_6_11", "", variable)) %>%
  spread(variable, prom) %>%
  rename("entidad"="pe$ent", "sexo"="pe$sexo")


jueces <- left_join(mean_edo_confianza,mean_edo_corrupto , by=c("entidad","sexo"))
jueces <- left_join(jueces, mean_edo_efectivo , by=c("entidad","sexo"))
save(jueces, file="jueces.rda")
