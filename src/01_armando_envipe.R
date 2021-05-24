rm(list=ls())
require(pacman)
p_load(sf, tidyverse, foreign, survey,  readxl, here, janitor)


# para 2019, 2018 y 2017 ---------------------------------------------------------------
options(survey.lonely.psu="adjust")
ultima <- tibble()
nombres <- c( "19", "18", "17")
pb = txtProgressBar(min=1, max=length(nombres), style=3)
for(i in 1:length(nombres)) {
inp <- "~/Documents/otros/inp"
df <- foreign::read.dbf(paste(inp, paste0("TPer_Vic1_", nombres[i], ".dbf"), sep="/"), as.is = T) %>%
     clean_names() %>%
     select(upm:aream, est_dis, upm_dis, ap4_1:fac_ele_am) %>%
     summarise(across(upm:upm_dis, ~as.character(.x)),
            across(ap4_1:fac_ele_am, ~as.numeric(.x)))



# Policia Estatal -----------
pe <- df %>%
      mutate(ap5_4_03 = factor(ap5_4_03, levels = c(1,2,3,4,9),
             labels = c("Mucha confianza", "Algo de confianza","Algo de desconfianza",
             "Mucha desconfianza", "Ns/Nc - confianza")),
             ap5_5_03=factor(ap5_5_03, levels = c(1,2,9),
             labels = c("Corrupto", "No corrupto","Ns/Nc - corrupto")),
             ap5_6_03=factor(ap5_6_03, levels = c(1,2,3,4,9),
             labels = c("Muy efectivo", "Algo efectivo","Poco efectivo", "Nada efectivo", "Ns/Nc -efectivo"))) %>%
             filter(!is.na(ap5_4_03))


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe, nest=F)
svymean(~ap5_4_03, design)
ENT = substr(pe$upm, 1, 2) #Entidad
SEX <- pe$sexo
mean_edo_confianza = svyby(~ap5_4_03, by=ENT, design=design, svymean)
mean_edo_corrupto = svyby(~ap5_5_03, by=ENT, design=design, svymean)
mean_edo_efectivo = svyby(~ap5_6_03, by=ENT, design=design, svymean)


tempo <- mean_edo_confianza %>%
         select(edo=by, 2:6) %>%
         gather(variable, prom, 2:6) %>%
         mutate(variable=gsub("ap5_4_03", "", variable),
                prom=round(prom*100, 2),
                autoridad="Policia Estatal") %>%
         arrange(variable) %>%
         spread(variable, prom)
final <- tempo
tempo <- mean_edo_corrupto %>%
  select(edo=by, 2:4) %>%
  gather(variable, prom, 2:4) %>%
  mutate(variable=gsub("ap5_5_03", "", variable),
         prom=round(prom*100, 2)) %>%
  spread(variable, prom)
final <- left_join(final, tempo, by="edo")

tempo <- mean_edo_efectivo %>%
  select(edo=by, 2:6) %>%
  gather(variable, prom, 2:6) %>%
  mutate(variable=gsub("ap5_6_03", "", variable),
         prom=round(prom*100, 2))%>%
  spread(variable, prom)
final <- left_join(final, tempo, by="edo")

final <- mutate(final, year=paste0("20", nombres[i]))

ultima <- bind_rows(ultima, final)
rm(tempo, final, mean_edo_confianza, mean_edo_corrupto, mean_edo_efectivo, pe, design)
setTxtProgressBar(pb, i)
}

# para 16 y 15 ---------------------------------------------------------


ultima_16 <- tibble()

nombres <- c( "16", "15")
pb = txtProgressBar(min=1, max=length(nombres), style=3)
for(i in 1:length(nombres)) {

tsdem <- foreign::read.dbf(paste(inp, paste0("TSDem_", nombres[i], ".dbf"), sep="/"), as.is = T) %>%
  clean_names() %>%
  select(upm:n_ren, sexo, edad)

df <- foreign::read.dbf(paste(inp, paste0("TPer_Vic1_", nombres[i], ".dbf"), sep="/"), as.is = T) %>%
  clean_names() %>%
  left_join(tsdem, by=c("upm", "viv_sel", "hogar", "resul_h", "r_sel"="n_ren")) %>%
      select(upm:r_sel,edad,sexo,aream, est_dis, upm_dis, ap4_1:fac_ele_am)%>%
  summarise(across(upm:upm_dis, ~as.character(.x)),
            across(ap4_1:fac_ele_am, ~as.numeric(.x)))

# Policia Estatal -----------
pe <- df %>%
  mutate(ap5_4_03 = factor(ap5_4_03, levels = c(1,2,3,4,9),
                           labels = c("Mucha confianza", "Algo de confianza","Algo de desconfianza",
                                      "Mucha desconfianza", "Ns/Nc - confianza")),
         ap5_5_03=factor(ap5_5_03, levels = c(1,2,9),
                         labels = c("Corrupto", "No corrupto","Ns/Nc - corrupto")),
         ap5_6_03=factor(ap5_6_03, levels = c(1,2,3,4,9),
                         labels = c("Muy efectivo", "Algo efectivo","Poco efectivo", "Nada efectivo", "Ns/Nc -efectivo"))) %>%
  filter(!is.na(ap5_4_03))


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe, nest=F)
svymean(~ap5_4_03, design)
ENT = substr(pe$upm, 1, 2) #Entidad
SEX <- pe$sexo
mean_edo_confianza = svyby(~ap5_4_03, by=ENT, design=design, svymean)
mean_edo_corrupto = svyby(~ap5_5_03, by=ENT, design=design, svymean)
mean_edo_efectivo = svyby(~ap5_6_03, by=ENT, design=design, svymean)


tempo <- mean_edo_confianza %>%
  select(edo=by, 2:6) %>%
  gather(variable, prom, 2:6) %>%
  mutate(variable=gsub("ap5_4_03", "", variable),
         prom=round(prom*100, 2),
         autoridad="Policia Estatal") %>%
  arrange(variable) %>%
  spread(variable, prom)
final <- tempo
tempo <- mean_edo_corrupto %>%
  select(edo=by, 2:4) %>%
  gather(variable, prom, 2:4) %>%
  mutate(variable=gsub("ap5_5_03", "", variable),
         prom=round(prom*100, 2)) %>%
  spread(variable, prom)
final <- left_join(final, tempo, by="edo")

tempo <- mean_edo_efectivo %>%
  select(edo=by, 2:6) %>%
  gather(variable, prom, 2:6) %>%
  mutate(variable=gsub("ap5_6_03", "", variable),
         prom=round(prom*100, 2))%>%
  spread(variable, prom)
final <- left_join(final, tempo, by="edo")

final <- mutate(final, year=paste0("20", nombres[i]))

ultima_16 <- bind_rows(ultima_16, final)
rm(tempo, final, mean_edo_confianza, mean_edo_corrupto, mean_edo_efectivo, pe, design)
setTxtProgressBar(pb, i)
}


