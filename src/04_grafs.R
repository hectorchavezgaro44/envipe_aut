# Por grupos de edad ------------------------------------------------------

rm(list=ls())
require(pacman)
p_load(sf, tidyverse, foreign, survey,  readxl, here, janitor, ggmosaic, extrafont, svglite)

# ministerios publicos ----------------------------------------------------

options(survey.lonely.psu="adjust")
source("src/00_fun_grafs.R")
estados_ximena <- c("08", "05", "13", "19", "18", "24", "26", "32")
inp <- "~/Users/hectorchavez/Git/envipe_autoridad/inp"

df <- read.dbf("~/Git/envipe_autoridad/inp/TPer_Vic1.dbf", as.is = T) %>%
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
  filter(!is.na(ap5_4_07)) %>%
  poner_edades()


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe)

mean_edo_confianza = svyby(~ap5_4_07, ~pe$ent+~pe$sexo+~pe$rango, design=design, svytotal)



pb = txtProgressBar(min=1, max=length(estados_ximena), style=3)
for(i in 1:length(estados_ximena)) {

tempo <- mean_edo_confianza %>%
  as.tibble() %>%
  select( 1:7) %>%
  gather(variable, total, 4:7) %>%
  mutate(variable=gsub("ap5_4_07", "", variable)) %>%
  rename("ent"="pe$ent", "sexo"="pe$sexo",
         "rango"="pe$rango") %>%
  poner_estados() %>%
  filter(ent==estados_ximena[i]) %>%
  mutate(var=case_when(variable=="Mucha confianza"| variable=="Algo de confianza"~ 1,
                       variable=="Algo de desconfianza" |variable=="Mucha desconfianza" ~2)) %>%
  arrange(desc(var)) %>%
  group_by(nom_ent, rango,var, sexo) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  mutate(var=factor(var, label=c("Mucha o algo de confianza", "Mucha o algo de desconfianza")))


ggplot(data = tempo) +
  geom_mosaic(aes(x = product(var),weight=totales, fill = rango), alpha=0.6) +
  scale_fill_brewer(palette="Spectral")+
  facet_grid(~sexo) +
  labs(x="",
       y="",
       title="¿Cuánta confianza le inspiran los ministerios públicos y fiscalías estatales?",
       subtitle = paste(tempo$nom_ent,
                        paste0(" mayores a 18 años según nivel de confianza en la autoridad"),
                        sep=" - ") ,
       caption="INEGI: ENVIPE 2020") +
  theme_minimal()+
  theme(legend.position = "none",
        text =  element_text(family="Georgia", size=20,color = "black"))


ggsave(here("out", "Vic", "final",paste(unique(tempo$nom_ent),"mp.svg", sep="_")),
       width = 18, height = 10,units = "in")


tree <- tempo %>%
  group_by(var, sexo) %>%
  summarise(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  group_by(sexo) %>%
  mutate(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  mutate(porc_confianza=round((total/totales)*100, 2)) %>%
  select(var, sexo, porc_confianza)

stack <-tempo %>%
  group_by(var, sexo) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  mutate(porc=round((totales/total)*100,2)) %>%
  mutate(rango_real=case_when(rango=="De 18 a 25 años" ~ 5,
                              rango=="De 26 a 35 años" ~ 4,
                              rango=="De 36 a 45 años" ~ 3,
                              rango=="De 46 a 60 años" ~ 2,
                              TRUE ~ 1),
         rango_real=factor(rango_real, label=c("Mas de 60 años", "De 46 a 60 años",
                                               "De 36 a 45 años", "De 26 a 35 años",
                                               "De 18 a 25 años"))) %>%
  left_join(tree, by=c("var", "sexo"))

ggplot(data=stack, aes(x = var, y= porc, fill=rango_real)) +
  geom_bar(stat="identity", alpha=0.6) +
  geom_text(aes(label=paste(paste0(porc, "%"), paste0(porc_confianza, "%"), sep="\n")),
            position=position_stack(vjust=0.5))+
  facet_grid(~sexo) +
  scale_fill_brewer(palette="Spectral", direction=-1)+
  labs(x="",
       y="",
       title="¿Cuánta confianza le inspiran los ministerios públicos y fiscalías estatales?",
       subtitle = paste(tempo$nom_ent,
                        paste0(" mayores a 18 años según nivel de confianza en la autoridad"),
                        sep=" - ") ,
       caption="INEGI: ENVIPE 2020")+
  theme_minimal()+
  theme(text =  element_text(family="Georgia", size=20,color = "black"))

ggsave(here("out","Vic", "labels",paste(unique(tempo$nom_ent),"mp_lab.svg", sep="_")),
       width = 18, height = 10,units = "in")


rm(tempo, stack, tree)
setTxtProgressBar(pb, i)

}


rm(list=ls())
source("src/00_fun_grafs.R")


# jueces ------------------------------------------------------------------


df <- read.dbf("~/Git/envipe_autoridad/inp/TPer_Vic1.dbf", as.is = T) %>%
  clean_names() %>%
  select(upm:aream, est_dis, upm_dis, ap4_1:fac_ele_am) %>%
  summarise(across(upm:upm_dis, ~as.character(.x)),
            across(ap4_1:fac_ele_am, ~as.numeric(.x)))

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
  filter(!is.na(ap5_4_11)) %>%
  poner_edades()


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe)

mean_edo_confianza = svyby(~ap5_4_11, ~pe$ent+~pe$sexo+~pe$rango, design=design, svytotal)



pb = txtProgressBar(min=1, max=length(estados_ximena), style=3)
for(i in 1:length(estados_ximena)) {

  tempo <- mean_edo_confianza %>%
    as.tibble() %>%
    select( 1:7) %>%
    gather(variable, total, 4:7) %>%
    mutate(variable=gsub("ap5_4_11", "", variable)) %>%
    rename("ent"="pe$ent", "sexo"="pe$sexo",
           "rango"="pe$rango") %>%
    poner_estados() %>%
    filter(ent==estados_ximena[i]) %>%
    mutate(var=case_when(variable=="Mucha confianza"| variable=="Algo de confianza"~ 1,
                         variable=="Algo de desconfianza" |variable=="Mucha desconfianza" ~2)) %>%
    arrange(desc(var)) %>%
    group_by(nom_ent, rango,var, sexo) %>%
    summarise(totales=sum(total, na.rm=T)) %>%
    ungroup() %>%
    mutate(var=factor(var, label=c("Mucha o algo de confianza", "Mucha o algo de desconfianza")))


  ggplot(data = tempo) +
    geom_mosaic(aes(x = product(var),weight=totales, fill = rango), alpha=0.6) +
    scale_fill_brewer(palette="Spectral", direction=-1)+
    facet_grid(~sexo) +
    labs(x="",
         y="",
         title="¿Cuánta confianza le inspiran los jueces?",
         subtitle = paste(tempo$nom_ent,
                          paste0(" mayores a 18 años según nivel de confianza en la autoridad"),
                          sep=" - ") ,
         caption="INEGI: ENVIPE 2020", fill="") +
    theme_minimal()+
    theme(legend.position = "none",
          text =  element_text(family="Georgia", size=20,color = "black"),
 )


  ggsave(here("out", "Vic","final", paste(unique(tempo$nom_ent),"jueces.svg", sep="_")),
         width = 18, height = 10,units = "in")
  tree <- tempo %>%
    group_by(var, sexo) %>%
    summarise(total=sum(totales, na.rm=T)) %>%
    ungroup() %>%
    group_by(sexo) %>%
    mutate(totales=sum(total, na.rm=T)) %>%
    ungroup() %>%
    mutate(porc_confianza=round((total/totales)*100, 2)) %>%
    select(var, sexo, porc_confianza)

  stack <-tempo %>%
    group_by(var, sexo) %>%
    mutate(total=sum(totales, na.rm=T)) %>%
    ungroup() %>%
    mutate(porc=round((totales/total)*100,2)) %>%
    mutate(rango_real=case_when(rango=="De 18 a 25 años" ~ 5,
                                rango=="De 26 a 35 años" ~ 4,
                                rango=="De 36 a 45 años" ~ 3,
                                rango=="De 46 a 60 años" ~ 2,
                                TRUE ~ 1),
           rango_real=factor(rango_real, label=c("Mas de 60 años", "De 46 a 60 años",
                                                 "De 36 a 45 años", "De 26 a 35 años",
                                                 "De 18 a 25 años"))) %>%
    left_join(tree, by=c("var", "sexo"))

  ggplot(data=stack, aes(x = var, y= porc, fill=rango_real)) +
    geom_bar(stat="identity", alpha=0.6) +
    geom_text(aes(label=paste(paste0(porc, "%"), paste0(porc_confianza, "%"), sep="\n")),
              position=position_stack(vjust=0.5))+
    facet_grid(~sexo) +
    scale_fill_brewer(palette="Spectral", direction=-1)+
    labs(x="",
         y="",
         title="¿Cuánta confianza le inspiran los jueces?",
         subtitle = paste(tempo$nom_ent,
                          paste0(" mayores a 18 años según nivel de confianza en la autoridad"),
                          sep=" - ") ,
         caption="INEGI: ENVIPE 2020") +
    theme_minimal()+
    theme(text =  element_text(family="Georgia", size=20,color = "black"))
  ggsave(here("out", "Vic","labels",paste(unique(tempo$nom_ent),"jueces_lab.svg", sep="_")),
         width = 18, height = 10,units = "in")

  rm(tempo, stack, tree)
  setTxtProgressBar(pb, i)

}





