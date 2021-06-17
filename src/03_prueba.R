rm(list=ls())
require(pacman)
p_load(sf, tidyverse, foreign, survey,  readxl, here, janitor, ggmosaic)



# ministerios publicos ----------------------------------------------------

options(survey.lonely.psu="adjust")

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
  filter(!is.na(ap5_4_07))


design = svydesign(id=~upm_dis, strata=~est_dis, weights=~pe$fac_ele, data=pe)

# ENT = substr(pe$upm, 1, 2) #Entidad
# SEX <- pe$sexo
mean_edo_confianza = svyby(~ap5_4_07, ~pe$ent+~pe$sexo, design=design, svytotal)
mean_edo_corrupto = svyby(~ap5_5_07, ~pe$ent+~pe$sexo, design=design, svytotal)

tempo <- mean_edo_confianza %>%
  as.tibble() %>%
  select( 1:6) %>%
  gather(variable, total, 3:6) %>%
  mutate(variable=gsub("ap5_4_07", "", variable)) %>%
  rename("ent"="pe$ent", "sexo"="pe$sexo") %>%
  filter(ent=="05") %>%
  mutate(var=case_when(variable=="Mucha confianza"| variable=="Algo de confianza"~ 2,
                       variable=="Algo de desconfianza" |variable=="Mucha desconfianza" ~1)) %>%
  arrange(desc(var)) %>%
  group_by(sexo,var) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  mutate(var=factor(var, label=c("Mucha o algo de desconfianza", "Mucha o algo de confianza")))



ggplot(data = tempo) +
  geom_mosaic(aes(x = product(sexo),weight=totales, fill = var)) +
  scale_fill_manual(values = c("#377eb8", "#4daf4a")) +
  labs(x="",
       y="",
       title="¿Cuánta confianza le inspiran los jueces?",
       caption="INEGI: ENVIPE 2020") +
  theme_bw()+
  theme(legend.position = "none")


