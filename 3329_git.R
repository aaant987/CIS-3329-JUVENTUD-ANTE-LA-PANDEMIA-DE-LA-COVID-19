
library(tidyverse)
library(haven)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(patchwork)
library(ggthemes)
library(ragg)

# download data http://analisis.cis.es/formulario.jsp?dwld=/Microdatos/MD3329.zip 


# boxplot percepcion pobreza  ----------------------- 

df <- read_sav("3329.sav") 

df <- df %>% 
  mutate(EDAD_REC = (EDAD*1))
df$EDAD_REC



df$EDAD_REC[df$EDAD_REC >= 18 & df$EDAD_REC <= 34] <- 1
df$EDAD_REC[df$EDAD_REC >= 35] <- 2



glimpse(df)


df <- df %>% 
  rename(espana = P3B_1,
         coma = P3B_2,
         provin = P3B_3,
         pueblo_barrio = P3B_4) %>% 
  select(SITLAB, espana, coma, provin, pueblo_barrio, EDAD_REC) %>% 
  filter(espana <101, coma <101, provin <101, pueblo_barrio <101)


df %>% 
  group_by(SITLAB) %>% 
  pivot_longer(
    2:5,
    names_to = "lugar",
    values_to = "value"
  ) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         lugar = as.factor(lugar),
         value = as.numeric(value),
         EDAD_REC = as.factor(EDAD_REC)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja",
                            SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD_REC = case_when(EDAD_REC == "1" ~ "18-34",
                              EDAD_REC == "2" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(SITLAB, lugar, EDAD_REC) %>% 
  summarize(m = mean(value)) %>% 
  arrange(-m) 

df <- df %>% 
  group_by(SITLAB) %>% 
  pivot_longer(
    2:5,
    names_to = "lugar",
    values_to = "value"
  ) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         lugar = as.factor(lugar),
         value = as.numeric(value),
         EDAD_REC = as.factor(EDAD_REC)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja",
                            SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD_REC = case_when(EDAD_REC == "1" ~ "18-34",
                              EDAD_REC == "2" ~ ">=35")) %>% 
  drop_na(SITLAB) 

df %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(SITLAB, lugar, EDAD_REC) %>% 
  ggplot(aes(x = interaction(EDAD_REC, SITLAB), y = value, fill = interaction(EDAD_REC, SITLAB))) +
  #geom_boxplot() +
  geom_violin(adjust = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  #ggdist::stat_halfeye(adjust = 0.2, position = position_nudge(x = -.3), .width = 1,
  #point_size = 5) +
  scale_fill_manual(values=c("#7F95D5", "#7F95D5", "#7FD593", "#7FD593")) +
  stat_summary(fun=mean, colour="red", geom="text", show.legend = T, family = "Bahnschrift",
               vjust=0, aes( label=round(..y.., digits=1))) +
  #geom_jitter(alpha = 0.5, fill = "red", color = "yellow", position = position_jitter(seed = 0)) +
  labs(title = "¿Qué porcentaje de personas con edades entre 18 a 34 años cree que viven en la pobreza en su...?",
       y = "",
       x = "",
       caption = "CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  facet_wrap(
    vars(
      # Change factor level name
      fct_recode(lugar, 
                 "CCAA" = "coma",
                 "España" = "espana",
                 "Provincia" = "provin",
                 "Pueblo/Barrio" = "pueblo_barrio")%>% 
        # Change factor level order
        fct_relevel("España"))) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        panel.spacing = unit(2.5, units = "cm"),
        plot.title = element_text(family = "Bahnschrift", size = 16, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 12),
        plot.caption = element_text(color = "black", size = 10),
        legend.position = "none", 
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 10, color = "black")) 


library(car)
leveneTest(df$value ~ as.factor(df$SITLAB), data = df, center = "median") # varianza no igual pues <0.05

anova <- aov(df$value ~ as.factor(df$SITLAB))
summary(anova)

res <- t.test(df$value ~ as.factor(df$SITLAB), var.equal = F)
res

ggsave("percepcionpobreza.png", width = 15.5, height = 12, device = agg_png, dpi = 500)


# diferencia despues - antes trabaja 18-34 ------------

df <- read_sav("3329.sav")

glimpse(df)

df1 <- df %>% 
  select(PPERSONAL1, EDAD, SITLAB) %>% 
  filter(EDAD <35) %>% 
  mutate(PPERSONAL1 = as.factor(PPERSONAL1),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <=34 ~ "18-34"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%  
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 12)

df2 <- df %>% 
  select(PPERSONAL1P, EDAD, SITLAB) %>% 
  filter(EDAD <35) %>% 
  mutate(PPERSONAL1P = as.factor(PPERSONAL1P),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <=34 ~ "18-34"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1P, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 13)

data <- left_join(df1, df2, by = "id")

data <- data %>% 
  filter(id <8) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  print(n = 12) 

library(ggalt)

pdiferencias__1834_trabaja <- data %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 1.5),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 1.5),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="7"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "El acceso a\nla vivienda",
                                                "2" = "El mercado de\ntrabajo",
                                                "3" = "El consumo de\ndrogas y alcohol ",
                                                "4" = "Los problemas en\nel sistema educativo",
                                                "5" = "La falta de apoyo\na los jóvenes",
                                                "6" = "La gran competitividad\nque existe en la sociedad",
                                                "7" = "La falta de confianza\nen los jóvenes")) +
  labs(title = "",
       subtitle = "*Trabaja de 18 a 34 años",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


# parado de 18 a 34 -------------------

df <- read_sav("3329.sav")

glimpse(df)

df1 <- df %>% 
  select(PPERSONAL1, EDAD, SITLAB) %>% 
  filter(EDAD <35) %>% 
  mutate(PPERSONAL1 = as.factor(PPERSONAL1),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <=34 ~ "18-34"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 12)

df2 <- df %>% 
  select(PPERSONAL1P, EDAD, SITLAB) %>% 
  filter(EDAD <35) %>% 
  mutate(PPERSONAL1P = as.factor(PPERSONAL1P),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <=34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1P, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 13)

data <- left_join(df1, df2, by = "id")

data <- data %>% 
  filter(id <8) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  print(n = 12) 

library(ggalt)

pdiferencias__1834_parado <- data %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="antes", size = 1.5),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="actualidad", size = 1.5),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="7"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "El acceso a\nla vivienda",
                                                "2" = "El mercado de\ntrabajo",
                                                "3" = "El consumo de\ndrogas y alcohol ",
                                                "4" = "Los problemas en\nel sistema educativo",
                                                "5" = "La falta de apoyo\na los jóvenes",
                                                "6" = "La gran competitividad\nque existe en la sociedad",
                                                "7" = "La falta de confianza\nen los jóvenes")) +
  labs(title = "",
       subtitle = "*Parado de 18 a 34 años",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


# diferencias mas 35 trabaja -----------

df <- read_sav("3329.sav")

glimpse(df)

df1 <- df %>% 
  select(PPERSONAL1, EDAD, SITLAB) %>% 
  filter(EDAD >34) %>% 
  mutate(PPERSONAL1 = as.factor(PPERSONAL1),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%  
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 12)

df2 <- df %>% 
  select(PPERSONAL1P, EDAD, SITLAB) %>% 
  filter(EDAD >34) %>% 
  mutate(PPERSONAL1P = as.factor(PPERSONAL1P),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ "18-34"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1P, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 13)

data <- left_join(df1, df2, by = "id")

data <- data %>% 
  filter(id <8) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  print(n = 12) 

library(ggalt)
pdiferencias___mas35trabaja <- data %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 1.5),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 1.5),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="7"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "El acceso a\nla vivienda",
                                                "2" = "El mercado de\ntrabajo",
                                                "3" = "El consumo de\ndrogas y alcohol ",
                                                "4" = "Los problemas en\nel sistema educativo",
                                                "5" = "La falta de apoyo\na los jóvenes",
                                                "6" = "La gran competitividad\nque existe en la sociedad",
                                                "7" = "La falta de confianza\nen los jóvenes")) +
  labs(title = "",
       subtitle = "*Trabaja >= 35 años",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


# diferencias mas 35 parado ---------------------

df <- read_sav("3329.sav")

glimpse(df)

df1 <- df %>% 
  select(PPERSONAL1, EDAD, SITLAB) %>% 
  filter(EDAD >34) %>% 
  mutate(PPERSONAL1 = as.factor(PPERSONAL1),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 12)

df2 <- df %>% 
  select(PPERSONAL1P, EDAD, SITLAB) %>% 
  filter(EDAD >34) %>% 
  mutate(PPERSONAL1P = as.factor(PPERSONAL1P),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>% 
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, PPERSONAL1P, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>%
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) %>% 
  print(n = 13)

data <- left_join(df1, df2, by = "id")

data <- data %>% 
  filter(id <8) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  print(n = 12) 

library(ggalt)

pdiferencias__mas35_parado <- data %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 1.5),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 1.5),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="7"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "El acceso a\nla vivienda",
                                                "2" = "El mercado de\ntrabajo",
                                                "3" = "El consumo de\ndrogas y alcohol ",
                                                "4" = "Los problemas en\nel sistema educativo",
                                                "5" = "La falta de apoyo\na los jóvenes",
                                                "6" = "La gran competitividad\nque existe en la sociedad",
                                                "7" = "La falta de confianza\nen los jóvenes")) +
  labs(title = "",
       subtitle = "*Parado >= 35 años",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


library(patchwork) 

pdiferencias_ <- (pdiferencias__mas35_parado|pdiferencias__1834_parado) / (pdiferencias___mas35trabaja|pdiferencias__1834_trabaja)
  

  pdiferencias_ <- pdiferencias_  + plot_annotation(
    title = 'En España ¿cuál cree que es el principal problema que tenían\nlos jóvenes antes de la COVID-19 y en la actualidad?',
    subtitle = '',
    caption = 'CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pdiferencias_ <- pdiferencias_ + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                                       plot.title = element_text(color = "darkgreen", hjust = 0.5),
                                                                       plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                                       #panel.background = element_rect(fill = "gray95", color = "gray95"),
                                                                       plot.caption = element_text(color = "black"))) 

pdiferencias_ <- pdiferencias_ + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pdiferencias_ <- pdiferencias_ + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                                       plot.caption = element_text(size = 9.5, hjust = 1)))
pdiferencias_


ggsave("pdiferencias_.png", width = 15.5, height = 9, device = agg_png, dpi = 500)


# P13 principal consecuencia jovenes -------------------


df <- read_sav("3329.sav")
glimpse(df)

df <- df %>% 
  mutate(EDAD_REC = (EDAD*1))
df$EDAD_REC



df$EDAD_REC[df$EDAD_REC >= 18 & df$EDAD_REC <= 34] <- 1
df$EDAD_REC[df$EDAD_REC >= 35] <- 2

df %>% 
  select(CONSE1, EDAD_REC, SITLAB) %>% 
  filter(CONSE1 <94) %>% 
  mutate(CONSE1 = as.factor(CONSE1),
         EDAD_REC = as.factor(EDAD_REC),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja",
                            SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD_REC = case_when(EDAD_REC == "1" ~ "18-34",
                              EDAD_REC == "2" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD_REC, CONSE1, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD_REC, SITLAB) %>%
  mutate(per = n/sum(n)*100) %>% 
  arrange(-per) %>% 
  ggplot(aes(reorder(CONSE1, per), per, label = scales::percent(per))) +
  geom_col(color = "blue", fill = "blue", width = 0.9)  + 
  geom_text(
    aes(label = round(per, digits = 1)), hjust = -1, vjust = 0.5, 
    family = "Bahnschrift") + 
  facet_wrap(SITLAB ~ EDAD_REC) +
  labs(
    title = "¿Cuál es la principal consecuencia que la pandemia del\nCOVID-19 va a producir en los jóvenes...?",
    subtitle = "",
    caption = "CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur",
    x = "", 
    y = "") + 
  scale_x_discrete(labels=c("1" = "Tendrán menos\noportunidades laborales", 
                            "2" = "Aumentará la edad\nde emancipación",
                            "3" = "Se incrementará\nla brecha digital",
                            "4" = "Habrá una\nbrecha educativa",
                            "5" = "Aumentará la\ndesigualdad generacional",
                            "6" = "Habrá efectos\npsicológicos",
                            "7" = "Empeorarán las\nrelaciones familiares",
                            "8" = "Aumento de la desigualdad\nsocial, brecha económica",
                            "95" = "Otro",
                            "96" = "Todas las\nconsecuencias ",
                            "97" = "Ninguna",
                            "98" = "N.S",
                            "99" = "N.C")) +
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0,60)) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 8.5, color = "black")) +
  coord_flip()

ggsave("pconsecuencias.png", width = 15.5, height = 9, device = agg_png, dpi = 500)


# ----------------- ¿Qué dos medidas cree que deberían realizar las administraciones públicas para apoyar a los/as jóvenes?------------


df <- read_sav("3329.sav")
glimpse(df)


df1 <- df %>%
  select(P15A, EDAD, SITLAB) %>% 
  filter(EDAD >=35, P15A <9) %>% 
  mutate(P15A = as.factor(P15A),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD = case_when(EDAD == "1" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>%
  select(P15B, EDAD, SITLAB) %>% 
  filter(EDAD >=35, P15B <9) %>% 
  mutate(P15B = as.factor(P15B),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD = case_when(EDAD == "1" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df <- inner_join(df1, df2, by = "id")

pparado_mas35 <- df %>% 
  mutate(per = per1+per2) %>% 
  ggplot(aes(reorder(id, per), per, label = scales::percent(per))) +
  geom_col(color = "blue", fill = "blue")  + 
  geom_text(
    aes(label = round(per, digits = 1)), hjust = -1, vjust = 0.5, 
    family = "Bahnschrift") +
  labs(
    title = "Parado\n>= 35 años",
    subtitle = "Suma de las dos respuestas",
    caption = "",
    x = "", 
    y = "") + 
  scale_x_discrete(labels=c("1" = "Aumentar las ayudas al\nalquiler de la vivienda", 
                            "2" = "Promover viviendas\nsociales",
                            "3" = "Ofrecer más ayudas\npara encontrar empleo",
                            "4" = "Mejorar las condiciones laborales",
                            "5" = "Proporcionar más ayudas\npara los estudios",
                            "6" = "Dar más ayudas a los\njóvenes emprendedores",
                            "7" = "Proporcionar más ayudas\nfiscales y apoyo para\nla formación de familias",
                            "8" ="Ayuda monetaria tipo\nsalario social")) +
  scale_y_continuous(breaks = seq(0, 65, by = 10), limits = c(0,65)) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 8, color = "black")) +
  coord_flip()

df <- read_sav("3329.sav")
glimpse(df)


df1 <- df %>%
  select(P15A, EDAD, SITLAB) %>% 
  filter(EDAD >=35, P15A <9) %>% 
  mutate(P15A = as.factor(P15A),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja"),
         EDAD = case_when(EDAD == "1" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>%
  select(P15B, EDAD, SITLAB) %>% 
  filter(EDAD >=35, P15B <9) %>% 
  mutate(P15B = as.factor(P15B),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja"),
         EDAD = case_when(EDAD == "1" ~ ">=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df <- inner_join(df1, df2, by = "id")

ptrabaja_mas35 <- df %>% 
  mutate(per = per1+per2) %>% 
  ggplot(aes(reorder(id, per), per, label = scales::percent(per))) +
  geom_col(color = "blue", fill = "blue")  + 
  geom_text(
    aes(label = round(per, digits = 1)), hjust = -1, vjust = 0.5, 
    family = "Bahnschrift") +
  labs(
    title = "Trabaja\n>= 35 años",
    subtitle = "Suma de las dos respuestas",
    caption = "",
    x = "", 
    y = "") + 
  scale_x_discrete(labels=c("1" = "Aumentar las ayudas al\nalquiler de la vivienda", 
                            "2" = "Promover viviendas\nsociales",
                            "3" = "Ofrecer más ayudas\npara encontrar empleo",
                            "4" = "Mejorar las condiciones laborales",
                            "5" = "Proporcionar más ayudas\npara los estudios",
                            "6" = "Dar más ayudas a los\njóvenes emprendedores",
                            "7" = "Proporcionar más ayudas\nfiscales y apoyo para\nla formación de familias",
                            "8" ="Ayuda monetaria tipo\nsalario social")) +
  scale_y_continuous(breaks = seq(0, 65, by = 10), limits = c(0,65)) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 8, color = "black")) +
  coord_flip()



df <- read_sav("3329.sav")
glimpse(df)


df1 <- df %>%
  select(P15A, EDAD, SITLAB) %>% 
  filter(EDAD <=34, P15A <9) %>% 
  mutate(P15A = as.factor(P15A),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD = case_when(EDAD == "1" ~ "<=34")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>%
  select(P15B, EDAD, SITLAB) %>% 
  filter(EDAD <=34, P15B <9) %>% 
  mutate(P15B = as.factor(P15B),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado"),
         EDAD = case_when(EDAD == "1" ~ "<=34")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df <- inner_join(df1, df2, by = "id")

pparado_menos35 <- df %>% 
  mutate(per = per1+per2) %>% 
  ggplot(aes(reorder(id, per), per, label = scales::percent(per))) +
  geom_col(color = "blue", fill = "blue")  + 
  geom_text(
    aes(label = round(per, digits = 1)), hjust = -1, vjust = 0.5, 
    family = "Bahnschrift") +
  labs(
    title = "Parado\n18 a 34 años",
    subtitle = "Suma de las dos respuestas",
    caption = "",
    x = "", 
    y = "") + 
  scale_x_discrete(labels=c("1" = "Aumentar las ayudas al\nalquiler de la vivienda", 
                            "2" = "Promover viviendas\nsociales",
                            "3" = "Ofrecer más ayudas\npara encontrar empleo",
                            "4" = "Mejorar las condiciones laborales",
                            "5" = "Proporcionar más ayudas\npara los estudios",
                            "6" = "Dar más ayudas a los\njóvenes emprendedores",
                            "7" = "Proporcionar más ayudas\nfiscales y apoyo para\nla formación de familias",
                            "8" ="Ayuda monetaria tipo\nsalario social")) +
  scale_y_continuous(breaks = seq(0, 65, by = 10), limits = c(0,65)) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 8, color = "black")) +
  coord_flip()


df <- read_sav("3329.sav")
glimpse(df)


df1 <- df %>%
  select(P15A, EDAD, SITLAB) %>% 
  filter(EDAD <35, P15A <9) %>% 
  mutate(P15A = as.factor(P15A),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja"),
         EDAD = case_when(EDAD == "1" ~ "<=34")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>%
  select(P15B, EDAD, SITLAB) %>% 
  filter(EDAD <35, P15B <9) %>% 
  mutate(P15B = as.factor(P15B),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(SITLAB = case_when(SITLAB == "1" ~ "Trabaja"),
         EDAD = case_when(EDAD == "1" ~ "<=35")) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P15B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df <- inner_join(df1, df2, by = "id")

ptrabaja_menos34 <- df %>% 
  mutate(per = per1+per2) %>% 
  ggplot(aes(reorder(id, per), per, label = scales::percent(per))) +
  geom_col(color = "blue", fill = "blue")  + 
  geom_text(
    aes(label = round(per, digits = 1)), hjust = -1, vjust = 0.5, 
    family = "Bahnschrift") +
  labs(
    title = "Trabaja\n18 a 34 años",
    subtitle = "Suma de las dos respuestas",
    caption = "",
    x = "", 
    y = "") + 
  scale_x_discrete(labels=c("1" = "Aumentar las ayudas al\nalquiler de la vivienda", 
                            "2" = "Promover viviendas\nsociales",
                            "3" = "Ofrecer más ayudas\npara encontrar empleo",
                            "4" = "Mejorar las condiciones laborales",
                            "5" = "Proporcionar más ayudas\npara los estudios",
                            "6" = "Dar más ayudas a los\njóvenes emprendedores",
                            "7" = "Proporcionar más ayudas\nfiscales y apoyo para\nla formación de familias",
                            "8" ="Ayuda monetaria tipo\nsalario social")) +
  scale_y_continuous(breaks = seq(0, 65, by = 10), limits = c(0,65)) +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 12, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 8, color = "black")) +
  coord_flip()


pmedidas <- (pparado_mas35|pparado_menos35)/(ptrabaja_mas35|ptrabaja_menos34)

pmedidas <- pmedidas + plot_annotation(
  title = "¿Qué dos medidas cree que deberían realizar las\nadministraciones públicas para apoyar a los/as jóvenes?
(Multirrespuesta)",
  subtitle = '',
  caption = 'CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pmedidas <- pmedidas + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                     plot.title = element_text(color = "black", hjust = 0.5),
                                                     plot.subtitle = element_text(color = "black", hjust = 0),
                                                     #panel.background = element_rect(fill = "black", color = "black"),
                                                     plot.caption = element_text(color = "black")))

pmedidas <- pmedidas + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pmedidas <- pmedidas + plot_annotation(theme = theme(plot.title = element_text(size = 12.5, hjust = 0.5),
                                                     plot.subtitle = element_text(size = 8, hjust = 0.5),
                                                     plot.caption = element_text(size = 8, hjust = 1)))

ggsave("pmedidas.png", width = 15.5, height = 9, device = agg_png, dpi = 500) 

# p16 situacion futura mejor igual o peor -----------------------------------


df <- read_sav("3329.sav")
glimpse(df)

df1 <- df %>% 
  select(P16A, EDAD, SITLAB) %>% 
  filter(EDAD <35, P16A <4) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         P16A = as.factor(P16A)) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P16A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>% 
  select(P16B, EDAD, SITLAB) %>% 
  filter(EDAD <35, P16B <4) %>% 
  mutate(P16B = as.factor(P16B),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>%  
  group_by(EDAD, P16B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) 

data <- inner_join(df1, df2, by = "id")

library(ggalt)

pgeneraciones_18a34parado <- data %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="antes", size = 2),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="actualidad", size = 2),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="3"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "Mejor",
                                                "2" = "Peor",
                                                "3" = "Igual",
                                                "8" = "N.S",
                                                "9" = "N.C")) +
  labs(title = "Parado\n18 a 34 años",
       subtitle = "",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size =10, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


df <- read_sav("3329.sav")
glimpse(df)

df1 <- df %>% 
  select(P16A, EDAD, SITLAB) %>% 
  filter(EDAD <35, P16A <4) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         P16A = as.factor(P16A)) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P16A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>% 
  select(P16B, EDAD, SITLAB) %>% 
  filter(EDAD <35, P16B <4) %>% 
  mutate(P16B = as.factor(P16B),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>%  
  group_by(EDAD, P16B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) 

data <- inner_join(df1, df2, by = "id")

library(ggalt)

pgeneraciones_18a34trabaja <- data %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 2),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 2),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="3"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "Mejor",
                                                "2" = "Peor",
                                                "3" = "Igual",
                                                "8" = "N.S",
                                                "9" = "N.C")) +
  labs(title = "Trabaja\n18 a 34 años",
       subtitle = "",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size =10, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


pgeneraciones_18a34parado/pgeneraciones_18a34trabaja


df <- read_sav("3329.sav")
glimpse(df)

df1 <- df %>% 
  select(P16A, EDAD, SITLAB) %>% 
  filter(EDAD >34, P16A <4) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         P16A = as.factor(P16A)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P16A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>% 
  select(P16B, EDAD, SITLAB) %>% 
  filter(EDAD >34, P16B <4) %>% 
  mutate(P16B = as.factor(P16B),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "4" ~ "Parado",
                            SITLAB == "5" ~ "Parado")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>%  
  group_by(EDAD, P16B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) 

data <- inner_join(df1, df2, by = "id")

library(ggalt)

pgeneraciones_mas35parado <- data %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 2),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 2),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="3"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "Mejor",
                                                "2" = "Peor",
                                                "3" = "Igual",
                                                "8" = "N.S",
                                                "9" = "N.C")) +
  labs(title = "Parado\n >= 35 años",
       subtitle = "",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size =10, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


df <- read_sav("3329.sav")
glimpse(df)

df1 <- df %>% 
  select(P16A, EDAD, SITLAB) %>% 
  filter(EDAD >34, P16A <4) %>% 
  mutate(SITLAB = as.factor(SITLAB),
         P16A = as.factor(P16A)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>% 
  group_by(EDAD, P16A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per1 = n/sum(n)*100) %>% 
  mutate(id = row_number())

df2 <- df %>% 
  select(P16B, EDAD, SITLAB) %>% 
  filter(EDAD >34, P16B <4) %>% 
  mutate(P16B = as.factor(P16B),
         SITLAB = as.factor(SITLAB)) %>% 
  mutate(EDAD = case_when(EDAD >34 ~ ">= 35 años"),
         SITLAB = case_when(SITLAB == "1" ~ "Trabaja")) %>%
  mutate(EDAD = as.factor(EDAD)) %>% 
  drop_na(SITLAB) %>%  
  group_by(EDAD, P16B, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per2 = n/sum(n)*100) %>% 
  mutate(id = row_number()) 

data <- inner_join(df1, df2, by = "id")

library(ggalt)

pgeneraciones_mas35trabaja <- data %>% 
  mutate(per = per2-per1,
         id = as.factor(id)) %>% 
  ggplot() +
  geom_segment(aes(y = id, yend=id, x=per1, xend= per2), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=id, x=per1, xend= per2),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per1, y=id, label="", size = 2),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(data, id=="2"),
            aes(x=per2, y=id, label="", size = 2),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=95, xmax=100, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(per, 1), y=id, x=97.5), fontface="bold", size=4.5, family="Bahnschrift",
            color = "black") +
  geom_text(data=filter(data, id=="3"), 
            aes(x=97.5, y=id, label=""),
            color="black", size=2.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(expand=c(0.05,0), labels = c("1" = "Mejor",
                                                "2" = "Peor",
                                                "3" = "Igual",
                                                "8" = "N.S",
                                                "9" = "N.C")) +
  labs(title = "Trabaja\n >= 35 años",
       subtitle = "",
       caption = "",
       x = "",
       y = "") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size =10, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 10),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 


pgeneraciones <- (pgeneraciones_mas35parado|pgeneraciones_18a34parado)/(pgeneraciones_mas35trabaja|pgeneraciones_18a34trabaja)

pgeneraciones <- pgeneraciones  + plot_annotation(
  title = '¿Las nuevas generaciones van a vivir mejor, peor o igual que\nsus padres antes de la pandemia y actualmente?',
  subtitle = '',
  caption = 'CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pgeneraciones <- pgeneraciones + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                               plot.title = element_text(color = "darkgreen", hjust = 0.5),
                                                               plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                               #panel.background = element_rect(fill = "gray95", color = "gray95"),
                                                               plot.caption = element_text(color = "black"))) 

pgeneraciones <- pgeneraciones + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pgeneraciones <- pgeneraciones + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                               plot.caption = element_text(size = 8, hjust = 1)))
pgeneraciones


ggsave("pgeneraciones.png", width = 15.5, height = 9, device = agg_png, dpi = 500)

# P17 interes en la politica ----------------------------


df <- read_sav("3329.sav")
glimpse(df)

library(waffle)

p17 <- df %>% 
  select(P17, EDAD, SITLAB) %>% 
  filter(EDAD <35, P17 <6) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado")) %>% 
  drop_na(SITLAB) %>% 
  mutate(P17 = as.factor(P17),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  group_by(EDAD, P17, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per = n/sum(n)*100) %>% 
  ggplot(aes(fill=P17, values=per)) +
  geom_waffle(color = "Black", size=2, n_rows = 10, make_proportional = T) +
  facet_wrap(~SITLAB, ncol = 1) +
  scale_fill_manual(
    name = NULL,
    values = c("darkgreen", "green", "yellow", "red", "darkred"),
    labels = c("Mucho", "Bastante", "Igual", "Poco", "Nada"))+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "¿Cree que la situación actual de pandemia\ny crisis económica,\nestá contribuyendo a que a los\njóvenes les interese la política?",
    subtitle = "Jóvenes de 18 a 34 años") +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        strip.background = element_rect(fill = "gray95"),
        strip.text.x = element_text(color = "black", face = "bold"),
        panel.grid = element_line(color = "gray95"),
        axis.text.y = element_text(color = "gray95"),
        plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", size = 8.5, hjust = 1),
        axis.title = element_text(color = "black"),
        legend.text = element_text(colour="black", size=8.33, 
                                   face="bold"),
        legend.title = element_text(color = "black", size = 10),
        legend.key = element_rect(fill = "gray95", color = NA),
        legend.background = element_rect(fill="gray95",
                                         size=0.5, linetype="solid", 
                                         colour ="gray95"),
        legend.position = "top")


# ---------------- P19A ¿Qué idea le viene a la cabeza para definir el papel de los/as jóvenes en España en estos momentos? ------------

df <- read_sav("3329.sav")
glimpse(df)


p19 <- df %>% 
  select(P19A, EDAD, SITLAB) %>% 
  filter(EDAD <35, P19A <8) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado")) %>% 
  drop_na(SITLAB) %>% 
  mutate(P19A = as.factor(P19A),
         EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB)) %>% 
  group_by(EDAD, P19A, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(EDAD, SITLAB) %>% 
  mutate(per = n/sum(n)*100) %>% 
  ggplot(aes(fill=P19A, values=per)) +
  geom_waffle(color = "Black", size=2, n_rows = 10, make_proportional = T) +
  facet_wrap(~SITLAB, ncol = 1) +
  scale_fill_manual(
    name = NULL,
    values = c("blue", "pink", "aquamarine", "orange", "red", "purple", "green"),
    labels = c("Estigmatización", 
               "Desempleados, precarios", 
               "Falta de oportunidades", 
               "Sin apoyo, falta de confianza", 
               "Sin proyección de futuro",
               "Buscarse la vida",
               "El futuro del país")) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "¿Qué idea le viene a la cabeza\npara definir el papel de los\njóvenes en España en\nestos momentos?(RESPUESTA ESPONTÁNEA)",
    subtitle = "Jóvenes de 18 a 34 años") +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        strip.background = element_rect(fill = "gray95"),
        strip.text.x = element_text(color = "black", face = "bold"),
        panel.grid = element_line(color = "gray95"),
        axis.text.y = element_text(color = "gray95"),
        plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", size = 8.5, hjust = 1),
        axis.title = element_text(color = "black"),
        legend.text = element_text(colour="black", size=8.33, 
                                   face="bold"),
        legend.title = element_text(color = "black", size = 10),
        legend.key = element_rect(fill = "gray95", color = NA),
        legend.background = element_rect(fill="gray95",
                                         size=0.5, linetype="solid", 
                                         colour ="gray95"),
        legend.position = "top")

library(patchwork)

pwaffle <- p17|p19

pwaffle <- pwaffle + plot_annotation(
  title = "",
  subtitle = '',
  caption = 'CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pwaffle <- pwaffle + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                    plot.title = element_text(color = "darkgreen", hjust = 0.5),
                                                    plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                    panel.background = element_rect(fill = "gray95", color = "gray95"),
                                                    plot.caption = element_text(color = "black"))) 

pwaffle <- pwaffle + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pwaffle <- pwaffle + plot_annotation(theme = theme(plot.title = element_text(size = 28, hjust = 0.5),
                                                   plot.subtitle = element_text(size = 8, hjust = 0.5),
                                                   plot.caption = element_text(size = 9, hjust = 1)))

pwaffle

ggsave("pwaffle.png", width = 15.5, height = 12, device = agg_png, dpi = 500)

  

# P21 porcentaje que aprueba o suspende -------------------

df <- read_sav("3329.sav")

glimpse(df)

psoe <- df %>% 
  select(P21_1, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_1 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         PSOE = case_when(P21_1 <=4 ~ "Suspende",
                          P21_1 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         PSOE = as.factor(PSOE)) %>% 
  group_by(EDAD, PSOE, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 

pp <-  df %>% 
  select(P21_2, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_2 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         PP = case_when(P21_2 <=4 ~ "Suspende",
                        P21_2 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         PP = as.factor(PP)) %>% 
  group_by(EDAD, PP, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 

vox <-  df %>% 
  select(P21_3, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_3 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         VOX = case_when(P21_3 <=4 ~ "Suspende",
                         P21_3 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         VOX = as.factor(VOX)) %>% 
  group_by(EDAD, VOX, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 


up <-  df %>% 
  select(P21_4, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_4 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         UP = case_when(P21_4 <=4 ~ "Suspende",
                        P21_4 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         UP = as.factor(UP)) %>% 
  group_by(EDAD, UP, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 

cs <-  df %>% 
  select(P21_5, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_5 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         CS = case_when(P21_5 <=4 ~ "Suspende",
                        P21_5 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         CS = as.factor(CS)) %>% 
  group_by(EDAD, CS, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 

mp <-  df %>% 
  select(P21_6, EDAD, SITLAB) %>% 
  filter(EDAD <35, P21_6 <11) %>% 
  mutate(EDAD = case_when(EDAD <35 ~ "18-34"),
         SITLAB = case_when(SITLAB == 1 ~ "Trabaja",
                            SITLAB == 4 ~ "Parado"),
         MP = case_when(P21_6 <=4 ~ "Suspende",
                        P21_6 >=5 ~ "Aprueba")) %>% 
  drop_na(SITLAB) %>% 
  mutate(EDAD = as.factor(EDAD),
         SITLAB = as.factor(SITLAB),
         MP = as.factor(MP)) %>% 
  group_by(EDAD, MP, SITLAB) %>% 
  summarise(n = n()) %>% 
  group_by(SITLAB) %>% 
  mutate(per = n/sum(n)*100) 

partidos <- bind_rows(psoe, pp, vox, up, cs, mp)

partidos <- partidos %>% 
  select(-EDAD, -n) %>%
  relocate(PSOE, .after = SITLAB) %>% 
  relocate(per, .after = MP) %>% 
  gather(c(-SITLAB, -per), key = "partido", value = "nota") %>% 
  drop_na() %>% 
  print(n = 40)

pcompromiso <- partidos %>% 
  #filter(partido == "PSOE") %>% 
  ggplot(aes(fill=as.factor(nota), y=per, x=as.factor(SITLAB))) + 
  #geom_bar(position="fill", stat="identity") +
  ggridges::geom_density_ridges() +
  scale_fill_manual(name = NULL,
                    values = c("darkgreen", "darkred"),
                    labels = c("Aprueba", "Suspende")) +
  scale_y_continuous(name = "",  labels = scales::percent) +
  facet_wrap(~factor(partido, levels=c('PSOE','PP','VOX','UP', "CS", "MP"))) +
  geom_text(
    aes(label = round(per, digits = 1)), hjust = 1, vjust = 0, position = "fill", size = 5.2,
    family = "Bahnschrift") +
  coord_flip() +
  labs(
    title = "Compromiso de cada partido en la lucha contra la desigualdad y la pobreza",
    subtitle = "Jóvenes entre 18 y 34 años",
    caption = "CIS 3329 | Trabajo de campo del 25 de junio al 5 de julio de 2021 | @dataR_amateur") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "gray95"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Bahnschrift", size = 16, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(family = "Bahnschrift", size = 12),
        plot.caption = element_text(color = "black", size = 10),
        axis.text.x = element_text(hjust=0.5, size = 12, color = "black"),
        axis.text.y = element_text(hjust=0.5, size = 12, color = "black"),
        axis.title.y = element_blank(),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="gray95",
                                         size=0.5, linetype="solid", 
                                         colour ="gray95"),
        legend.position = "top")


ggsave("pcompromiso.png", width = 15.5, height = 12, device = agg_png, dpi = 500)

