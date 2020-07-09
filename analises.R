library(tidyverse)
data <- read_rds("dados/covid.rds") %>% 
  filter(regiao %in% "Brasil") %>% 
  select(data, semanaEpi, casosAcumulado, casosNovos, obitosAcumulado, obitosNovos) %>% 
  mutate(dia_semana = lubridate::wday(data, label = TRUE))


x <- data %>% select(dia_semana, casosNovos, obitosNovos) %>%
  group_by(dia_semana) %>% 
  dplyr::summarise(Media_Casos_Novos = mean(casosNovos, na.rm = TRUE),
            DP_Casos_Novos = sd(casosNovos, na.rm = TRUE),
            Max_Casos_Novos = max(casosNovos, na.rm = TRUE),
            Media_Obitos = mean(obitosNovos, na.rm = TRUE),
            DP_Obitos = mean(obitosNovos, na.rm = TRUE),
            Max_Obitos = max(obitosNovos, na.rm = TRUE))


ggstatsplot::ggwithinstats(
  data = data,
  x = "dia_semana",
  y = "obitosNovos", #casosNovos
  pairwise.comparisons = TRUE, # show pairwise comparison test results
  title = "Comparação da média de óbitos por COVID-19 por dia", #"Comparação da média de casos de COVID-19 por dia"
  ggtheme = theme_classic(),
  ggstatsplot.layer = FALSE,
  messages = FALSE,
  bf.message = FALSE,
  ylab = "Óbitos novos por Covid", # "Casos novos por Covid"
  xlab = "Dias da Semana"
)
ggsave(filename = "anova_obitos.jpeg")


data %>% 
  ggplot(aes(x = dia_semana, y = casosNovos, fill = dia_semana))+
  see::geom_violindot(fill_dots = "black", size_dots = 10000) +
  see::scale_fill_material() +
  see::theme_modern() +
  theme(legend.position = "bellow")


data %>% select(data, casosNovos, obitosNovos) %>% 
  rename(Casos = casosNovos, Obitos = obitosNovos) %>% 
  pivot_longer(-data) %>% rename(tipo_dado = name) %>% 
  ggplot(aes(x = data, y = value, color = tipo_dado)) +
  geom_rect(xmin = data$data[90], xmax = data$data[97], ymin = 0, ymax = 100*600, data = NULL, inherit.aes = FALSE, alpha = 0.01, fill = "#FA8072")+
  geom_line() +
  geom_vline(xintercept = data$data[76], linetype = "dashed") +
  scale_color_manual(values = c("blue","darkred")) +
  labs(x = "Data", 
       y = "Número de casos/óbitos novos", 
       caption = "Linha tracejada indica domingo de dia das mães.
       Período realçado em vermelho indica semana após 14 dias do dia das mães.",
       color = "Tipo de dado") +
  theme_classic() +
  theme(legend.position = "bottom")
  
data %>%  
  ggplot(aes(x = casosNovos)) +
  geom_histogram(color = "red", fill = "red", alpha = .5) +
  theme_classic()
  
p <- data %>% 
  ggplot(aes(x = dia_semana, y = casosNovos, color = dia_semana, fill = dia_semana)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter() +
  theme_classic() + theme(legend.position = "bellow")
plotly::ggplotly(p)




data_mapa <- read_rds("dados/covid.rds") %>% 
  filter(regiao != "Brasil" & is.na(municipio)) %>%
  select(estado, regiao, data, semanaEpi, casosAcumulado, obitosAcumulado, lat, lon) %>% 
  group_by(estado) %>% 
  summarise(casos = max(casosAcumulado), 
            obitos = max(obitosAcumulado), 
            data = max(data),
            semana = max(semanaEpi)) %>% 
  left_join(read_csv2("meu_shiny/capitaislatlon.csv"))


library(leaflet)
leaflet(data_mapa) %>% addTiles() %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~casos*2, popup = ~paste0("Estado: ", estado, 
                                                "<br> Casos: ", casos)) %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~obitos*10, popup = ~paste0("Estado: ", estado, 
                                                  "<br> Obitos: ", obitos), 
             color = "red")
