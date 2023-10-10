library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(readxl) #carga de archivos
library(tidyverse) #manipulacion de datos y gráficos
library(lubridate) #manipulacion de fechas
library(scales) #manipulacion de escalas en gráficos
library(igraph) #network analysis
library(ggraph) # graficos para network analysis
library(stringi) #manipulacion de datos tipo character
library(tm) #text minning
library(stringr) #manipulacion de datos tipo character
library(parallel)
library(udpipe)
library(syuzhet)



# Base de datos -----------------------------------------------------------

setwd("~/Análisis debate")

Debate2023_agrupado <- read_csv("Debate2023_agrupado.txt", 
                                col_names = FALSE)

debate = Debate2023_agrupado %>% mutate(ind = row_number()) %>% 
  mutate(duplicado = ifelse(ind %% 2 == 0, NA, X1)) %>% 
  mutate(bandera = ifelse(ind %% 2 == 0, 1, 0)) %>% 
  mutate(tiempo = substring(duplicado, 1, 8)) %>% 
  mutate(participantes = substring(duplicado, 10, 100))

glimpse(debate)

debate1 = debate %>% filter(bandera == 0) %>% select(tiempo, participantes, duplicado)
tail(debate1)

debate2 = debate %>% filter(bandera == 1) %>% select(X1)
tail(debate2)

debate_final = cbind(debate1, debate2) %>% select(-duplicado)

#glimpse(debate_final)

#write.csv2(debate_final, "debate_final_agrupado.csv")

View(debate_final)

debate_final %>% group_by(participantes) %>% count()

# Análisis ----------------------------------------------------------------

#limpieza
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  #eliminar tilde 
  nuevo_texto <- stri_trans_general(nuevo_texto,"Latin-ASCII")
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
debate_final <- debate_final %>% mutate(texto_tokenizado = map(.x = X1,
                                                               .f = limpiar_tokenizar))

debate = debate_final %>% select(-X1) %>% unnest()
debate <- debate %>% rename(token = texto_tokenizado) 

# contar = debate %>% 
#   dplyr::group_by(token) %>% count() %>% arrange(desc(n))
# 
# stop_words = debate %>% mutate(largo = nchar(token)) %>% 
#   filter(largo < 3) 
# 
# stop_words %>% group_by(participantes) %>% count() %>% arrange(desc(n))
# 
# stop_words %>% filter(participantes %in% c("Luisa Gonzalez", "Daniel Noboa")) %>% 
#   group_by(participantes, token) %>% count(token) %>% group_by(participantes) %>%
#   top_n(10, n) %>% arrange(participantes, desc(n)) %>%
#   ggplot(aes(x = reorder(token,n), y = n, fill = participantes)) +
#   geom_col() +
#   theme_bw() +
#   labs(y = "", x = "") +
#   theme(legend.position = "none") +
#   coord_flip() +
#   facet_wrap(~participantes,scales = "free", ncol = 1, drop = TRUE)

debate$token = removeWords(debate$token, words = stopwords("spanish"))
debate = debate %>% filter(token != "")


### wordcloud 

contar = debate %>% 
  dplyr::group_by(participantes, token) %>% count() %>% mutate(largo = nchar(token)) %>% 
  filter(largo > 3) %>% arrange(desc(n))

#wordcloud2(cc, figPath = "Imagen3.png", size = 1.5)

dn = contar %>% filter(participantes == "Daniel Noboa") %>%  ungroup() %>%
  select(token, n)

wordcloud2(dn, size = 0.5)

lg = cc %>% filter(participantes == "Luisa Gonzalez") %>%  ungroup() %>%
  select(token, n)

wordcloud2(lg, size = 0.5)


# ###### palabras más largas
# 
# dn2 = contar %>% mutate(len = nchar(token)) %>% arrange(desc(n)) %>% 
#   filter(len > 4) %>% 
#   filter(participantes == "Daniel Noboa") %>%  ungroup() %>%
#   select(token, n)
# 
# wordcloud2(dn2)



##### barras conteo de palabras - geom_col()

contar %>% mutate(len = nchar(token)) %>% filter(len > 3) %>% 
  filter(participantes == "Daniel Noboa") %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(reorder(token,n), n, fill = "purple")) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()
        ) +
  xlab("") + 
  ylab("") + coord_flip() +
  ggtitle("Daniel Noboa") +
  theme_minimal() +
  scale_fill_manual(values=c("purple"))

contar %>% mutate(len = nchar(token)) %>% filter(len > 3) %>% 
  filter(participantes == "Luisa Gonzalez") %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(reorder(token,n), n, fill = "#0498da")) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()
  ) +
  xlab("") + 
  ylab("") + coord_flip() +
  ggtitle("Luisa Gonzalez") +
  theme_minimal() +
  scale_fill_manual(values=c("#0498da"))

######## grafico palabras vs el largo geom_histogram()

contar %>% mutate(len = nchar(token)) %>% 
  filter(participantes == "Luisa Gonzalez") %>%
  group_by(len) %>% count() %>% arrange(desc(n))
  ggplot(aes(len), 
         binwidth = 20) + 
  geom_col(aes(y = n, fill = "#0498da"),
                 #breaks = seq(1,20, by = 1), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) +
  ggtitle("Luisa Gonzalez") +
  theme_minimal() +
  scale_fill_manual(values=c("#0498da")) +
  scale_x_continuous(breaks = seq(1,20, by = 1))

contar %>% mutate(len = nchar(token)) %>% 
  filter(participantes == "Daniel Noboa") %>%
  group_by(len) %>% count() %>%
  ggplot(aes(len), 
         binwidth = 20) + 
  geom_col(aes(y = n, fill = "purple"),
           #breaks = seq(1,20, by = 1), 
           show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) +
  ggtitle("Daniel Noboa") +
  theme_minimal() +
  scale_fill_manual(values=c("purple")) +
  scale_x_continuous(breaks = seq(1,20, by = 1))

###### correlaciones

relacion = debate %>% group_by(participantes, token) %>% count(token) %>%
  spread(key = participantes, value = n, fill = NA, drop = TRUE)

cor.test(~ `Daniel Noboa` + `Ruth del Salto`, method = "pearson", data = relacion)

cor.test(~ `Luisa Gonzalez` + `Ruth del Salto`, method = "pearson", data = relacion)

cor.test(~ `Luisa Gonzalez` + `Daniel Noboa`, method = "pearson", data = relacion)

ggplot(relacion, aes(`Daniel Noboa`,`Ruth del Salto`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "purple") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggplot(relacion, aes(`Luisa Gonzalez`,`Ruth del Salto`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "#0498da") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggplot(relacion, aes(`Luisa Gonzalez`,`Daniel Noboa`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


###### diversidad de lexico


lexico = debate %>% group_by(participantes, tiempo) %>% 
  filter(participantes %in% c("Daniel Noboa", "Luisa Gonzalez")) %>%
  summarise(lexico = n_distinct(token)) %>% 
  arrange(desc(lexico)) %>% as.data.frame.data.frame()

lexico %>% 
  mutate(tiempo2 = as.POSIXct(hms::parse_hm(tiempo))) %>% 
  filter(participantes %in% c("Daniel Noboa")) %>% 
  ggplot(aes(tiempo2, lexico, color = participantes)) +
  geom_point(color = "purple",
             alpha = .4, #transparencia
             size = 4, #tamaño
             position = "jitter") + #posicion aleatoria de los puntos 
  stat_smooth(color = "black", se = FALSE, method = "lm") + #traza una regresión lineal - "se" = intervalos de confianza
  geom_smooth(aes(x = tiempo2, y = lexico), se = FALSE,         #traza una linea suavizada
              color = "purple", lwd = 2) +                   #"lw": grueso de la linea
  ggtitle("Daniel Noboa - Lexical Diversity") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 175)

lexico %>% 
  mutate(tiempo2 = as.POSIXct(hms::parse_hm(tiempo))) %>% 
  filter(participantes %in% c("Luisa Gonzalez")) %>% 
  ggplot(aes(tiempo2, lexico, color = participantes)) +
  geom_point(color = "#0498da",
    alpha = .4, #transparencia
    size = 4, #tamaño
    position = "jitter") + #posicion aleatoria de los puntos 
  stat_smooth(color = "black", se = FALSE, method = "lm") + #traza una regresión lineal - "se" = intervalos de confianza
  geom_smooth(aes(x = tiempo2, y = lexico), se = FALSE,         #traza una linea suavizada
              color = "#0498da", lwd = 2) +                   #"lw": grueso de la linea
  ggtitle("Luisa Gonzalez - Lexical Diversity") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 175)



###### densidad de lexico

densidad = debate %>%  group_by(participantes, tiempo) %>% 
  filter(participantes %in% c("Daniel Noboa", "Luisa Gonzalez")) %>%
  summarise(lexico = n_distinct(token)/n()) %>% 
  arrange(desc(lexico))

densidad %>% mutate(tiempo2 = as.POSIXct(hms::parse_hm(tiempo))) %>% 
  filter(participantes %in% c("Daniel Noboa")) %>% 
  ggplot(aes(tiempo2, lexico)) +
  geom_point(color = "purple",
    alpha = .4, 
    size = 4, 
    position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = tiempo2, y = lexico), se = FALSE,
              color = "purple", lwd = 2) +
  ggtitle("Daniel Noboa - Lexical Density") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))



densidad %>% mutate(tiempo2 = as.POSIXct(hms::parse_hm(tiempo))) %>% 
  filter(participantes %in% c("Luisa Gonzalez")) %>% 
  ggplot(aes(tiempo2, lexico)) +
  geom_point(color = "#0498da",
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = tiempo2, y = lexico), se = FALSE,
              color = "#0498da", lwd = 2) +
  ggtitle("Luisa Gonzalez - Lexical Density") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

######

head(debate)

spread <- debate %>% group_by(participantes, token) %>% count(token) %>% 
  spread(key = participantes, value = n, fill = 0, drop = TRUE)
unpivot <- spread %>% gather(key = "participantes", value = "n", -token)

# Selección de los parrticipanteses elonmusk y mayoredlee
unpivot <- unpivot %>% filter(participantes %in% c("Daniel Noboa",
                                                         "Luisa Gonzalez"))
# Se añade el total de palabras de cada participantes
unpivot <- unpivot %>% left_join(debate %>%
                                                 group_by(participantes) %>%
                                                 summarise(N = n()),
                                               by = "participantes")
# Cálculo de odds y log of odds de cada palabra
logOdds <- unpivot %>%  mutate(odds = (n + 1) / (N + 1))
logOdds <- logOdds %>% select(participantes, token, odds) %>% 
  spread(key = participantes, value = odds)
logOdds <- logOdds %>%  mutate(log_odds = log(`Daniel Noboa`/`Luisa Gonzalez`),
                                             abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Daniel Noboa. Si es menor, es de Luisa Gonzalez
logOdds <- logOdds %>%
  mutate(parrticipantes_frecuente = if_else(log_odds > 0,
                                   "Daniel Noboa",
                                   "Luisa Gonzalez"))
logOdds %>% arrange(desc(abs_log_odds)) %>% head() 

logOdds %>% group_by(parrticipantes_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = parrticipantes_frecuente)) +
  geom_col() +
  #labs(x = "palabra", y = "log odds ratio (Daniel Noboa / Luisa Gonzalez)") +
  coord_flip() + 
  theme_bw() + 
  scale_fill_manual(values=c("purple", "#0498da")) 

###################

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}


bigramas_dn <- debate_final %>% filter(participantes =="Daniel Noboa") %>% 
  mutate(texto = limpiar(X1)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas_dn  %>% count(bigrama, sort = TRUE)

bigrams_separados_dn <- bigramas_dn %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")

bigrams_separados_dn <- bigrams_separados  %>%
  filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
  filter(!palabra2 %in% tm::stopwords(kind="es"))

# Unión de las palabras para formar de nuevo los bigramas
bigramas_dn <- bigrams_separados_dn %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas_dn  %>% count(bigrama, sort = TRUE) %>% head(20)

graph_dn <- bigramas_dn %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 2) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph_dn, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

bigramas_dn %>% group_by(bigrama) %>% summarise(total=n()) %>% 
  arrange(desc(total)) %>%  top_n(10) %>% 
  ggplot(aes(x = reorder(bigrama,total), y = total, fill = "purple")) + 
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Daniel Noboa - Conteo de Bigramas", x = "Bigramas",
       y = "Cantidad") +
  scale_fill_manual(values=c("purple")) 

#######

bigramas_lg <- debate_final %>% filter(participantes =="Luisa Gonzalez") %>% 
  mutate(texto = limpiar(X1)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas_lg  %>% count(bigrama, sort = TRUE)

bigrams_separados_lg <- bigramas_lg %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")

bigrams_separados_lg <- bigrams_separados_lg  %>%
  filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
  filter(!palabra2 %in% tm::stopwords(kind="es"))

# Unión de las palabras para formar de nuevo los bigramas
bigramas_lg <- bigrams_separados_lg %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas_lg  %>% count(bigrama, sort = TRUE) %>% head(20)

graph_lg <- bigramas_lg %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 2) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph_lg, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

bigramas_lg %>% group_by(bigrama) %>% summarise(total=n()) %>% 
  arrange(desc(total)) %>%  top_n(10) %>% 
  ggplot(aes(x = reorder(bigrama,total), y = total, fill = "#0498da")) + 
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Luisa Gonzalez - Conteo de Bigramas", x = "Bigramas",
       y = "Cantidad") +
  scale_fill_manual(values=c("#0498da")) +
  ylim(0, 13) 
  




######## ANALISIS DE SENTIMIENTOS (BING - SENTIMIENTOS BINARIOS)

sentimientos <- get_sentiments(lexicon = "bing")

sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

sent <- inner_join(x = debate, y = sentimientos,
                          by = c("token" = "word"))

sent %>% group_by(participantes, tiempo) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(participantes) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

sent %>% mutate(tiempo2 = as.POSIXct(hms::parse_hm(tiempo))) %>% 
        filter(participantes %in% c("Daniel Noboa", "Luisa Gonzalez")) %>% 
  group_by(participantes, tiempo2) %>%
  summarise(sentimiento = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = tiempo2, y = sentimiento, color = participantes)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "tiempo") +
  facet_wrap(~ participantes, ncol = 1) +
  theme_bw() +
  scale_color_manual(values=c("purple", "#0498da")) +
  theme(legend.position = "none")
  #theme(legend.position = "none"))

sent %>% group_by(participantes, tiempo) %>%
  filter(participantes %in% c("Daniel Noboa", "Luisa Gonzalez") ) %>% 
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(participantes) %>%
  summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
            neutros = 100*sum(sentimiento_promedio == 0) / n(),
            negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -participantes) %>%
  ggplot(aes(x = participantes, y = valor, fill = sentimiento, label = trunc(valor))) + 
  geom_col(position = "dodge", color = "black", ) + 
  geom_label(aes(label = trunc(valor)),
            position = position_dodge2(width = 0.9))+
  #geom_richtext(angle = 90) +
  coord_flip() +
  theme_bw()
  

# ANALISIS DE SENTIMIENTOS ESPECIFICOS ------------------------------------


cl = makeCluster(detectCores()-1)
clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"), envir=environment())

#udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez
model = udpipe_load_model("C:/Users/ferna/OneDrive/Documentos/Análisis debate/spanish-gsd-ud-2.5-191206.udpipe")

dn_sent = debate %>% filter(participantes == "Daniel Noboa")

debate_dn = as_tibble(udpipe_annotate(model, dn_sent$token))


# Stemming
debate_dn = debate_dn %>% 
  select(token, lemma) %>% 
  filter(!is.na(lemma))


# Análisis de sentimientos
dn_sentiment_nrc = get_nrc_sentiment(debate_dn$lemma,language = "spanish", cl=cl)

#stopCluster(cl)

# Etiquetado de sentimientos
dn_sentiment_nrc = cbind(debate_dn, dn_sentiment_nrc)
dn_sentiment_nrc %>% filter(rowSums(dn_sentiment_nrc[,-c(1,2)]) > 0) %>% head()

sentimentscores_dn = data.frame(colSums(dn_sentiment_nrc %>% filter(lemma!="general") %>% 
                                       select(-token,-lemma)))
names(sentimentscores_dn) = "Score"
sentimentscores_dn = cbind("sentiment"=rownames(sentimentscores_dn),sentimentscores_dn)
rownames(sentimentscores_dn) = NULL
sentimentscores_dn = sentimentscores_dn %>% 
  mutate(sentiment = recode(sentiment, 
                            "anger"="enfado",
                            "anticipation"="anticipación",
                            "disgust"="disgusto",
                            "fear"="miedo",
                            "joy"="alegría",
                            "negative"="negativo",
                            "positive"="positivo",
                            "sadness"="tristeza",
                            "surprise"="sorpresa",
                            "trust"="confianza"))

ggplot(data=sentimentscores_dn,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  xlab("Sentimientos")+ylab("Scores")+
  ggtitle("Daniel Noboa - Análisis de Sentimientos")+
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none") +
  ylim(0, 401)

dn_sentiment_nrc %>% 
  filter(fear > 0) %>% 
  select(lemma) %>% 
  count(lemma) %>% 
  select(word=lemma, freq=n) %>% arrange(desc(freq)) %>% 
wordcloud2()




##################

lg_sent = debate %>% filter(participantes == "Luisa Gonzalez")

debate_ann = as_tibble(udpipe_annotate(model, lg_sent$token))


# Stemming
debate_lg = debate_lg %>% 
  select(token, lemma) %>% 
  filter(!is.na(lemma))


# Análisis de sentimientos
lg_sentiment_nrc = get_nrc_sentiment(debate_ann$lemma,language = "spanish", cl=cl)

#stopCluster(cl)

# Etiquetado de sentimientos
lg_sentiment_nrc = cbind(debate_ann, lg_sentiment_nrc)
lg_sentiment_nrc %>% filter(rowSums(lg_sentiment_nrc[,-c(1,2)]) > 0) %>% head()

sentimentscores_lg = data.frame(colSums(lg_sentiment_nrc %>% filter(lemma!="general") %>% 
                                       select(-token,-lemma)))
names(sentimentscores_lg) = "Score"
sentimentscores_lg = cbind("sentiment"=rownames(sentimentscores_lg),sentimentscores_lg)
rownames(sentimentscores_lg) = NULL
sentimentscores_lg = sentimentscores_lg %>% 
  mutate(sentiment = recode(sentiment, 
                            "anger"="enfado",
                            "anticipation"="anticipación",
                            "disgust"="disgusto",
                            "fear"="miedo",
                            "joy"="alegría",
                            "negative"="negativo",
                            "positive"="positivo",
                            "sadness"="tristeza",
                            "surprise"="sorpresa",
                            "trust"="confianza"))

ggplot(data=sentimentscores_lg,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  xlab("Sentimientos")+ylab("Scores")+
  ggtitle("Luisa Gonzalez - Análisis de Sentimientos")+
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none") +
  ylim(0, 401)

lg_sentiment_nrc %>% 
  filter(fear > 0) %>% 
  select(lemma) %>% 
  count(lemma) %>% 
  select(word=lemma, freq=n) %>% arrange(desc(freq))
  wordcloud2()