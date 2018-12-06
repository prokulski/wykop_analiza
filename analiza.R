library(tidyverse)
library(lubridate)

library(ggrepel)

library(tidytext)
library(wordcloud)


library(urltools)

library(randomForest)


FILE <- "~/PythonProjects/Wykop/plik.csv"

wykopy <- data.table::fread(FILE,
                            col.names = c("id", "wykopy", "nick", "czas",
                                          "tag", "tytul", "link", "opis")) %>%
  select(-id) %>%
  mutate(czas = ymd_hms(czas)) %>%
  mutate(tag = gsub("##", "#", tag)) %>%
  filter(czas < "2018-12-01", tag != "#") %>%
  mutate(link_domain = domain(link)) %>%
  mutate(link_domain = str_replace_all(link_domain, "^www\\.|^m\\.", "")) %>%
  mutate(link_domain = if_else(link_domain == "youtu.be", "youtube.com", link_domain)) %>%
  as_tibble()

pl_stop_words <- read_lines("~/RProjects/!polimorfologik/polish_stopwords.txt") %>%
  c(., "km", "tys", "mln", "mld", "vs", "r", "of", "the", "eng", "ws", "and")



# liczba znalezisk wg czasu
wykopy %>%
  distinct(czas, link) %>%
  mutate(data = as_date(czas)) %>%
  count(data) %>%
  ggplot() +
  geom_point(aes(data, n),
             size = 0.1, alpha = 0.15) +
  geom_smooth(aes(data, n))


# najwyżej wykopane
tabela <- wykopy %>%
  distinct(wykopy, tytul, link, czas, opis) %>%
  top_n(20, wykopy)

tabela %>%
  arrange(wykopy) %>%
  mutate(tytul = fct_inorder(tytul)) %>%
  ggplot() +
  geom_col(aes(tytul, wykopy)) +
  coord_flip()

tabela %>%
  arrange(czas) %>%
  select(tytul, link, opis, wykopy)


# najniżej wykopane
tabela <- wykopy %>%
  distinct(wykopy, tytul, link, czas, opis) %>%
  top_n(-20, wykopy)


tabela %>%
  arrange(wykopy) %>%
  mutate(tytul = fct_inorder(tytul)) %>%
  ggplot() +
  geom_col(aes(tytul, wykopy)) +
  coord_flip()

tabela %>%
  arrange(czas) %>%
  select(tytul, link, opis, wykopy)




# najpopularniejsze tagi (najczesciej uzywane)
wykopy %>%
  distinct(link, tag) %>%
  # liczba tagów
  count(tag) %>%
  mutate(p = 100*n/sum(n)) %>%
  top_n(30, p) %>%
  arrange(p) %>%
  mutate(tag = fct_inorder(tag)) %>%
  ggplot() +
  geom_col(aes(tag, p)) +
  coord_flip()



# tagi z największą srednia  liczbą wykopów
wykopy %>%
  distinct(link, wykopy, tag) %>%
  # średnia liczba wykopów na tag
  group_by(tag) %>%
  summarise(s = mean(wykopy)) %>%
  ungroup() %>%
  # top 30 takich tagów
  top_n(30, s) %>%
  arrange(s) %>%
  mutate(tag = fct_inorder(tag)) %>%
  ggplot() +
  geom_col(aes(tag, s)) +
  coord_flip()



# tagi, które najczęściej wygrywały top w danym dniu
plot_data <- wykopy %>%
  distinct(czas, wykopy, tag) %>%
  mutate(czas = floor_date(czas, unit = "days")) %>%
  # średnia liczba wykopów per tag w danym dniu
  group_by(czas, tag) %>%
  summarise(s = mean(wykopy)) %>%
  ungroup() %>%
  # jeden tag z najwyższą średnią z danego dnia
  group_by(czas) %>%
  top_n(1, s) %>%
  ungroup() %>%
  # ile razy tag wygrał dzień?
  group_by(tag) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  # do wykresu bierzemy tylko górne 10% zwycięzców dni
  filter(n >= quantile(n, 0.9))

plot_data %>%
  arrange(n) %>%
  mutate(tag = fct_inorder(paste0(tag, " (", n, ")"))) %>%
  ggplot() +
  geom_point(aes(czas, tag))


# co wpadlo w #afera?
afera_daty <- plot_data %>%
  filter(tag == "#afera") %>%
  pull(czas) %>%
  as_date()

wykopy %>%
  filter(tag == "#afera") %>%
  distinct(link, .keep_all = TRUE) %>%
  mutate(data = as_date(czas)) %>%
  filter(data %in% afera_daty) %>%
  group_by(data) %>%
  top_n(1, wykopy) %>%
  ungroup() %>%
  arrange(data) %>%
  select(tytul, link, opis, data, wykopy)




# dodawacze najlepszych znalezisk (wg sredniej wykopow dla ich znalezisk)
plot_data <- wykopy %>%
  distinct(link, wykopy, nick) %>%
  group_by(nick) %>%
  summarise(s = mean(wykopy),
            n = n()) %>%
  ungroup()

# nicki z najwyzsza siernia wykopow
plot_data %>%
  top_n(20, s) %>%
  arrange(s) %>%
  mutate(nick = fct_inorder(paste0(nick, " (", n, ")"))) %>%
  ggplot() +
  geom_col(aes(nick, s)) +
  coord_flip()

# nicki z top 100 ktore trafili na glowna i z nich top 20 z najwyzsza srednia
plot_data %>%
  top_n(100, n) %>%
  top_n(20, s) %>%
  arrange(s) %>%
  mutate(nick = fct_inorder(paste0(nick, " (", n, ")"))) %>%
  ggplot() +
  geom_col(aes(nick, s)) +
  coord_flip()


# najczęscie na głownej lądują wykopy od...
wykopy %>%
  distinct(link,  nick) %>%
  count(nick) %>%
  mutate(p = 100*n/sum(n)) %>%
  top_n(20, p) %>%
  arrange(p) %>%
  mutate(nick = fct_inorder(nick)) %>%
  ggplot() +
  geom_col(aes(nick, p)) +
  coord_flip()


# kto wygrywa najczęściej dzienne zestawienia
wykopy %>%
  distinct(czas, wykopy, nick) %>%
  mutate(czas = floor_date(czas, unit = "days")) %>%
  group_by(czas, nick) %>%
  summarise(s = mean(wykopy)) %>%
  ungroup() %>%
  group_by(czas) %>%
  top_n(1, s) %>%
  ungroup() %>%
  group_by(nick) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(-n) %>%
  filter(n >= 4) %>%
  ggplot() +
  geom_point(aes(czas, nick))



# kiedy dodawane są wykopy trafiające na główną?
wykopy %>%
  distinct(czas, wykopy, tag, link) %>%
  mutate(hour = hour(czas)) %>%
  group_by(hour) %>%
  summarise(s = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_area(aes(hour, s), fill = "lightgreen", color = "darkgreen")


# czy pora dodania ma wpływ na liczbę wykopów?
wykopy %>%
  distinct(czas, wykopy, link) %>%
  # bez ekstremów
  filter(wykopy > mean(wykopy) - 2 * sd(wykopy),
         wykopy < mean(wykopy) + 2 * sd(wykopy)) %>%
  mutate(hour = hour(czas),
         wday = wday(czas, week_start = 1)) %>%

  group_by(wday, hour) %>%
  summarise(s = mean(wykopy)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(wday, hour, fill = s)) +
  scale_y_reverse()



# kiedy popularne były jakie tagi z 30 najpopularniejszymi tagami?
top_tags <- wykopy %>%
  distinct(link,  tag) %>%
  count(tag) %>%
  top_n(30, n) %>%
  pull(tag)

wykopy %>%
  distinct(czas, wykopy, tag, link) %>%
  filter(tag %in% top_tags) %>%
  mutate(czas = floor_date(czas, unit = "weeks")) %>%
  count(tag, czas) %>%
  group_by(tag) %>%
  mutate(n = 100*n/sum(n)) %>%
  ungroup() %>%
  arrange(desc(tag)) %>%
  mutate(tag = fct_inorder(tag)) %>%
  ggplot() +
  geom_tile(aes(czas, tag, fill = n), show.legend = FALSE) +
  scale_fill_distiller(palette = "Reds", direction = 1)




# z jakich serwisow (domen) trafiaja na glowna?
domeny <- wykopy %>%
  distinct(link, link_domain, wykopy) %>%
  group_by(link_domain) %>%
  summarise(n = n(), s = mean(wykopy)) %>%
  ungroup()

# ile znalezisk na głównej z danej domeny?
domeny %>%
  top_n(30, n) %>%
  arrange(n) %>%
  mutate(link_domain = fct_inorder(link_domain)) %>%
  ggplot() +
  geom_col(aes(link_domain, n)) +
  coord_flip()

# ile średnio wykopów na domene ( z tych ktorych bylo w top 100 domen)
domeny %>%
  top_n(100, n) %>%
  top_n(30, s) %>%
  arrange(s) %>%
  mutate(link_domain = sprintf("%s (%d)", link_domain, n)) %>%
  mutate(link_domain = fct_inorder(link_domain)) %>%
  ggplot() +
  geom_col(aes(link_domain, s)) +
  coord_flip()



# chmurka słów z opisów i tytułów
tytul_words <- wykopy %>%
  distinct(tytul) %>%
  unnest_tokens(words, tytul, token = "words") %>%
  filter(!words %in% pl_stop_words) %>%
  count(words, sort = TRUE) %>%
  filter(is.na(as.numeric(words))) %>%
  filter(str_length(words) > 2)

wordcloud(tytul_words$words, tytul_words$n,
          max.words = 200, scale = c(0.5, 1.5),
          colors = rev(RColorBrewer::brewer.pal(9, "Reds")))



opis_words <- wykopy %>%
  distinct(opis) %>%
  unnest_tokens(words, opis, token = "words") %>%
  filter(!words %in% pl_stop_words) %>%
  count(words, sort = TRUE) %>%
  filter(is.na(as.numeric(words))) %>%
  filter(str_length(words) > 2)

wordcloud(opis_words$words, opis_words$n,
          max.words = 200, scale = c(0.5, 1.5),
          colors = rev(RColorBrewer::brewer.pal(9, "Reds")))




# graf tagów - jak się łączą ze sobą?

link_tag <- wykopy %>% select(link, tag)

g_df <- left_join(link_tag, link_tag, by = "link") %>%
  count(tag.x, tag.y, sort = T) %>%
  filter(tag.x != tag.y) %>%
  filter(n >= quantile(n, 0.975)) %>%
  mutate(tagA = if_else(tag.x < tag.y, tag.x, tag.y),
         tagB = if_else(tag.x < tag.y, tag.y, tag.x)) %>%
  select(tagA, tagB, n) %>%
  distinct() %>%
  top_n(500, n)

library(igraph)


g <- graph_from_data_frame(g_df, directed = TRUE)
g <- set_edge_attr(g, "weight", value = g_df$n)

plot(g,
     edge.width = E(g)$weight/500,
     vertex.size = 5,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0,
     layout = layout_with_kk)


# zmiana grafu na obiekt D3
library(networkD3)

# przygotowanie interaktywnego grafu
D3_network_LM <- simpleNetwork(g_df,
                               Source = "tagA", Target = "tagB",
                               linkColour = "#bbb", nodeColour = "#000",
                               opacity = 0.8, zoom = TRUE,
                               fontSize = 15, fontFamily = "sans-serif",
                               height = 500, width = 500)

# zapisanie do HTMLa
saveNetwork(D3_network_LM, "D3_LM.html", selfcontained = TRUE)


# Stopień węzła
# Stopień Węzła lub stopień koncentracji opisuje, jak bardzo “centralny” jest węzeł sieci, czyli ile ma wchodzących i wychodzących krawędzi, albo inaczej mówiąc – ile innych węzłów jest z nim bezpośrednio połączonych (za pośrednictwem jednej krawędzi).
degree(g, mode = "total") %>%
  sort(decreasing = TRUE) %>%
  .[1:30] %>%
  as.data.frame() %>%
  rownames_to_column("tag") %>%
  set_names(c("edge", "stopien")) %>%
  arrange(stopien) %>%
  mutate(edge = fct_inorder(edge)) %>%
  ggplot() +
  geom_col(aes(edge, stopien)) +
  coord_flip()


wybrany_tag <- "#4konserwy"

g_df %>%
  filter(tagA == wybrany_tag | tagB == wybrany_tag) %>%
  mutate(tag = if_else(tagA ==  wybrany_tag, tagB, tagA)) %>%
  top_n(30, n) %>%
  arrange(n) %>%
  mutate(tag = fct_inorder(tag)) %>%
  ggplot() +
  geom_col(aes(tag, n)) +
  coord_flip()

wykopy %>%
  filter(tag == wybrany_tag) %>%
  select(tytul, opis, wykopy, link) %>%
  distinct() %>%
  top_n(20, wykopy) %>%
  arrange(desc(wykopy))


# Centralność (ang. betweenness)
# W przypadku krawędzi – są to te najbardziej istotne połączenia, “mosty” przenoszące informacje, łączące grupy.

edges <- tibble(edge = attr(E(g), "vnames") %>% gsub("|", " - ", ., fixed = TRUE))
edges$betweenness <- edge_betweenness(g, directed = TRUE)
edges %>%
  top_n(30, betweenness) %>%
  arrange(betweenness) %>%
  mutate(edge = fct_inorder(edge)) %>%
  ggplot() +
  geom_col(aes(edge, betweenness)) +
  coord_flip()


wc <- cluster_walktrap(g)
V(g)$class <- wc$membership

plot(g,
     edge.width = E(g)$weight/500,
     vertex.size = 5,
     vertex.color = V(g)$class,
     vertex.label = NA,
     edge.arrow.size = 0,
     layout = layout_with_kk)

# Modułowość to miara struktury sieci. Została zaprojektowana do pomiaru siły podziału sieci na moduły (zwane również grupami, klastrami lub społecznościami). Sieci o wysokiej modularności mają gęste połączenia między węzłami w obrębie modułów, ale rzadkie połączenia między węzłami w różnych modułach. Modułowość jest często wykorzystywana w metodach optymalizacyjnych do wykrywania struktury społeczności w sieciach.

plot_data <- tibble(names = wc$names,
                    membership = wc$membership,
                    modularity = wc$modularity) %>%
  group_by(membership) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= quantile(n, 0.5))

plot_data %>%
  arrange(modularity) %>%
  mutate(names = fct_inorder(names)) %>%
  ggplot() +
  geom_col(aes(names, modularity, fill = as.factor(membership)), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~membership, scales="free_y")

plot_data %>%
  arrange(membership, desc(modularity))



## model
model_data <- wykopy %>%
  filter(wykopy > mean(wykopy) - 2 * sd(wykopy),
         wykopy < mean(wykopy) + 2 * sd(wykopy)) %>%
  select(wykopy, nick, tag, tytul, link, opis) %>%
  mutate(nick = str_replace_all(nick, "[:punct:]", ""),
         tag = str_replace_all(tag, "[:punct:]", ""))

N_cols <- 30


# tagi

top_50_tags <- model_data %>% count(tag) %>% top_n(N_cols, n) %>% arrange(desc(n))
top_50_tags <- model_data %>%
  select(link, tag) %>%
  filter(tag %in% top_50_tags$tag) %>%
  distinct() %>%
  mutate(val = 1) %>%
  mutate(tag = paste0("tag_", tag)) %>%
  spread(tag, val, fill = 0)


# nicki

top_50_nicks <- model_data %>% distinct(link, nick) %>% count(nick) %>% top_n(N_cols, n) %>% arrange(desc(n))
top_50_nicks <- model_data %>%
  select(link, nick) %>%
  filter(nick %in% top_50_nicks$nick) %>%
  distinct() %>%
  mutate(val = 1) %>%
  mutate(nick = paste0("nick_", nick)) %>%
  spread(nick, val, fill = 0)


# tytuły

title_words <- model_data %>%
  distinct(link, tytul) %>%
  unnest_tokens(words, tytul, token = "words") %>%
  filter(!words %in% pl_stop_words) %>%
  filter(is.na(as.numeric(words))) %>%
  filter(str_length(words) > 2) %>%
  count(link, words)

top_50_title_words <- title_words %>%
  count(words) %>%
  top_n(N_cols, nn) %>%
  arrange(desc(nn))

title_words <- title_words %>%
  filter(words %in% top_50_title_words$words) %>%
  mutate(words = paste0("title_", words)) %>%
  spread(words, n, fill = 0)


# opisy

desc_words <- model_data %>%
  distinct(link, tytul) %>%
  unnest_tokens(words, tytul, token = "words") %>%
  filter(!words %in% pl_stop_words) %>%
  filter(is.na(as.numeric(words))) %>%
  filter(str_length(words) > 2) %>%
  count(link, words)

top_50_desc_words <- desc_words %>%
  count(words) %>%
  top_n(N_cols, nn) %>%
  arrange(desc(nn))

desc_words <- desc_words %>%
  filter(words %in% top_50_title_words$words) %>%
  mutate(words = paste0("desc_", words)) %>%
  spread(words, n, fill = 0)



model_data_train <- inner_join(top_50_tags, top_50_nicks, by = "link") %>%
  inner_join(title_words, by = "link") %>%
  inner_join(desc_words, by = "link") %>%
  inner_join(model_data %>% select(link, wykopy), by = "link") %>%
  select(link, wykopy, everything())


model_rf <- randomForest(wykopy ~ .,
                         data = model_data_train[, 2:ncol(model_data_train)],
                         importance = TRUE,
                         ntree = 100, mtry = 5)

model_rf

plot(model_rf)

varImpPlot(model_rf)

importance(model_rf)



model_lm <-lm(wykopy ~ ., data = model_data_train[, 2:ncol(model_data_train)])


summary(model_lm)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  filter(!variable %in% c("(Intercept)", "nick_wykoppl", "nick_youtube", "nick_youtubecom")) %>%
  separate(variable, c("typ", "value")) %>%
  set_names(c("typ", "value", "Estimate", "StdError", "t_value", "p_value")) %>%
  as_tibble() %>%
  group_by(typ) %>%
  top_n(-10, p_value) %>%
  ungroup() %>%
  arrange(typ, p_value)


pca <- princomp(model_data_train[, 3:ncol(model_data_train)])
pca_df <- as_tibble(pca$scores[, 1:2]) %>% set_names(c("PC1", "PC2"))
pca_df$cluster <- kmeans(pca_df, centers = 2)$cluster
pca_df$link <- model_data_train$link

pca_df <- distinct(pca_df) %>%
  mutate(domain = domain(link)) %>%
  mutate(domain = str_replace_all(domain, "^www\\.|^m\\.", "")) %>%
  mutate(domain = if_else(domain == "youtu.be", "youtube.com", domain))

ggplot(pca_df) + geom_point(aes(PC1, PC2, color = as.factor(cluster)))


pca_df %>%
  count(domain, cluster) %>%
  filter(n != 1) %>%
  group_by(domain) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(p != 100) %>%
  group_by(cluster) %>%
  top_n(10, p) %>%
  ungroup() %>%
  arrange(cluster, desc(p), domain) %>%
  select(cluster, domain, p)

