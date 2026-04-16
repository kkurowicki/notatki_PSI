#' ---
#' title: "Zaj_8: Klastrowanie - textfolder3"
#' author: "Autor: Kamil Kurowicki"
#' date: "r Sys.Date()"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide
#'     number_sections: true
#' ---

# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury s³ów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele

# Dane tekstowe ----

# Ustaw Working Directory przed uruchomieniem lub upewnij siê, ¿e folder jest w tym samym miejscu co skrypt
# Za³aduj dokumenty z folderu textfolder3
docs <- DirSource("textfolder3") 

# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)

# Korpus - inspekcja
inspect(corpus)

# 1. Przetwarzanie i oczyszczanie tekstu ----

# Zapewnienie kodowania UTF-8
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spacjê
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usuwanie zbêdnych elementów (czyszczenie)
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Usuniêcie specyficznych nazw w³asnych/zbêdnych s³ów
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug"))
corpus <- tm_map(corpus, stripWhitespace)

# --- POMINIÊTO STEMMING I STEM COMPLETION ---
# Zgodnie z poleceniem sekcja zosta³a wy³¹czona
# corpus_copy <- corpus
# corpus_stemmed <- tm_map(corpus, stemDocument)
# complete_stems <- content_transformer(function(x, dict) {
#   x <- unlist(strsplit(x, " "))
#   x <- stemCompletion(x, dictionary = corpus_copy, type="longest")
#   paste(x, collapse = " ")
# })
# corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)
# corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
# corpus_completed <- tm_map(corpus_completed, stripWhitespace)

# Do dalszej analizy u¿ywamy obiektu 'corpus' (oczyszczony, bez stemmingu)
final_corpus <- corpus

# Tokenizacja i Macierze ----

# Macierz DTM (dokumenty = wiersze)
dtm <- DocumentTermMatrix(final_corpus)
dtm_m <- as.matrix(dtm)

# 2. Zliczanie czêstoœci s³ów ----
v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)

# 3. Eksploracyjna analiza danych (EDA) ----

# Chmura s³ów dla ca³ego korpusu
wordcloud(words = dtm_df$word, freq = dtm_df$freq, min.freq = 5, 
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)

# 4. Klastrowanie k-œrednich (k-means) ----

# Wybór optymalnej liczby klastrów
fviz_nbclust(dtm_m, kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")

set.seed(123)

# PÊTLA DLA RÓ¯NYCH WARTOŒCI K (2, 3, 4)
for (k in 2:4) {
  cat("\n\n### Analiza dla k =", k, "clusters\n")
  
  klastrowanie <- kmeans(dtm_m, centers = k)
  
  # Wizualizacja
  print(fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
                     geom = "point", main = paste("Klastry dokumentów dla k =", k)))
  
  # Przygotowanie informacji o klastrach
  cluster_info <- lapply(1:k, function(i) {
    cluster_docs_idx <- which(klastrowanie$cluster == i)
    cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
    word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
    top_words <- paste(names(word_freq)[1:min(5, length(word_freq))], collapse = ", ")
    data.frame(Klaster = i, Liczba_dok = length(cluster_docs_idx), Top_5_s³ów = top_words)
  })
  
  cluster_info_df <- do.call(rbind, cluster_info)
  
  # Tabela przypisania
  documents_clusters <- data.frame(
    Dokument = names(final_corpus),
    Klaster = klastrowanie$cluster
  ) %>% left_join(cluster_info_df, by = "Klaster")
  
  print(datatable(documents_clusters, caption = paste("Podsumowanie dla k =", k)))
  
  # Chmury s³ów dla klastrów
  par(mfrow=c(1, k))
  for (i in 1:k) {
    cluster_docs_idx <- which(klastrowanie$cluster == i)
    cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
    word_freq <- colSums(cluster_docs)
    wordcloud(names(word_freq), freq = word_freq, max.words = 10, 
              colors = brewer.pal(8, "Dark2"), main = paste("K:", i))
  }
  par(mfrow=c(1,1))
}

# ODPOWIEDZI NA PYTANIA:
# 1- Czy na podstawie wyników (otrzymanych klastrów) mo¿esz wnioskowaæ o treœci dokumentów?
# no tak œrednio ale, w klastrach 2, 3 i 4 mog¹ mówiæ coœ o przysz³ych planach bo s¹ s³owa takie jak will i going. Klaster 1 mowi coœ o metodologii IBIS.
#Klaster 1 - ibis, can, issues, knowledge, example
#Klaster 2 - can, project, will, system, work
#Klaster 3 - people, are”, will, going, know
#klaster 4 - will, going, people, country, great

# 2- Jakich ogólnie obszarów tematycznych dotycz¹ klastry oraz ile jest tych obszarów?

# 1. Zarz¹dzanie wiedz¹ i analiza problemów (S³owo kluczowe: IBIS).
# 2. Zarz¹dzanie projektami i praca systemowa (S³owa: project, system).
# 3 i 4. Tematyka polityczna (S³owa: country, great, people).



