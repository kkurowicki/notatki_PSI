# ---
# title: "Text Mining: Model Bag of Words - textfolder"
# author: "Autor: Gemini"
# date: "`r Sys.Date()`"
# output: html_document
# ---

# 1. £adowanie pakietów ----
library(tm)
library(SnowballC)
library(cluster)
library(wordcloud)
library(factoextra)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(DT)

# 2. £adowanie danych ----
# Upewnij siê, ¿e folder "textfolder" jest w Twoim Working Directory (getwd())
# Jeœli nie chcesz zmieniaæ nazwy folderu, u¿ywamy "textfolder"
if(!dir.exists("textfolder")) stop("Folder 'textfolder' nie istnieje!")
docs <- DirSource("textfolder")
corpus <- VCorpus(docs)

# 3. Przetwarzanie i oczyszczanie tekstu ----
# Zamiana na UTF-8 i czyszczenie znaków specjalnych
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "@|\\||[ \t]{2,}|(s?)(f|ht)tp(s?)://\\S+\\b|http\\w*|/|RT|via|www|~")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Usuniêcie specyficznych s³ów (dodaj w³asne, jeœli trzeba)
custom_stop <- c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", "javi", "javis", "emily", "matt", "chuck", "alice",
                 "reed", "josh", "joshs", "elliot", "elliots", "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug") 
corpus <- tm_map(corpus, removeWords, custom_stop)

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# KLUCZOWA ZMIANA: U¿ywamy oczyszczonego korpusu bezpoœrednio (pomijamy stemming)
final_corpus <- corpus

# 4. Tokenizacja i macierze czêstoœci ----
dtm <- DocumentTermMatrix(final_corpus)
dtm_m <- as.matrix(dtm)

# Zliczanie czêstoœci do analizy EDA
v <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v), freq = v)

# 5. Klastrowanie K-means ----
# Dobór liczby klastrów (Metoda sylwetki)
fviz_nbclust(dtm_m, kmeans, method = "silhouette") +
  labs(title = "Optymalna liczba klastrów")

# Wykonanie klastrowania dla k = 3 (mo¿esz zmieniæ na wynik z wykresu wy¿ej)
set.seed(123)
k <- 3
klastrowanie <- kmeans(dtm_m, centers = k)

# 6. Wizualizacja i Wyniki ----

# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point", main = "Klastry dokumentów w textfolder")

# Przygotowanie tabeli podsumowuj¹cej
cluster_info_df <- do.call(rbind, lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:min(5, length(word_freq))], collapse = ", ")
  data.frame(Klaster = i, Liczba_dok = length(cluster_docs_idx), Top_5_Words = top_words)
}))

# Tabela przypisania dokumentów
documents_clusters <- data.frame(
  Dokument = names(final_corpus),
  Klaster = klastrowanie$cluster
) %>% left_join(cluster_info_df, by = "Klaster")

datatable(documents_clusters, caption = "Podsumowanie klastrów dla textfolder")

# Chmury s³ów dla klastrów
par(mfrow=c(1, k))
for (i in 1:k) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  word_freq <- colSums(dtm_m[cluster_docs_idx, , drop = FALSE])
  wordcloud(names(word_freq), freq = word_freq, max.words = 20, 
            colors = brewer.pal(8, "Dark2"), main = paste("Klaster", i))
}


#UWAGA kod nie chcia³ mi dzia³aæ, lekko go zmieni³ gemini
