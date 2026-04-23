


#' ---
#' title: "Modelowanie temat?w LDA"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygl?d (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie sk?adni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis tre?ci
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false # Numeruje nag??wki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)



#' # 0. Funkcja top_terms_by_topic_LDA
# 0. Funkcja top_terms_by_topic_LDA ----
# kt?ra wczytuje tekst 
# (wektor lub kolumna tekstowa z ramki danych)
# i wizualizuje s?owa o najwi?kszej informatywno?ci
# przy metody u?yciu LDA
# dla wyznaczonej liczby temat?w



top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domy?lnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k temat?w
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usu? wszystkie puste wiersze w macierzy cz?sto?ci
  # poniewa? spowoduj? b??d dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks ka?dej unikalnej warto?ci
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbi?r tylko tych unikalnych indeks?w
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz s?owa/tematy w uporz?dkowanym formacie tidy
  
  # pobierz dziesi?? najcz?stszych s??w dla ka?dego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporz?dkuj s?owa w malej?cej kolejno?ci informatywno?ci
  
  
  
  # rysuj wykres (domy?lnie plot = TRUE)
  if(plot == T){
    # dziesi?? najcz?stszych s??w dla ka?dego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj s?owa wed?ug warto?ci beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta wed?ug tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # ka?dy temat na osobnym wykresie
      labs(x = "Terminy", y = "? (wa?no?? s?owa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    # je?li u?ytkownik nie chce wykresu
    # wtedy zwr?? list? posortowanych s??w
    return(top_terms)
  }
  
  
}




#' # Dane tekstowe
# Dane tekstowe ----

# Ustaw Working Directory!
# Za?aduj dokumenty z folderu
docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj ?cie?k?
# np.: docs <- DirSource("C:/User/Documents/textfolder2")


# Utw?rz korpus dokument?w tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje si? w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
# inspect(corpus)


# Korpus - zawarto?? przyk?adowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usuni?cie zb?dnych znak?w ----

# Zapewnienie kodowania w ca?ym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znak?w na spacj?
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usu? zb?dne znaki lub pozosta?o?ci url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze s?owem (zazw. nazwa u?ytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CA?Y adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko uko?nik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozosta?o?? po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozosta?o?ci
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "–")


# Sprawdzenie
corpus[[1]][[1]][7:9]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usuni?cie ewt. zb?dnych nazw w?asnych
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", 
                                        "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", 
                                        "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug", 
                                        "emily", "emilys", "matt", "matts", "steve", "steves", "chuck", "chucks",
                                        "joel", "joels", "mckenna", "gabriel", "gabriels", "erin", "erins",
                                        "dane", "danes", "george", "georges", "marshall", "marshalls",
                                        "cliff", "cliffs", "sathyamurthys", "robert", "roberts", "elsa", "elsas", "laura", "lauras", "ray", "rays",
                                        "throw", "alex", "alexs", "angela", "angelas", "garrett", "garrets",
                                        "sam", "sams", "michael", "michaels", "soren", "sorens", "deepika", "sergey", "sergeys", "bullock", "bullocks",
                                        "felicity", "felicitys", "victoria", "victorias", "madeline", "madelines", "andrew", "andrews",
                                        "hendrix", "hendrixs", "powell", "glenn", "glenns"
))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]



# Decyzja dotycz?ca korpusu ----
# do dalszej analizy u?yj:
#
# - corpus (oryginalny, bez stemmingu)
#




#' # Tokenizacja
# Tokenizacja ----



# Macierz cz?sto?ci TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie cz?sto?ci s??w
# 2. Zliczanie cz?sto?ci s??w ----
# (Word Frequency Count)


# Zlicz same cz?sto?ci s??w w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura s??w (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wy?wietl top 10
print(head(tdm_df, 10))



#' # 4. In?ynieria cech w modelu Bag of Words:
#' # Reprezentacja s??w i dokument?w w przestrzeni wektorowej
# 4. In?ynieria cech w modelu Bag of Words: ----
# Reprezentacja s??w i dokument?w w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podej?cie surowych cz?sto?ci s??w
# (cz?sto?? s?owa = liczba wyst?pie? w dokumencie)
# (Raw Word Counts)



#' # UCZENIE MASZYNOWE NIENADZOROWANE
# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)




#' # Modelowanie temat?w: ukryta alokacja Dirichleta
# Modelowanie temat?w: ukryta alokacja Dirichleta (LDA) ----




# Rysuj dziesi?? s??w 
# o najwi?kszej informatywno?ci wed?ug tematu
# dla wyznaczonej liczby temat?w 


# Dob?r liczby temat?w
number_of_topics = 2
top_terms_by_topic_LDA(tdm_df$word)


# Zmie? wyznaczon? liczb? temat?w
number_of_topics = 3
top_terms_by_topic_LDA(tdm_df$word)


# Zmie? wyznaczon? liczb? temat?w
number_of_topics = 4
top_terms_by_topic_LDA(tdm_df$word)


# Zmie? wyznaczon? liczb? temat?w
number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)








