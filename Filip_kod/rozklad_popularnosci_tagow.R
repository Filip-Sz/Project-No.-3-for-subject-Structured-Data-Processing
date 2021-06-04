# Wykorzystywane przeze mnie biblioteki
library(sqldf)
library(dplyr, warn.conflicts = FALSE)
library(compare)
library(data.table)
library(stringi)
library(stringi)
library(RColorBrewer)

# Ustawienie potrzebnych opcji
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# Ustawianie zmiennych (data.frame)
Badges_fitness <- read.csv("./fitness/Badges.csv")
Comments_fitness <- read.csv("./fitness/Comments.csv")
PostLinks_fitness <- read.csv("./fitness/PostLinks.csv")
Posts_fitness <- read.csv("./fitness/Posts.csv")
Tags_fitness <- read.csv("./fitness/Tags.csv")
Users_fitness <- read.csv("./fitness/Users.csv")
Votes_fitness <- read.csv("./fitness/Votes.csv")

Badges_sport <- read.csv("./sport/Badges.csv")
Comments_sport <- read.csv("./sport/Comments.csv")
PostLinks_sport <- read.csv("./sport/PostLinks.csv")
Posts_sport <- read.csv("./sport/Posts.csv")
Tags_sport <- read.csv("./sport/Tags.csv")
Users_sport <- read.csv("./sport/Users.csv")
Votes_sport <- read.csv("./sport/Votes.csv")




#' Rozklad_popularnosci_tagow
#'
#' @param TopTags Zmienna typu character zawierająca wybrane wcześniej tagi, których rozkład popularności badamy w okresie pomiedzy data1 a data2.
#' @param data1 dolna granica daty (wcześiejsza data) format: yyyy-mm-ddTHH:MM:SS
#' @param data2 górna granica daty (późniejsza data) format: yyyy-mm-ddTHH:MM:SS
#' @param rodzaj_pliku Ta zmienna typu character określa w jakim formacie ma zostać zapisany wykres
#' @param dane Ta zmienna będąca ramką danych mówi nam, gdzie będziemy badać popularność. \n
#' Ogólnie jest to tabela Posts, ale mamy różne fora, dlatego należy to wyróżnić.
#' @param sciezka zmienna typu character mowi nam, gdzie mamy zapisać wykres
#'
#' @return Funkcja zwraca ramkę danych zawieracąca informację o ilości wystąpień (TotalScore) danego tagu (TagName). \n
#' Całość jest posortaowana roznąco według wartości kolumny TotalScore. \n
#' Dodatkowo jest robony wykres kołowy, który jest zapisywany do pliku jpg.
#' 
#' @export
#'
#' @examples


rozklad_popularnosci_tagow <- function(TopTags, dane, data1, data2, rodzaj_pliku = "jpg", sciezka){
  
  stopifnot(is.character(data1), is.character(TopTags), is.character(data2), is.character(sciezka))
  stopifnot(rodzaj_pliku == "jpg" || rodzaj_pliku == "png" || rodzaj_pliku == "pdf")
  
  tabelaAll <- NULL # Na razie pusta ramka danych, do której będziemy przyłączać kolejne ramki danych
  # zliczające ilość wystąpeuń konkretnych tagów
  
  for(k in 1:length(TopTags)){
    
    tabela <- dane %>% filter(CreationDate > data1) # Ustalanie dolnej granicy dla kolumny CreationDate
    
    # Ustalanie górnej granicy dla kolumny CreationDate oraz wybieranie tych wierszy, gdzie występuje tag z k-tej pozycji z TopTags
    # Dodatkowo zliczamy ilośc wystąpień danego tagu (TotalScore)
    tabela <- tabela %>% filter(CreationDate < data2, stri_detect_fixed(Tags, TopTags[k])) %>%
      summarize(TotalScore = sum(ViewCount))
    
    tabela <- tabela %>% mutate(TagName = TopTags[k]) # Dodanie kolumny z nazwą tagu
    tabelaAll <- rbind(tabelaAll, tabela) # dołączenie do ramki danych tabelaAll
    
  }
  
  tabelaAll <- tabelaAll %>% arrange(desc(TotalScore)) # ustawianie kolejności malejącej według TotalScore
  
  
  if (rodzaj_pliku == "jpg"){
    napis <- paste(sciezka, "/wykresy/jpg/", sep = "")
    napis <- paste(napis, data1, sep = "")
    napis <- paste(napis, data2, sep = "_")
    napis <- paste(napis, ".jpg", sep="")
    
    jpeg(napis, width = 800, height = 800)
  }
  
  else if (rodzaj_pliku == "png"){
    napis <- paste(sciezka, "/wykresy/png/", sep = "")
    napis <- paste(napis, data1, sep = "")
    napis <- paste(napis, data2, sep = "_")
    napis <- paste(napis, ".png", sep="")
    
    png(napis, width = 800, height = 800)
  }
  
  else {
    napis <- paste(sciezka, "/wykresy/pdf/", sep = "")
    napis <- paste(napis, data1, sep = "")
    napis <- paste(napis, data2, sep = "_")
    napis <- paste(napis, ".pdf", sep="")
    
    pdf(napis)
  }
  
  # Ustawienia własnych kolorków
  myPalette <- brewer.pal(12, "Set3")
  
  # Tworzenie wykresu kołowego
  wyniki <- as.vector(tabelaAll$TotalScore)
  Slices <- wyniki
  
  Labels <- as.vector(tabelaAll$TagName)
  procenty <- round(Slices/sum(Slices)*100)
  Labels <- paste(Labels, procenty)
  Labels <- paste(Labels, "%", sep = "")
  
  tytul <- paste("Rozkład popularności wybranych tagów miedzy rokiem", substr(data1, 1, 4))
  tytul <- paste(tytul, "a", sep = " ")
  tytul <- paste(tytul, substr(data2, 1, 4), sep = " ")
  pie(Slices, labels = Labels, radius = 0.7, border = "white", col = myPalette, main = tytul)
  
  dev.off()
  
  
  # Zwracamy tabelaAll  
  return(tabelaAll)
}


TopTags_fitness <- Tags_fitness %>% filter(Count>500) %>% select(TagName)
TopTags_fitness <- as.list(TopTags_fitness)
TopTags_fitness <- TopTags_fitness[[1]]

TopTags_sport <- Tags_sport %>% filter(Count>310) %>% select(TagName)
TopTags_sport <- as.list(TopTags_sport)
TopTags_sport <- TopTags_sport[[1]]



# FITNESS

print(rozklad_popularnosci_tagow(TopTags_fitness, Posts_fitness,  "2014-01-01T00:00:00", "2015-01-01T00:00:00", "pdf", "./fitness"))
print(rozklad_popularnosci_tagow(TopTags_fitness, Posts_fitness,  "2013-01-01T00:00:00", "2014-01-01T00:00:00", "pdf", "./fitness"))
print(rozklad_popularnosci_tagow(TopTags_fitness, Posts_fitness,  "2012-01-01T00:00:00", "2013-01-01T00:00:00", "pdf", "./fitness"))
print(rozklad_popularnosci_tagow(TopTags_fitness, Posts_fitness,  "2011-01-01T00:00:00", "2012-01-01T00:00:00", "pdf", "./fitness"))







# SPORT

print(rozklad_popularnosci_tagow(TopTags_sport, Posts_sport,  "2014-01-01T00:00:00", "2015-01-01T00:00:00", "pdf", "./sport"))
print(rozklad_popularnosci_tagow(TopTags_sport, Posts_sport,  "2013-01-01T00:00:00", "2014-01-01T00:00:00", "pdf", "./sport"))
print(rozklad_popularnosci_tagow(TopTags_sport, Posts_sport,  "2012-01-01T00:00:00", "2013-01-01T00:00:00", "pdf", "./sport"))
# print(rozklad_popularnosci_tagow(TopTags_sport, Posts_sport,  "2011-01-01T00:00:00", "2012-01-01T00:00:00", "pdf", "./sport"))
# Brak danych z tego roku




