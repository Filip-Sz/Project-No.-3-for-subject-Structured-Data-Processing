# Wykorzystywane przeze mnie biblioteki
library(sqldf)
library(dplyr, warn.conflicts = FALSE)
library(compare)
library(data.table)
library(stringi)
library(RColorBrewer)

# Ustawienie potrzebnych opcji
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# Ustawianie zmiennych (data.frame)
Badges_fitness <- read.csv("./fitness/Badges.csv")
Comments_fitness <- read.csv("./fitness/Comments.csv")
PostHistory_fitness <- read.csv("./fitness/PostHistory.csv")
PostLinks_fitness <- read.csv("./fitness/PostLinks.csv")
Posts_fitness <- read.csv("./fitness/Posts.csv")
Tags_fitness <- read.csv("./fitness/Tags.csv")
Users_fitness <- read.csv("./fitness/Users.csv")
Votes_fitness <- read.csv("./fitness/Votes.csv")

Badges_sport <- read.csv("./sport/Badges.csv")
Comments_sport <- read.csv("./sport/Comments.csv")
PostHistory_sport <- read.csv("./sport/PostHistory.csv")
PostLinks_sport <- read.csv("./sport/PostLinks.csv")
Posts_sport <- read.csv("./sport/Posts.csv")
Tags_sport <- read.csv("./sport/Tags.csv")
Users_sport <- read.csv("./sport/Users.csv")
Votes_sport <- read.csv("./sport/Votes.csv")

Badges_gaming <- read.csv("./gaming/Badges.csv")
Comments_gaming <- read.csv("./gaming/Comments.csv")
PostHistory_gaming <- read.csv("./gaming/PostHistory.csv")
PostLinks_gaming <- read.csv("./gaming/PostLinks.csv")
Posts_gaming <- read.csv("./gaming/Posts.csv")
Tags_gaming <- read.csv("./gaming/Tags.csv")
Users_gaming <- read.csv("./gaming/Users.csv")
Votes_gaming <- read.csv("./gaming/Votes.csv")



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



#' Rozklad_popularnosci_tagow
#'
#' @param TopTags Zmienna typu character zawierająca wybrane wcześniej tagi, których rozkład popularności badamy w okresie pomiedzy data1 a data2.
#' @param date1 dolna granica daty (wcześiejsza data) format: yyyy-mm-ddTHH:MM:SS
#' @param date2 górna granica daty (późniejsza data) format: yyyy-mm-ddTHH:MM:SS
#' @param file_type Ta zmienna typu character określa w jakim formacie ma zostać zapisany wykres
#' @param data Ta zmienna będąca ramką danych mówi nam, gdzie będziemy badać popularność. \n
#' Ogólnie jest to tabela Posts, ale mamy różne fora, dlatego należy to wyróżnić.
#' @param path zmienna typu character mowi nam, gdzie mamy zapisać wykres
#'
#' @return Funkcja zwraca ramkę danych zawieracąca informację o ilości wystąpień (TotalScore) danego tagu (TagName). \n
#' Całość jest posortaowana roznąco według wartości kolumny TotalScore. \n
#' Dodatkowo jest robony wykres kołowy, który jest zapisywany do pliku jpg.
#' 
#' @export
#'
#' @examples


rozklad_popularnosci_tagow_view <- function(TopTags, data, date1, date2, file_type = "jpg", path){
  
  stopifnot(is.character(date1), is.character(TopTags), is.character(date2), is.character(path))
  stopifnot(file_type == "jpg" || file_type == "png" || file_type == "pdf")
  
  final_table <- NULL # Na razie pusta ramka danych, do której będziemy przyłączać kolejne ramki danych
  # zliczające ilość wystąpeuń konkretnych tagów
  
  for(k in 1:length(TopTags)){
    
    temp_table <- data %>% filter(CreationDate >= date1) # Ustalanie dolnej granicy dla kolumny CreationDate
    
    # Ustalanie górnej granicy dla kolumny CreationDate oraz wybieranie tych wierszy, gdzie występuje tag z k-tej pozycji z TopTags
    # Dodatkowo zliczamy ilośc wystąpień danego tagu (TotalScore)
    temp_table <- temp_table %>% filter(CreationDate < date2, stri_detect_fixed(Tags, TopTags[k])) %>%
    summarize(TotalScore = sum(ViewCount))
    
    temp_table <- temp_table %>% mutate(TagName = substr(TopTags[k], 2, nchar(TopTags[k])-1)) # Dodanie kolumny z nazwą tagu
    final_table <- rbind(final_table, temp_table) # dołączenie do ramki danych tabelaAll
    
  }
  
  final_table <- final_table %>% arrange(desc(TotalScore)) # ustawianie kolejności malejącej według TotalScore
  
  
  if (file_type == "jpg"){
    file_name <- paste(path, "/wykresy_view_sum/jpg/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".jpg", sep="")
    
    jpeg(file_name, width = 800, height = 800)
  }
  
  else if (file_type == "png"){
    file_name <- paste(path, "/wykresy_view_sum/png/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".png", sep="")
    
    png(file_name, width = 850, height = 600)
  }
  
  else {
    file_name <- paste(path, "/wykresy_view_sum/pdf/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".pdf", sep="")
    
    pdf(file_name)
  }
  
  # Ustawienia własnych kolorków
  myPalette <- brewer.pal(12, "Set3")
  
  # Tworzenie wykresu kołowego
  Slices <- as.vector(final_table$TotalScore)
  
  Labels <- as.vector(final_table$TagName)
  percent <- round(Slices/sum(Slices)*100)
  Labels <- paste(Labels, percent)
  Labels <- paste(Labels, "%", sep = "")
  
  title <- paste("Rozkład popularności wybranych tagów według ilości wyświetleń w roku", substr(date1, 1, 4))
  # tytul <- paste(tytul, "a", sep = " ")
  # tytul <- paste(tytul, substr(data2, 1, 4), sep = " ")
  pie(Slices, labels = Labels, radius = 0.7, border = "white", col = myPalette, main = title, cex=1.2, cex.main = 1.7 )
  
  dev.off()
  
  
  # Zwracamy tabelaAll  
  return(final_table)
}



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



rozklad_popularnosci_tagow_count <- function(TopTags, data, date1, date2, file_type = "jpg", path){
  
  stopifnot(is.character(date1), is.character(TopTags), is.character(date2), is.character(path))
  stopifnot(file_type == "jpg" || file_type == "png" || file_type == "pdf")
  
  final_table <- NULL # Na razie pusta ramka danych, do której będziemy przyłączać kolejne ramki danych
  # zliczające ilość wystąpeuń konkretnych tagów
  
  for(k in 1:length(TopTags)){
    
    temp_table <- data %>% filter(CreationDate >= date1) # Ustalanie dolnej granicy dla kolumny CreationDate
    
    # Ustalanie górnej granicy dla kolumny CreationDate oraz wybieranie tych wierszy, gdzie występuje tag z k-tej pozycji z TopTags
    # Dodatkowo zliczamy ilośc wystąpień danego tagu (TotalScore)
    temp_table <- temp_table %>% filter(CreationDate < date2, stri_detect_fixed(Tags, TopTags[k])) %>%
      summarize(TotalScore = n())
    
    temp_table <- temp_table %>% mutate(TagName = substr(TopTags[k], 2, nchar(TopTags[k])-1)) # Dodanie kolumny z nazwą tagu
    final_table <- rbind(final_table, temp_table) # dołączenie do ramki danych tabelaAll
    
  }
  
  final_table <- final_table %>% arrange(desc(TotalScore)) # ustawianie kolejności malejącej według TotalScore
  

  if (file_type == "jpg"){
    file_name <- paste(path, "/wykresy_count/jpg/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".jpg", sep="")
    
    jpeg(file_name, width = 800, height = 800)
  }
  
  else if (file_type == "png"){
    file_name <- paste(path, "/wykresy_count/png/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".png", sep="")
    
    png(file_name, width = 800, height = 800)
  }
  
  else {
    file_name <- paste(path, "/wykresy_count/pdf/", sep = "")
    file_name <- paste(file_name, date1, sep = "")
    file_name <- paste(file_name, date2, sep = "_")
    file_name <- paste(file_name, ".pdf", sep="")
    
    pdf(file_name)
  }
  
  # Ustawienia własnych kolorków
  myPalette <- brewer.pal(12, "Set3")
  
  # Tworzenie wykresu kołowego
  Slices <- as.vector(final_table$TotalScore)
  
  Labels <- as.vector(final_table$TagName)
  percent <- round(Slices/sum(Slices)*100)
  Labels <- paste(Labels, percent)
  Labels <- paste(Labels, "%", sep = "")
  
  title <- paste("Rozkład popularności wybranych tagów według ilości postów w roku", substr(date1, 1, 4))
  # tytul <- paste(tytul, "a", sep = " ")
  # tytul <- paste(tytul, substr(data2, 1, 4), sep = " ")
  pie(Slices, labels = Labels, radius = 0.7, border = "white", col = myPalette, main = title, cex=1.2, cex.main = 1.7 )
  
  dev.off()
  
  
  # Zwracamy tabelaAll  
  return(final_table)
}



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



getting_n_most_popular_tags <- function(file, n_best_tags = 10) {
  
  best_n_tags <- file %>% group_by(TagName) %>% summarise(SumCount = sum(Month_Count))
  best_n_tags <- best_n_tags %>% arrange(desc(SumCount)) %>% slice(1:n_best_tags)
  best_n_tags <- best_n_tags %>% select(TagName)
  best_n_tags <- as.list(best_n_tags)
  best_n_tags <- best_n_tags[[1]]
  best_n_tags <- paste("<", best_n_tags, sep = "")
  best_n_tags <- paste(best_n_tags, ">", sep = "")
  
  return(best_n_tags)
  
}



getting_n_most_popular_tags_v2 <- function(file, n_best_tags = 10) {
  
  best_n_tags <- file %>% arrange(desc(Count)) %>% slice(1:n_best_tags)
  best_n_tags <- best_n_tags %>% select(TagName)
  best_n_tags <- as.list(best_n_tags)
  best_n_tags <- best_n_tags[[1]]
  best_n_tags <- paste("<", best_n_tags, sep = "")
  best_n_tags <- paste(best_n_tags, ">", sep = "")
  
  return(best_n_tags)
  
}



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



# Najpopularniejsze tagi dla każdego roku osobno (top 10)

TopTags_gaming_2016 <- getting_n_most_popular_tags(read.csv("./gaming/Top_Tags/Gaming_Tags_Monthly_All_2016.csv"))
TopTags_gaming_2017 <- getting_n_most_popular_tags(read.csv("./gaming/Top_Tags/Gaming_Tags_Monthly_All_2017.csv"))
TopTags_gaming_2018 <- getting_n_most_popular_tags(read.csv("./gaming/Top_Tags/Gaming_Tags_Monthly_All_2018.csv"))
TopTags_gaming_2019 <- getting_n_most_popular_tags(read.csv("./gaming/Top_Tags/Gaming_Tags_Monthly_All_2019.csv"))
TopTags_gaming_2020 <- getting_n_most_popular_tags(read.csv("./gaming/Top_Tags/Gaming_Tags_Monthly_All_2020.csv"))


TopTags_sport_2016 <- getting_n_most_popular_tags(read.csv("./sport/Top_Tags/Sport_Tags_Monthly_All_2016.csv"))
TopTags_sport_2017 <- getting_n_most_popular_tags(read.csv("./sport/Top_Tags/Sport_Tags_Monthly_All_2017.csv"))
TopTags_sport_2018 <- getting_n_most_popular_tags(read.csv("./sport/Top_Tags/Sport_Tags_Monthly_All_2018.csv"))
TopTags_sport_2019 <- getting_n_most_popular_tags(read.csv("./sport/Top_Tags/Sport_Tags_Monthly_All_2019.csv"))
TopTags_sport_2020 <- getting_n_most_popular_tags(read.csv("./sport/Top_Tags/Sport_Tags_Monthly_All_2020.csv"))


TopTags_fitness_2016 <- getting_n_most_popular_tags(read.csv("./fitness/Top_Tags/Fitness_Tags_Monthly_All_2016.csv"))
TopTags_fitness_2017 <- getting_n_most_popular_tags(read.csv("./fitness/Top_Tags/Fitness_Tags_Monthly_All_2017.csv"))
TopTags_fitness_2018 <- getting_n_most_popular_tags(read.csv("./fitness/Top_Tags/Fitness_Tags_Monthly_All_2018.csv"))
TopTags_fitness_2019 <- getting_n_most_popular_tags(read.csv("./fitness/Top_Tags/Fitness_Tags_Monthly_All_2019.csv"))
TopTags_fitness_2020 <- getting_n_most_popular_tags(read.csv("./fitness/Top_Tags/Fitness_Tags_Monthly_All_2020.csv"))



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



# Najpopularniejsze tagi ogółem (top 10)

TopTags_fitness <- getting_n_most_popular_tags_v2(Tags_fitness)
TopTags_sport <- getting_n_most_popular_tags_v2(Tags_sport)
TopTags_gaming <- getting_n_most_popular_tags_v2(Tags_gaming)



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



# Rozkład popularności według top 10 tagów w danym roku, wykres dla każdego roku osobno

rodzaj_pliku <- "png"
sciezka <- "./fitness/Top_Tags"
data <- Posts_fitness

print(rozklad_popularnosci_tagow_view(TopTags_fitness_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))

print(rozklad_popularnosci_tagow_count(TopTags_fitness_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_fitness_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_fitness_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_fitness_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_fitness_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))




rodzaj_pliku <- "png"
sciezka <- "./sport/Top_Tags"
data <- Posts_sport

print(rozklad_popularnosci_tagow_view(TopTags_sport_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_sport_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_sport_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_sport_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_sport_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))

print(rozklad_popularnosci_tagow_count(TopTags_sport_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_sport_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_sport_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_sport_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_sport_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))




rodzaj_pliku <- "png"
sciezka <- "./gaming/Top_Tags"
data <- Posts_gaming

print(rozklad_popularnosci_tagow_view(TopTags_gaming_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_gaming_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_gaming_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_gaming_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_gaming_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))

print(rozklad_popularnosci_tagow_count(TopTags_gaming_2016, data,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_gaming_2017, data,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_gaming_2018, data,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_gaming_2019, data,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_count(TopTags_gaming_2020, data,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))



# PRZERWA
# -------------------------------------------------------------------------
# PRZERWA



# Rozkład popularności według top 10 tagów w ogółem, wykres dla każdego roku osobno

# FITNESS

rodzaj_pliku <- "png"
sciezka <- "./fitness"

print(rozklad_popularnosci_tagow_view(TopTags_fitness, Posts_fitness,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness, Posts_fitness,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness, Posts_fitness,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness, Posts_fitness,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka))
print(rozklad_popularnosci_tagow_view(TopTags_fitness, Posts_fitness,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka))

rozklad_popularnosci_tagow_count(TopTags_fitness, Posts_fitness,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_fitness, Posts_fitness,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_fitness, Posts_fitness,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_fitness, Posts_fitness,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_fitness, Posts_fitness,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka)


# SPORT

rodzaj_pliku <- "png"
sciezka <- "./sport"

rozklad_popularnosci_tagow_view(TopTags_sport, Posts_sport,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_sport, Posts_sport,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_sport, Posts_sport,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_sport, Posts_sport,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_sport, Posts_sport,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka)

rozklad_popularnosci_tagow_count(TopTags_sport, Posts_sport,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_sport, Posts_sport,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_sport, Posts_sport,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_sport, Posts_sport,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_sport, Posts_sport,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka)



# GAMING

rodzaj_pliku <- "png"
sciezka <- "./gaming"

rozklad_popularnosci_tagow_view(TopTags_gaming, Posts_gaming,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_gaming, Posts_gaming,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_gaming, Posts_gaming,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_gaming, Posts_gaming,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_view(TopTags_gaming, Posts_gaming,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka)

rozklad_popularnosci_tagow_count(TopTags_gaming, Posts_gaming,  "2016-01-01T00:00:00", "2017-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_gaming, Posts_gaming,  "2017-01-01T00:00:00", "2018-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_gaming, Posts_gaming,  "2018-01-01T00:00:00", "2019-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_gaming, Posts_gaming,  "2019-01-01T00:00:00", "2020-01-01T00:00:00", rodzaj_pliku, sciezka)
rozklad_popularnosci_tagow_count(TopTags_gaming, Posts_gaming,  "2020-01-01T00:00:00", "2021-01-01T00:00:00", rodzaj_pliku, sciezka)

