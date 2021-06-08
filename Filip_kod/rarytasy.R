# Wykorzystywane przeze mnie biblioteki
library(sqldf)
library(dplyr, warn.conflicts = FALSE)
library(compare)
library(data.table)
library(stringi)
library(formattable)
library(tidyr)
library(htmltools)
library(webshot) 

customGreen = "#DeF7E9"
customGreen0 = "#71CA97"

customBlue = "#ade2ff"
customBlue0 = "#627dff"

# Ustawienie potrzebnych opcji
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# Ustawianie zmiennych (data.frame)
# Badges_fitness <- read.csv("./fitness/Badges.csv")
# Comments_fitness <- read.csv("./fitness/Comments.csv")
# PostHistory_fitness <- read.csv("./fitness/PostHistory.csv")
PostLinks_fitness <- read.csv("./fitness/PostLinks.csv")
Posts_fitness <- read.csv("./fitness/Posts.csv")
# Tags_fitness <- read.csv("./fitness/Tags.csv")
# Users_fitness <- read.csv("./fitness/Users.csv")
# Votes_fitness <- read.csv("./fitness/Votes.csv")
PostLinks_sport <- read.csv("./sport/PostLinks.csv")
Posts_sport <- read.csv("./sport/Posts.csv")
PostLinks_gaming <- read.csv("./gaming/PostLinks.csv")
Posts_gaming <- read.csv("./gaming/Posts.csv")




tabelka <- PostLinks_sport # %>% filter(LinkTypeId )
tabelka <- tabelka %>% group_by(RelatedPostId) %>% summarize(HowMany = n()) %>% arrange(desc(HowMany))
tabelka <- tabelka %>% slice(1:10)

TopPostsId <- as.list(tabelka %>% select(RelatedPostId))
TopPostsId <- TopPostsId[[1]]

# print(typeof(TopPostsId))
# print(TopPostsId)

tabAll <- NULL

for (id in 1:10){
  
  tab <- Posts_sport %>% filter(Id == TopPostsId[id]) %>% select(Id, Score, ViewCount, Title, Tags)
  tabAll <- rbind(tabAll, tab)
  
}
tabAll <- cbind(tabAll, tabelka %>% select(HowMany))

tabAll <- tabAll[,c(4,2,3,6)]

NewTabAllBlue <- formattable(tabAll, align = c("l", "c", "c", "c"), list(
  'Title' = formatter("span", style = style(color = "grey", font.weight = "bold")),
  'Score'= color_tile(customBlue, customBlue0),
  'ViewCount'= color_tile(customBlue, customBlue0),
  'HowMany'= color_tile(customBlue, customBlue0)
))

NewTabAllGreen <- formattable(tabAll, align = c("l", "c", "c", "c"), list(
  'Title' = formatter("span", style = style(color = "grey", font.weight = "bold")),
  'Score'= color_tile(customGreen, customGreen0),
  'ViewCount'= color_tile(customGreen, customGreen0),
  'HowMany'= color_tile(customGreen, customGreen0)
))


export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(NewTabAllGreen, "BestLinkedPostsGreen_sport.png")
export_formattable(NewTabAllBlue, "BestLinkedPostsBlue_sport.png") 


