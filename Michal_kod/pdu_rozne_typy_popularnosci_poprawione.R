library("sqldf")
library("dplyr")
library("stringi")
library("formattable")
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)
SportsPosts <- read.csv("C:/Users/micha/Downloads/Sports_Posts.csv")
SportsTags <- read.csv("C:/Users/micha/Downloads/Sports_Tags.csv")
SportsComments <- read.csv("C:/Users/micha/Downloads/Sports_Comments.csv")
FitnessPosts <- read.csv("C:/Users/micha/Downloads/Fitness_Posts.csv")
FitnessTags <- read.csv("C:/Users/micha/Downloads/Fitness_Tags.csv")
FitnessComments <- read.csv("C:/Users/micha/Downloads/Fitness_Comments.csv")
GamingTags <- read.csv("C:/Users/micha/Downloads/Gaming_Tags.csv")
GamingPosts <- read.csv("C:/Users/micha/Downloads/Gaming_Posts.csv")


##USTALENIE NAJPOPULARNIEJSZYCH TAGÓW:


BestTags <- SportsTags %>% filter(X._Count > 100) %>% select(X._TagName)

TagsList <- list()
for (i in 1:nrow(BestTags)){
  TagsList[i] <- BestTags[i,]
}

BestTagsFitness <- FitnessTags %>% filter(X._Count > 200) %>% select(X._TagName)

FitnessTagsList <- list()
for (i in 1:nrow(BestTagsFitness)){
  FitnessTagsList[i] <- BestTagsFitness[i,]
}

BestTagsGaming <- GamingTags %>% filter(X._Count > 800) %>% select(X._TagName)

GamingTagsList <- list()
for (i in 1:nrow(BestTagsGaming)){
  GamingTagsList[i] <- BestTagsGaming[i,]
}
GamingTagsList


##SPORTS:


tabela_ViewCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- SportsPosts %>% filter(stri_detect_fixed(X._Tags, TagsList[i])) %>% summarise(ViewScore = sum(X._ViewCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = TagsList[i])
  tabela_ViewCount <- rbind(tabela_ViewCount, tabela_tymczasowa)
}
tabela_ViewCount <- tabela_ViewCount %>% arrange(desc(ViewScore))
tabela_ViewCount


tabela_CommentCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- SportsPosts %>% filter(stri_detect_fixed(X._Tags, TagsList[i])) %>% summarise(CommentScore = sum(X._CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = TagsList[i])
  tabela_CommentCount <- rbind(tabela_CommentCount, tabela_tymczasowa)
}
tabela_CommentCount <- tabela_CommentCount %>% arrange(desc(CommentScore))
tabela_CommentCount


tabela_MeanCommentCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- SportsPosts %>% filter(stri_detect_fixed(X._Tags, TagsList[i])) %>% summarise(MeanCommentScore = mean(X._CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = TagsList[i])
  tabela_MeanCommentCount <- rbind(tabela_MeanCommentCount, tabela_tymczasowa)
}
tabela_MeanCommentCount <- tabela_MeanCommentCount %>% arrange(desc(MeanCommentScore))
tabela_MeanCommentCount


tabela_AnswerCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- SportsPosts %>% filter(stri_detect_fixed(X._Tags, TagsList[i])) %>% summarise(AnswerScore = sum(X._AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = TagsList[i])
  tabela_AnswerCount <- rbind(tabela_AnswerCount, tabela_tymczasowa)
}
tabela_AnswerCount <- tabela_AnswerCount %>% arrange(desc(AnswerScore))
tabela_AnswerCount


tabela_MeanAnswerCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- SportsPosts %>% filter(stri_detect_fixed(X._Tags, TagsList[i])) %>% summarise(MeanAnswerScore = mean(X._AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = TagsList[i])
  tabela_MeanAnswerCount <- rbind(tabela_MeanAnswerCount, tabela_tymczasowa)
}
tabela_MeanAnswerCount <- tabela_MeanAnswerCount %>% arrange(desc(MeanAnswerScore))
tabela_MeanAnswerCount


## FITNESS:

tabela_FitnessViewCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- FitnessPosts %>% filter(stri_detect_fixed(X._Tags, FitnessTagsList[i])) %>% summarise(ViewScore = sum(X._ViewCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = FitnessTagsList[i])
  tabela_FitnessViewCount <- rbind(tabela_FitnessViewCount, tabela_tymczasowa)
}
tabela_FitnessViewCount <- tabela_FitnessViewCount %>% arrange(desc(ViewScore))
tabela_FitnessViewCount


tabela_FitnessAnswerCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- FitnessPosts %>% filter(stri_detect_fixed(X._Tags, FitnessTagsList[i])) %>% summarise(ViewScore = sum(X._AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = FitnessTagsList[i])
  tabela_FitnessAnswerCount <- rbind(tabela_FitnessAnswerCount, tabela_tymczasowa)
}
tabela_FitnessAnswerCount <- tabela_FitnessAnswerCount %>% arrange(desc(ViewScore))
tabela_FitnessAnswerCount


tabela_FitnessMeanAnswerCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- FitnessPosts %>% filter(stri_detect_fixed(X._Tags, FitnessTagsList[i])) %>% summarise(ViewScore = mean(X._AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = FitnessTagsList[i])
  tabela_FitnessMeanAnswerCount <- rbind(tabela_FitnessMeanAnswerCount, tabela_tymczasowa)
}
tabela_FitnessMeanAnswerCount <- tabela_FitnessMeanAnswerCount %>% arrange(desc(ViewScore))
tabela_FitnessMeanAnswerCount


tabela_FitnessCommentCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- FitnessPosts %>% filter(stri_detect_fixed(X._Tags, FitnessTagsList[i])) %>% summarise(ViewScore = sum(X._CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = FitnessTagsList[i])
  tabela_FitnessCommentCount <- rbind(tabela_FitnessCommentCount, tabela_tymczasowa)
}
tabela_FitnessCommentCount <- tabela_FitnessCommentCount %>% arrange(desc(ViewScore))
tabela_FitnessCommentCount[2]


tabela_FitnessMeanCommentCount <- data.frame()
for(i in 1:length(TagsList)){
  tabela_tymczasowa <- FitnessPosts %>% filter(stri_detect_fixed(X._Tags, FitnessTagsList[i])) %>% summarise(ViewScore = mean(X._CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = FitnessTagsList[i])
  tabela_FitnessMeanCommentCount <- rbind(tabela_FitnessMeanCommentCount, tabela_tymczasowa)
}
tabela_FitnessMeanCommentCount <- tabela_FitnessMeanCommentCount %>% arrange(desc(ViewScore))
tabela_FitnessMeanCommentCount[2]


##GAMING:


tabela_GamingViewCount <- data.frame()
for(i in 1:length(GamingTagsList)){
  tabela_tymczasowa <- GamingPosts %>% filter(stri_detect_fixed(Tags, GamingTagsList[i])) %>% summarise(ViewScore = sum(ViewCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = GamingTagsList[i])
  tabela_GamingViewCount <- rbind(tabela_GamingViewCount, tabela_tymczasowa)
}
tabela_GamingViewCount <- tabela_GamingViewCount %>% arrange(desc(ViewScore))
tabela_GamingViewCount


tabela_GamingAnswerCount <- data.frame()
for(i in 1:length(GamingTagsList)){
  tabela_tymczasowa <- GamingPosts %>% filter(stri_detect_fixed(Tags, GamingTagsList[i])) %>% summarise(ViewScore = sum(AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = GamingTagsList[i])
  tabela_GamingAnswerCount <- rbind(tabela_GamingAnswerCount, tabela_tymczasowa)
}
tabela_GamingAnswerCount <- tabela_GamingAnswerCount %>% arrange(desc(ViewScore))
tabela_GamingAnswerCount


tabela_GamingMeanAnswerCount <- data.frame()
for(i in 1:length(GamingTagsList)){
  tabela_tymczasowa <- GamingPosts %>% filter(stri_detect_fixed(Tags, GamingTagsList[i])) %>% summarise(ViewScore = mean(AnswerCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = GamingTagsList[i])
  tabela_GamingMeanAnswerCount <- rbind(tabela_GamingMeanAnswerCount, tabela_tymczasowa)
}
tabela_GamingMeanAnswerCount <- tabela_GamingMeanAnswerCount %>% arrange(desc(ViewScore))
tabela_GamingMeanAnswerCount


tabela_GamingCommentCount <- data.frame()
for(i in 1:length(GamingTagsList)){
  tabela_tymczasowa <- GamingPosts %>% filter(stri_detect_fixed(Tags, GamingTagsList[i])) %>% summarise(ViewScore = sum(CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = GamingTagsList[i])
  tabela_GamingCommentCount <- rbind(tabela_GamingCommentCount, tabela_tymczasowa)
}
tabela_GamingCommentCount <- tabela_GamingCommentCount %>% arrange(desc(ViewScore))
tabela_GamingCommentCount


tabela_GamingMeanCommentCount <- data.frame()
for(i in 1:length(GamingTagsList)){
  tabela_tymczasowa <- GamingPosts %>% filter(stri_detect_fixed(Tags, GamingTagsList[i])) %>% summarise(ViewScore = mean(CommentCount))
  tabela_tymczasowa <- tabela_tymczasowa %>% mutate(TagName = GamingTagsList[i])
  tabela_GamingMeanCommentCount <- rbind(tabela_GamingMeanCommentCount, tabela_tymczasowa)
}
tabela_GamingMeanCommentCount <- tabela_GamingMeanCommentCount %>% arrange(desc(ViewScore))
tabela_GamingMeanCommentCount


##PODSUMOWANIE


## SPORTS:
tabela_podsumowanie_Sports <- data.frame(Pozycja = 1:24,
                                         wgCommentCount = tabela_CommentCount[2],
                                         sredniaCommentCount = tabela_MeanCommentCount[2],
                                         wgAnswerCount = tabela_AnswerCount[2],
                                         sredniaAnswerCount = tabela_MeanAnswerCount[2])
colnames(tabela_podsumowanie_Sports) <- c("Pozycja", "Według CommentCount", "Średnia CommentCount", "Według AnswerCount", "Średnia AnswerCount")
tabela_podsumowanie_Sports
tabela1 <- formattable(tabela_podsumowanie_Sports)
tabela1

##FITNESS:
tabela_podsumowanie_Fitness <- data.frame(Pozycja = 1:24,
                                         wgCommentCount = tabela_FitnessCommentCount[2],
                                         sredniaCommentCount = tabela_FitnessMeanCommentCount[2],
                                         wgAnswerCount = tabela_FitnessAnswerCount[2],
                                         sredniaAnswerCount = tabela_FitnessMeanAnswerCount[2])
colnames(tabela_podsumowanie_Fitness) <- c("Pozycja", "Według CommentCount", "Średnia CommentCount", "Według AnswerCount", "Średnia AnswerCount")
tabela_podsumowanie_Fitness
tabela2 <- formattable(tabela_podsumowanie_Fitness)
tabela2
##GAMING:
tabela_podsumowanie_Gaming <- data.frame(Pozycja = 1:24,
                                         wgCommentCount = tabela_GamingCommentCount[2],
                                         sredniaCommentCount = tabela_GamingMeanCommentCount[2],
                                         wgAnswerCount = tabela_GamingAnswerCount[2],
                                         sredniaAnswerCount = tabela_GamingMeanAnswerCount[2])
colnames(tabela_podsumowanie_Gaming) <- c("Pozycja", "Według CommentCount", "Średnia CommentCount", "Według AnswerCount", "Średnia AnswerCount")
tabela_podsumowanie_Gaming
tabela3 <- formattable(tabela_podsumowanie_Gaming)
tabela3

