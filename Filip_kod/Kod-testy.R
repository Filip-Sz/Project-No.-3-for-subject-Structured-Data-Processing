# FITNESS


# Wykorzystywane przeze mnie biblioteki
library(sqldf)
library(dplyr, warn.conflicts = FALSE)
library(compare)
library(data.table)
library(stringi)
library(stringi)

# Ustawienie potrzebnych opcji
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# # Ustawianie zmiennych (data.frame)
# Badges <- read.csv("Badges.csv")
# Comments <- read.csv("Comments.csv")
# PostLinks <- read.csv("PostLinks.csv")
# Posts <- read.csv("Posts.csv")
# Tags <- read.csv("Tags.csv")
# Users <- read.csv("Users.csv")
# Votes <- read.csv("Votes.csv")



# tab <- Posts %>% filter(CreationDate > "2013-01-01T00:00:00") %>% group_by(Tags) %>% summarize(TotalScore = sum(ViewCount))
# tab <- tab %>% select(CreationDate, TotalScore, Tags) %>% arrange(desc(TotalScore))
# tab <- tab %>% slice(1:10)

# tab1 <- Posts %>% filter(CreationDate > "2013-01-01T00:00:00") %>% group_by(Tags) %>% select(CreationDate, Tags)
tab2 <- Posts %>% filter(CreationDate < "2013-01-01T00:00:00", stri_detect_fixed(Tags, "<workout>")) %>% group_by(Tags) %>% summarize(TotalScore = sum(ViewCount))
# tab <- inner_join(tab1, tab2, by = c("Tags" = "Tags"))
tab2 <- tab2 %>% filter(Tags != "")
tab2 <- tab2 %>% arrange(desc(TotalScore)) %>% slice(1:10)

print(tab2)



# ------------------------------------------



najlepsiRep <- Users %>% arrange(desc(Reputation))
najlepsiRep <- najlepsiRep %>% select(Reputation, Views, DisplayName, UpVotes, DownVotes) %>% mutate(UpToAll = UpVotes/(UpVotes+DownVotes)) %>% slice(1:10)
print(najlepsiRep)


najlepsiView <- Users %>% arrange(desc(Views))
najlepsiView <- najlepsiView %>% select(Reputation, Views, DisplayName, UpVotes, DownVotes) %>% mutate(UpToAll = UpVotes/(UpVotes+DownVotes)) %>% slice(1:10)
print(najlepsiView)



# ------------------------------------------



TopTags <- Tags %>% filter(Count>500) %>% select(TagName)
print(TopTags)
TopTags <- as.list(TopTags)
print(TopTags)
print(TopTags[[1]][1])

tabelaAll <- Posts %>% filter(CreationDate > "2020-01-01T00:00:00", stri_detect_fixed(Tags, TopTags[[1]][1])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
tabelaAll <- tabelaAll %>% mutate(TagName = TopTags[[1]][1])

for(i in 2:length(TopTags[[1]])){
  tabela <- Posts %>% filter(CreationDate > "2020-01-01T00:00:00", stri_detect_fixed(Tags, TopTags[[1]][i])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
  tabela <- tabela %>% mutate(TagName = TopTags[[1]][i])
  tabelaAll <- rbind(tabelaAll, tabela)
}

tabelaAll <- tabelaAll %>% arrange(desc(TotalScore))
print(tabelaAll)


tabelaAll2 <- Posts %>% filter(CreationDate > "2018-01-01T00:00:00")
tabelaAll2 <- tabelaAll2 %>% filter(CreationDate < "2020-01-01T00:00:00", stri_detect_fixed(Tags, TopTags[[1]][1])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
tabelaAll2 <- tabelaAll2 %>% mutate(TagName = TopTags[[1]][1])



for(k in 2:length(TopTags[[1]])){
  tabela <- Posts %>% filter(CreationDate > "2018-01-01T00:00:00")
  tabela <- tabela %>% filter(CreationDate < "2020-01-01T00:00:00", stri_detect_fixed(Tags, TopTags[[1]][k])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
  tabela <- tabela %>% mutate(TagName = TopTags[[1]][k])
  tabelaAll2 <- rbind(tabelaAll2, tabela)
}

tabelaAll2 <- tabelaAll2 %>% arrange(desc(TotalScore))
print(tabelaAll2)

plot.new()
wyniki <- as.vector(tabelaAll$TotalScore)
slices <- wyniki
print(slices)
Labels <- as.vector(tabelaAll$TagName)
print(Labels)
pie(slices, labels = Labels)

# plot.new()
wyniki <- as.vector(tabelaAll2$TotalScore)
slices <- wyniki
print(slices)
Labels <- as.vector(tabelaAll2$TagName)
print(Labels)
pie(slices, labels = Labels)



rozklad_popularnosci <- function(TopTags, data1, data2){
  # data1 - dolna granicca
  # data2 - gorna granica
  
  tabelaAll2 <- Posts %>% filter(CreationDate > data1)
  tabelaAll2 <- tabelaAll2 %>% filter(CreationDate < data2, stri_detect_fixed(Tags, TopTags[[1]][1])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
  tabelaAll2 <- tabelaAll2 %>% mutate(TagName = TopTags[[1]][1])
  
  for(k in 2:length(TopTags[[1]])){
    tabela <- Posts %>% filter(CreationDate > data1)
    tabela <- tabela %>% filter(CreationDate < data2, stri_detect_fixed(Tags, TopTags[[1]][k])) %>% summarize(TotalScore = sum(ViewCount)) %>% arrange(desc(TotalScore))
    tabela <- tabela %>% mutate(TagName = TopTags[[1]][k])
    tabelaAll2 <- rbind(tabelaAll2, tabela)
  }
  napis <- paste(data1, data2, sep = "_")
  napis <- paste(napis, ".jpg", sep="")
  
  jpeg(napis, width = 500, height = 500)
  
  wyniki <- as.vector(tabelaAll2$TotalScore)
  slices <- wyniki
  print(slices)
  Labels <- as.vector(tabelaAll2$TagName)
  print(Labels)
  pie(slices, labels = Labels)
  
  dev.off()
}

rozklad_popularnosci(TopTags, "2018-01-01T00:00:00", "2020-01-01T00:00:00")
rozklad_popularnosci(TopTags, "2020-01-01T00:00:00", "2022-01-01T00:00:00")




