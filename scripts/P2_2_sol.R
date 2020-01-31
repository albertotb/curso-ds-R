library(readr)

# Ejercicio 1
data <- read_csv("titanic.csv")

# Ejercicio 2
hist(data$Age[data$Sex == "male"], col=rgb(1, 0, 0, alpha=0.25))
hist(data$Age[data$Sex == "female"], col=rgb(0, 0, 1, alpha=0.25), add=T)

ggplot(data, aes(x=Age, fill=Sex)) + geom_histogram(alpha=0.5, position='identity')

# Ejercicio 3
title <- gsub('(.*,)|(\\..*)', '', data$Name)
# o
title <- sapply(strsplit(data$Name, split='[,.]'), "[[", 2)

replace_title <- function(title) {
  title <- trimws(title)
  if (title %in% c("Mme", "Ms")) {
    return("Mrs")
  } else if (title %in% c("Mlle")) {
    return("Miss")
  } else if (title %in% c("Capt", "Dr", "Rev", "Major", "Col")) {
    return("Prof")
  } else if (!title %in% c("Mr", "Mrs", "Miss", "Master")) {
    return("Otros")
  } else {
    return(title)  
  }
}

data$Title <- sapply(title, replace_title, USE.NAMES=F)


# Ejercicio 4
boxplot(Age ~ Title, data=data)

# Ejercicio 5
barplot(tapply(data$Survived, data$Title, mean))
# o
data %>%
  group_by(Title) %>%
  summarize(Survived = mean(Survived)) %>%
  ggplot(aes(x=Title, y=Survived)) + geom_bar(stat='identity')

# Ejercicio 6
table(title[data$Title == "Otros"])

# Ejercicio 7
ggplot(data, aes(x=Age, y=Title, color=factor(Survived))) + 
  geom_point(position = position_jitter(w = 0, h = 0.2)) + facet_wrap(~Pclass)

# Ejercicio 8

# tambien replace
data$Age <- ave(data$Age, data$Sex, data$Pclass, FUN = function(x) ifelse(is.na(x), median(x, na.rm=T), x))

# tambien replace_na
data %>% 
  group_by(Sex, Pclass) %>% 
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm=T), Age))
