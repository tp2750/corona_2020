library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

CVpct <- function(x) 100*sd(x,na.rm=TRUE)/mean(x, na.rm=TRUE)

tb1 <- read_excel("dødsfald_ialt_20201231142523309636173DODC2.xlsx", skip = 2)
names(tb1)[3] <- "Total"

tb1.long <- tidyr::pivot_longer(tb1[,-c(1:3)], everything(), names_to = "week_id", values_to = "count")
tb1.2 <- transform(tb1.long, year = as.numeric(sub("(\\d+)U(\\d+)$", "\\1", week_id)),
                                             week = as.numeric(sub("(\\d+)U(\\d+)$", "\\2", week_id))
                                             )
#ggplot(tb1.2, aes(x = week, y = count, color = factor(year))) + geom_line() + geom_line(data = subset(tb1.2, year == 2020), aes(x = week, y = count), color = "black") + ggtitle("Døde i DK per uge")
tb1_mean <- tb1.2 %>% group_by(week) %>% summarise(week_mean = mean(count))
ggplot(tb1_mean, aes(x = week, y = week_mean)) + geom_point(color = "blue") + geom_line(data = subset(tb1.2, year == 2020), aes(x = week, y = count), color = "black") + ggtitle("Døde i DK per uge")

## Split per age and gender
tb2.in <- read_excel("dødsfald_dk_2020-12-31_edit.xlsx", skip=2)
tb2 <- tidyr::pivot_longer(tb2.in, matches("\\d+U\\d+"), names_to = "week_id", values_to = "count") %>%
    mutate(year = as.numeric(sub("(\\d+)U(\\d+)$", "\\1", week_id)),
           week = as.numeric(sub("(\\d+)U(\\d+)$", "\\2", week_id)),
           age = ordered(Age, levels= unique(tb2$Age))
           )

tb2_mean <- tb2 %>% group_by(Region, Gender, Age, age , week) %>% summarise(week_mean = mean(count))
ggplot(subset(tb2_mean, Gender =="I alt" & Age == "I alt"), aes(x = week, y = week_mean)) + geom_point(color = "blue") + geom_line(data = subset(tb2, year == 2020 & Gender =="I alt" & Age == "I alt"), aes(x = week, y = count), color = "black") + facet_wrap(~age)+ ggtitle("Døde i DK per uge")

ggplot(subset(tb2_mean, Gender =="I alt" & Age != "I alt"), aes(x = week, y = week_mean)) + geom_point(color = "blue") + geom_line(data = subset(tb2, year == 2020 & Gender =="I alt" & Age != "I alt"), aes(x = week, y = count), color = "black") + facet_wrap(~age)+ ggtitle("Døde i DK per uge")


ggplot(subset(tb2_mean, Gender =="I alt" & Age != "I alt"), aes(x = week, y = week_mean)) + geom_point(color = "blue") + geom_line(data = subset(tb2, year == 2020 & Gender =="I alt" & Age != "I alt"), aes(x = week, y = count), color = "black") + facet_wrap(~age, scale="free")+ ggtitle("Døde i DK per uge")

ggplot(subset(tb2_mean, Gender !="I alt" & Age != "I alt"), aes(x = week, y = week_mean, color = Gender)) + geom_point() + geom_line(data = subset(tb2, year == 2020 & Gender !="I alt" & Age != "I alt"), aes(x = week, y = count), color = "black") + facet_wrap(~age)+ ggtitle("Dødea i DK per uge")

ggplot(subset(tb2_mean, Gender !="I alt" & Age != "I alt"), aes(x = week, y = week_mean, color = Gender)) + geom_point() + geom_line(data = subset(tb2, year == 2020 & Gender !="I alt" & Age != "I alt"), aes(x = week, y = count), color = "black") + facet_wrap(~age, scale = "free")+ ggtitle("Døde i DK per uge")


