#Read the batting data

batting <- read.csv('Batting.csv')
print(head(batting))
print(str(batting))

#Show the top 6 rows of At base (AB) and doubles (2B)
print(head(batting$AB))
print(head(batting$X2B))

#Adding Statistics (Feature engineering)

batting$BA <- batting$H / batting$AB
print(tail(batting$BA, 5))
batting$OBP <- (batting$H + batting$BB + batting$HBP)/ (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- (batting$H - batting$X2B - batting$X3B - batting$HR)
batting$SLG <- (batting$X1B + (2* batting$X2B) + (3* batting$X3B) + (4* batting$HR))/ batting$AB

#Reading the salaries data

sal <- read.csv('Salaries.csv')
print(summary(batting))

#Taking a subset of batting data to make it compatible with salaries data

batting2 <- subset(batting,subset=yearID>=1985)
print(summary(batting2))

#Merging batting and salaries data

combo <- merge(batting2, sal, by= c('playerID', 'yearID'))
print(summary(combo))

#Getting the Statistics of lost players

lost_players <- subset(combo,subset=playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))

#Getting the data only for year 2001

lost_players2 <- subset(lost_players, subset= yearID == 2001)

#Getting the required columns

lost_players3 <- (lost_players2[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')])
print(lost_players3)

#Finding the Replacement players


combo2 <- subset(combo, subset= !(playerID %in% c('giambja01', 'damonjo01', 'saenzol01')))
combo3 <- subset(combo2, subset= yearID == 2001)
print(combo3)

#Constraints calculation

#con2 <- sum(lost_players3$AB) , 1469
#con3 <- mean(lost_players3$OBP) , 0.364 approx
#con1 <- 15000000

#Code for selection of players begins here

library(dplyr)
library(ggplot2)

pl <- ggplot(combo3,aes(x=OBP,y=salary)) + geom_point()
print(pl)

can_be <- filter(combo3,salary<4000000,OBP>0)
can_be <- filter(can_be,AB >= 490)
print(can_be)

possible <- head(arrange(can_be,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
print(possible)

con3check <- mean(possible[c(1:3),'OBP'])
print(con3check)
con2check <- sum(possible[c(1:3),'AB'])
print(con2check)
con1check <- sum(possible[c(1:3),'salary'])
print(con1check)

#Selected players

result <- possible[c(1:3),]
print(result)