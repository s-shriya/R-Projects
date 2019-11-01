library(ggplot2)
library(data.table)
library(ggthemes)

df <- fread('C:\\Users\\CSP KESHRI\\Documents\\DATA SCIENCE\\UDEMY COURSE\\This one has all\\Training Exercises\\Capstone and Data Viz Projects\\Data Visualization Project\\Economist_Assignment_Data.csv', drop= 1)

print(head(df))

pl<- ggplot(df, aes(x= CPI, y= HDI)) +geom_point(aes(color= Region),size= 5, shape= 1)+ geom_smooth(aes(group=1), method= 'lm', formula= y ~ log(x), se= F, color= 'red')

point <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
           "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
           "India", "Italy", "China", "South Africa", "Spane",
           "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
           "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
           "New Zealand", "Singapore")

pl2 <- pl + geom_text(aes(label= Country), color='gray20', data= subset(df, Country %in% point), check_overlap= T)
pl3 <- pl2+ theme_bw()
pl4 <- pl3 + scale_x_continuous(name= 'Corruption Perceptions Index, 2011(10= least corrupt)',breaks= 1:10, limits=c(.9, 10.5))
pl5 <- pl4 + scale_y_continuous(name= 'Human Development Index, 2011(1= Best)',limits= c(0.2,1.0)) + ggtitle('Corruption and Human development') + theme_economist_white()
print(pl5)

