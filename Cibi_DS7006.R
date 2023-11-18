#To open the csv file:
setwd(dirname(file.choose()))
getwd()

stud <- read.csv("Covid_data.csv", stringsAsFactors = FALSE)
str(stud)


#visualize the missing values:

apply(stud, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
library(funModeling)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(forcats)


#Putting the Integer data types variables in one data frame and naming the column:
Covid <- data.frame(stud$Area_name,stud$Deaths, stud$Shareddwelling, stud$Unshareddwelling, stud$totalDwelling,
                    stud$Fulltime, stud$Parttime, stud$Economicallyactive, stud$EconomicallyInactive, 
                    stud$Total.Eco, stud$young_age, stud$Middle_Age, stud$Old_age, stud$total.age_count,
                    stud$Badhealth, stud$Goodhealth, stud$TotalHealth, stud$Total_population)

colnames(Covid) <- c("Area_name","Deaths", "Shareddwelling", "Unshareddwelling", "totalDwelling",
                     "Fulltime","Parttime", "Economicallyactive", "EconomicallyInactive",
                     "Total.Eco", "young_age", "Middle_Age", "Old_age","total_age",
                     "Badhealth", "Goodhealth", "TotalHealth", "Total_population")
str(Covid)
View(Covid)
missmap(Covid, col = c("black", "green"), legend = FALSE)
str(stud)
#Converting the All the Attributes into proportion by 10000:

#total population theme:
Total_deaths_1<- (Covid$Deaths/Covid$Total_population)*1000

#Dwelling theme:
Shareddwelling_1<- (Covid$Shareddwelling/Covid$totalDwelling)*1000
UnShareddwelling_1<- (Covid$Unshareddwelling/Covid$totalDwelling)*1000

#Eco theme:
Fulltime_1<- (Covid$Fulltime/Covid$Total.Eco)*1000
parttime_1<- (Covid$Parttime/Covid$Total.Eco)*1000

Economicallyactive_1<- (Covid$Economicallyactive/Covid$Total.Eco )*1000
EconomicallyInactive_1<- (Covid$EconomicallyInactive/Covid$Total.Eco )*1000


#Age Theme:
Young_age_1<- (Covid$young_age/Covid$total_age)*1000
Middle_age_1<- (Covid$Middle_Age/Covid$total_age)*1000
Old_age_1<- (Covid$Old_age/Covid$total_age)*1000

#Health Theme:
Badhealth_1<- (Covid$Badhealth/Covid$TotalHealth)*1000
Goodhealth_1<- (Covid$Goodhealth/Covid$TotalHealth)*1000

#putting into Relevant data frame:
Co <- data.frame(Total_deaths_1,Shareddwelling_1,UnShareddwelling_1,Fulltime_1,parttime_1
                 ,Economicallyactive_1,EconomicallyInactive_1, Young_age_1
                 ,Middle_age_1,Old_age_1,Badhealth_1,Goodhealth_1)


colnames(Co) <-c("Tp","Shd","UnShd","Ftw","Ptw","Ea","Ein","Ya","Ma","Oa","Bh","Gh")
str(Co)
boxplot(Co,main="Boxplot After Proportion by 1000",xlab="Dependent and Independant Variables",ylab="count",col="Bisque")

#Replacing the outlier functions:
replace_outlier <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
London_4 = replace_outlier(Co)
boxplot(London_4, xlab="numerical values", ylab="Count", col="Bisque", main="Outliers Replaced :")

#Normalization(3 Methods Used):

#1st Method min-max scaling:
London.mms<- apply(London_4 , MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))
boxplot(London.mms, main= "Min Max Scaling ",xlab="numerical values",ylab="count")


#2nd Method z-score:
London.z1 <- apply(London_4, MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
London.z2 <- apply(London_4, MARGIN = 2, FUN = function(x) (x - mean(x))/(2*sd(x)))
boxplot(London.z1,main= "Standard deviation 1",xlab="numerical values",ylab="count")
boxplot(London.z2,main= "Standard deviation 2",xlab="numerical values",ylab="count")

##3rd Method soft Max Scaling:
library(DMwR2)
insyahelp(SoftMax)

sts <- apply(London_4, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 1")

sts <- apply(London_4, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 2, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 2")

sts <- apply(London_4, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 3, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 3")

sts <- apply(London_4, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 4, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 4")

sts <- apply(London_4, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 5, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 5")


#Checking the Given Var are normally Distributed through Ks test: if norm>0.05 then its normally Distributed,

ks.test(Co$Shd,"pnorm", mean(Co$Shd), sd(Co$Shd))
ks.test(Co$UnShd,"pnorm", mean(Co$UnShd), sd(Co$UnShd))
ks.test(Co$Ftw,"pnorm", mean(Co$Ftw), sd(Co$Ftw))
ks.test(Co$Ptw,"pnorm", mean(Co$Ptw), sd(Co$Ptw))
ks.test(Co$Ea,"pnorm", mean(Co$Ea), sd(Co$Ea))
ks.test(Co$Ein,"pnorm", mean(Co$Ein), sd(Co$Ein))
ks.test(Co$Ya,"pnorm", mean(Co$Ya), sd(Co$Ya))
ks.test(Co$Ma,"pnorm", mean(Co$Ma), sd(Co$Ma))
ks.test(Co$Oa,"pnorm", mean(Co$Oa), sd(Co$Oa))
ks.test(Co$Bh,"pnorm", mean(Co$Bh), sd(Co$Bh))
ks.test(Co$Gh,"pnorm", mean(Co$Gh), sd(Co$Gh))

#Histogram for normality of dependent var:
histogram(x=Co$Tp,xlab="Covid deaths",main="Distribution of Dependent Variable")

ks.test(Co$Tp, "pnorm", mean(Co$Tp), sd(Co$Tp))
# putting into one data frame:
ts <- data.frame(Co$Tp,Co$Ftw,Co$Ea,Co$Ein,Co$Oa,Co$Bh,Co$Gh)
colnames(ts) <-c("Tp","Ftw","Ea","Ein","Oa","Bh","Gh")

# Correlations among numeric variables :
cor.matrix <- cor(ts, use = "pairwise.complete.obs", method = "pearson")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)
round(cor.df,2)

#Co-relation Map:
library(corrgram)
corrgram(ts, order=FALSE, cor.method = "pearson", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt, main="Deaths of Covid (pearson correlation)")


# test correlation of dependent variable with all independent variables:
cor(ts)
cor.test(Co$Tp, Co$Ftw, method ="pearson")
cor.test(Co$Tp, Co$Ea, method ="pearson")
cor.test(Co$Tp, Co$Ein,method ="pearson")
cor.test(Co$Tp, Co$Oa, method ="pearson")
cor.test(Co$Tp, Co$Bh, method ="pearson")
cor.test(Co$Tp, Co$Gh, method ="pearson")

## looking at internal correlations between Four variables:
cor.test(Co$Bh,Co$Ftw, method = "pearson")
cor.test(Co$Ein,Co$Ftw, method = "pearson")
cor.test(Co$Bh,Co$Ein, method = "pearson")

#partial correlation
library(ppcor)

#calculate partial correlation using Pearson 
pcor.test(Co$Tp,Co$Bh,Co$Ein)
pcor.test(Co$Tp,Co$Ein,Co$Bh)

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(Co))



#Histogram for Deaths in Covid:
plot_num(Co,bins=30)

#Bad health vs Total deaths: 
library(ggplot2)
ggplot(Covid, aes(y =Badhealth, x = Deaths)) +
  geom_point(color="red", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(breaks = seq(50,20050,5000), 
                     limits = c(50,20050)) +
  scale_x_continuous(breaks = seq(0,600,100), 
                     limits=c(0,600)) + 
  labs(y = "Bad Health population",
       x = "Covid Deaths",
       title = "Total Deaths vs Bad health")

#Very-Low Area's and Deaths:
d <- data.frame(Area_names= c('Adur','Christchurch','City of London',
'East Cambridgeshire',
'Forest Heath',
'Isles of Scilly',
'Melton',
'Mid Devon',
'North Devon',
'North Dorset',
'Purbeck',
'Richmondshire',
'Rutland',
'Ryedale',
'South Hams',
'Torridge',
'West Devon',
'West Somerset',
'Weymouth and Portland'),
                 Death_count= c(87,99,11,97,64,0,91,69,55,83,68,82,65,67,45,44,30,32,68))
ggplot(d,aes(x=Area_names, y=Death_count)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,300,100), 
                     limits=c(0,300)) +
  ylab("Covid Deaths") +xlab("City's in England")+
  ggtitle("Low Covid deaths and its City names") +
  theme_bw()

#Low-Area deaths:
d <- data.frame(Area_names= c('Cambridge',
                              'Corby',
                              'Cotswold',
                              'Craven',
                              'Daventry',
                              'Eden',
                              'Exeter',
                              'Forest of Dean',
                              'Harborough',
                              'Hart',
                              'Maldon',
                              'Malvern Hills',
                              'North Warwickshire',
                              'Ribble Valley',
                              'Runnymede',
                              'Selby',
                              'South Northamptonshire',
                              'St Edmundsbury',
                              'Stevenage',
                              'Teignbridge',
                              'Torbay',
                              'Uttlesford',
                              'Wellingborough',
                              'West Dorset'),
                Death_count= c(121,124,133,124,136,132,114,112,151,150,139,130,150,145,155,119,
                               129,116,153,105,149,129,151,138))
ggplot(d,aes(x=Area_names, y=Death_count)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,400,100), 
                     limits=c(0,400)) +
  ylab("Covid Deaths") +xlab("City's in England")+
  ggtitle("Low Covid deaths and its City names") +
  theme_bw()

#Medium-Area deaths:
t <- data.frame(Area_names= c('Amber Valley',
                              'Blackburn with Darwen',
                              'Braintree',
                              'Brighton and Hove','Broadland',
                              'Camden',
                              'Chelmsford',
                              'Dacorum',
                              'Kingston upon Thames',
                              'Merton',
                              'New Forest',
                              'North Tyneside',
                              'Preston',
                              'Southwark',
                              'Stockton-on-Tees',
                              'Swale',
                              'Waverley'),
              Death_count= c(306,416,402,405,309,308,414,311,305,430,303,431,311,419,438,401,300))
ggplot(t,aes(x=Area_names, y=Death_count)) +
  geom_bar(stat="identity", fill="darkgoldenrod", alpha=.6, width=.4) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,800,100), 
                     limits=c(0,800)) +
  ylab("Covid Deaths") +xlab("City's in England")+
  ggtitle("Medium Covid deaths and its City names") +
  theme_bw()

#High-Area deaths:
d <- data.frame(Area_names= c('Bolton',
                              'Brent',
                              'Bromley',
                              'Doncaster',
                              'Ealing',
                              'East Riding of Yorkshire',
                              'Enfield',
                              'Harrow',
                              'Kingston upon Hull',
                              'Newham',
                              'Nottingham',
                              'Oldham',
                              'Rochdale',
                              'Rotherham',
                              'Salford',
                              'Sefton',
                              'Southend-on-Sea',
                              'Stockport',
                              'Sunderland',
                              'Tameside',
                              'Wiltshire'),
                Death_count= c(680,792,673,799,741,748,743,645,611,745,636,628,600,745,619,781,607,654,
                               775,641,773))
ggplot(d,aes(x=Area_names, y=Death_count)) +
  geom_bar(stat="identity", fill="orange", alpha=.6, width=.4) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,1000,100), 
                     limits=c(0,1000)) +
  ylab("Covid Deaths") +xlab("City's in England")+
  ggtitle("High Covid deaths and its city names") +
  theme_bw()

#Very High-Area deaths:
d <- data.frame(Area_names= c('Barnet',
                              'Birmingham',
                              'Bradford',
                              'Cheshire East',
                              'Cheshire West and Chester',
                              'County Durham',
                              'Croydon',
                              'Havering',
                              'Kirklees',
                              'Leeds',
                              'Leicester',
                              'Liverpool',
                              'Manchester',
                              'Redbridge',
                              'Sandwell',
                              'Sheffield',
                              'Wakefield',
                              'Walsall',
                              'Wigan',
                              'Wirral'),
                Death_count= c(867,2716,1162,909,863,1349,913,844,869,1535,810,1280,913,816,963,
                               1204,802,880,907,896))
ggplot(d,aes(x=Area_names, y=Death_count)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,3000,400), 
                     limits=c(0,3000)) +
  ylab("Covid Deaths") +xlab("City's in England")+
  ggtitle("Very High Covid deaths and its City names") +
  theme_bw()


#Total Death Vs Old Age:
p <-ggplot(Covid, aes(x=Deaths,y=Old_age))+
  geom_point(color='blue')+geom_smooth(color='red')+scale_x_continuous(breaks = seq(0,1500,500), limits=c(0,1500))
scale_y_continuous(breaks = seq(0,6000,1000), 
                    limits=c(0,6000))

p+ggtitle("Total covid Deaths Vs Old Age")+theme(panel.background = element_rect(fill=
"lavender",colour="lightblue",size=0.5,linetype="solid"))
 
#Total Death Vs Middle Age:
p <-ggplot(Covid, aes(x=Deaths,y=Middle_Age))+
  geom_point(color='black')+geom_smooth(color='yellow')+scale_x_continuous(breaks = seq(0,1500,500), limits=c(0,1500))
scale_y_continuous(breaks = seq(500,1750000,500000), 
                   limits=c(500,1750000))

p+ggtitle("Total covid Deaths Vs Middle Age")+theme(panel.background = element_rect(fill=
"lavender",colour="lightblue",size=0.5,linetype="solid"))
                                                                                 
#Total Death vs young Age:
f <-ggplot(Covid, aes(x=Deaths,y=young_age))+
  geom_point(color='orange')+geom_smooth(color='black')+scale_x_continuous(breaks = seq(0,1700,500), limits=c(0,1700))
scale_y_continuous(breaks = seq(0,125000,30000), 
                   limits=c(0,125000))

f+ggtitle("Total covid Deaths Vs Young Age")+theme(panel.background = element_rect(fill=
"lavender",colour="lightblue",size=2,linetype="solid"))

                                                                         
#Total deaths Vs Full time work:
t<-ggplot(Covid, aes(x=Deaths,y=Fulltime))+
  geom_point(color='green')+geom_smooth(color='black')+scale_x_continuous(breaks = seq(0,1700,500), limits=c(0,1700))
scale_y_continuous(breaks = seq(500,2500000,500000), 
                   limits=c(500,2500000))

t+ggtitle("Total covid Deaths Vs Fulltime Workers")+theme(panel.background = element_rect(fill=
"lavender",colour="lightblue",size=2,linetype="solid"))


#Total deaths vs part time work:
t<-ggplot(Covid, aes(x=Deaths,y=Parttime))+
  geom_point(color='white')+geom_smooth(color='red')+scale_x_continuous(breaks = seq(0,1700,500), limits=c(0,1700))
scale_y_continuous(breaks = seq(200,100000,25000), 
                   limits=c(200,100000))

t+ggtitle("Total covid Deaths Vs part time Workers")+theme(panel.background = element_rect(fill=
"black",colour="lightblue",size=2,linetype="solid"))

#Total deaths vs Shared Dwelling:
t<-ggplot(Covid, aes(x=Deaths,y=Shareddwelling))+
  geom_point(color='orange')+geom_smooth(color='white')+scale_x_continuous(breaks = seq(0,1700,500), limits=c(0,1700))
scale_y_continuous(breaks = seq(0,600,100), 
                   limits=c(0,600))

t+ggtitle("Total covid Deaths Vs Shared Accomdation")+theme(panel.background = element_rect(fill=
"black",colour="lightblue",size=2,linetype="solid"))
                                                                                           
                                                                                


# Test dependent variable for normality
# graphically
qqnorm(Co$Tp, xlab = "Theoretical Quantiles: Total population Deaths" )
qqline(Co$Tp,col = 2) ## red color

# K-S test
ks.test(Co$Tp, "pnorm", mean(Co$Tp), sd(Co$Tp))

#Linear regression Model:
model1 <- lm(Co$Tp ~ Co$Bh)

# add regression line to scatter plot
plot(Co$Bh, Co$Tp, main = "Scatterplot of Linear Regression",
     xlab = "Bad healths", ylab = "Total Deaths",col="orange")
abline(model1, col = "red")

summary(model1)

hist(model1$residuals)
rug(model1$residuals)

# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))



#poisson regression:
p1 <-glm(formula = Co$Tp ~ Co$Bh+Co$Oa+Co$Ein+Co$Shd, data = Co, family = poisson)

# add poisson regression line to scatter plot:
plot(Co$Bh+Co$Oa+Co$Ein+Co$Shd, Co$Tp, main = "Scatterplot of poisson Regression",
     xlab = "Other independent Var", ylab = "Total Deaths",col="orange")
abline(p1, col = "black")

summary(p1)
hist(p1$residuals)
rug(p1$residuals)

# consider normality of residuals:
plot(p1$residuals ~ p1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(p1$residuals, "pnorm", mean(p1$residuals), sd(p1$residuals))

str(Covid)
