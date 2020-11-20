install.packages("faraway")
library(faraway)
data(divusa)
divusa
?divusa
head(divusa)
dataworkshop<-data.frame(divusa[,-1])
dataworkshop
cor(dataworkshop)
round(cor(dataworkshop),2)
attach(dataworkshop)
attach(divusa)
divmodel<-lm(divorce~unemployed+femlab+marriage+birth+military)
summary(divmodel)
vif(divmodel)
print("hello world")
