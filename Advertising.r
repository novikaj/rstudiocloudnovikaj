Advertising<-read.csv("Advertising.csv", header=TRUE, sep=",")
Advertising
names(Advertising)
attach(Advertising)
mod1<-lm(sales~TV)
mod2<-lm(sales~radio)
mod3<-lm(sales~newspaper)
bptest(mod1)
bptest(mod2)
bptest(mod3)
#розглянемо графіки залежностей продажів від різних видів реклам
plot(TV,sales)
plot(radio,sales)
plot(newspaper,sales)
model1<-lm(sales~TV)
model2<-lm(sales~log(TV))
summary(model2)
plot(log(TV),sales)
model3<-lm(log(sales)~TV)
summary(model2)
plot(TV,log(sales))
model4<-lm(log(sales)~log(TV))
plot(log(TV),log(sales))
summary(model3)
bptest(model3)
bptest(model2)
bptest(model1)
bptest(model4)
#З усіх протестованих моделей, найкращою є модель 
#3, оскільки в ній відсутня гетероскедастичність. Тому 
#тестуємо її далі на наявність автокореляції

bgtest(model3)
bgtest(model3,2)
bgtest(model3,4)
bgtest(model3,5)
# ВИСНОВОК: в моделі 3 відсутня автокореляція першого та вищих порядків

# ПРИБИРАЄМО ГЕТЕРОСКЕДАСТИЧНІСТЬ 
#ПЕРШИЙ СПОСІБ: 
#removing heteroscedasticity by logarithming "sales"
model3<-lm(log(sales)~TV)
summary(model3)
bptest(model3) 
# Гетероскедастичності немає

#ДРУГИЙ СПОСІБ: Ділимо на квадратний корінь X
sqrt_TV<-sqrt(TV)
sqrt_TV
plot(TV)
plot(sqrt_TV)
sales2<-sales/sqrt_TV
sales2
model5<-lm(sales2~sqrt_TV)
summary(model5)

#за результатами моделювання збільшення витрат на рекламу призводить 
#до зменшення продажів. 

bptest(model5)

#при цьому гетероскедастичність зникла. 
#спробуємо додати додатково змінну radio
model6<-lm(sales2~sqrt_TV+(radio/sqrt_TV))
summary(model6)
bptest(model6)
#модель покращилась, проте гетероскедастичність не зникла

#global validation test (підключаємо пакети)
install.packages("caret")
library(caret)
install.packages("gvlma")
library(gvlma)
install.packages("e1071")
library(e1071)

gvlma(model1)
gvlma(model2)
gvlma(model3)
gvlma(model4)
gvlma(model5)
gvlma(model6)

#для моделей 1-5 по gvlma тесту гетероскедастичність відсутня (по bptest це не так (крім моделі 3)!)
#графіки для всіх моделей plot(modelN) також показують наявність гетероскедастичності
#спробуємо зробити трансформацію box-cox для прибирання гетероскедастичності для моделі 2

sales_BC<-BoxCoxTrans(sales)
Advertising<-cbind(Advertising,BCsales=predict(sales_BC,sales))
Advertising
model7<-lm(BCsales~log(TV), data=Advertising)
summary(model7)
gvlma(model7)
bptest(model7)
plot(model7)
