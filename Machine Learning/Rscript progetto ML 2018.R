setwd("F:\\Machine Learning\\Definitivo")

dati = read.table("adultnomissing.csv", header=T, sep=",")

#install.packages("ggplot2")
#install.packages("gridExtra)
#install.packages("ggcorrplot")
#install.packages("unbalanced")
#install.packages("ROSE")
#install.packages("lsr")
#install.packages("corrplot")
#install.packages("plotly")
#install.packages("Rserve")
#install.packages("googleVis")
#install.packages("plotrix")
library(plotrix)
library(googleVis)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(unbalanced)
library(ROSE)
library(lsr)
library(corrplot)
library(plotly)
library(Rserve)

dati <- dati[, -1] #rimuovo colonna row.ID

#Analisi descrittive ed esplorative 
str(dati)
summary(dati) 

boxplot(dati[,5]~dati$education, xlab="Education", ylab="Education.num") 
#100% correlazione teniamo solo il qualitativo

ordered_levels <- levels(dati$education)[c(14,4,5,6,7,1,2,3,12,16,9,8,10,13,15,11)]
dati$education <- factor(dati$education, ordered_levels)

dati=dati[,-5] #Abbiamo eliminato la variabile education.num 
str(dati) 

#Creaiamo degli istogrammi per ogni variabile qualitativi: 
#condizionati e proporzionati rispetto alla variabile risposta.

g <- ggplot(dati, aes(income)) 

#La variabile workclass

ggplot(dati, aes(x = workclass, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Workclass", y = "Proportion", title = "Income bias \n based on workclass")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile education 

ggplot(dati, aes(x = education, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Education", y = "Proportion", title = "Income bias \n based on education", subtitle="People with more education earn more")+
  theme(plot.title = element_text(hjust = 0.5))

#Variabile marital status

ggplot(dati, aes(x =marital.status, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Marital.status", y = "Proportion", title = "Income bias \n based on marital.status")+
  theme(plot.title = element_text(hjust = 0.5))

#Variabile occupation

ggplot(dati, aes(x = occupation, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Occupation", y = "Proportion", title = "Income bias \n based on occupation")+
  theme(plot.title = element_text(hjust = 0.5))

#Variabile relationship

ggplot(dati, aes(x = relationship, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Relationship", y = "Proportion", title = "Income bias \n based on relationship", 
       subtitle="Married people earn more")+ theme(plot.title = element_text(hjust = 0.5))

#Variabile race

ggplot(dati, aes(x = race, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Race", y = "Proportion", title = "Income bias \n based on race")+
  theme(plot.title = element_text(hjust = 0.5))

#Variabile sex

ggplot(dati, aes(x = sex, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Sex", y = "Proportion", title = "Income bias \n based on sex", 
       subtitle="Men earn more than women") + theme(plot.title = element_text(hjust = 0.5))

#Variabile native country 

ggplot(dati, aes(x = native.country, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Native.country", y = "Proportion", title = "Income bias \n based on native.country")+
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################
#                        CICLO BOX PLOT - SENZA OUTLIER                   #
###########################################################################

names.cont.vars <- names(dati)[c(1,3, (10:12))]
names.cont.vars


for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei boxplot (opzione per migliorare la visualizzazione)
  #idem a prima 
  boxplot(dati[,i]~dati$income, xlab="income", ylab=i, outline=F, col = c("indianred1", "cyan3"))
  cat ("Premere [invio] per continuare")
  readline()
}

###########################################################################
#                        CICLO BOX PLOT - CON OUTLIER                     #
###########################################################################

names.cont.vars <- names(dati)[c(1,3, (10:12))]
names.cont.vars

for(i in names.cont.vars)
{
  boxplot(dati[,i]~dati$income, xlab="income", ylab=i,col=c("indianred1", "cyan3"), outline=T) #su pc Elena usare
  #boxplot come comando se no lanci per nulla perch? non va 
  cat ("Premere [invio] per continuare")
  readline()
}

#corrplot
C <- cor(dati[,c(1,3,10,11, 12)])#prendo le variabili quantitative 
ggcorrplot(C, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Corrplot", 
           ggtheme=theme_bw)#dal grafico si deduce che non c'è correlazione

#grafico a dispersione
colori<- ifelse(dati$income=="<=50K", "indianred1", "cyan3")
pairs(dati[,c(1,3, (10:12))], col= "black",bg=colori,pch=21)

#densità condizionate
ggplot(dati, aes(x = age, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Age", y = "Density",
       subtitle = "Density plot")

ggplot(dati, aes(x = fnlwgt, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Fnlwgt", y = "Density", 
       subtitle = "Density plot")

ggplot(dati, aes(x = capital.gain, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Capital.gain", y = "Density", 
       subtitle = "Density plot")

ggplot(dati, aes(x = capital.loss, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Capital.loss", y = "Density", 
       subtitle = "Density plot")

ggplot(dati, aes(x = hours.per.week, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Hours.per.week", y = "Density", 
       subtitle = "Density plot")

#rimuovo variabile fnlwgt
dati=dati[,-3]

#vediamo correlazione e grafici per variabili qualitative: "workclass", "education", "marital.status", "occupation", "relationship", "race", "sex",
#"native.country"
#correlazione

datinominal=dati[,-c(1,9,10, 11)]#crea un dataset solo con le variabili qualitative
cramercorr=matrix(ncol = ncol(datinominal), nrow=ncol(datinominal))
nomi <- names(datinominal)
colnames(cramercorr)=rownames(cramercorr)=nomi
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(datinominal[,i],datinominal[,j]))}
}#usiamo la V di Cramer per studiare le correlazioni tra variabili qualitative


ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Associations", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "violet"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1))


#Raggruppiamo i livelli delle variabili qualitative in modo da ridurli
#per favorire l'implementazione dei classificatori

levels(dati$workclass)= list(Gov=c("Federal-gov","State-gov","Local-gov"),
                             Selfempinc="Self-emp-inc",
                             Selfempnotinc= "Self-emp-not-inc", 
                             Nopay=c("Without-pay","Never-worked"),
                             Private ="Private")

levels(dati$marital.status) = list(Divorced=c("Divorced","Separated"),
                                   Married=c("Married-civ-spouse","Married-AF-spouse","Married-spouse-absent"),
                                   Notmarried="Never-married",
                                   Widowed="Widowed")

levels(dati$education)=list(Preschool="Preschool", 
                            Primary=c("1st-4th","5th-6th"), 
                            Upperprimary="7th-8th", 
                            Highschool=c("9th", "10th", "11th", "12th"),
                            Hsgrad="HS-grad",
                            Somecollege="Some-college",
                            Assoc=c("Assoc-voc", "Assoc-acdm"), 
                            Bachelor="Bachelors",
                            Master="Masters",
                            Doctorate="Doctorate",
                            Profsch="Prof-school")

levels(dati$occupation)= list(Lowincome=c("Adm-clerical", "Farming-fishing","Handlers-cleaners",  "Armed-Forces","Machine-op-inspct","Other-service", "Priv-house-serv"),
                              Medincomelow=c("Craft-repair", "Transport-moving"), 
                              Medincomehigh=c("Protective-serv", "Sales", "Tech-support"),
                              Highincome=c("Exec-managerial", "Prof-specialty")) #Armed-Forces

levels(dati$race) = list(White = "White", Black = "Black", Asian = "Asian-Pac-Islander", Other=c("Amer-Indian-Eskimo", "Other"))

levels(dati$native.country)= list(South="South",
                                  SouthEastAsia=c("Vietnam","Laos","Cambodia","Thailand"),
                                  Asia=c("China","India","HongKong","Iran","Philippines","Hong"),
                                  NorthAmerica=c("Canada","United-States", "Mexico"),
                                  CenterAmerica=c("Cuba","Dominican-Republic","Guatemala","Haiti","Honduras","Jamaica","Nicaragua","Puerto-Rico","El-Salvador"),    
                                  SouthAmerica=c("Ecuador","Peru","Columbia","Trinadad&Tobago"),
                                  Europe=c("France","Germany","Greece","Holand-Netherlands","Italy","Hungary","Ireland","Poland","Portugal","Scotland","England","Yugoslavia"),
                                  Oceania=c("Outlying-US(Guam-USVI-etc)"))



#Verifichiamo lo sbilanciamento con l'uso di una pie chart

torta <- data.frame(perc = table(dati$income))
pie3D(torta$perc.Freq, labels = torta$perc.Var1, main = "An exploded 3D pie chart", explode=0, radius=.9, labelcex = 1.2,  start=0.7)
text(x= -0.5, y= 0.10 , labels= "75.1%", col = "white")

#oppure

grafico = gvisPieChart(torta, options = list( width = 1000, height = 500))
plot(grafico)

#bilanciamento
table(dati$income)
table(dati$income)/nrow(dati)

#oversampling con ROSE package
train = read.table("train.csv", header = T, sep = ",")
train = train[,-1] #elimino row.ID
table(train$income)
table(train$income)/nrow(train)

data_balanced_over = ovun.sample( income ~ ., data = train,seed = 123, method = "over", N=30936)$data
table(data_balanced_over$income)
str(data_balanced_over)



