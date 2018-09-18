
#########################################################################################################
#-----------PROGETTO: CLASSIFICAZIONE IPOTIROIDISMO, IPERTIROIDISMO, NORMALE----------------------------#
#-------------------------------------------------------------------------------------------------------#
#-----------GRUPPO ALPHA: Aldeghi Chiara, Crippa Chiara, Iacoban Iuliana Anastasia, Vigano' Elena-------# 
#-------------------------------------------------------------------------------------------------------#
#-----------DATASET: D8---------------------------------------------------------------------------------#
#########################################################################################################

setwd("G:\\Multivariata\\Lavoro di gruppo\\DS8\\DS8")

dati<-read.table("thyroid.dat",header=F, sep=",")

library(MASS)
library(corrplot)
library(lattice)
#install.packages("tree")
library(tree)
library(ggplot2)
library(grid)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(gridExtra)
library(vcd)  
library(corrplot)
library(car)
#install.packages("ROSE")
library(ROSE)
library(plotrix)

#verifichiamo la presenza di NA
is.na(dati)
sum(is.na(dati)) #non ci sono valori mancanti

colnames(dati) <-c("age", "sex", "on_thyroxine", "query_on_thyroxine",
                   "on_antithyroid_medication","sick","pregnant", "thyroid_surgery","I131_treatment", "query_hypothyroid",
                   "query_hyperthyroid",   "lithium","goitre", "tumor",
                   "hypopituitary", "psych", "TSH", "T3", "TT4",
                   "T4U", "FTI","class")

str(dati)
summary(dati)

#trasformiamo age
dati$age=dati$age*100

#ciclo factor 
names.cont.vars <- names(dati)[-c(1, (17:21))]

for(i in names.cont.vars)
{
  dati[, i] =as.factor(dati[, i])
}
str(dati)

######################################################################################################################
#                                                   ANALISI DESCRITTIVE                                                                  #
######################################################################################################################

## FREQUENZE ##
table(dati$class)

## PIE CHART ##
slices <- c(2,5,93) 
lbls <- c("Ipotensione 2%", "Ipertensione 5%", "Normale 93%")
pie(slices,labels=lbls,explode=0.1,
    main="Grafico a torta della variabile target",col=c("coral","dodgerblue3","mediumseagreen"))


## ISTOGRAMMI condizionati ##
histogram(~age|class, data=dati,nint = 50)
histogram(~TSH|class, data=dati,nint = 50)
histogram(~T3|class, data=dati,nint = 50)
histogram(~TT4|class, data=dati,nint = 50)
histogram(~FTI|class, data=dati,nint = 50)
histogram(~T4U|class, data=dati,nint = 50)

## BARPLOT ##
names.cont = names(dati[2:16])

par(mfrow=c(1,3))
for(i in names.cont)
{
  x=dati[,i]
  plot(dati$class~x,col=c("coral","dodgerblue3","mediumseagreen"),ylab="class_label",
       xlab=i)
  cat ("Premere [invio] per continuare")
  readline()
}

par(mfrow=c(1,1))

#Barplot con ggplot
g <- ggplot(dati, aes(class)) 

#La variabile sex
ggplot(dati, aes(x = sex, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "sex", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile on_thyroxine
ggplot(dati, aes(x = on_thyroxine, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "On thyroxine", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile query_on_thyroxine
ggplot(dati, aes(x = query_on_thyroxine, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Query on thyroxine", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile on_antithyroid_medication
ggplot(dati, aes(x = on_antithyroid_medication, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "On antithyroid medication", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile sick
ggplot(dati, aes(x = sick, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Sick", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile pregnant
ggplot(dati, aes(x = pregnant, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Pregnant", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile thyroid_surgery
ggplot(dati, aes(x = thyroid_surgery, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Thyroid surgery", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile I131_treatment
ggplot(dati, aes(x = I131_treatment, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "I131 treatment", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile query_hypothyroid
ggplot(dati, aes(x = query_hypothyroid, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Query hypothyroid", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile query_hyperthyroid
ggplot(dati, aes(x = query_hyperthyroid, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Query hyperthyroid", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile lithium
ggplot(dati, aes(x = lithium, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Lithium", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile goitre
ggplot(dati, aes(x = goitre, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "goitre", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile tumor
ggplot(dati, aes(x = tumor, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Tumor", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile hypopituitary
ggplot(dati, aes(x = hypopituitary, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Hypopituitary", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#La variabile psych
ggplot(dati, aes(x = psych, fill = class, color = class)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Psych", y = "Proportion")+
  theme(plot.title = element_text(hjust = 0.5))

#Barplot variabile age
data=dati
data$eta=cut(dati$age,breaks=c(0,10,20,30,40,50,60,70,80,90,100),
             labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80",
                      "80-90","90-100"))
counts=table(data$class,data$eta)
barplot(counts,col=c("coral","dodgerblue3","mediumseagreen"))

## PLOT ORMONI ##
p1 <- ggplot(dati,aes(x = class,y = TSH)) + 
  theme(legend.position="top", axis.text=element_text(size = 6))
(p2 <- p1 + geom_point(aes(color = class), alpha = 0.5, size = 1.5,
position = position_jitter(width = 0.25, height = 0)))

p3 <- ggplot(dati,aes(x = class, y = T3)) + 
  theme(legend.position="top", axis.text=element_text(size = 6))
(p4 <- p3 + geom_point(aes(color = class), alpha = 0.5,size = 1.5,
 position = position_jitter(width = 0.25, height = 0)))

p5 <- ggplot(dati, aes(x = class, y = TT4)) + theme(legend.position="top",
        axis.text=element_text(size = 6))
(p6 <- p5 + geom_point(aes(color = class), alpha = 0.5,size = 1.5,
position = position_jitter(width = 0.25, height = 0)))


#######################################################################################################################
#                                             BOX PLOT - univariati                           
############################################################################################################################

names.cont.vars <- names(dati)[c(1,17:21)]

for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei plot (opzione per migliorare la visualizzazione)
  boxplot(dati[,i], xlab=i, outline=F,col="slategray1")
  cat ("Premere [invio] per continuare")
  readline()
}


############################################################################################################################
#                                           CICLO BOX PLOT - SENZA OUTLIER                   
######################################################################################################################

names.cont.vars <- names(dati)[-c((2:16), 22)]

par(mfrow=c(2,3))
for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei plot (opzione per migliorare la visualizzazione)
  boxplot(dati[,i]~dati$class, xlab="Class", ylab=i, outline=F, col =c("coral","dodgerblue3","mediumseagreen" ))
  cat ("Premere [invio] per continuare")
  readline()
}

par(mfrow=c(1,1))

###################################################################################################################
#                                          CICLO BOX PLOT - CON OUTLIER                     
##################################################################################################################

names.cont.vars <- names(dati)[-c((2:16), 22)]

for(i in names.cont.vars)
{
  boxplot(dati[,i]~dati$class, xlab="Class", ylab=i, outline=TRUE, col= c("coral","dodgerblue3","mediumseagreen")) 
  cat ("Premere [invio] per continuare")
  readline()
}

######################################################################################################################
#                                              ANALISI ESPLORATIVE                                                                
######################################################################################################################


## COR ##
m=cor(dati[,c("age", "TSH", "T3", "TT4", "T4U", "FTI")])
ggcorrplot(m, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of thyroid", 
           ggtheme=theme_bw)


#TT4 fortemente correlato con FTI 0.79 positivamente
#T3 correlato con TT4 0.48

## PAIRS ## 
colori<- ifelse(dati$class=="1", "coral", ifelse(dati$class=="2","dodgerblue3","mediumseagreen" ))
pairs(dati[,c("age", "TSH", "T3", "TT4", "T4U", "FTI")], col= "black",bg=colori,pch=21)


###############################################################################################################################
#                                               ANALISI VARIABILI CATEGORIALI                    
###############################################################################################################################

## V di CRAMER ##
names.cont <- names(dati)[-c(1, (17:22))]

for(i in names.cont)
{
  tbl = as.matrix(table(dati[,i],dati$class),nrow=2, ncol=3, byrow=TRUE)
  cat("Cramer's V:")
  h=assocstats(tbl)$cramer
  print(h)
  cat ("Premere [invio] per continuare", "\n")
  cat("---------------------------------------------------")
  readline()
}
#nessuna variabile risulta essere legata al target

cat_var <- colnames(dati[c(2:16)])
df=dati[c(2:16)]
empty_m <- matrix(ncol = length(df),
                  nrow = length(df),
                  dimnames = list(names(df), names(df)))
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}
cor_matrix <- calculate_cramer(empty_m ,dati)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",  lab = TRUE, lab_size = 3, 
method="circle",colors = c("tomato2", "white", "springgreen3"), 
title="V Cramer-corrplot", ggtheme=theme_bw,tl.cex = 10)

##########################################################################################################################
#                                     BOX-PLOT CONDIZIONATI                           
##########################################################################################################################

#si può pensare che ad esempio goitre insieme agli ormoni possa discriminar bene la variabile classe
#verifichiamolo per tutte le variabili

NAMES = names(dati)[-c(1,(17:22))]

par(mfrow=c(1,2))
for(j in NAMES){
  for(i in  names.cont.vars)
  {
    boxplot(dati[dati[,j]== 0 ,i]~dati$class[dati[,j]==0], pch=20, cex=.5, xlab="class", ylab=i, main="0", outline=F, col=c("coral","dodgerblue3","mediumseagreen" ))
    boxplot(dati[dati[,j]==1,i]~dati$class[dati[,j]==1], pch=20, cex=.5, xlab="class", ylab=i, main="1", outline=F, col=c("coral","dodgerblue3","mediumseagreen" ))
    cat ("Premere [invio] per continuare")
    readline()
  }
}
par(mfrow=c(1,1))

#NB
#osservando i grafici e cercando di comprendere il fenomeno dal punto di vista medico, ci si e' resi conto
#di una errata codifica della variabile risposta.
#per questo motivo si è modificata la dicitura nel seguente modo:
#1-ipotiroidismo
#2-ipertiroidismo
#3-normale

##########################################################################################################################
#                                             LAVORIAMO SUL DATASET 
###########################################################################################################################

table(dati$class)/nrow(dati)
#         1          2          3 
# 0.02305556 0.05111111 0.92583333
#l'evento ipotiroidismo ipertiroidismo raro --> bisogna bilanciare il dataset

# TRAINING E TEST
set.seed(123)
index=sample(1:nrow(dati),5040, replace = F) #70%
train=dati[index,]
test=dati[-index,]
dim(test)
dim(train)

#verifichiamo che training e test siano rappresentativi del nostro campione iniziale
table(train$class)/nrow(train)
table(test$class)/nrow(test)
#sbilanciato, probabilmente non riusciremo a cogliere l'evento raro

#proviamo a vedere come funziano i modelli sul dataset senza bilanciamento
#scegliamo solo alcune delle variabili quantitative (le più significative), che rilevano il livello 
#degli ormoni nel sangue.

#lavoriamo sulle variabili che non sono normali (TSH)
#poiche TSH presenta dei valori vicini allo 0 decidiamo di aggiungere una costante a tutti i valori
#in modo da riuscire a normalizzare la variabile senza avere problemi nella trasformazione

dati_trasf=dati
dati_trasf$TSH=dati_trasf$TSH+1

var_trasf=function(variable,lambda)
{
  if(lambda!=0)
    (variable ^lambda-1)/lambda
  else
    log(variable)
}

lambda_hat_1=powerTransform(dati_trasf$TSH[dati_trasf$class=="1"])
lambda_hat_2=powerTransform(dati_trasf$TSH[dati_trasf$class=="2"])
lambda_hat_3=powerTransform(dati_trasf$TSH[dati_trasf$class=="3"])
lambda_hat_1$convergence
lambda_hat_2$convergence
lambda_hat_3$convergence
Lambda_hat_1=lambda_hat_1$lambda
Lambda_hat_2=lambda_hat_2$lambda
Lambda_hat_3=lambda_hat_3$lambda

TSH_trasf_1=var_trasf(dati_trasf$TSH[dati_trasf$class=="1"],Lambda_hat_1)
TSH_trasf_2=var_trasf(dati_trasf$TSH[dati_trasf$class=="2"],Lambda_hat_2)
TSH_trasf_3=var_trasf(dati_trasf$TSH[dati_trasf$class=="3"],Lambda_hat_3)

par(mfrow=c(1,2))
hist(dati_trasf$TSH[dati_trasf$class=="1"])
hist(TSH_trasf_1)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(dati_trasf$TSH[dati_trasf$class=="2"])
hist(TSH_trasf_2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(dati_trasf$TSH[dati_trasf$class=="3"])
hist(TSH_trasf_3)
par(mfrow=c(1,1))


#poiche la trasformazione non si può dire una normale, decidiamo di togliere TSH 
#dall'analisi discriminate e di lavorare con gli altri ormoni risultati "discriminanti"
#ai fini dell'analisi

#test bartlett
bartlett.test(T4U+T3+FTI~class,dati)#rifiutiamo l'ipotesi nulla di omogeneità
#il test di bartlett ci suggerisce di usare la qda

#QDA
fit_qda_cv=qda(class~T4U+T3+FTI, data=dati,CV=T)
table(fit_qda_cv$class,dati$class)
sum(diag(table(fit_qda_cv$class,dati$class)))/nrow(dati)
#predice abbastanza bene solo la classe 3
#la classe uno e la due non la coglie (la due 0)

#ALBERO
fit_tree <- tree(class~., data=train, split="gini")
#mettiamo tutte le variabili poichè l'albero fa feature selection da solo
#vediamo che comunque le variabili piu importanti sono quelle che abbiamo stabilito inizialmente
fit_tree
plot(fit_tree)
text(fit_tree, cex=1,2,pretty=0)
#sembra lavorare bene
prev_nobil <- predict(fit_tree, newdata=test, type="class")
table(prev_nobil,test$class)
sum(diag(table(prev_nobil,test$class)))/nrow(test)

#ordine di selezione delle variabili: TSH,age,on_thyroxine,FTI,T3,thyroid_surgery,TT4,T4U

#proviamo a farlo crescere e potiamo
fit_tree_opts <- tree(class~., split="gini",data=train, control=tree.control(nrow(train), mincut = 20, minsize = 40, mindev = 0))
plot(fit_tree_opts)
text(fit_tree_opts, cex=.5)

tree_cv <- cv.tree(fit_tree_opts, , prune.tree, K=100, method="misclass")
str(tree_cv)

id.min <- which.min(tree_cv$dev)
tree_cv$size[id.min]
obj_pruned_best <- prune.tree(fit_tree_opts, best=tree_cv$size[id.min])
plot(obj_pruned_best)
text(obj_pruned_best,col="darkblue",lwd=5)

prev <- predict(obj_pruned_best, newdata=test, type="class")
table(prev,test$class)
sum(diag(table(prev,test$class)))/nrow(test)

#UNDERSAMPLING
set.seed(123)
N_TOT<-1650

results_qda=rep(NA,nrow(dati))
results_fin_qda=rep(NA,nrow(dati))
results_tree=rep(NA,nrow(dati))
results_fin_tree=rep(NA,nrow(dati))
prova=rep(NA,nrow(dati))
prova_all=rep(NA,nrow(dati))

for (i in 1:7200){
  #rimuoviamo i soggetti da validare
  training_data <- dati[-i, ]
  
  training_data_hypo <- training_data[training_data$class==1,]
  training_data_hyper<- training_data[training_data$class==2,] 
  training_data_normal<- training_data[training_data$class==3,]
  
  #otteniamo un sottocampione da bilanciare
  indices_hypo <- sample(nrow(training_data_hypo), N_TOT*0.1)
  training_data_hypo <-training_data_hypo[indices_hypo, ]
  indices_hyper <- sample(nrow(training_data_hyper), N_TOT*0.2)
  training_data_hyper <-training_data_hyper[indices_hyper, ]
  indices_normal <- sample(nrow(training_data_normal), N_TOT*0.7)
  training_data_normal <-training_data_normal[indices_normal, ]
  
  #costruiamo il training bilanciato
  training_data_fin <- rbind(training_data_normal, training_data_hyper,training_data_hypo)
  
  #selezioniamo le variabili nel training set
  training_data_formula <- training_data_fin[, c(18,20,21,22)]
  training_data_formula_all<- training_data_fin
  
  #selezioniamo le variabili nel test set
  test_data <-dati[i, c(18,20,21,22)]
  test_data_all<-dati[i,]
  prova[i]=test_data$class
  prova_all[i]=test_data_all$class
  
  #modello qda
  model_q=qda(class~.,data= training_data_formula )
  previsioni_q=predict(model_q,newdata=test_data)
  results_qda[i]=previsioni_q$class
  accuracy_q=sum(diag(table(previsioni_q$class,test_data$class)))/nrow(test_data)
  results_fin_qda[i] <-accuracy_q
  
  #classification tree
  tree.fit <- tree(class ~.,
                   data = training_data_formula_all)
  predictions_tree <- predict(tree.fit, test_data_all, type = "class")
  results_tree[i] <- predictions_tree
  accuracy_tree=sum(diag(table(predictions_tree,test_data_all$class)))/nrow(test_data_all)
  results_fin_tree[i] <-accuracy_tree
  #print(table(predictions_tree,test_data$class))
  
}

table(results_tree,prova_all)
table(results_qda,prova)
mean(results_fin_tree)
mean(results_fin_qda)

#OVERSAMPLING
set.seed(123)
N_TOT <- 3000

results_qda=rep(NA,nrow(dati))
results_fin_qda=rep(NA,nrow(dati))
results_tree=rep(NA,nrow(dati))
results_fin_tree=rep(NA,nrow(dati))
prova=rep(NA,nrow(dati))
prova_all=rep(NA,nrow(dati))

for (i in 1:nrow(dati)){
  #rimuoviamo i soggetti da validare
  training_data <- dati[-i, ]
  training_data_hypo <- training_data[training_data$class==1,]
  training_data_hyper<- training_data[training_data$class==2,] 
  training_data_normal<- training_data[training_data$class==3,]
  #otteniamo un sottocampione per bilanciare il dataset
  indices_hypo <- sample(nrow(training_data_hypo), N_TOT*.2, replace=T)
  training_data_hypo <-training_data_hypo[indices_hypo, ]
  indices_hyper <- sample(nrow(training_data_hyper), N_TOT*.3, replace=T)
  training_data_hyper <-training_data_hyper[indices_hyper, ]
  indices_normal <- sample(nrow(training_data_normal), N_TOT*0.5)
  training_data_normal <-training_data_normal[indices_normal, ]
  
  #costruiamo il training bilanciato
  training_data_fin <- rbind(training_data_normal, training_data_hyper,training_data_hypo)
  
  #selezioniamo le variabili nel training set
  training_data_formula <- training_data_fin[, c(18,20,21,22)]
  training_data_formula_all<- training_data_fin
  
  #selezioniamo le variabili nel test set
  test_data <-dati[i, c(18,20,21,22)]
  test_data_all<-dati[i,]
  prova[i]=test_data$class
  prova_all[i]=test_data_all$class
  
  #modello qda
  model_q=qda(class~.,data= training_data_formula )
  previsioni_q=predict(model_q,newdata=test_data)
  results_qda[i]=previsioni_q$class
  accuracy_q=sum(diag(table(previsioni_q$class,test_data$class)))/nrow(test_data)
  results_fin_qda[i] <-accuracy_q
  
  #classification tree
  tree.fit <- tree(class ~.,
                   data = training_data_formula_all)
  predictions_tree <- predict(tree.fit, test_data_all, type = "class")
  results_tree[i] <- predictions_tree
  accuracy_tree=sum(diag(table(predictions_tree,test_data_all$class)))/nrow(test_data_all)
  results_fin_tree[i] <-accuracy_tree
  #print(table(predictions_tree,test_data$class))
}

table(results_tree,prova_all)
table(results_qda,prova)
mean(results_fin_tree)
mean(results_fin_qda)


###############################################################################################################
#                               ANDIAMO A CREARE I DATASET PER I CONFRONTI A COPPIE                                                          #
###############################################################################################################

#Ora il nostro obiettivo sara' quello di effettuare delle analisi in cui ogni volta
#andremo a confrontare le classi minoritarie con la classe maggioritaria, in quanto generalmente 
#l'obiettivo che ci si pone effettuando un'analisi medica e' quello di classificare al meglio le classi 
#minoritarie che rappresentano le patologie dei pazienti

set.seed(123)

#dataset con classe 1 e 3
dati1=subset(dati,dati$class==1)
dati3=subset(dati,dati$class==3)
dati_13=rbind(dati1,dati3)
table(dati_13$class)

#classe 1==1, classe3==0 
dati_13$class=ifelse(dati_13$class=="3",0,1)
table(dati_13$class)
dati_13$class=as.factor(dati_13$class)
str(dati_13)

#analisi esplorativa 
c=cor(dati_13[,c("age", "TSH", "T3", "TT4", "T4U", "FTI")])
corrplot(c, method="number", diag = FALSE,type = "upper", tl.srt=30) 

names.cont.vars <- names(dati_13)[-c((2:16), 22)]
for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei boxplot (opzione per migliorare la visualizzazione)
  #idem a prima 
  boxplot(dati_13[,i]~dati_13$class, xlab="Class", ylab=i, outline=F)
  cat ("Premere [invio] per continuare")
  readline()
}

#training e test
index=sample(1:nrow(dati_13),floor(6832*.7), replace = F)
train_13=dati_13[index,]
head(train_13)
dim(train_13)
test_13=dati_13[-index,]
head(test_13)
dim(test_13)

#QDA
fit_qda_cv=qda(class~T3+T4U+FTI, data=dati_13,CV=T)
table(fit_qda_cv$class,dati_13$class)
sum(diag(table(fit_qda_cv$class,dati_13$class)))/nrow(dati_13) #0.9859485

#recall-precision
z=as.matrix(table(fit_qda_cv$class,dati_13$class))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision # 0.8804348
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall #0.4879518

#auc
obj_roc = roc.curve(dati_13$class,fit_qda_cv$class )
obj_roc$auc #0.7431508
legend("bottomright", legend=round(obj_roc$auc, 3), col=1, lwd=1)

#ALBERO
fit_tree_13 <- tree(class~., data=train_13, split="gini")
fit_tree_13
plot(fit_tree_13)
text(fit_tree_13, cex=.5)
prev_nobil_13 <- predict(fit_tree_13, newdata=test_13, type="class")
sum(diag(table(prev_nobil_13,test_13$class)))/nrow(test_13) #0.9970732

#recall-precision
z=as.matrix(table(prev_nobil_13,test_13$class))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision #0.9534884
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall # 0.9111111

#roc albero non potato
roc_13 = roc.curve(test_13$class, prev_nobil_13)
legend("bottomright", legend=round(roc_13$auc, 3), col=1, lwd=1)

#proviamo a farlo crescere e potiamo
fit_tree_opts_13 <- tree(class~., split="gini",data=train_13, control=tree.control(nrow(train_13), mincut = 1, minsize = 2, mindev = 0))
plot(fit_tree_opts_13)
text(fit_tree_opts_13, cex=.5)
tree_cv_13 <- cv.tree(fit_tree_opts_13, , prune.tree, K=100, method="misclass")

id.min_13 <- which.min(tree_cv_13$dev)
tree_cv_13$size[id.min_13]
obj_pruned_best_13 <- prune.tree(fit_tree_opts_13, best=tree_cv_13$size[id.min_13])
plot(obj_pruned_best_13)
text(obj_pruned_best_13)

prev_13 <- predict(obj_pruned_best_13, newdata=test_13, type="class")
table(prev_13,test_13$class)
sum(diag(table(prev_13,test_13$class)))/nrow(test_13) #0.9970732

#recall-precision
z=as.matrix(table(prev_13,test_13$class))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision#0.9148936
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall#0.9555556

#roc albero
obj_roc_13 = roc.curve(test_13$class, prev_13)
legend("bottomright", legend=round(obj_roc_13$auc, 3), col=1, lwd=1)


#UNDERSAMPLING
set.seed(123)
N_TOT=166
results_qda=rep(NA,nrow(dati_13))
results_fin_qda=rep(NA,nrow(dati_13))
results_tree=rep(NA,nrow(dati_13))
results_fin_tree=rep(NA,nrow(dati_13))
prova_13=rep(NA,nrow(dati_13))
prova_13_2=rep(NA,nrow(dati_13))

for (i in 1:nrow(dati_13)){
  #rimozione soggetti per la validazione
  training_data <- dati_13[-i, ]
  training_data_0 <- training_data[training_data$class==0,]
  training_data_1<- training_data[training_data$class==1,]
  #estrazione di un sottogruppo per la creazione di un dataset bilanciato
  indices_0 <- sample(nrow(training_data_0), N_TOT)
  training_data_0 <-training_data_0[indices_0, ]
  training_data <- rbind(training_data_0, training_data_1)
  
  #training set
  training_data_formula <- training_data[, c(18,20,21,22)]
  training_data_formula_13 <- training_data
  
  #test set
  test_data <-dati_13[i, c(18,20,21,22)]
  test_data_13 <- dati_13[i,]
  prova_13[i]=test_data$class
  prova_13_2[i]=test_data_13$class
  
  #modello qda
  model_q=qda(class~.,data= training_data_formula )
  previsioni_q=predict(model_q,newdata=test_data)
  results_qda[i]=previsioni_q$class
  accuracy_q=sum(diag(table(previsioni_q$class,test_data$class)))/nrow(test_data)
  results_fin_qda[i] <-accuracy_q
  
  #albero di classificazione
  tree.fit <- tree(class ~.,
                   data = training_data_formula_13)
  predictions_tree <- predict(tree.fit, test_data_13, type = "class")
  results_tree[i] <- predictions_tree
  accuracy_tree=sum(diag(table(predictions_tree,test_data_13$class)))/nrow(test_data_13)
  results_fin_tree[i] <-accuracy_tree
  #print(table(predictions_tree,test_data_13$class))
  
}

table(results_tree,prova_13_2)
table(results_qda,prova_13)
mean(results_fin_tree) #0.9831674
mean(results_fin_qda) # 0.9732143
     
#recall-precision
h=as.matrix(table(results_qda,prova_13))
precision=h[2,2]/sum(h[2,2]+h[2,1]);precision #0.4746269
recall=h[2,2]/sum(h[2,2]+h[1,2]);recall # 0.9578313

#recall-precision
z=as.matrix(table(results_tree, prova_13_2))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision # 0.5907473
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall #1

#roc qda
obj_roc_bil_qda_13 = roc.curve(prova_13, results_qda)
legend("bottomright", legend=round(obj_roc_bil_qda_13$auc, 3), col=1, lwd=1) #0.966

#roc albero
obj_roc_bil_tree_13 = roc.curve(prova_13_2, results_tree)
legend("bottomright", legend=round(obj_roc_bil_tree_13$auc, 3), col=1, lwd=1) #0.991

######################################################################################################################
######################################################################################################################

#dataset con classe 2 e 3
dati2=subset(dati,dati$class==2)
dati3=subset(dati,dati$class==3)
dati_23=rbind(dati2,dati3)
table(dati_23$class)

#classe 2==1, classe3==0 
dati_23$class=ifelse(dati_23$class=="3",0,1)
table(dati_23$class)
dati_23$class=as.factor(dati_23$class)
str(dati_23)

#analisi esplorativa
c=cor(dati_23[,c("age", "TSH", "T3", "TT4", "T4U", "FTI")])
corrplot(c, method="number", diag = FALSE,type = "upper", tl.srt=30) 

names.cont.vars <- names(dati_23)[-c((2:16), 22)]
for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei boxplot (opzione per migliorare la visualizzazione)
  #idem a prima 
  boxplot(dati_23[,i]~dati_23$class, xlab="Class", ylab=i, outline=F)
  cat ("Premere [invio] per continuare")
  readline()
}

#training e test
set.seed(123)
index_23=sample(1:nrow(dati_23),floor(7034*.7), replace = F)
train_23=dati_23[index_23,]
test_23=dati_23[-index_23,]
dim(train_23)
dim(test_23)

#QDA
fit_qda_cv=qda(class~T3+T4U+FTI, data=dati_23,CV=T)
table(fit_qda_cv$class,dati_23$class)
sum(diag(table(fit_qda_cv$class,dati_23$class)))/nrow(dati_23) #0.9476827

#recall-precision
z=as.matrix(table(fit_qda_cv$class,dati_23$class));z
precision=z[2,2]/sum(z[2,2]+z[2,1]); precision #0/0
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall #0

obj_roc = roc.curve(dati_23$class, fit_qda_cv$class)
legend("bottomright", legend=round(obj_roc$auc, 3), col=1, lwd=1) #0.5


#ALBERO
fit_tree_23<- tree(class~., data=train_23, split="gini")
fit_tree_23
plot(fit_tree_23)
text(fit_tree_23, cex=.5)
prev_nobil_23 <- predict(fit_tree_23, newdata=test_23, type="class")
sum(diag(table(prev_nobil_23,test_23$class)))/nrow(test_23) #0.9976315

#recall-precision
z=as.matrix(table(prev_nobil_23,test_23$class))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision # 0.9827586
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall# 0.974359

obj_roc_23 = roc.curve(test_23$class, prev_nobil_23)
obj_roc_23$auc
legend("bottomright", legend=round(obj_roc_23$auc, 3), col=1, lwd=1) #0.986678

#proviamo a farlo crescere e potiamo
fit_tree_opts_23 <- tree(class~., split="gini",data=train_23, control=tree.control(nrow(train_23), mincut = 1, minsize = 2, mindev = 0))
plot(fit_tree_opts_23)
text(fit_tree_opts_23, cex=.5)
tree_cv_23 <- cv.tree(fit_tree_opts_23, , prune.tree, K=100, method="misclass")

id.min_23 <- which.min(tree_cv_23$dev)
tree_cv_23$size[id.min_23]
obj_pruned_best_23 <- prune.tree(fit_tree_opts_23, best=tree_cv_23$size[id.min_23])
plot(obj_pruned_best_23)
text(obj_pruned_best_23)

prev_23 <- predict(obj_pruned_best_23, newdata=test_23, type="class")
table(prev_23,test_23$class)
sum(diag(table(prev_23,test_23$class)))/nrow(test_23) #0.9976315
#recall-precision
z=as.matrix(table(prev_23,test_23$class))
precision=z[2,2]/sum(z[2,2]+z[2,1]);precision # 0.9827586
recall=z[2,2]/sum(z[2,2]+z[1,2]);recall #0.974359

#roc albero
obj_roc_23_1 = roc.curve(test_23$class, prev_23)
legend("bottomright", legend=round(obj_roc_23_1$auc, 3), col=1, lwd=1) # 0.987

#UNDERSAMPLING
set.seed(123)
N_TOT=368
results_qda=rep(NA,nrow(dati_23))
results_fin_qda=rep(NA,nrow(dati_23))
results_tree=rep(NA,nrow(dati_23))
results_fin_tree=rep(NA,nrow(dati_23))
prova_23=rep(NA,nrow(dati_23))
prova_23_2=rep(NA,nrow(dati_23))


for (i in 1:nrow(dati_23)){
  #rimuovere i soggetti da validare
  training_data <- dati_23[-i, ]
  training_data_0 <- training_data[training_data$class==0,]
  training_data_1<- training_data[training_data$class==1,]
  #ottenere un campione per bilanciare il dataset
  indices_0 <- sample(nrow(training_data_0), N_TOT)
  training_data_0 <-training_data_0[indices_0, ]
  training_data <- rbind(training_data_0, training_data_1)
  
  #training set
  training_data_formula <- training_data[, c(18,20,21,22)]
  training_data_formula_23 <- training_data
  #test set
  test_data <-dati_23[i,c(18,20,21,22)]
  test_data_23 <- dati_23[i, 1:22]
  prova_23[i]=test_data$class
  prova_23_2[i]=test_data_23$class
  
  #modello qda
  model_q=qda(class~.,data= training_data_formula )
  previsioni_q=predict(model_q,newdata=test_data)
  results_qda[i]=previsioni_q$class
  accuracy_q=sum(diag(table(previsioni_q$class,test_data$class)))/nrow(test_data)
  results_fin_qda[i] <-accuracy_q
  
  #classification tree
  tree.fit <- tree(class ~.,
                   data = training_data_formula_23)
  predictions_tree <- predict(tree.fit, test_data_23, type = "class")
  results_tree[i] <- predictions_tree
  accuracy_tree=sum(diag(table(predictions_tree,test_data_23$class)))/nrow(test_data_23)
  results_fin_tree[i] <-accuracy_tree
  #print(table(predictions_tree,test_data_13$class))
  
}

table(results_tree,prova_23_2)
table(results_qda,prova_23)
mean(results_fin_tree) #0.9862098
mean(results_fin_qda) #0.4792437

#recall-precision
h=as.matrix(table(results_qda,prova_23))
precision=h[2,2]/sum(h[2,2]+h[2,1]); precision # 0.0778888
recall=h[2,2]/sum(h[2,2]+h[1,2]); recall # 0.826087

#recall-precision
z=as.matrix(table(results_tree, prova_23_2))
precision=z[2,2]/sum(z[2,2]+z[2,1]); precision # 0.791397
recall=z[2,2]/sum(z[2,2]+z[1,2]); recall #1

#roc qda
obj_roc_bil_qda_23 = roc.curve(prova_23, results_qda)
legend("bottomright", legend=round(obj_roc_bil_qda_23$auc, 3), col=1, lwd=1) #0.643

#roc albero
obj_roc_bil_tree_23 = roc.curve(prova_23_2, results_tree)
legend("bottomright", legend=round(obj_roc_bil_tree_23$auc, 3), col=1, lwd=1) #0.993














