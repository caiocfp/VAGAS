####certifica-se do diretorio####

setwd('C:/Users/Admin/Desktop/Teste R/Vagas')
getwd()



####setar os pacotes necessarios####

# necessario que instale os pacotes abaixo com o install.packages # 

library(dplyr)
library(tidyverse)
library(lubridate)
library(lmtest)
library(generalhoslem)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(psych)
library(MASS)
library(ordinal)
library(ggplot2)
library(generalhoslem)
library(MLmetrics)

####importando os dados###

base_test <- read.csv('base_treinamento_testes.csv',
                      header = T,
                      sep = ",",
                      dec = ".",
                      stringsAsFactors = T)

base_test


###estatistica descritiva###

summary(base_test1)

###é necessário retirar os outliers###

base_test1 <- base_test %>%
              subset(ultimo_salario_candidato < 10000)

###componente principal###

IQ <- cbind(base_test1$mediana_teste_ingles_candidato,
            base_test1$mediana_teste_logica_candidato,
            base_test1$mediana_teste_espanhol_candidato,
            base_test1$mediana_teste_outros_candidato)

colnames(IQ) <- c("ingles",
                  "logica",
                  "espanhol",
                  "outros")

cor(IQ)

PcIQ <- PCA(IQ,
            scale.unit = F)

fviz_screeplot(PcIQ,
               addlabels = TRUE,
               ylim = c(0, 100))

fviz_pca_var(PcIQ)


IqTest <- predict.PCA(PcIQ,
                      IQ)

DistIQ <- IqTest$dist

###cluster###

Grp1 <- cbind(scale(DistIQ),
              scale(base_test1$ultimo_salario_candidato))

colnames(Grp1) <- c('DistIQ',
                    'UltSal')


Grp1 <- as.data.frame(Grp1)


fviz_nbclust(Grp1,
             kmeans,
             method = "silhouette")


clusterK  <-  kmeans(Grp1,3)

Grp1 <- cbind(Grp1,
              clusterK$cluster)

colnames(Grp1) <- c('DistIQ',
                    'UltSal',
                    'grupo')


ggplot(data = Grp1) +
  geom_point(mapping = aes(x = DistIQ,
                           y = UltSal,
                           color = grupo))



####Logit####

base_test2 <- cbind(base_test1,
                    Grp1$grupo)

fit1 <- glm(aprovado_vaga ~  Grp1$grupo +
            nivel_candidato +
            formacao_candidato +
            area_interesse_candidato +  
            ultimo_salario_candidato,
            data = base_test2,
            family = "binomial")




coeftest(fit1)

summary(fit1)

plot(fit1$residuals)

##predição da probabilidade de ser empregado##

PredFit <- as.numeric(predict(fit1,
                      base_test2,
                      type = "response" ))

##teste para saber se predição compartilha com os "valores reais"
logitgof(base_test2$aprovado_vaga,
         PredFit)

##na bacia das almas##

##F score####

summary(PredFit)


PredFit1 <-  ifelse(PredFit < 0.125, 0, 1)
          


head(PredFit1)
tail(PredFit1)


F1_Score(PredFit1,
         base_test2$aprovado_vaga)



