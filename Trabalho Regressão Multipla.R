#Trabalho RLM

#esvaziando dados já carregados - comando rm
rm(list=ls())

#Definindo o diretório de trabalho - para importação de dados
#Mudança de diretorio
setwd("C:/Users/astol/Desktop/teste R")

#mostra o diretorio de trabalho
getwd ()

# Listar os arquivos do diretório
dir()

#FBCF e as subdivisões estão todas em %, e nos temos apenas em valoro PIB e o Valor Adicionado as exportações, logo temos que
#caluluar a partir do PIB os valores das subdivisões do FBCF, pois as % desses são do total de FBCF, e a do FBCF é do PIB

PIB_BR_BI = read.csv("Base de Dados OCDE PIB-Brasil(2000-2020).csv", header = T, sep = ",", dec = ".")
FBCF_CSV = read.csv("Base de Dados OCDE FBCF_PIB%(2000-2020).csv", header = T, sep = ",", dec = ".")
FBCF_Porcentagem = FBCF_CSV$OBS_VALUE/100
FBCF_Valor = (PIB_BR_BI$OBS_VALUE*FBCF_Porcentagem)
FBCF_Empresas_Porcentagem = read.csv("Base de Dados OCDE FBCF-Empresas(2000-2020).csv", header = T, sep = ",", dec = ".")
FBCF_Governo_Porcentagem = read.csv("Base de Dados OCDE FBCF-Governo(2000-2020).csv", header = T, sep = ",", dec = ".")
FBCF_Familias_Porcentagem = read.csv("Base de Dados OCDE FBCF-Familias e ONGs(2000-2020).csv", header = T, sep = ",", dec = ".")
FBCF_Empresas_Valor = ((FBCF_Empresas_Porcentagem$OBS_VALUE)/100)*FBCF_Valor
FBCF_Governo_Valor = ((FBCF_Governo_Porcentagem$OBS_VALUE)/100)*FBCF_Valor
FBCF_Familias_Valor = ((FBCF_Familias_Porcentagem$OBS_VALUE)/100)*FBCF_Valor
VAE_BR = read.csv("Base de Dados OCDE VAE(2000-2020).csv", header = T, sep = ",", dec = ".")
VAE_BR_BI = VAE_BR$OBS_VALUE/1000

#FBCF_Empresas_Valor
#FBCF_Governo_Valor
#FBCF_Familias_Valor
#Essas 3 variaveis acima são dos B1,B2 e B3
#VAE_BR_BI
#É a variavel B0
#todos os valores estão convertidos em Bilhões

#Instalação de pacotes:
install.packages("wooldridge")
install.packages("AER")
install.packages("car")
install.packages("effects")
install.packages("stargazer")
install.packages("lmtest")
install.packages("skedastic")
install.packages("orcutt")

#carrega o pacote
library(wooldridge)
library(AER)
library(car)
library(effects)
library(stargazer)
library(lmtest)
library(skedastic)
library(orcutt)

#Criando Série temporal (2000 a 2020)
Empresas = ts(FBCF_Empresas_Valor, start=2000, end=2020, frequency=1 )
Governo = ts(FBCF_Governo_Valor, start=2000, end=2020, frequency=1 )
Famílias = ts(FBCF_Familias_Valor, start=2000, end=2020, frequency=1 )
VAE = ts(VAE_BR_BI, start=2000, end=2020, frequency=1 )

#Gráficos de séries temporais
par(mfrow=c(2,2))
plot.ts(Empresas, ylab="Valor em Bilhões", xlab="período",las=2 ,main= "Formação Bruta de Capital Fixo Empresas", col=c("green"))
plot.ts(Famílias, ylab="Valor em Bilhões", xlab="período",las=2 ,main= "Formação Bruta de Capital Fixo Famílias", col=c("blue"))
plot.ts(Governo, ylab="Valor em Bilhões", xlab="período",las=2 ,main= "Formação Bruta de Capital Fixo Governo", col=c("red"))
plot.ts(VAE, ylab="Valor em Bilhões", xlab="período",las=2 ,main= "Valor Adicionado Exportação", col=c("orange"))

#Modelo de regressão
modregVAE = lm(VAE~Empresas +Governo +Famílias)
summary(modregVAE)

modregVAEdiff = lm(diff(VAE)~ diff(Empresas) +diff(Governo) + diff(Famílias))
summary(modregVAEdiff)
#Gráficos da Regressão (Diagnósticos)
plot(modregVAE)
plot(modregVAEdiff)

#Gráficos da Regressão (Efeitos parciais)
plot(allEffects(modregVAE))
plot(allEffects(modregVAEdiff))

#Quadrado dos resíduos
usq<-(modregVAE$residuals)^2
usqdiff = (modregVAEdiff$residuals)^2

#Análise gráfica dos Resíduos
par(mfrow=c(1,1))
plot(usq)
plot(usqdiff)
plot(res)
#teste de autocorrelação
dwtest(modregVAE)
dwtest(modregVAEdiff)

#outro teste de autocorrelação
bgtest(modregVAE, order=1, type="Chisq")
bgtest(modregVAEdiff, order=1, type =  "Chisq")

#corrigir o modelo com autocorrelação
reg_orc<- cochrane.orcutt(modregVAE)
summary(reg_orc)

#outra forma de corrigir o modelo com autocorrelação
coeftest(modregVAE)

DP_robustos<-coeftest(modregVAE,vcov=vcovHAC)

#exibir os resultados de todas os modelos de regrressões
stargazer(list(modregVAE,modregVAEdiff,reg_orc, DP_robustos), type="text", column.labels = c("Regressão 1", "Regressão DIFF", "Cochrane-Orcutt", "EP Robustos"), keep.stat=c("n","rsq", "F","ser"))
