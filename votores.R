## Modelos mencionados en el artículo 
## "Voto retrospectivo en América Latina. ¿Por qué se premia el mal desempeño?"

AL <- read.csv("alrawdata.csv")
names(AL)
## Variables en la base de datos
names(AL)

## Rezago de crecimiento porcentual del PIB per capita
library("dyn")
AL$growth.lag <- lag(AL$gdppc.g, 1)  

## Indicadora de Alba
AL$alba <- ifelse((AL$code == "BOL" | AL$code == "ECU" | AL$code ==  "NIC"
                   | AL$code ==  "VEN"), 1, 0)

## Indicadores para Bolivia y Nicaragua
AL$bol <- ifelse((AL$code == "BOL"), 1, 0)
AL$nic <- ifelse((AL$code == "NIC"), 1, 0)

## Modelo de efectos aletoris con triple interacción (nota al pie 8)
library("plm")
random <- plm(partido.pres.perc ~ growth.lag*inmediata*alba , data = AL, index = c("pais", "year"),
              model = "random")

## Resultados con errores estándar robustos
library("clubSandwich")
coef_test(random, vcov = "CR1", test = "Satterthwaite") 

## un punto porcentual adicional en el crecimiento del PIB per cápita
## se asocia con un 1.1% adicional en el voto por el partido en el gobierno
## en los países del Alba, con reelección
coef(random) # Coeficientes del modelo
coef(random)[2] + coef(random)[5] + coef(random)[6] + coef(random)[8]

## Modelo de Efectos fijos con dummy por pais y año
## Regresor: reelección inmediata (nota al pie 9)

growth.i <- plm(gdppc.g ~ inmediata, data = AL, index = c("pais", "year"),
                model = "within", effect = "twoway")
summary(growth.i)

## Error estándar robusto 
library("lmtest")
coeftest(growth.i, vcov = vcovHC, type = "HC1")

## El crecimiento medio de los casos de mejor desempeño (Bolivia y Nicaragua)
## es indistinguible del observado en el resto de los países (nota al pie 10)
ols <- lm(gdppc.g ~ bol + nic, data = AL)
summary(ols)

## Errores estándar robustos
library(multiwayvcov)
library(lmtest)
vcov_pais <- cluster.vcov(ols, AL$pais)
coeftest(ols, vcov_pais)
