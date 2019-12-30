#### Limpiamos el ambiente de trabajo
rm(list = ls())

### Cargamos las librerías necesarias
library(stringr)
library(sqldf)
### Cargamos los datos

# Elección 2018
data_2018_1<-read.csv('/home/milo/Documentos/INE/participacion-ciudadana-2009-2018/CC2018_ES_SEXO_EDAD_PARTE1.csv',sep=";")
data_2018_2<-read.csv('/home/milo/Documentos/INE/participacion-ciudadana-2009-2018/CC2018_ES_SEXO_EDAD_PARTE2.csv',sep = ";")

data_2018<-rbind.data.frame(data_2018_1,data_2018_2)
# Elección 2012

data_2012_1<-read.csv('/home/milo/Documentos/INE/participacion-ciudadana-2009-2018/CC2012_ES_SEXO_EDAD_PARTE1.csv',sep=";")
data_2012_2<-read.csv('/home/milo/Documentos/INE/participacion-ciudadana-2009-2018/CC2012_ES_SEXO_EDAD_PARTE2.csv',sep = ";")

data_2012<-rbind.data.frame(data_2012_1,data_2012_2)

### -----------------------------------------------------------------------------------------
### Prueba para calulo de la tasa de participación
prueba<-sqldf('SELECT EDONOM, SUM(LN) ,SUM(SV), SUM(NV), SUM(NS) FROM data_2012 GROUP BY EDONOM')

### La tasa de participación para la elección de 2012 fue del 62.08%
sum(prueba$`SUM(SV)`)/(sum(prueba$`SUM(LN)`)-sum(prueba$`SUM(NS)`))
### -----------------------------------------------------------------------------------------

# Removemos data frame que no necesitamos
rm(data_2012_1,data_2012_2,data_2018_1,data_2018_2,prueba)

# Nos quedamos solo con los campos necesarios
data_2018<-data_2018[c('AELEC','EDOCVE','MPIOCVE','SECCION','EDAD','LN','SV','NV','NS')]
data_2012<-data_2012[c('AELEC','EDOCVE','MPIOCVE','SECCION','EDAD','LN','SV','NV','NS')]

# Cambiamos los formatos de EDOCVE, MPIOCVE, SECCION
data_2012$EDOCVE<-sprintf("%.2i", data_2012$EDOCVE)
data_2012$MPIOCVE<-sprintf("%.3i", data_2012$MPIOCVE)
data_2012$SECCION<-sprintf("%.4i", data_2012$SECCION)

data_2018$EDOCVE<-sprintf("%.2i", data_2018$EDOCVE)
data_2018$MPIOCVE<-sprintf("%.3i", data_2018$MPIOCVE)
data_2018$SECCION<-sprintf("%.4i", data_2018$SECCION)

# Generamos una llave con los campos EDOCVE, MPIOCVE, SECCION

data_2012$LLAVE<-paste(data_2012$EDOCVE,data_2012$MPIOCVE,data_2012$SECCION,sep = "")
data_2018$LLAVE<-paste(data_2018$EDOCVE,data_2018$MPIOCVE,data_2018$SECCION,sep = "")

# Generamos rangos de edad

data_2012$RANG_EDAD<-0
data_2012$RANG_EDAD[data_2012$EDAD==18 | data_2012$EDAD==19]<-"18-19"
data_2012$RANG_EDAD[data_2012$EDAD>=20 & data_2012$EDAD<=24]<-"20-24"
data_2012$RANG_EDAD[data_2012$EDAD>=25 & data_2012$EDAD<=29]<-"20-24"
data_2012$RANG_EDAD[data_2012$EDAD>=30 & data_2012$EDAD<=34]<-"25-29"
data_2012$RANG_EDAD[data_2012$EDAD>=35 & data_2012$EDAD<=39]<-"30-34"
data_2012$RANG_EDAD[data_2012$EDAD>=40 & data_2012$EDAD<=44]<-"35-39"
data_2012$RANG_EDAD[data_2012$EDAD>=45 & data_2012$EDAD<=49]<-"40-44"
data_2012$RANG_EDAD[data_2012$EDAD>=50 & data_2012$EDAD<=54]<-"45-49"
data_2012$RANG_EDAD[data_2012$EDAD>=55 & data_2012$EDAD<=59]<-"50-54"
data_2012$RANG_EDAD[data_2012$EDAD>=60 & data_2012$EDAD<=64]<-"55-59"
data_2012$RANG_EDAD[data_2012$EDAD>=65 & data_2012$EDAD<=69]<-"60-64"
data_2012$RANG_EDAD[data_2012$EDAD>=70 & data_2012$EDAD<=74]<-"65-69"
data_2012$RANG_EDAD[data_2012$EDAD>=75 & data_2012$EDAD<=79]<-"70-74"
data_2012$RANG_EDAD[data_2012$EDAD>=80]<-"80+"

data_2018$RANG_EDAD<-0
data_2018$RANG_EDAD[data_2018$EDAD==18 | data_2018$EDAD==19 ]<-"18-19"
data_2018$RANG_EDAD[data_2018$EDAD>=20 & data_2018$EDAD<=24]<-"20-24"
data_2018$RANG_EDAD[data_2018$EDAD>=25 & data_2018$EDAD<=29]<-"20-24"
data_2018$RANG_EDAD[data_2018$EDAD>=30 & data_2018$EDAD<=34]<-"25-29"
data_2018$RANG_EDAD[data_2018$EDAD>=35 & data_2018$EDAD<=39]<-"30-34"
data_2018$RANG_EDAD[data_2018$EDAD>=40 & data_2018$EDAD<=44]<-"35-39"
data_2018$RANG_EDAD[data_2018$EDAD>=45 & data_2018$EDAD<=49]<-"40-44"
data_2018$RANG_EDAD[data_2018$EDAD>=50 & data_2018$EDAD<=54]<-"45-49"
data_2018$RANG_EDAD[data_2018$EDAD>=55 & data_2018$EDAD<=59]<-"50-54"
data_2018$RANG_EDAD[data_2018$EDAD>=60 & data_2018$EDAD<=64]<-"55-59"
data_2018$RANG_EDAD[data_2018$EDAD>=65 & data_2018$EDAD<=69]<-"60-64"
data_2018$RANG_EDAD[data_2018$EDAD>=70 & data_2018$EDAD<=74]<-"65-69"
data_2018$RANG_EDAD[data_2018$EDAD>=75 & data_2018$EDAD<=79]<-"70-74"
data_2018$RANG_EDAD[data_2018$EDAD>=80]<-"80+"

# Eliminamos algunas columnas que no necesitamos

data_2012<-data_2012[c("LLAVE","AELEC","RANG_EDAD","LN","SV","NV","NS")]
data_2018<-data_2018[c("LLAVE","AELEC","RANG_EDAD","LN","SV","NV","NS")]

# Agrupamos los datos

data_2012<-sqldf('SELECT LLAVE, AELEC, RANG_EDAD, SUM(LN) ,SUM(SV), SUM(NV), SUM(NS) FROM data_2012 GROUP BY LLAVE, AELEC, RANG_EDAD')
data_2018<-sqldf('SELECT LLAVE, AELEC, RANG_EDAD, SUM(LN) ,SUM(SV), SUM(NV), SUM(NS) FROM data_2018 GROUP BY LLAVE, AELEC, RANG_EDAD')

colnames(data_2012)<-c('LLAVE', 'AELEC', 'RANG_EDAD', 'LN' ,'SV', 'NV','NS')
colnames(data_2018)<-c('LLAVE', 'AELEC', 'RANG_EDAD', 'LN' ,'SV', 'NV','NS')

# Calculamos las tasas de particiáción electoral
data_2012$part_elec<-data_2012$SV/(data_2012$LN- data_2012$NS)
data_2018$part_elec<-data_2018$SV/(data_2018$LN- data_2018$NS)

# Eliminamos NA's
data_2012<-data_2012[!is.na(data_2012$part_elec),]
data_2018<-data_2018[!is.na(data_2018$part_elec),]

# Eliminamos los 0's
data_2012<-data_2012[!data_2012$part_elec==0,]
data_2018<-data_2018[!data_2018$part_elec==0,]

# Eliminamos los 1's
data_2012<-data_2012[!data_2012$part_elec==1,]
data_2018<-data_2018[!data_2018$part_elec==1,]

# Juntamos ambos dataframe
# Graficamos las densidades por rango de edad

elec_presi<-rbind.data.frame(data_2012,data_2018)

ggplot(elec_presi,aes(y=as.factor(RANG_EDAD))) +
  geom_density_ridges(aes(x=part_elec,fill=paste(RANG_EDAD,AELEC)), alpha = .6, color = "white", from = 0, to = 1) +
  labs(
    x = "Participación electoral",
    y = "Rango de edad",
    title = "Participación electoral de las elecciones de 2012 y 2018",
    subtitle = "Unidad de análisis: sección electoral ( Elección 2012 n = 65500;  Eleccion 2018 n=65882)",
    caption = "Fuente:INE.Estudio Censal de Participación Ciudadana en las elecciones federales 2009-2018 \nhttps://www.ine.mx/transparencia/datos-abiertos/#/archivo/participacion-ciudadana-2009-2018"
  ) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_cyclical(
    labels = c(`18-19 2012` = "2012", `18-19 2018` = "2018"),
    values = c("#FFBD33", "#900C3F"),
    name = "Elección", guide = "legend"
  ) +
  theme_ridges(grid = FALSE)

