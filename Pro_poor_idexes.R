############# bases de datos #############
library(foreign)
library(tidyverse)
library(survey)

########## 2010

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
Deciles_por_fuente_2010<-read.dbf("Nacional por fuente por DECIL estimaciones 2010.dbf")

Conc_2010<-read.dbf("Conc2010.dbf")

bottom_40_ingreso_2010<-read.dbf("bottom_por_ingresos_2010.dbf")

names(bottom_40_ingreso_2010)<-c("bottom_ingreso_2010")

bottom_40_consumo_2010<-read.dbf("Consumo_por_bottom_2010.dbf")

names(bottom_40_consumo_2010)<-c("bottom_consumo_2010")

consumo_2010<-read.dbf("Nacional Consumo  por DECIL 2010.dbf")

consumo_2010<-as.data.frame(consumo_2010)

names(consumo_2010)=c("Consumo_2010")

names(Deciles_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba=Deciles_por_fuente_2010$TRABAJO2010+Deciles_por_fuente_2010$RENTAS2010+
           Deciles_por_fuente_2010$JUBILACION2010+Deciles_por_fuente_2010$BECAS2010+
           Deciles_por_fuente_2010$DONATIVOS2010+Deciles_por_fuente_2010$REMESAS2010+
           Deciles_por_fuente_2010$BENEGOBIERNO2010+Deciles_por_fuente_2010$`TRANS HOG2010`+
           Deciles_por_fuente_2010$`TRANS INST2010`+Deciles_por_fuente_2010$`ESTIM ALQU2010`+
           Deciles_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba)


Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba2)




######### 2012

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))

Deciles_por_fuente_2012<-read.dbf("Nacional por fuente por DECIL estimaciones 2012.dbf")

Conc_2012<-read.dbf("Conc_2012.dbf")

bottom_40_ingreso_2012<-read.dbf("bottom_por_ingresos_2012.dbf")

names(bottom_40_ingreso_2012)<-c("bottom_ingreso_2012")

bottom_40_consumo_2012<-read.dbf("Consumo_por_bottom_2012.dbf")

names(bottom_40_consumo_2012)<-c("bottom_consumo_2012")


consumo_2012<-read.dbf("Nacional Consumo  por DECIL 2012.dbf")

consumo_2012<-as.data.frame(consumo_2012)

names(consumo_2012)=c("Consumo_2012")

names(Deciles_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate(prueba=Deciles_por_fuente_2012$TRABAJO2012+Deciles_por_fuente_2012$RENTAS2012+
           Deciles_por_fuente_2012$JUBILACION2012+Deciles_por_fuente_2012$BECAS2012+
           Deciles_por_fuente_2012$DONATIVOS2012+Deciles_por_fuente_2012$REMESAS2012+
           Deciles_por_fuente_2012$BENEGOBIERNO2012+Deciles_por_fuente_2012$`TRANS HOG2012`+
           Deciles_por_fuente_2012$`TRANS INST2012`+Deciles_por_fuente_2012$`ESTIM ALQU2012`+
           Deciles_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(Deciles_por_fuente_2012$`ING COR2012`,Deciles_por_fuente_2012$prueba)

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(Deciles_por_fuente_2012$`ING COR2012`,Deciles_por_fuente_2012$prueba2)




######### 2014

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2014/ENIGH_2014"))
Deciles_por_fuente_2014<-read.dbf("Nacional por fuente por DECIL estimaciones 2014.dbf")

Conc_2014<-read.dbf("Conc_2014.dbf")

bottom_40_ingreso_2014<-read.dbf("bottom_por_ingresos_2014.dbf")

names(bottom_40_ingreso_2014)<-c("bottom_ingreso_2014")

bottom_40_consumo_2014<-read.dbf("Consumo_por_bottom_2014.dbf")

names(bottom_40_consumo_2014)<-c("bottom_consumo_2014")


consumo_2014<-read.dbf("Nacional Consumo  por DECIL 2014.dbf")

consumo_2014<-as.data.frame(consumo_2014)

names(consumo_2014)=c("Consumo_2014")

names(Deciles_por_fuente_2014)=c("ING COR2014", "TRABAJO2014", "SUBORDINADO2014", "NEGOCIOS2014","OTROS TRAB2014", "RENTAS2014","UTILIDAD2014", "ARRENDA2014", "TRANSFER2014","JUBILACION2014", "BECAS2014", "DONATIVOS2014", "REMESAS2014", "BENEGOBIERNO2014", "TRANS HOG2014", "TRANS INST2014", "ESTIM ALQU2014", "OTROS INGRESOS2014")

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate(prueba=Deciles_por_fuente_2014$TRABAJO2014+Deciles_por_fuente_2014$RENTAS2014+
           Deciles_por_fuente_2014$JUBILACION2014+Deciles_por_fuente_2014$BECAS2014+
           Deciles_por_fuente_2014$DONATIVOS2014+Deciles_por_fuente_2014$REMESAS2014+
           Deciles_por_fuente_2014$BENEGOBIERNO2014+Deciles_por_fuente_2014$`TRANS HOG2014`+
           Deciles_por_fuente_2014$`TRANS INST2014`+Deciles_por_fuente_2014$`ESTIM ALQU2014`+
           Deciles_por_fuente_2014$`OTROS INGRESOS2014`)

all.equal(Deciles_por_fuente_2014$`ING COR2014`,Deciles_por_fuente_2014$prueba)

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate("TRANSFERENCES2014"=JUBILACION2014+BECAS2014+DONATIVOS2014+REMESAS2014+`TRANS HOG2014`+`TRANS INST2014`,
         "OTHERS2014"=`ESTIM ALQU2014`+`OTROS INGRESOS2014`)

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate(prueba2=TRABAJO2014+RENTAS2014+BENEGOBIERNO2014+TRANSFERENCES2014+OTHERS2014)

all.equal(Deciles_por_fuente_2014$`ING COR2014`,Deciles_por_fuente_2014$prueba2)


######## 2016

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2016/ENIGH_2016"))
Deciles_por_fuente_2016<-read.dbf("Nacional por fuente por DECIL estimaciones 2016.dbf")

Conc_2016<-read.dbf("Conc_2016.dbf")

bottom_40_ingreso_2016<-read.dbf("bottom_por_ingresos_2016.dbf")

names(bottom_40_ingreso_2016)<-c("bottom_ingreso_2016")

bottom_40_consumo_2016<-read.dbf("Consumo_por_bottom_2016.dbf")

names(bottom_40_consumo_2016)<-c("bottom_consumo_2016")


consumo_2016<-read.dbf("Nacional Consumo  por DECIL 2016.dbf")

consumo_2016<-as.data.frame(consumo_2016)

names(consumo_2016)=c("Consumo_2016")

names(Deciles_por_fuente_2016)=c("ING COR2016", "TRABAJO2016", "SUBORDINADO2016", "NEGOCIOS2016","OTROS TRAB2016", "RENTAS2016","UTILIDAD2016", "ARRENDA2016", "TRANSFER2016","JUBILACION2016", "BECAS2016", "DONATIVOS2016", "REMESAS2016", "BENEGOBIERNO2016", "TRANS HOG2016", "TRANS INST2016", "ESTIM ALQU2016", "OTROS INGRESOS2016")

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate(prueba=Deciles_por_fuente_2016$TRABAJO2016+Deciles_por_fuente_2016$RENTAS2016+
           Deciles_por_fuente_2016$JUBILACION2016+Deciles_por_fuente_2016$BECAS2016+
           Deciles_por_fuente_2016$DONATIVOS2016+Deciles_por_fuente_2016$REMESAS2016+
           Deciles_por_fuente_2016$BENEGOBIERNO2016+Deciles_por_fuente_2016$`TRANS HOG2016`+
           Deciles_por_fuente_2016$`TRANS INST2016`+Deciles_por_fuente_2016$`ESTIM ALQU2016`+
           Deciles_por_fuente_2016$`OTROS INGRESOS2016`)

all.equal(Deciles_por_fuente_2016$`ING COR2016`,Deciles_por_fuente_2016$prueba)

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate("TRANSFERENCES2016"=JUBILACION2016+BECAS2016+DONATIVOS2016+REMESAS2016+`TRANS HOG2016`+`TRANS INST2016`,
         "OTHERS2016"=`ESTIM ALQU2016`+`OTROS INGRESOS2016`)

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate(prueba2=TRABAJO2016+RENTAS2016+BENEGOBIERNO2016+TRANSFERENCES2016+OTHERS2016)

all.equal(Deciles_por_fuente_2016$`ING COR2016`,Deciles_por_fuente_2016$prueba2)


############ 2018

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018"))
Deciles_por_fuente_2018<-read.dbf("Nacional por fuente por DECIL estimaciones 2018.dbf")

consumo_2018<-read.dbf("Nacional Consumo  por DECIL 2018.dbf")

bottom_40_ingreso_2018<-read.dbf("bottom_por_ingresos_2018.dbf")

names(bottom_40_ingreso_2018)<-c("bottom_ingreso_2018")

bottom_40_consumo_2018<-read.dbf("Consumo_por_bottom_2018.dbf")

names(bottom_40_consumo_2018)<-c("bottom_consumo_2018")


Conc_2018<-read.dbf("Conc_2018.dbf")

consumo_2018<-as.data.frame(consumo_2018)

names(consumo_2018)=c("Consumo_2018")

names(Deciles_por_fuente_2018)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba=Deciles_por_fuente_2018$TRABAJO2018+Deciles_por_fuente_2018$RENTAS2018+
           Deciles_por_fuente_2018$JUBILACION2018+Deciles_por_fuente_2018$BECAS2018+
           Deciles_por_fuente_2018$DONATIVOS2018+Deciles_por_fuente_2018$REMESAS2018+
           Deciles_por_fuente_2018$BENEGOBIERNO2018+Deciles_por_fuente_2018$`TRANS HOG2018`+
           Deciles_por_fuente_2018$`TRANS INST2018`+Deciles_por_fuente_2018$`ESTIM ALQU2018`+
           Deciles_por_fuente_2018$`OTROS INGRESOS2018`)

all.equal(Deciles_por_fuente_2018$`ING COR2018`, Deciles_por_fuente_2018$prueba)

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate("TRANSFERENCES2018"=JUBILACION2018+BECAS2018+DONATIVOS2018+REMESAS2018+`TRANS HOG2018`+`TRANS INST2018`,
         "OTHERS2018"=`ESTIM ALQU2018`+`OTROS INGRESOS2018`)

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba2=TRABAJO2018+RENTAS2018+BENEGOBIERNO2018+TRANSFERENCES2018+OTHERS2018)

all.equal(Deciles_por_fuente_2018$`ING COR2018`,Deciles_por_fuente_2018$prueba2)

############ shared prosperity ###########
########### 2010 - 2012 #################
########## ingreso 

shared_2010_2012_ingreso<-data.frame(bottom_40_ingreso_2010,bottom_40_ingreso_2012)


shared_2010_2012_ingreso<-shared_2010_2012_ingreso%>%
  mutate(Rate=((bottom_ingreso_2012-bottom_ingreso_2010)/bottom_ingreso_2010)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2010_2012_ingreso[3],2)

########## consumo

shared_2010_2012_consumo<-data.frame(bottom_40_consumo_2010,bottom_40_consumo_2012)

shared_2010_2012_consumo<-shared_2010_2012_consumo%>%
  mutate(Rate=((bottom_consumo_2012-bottom_consumo_2010)/bottom_consumo_2010)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2010_2012_consumo[3],2)

########### 2012 - 2014 #################
########## ingreso 

shared_2012_2014_ingreso<-data.frame(bottom_40_ingreso_2012,bottom_40_ingreso_2014)


shared_2012_2014_ingreso<-shared_2012_2014_ingreso%>%
  mutate(Rate=((bottom_ingreso_2014-bottom_ingreso_2012)/bottom_ingreso_2012)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2012_2014_ingreso[3],2)

########## consumo

shared_2012_2014_consumo<-data.frame(bottom_40_consumo_2012,bottom_40_consumo_2014)

shared_2012_2014_consumo<-shared_2012_2014_consumo%>%
  mutate(Rate=((bottom_consumo_2014-bottom_consumo_2012)/bottom_consumo_2012)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2012_2014_consumo[3],2)

########### 2014 - 2016 #################
########## ingreso 

shared_2014_2016_ingreso<-data.frame(bottom_40_ingreso_2014,bottom_40_ingreso_2016)


shared_2014_2016_ingreso<-shared_2014_2016_ingreso%>%
  mutate(Rate=((bottom_ingreso_2016-bottom_ingreso_2014)/bottom_ingreso_2014)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2014_2016_ingreso[3],2)

########## consumo

shared_2014_2016_consumo<-data.frame(bottom_40_consumo_2014,bottom_40_consumo_2016)

shared_2014_2016_consumo<-shared_2014_2016_consumo%>%
  mutate(Rate=((bottom_consumo_2016-bottom_consumo_2014)/bottom_consumo_2014)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2014_2016_consumo[3],2)

########### 2016 - 2018 #################
########## ingreso 

shared_2016_2018_ingreso<-data.frame(bottom_40_ingreso_2016,bottom_40_ingreso_2018)


shared_2016_2018_ingreso<-shared_2016_2018_ingreso%>%
  mutate(Rate=((bottom_ingreso_2018-bottom_ingreso_2016)/bottom_ingreso_2016)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2016_2018_ingreso[3],2)

########## consumo

shared_2016_2018_consumo<-data.frame(bottom_40_consumo_2016,bottom_40_consumo_2018)

shared_2016_2018_consumo<-shared_2016_2018_consumo%>%
  mutate(Rate=((bottom_consumo_2018-bottom_consumo_2016)/bottom_consumo_2016)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2016_2018_consumo[3],2)


########### 2010 - 2014 #################
########## ingreso 

shared_2010_2014_ingreso<-data.frame(bottom_40_ingreso_2010,bottom_40_ingreso_2014)


shared_2010_2014_ingreso<-shared_2010_2014_ingreso%>%
  mutate(Rate=((bottom_ingreso_2014-bottom_ingreso_2010)/bottom_ingreso_2010)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2010_2014_ingreso[3],2)

########## consumo

shared_2010_2014_consumo<-data.frame(bottom_40_consumo_2010,bottom_40_consumo_2014)

shared_2010_2014_consumo<-shared_2010_2014_consumo%>%
  mutate(Rate=((bottom_consumo_2014-bottom_consumo_2010)/bottom_consumo_2010)*100,
         nivel=c("Mean","Upper 60","Bottom 40"))

round(shared_2010_2014_consumo[3],2)

