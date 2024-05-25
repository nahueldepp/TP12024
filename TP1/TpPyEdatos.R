#Video Juan
setwd("/home/nahuel/Documentos/PyE/TP1/")

library(readxl)

library(reshape)

base_datos<-read_xlsx("/home/nahuel/Documentos/PyE/TP1/TpPyEDatos.xlsx")

#Variables categoricas
base_datos$`El lugar que habitan actualmente es:`<-as.factor(base_datos$`El lugar que habitan actualmente es:`)
base_datos$`¿Posee el Certificado de Vivienda (RENABAP)?`<-as.factor(base_datos$`¿Posee el Certificado de Vivienda (RENABAP)?`)
base_datos$`¿Cómo es la presión del agua?`<-as.factor(base_datos$`¿Cómo es la presión del agua?`)
View(base_datos)


base_datos$`El lugar que habitan actualmente es:`=factor(base_datos$`El lugar que habitan actualmente es:`,levels=c("Alquilado","Ocupado","Otro","Prestado","Propio CC","Propio SC"),
                                                          labels=c("Alquilado","Ocupado","Otro","Prestado","Propio CC","Propio SC"))
#tabla condicion de la tenencia de la vivienda
condicion_vivienda<-table(base_datos$`El lugar que habitan actualmente es:`)
vivienda_orden<-order(condicion_vivienda,decreasing = T)
frec_rel_convivienda<-round(condicion_vivienda/sum(condicion_vivienda),2)
oreden_frec<-order(frec_rel_convivienda,decreasing = F)

barplot(frec_rel_convivienda[oreden_frec],horiz = T,xlim=c(0,0.5),xlab="Porcentaje relativo",cex.names=0.7,xaxt = "n",main="Condición de tenencia de la vivienda \n en barrios argentinos en 2022",
        las=1,col="#f68199")
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,El Observatorio Villero,La Poderosa",
      side=1,at=0.15,line=4,cex=0.7)
axis(side=1,at=c(0,0.1,0.2,0.3,0.4,0.5))
grid()

#Tabla Renabap
renabap_t<-table(base_datos$`¿Posee el Certificado de Vivienda (RENABAP)?`)
renabap_frec_rel<-round(renabap_t/sum(renabap_t),2)
pie(renabap_frec_rel,clockwise = T,col=c("#AD00FF","#80FFFF","#8DFFD" ),main="Personas que poseen RANABAP \n en barrios argentinos 2022"
    ,labels = c("No 49%","No corresponde 9%","Sí 42%"),lty=1,density =200 , angle = 180)

mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022, La Poderosa",
      side=1,at=0.15,line=4,cex=0.7)

#litros por persona

litros_pv<-as.factor(base_datos$`¿Cuántos litros de almacenamiento de agua posee?`)
litros_pv_tabla<-table(litros_pv)
litros_pv_fec_rel<-round(litros_pv_tabla/sum(litros_pv_tabla),2)
litros_orden<-order(litros_pv_fec_rel,method=c("shell"))
barplot(litros_pv_fec_rel[c(3,1,2)],xlim=c(0,4),ylab="Frecuencia relativa",ylim=c(0,0.6),col = c("#0066FF"),cex.names = 0.8,main="Almacenamiento de agua en las viviendas\n de barrios argentinos 2022")
grid()
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,La Poderosa",
      side=1,at=2,line=4,cex=0.7)


#Presion de agua

presion_agua<-table(base_datos$`¿Cómo es la presión del agua?`)
presion_frec_rel<-round(presion_agua/sum(presion_agua),3)
orden_presion<-order(presion_frec_rel,decreasing = F)


barplot(presion_frec_rel[orden_presion],horiz = T,las=1,cex.names=0.8,xlab="Fecuencia Relativa",ylim=c(0,4),col="#0088FF",xlim=c(0,0.5),main="Valoración de la presión de agua en las vivienda  \nde losbarrios argentinos 2022")
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,La Poderosa",
      side=1,at=0.2,line=4,cex=0.6)

#Capacidad de almacenamiento de agua

capacidad_de_almace<-as.factor(base_datos$`¿Tiene capacidad de almacenamiento de agua en altura?`)
capacidad_al<-table(capacidad_de_almace)
capacidad_al_rel<-round(capacidad_al/sum(capacidad_al),2)

pie(capacidad_al_rel,clockwise = T,col=c( "#AD00FF","#80FFFF" ),main="Viviendas que poseen capacidad de\n almacenamiento en altura  barrios argentinos"
    ,labels = c("No 56%","Sí 44%%"),lty=1,density = 800, angle = 180)
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022 La Poderosa",
      side=1,at=0.1,line=3,cex=0.7)

#Variables Cuantitativas

#integrantes en una vivienda

integrantes_por_vivienda<-table(base_datos$`¿Cuántos integrantes hay en su vivienda?`)

frec_rel_intvivienda<-round(integrantes_por_vivienda/sum(integrantes_por_vivienda),2)
frec_rel_acu_intvivi<-cumsum(frec_rel_intvivienda)
frec_abs_acu_intvivi<-cumsum(integrantes_por_vivienda)

tabla_de_frec_int_viviendas<-cbind(integrantes_por_vivienda,frec_abs_acu_intvivi,frec_rel_intvivienda,frec_rel_acu_intvivi)

boxplot(base_datos$`¿Cuántos integrantes hay en su vivienda?`,yaxt="n",ylab="Integrantes por vivienda",col ="#FF9900"  ,main="Integrantes en una vivienda barrial \n en Argentina 2022")
axis(side=2,at=1:10,)
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,\nEl Observatorio Villero,La Poderosa",
      side=1,at=1,line=4,cex=0.7)
# Varones por vivienda

varones<-(base_datos$`¿Cuántos varones hay en la vivienda?
`)
frec_rel_varones<-round(varones/sum(varones),2)

#mujeres por vivienda

mujeres<-(base_datos$`¿Cuántas mujeres hay en la vivienda?`)


#MvsV

tabla_sexos<-cbind(mujeres,varones)

boxplot(varones,mujeres,col=c(  "#00B3FF","#FF0099"),main="Distrubución de varones y mujeres \nen los barrios argentinos 2022"
        ,yaxt="n")
axis(side=2,at=0:10,)
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,\nEl Observatorio Villero,La Poderosa",
      side=1,at=1.5,line=4,cex=0.7)

#Dormitorios por vivienda

dormitorios<-table(base_datos$`¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`)
frec_rel_dor<-round(dormitorios/sum(dormitorios),2)
plot(frec_rel_dor,ylim=c(0,0.5),xaxt="n",ylab="Frecuencia relativa",xlab="Dormitorios por vivienda",col="#FF00E6",main="Porcentaje de dormitorios por vivienda \n en barrios argentinos 2022")
mtext("Fuente: El Observatorio Villero,La Poderosa",
      side=1,at=2,line=4,cex=0.7)
axis(side=1,at=1:10)

#maximo de personas q duermen por dormitorio

maxper_por_dor<-(base_datos$`¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?`)
frec_rel_personas_dor<-round(maxper_por_dor/sum(maxper_por_dor),3)

boxplot(base_datos$`¿Cuántos integrantes hay en su vivienda?`,base_datos$`¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`,col=c( "#00FF66", "#FF00E6"),xaxt="n",main="Personas por vivienda\nvs dormitorios por vivienda")
axis(side=1,at=c(1,2),labels =c("Integrantes por vivienda","Dormitorios\n por vivienda"),tick = F)
axis(side=2,at=1:10)
mtext("Fuente: El Observatorio Villero,La Poderosa",
      side=1,at=2,line=4,cex=0.7)
grid()
summary(base_datos$`¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?`,)

x<-base_datos$`¿Cuántos integrantes hay en su vivienda?`
y<-base_datos$`¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`
plot(x,y,pch=19,col=c("black"),xlab="Maximo de personas por dormitorio",ylab="Ambientes usados como dormitorios",xlim=c(0,11))
plot(y)


