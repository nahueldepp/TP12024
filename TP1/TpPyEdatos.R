#Video Juan
setwd("/home/nahuel/Documentos/PyE/")

library(readxl)

library(reshape)

base_datos<-read_xlsx("/home/nahuel/Documentos/PyE/TpPyEDatos.xlsx")

#Variables categoricas
base_datos$`El lugar que habitan actualmente es:`<-as.factor(base_datos$`El lugar que habitan actualmente es:`)
base_datos$`¿Posee el Certificado de Vivienda (RENABAP)`<-as.factor(base_datos$`¿Posee el Certificado de Vivienda (RENABAP)`)
base_datos$`¿Cómo es la presión del agua?`<-as.factor(base_datos$`¿Cómo es la presión del agua?`)
View(base_datos)


base_datos$`El lugar que habitan actualmente es:`=factor(base_datos$`El lugar que habitan actualmente es:`,levels=c("Alquilado","Ocupado/Tomado","Otro","Prestado","Propio con algún comprobante de tenencia","Propio sin títulos"),
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
renabap_t<-table(base_datos$`¿Posee el Certificado de Vivienda (RENABAP)`)
renabap_frec_rel<-round(renabap_t/sum(renabap_t),2)
pie(renabap_frec_rel,clockwise = T,col=c("#80FFFF","#33FF00" ,"#FF80FF" ),main="Personas que poseen RANABAP \n en barrios argentinos 2022"
    ,labels = c("No 49%","No corresponde 9%","Sí 42%"),lty=2,density = 200, angle = 180)
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,El Observatorio Villero,La Poderosa",
      side=1,at=0.15,line=4,cex=0.7)

#litros por persona

litros_pv<-as.factor(base_datos$`¿Cuántos litros de almacenamiento de agua posee?`)
litros_pv_tabla<-table(litros_pp)
litros_pv_fec_rel<-round(litros_pv_tabla/sum(litros_pv_tabla),2)
litros_orden<-order(litros_pv_fec_rel,decreasing = T)
barplot(litros_pv_fec_rel[litros_orden],xlim=c(0,4),ylab="Frecuencia relativa",ylim=c(0,0.6),col = c("#0066FF"),cex.names = 0.8,main="Almacenamiento de agua en las viviendas\n de barrios argentinos 2022")
grid()
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,El Observatorio Villero,La Poderosa",
      side=1,at=2,line=4,cex=0.7)

#Presion de agua

presion_agua<-table(base_datos$`¿Cómo es la presión del agua?`)
presion_frec_rel<-round(presion_agua/sum(presion_agua),3)
orden_presion<-order(presion_frec_rel,decreasing = T)


barplot(presion_frec_rel[orden_presion],horiz = T,las=1,cex.names=0.8,xlab="Fecuencia Relativa",ylim=c(0,4),col="#0088FF",xlim=c(0,0.5),main="Valoreción de la presión de agua en las vivienda  \nde losbarrios argentinos 2022")


#Capacidad de almacenamiento de agua

capacidad_de_almace<-as.factor(base_datos$`¿Tiene capacidad de almacenamiento de agua en altura?`)
capacidad_al<-table(capacidad_de_almace)
capacidad_al_rel<-round(capacidad_al/sum(capacidad_al),2)

pie(capacidad_al_rel,clockwise = T,col=c("#FFE501","#80FFFF" ),main="Personas que poseen RANABAP \n en barrios argentinos 2022"
    ,labels = c("No 56%","Sí 44%%"),lty=1,density = 800, angle = 180)
mtext("Fuente:Relevamiento de Condiciones Habitacionales 2022,El Observatorio Villero,La Poderosa",
      side=1,at=0.15,line=4,cex=0.7)

#Variables Cuantitativas

#integrantes en una vivienda

integrantes_por_vivienda<-table(base_datos$`¿Cuántos integrantes hay en su vivienda?`)

frec_rel_intvivienda<-round(integrantes_por_vivienda/sum(integrantes_por_vivienda),2)
frec_rel_acu_intvivi<-cumsum(frec_rel_intvivienda)
frec_abs_acu_intvivi<-cumsum(integrantes_por_vivienda)

tabla_de_frec_int_viviendas<-cbind(integrantes_por_vivienda,frec_abs_acu_intvivi,frec_rel_intvivienda,frec_rel_acu_intvivi)

# Varones por vivienda

varones<-table(base_datos$`¿Cuántos varones hay en la vivienda?
`)
frec_rel_varones<-round(varones/sum(varones),2)

#mujeres por vivienda

mujeres<-table(base_datos$`¿Cuántas mujeres hay en la vivienda?`)
frec_rel_mujer<-round(mujeres/sum(mujeres),2)

#MvsV

tabla_sexos<-cbind(mujeres,varones)


#Dormitorios por vivienda

dormitorios<-table(base_datos$`¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`)
frec_rel_dor<-round(dormitorios/sum(dormitorios),2)
barplot(dormitorios)

#maximo de personas q duermen por dormitorio

maxper_por_dor<-table(base_datos$`¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?`)
frec_rel_personas_dor<-round(maxper_por_dor/sum(maxper_por_dor),3)

cplot(frec_rel_personas_dor,col = "green")



