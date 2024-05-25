setwd("C:/Users/julianita/Desktop/Rpractica")

install.packages("readxl")
library(readxl)

#Leemos el excel 
datos <- read_excel("datos.xlsx",range=cell_rows(c(4, NA)),col_names = FALSE,
                    col_types = c("text","numeric","numeric","text","text","numeric","text"))
#cambiamos el nombre de las columnas a C1 C2 C3 C4 C5 C6 C7
colnames(datos) <- paste0("C", seq_len(ncol(datos)))

print(datos)
frecuencia_de_renabap <- table(datos$C1)

# Convertimos a porcentajes la tabla
slices <- frecuencia_de_renabap
pct <- round(slices/sum(slices)*100,2)

unicos <- unique(datos$C1)

palabras <- paste(unicos,"\n", pct,"%",sep="")
par(bg = "#F2EBD8")
pie(slices,labels = palabras, clockwise = TRUE, col=c("#C5D1B9", "#423515","#a7cdd5"),
    col.lab = "#423515",
    main = "¿POSEE EL CERTIFICADO DE VIVIENDA (RENABAP)?",
    col.main = "#423515",
    border = "#423515",
    sub = "Fuente: La Poderosa. Encuesta realizada a barrios populares de Argentina.",
    cex.sub = 0.9,
    col.sub = "#423515",
    )


#Grafico de bastones cantidad de integrantes por vivienda
par(bg = "#C5D1B9")
frec_cant_integrantes <- table(datos$C2)


plot(frec_cant_integrantes,
     type = "h",
     ylim = c(0, 250),
     xlim = c(0, 11),
     main = "CANTIDAD DE INTEGRANTES
     POR VIVIENDA",
     col.main = "#423515",
     xlab = "Cantidad de integrantes",
     ylab = "Frecuencia Absoluta",
     col.axis = "#423515",
     col.lab = "#423515",
     sub = "Fuente: La Poderosa. Encuesta realizada a barrios populares de Argentina.",
     col.sub = "#423515",
     cex.sub = 0.7,
     col = "#423515")



#grafico escalonado de cantidad de integrantes por vivienda
frec_rel_cant_integrantes <- round(frec_cant_integrantes/sum(frec_cant_integrantes), 4) 
frec_acum_cant_integrantes <- cumsum(frec_cant_integrantes)
frec_acum_rel_cant_integrantes <- round(frec_acum_cant_integrantes/sum(frec_cant_integrantes), 4) 

plot(frec_acum_rel_cant_integrantes,
     type = "s",
     ylim = c(0, 1),
     xlim = c(0, 10),
     main = "CANTIDAD DE INTEGRANTES
     POR VIVIENDA",
     xlab = "Cantidad de integrantes",
     ylab = "Frecuencia relativa acumulada",
     lwd = 2,
     xaxt = "n",
     col.axis = "#423515",
     col.lab = "#423515",
     col = "#423515",
     col.main = "#423515",
     sub = "Fuente: La Poderosa. Encuesta realizada a barrios populares de Argentina.",
     col.sub = "#423515",
     cex.sub = 0.7,)

axis(side = 1, at = seq(0, 10, by = 1))
abline(h = seq(0, 1, 0.2), lty = 3)


#EDAD JEFE HOGAR BOXPLOT
par(bg = "#F2EBD8")
boxplot(datos$C3,
        horizontal = TRUE,
        col = "#C5D1B9",
        main = "EDAD JEFE/A DEL HOGAR", 
        xlab = "Edad",
        sub = "Fuente: La Poderosa. Encuesta realizada a barrios populares de Argentina.",
        cex.sub = 0.7,
        col.sub = "#423515",
        ylim = c(10,90),
        yaxt = "n",
        col.axis = "#423515",
        col.lab = "#423515",
        col.main = "#423515")

axis(side = 1, at = seq(10, 90, by = 10))


datosmax <- max(datos$C6)
#histograma tiempo residencia
print(datosmax)
par(bg = "#C5D1B9")
frec_tiempo <- table(datos$C6)
title_hist = strsplit(c("TIEMPO DE RESIDENCIA;EN LA VIVIENDA ACTUAL (EN AÑOS)"), split = ";")
fuente = "Fuente: La Poderosa. Encuesta realizada a barrios populares de Argentina."
breaks_tiempo <- seq(0,120,10) 
print(breaks_tiempo)
h <- hist(datos$C6,
     border = "#423515",
     col = "#F2EBD8",
     xaxt = 'n', 
     yaxt = 'n',
     ylim = c(0,700),
     breaks = breaks_tiempo,
     xlab = "Tiempo de residencia",
     ylab = "Cantidad de viviendas",
     col.lab = "#423515",
     main = title_hist,
     col.main = "#423515",
     sub = fuente,
     col.sub = "#423515",
     cex.sub = 0.7) 
axis(side = 2, at = seq(0,600, by=100), col = "#423515", col.axis = "#423515") 
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5), col = "#423515")
axis(side = 1, at = seq(0,120, by=10), col = "#423515", col.axis = "#423515") 

# Creamos las tablas necesarias
intervalos_tiempo <- cut(datos$C6, breaks = breaks_tiempo, right = FALSE) #corta las edades en intervalos.
frec_abs_tiempo <- table(datos$C6) 
frec_rel_tiempo <- round(frec_abs_tiempo / sum(frec_abs_tiempo),4)
frec_rel_ac_tiempo <- cumsum(frec_rel_tiempo) 

# Pol?gono de frecuencia para Edad:
plot(frec_rel_tiempo,
     type = "l",
     xaxt="n",
     main = title_hist,
     col.main = "#423515",
     col = "#423515",
     col.axis = "#423515",
     ylab = "Frecuencia relativa",
     xlab = "Tiempo de residencia",
     col.lab = "#423515",
     sub = fuente,
     col.sub = "#423515",
     cex.sub = 0.7,
     lwd = 2)
axis(side = 1, at = seq(0,120, by=10), labels = breaks_tiempo, col = "#423515", col.axis = "#423515")
grid(col = "#F2EBD8")

# Pol?gono acumulativo
plot(frec_rel_ac_tiempo,
     type = "l",
     xaxt="n",
     main = title_hist,
     col.main = "#423515",
     col = "#423515",
     col.axis = "#423515",
     ylab = "Frecuencia acumulada", 
     xlab = "Tiempo de residencia",
     col.lab = "#423515",
     sub = fuente,
     col.sub = "#423515",
     cex.sub = 0.7,lwd = 2)
axis(side = 1, at = seq(0,120, by=10), labels = breaks_tiempo, col = "#423515", col.axis = "#423515")
grid(col = "#F2EBD8")

#boxplot comparativo 
par(bg = "#F2EBD8")
boxplot(datos$C3~datos$C2, #queremos analizar la edad de dos sexos
        col = "#C5D1B9",
        main = "EDAD JEFE DEL HOGAR RESPECTO 
        A LA CANTIDAD DE INTEGRANTES", 
        col.main = "#423515",
        ylab = "Edad del jefe/a del hogar ",
        xlab = "Cantidad de integrantes por vivienda",
        col.lab = "#423515",
        sub = fuente,
        col.sub = "#423515",
        cex.sub = 0.7,
        col.axis = "#423515",
        ylim = c(20,90),
        boxwex = 0.5,) #ancho mas chico


max(datos$C3)
#barras comparativas
for (i in seq_along(datos$C7)){
  if (grepl("No", datos$C7[i])){ 
    datos$C7[i] <- "No tiene capacidad
    de almacenamiento 
    de agua en altura"
  }
  
  if (grepl("Sí", datos$C7[i])){ 
    datos$C7[i] <- "Si tiene capacidad
    de almacenamiento 
    de agua en altura"
  }
}

for (i in seq_along(datos$C4)){
  if (grepl("No", datos$C4[i])){ 
    datos$C4[i] <- "No posee instalación de agua en el baño"
  }
  
  if (grepl("S", datos$C4[i])){ 
    datos$C4[i] <- "Posee instalación de agua en el baño"
  }
}


#barras comparativo
#si posee instalación de agua en el baño (si/no) y tiene capacidad de almacenamiento de agua en altura
tabla_bivariada_agua <- table(datos$C7,datos$C4)


orden_inverso2 <- order(tabla_bivariada_agua, decreasing = TRUE)
# Crear el gráfico de barras compuesto
par(mar = c(5, 10, 5, 5))
barplot(t(tabla_bivariada_agua),
        beside = TRUE,
        horiz = TRUE,
        legend.text = colnames(tabla_bivariada_agua),
        args.legend = list(x = "right",y = "center",text.col = "#423515" ,box.lty=1,box.col = "#423515"),
        main = "POSEE INSTALACION DE AGUA EN EL BAÑO 
        RESPECTO A CAPACIDAD DE ALMACENAMIENTO 
        DE AGUA EN ALTURA",
        width = 0.5,
        xlab = expression(bold("Numero de hogares")),
        xlim = c(0, 600),
        xaxt="n",
        las = 1,
        col = c("#423515","#C5D1B9"),
        col.lab = "#423515",
        col.main = "#423515",
        col.axis = "#423515")
axis(side = 1, at = seq(0,600, by=50), col = "#423515", col.axis = "#423515")

