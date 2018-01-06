# PRIMERA PARTE
# Carga, establecimiento de tipos adecuados a cada columna, selección y limpieza de datos.
library(ggplot2) # Visualización de datos
library(readr) # Manejo de ficheros CSV
library(nortest) # Tests de normalidad en una distribución

# Carga del fichero de datos de citas médicas
citas <- read.csv("./inputData/KaggleV2May2016.csv")

# Selección solo aquellas citas en que el paciente sea adulto (tenga la mayoría de edad, >18 años)
# y no tenga más de 100 años. A partir de esa edad, consideramos el registro como sospechoso de
# no ser correcto.
citas <- sqldf(c('delete from citas where Age < 18', 'select * from citas')) # 27380 citas
citas <- sqldf(c('delete from citas where Age > 100', 'select * from citas')) # 7 citas

# Establecimiento de los tipos de datos correctos para cada columna
# ScheduledDay es una fecha
citas$ScheduledDay <- as.Date(citas$ScheduledDay) 
# AppointmentDay es una fecha
citas$AppointmentDay <- as.Date(citas$AppointmentDay)
# Scholarship, variable binaria (T/F)
citas$Scholarship <- ifelse(citas$Scholarship == 1, T, F)
# Handcap, variable binaria (T/F)
citas$Handcap <- ifelse(citas$Handcap == 0, F, T)
# SMS_received, variable binaria (T/F)
citas$SMS_received <- ifelse(citas$SMS_received == 1, T, F)
# Creamos una nueva columna (GenderTF) en el que la transformamos
# los valores de la columna Gender (M/F) a una variable binaria (M = true /Female = false)
citas$GenderTF <- ifelse(citas$Gender == "M", T, F)
# No.show, variable binaria (T/F)
citas$No.show <- ifelse(citas$No.show == "Yes", T, F)

# Seleccionar solo las variables de interés a analizar
variablesDeInteres <- c("GenderTF","Age","Scholarship","Handcap","SMS_received","No.show")
citas <- citas[variablesDeInteres]

# Escribir el fichero a analizar en CSV
write.csv(citas, file = "./outputData/citas.csv")

# Sumario: Obtención de primeros datos descriptivos globales
summary(citas)

# Número de citas en total. Número de filas de un data frame
print(paste("Número total de citas consideradas:", nrow(citas)))

# Número total de asistencias
asistencias <- sqldf('select * from citas where "No.show" = 0')
print(paste("Número total de asistencias:", nrow(asistencias)))

# Número total de faltas
faltas <- sqldf('select * from citas where "No.show" = 1')
print(paste("Número total de faltas:", nrow(faltas)))

# Pastel con citas según su asistencia
dPastelCitas.slices <- c(nrow(asistencias), nrow(faltas)) 
dPastelCitas.lbls <- c("Asistencias", "Faltas")
dPastelCitas.pct <- round(dPastelCitas.slices/sum(dPastelCitas.slices)*100)
dPastelCitas.lbls <- paste(dPastelCitas.lbls, dPastelCitas.pct) # add percents to labels 
dPastelCitas.lbls <- paste(dPastelCitas.lbls,"%",sep="") # ad % to labels 
pie(dPastelCitas.slices,labels = dPastelCitas.lbls, col=rainbow(length(dPastelCitas.lbls)),
    main="Citas según asistencia")

# SEGUNDA PARTE: Visualización y análisis de variables dicotómicas. Comparación de proporciones
# de ciertas variables de interés y su posible contribución a una posible falta a una cita.

# Variables dicotómicas de interés a visualizar
variableAVisualizar <- c("GenderTF", "Scholarship", "Handcap", "SMS_received")

for (i in seq_along(variableAVisualizar)) {
  faltasParametroFalse <- sqldf(paste('select count(*) as total from citas where "No.show" = 1 and ', variableAVisualizar[i], ' = 0'))
  faltasParametroTrue <- sqldf(paste('select count(*) as total from citas where "No.show" = 1 and ', variableAVisualizar[i], ' = 1'))
  asistenciasParametroFalse <- sqldf(paste('select count(*) as total from citas where "No.show" = 0 and ', variableAVisualizar[i], ' = 0'))
  asistenciasParametroTrue <- sqldf(paste('select count(*) as total from citas where "No.show" = 0 and ', variableAVisualizar[i], ' = 1'))
  
  # Crear el data frame para la gráfica de proporcione
  Grupo <- c(rep(paste(variableAVisualizar[i], "FALSE"), 2), rep(paste(variableAVisualizar[i], "TRUE"), 2))
  Resultado <- rep(c("Asistencia", "Falta"), 2)
  valores <- c(asistenciasParametroFalse$total, faltasParametroFalse$total, asistenciasParametroTrue$total, faltasParametroTrue$total)
  datos <- data.frame(Grupo, Resultado, valores)
  
  # Gráfica de porcentajes apilados construida sobre el data frame
  grafico <- ggplot(datos, aes(fill=Resultado, y=valores, x=Grupo)) + 
    geom_bar(stat="identity", position="fill") +
    scale_fill_manual(values = c("green", "red"))
  print(grafico)
}

# Variables dicotómicas de interés a analizar
variableAAnalizar <- c("GenderTF", "Scholarship", "Handcap", "SMS_received")

# Nivel de significación elegido para las pruebas estadísticas
nivelDeSignificacion <- 0.05

for (i in seq_along(variableAAnalizar)) {
  faltasParametroFalse <- sqldf(paste('select count(*) as total from citas where "No.show" = 1 and ', variableAAnalizar[i], ' = 0'))
  faltasParametroTrue <- sqldf(paste('select count(*) as total from citas where "No.show" = 1 and ', variableAAnalizar[i], ' = 1'))
  asistenciasParametroFalse <- sqldf(paste('select count(*) as total from citas where "No.show" = 0 and ', variableAAnalizar[i], ' = 0'))
  asistenciasParametroTrue <- sqldf(paste('select count(*) as total from citas where "No.show" = 0 and ', variableAAnalizar[i], ' = 1'))
  
  # Efectuar un test de dos proporciones. Este test mide la probabilidad de que las
  # proporcion de faltas sea la misma con respecto a la variable dicotómica (true/false)
  # analizada.
  print("***************************************************************************")
  print(paste("VARIABLE ANALIZADA: ", variableAAnalizar[i]))
  
  # Las faltas a una cita las consideramos como "éxitos"
  exitos <- c(
    faltasParametroTrue$total,
    faltasParametroFalse$total
  )
  
  # Totales para faltas y asistencias
  totales <- c(
    faltasParametroTrue$total + faltasParametroFalse$total,
    asistenciasParametroTrue$total + asistenciasParametroFalse$total
  )
  
  testProporciones <- prop.test(exitos, totales)
  print("RESULTADO TEST DE DOS PROPORCIONES")
  print(testProporciones)
  print(paste("H0 = La proporción de faltas para los valores true y false de la variable",variableAAnalizar[i],"es la misma."))
  print(paste("Utilizando un nivel de significación de", nivelDeSignificacion))
  print(ifelse(testProporciones$p.value < nivelDeSignificacion, "H0 -> RECHAZADA", "H0 -> ACEPTADA"))
  print("***************************************************************************")
  
  # Crear la tabla de contingencias a utilizar para los tests de CHI-CUADRADO y FISHER
  print("Tabla de contingencia utilizada para los tests de CHI-CUADRADO y FISHER")
  
  # Crear la tabla de contingencia e imprimirla en pantalla
  tablaContingencia <- matrix(
    c(faltasParametroTrue$total,
      faltasParametroFalse$total,
      asistenciasParametroTrue$total,
      asistenciasParametroFalse$total),
    nrow=2, byrow=T)
  rownames(tablaContingencia) <- c("Falta", "Asistencia")
  colnames(tablaContingencia) <- c(paste(variableAAnalizar[i], "true"), paste(variableAAnalizar[i], "false"))
  print(tablaContingencia)
  print("***************************************************************************")
  
  # Efectuar el test de chi-cuadrado e imprimir resultados
  testChiCuadrado <- chisq.test(tablaContingencia)
  print("RESULTADO DEL TEST DE CHI-CUADRADO")
  print(testChiCuadrado)
  print(paste("H0 = Las variables de asistencia o no a una cita y",variableAAnalizar[i],"son independientes."))
  print(paste("Utilizando un nivel de significación para la prueba de", nivelDeSignificacion))
  print(ifelse(testChiCuadrado$p.value < 0.01, "H0 -> RECHAZADA", "H0 -> ACEPTADA"))
  print("***************************************************************************")
  
  # Efectuar el test exacto de Fisher e imprimir resultados
  testFisher <- fisher.test(tablaContingencia)
  print("RESULTADO DEL TEST EXACTO FISHER")
  print(testFisher)
  print(paste("H0 = Las variables de asistencia o no a una cita y",variableAAnalizar[i],"son independientes."))
  print(paste("Utilizando un nivel de significación para la prueba de", nivelDeSignificacion))
  print(ifelse(testFisher$p.value < 0.01, "H0 -> RECHAZADA", "H0 -> ACEPTADA"))
  print("***************************************************************************")
}

# TERCERA PARTE: Análisis de la variable cuantitativa edad (Age)

# Esta función imprime el valor p resultado de un test de normalidad y el resultado de acuerdo con el
# nivel de significación elegido
imprimeResultadoTestNormalidad <- function(nombreCurva, valorP, nivelDeSignificacion){
  print(paste(nombreCurva, "valor p: ", faltasNormalValorP))
  print(ifelse(faltasNormalValorP < nivelDeSignificacion, "NO es normal", "Es normal"))
}

# Ejecuta los tests de normalidad en secuencia e imprime los resultados para un vector de datos
ejecutaTestsDeNormalidad <- function(nombre, datos, nivelDeSignificacion) {
  print("EJECUTANDO TESTS DE NORMALIDAD SOBRE:", nombre)
  
  # Anderson-Darling
  valorP <- ad.test(datos)$p.value
  
  print("TEST DE ANDERSON-DARLING")
  imprimeResultadoTestNormalidad("Curva de faltas", valorP, nivelDeSignificacion)

  # Cramer-Von Mises
  valorP <- cvm.test(datos)$p.value

  print("TEST DE CRAMER-VON MISES")
  imprimeResultadoTestNormalidad("Curva de faltas", valorP, nivelDeSignificacion)

  # Lilliefors (Kolmogorov-Smirnov)
  valorP <- lillie.test(datos)$p.value

  print("TEST DE LILLIEFORS (KOLMOGOROV-SMIRNOV)")
  imprimeResultadoTestNormalidad("Curva de faltas", valorP, nivelDeSignificacion)

  # Pearson test
  valorP <- pearson.test(datos)$p.value

  print("TEST DE PEARSON")
  imprimeResultadoTestNormalidad("Curva de faltas", valorP, nivelDeSignificacion)
}

# Obtener las citas que han acabado en falta
faltas <- sqldf(paste('select * from citas where "No.show" = 1'))

# Obtener las citas en las que ha habido asistencia por parte del paciente
asistencias <- sqldf(paste('select * from citas where "No.show" = 0'))

# Establecer el nivel de significación para las pruebas de normalidad
nivelDeSignificacion <- 0.05

# Muestra un sumario descriptivo de las edades en faltas y asistencias
summary(faltas$Age)
summary(asistencias$Age)

# Prueba visual de distribución. Histograma de edades para faltas y asistencias.
hist(faltas$Age, breaks=seq(18,100))
hist(asistencias$Age, breaks=seq(18,100))

# Ejecuta los tests de normalidad sobre la distribución de edades de los pacientes en faltas y asistencias
ejecutaTestsDeNormalidad("Edades de faltas", faltas$Age, nivelDeSignificacion)
ejecutaTestsDeNormalidad("Edades de asistencias", asistencias$Age, nivelDeSignificacion)

# Transformación de edades usando el logaritmo

faltasEdadesLog10 <- log10(faltas$Age)
asistenciasEdadesLog10 <- log10(asistencias$Age)

# Prueba visual de distribución. Histograma de edades para faltas y asistencias.
hist(faltasEdadesLog10)
hist(asistenciasEdadesLog10)

# Ejecuta los tests de normalidad sobre la distribución transformada de log(edades) de los pacientes en faltas
# y asistencias
ejecutaTestsDeNormalidad("Edades (log10) de faltas", faltasEdadesLog10, nivelDeSignificacion)
ejecutaTestsDeNormalidad("Edades (log10) de asistencias", asistenciasEdadesLog10, nivelDeSignificacion)

# Transformación de edades usando la potencia de dos

faltasEdadesPotencia2 <- faltas$Age^2
asistenciasEdadesPotencia2 <- asistencias$Age^2

# Visualización histograma
hist(faltasEdadesPotencia2)
hist(asistenciasEdadesPotencia2)

# Ejecuta los tests de normalidad sobre la distribución transformada de edades^2 de los pacientes en faltas
# y asistencias
ejecutaTestsDeNormalidad("Edades (x^2) de faltas", faltasEdadesPotencia2, nivelDeSignificacion)
ejecutaTestsDeNormalidad("Edades (x^2) de asistencias", asistenciasEdadesPotencia2, nivelDeSignificacion)

# Test no paramétrico de Wilcox de dos muestras
# ¿Hay diferencia significativa entre las edades de los pacientes de las faltas y las edades de los pacientes
# de asistencias?
wilcox.test(Age~No.show, data=citas)
