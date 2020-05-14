### Librerias a utilizar.

library(dplyr)
library(ggplot2)
library(caret)

### Introduccion.
En el trabajo que a continuacion se muestra se estara analizando la informacion que se recolecto con la encuesta aplicada a los
estudiantes de la Facultad de Ingenieria en Sistemas, esta con el fin de analizar la problematica de sobrepoblacion que se presenta 
en esta facultad y como los estudiantes se desenvuelven mostrando un buen o mal rendimientio academico, para los docentes y alumnos es
vital conocer que problemas conlleva esta situacion y de igual foema conocer que estrategias o tecnologias implementar para ayudar a aliviar
esta circunstancia.

### Pasos preliminares.

Descargaremos nuestro archivo .csv de la pagina de google forms para darle el siguiente tratamiento.


### Leemos nuestro archivo deonde tenemos nuestra encuesta.

Tesis <-read.csv("C:/Users/Junior/Desktop/tesis_cleaned.csv",header = T,sep = ",",encoding = "UTF-8",na.strings = c(""))

### El archivo nombre_columnas es donde tenemos nuestras preguntas asociadas a una variable.

nombre_columnas <- read.csv("nombre_columnas.csv", encoding="UTF8", sep=";")


my.names <- names(Tesis)

### Con las siguientes lineas de codigo aliminamos las preguntas que consideramos redundantes o que no aportan informacion de interes.

my.names <- my.names[!(my.names %in% c("X"))]
my.names <- my.names[!(my.names %in% c("X.Cuantas.clases.matriculaste.este.periodo."))]
my.names <- my.names[!(my.names %in% c("X.En.que.año..académico.te.encuentras."))]
my.names <- my.names[!(my.names %in% c("X.Consideras.que.la.UNAH.debe.proporcionar.mas.recursos.materiales.a.disposición.de.la.facultad.de.ingeniería.en.sistemas."))]
my.names <- my.names[!(my.names %in% c("X.Considera.que.los.recursos.económicos.que.invierte.la.UNAH.en.la.facultad.de.ingeniería.en.sistemas.deben.ser.mayores."))]
my.names <- my.names[!(my.names %in% c("X.Consideras.que.la.UNAH.debe.contratar.mas.docentes.para.la.facultad.de.ingeniería.en.sistemas."))]

my.names

### actualizamos Tesis para que tenga solamente las columnas con las que debe de trabajar 
Tesis <- Tesis[,my.names]

### Vector que servira para remplazar los names del dataset de Tesis
nombre_columnas$alias <- as.character(nombre_columnas$alias)

### Cambiamos nombres en el DF original
names(Tesis) <- nombre_columnas$alias
names(Tesis)
head(Tesis)

### Creamos un nuevo documento .cvs con los cambios realizados.
write.csv(Tesis,"Tesis_cleaned", row.names = F)
 

### Leemos nuestro nuevo documento.

Tesis <- read.csv("C:/Users/Junior/Desktop/Tesis_cleaned.csv", encoding="UTF-8")

### Nivel descriptivo, importante importar la libreria dplyr.

### Realizaremos una transformacion de la variable Mejorar_indice para tener una mejor lectura de datos

df_mejorar <- as.data.frame(prop.table(table(Tesis$Mejorar_..ndice))) %>% arrange(-Freq)
df_mejorar[df_mejorar$Var1 %in% c("Mejor Planificaci?n", "Flexibilidad de horarios", "Mejor Planificaci?n;Flexibilidad de horarios", "Mejor planificaci?n"), "Transformacion"] <- "Organizacion"
df_mejorar[df_mejorar$Var1 %in% c("Otros", "Flexibilidad de horarios;Otros", "Mejor Planificaci?n;Otros", "Mejor Planificaci?n;Flexibilidad de horarios;Otros"), "Transformacion"] <- "otras opciones"

### Al dataframe df_mejorar le asignamos nuestras nuevas columnas con las trasnformaciones realizadas.

df_mejorar <- df_mejorar %>% select(Var1, Transformacion)

### Unimos nuestro nuevo daframe df_mejorar a nuestra encuesta.

Tesis <- left_join(Tesis, df_mejorar, by=c("Mejorar_..ndice"="Var1"))

### Borramos de nuestra encuesta la variable sobre la cual realizamos la transformacion esto ya que depuramos la misma con todo el proceso realizado.

Tesis <- Tesis[,!(names(Tesis) %in% c("Mejorar_..ndice"))]

### Si corremos esta linea de comandos podremos ver que organizacion posee una mayoria de 107 registros que la aopoyan y otras opciones tiene un total de 23 registros que la apoyan.

table(Tesis$Transformacion)

### Nivel descriptivo, resumen estadistico

### Para obtener nuestro resumen estadistico vamos a procesar las proporciones de cada una de nuetras variables teniendo en cuenta las transformaciones que ya hemos realizado, 
### en este caso se realizo una por una de la siguiente manera.

as.data.frame(prop.table(table(Tesis$Edad))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Genero))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Procedencia))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Traslado_.UNAH))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Computadora_.permanente))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Conexi.f3.n_.permanente))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Calidad_.conexi.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estudio_.secundaria))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Cantidad_.carreras))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Rango_.acad.e9.mico))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Transformacion))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Exelencia_academica))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Reprobaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Rango_.promedio_.clases))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Flexibilidad_.horarios))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Horas_.libres))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estimaci.f3.n_.horas_.libres))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Lista_.espera))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Mas_.cupos))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Preferencia_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Horas_.diarias_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Disciplina))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Autodidacta))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Demora))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Entorno))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Motivo_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Seguimiento))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Deserci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Sobrepoblaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Aumento._de._la.poblaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Sobrecarga_.laboral))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Planificaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estrategias_.educativas))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Recursos))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Plataforma))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Frecuencia_.de_.uso))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Aumento_.de_.uso))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Uso_.lms))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$lms_.utiliza))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estimaci.f3.n_.lms))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Cambio._LMS))) %>% arrange(-Freq)


###Analisis de diagramas y valores atipivos encontrados en el nivel descriptivo.

### Variable edad: Como se podra analizar con las siguientes lineas de comandos encontramos valores atipocos e la variable Edad
### se generaron los diagramas correspondientes para concluir acerca de esta situacion.  

df_edad <- as.data.frame(prop.table(table(Tesis$Edad))) %>% arrange(-Freq)
boxplot(df_edad$Freq)

### Variable Mejorar_indice: Teniendo en cuenta la transformacion realizada anterior mente para esta variable en las siguientes
### lineas de codigo veremos los diagramas que nos ayudaron a analizar y tratar esta variable.

df_mejorar <- as.data.frame(prop.table(table(Tesis$Transformacion))) %>% arrange(-Freq)
boxplot(df_mejorar$Freq)
hist(df_mejorar$Freq)
qqnorm(df_mejorar$Freq)

### Variable Rango_.promedio_.clases: esta variable se analizo ya que se penso podria tener valores atipicos, sin embargo, 
### los diagramas generados a continuacion nos ayudaron a confirmar la validez de la data que almacena esta variable.

df_Rango <- as.data.frame(prop.table(table(Tesis$Rango_.promedio_.clases))) %>% arrange(-Freq)
boxplot(df_Rango$Freq)
hist(df_Rango$Freq)
qqnorm(df_Rango$Freq)

### Variable Horas_diarias_estudio: Esta variable se estudio ya que al ser una variable abierta al encuestado para ser llenada
### se penso que posible mente pdroa mostrar errores en los valores que se llenaron en ella, pero los diagramas muestran todo lo
### contrario ya que verifican que la informacion es correcta.

df_Horas <- as.data.frame(prop.table(table(Tesis$Horas_.diarias_.estudio))) %>% arrange(-Freq)
boxplot(df_Horas$Freq)
hist(df_Horas$Freq)
qqnorm(df_Horas$Freq)

### Variable Disciplina: Debido a que en esta variable se preguntaba con que frecuiencia se cumplian las obligaciones del estudiante
### y se presento un valor dificl de creer ya que un procentaje de los encuestados muy bajo no cumple con sus obligaciones,
### sin embargo los diagramas nos ayudaron a revisar esta informacion y concluir que la misma es correcta.

df_Disciplina <- as.data.frame(prop.table(table(Tesis$Disciplina))) %>% arrange(-Freq)
boxplot(df_Disciplina$Freq)
hist(df_Disciplina$Freq)
qqnorm(df_Disciplina$Freq)

### Variable Planificacion: Debido a las distintas opciones que presenta esta variable en forma de rango se nos hizo interesante
### de estudiar la misma ya que podrian existir valores atipicos, pero los diagramas confirman que los registros son correctos.

df_Planificaion <- as.data.frame(prop.table(table(Tesis$Planificaci.n))) %>% arrange(-Freq)
boxplot(df_Planificaion$Freq)
hist(df_Planificaion$Freq)
qqnorm(df_Planificaion$Freq)

### Variable Estrategias educativas: Debido a las distintas opciones que presenta esta variable en forma de rango se nos hizo interesante
### de estudiar la misma ya que podrian existir valores atipicos, pero los diagramas confirman que los registros son correctos.

df_Estrategias <- as.data.frame(prop.table(table(Tesis$Estrategias_.educativas))) %>% arrange(-Freq)
boxplot(df_Estrategias$Freq)
hist(df_Estrategias$Freq)
qqnorm(df_Estrategias$Freq)

### Variable LMS_utilizado: Con respecto a esta variable podemos decir que en las proporciones o distribucion
### mostraba algunos datos sospechos de que fuesen incorrectos, en efecto al analizar los diagramas nos dimos cuenta de
### que la variable posee registros que son atipicos, para solucionar este problema se decidio reformular la pregunta
### en la seccion de limpieza de daros veremos el proceso que se realizo, con los diagramas que se muestran a continuacion se apreciaran
### los valores atipicos en cuestion.

df_Uso <- as.data.frame(prop.table(table(Tesis$lms_.utiliza))) %>% arrange(-Freq)
boxplot(df_Uso$Freq)
hist(df_Uso$Freq)
qqnorm(df_Uso$Freq)

### Variable Cambio_LMS: esta variable nos parecio interesante de corroborar los registros que posee ya que es una de las 
### que sustenta o aporta informacion a nuestra solucion tecnologica, los diagramas muestran que la informacion que almacena
### es correcta.

df_Cambiolms <- as.data.frame(prop.table(table(Tesis$Cambio._LMS))) %>% arrange(-Freq)
boxplot(df_Cambiolms$Freq)
hist(df_Cambiolms$Freq)
qqnorm(df_Cambiolms$Freq)

### Nivel correlacional.

#### Con las lineas de codigo siguientes veremos el comportamiento de las variables categoricas de interes.

summary(str(Tesis))
table(Tesis$Sobrepoblaci.n)
table(Tesis$Aumento._de._la.poblaci.n)
table(Tesis$Computadora_.permanente)
table(Tesis$Conexi.n_.permanente)
table(summary(Tesis))
table(Tesis$Procedencia)
str(Tesis$Procedencia)
names(Tesis)

### Ahora vamos a correlacionar nuestras variables segun nuestras preguntas de investigacion.

### Las personas que poseen computadora poseen internet permanete?
### Aqui correlacionamos las variables: Conexion_.permanente vrs Computadora_.permanente

prop.table(table(Tesis$Computadora_.permanente,Tesis$Conexi.n_.permanente),2)

### Utilizamos la funcion ggplot para generar graficos que nos ayuden a interpretar mejor nuestra correlacion

ggplot(Tesis)+
  aes(x= Computadora_.permanente, fill= Conexi.n_.permanente)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(Tesis)+
  aes(x= Computadora_.permanente, fill= Conexi.n_.permanente)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))

### Con la funcion Chi cuadrada descartaremos o aprobaremos nuestras hipotesis

#H_O Las categorias computadora permanente y conexion permanente son independientes
#H_A Las categorias computadora permanente y conexion permanente son dependientes

chisq.test(table(Tesis$Computadora_.permanente,Tesis$Conexi.n_.permanente))


### Regla: Aceptamos nuestra hipotesis nula cuando el p_value de nuestra prueba es menor a 0.05

### Conclusion: segun nuestro p_value rechasamos nuestra hipotesis nula, por lo tanto nuestas variables son dependientes, es decir 
### la conexion permanete a internet depende de que los estudiantes posean una computadora de forma permanente en sus hogares.

### Recomendacion: Es de vital importacia que los estudiantes de alguna manera cunten con el recurso de una computadora y el 
### recurso de acceso a internet ya sea en sus hogares, lugares de trabajo etc. Esto ya que muy probablemente necesitaran estos
### recursos para tener un mejor aprovechamiento de sus clases, campus virtual, sesiones de estudios etc.



### El aumento de poblacion tiene un efecto negativo en el rendimiento academico?

### Correlacionaremos la vartiable Rango_.academico vrs la variable Aumento_de_la_poblacion, con la siguiente linea de codigos
### realizamos la correlacion mencionada en funcion de columnas.

prop.table(table(Tesis$Rango_.acad.mico,Tesis$Aumento._de._la.poblaci.n),2)

#Funcion ggplot para generar grafico pata interpretar mejor la correlacion

ggplot(Tesis)+
  aes(x= Rango_.acad.mico, fill= Aumento._de._la.poblaci.n)+
  geom_bar(position = "Stack")+
  theme(axis.text.x = element_text(angle = 45))

### Con la funcion Chi cuadrada descartaremos o aprobaremos nuestras hipotesis

### H_O las categorias rango academico y aumento en la poblacion son independientes
### H_A las categorias son dependientes

chisq.test(table(Tesis$Rango_.acad.mico,Tesis$Aumento._de._la.poblaci.n))

### Aceptamos nuestra hipotesis nula cuando el p_value de nuestra prueba es menor a 0.05

### Conclusion: Segun nuestro p_value rechasamos nuestra hipotesis nula, por lo tanto nuestas variables son dependientes, es decir
### las categorias rango academico y aumento de la poblacion estan relacionadas y un aumento de la poblacion de estudiantes refleja un
### efecto negativo en el rendimiento academico, aunque segun los datos obtenidos esto depende del rango en que se encuentre tu
### indice academico.

### Recomendacion: La sobrepoblacion de la carrera de ingenieria en sistemas esta comprobada, si embargo se recomienda al estdiante
### que aun en estas condiciones se esmere y esfuerce para que su rendimiento academico no se vea afectado por esta situacion.



### Las condiciones de sobrepoblacion generan problemas de retrazo en la carrera?

### Correlacionaremos las variables Demora y sobrepoblacion en funcion de columnas para comprobar o descartar la correlacion.

prop.table(table(survey$Demora,survey$Sobrepoblaci.n),2)


### Funcion ggplot para generar grafico para interpretar mejor la correlacion

ggplot(Tesis)+
  aes(x= Demora, fill= Sobrepoblaci.n)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))


### Prueba de chi2 para comprobar la dependecia o independencia de variables

### H_O Las categorias  demora y sobrepoblacion son independientes
### H_A las categorias demora y sobrepoblacion son dependientes

chisq.test(table(Tesis$Demora,Tesis$Sobrepoblaci.n))

### Regla: Aceptamos nuestra hipotesis nula cuando el p_value de nuestra prueba es menor a 0.05

### Conclusion: segun nuestro p_value rechasamos nuestra hipotesis nula, por lo tanto nuestas variables son dependientes,
### es decir que los retrasos de los estudiantes en la carrera de ingenieria en sistemas se ven afectados por el sobrepoblamiento de la misma.

### Recomendacion: Con relacion a toda la encuesta aplicada ya definimos los problemas que trae con sigo la sobrepoblacion de estudiantes
### como la falta de secciones, reprobaciones u horarios complicados, es necesario que se atiendan las complicaciones causadas
### por la sobrepoblacion para que los estudiantes no se demoren en culminar su carrera.

### Limpieza de datos

##recorer la tabla para buscar los valores NA


na.Tesis <- c()

for (myname in names(Tesis)){
  
  print(myname)
  
  en<-as.data.frame(prop.table(table(is.na(Tesis[,myname]))))
  
  operacion <- en %>% filter(Var1==TRUE) %>% select(Freq)
  
  length(operacion$Freq)
  
  df_temp <- data.frame(
    
    column.name=c(myname),
    na.percentage=ifelse(length(operacion$Freq)==0,0,operacion$Freq[1])
  )
  na.Tesis <- rbind(na.Tesis,df_temp)
}


na.Tesis %>% arrange(-na.percentage) %>% filter(na.percentage > 0) 

Tesis$Estimaci.n_.horas_.libres
Tesis$Calidad_.conexi.n
Tesis$lms_.utiliza
Tesis$Estimaci.n_.lms


Tesis[is.na(Tesis$Estimaci.n_.horas_.libres),"Estimaci.n_.horas_.libres"] <- "Ninguna"
Tesis[is.na(Tesis$Estimaci.n_.lms), "Estimaci.n_.lms"] <- "No utilizó LMS"
Tesis[is.na(Tesis$Calidad_.conexi.n), "Calidad_.conexi.n"] <-"No"
Tesis[is.na(Tesis$lms_.utiliza), "lms_.utiliza"] <-"Moodle"


Tesis$Estimaci.n_.horas_.libres
Tesis$Calidad_.conexi.n
Tesis$lms_.utiliza
Tesis$Estimaci.n_.lms


