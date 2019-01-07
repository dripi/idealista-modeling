library(data.table)
library(class)
library(readxl)
library(car)
library(randomForest)
library(arules)
library(knitr)
library(pROC)

#table.resume
table.resume <- function (x, y){
  t <- table(x, y)
  N <- t[,1]+t[,2]
  return(data.frame('y' = t[,2], 'N' = N, 'var' = t[,2]/N))
}

df <- fread("C:\\Users\\User\\Documents\\Master Ciencia de Datos\\Tipologia y Ciclo de Vida de los Datos\\Practica2\\data\\dataset.csv", encoding = 'UTF-8')
clase <- sapply(df, class)
kable(data.frame(variable=names(clase), class=(clase)))

#Creamos la variable respuesta del modelo 
df$target <- ifelse(df$off > 0, 1, 0)
df$target <- as.factor(df$target)
kable(table(df$target))


#Tratamiento de variables
#Variable garage
df$garage <- as.factor(df$garage)
table(df$garage)
kable(table.resume(df$garage, df$target))

#cargamos los barrios de bcn
barris <- read_xlsx("C:\\Users\\User\\Documents\\Master Ciencia de Datos\\Tipologia y Ciclo de Vida de los Datos\\Practica2\\data\\barrios barcelona.xlsx")
df$zone <- ''

for(i in barris$barris){
  df$zone[grepl(i, df$title)] <- i
}

tz <- table(df$zone)
tz[order(tz, decreasing = T)] #Las zonas menos representativas se unirán a barrios vecinos más representativos
#por criterio experto se observa que los barrios con menos de 200 inmuebles en venta se pueden unir a barrios 
#vecinos sin problema

#######################
df$zone[df$zone == 'Provençals del Poblenou'] <- 'El Poblenou'
df$zone[df$zone == 'Hostafrancs'] <- 'Sants'
df$zone[df$zone == 'La Maternitat i Sant Ramon'] <- 'Les Corts'
df$zone[df$zone == 'El Camp d\'En Grassot i Gràcia Nova'] <- 'Vila de Gràcia'
df$zone[df$zone == 'El Besòs'] <- 'El Poblenou'
df$zone[df$zone == 'El Baix Guinardó'] <- 'El Guinardó'
df$zone[df$zone == 'Les Tres Torres'] <- 'Sant Gervasi'
df$zone[df$zone == 'Porta'] <- 'Nou Barris'
df$zone[df$zone == 'El Parc i la Llacuna del Poblenou'] <- 'El Poblenou'
df$zone[df$zone == 'El Congrés i els Indians'] <- 'Navas'
df$zone[df$zone == 'La Sagrera'] <- 'Navas'
df$zone[df$zone == 'La Prosperitat'] <- 'Nou Barris'
df$zone[df$zone == 'La Salut'] <- 'El Carmel'
df$zone[df$zone == 'Vilapicina i la Torre Llobeta'] <- 'Nou Barris'
df$zone[df$zone == 'Sant Martí'] <- 'El Clot'
df$zone[df$zone == 'Les Roquetes'] <- 'Nou Barris'
df$zone[df$zone == 'Horta'] <- 'Nou Barris'
df$zone[df$zone == 'Vallvidrera'] <- 'Sarrià'
df$zone[df$zone == 'La Bordeta'] <- 'Sants'
df$zone[df$zone == 'La Verneda i la Pau'] <- 'El Clot'
df$zone[df$zone == 'La Teixonera'] <- 'El Carmel'
df$zone[df$zone == 'Montbau'] <- 'El Carmel'
df$zone[df$zone == 'La Trinitat Vella'] <- 'Nou Barris'
df$zone[df$zone == 'Vila Olímpica'] <- 'El Poblenou'
df$zone[df$zone == 'La Marina del Port'] <- 'El Poble Sec'
df$zone[df$zone == 'La Font d\'En Fargues'] <- 'El Guinardó'
df$zone[df$zone == 'Verdun'] <- 'Nou Barris'
df$zone[df$zone == 'La Font de la Guatlla'] <- 'El Poble Sec'
df$zone[df$zone == 'Can Baró'] <- 'El Guinardó'
df$zone[df$zone == 'El Coll'] <- 'Vallcarca'
df$zone[df$zone == 'El Bon Pastor'] <- 'Sant Andreu'
df$zone[df$zone == 'Torre Baró'] <- 'Nou Barris'
df$zone[df$zone == 'La Trinitat Nova'] <- 'Nou Barris'
df$zone[df$zone == 'El Turó de la Peira'] <- 'Nou Barris'
df$zone[df$zone == 'La Guineueta'] <- 'Nou Barris'
df$zone[df$zone == 'Zona Franca'] <- 'El Poble Sec'
df$zone[df$zone == 'Baró de Viver'] <- 'Nou Barris'
df$zone[df$zone == 'La Clota'] <- 'El Carmel'
df$zone[df$zone == 'La Marina del Prat Vermell'] <- 'El Poble Sec'
#######################

df$zone <- factor(df$zone)
kable(table.resume(df$zone, df$target))

#Variable floor
df$floor[df$floor == ''] <- 'Casa'

tf <- table(df$floor)
tf[order(tf, decreasing = T)] #Los factores no representativos se agrupan en un único factor
df$floor[df$floor %in% names(tf)[tf < 500]] <- 'Atico'
df$floor <- factor(df$floor)

#Hacer table resume con floor
kable(table.resume(df$floor, df$target))

#Comprobamos nulos
colSums(is.na(df)) # las variable rooms tienen nulos

#Comprobacion de outliers
boxplot(df$rooms, main = "Distribución variable rooms") 
boxplot(df$m2, main = "Distribución variable m2")
boxplot(df$price, main = "Distribución variable price") 

remove_outliers <- function(x, limit = 3) {
  mn <- mean(x, na.rm = T)
  out <- limit * sd(x, na.rm = T)
  x < (mn - out) | x > (mn + out)
}
df <- df[remove_outliers(df$m2, 3)==FALSE | remove_outliers(df$price, 3)==FALSE | remove_outliers(df$rooms, 3)==FALSE,]

#Variable rooms
df$rooms[is.na(df$rooms)] <- round(mean(df$rooms, na.rm = T))

#Desechamos las variables que nos vamos a utilizar
df$date <- NULL
df$link <- NULL
df$off <- NULL
df$title <- NULL

#Comprobamos la distribución de las variables numericas que formarán parte del modelo
shapiro.test(sample(df$price, 5000))
shapiro.test(sample(df$m2, 5000))
shapiro.test(sample(df$rooms, 5000)) # no normalizaremos pq no es necesario en random forest

leveneTest(y = df$price, group = df$target, center = "median")
leveneTest(y = df$m2, group = df$target, center = "median")
leveneTest(y = df$rooms, group = df$target, center = "median")

leveneTest(y = df$price, group = df$target, center = "mean")
leveneTest(y = df$m2, group = df$target, center = "mean")
leveneTest(y = df$rooms, group = df$target, center = "mean")

#Se ha comprobado que hay una determinancia significativa entre la homocedasticidad de cada clase de la variable
#respuesta, por lo que aplicando técnicas de mineria de datos se podría obtener un resultado satisfactorio

set.seed(0)
r <- nrow(df)
train_ind <- sample(r, 0.9 * r)
train <- df[train_ind,]
test <- df[-train_ind,]

n <- table(train$target)[[2]]

rf <- randomForest(target~garage+m2+price+rooms+zone+floor,
                   mtry = length(train)-1, 
                   importance=TRUE,
                   sampsize=c(n, n),
                   data = train)
rf
varImpPlot(rf)

pred <- predict(rf, test, type = "prob")[,2]
roc_val <-roc(test$target, pred) 
gini <- 2*auc(roc_val)-1
gini

plot(roc_val,col='blue')

kable(table.resume(discretize(pred, "interval", 10), test$target))
