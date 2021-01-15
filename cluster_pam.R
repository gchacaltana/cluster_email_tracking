######################################################
### -- Clusterización de una base Email Tracking -- ## 
######################################################
### Autor: Gonzalo Chacaltana
### Cluster: PAM
#####################################################

# PREPARAR ESPACIO DE TRABAJO
# ----------------------------
# Limpiar espacio de trabajo
rm(list = ls())

# Setear directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# CARGA DE PAQUETES
# ---------------------------

# Carga de paquetes
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c(
    "ggplot","data.table","readxl", "DataExplorer","fpc","NbClust",
    "cluster","factoextra","tidyr","dplyr","ggcorrplot")
ipak(packages)


# CONJUNTO DE DATOS
# ---------------------------

dataset <- read_excel("data/data_email_tracking.xlsx")

# TRATAMIENTO DE LA DATA
# --------------------------------------------

str(dataset)
summary(dataset)

# Convertir variables a factor
dataset$gender <- as.factor(dataset$gender)
dataset$type_message <- as.factor(dataset$type_message)
dataset$schedule <- as.factor(dataset$schedule)

# Convertir variables a formato numérico
dataset$minutes_after <- as.numeric(dataset$minutes_after)
dataset$hour_open <- as.numeric(dataset$hour_open)
dataset$minute_open <- as.numeric(dataset$minute_open)

str(dataset)

# ANÁLISIS EXPLORATORIO
# ---------------------

# Distribución de la edad
ggplot(dataset, aes(x=age)) + 
    geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
    geom_density(alpha=.2, fill="red") +
    labs(title="Distribución de la edad",x="Edad", y="Densidad")

plot(dataset$gender)

boxplot(dataset$minutes_after,main="Minutos en abrir correo", ylab="Frecuencia")

# Distribución de la hora que leen los correos
ggplot(dataset, aes(x=hour_open)) + 
    geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
    geom_density(alpha=.2, fill="red") +
    labs(title="Distribución de la Hora que leen los correos",x="Horas del día", y="Densidad")


# Distribución minutes after
ggplot(dataset, aes(x=minutes_after)) + 
    geom_histogram(bins=30, colour="darkblue", fill="lightblue")+
    labs(title="Minutos que transcurren en abrir mensaje",x="Horas del día", y="Densidad")


# Validar si existen valores nulos en el conjunto de datos
plot_missing(dataset)

# Transformación de las variables numéricas.
# -----------------------------------------

variables_numericas <- dataset %>% dplyr::select(age,minutes_after,hour_open,minute_open)
variables_categoricas <- dataset %>% dplyr::select(gender,date_send,date_open,schedule)

names(variables_categoricas)
variables_numericas <- lapply(variables_numericas,function(x) (x - min(x)) / (max(x) - min(x) ))
variables_numericas <- as.data.frame(variables_numericas)

data_modelo <- cbind(variables_numericas,variables_categoricas)

df <- variables_numericas


# Correlación de variables
# ---------------------------

correlacion <- cor(df, method = 'spearman')

ggcorrplot(correlacion, lab = TRUE, type = 'lower')

# MATRIZ DE DISTANCIA
# ---------------------
# Calculamos la matriz de distancia mediante el método de Kendall
# Otros métodos: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
m.distancia <- get_dist(df, method = "kendall")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))


# Estimación de número de clústers
# -----------------------------------

# Metodos: Elbow, silhouette, gap_stat
fviz_nbclust(df, pam, method = "wss")
fviz_nbclust(df, pam, method = "silhouette")
fviz_nbclust(df, pam, method = "gap_stat")

# Generamos los clústers con PAM
pam6 <- pam(df, 6)
print(pam6)
pam4 <- pam(df, 4)
print(pam4)
pam2 <- pam(df, 2)
print(pam2)

# Visualizamos los clústers
fviz_cluster(pam2, data = df, ellipse.type = "norm")
fviz_cluster(pam4, data = df)
fviz_cluster(pam6, data = df)

# Visualiazamos los cluster como dendogramas
vz_dd <- hcut(df, k = 4, stand = TRUE, method = "median")
fviz_dend(vz_dd, rect = TRUE, cex = 0.5, k_colors = "simpsons")

# Mostrar resumen de cluster
dataset %>%
    mutate(Cluster = pam6$clustering) %>%
    group_by(Cluster) %>%
    summarise_all("mean")

df$clus<-as.factor(pam6$clustering)
df

df$clus<-factor(df$clus)

# basado en minutos after por edad.
data_long <- gather(df, caracteristica, valor, age:minutes_after, factor_key=TRUE)
data_long

ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
    stat_summary(fun = mean, geom="pointrange", size = 1, aes(shape = clus))+
    stat_summary(geom="line")