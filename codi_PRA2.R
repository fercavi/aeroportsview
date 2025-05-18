# install.packages("countrycode")
# install.packages("geosphere")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("hms")

library(countrycode)
library(geosphere)
library(dplyr)
library(tidyr)
library(hms)

rm(list = ls())

#web extra: https://ourairports.com/data/
#web extra2:  https://gain.nd.edu/our-work/country-index/download-data/

#Canviar a conveniencia
ruta_directori <- "C:/Users/rogem/Desktop/UOC/Tipologia i cicle de vida de les dades/PRA2/"
ruta_directori <- "C:\\Users\\Vicent\\Desktop\\Visualització\\PRA2\\"

#Carreguem les funcions definides a un codi auxiliar
source(paste0(ruta_directori,"Funcions_auxiliars_PRA2.R"), encoding = "UTF-8")

#Definim rutes dels datasets
ruta_arribades <- paste0(ruta_directori, "dataset/arribades.csv")
ruta_eixides <- paste0(ruta_directori, "dataset/eixides.csv")
ruta_aeroports <- paste0(ruta_directori, "dataset/aeroports.csv")
ruta_aeroports_ourairports <- paste0(ruta_directori, "dataset_complementari/airports_ourairports.csv")
ruta_exposure_ndgain <- paste0(ruta_directori, "dataset_complementari/exposure_NDGAIN.csv")

#Carreguem els datasets
dataset_arribades_orig <- read.csv(ruta_arribades, sep=",")
dataset_eixides_orig <- read.csv(ruta_eixides, sep=",")
dataset_aeroports_orig <- read.csv(ruta_aeroports, sep=",")
dataset_aeroports_ourairports_orig <- read.csv(ruta_aeroports_ourairports, sep=",")
dataset_exposure_ndgain_orig <- read.csv(ruta_exposure_ndgain, sep=",")

#Per començar ens centrarem només amb el dataset d'eixides (replicable amb el d'arribades)
#Ens quedem només amb aquelles columnes que ens interessen dels diferents datasets

dataset_eixides <- setNames(subset(dataset_eixides_orig, 
                                   select = c("Airport.Code", "Companyia.aèria", "Destinació", "Estat", "Sortida")),
                                    c("iata_code_origen", "companyia", "destinacio", "estat_vol", "hora_sortida"))

dataset_aeroports_ourairports <- setNames(subset(dataset_aeroports_ourairports_orig,
                                    select=c("iata_code", "name", "iso_country", "type", "latitude_deg", "longitude_deg", "elevation_ft")),
                                    c("iata_code", "airport_name", "iso2c", "airport_type", "latitude_deg", "longitude_deg", "elevation_ft"))

#Del dataset de exposures ens quedem només amb dades de l'ultim any disponible, que és 2022
dataset_exposure_ndgain <- setNames(subset(dataset_exposure_ndgain_orig, 
                                    select=c("ISO3","X2022")),
                                    c("ISO3", "country_climate_risk_exposure"))

#Netegem els tres dataframes per separat

##########################################
####### TRACTAMENT DATASET EIXIDES #######
##########################################

#Revisem possibles valors faltants
print("Observem que no hi ha valors faltants al dataset d'eixides:")
print(colSums(is.na(dataset_eixides)))

# Convertim la destinacio a codi iata
dataset_eixides$destinacio <- sub(".*\\(([^)]+)\\).*", "\\1", dataset_eixides$destinacio)

#Eliminem aquells registres on l'aeroport d'origen i desti sigui el mateix perque no ens distorsionin el model
dataset_eixides <- dataset_eixides[dataset_eixides$iata_code_origen!= dataset_eixides$destinacio,]

# Tractem les hores separant en original i estimada pels vols amb retard
dataset_eixides <- dataset_eixides %>%
  extract(hora_sortida, into = c("hora_original", "hora_estimada"), 
          regex = "^([0-9]{2}:[0-9]{2})([0-9]{2}:[0-9]{2})?$", remove = FALSE)
dataset_eixides$hora_estimada <- ifelse(dataset_eixides$hora_estimada=="", dataset_eixides$hora_origina, dataset_eixides$hora_estimada)
# Convertim les hores en format hms
dataset_eixides <- dataset_eixides %>%
  mutate(
    # Afegim els segons i convertim
    hora_original = as_hms(paste0(hora_original, ":00")),
    hora_estimada = ifelse(is.na(hora_estimada), NA, paste0(hora_estimada, ":00")),
    hora_estimada = as_hms(hora_estimada)
  )
dataset_eixides <- dataset_eixides %>%
  mutate(
    minuts_retard = as.numeric(hora_estimada) - as.numeric(hora_original),
    minuts_retard = ifelse(minuts_retard < 0, minuts_retard + 24*3600, minuts_retard),  # corregim si canvia de dia
    minuts_retard = minuts_retard / 60  # passem a minuts
  )
#Definim la variable objectiu
dataset_eixides$y_retard <- dataset_eixides$minuts_retard != 0

#Afegim la distancia entre l'aeroport origen i desti per veure la longitud del vol
# Seleccionem només les columnes necessàries del dataframe d'aeroports
aeroports_posicions <- subset(dataset_aeroports_ourairports, iata_code != "", 
                    select = c("iata_code", "latitude_deg", "longitude_deg"))
# Unim al dataframe d'eixides
dataset_eixides <- merge(dataset_eixides, aeroports_posicions, 
                        by.x = "iata_code_origen", by.y = "iata_code", 
                        all.x = TRUE)
names(dataset_eixides)[names(dataset_eixides) == "latitude_deg"] <- "lat_origen"
names(dataset_eixides)[names(dataset_eixides) == "longitude_deg"] <- "lon_origen"
dataset_eixides <- merge(dataset_eixides, aeroports_posicions, 
                        by.x = "destinacio", by.y = "iata_code", 
                        all.x = TRUE)
names(dataset_eixides)[names(dataset_eixides) == "latitude_deg"] <- "lat_desti"
names(dataset_eixides)[names(dataset_eixides) == "longitude_deg"] <- "lon_desti"
# Calculem la distancia en km
dataset_eixides$distancia_vol_km <- distHaversine(cbind(dataset_eixides$lon_origen, dataset_eixides$lat_origen), 
                                             cbind(dataset_eixides$lon_desti, dataset_eixides$lat_desti)) / 1000 # en km

# Reduïm el nombre d'aerolinies ja que n'hi ha moltes que tenen molts pocs vols (1, 2, ...)
#Agrupem les aerolinies que menys apareixen sota el nom "Altres" i ens quedem unicament amb el top 10
freqs_aerolinies <- dataset_eixides %>%
  count(companyia, sort = TRUE)
top_n <- 10
companyies_top <- freqs_aerolinies %>%
  slice_max(n, n = top_n) %>%
  pull(companyia)

dataset_eixides <- dataset_eixides %>%
  mutate(companyia_reduida = ifelse(companyia %in% companyies_top, companyia, "Altres"))
##########################################
###### TRACTAMENT DATASET AEROPORTS ######
##########################################

aeroports_scope <- unique(c(dataset_eixides$iata_code_origen, dataset_eixides$destinacio))
#Ens quedem únicament amb aquells aeroports que ens interessen 
dataset_aeroports_ourairports <- dataset_aeroports_ourairports[dataset_aeroports_ourairports$iata_code %in% aeroports_scope,]

#Afegim l'iso code ISO3 per a poder creuar amb el dataframe de exposures
dataset_aeroports_ourairports$ISO3 <- countrycode(dataset_aeroports_ourairports$iso2c,
                                                  origin = "iso2c", 
                                                  destination = "iso3c")
# Afegim el nom del país a partir del codi ISO2
dataset_aeroports_ourairports$pais <- countrycode(dataset_aeroports_ourairports$iso2c,
                                                  origin = "iso2c", 
                                                  destination = "country.name")

#Deixem com a Unknown els paisos que no identifiquem
dataset_aeroports_ourairports$ISO3[is.na(dataset_aeroports_ourairports$ISO3)] <- "Unknown"
dataset_aeroports_ourairports$ISO3[is.na(dataset_aeroports_ourairports$pais)] <- "Unknown"

#Creuem amb el dataframe de Climate Exposures
dataset_aeroports_ourairports <- merge(dataset_aeroports_ourairports, dataset_exposure_ndgain, all.x=TRUE)

#Revisem possibles valors faltants
print("Observem que les úniques columnes amb valors faltants del dataframe dataset_aeroports_ourairports són elevation_ft i country_climate_risk_exposure:")
print(colSums(is.na(dataset_aeroports_ourairports)))

print("Substituim els buits de la columna elevation_ft pel valor de la mitja d'aquesta columna...")
dataset_aeroports_ourairports$elevation_ft[is.na(dataset_aeroports_ourairports$elevation_ft)] <-
  mean(dataset_aeroports_ourairports$elevation_ft, na.rm = TRUE)

#Completem els buits de country_climate_risk_exposure amb el valor de l'aeroport més proper aprofitant les longituds i latituds
# Apliquem la funció al dataset
dataset_aeroports_ourairports <- trobar_aeroport_mes_proper(dataset_aeroports_ourairports, filtrar_na_exposure=TRUE)
# Vector amb valors de exposure per codi iata
exposure_lookup <- setNames(dataset_aeroports_ourairports$country_climate_risk_exposure, dataset_aeroports_ourairports$iata_code)
#Finalment substituim els valors
dataset_aeroports_ourairports <- dataset_aeroports_ourairports %>%
  mutate(country_climate_risk_exposure = ifelse(
    is.na(country_climate_risk_exposure),
    exposure_lookup[nearest_airport_iata],
    country_climate_risk_exposure
  ))

dataset_aeroports_ourairports_reduit <- subset(dataset_aeroports_ourairports, select=c("iata_code","airport_name","pais","airport_type",
                                                                                       "elevation_ft","country_climate_risk_exposure"))
# AJUNTEM ELS DATAFRAMES
dataset_complet <- merge(dataset_eixides, dataset_aeroports_ourairports_reduit, by.x="iata_code_origen", by.y="iata_code", all.x=TRUE)

#Creem un dataframe amb tant sols les variables que utilitzarem per al model
# COMPLETAR COM ES CREGUI (NO TINC CLAR SI INCLOURE AEROPORT ORIGE, DESTI O PAIS JA QUE SON VARIABLES CATEGORIQUES AMB MOLTS POSSIBLES VALORS)
dataset_model <- subset(dataset_complet, select=
                          c("hora_original","companyia_reduida", "airport_type", "distancia_vol_km", "elevation_ft", "country_climate_risk_exposure","y_retard"))

#Normalitzem les variables numeriques del model
var_num <- names(dataset_model)[sapply(dataset_model, is.numeric)]
dataset_model[var_num] <- scale(dataset_model[, var_num])

#Convertim en factor les variables categoriques
var_cat <- names(dataset_model)[sapply(dataset_model, is.character)]
for(var_cat_i in var_cat){
  dataset_model[,var_cat_i] <- factor(dataset_model[,var_cat_i])
}

#guardem els datasets
ruta_output <- paste0(ruta_directori, "dataset_output/")
write.csv(dataset_model, paste0(ruta_output, "dataset_model_netejat.csv"), row.names = FALSE)

# Si vols guardar altres datasets també:
write.csv(dataset_eixides, paste0(ruta_output, "eixides_netejat.csv"), row.names = FALSE)
write.csv(dataset_arribades_orig, paste0(ruta_output, "arribades_netejat.csv"), row.names = FALSE)
write.csv(dataset_aeroports_ourairports, paste0(ruta_output, "aeroports_netejat.csv"), row.names = FALSE)

#4 Anàlisi de les dades

#primer de tot anem a veure si vegem classes descomplensades (retard o no)
#library(ggplot2)
#library(dplyr)

# Dataset bàsic
df_resum <- dataset_model %>%
  group_by(y_retard) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

#Barplot simple
ggplot(df_resum, aes(x = y_retard, y = n, fill = y_retard)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Nombre de vols amb i sense retard", x = "Retard", y = "Nombre de vols") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme(legend.position = "none")




#4.1 Model supervisat
set.seed(123)
n <- nrow(dataset_model)
idx_train <- sample(seq_len(n), size = 0.7 * n)
train_data <- dataset_model[idx_train, ]
test_data <- dataset_model[-idx_train, ]


#preprocés auxiliar
train_data <- na.omit(train_data)
train_data$y_retard <- as.factor(train_data$y_retard)
test_data$y_retard <- as.factor(test_data$y_retard)
#fi del preprocés



#triem random_forest ja que treballa bé amb variables categòriques 
library(randomForest) #ens dirà que la funció combine (no gastada crea conflicte de nom i utilitzarà la de dplyr, podem ignorar-ho)


rf_model <- randomForest(y_retard ~ ., data = train_data, ntree = 100, importance = TRUE)
print(rf_model)

library(caret)

predictions <- predict(rf_model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$y_retard)
#matriu de confussió
print(conf_matrix)

#visualitzar importància de les variables
varImpPlot(rf_model)


#4.1 model no supervisat

unsupervised_data <- dataset_model %>% select(-y_retard)
unsupervised_data$hora_original <- as.numeric(unsupervised_data$hora_original) / 60  # de segons a minuts
library(fastDummies)
unsupervised_data <- dummy_cols(unsupervised_data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
unsupervised_data <- na.omit(unsupervised_data)
set.seed(123)
kmeans_model <- kmeans(unsupervised_data, centers = 3)
table(kmeans_model$cluster)

#anem a veure si podem extraure alguna característica:

unsupervised_data$cluster <- kmeans_model$cluster

# Mitjanes per variable i clúster
cluster_profiles <- aggregate(. ~ cluster, data = unsupervised_data, FUN = mean)
print(cluster_profiles)



#4.2 Ara anem a fer un contrast d'hipòtesi primer amb la variable de distància de vol:
dataset_model$y_retard <- as.factor(dataset_model$y_retard)

# Resum descriptiu per grup
library(dplyr)
dataset_model %>%
  group_by(y_retard) %>%
  summarise(mitjana = mean(distancia_vol_km),
            desviacio = sd(distancia_vol_km),
            n = n())

sample_false <- sample(dataset_model$distancia_vol_km[dataset_model$y_retard == FALSE], size = 5000)
sample_true  <- sample(dataset_model$distancia_vol_km[dataset_model$y_retard == TRUE],  size = 5000)

shapiro.test(sample_false)
shapiro.test(sample_true)

require(graphics)
# Exemples per cada grup
qqnorm(sample_false, main = "Q-Q Plot - Vols sense retard")
qqline(sample_false, col = "red")

qqnorm(sample_true, main = "Q-Q Plot - Vols amb retard")
qqline(sample_true, col = "red")

#apareix un p-value molt menut, no podem assumir normalitat en les variables amb Shapiro-Wilk
#provem amb Mann-Whitney
wilcox.test(distancia_vol_km ~ y_retard, data = dataset_model)
#açò voldrà dir que quan mé

library(ggplot2)

ggplot(dataset_model, aes(x = y_retard, y = distancia_vol_km, fill = y_retard)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(title = "Distància del vol segons retard",
       x = "Retard (TRUE/FALSE)", y = "Distància del vol (normalitzada)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme(legend.position = "none")

#anem a repetir per a hora_original
# Convertir hora_original a minuts des de mitjanit
dataset_model$hora_minuts <- as.numeric(dataset_model$hora_original) / 60

# Resum descriptiu per grup
dataset_model %>%
  group_by(y_retard) %>%
  summarise(mitjana = mean(hora_minuts, na.rm = TRUE),
            desviacio = sd(hora_minuts, na.rm = TRUE),
            n = n())

# Mostres per al Shapiro-Wilk
set.seed(123)
sample_false <- sample(dataset_model$hora_minuts[dataset_model$y_retard == FALSE], size = 5000)
sample_true  <- sample(dataset_model$hora_minuts[dataset_model$y_retard == TRUE],  size = 5000)

# Test de normalitat
shapiro.test(sample_false)
shapiro.test(sample_true)

# Com que la normalitat segurament no es compleix, usem Mann-Whitney
wilcox.test(hora_minuts ~ y_retard, data = dataset_model)





#visualització PCA:

library(ggplot2)


unsupervised_data$cluster <- as.factor(kmeans_model$cluster)


pca <- prcomp(unsupervised_data %>% select(-cluster), scale. = TRUE)

# Preparem els dos primers components per visualització
pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$cluster <- unsupervised_data$cluster

# Gràfic
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(title = "Visualització de clústers (K-means + PCA)",
       x = "Component principal 1",
       y = "Component principal 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")