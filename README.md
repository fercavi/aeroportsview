# aeroportsview
Anàlisi de dades (PRA2) de l'assignatura de tipologia i cicle de vida de les dades de les dades obteses en la pràctica anterior de l'assignatura
## Autors

- **Roger Mas Bosch**  
- **Vicent Fernàndez i Capilla**

## Estructura del projecte

El projecte es divideix en les carpetes i arxius següents:

###  dataset/
Dades extretes de la pràctica anterior. Conté els arxius principals en brut, utilitzats com a font per a l’anàlisi.

###  dataset_complementari/
Fitxers complementaris que poden aportar informació addicional (com dades d’aeroports), en format `.csv`. No tots han estat utilitzats directament.

###  dataset_output/
Conté els resultats del **procés de neteja i transformació** de dades, preparats per a l’anàlisi final.

###  Funcions_auxiliars_PRA2.R
Script amb funcions de suport definides per modularitzar i reutilitzar codi durant el procés de tractament de dades.

###  codi_PRA2.R
Codi principal del projecte: carrega dades, les neteja, genera variables, aplica models i visualitza resultats.
