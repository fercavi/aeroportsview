library(geosphere)

trobar_aeroport_mes_proper <- function(df, filtrar_na_exposure = FALSE) {
  # Aeroports candidats per ser "nearest" (sense NA en exposure si filtrar_na_exposure=TRUE)
  if (filtrar_na_exposure) {
    candidats <- df[!is.na(df$country_climate_risk_exposure), ]
  } else {
    candidats <- df
  }
  
  # Coordenades de tots els aeroports (rows originals)
  coords_tots <- df[, c("longitude_deg", "latitude_deg")]
  
  # Coordenades dels aeroports candidats
  coords_candidats <- candidats[, c("longitude_deg", "latitude_deg")]
  
  # Matriu distància de tots contra candidats
  dist_matrix <- distm(coords_tots, coords_candidats, fun = distHaversine)
  
  # Per a cada aeroport, busquem l'índex del més proper (que no sigui ell mateix si és candidat)
  
  nearest_index <- integer(nrow(df))
  
  for (i in seq_len(nrow(df))) {
    # Si l'aeroport és també candidat, posem distància a si mateix a Inf perquè no es trii
    if (filtrar_na_exposure && !is.na(df$country_climate_risk_exposure[i])) {
      # localitzar la posició de l'aeroport i dins candidats
      pos_candidat <- which(candidats$iata_code == df$iata_code[i])
      dist_matrix[i, pos_candidat] <- Inf
    }
    # Índex del més proper candidat
    nearest_index[i] <- which.min(dist_matrix[i, ])
  }
  
  # Afegim el codi iata de l'aeroport més proper (entre candidats)
  df$nearest_airport_iata <- candidats$iata_code[nearest_index]
  
  return(df)
}
