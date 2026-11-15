# Ejercicio 1

alerta.movil <- function(df, ventana, k.alerta) {
  df$fecha <- as.Date(df$fecha)
  d <- aggregate(id_tx ~ fecha, data = df, FUN = length)
  names(d) <- c("fecha", "n.tx")
  d <- d[order(d$fecha), ]
  n <- nrow(d)
  d$media <- NA_real_
  d$desv  <- NA_real_
  for (t in seq(ventana + 1, n)) {
    x <- d$n.tx[(t - ventana):(t - 1)]  
    d$media[t] <- mean(x)
    d$desv[t]  <- sd(x)
  }
  d$alerta <- !is.na(d$media) & d$n.tx > d$media + k.alerta * d$desv
  d
}

# Ejercicio 2

# Recibe montos positivos y devuelve la desviación absoluta media
# entre los primeros dígitos observados y la distribución de Benford.
benford.mad <- function(x) {
  x <- x[!is.na(x) & is.finite(x) & x > 0]
  
  if (length(x) == 0) return(NA_real_)
  
  digito <- floor(x / 10^floor(log10(x)))
  observada <- tabulate(as.integer(digito), nbins = 9) / length(digito)
  esperada <- log10(1 + 1 / (1:9))
  
  mean(abs(observada - esperada))
}


# Recibe las transacciones, el nombre de un canal y el catálogo de cantones.
# Devuelve una fila por cantón, ordenada desde la MAD más alta.
benford.canton <- function(df, canal, cantones) {
  sub <- df[df$canal == canal & df$monto_crc > 0, ]
  
  codigos <- unique(sub$cod_canton)
  
  resultado <- do.call(rbind, lapply(codigos, function(cod) {
    montos <- sub$monto_crc[sub$cod_canton == cod]
    mad <- benford.mad(montos)
    
    clase <- if (mad <= 0.006) {
      "conformidad cercana"
    } else if (mad <= 0.012) {
      "conformidad aceptable"
    } else if (mad <= 0.015) {
      "conformidad marginal"
    } else {
      "no conformidad"
    }
    
    data.frame(
      cod_canton = cod,
      n = length(montos),
      mad = mad,
      clase = clase
    )
  }))
  
  resultado$canton <- cantones$canton[
    match(resultado$cod_canton, cantones$cod_canton)
  ]
  
  resultado <- resultado[, c("cod_canton", "canton", "n", "mad", "clase")]
  resultado <- resultado[order(resultado$mad, decreasing = TRUE), ]
  rownames(resultado) <- NULL
  resultado
}