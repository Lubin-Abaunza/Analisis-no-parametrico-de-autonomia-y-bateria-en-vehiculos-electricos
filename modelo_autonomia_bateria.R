# ------------------------------------------------------------
# Título: Modelo de regresión no paramétrica
# Tema: Relación entre autonomía y capacidad de batería en vehículos eléctricos
# Autor: Lubin Fernando Abaunza Melo
# Curso: Teoría de Estadística No Paramétrica
# Universidad Nacional de Colombia
# Fecha: Julio 2025
# ------------------------------------------------------------

# --------------------------------------------
# Cargar librerías necesarias
# --------------------------------------------
library(readxl)
library(dplyr)
library(sm)
library(bde)
library(extraDistr)

# --------------------------------------------
# Cargar la base de datos
# --------------------------------------------

# Leer datos
ev <- read_excel("electric_vehicles_spec_2025.xlsx")


## ================================================== ##
##      Análisis No Paramétrico de autonomia_km       ##
## ================================================== ##

# --------------------------------------------
# 1. Estadísticas descriptivas básicas 
# --------------------------------------------
datos <- ev$range_km
datos <- datos[!is.na(datos)]

Min <- min(datos)
Max <- max(datos) 
Med <- median(datos)
RIC <- IQR(datos)

cat("Mínimo:", Min, "\nMáximo:", Max, "\nMediana:", Med, "\nRIC:", RIC, "\n")

# --------------------------------------------
# 2. Estimación Kernel y IC
# --------------------------------------------

# Bandwidth de referencia a la normal
n <- length(datos)
s <- min(sd(datos), IQR(datos)/1.35)
ho <- 1.059 * s * n^(-1/5)

# Estimación kernel
Densidad <- density(datos, kernel = "gaussian", bw = ho)

# Graficar densidad
plot(Densidad, col = "skyblue", lwd = 2, 
     main = "Estimación Kernel de la densidad",       
     xlab = "Autonomía (km)", ylab = "Densidad")

# IC del 95%
x_vals <- Densidad$x
f_hat <- Densidad$y
R_K <- 1 / (2 * sqrt(pi))
z <- qnorm(0.975)
sqrt_f_hat <- sqrt(f_hat)
margen <- z * sqrt(R_K / (4 * n * ho))
IC_inf <- (sqrt_f_hat - margen)^2
IC_sup <- (sqrt_f_hat + margen)^2

plot(x_vals, f_hat, type = "l", lwd = 2.5, col = "darkgreen", 
     ylim = range(c(f_hat, IC_inf, IC_sup)),
     main = "Estimación de Densidad Kernel con IC del 95%", 
     xlab = "Autonomía (Km)", ylab = "Densidad")

lines(x_vals, IC_inf, col = "gray50", lty = 2, lwd = 1.8)
lines(x_vals, IC_sup, col = "gray50", lty = 2, lwd = 1.8)
legend("topright", legend = c("Densidad", "IC 95%"), col = c("darkgreen", "gray50"), lty = c(1, 2))

# --------------------------------------------
# 3. Curva esperada y varianza bootstrap
# --------------------------------------------
x_grid <- seq(Min, Max, length.out = 512)
ts <- 1000
estimaciones <- matrix(0, nrow = length(x_grid), ncol = ts)

for (i in 1:ts) {
  muestra <- sample(datos, replace = TRUE)
  dens <- density(muestra, from = Min, to = Max, n = 512, bw = ho)
  estimaciones[, i] <- dens$y
}

media <- apply(estimaciones, 1, mean)
varianza <- apply(estimaciones, 1, var)

par(mfrow = c(1, 2))
plot(x_grid, media, type = "l", col = "darkgreen", lwd = 2,
     main = "Curva Esperada E[f_k(x)]", xlab = "Autonomía (km)", ylab = "Densidad")

plot(x_grid, varianza, type = "l", col = "gold", lwd = 2,
     main = "Curva de Varianza V[f_k(x)]", xlab = "Autonomía (km)", ylab = "Varianza")
par(mfrow = c(1, 1))

# --------------------------------------------
# 4. Pruebas de normalidad no paramétrica
# --------------------------------------------

datos <- datos

n <- length(datos)

Tobs <- nise(datos)
Tsim <- replicate(ts, nise(rnorm(n)))
pval <- sum(Tsim > Tobs) / ts
cat("p-valor normalidad :", pval, "\n")

hist(Tsim, freq=FALSE, main="Normalidad", 
     xlab="T simulado", ylab="Densidad")
abline(v=Tobs, col="red", lwd=2, lty=2)
abline(v=quantile(Tsim, 0.95), col="blue", lwd=2, lty=2)

sm.density(datos, display="se", model="normal", col=2,main = "Bandas de referencia a la normal", 
           ylab="Densidad", xlab="Autonomía (km)", lwd=2)


## =========================================================== ##
##      Análisis No Paramétrico de capacidad_bateria_kwh       ##
## =========================================================== ##

# --------------------------------------------
# 1. Estadísticas descriptivas básicas 
# --------------------------------------------
datos2 <- ev$battery_capacity_kWh
datos2 <- datos2[!is.na(datos2)]

Min2 <- min(datos2)
Max2 <- max(datos2) 
Med2 <- median(datos2)
RIC2 <- IQR(datos2)

cat("Mínimo:", Min2, "\nMáximo:", Max2, "\nMediana:", Med2, "\nRIC:", RIC2, "\n")

# --------------------------------------------
# 2. Estimación Kernel y IC
# --------------------------------------------

# Bandwidth de referencia a la normal
n2 <- length(datos2)
s2 <- min(sd(datos2), IQR(datos2)/1.35)
ho2 <- 1.059 * s2 * n2^(-1/5)

# Estimación kernel
Densidad2 <- density(datos2, kernel = "gaussian", bw = ho2)

# Graficar densidad
plot(Densidad2, col = "skyblue", lwd = 2, 
     main = "Estimación Kernel de la densidad",       
     xlab = "Capacidad de la batería (kwh)", ylab = "Densidad")

# IC del 95%
x_vals2 <- Densidad2$x
f_hat2 <- Densidad2$y
R_K2 <- 1 / (2 * sqrt(pi))
z <- qnorm(0.975)
sqrt_f_hat2 <- sqrt(f_hat2)
margen2 <- z * sqrt(R_K2 / (4 * n2 * ho2))
IC_inf2 <- (sqrt_f_hat2 - margen2)^2
IC_sup2 <- (sqrt_f_hat2 + margen2)^2

plot(x_vals2, f_hat2, type = "l", lwd = 2.5, col = "darkblue", 
     ylim = range(c(f_hat2, IC_inf2, IC_sup2)),
     main = "Estimación de Densidad Kernel con IC del 95%", 
     xlab = "Capacidad de la batería (kWh)", ylab = "Densidad")

lines(x_vals2, IC_inf2, col = "gray40", lty = 2, lwd = 1.8)
lines(x_vals2, IC_sup2, col = "gray40", lty = 2, lwd = 1.8)

legend("topright", legend = c("Densidad", "IC 95%"),
       col = c("darkblue", "gray40"), lty = c(1, 2), lwd = c(2.5, 1.8))

# --------------------------------------------
# 3. Curva esperada y varianza bootstrap
# --------------------------------------------
x_grid2 <- seq(Min2, Max2, length.out = 512)
ts <- 1000
estimaciones2 <- matrix(0, nrow = length(x_grid2), ncol = ts)

for (i in 1:ts) {
  muestra <- sample(datos2, replace = TRUE)
  dens <- density(muestra, from = Min2, to = Max2, n = 512, bw = ho2)
  estimaciones2[, i] <- dens$y
}

media2 <- apply(estimaciones2, 1, mean)
varianza2 <- apply(estimaciones2, 1, var)

par(mfrow = c(1, 2))
plot(x_grid2, media2, type = "l", col = "#1B4F72", lwd = 2,
     main = "Curva Esperada E[f_k(x)]", xlab = "Capacidad de la batería (kwh)", ylab = "Densidad")

plot(x_grid2, varianza2, type = "l", col = "#A93226", lwd = 2,
     main = "Curva de Varianza V[f_k(x)]", xlab = "Capacidad de la batería (kwh)", ylab = "Varianza")

par(mfrow = c(1, 1))

# --------------------------------------------
# 4. Pruebas de normalidad no paramétrica
# --------------------------------------------

datos2 <- datos2
n <- length(datos2)

# SUV
Tobs <- nise(datos2)
Tsim <- replicate(ts, nise(rnorm(n)))
pval <- sum(Tsim > Tobs) / ts
cat("p-valor normalidad :", pval, "\n")

hist(Tsim, freq=FALSE, main="Normalidad", 
     xlab="T simulado", ylab="Densidad")
abline(v=Tobs, col="red", lwd=2, lty=2)
abline(v=quantile(Tsim, 0.95), col="blue", lwd=2, lty=2)

sm.density(datos2, display="se", model="normal", col=2, 
           ylab="Densidad", xlab="Capacidad de la batería (kwh)", lwd=2)



## ============================================================= ##
##  Regrecion No Paramétrico de range_km - battery_capacity_kwh  ##
## ============================================================= ##

#--------------------------------------------------------
# 1. Cargar librerías y datos
#--------------------------------------------------------
library(sm)
library(FNN)        
library(RColorBrewer)
library(KernSmooth)


# Variable respuesta y regresora
x <- ev$battery_capacity_kWh
y <- ev$range_km

#--------------------------------------------------------
# 2. División entrenamiento / prueba
#--------------------------------------------------------
set.seed(123)
n <- length(x)
indices <- sample(1:n, size = 0.8 * n)
x_train <- x[indices]
y_train <- y[indices]
x_test <- x[-indices]
y_test <- y[-indices]

#--------------------------------------------------------
# 3. REGRESIÓN KERNEL - Visualización + Inferencia
#--------------------------------------------------------

# a. Curva ajustada
plot(x_train, y_train, pch = 16, col = "gray", xlab = "Battery Capacity (kWh)", ylab = "Range (km)", main = "Regresión Kernel")
modelo_kernel <- sm.regression(x = x_train, y = y_train, model = "none", col = "blue", lwd = 2, add = TRUE)

# b. Intervalo de confianza
sm.regression(x = x_train, y = y_train, model = "none", display = "se",
              xlab = "Capacidad de la batería (kWh)", ylab = "Autonomía (km)", col = 2, lwd = 2, main = "Kernel con intervalo de confianza")


# c. Test de no efecto (¿y depende de x?)
sm.regression(x = x_train, y = y_train, model = "no effect", main = "Test de no efecto", xlab = "Capacidad de la batería (kwh)", ylab = "Autonomía (km)")

# d. Test de linealidad
sm.regression(x = x_train, y = y_train, model = "linear", main = "Test de no linealidad", xlab = "Capacidad de la batería (kwh)", ylab = "Autonomía (km)")

#--------------------------------------------------------
# 4. MSE y MAE para Regresión Kernel (en test)
#--------------------------------------------------------
kernel_pred_test <- sm.regression(x = x_train, y = y_train,
                                  eval.points = x_test,
                                  model = "none", display = "none")

y_hat_kernel <- kernel_pred_test$estimate

mse_kernel <- mean((y_test - y_hat_kernel)^2)
mae_kernel <- mean(abs(y_test - y_hat_kernel))


cat("MSE Kernel:", mse_kernel, "\n")
cat("MAE Kernel:", mae_kernel, "\n")

#--------------------------------------------------------
# 5. REGRESIÓN KNN - Comparación con Kernel
#--------------------------------------------------------

# Crear conjunto de entrenamiento y prueba
train_df <- data.frame(x = x_train, y = y_train)
test_df  <- data.frame(x = x_test)

# Buscar el mejor valor de K
valores_k <- c(2, 3, 5, 10, 20, 30, 40, 50)
errores <- data.frame(k = valores_k, MSE = NA, MAE = NA)

for (i in seq_along(valores_k)) {
  k <- valores_k[i]
  pred <- knn.reg(train = data.frame(x_train), test = data.frame(x_test), y = y_train, k = k)
  y_hat <- pred$pred
  errores$MSE[i] <- mean((y_test - y_hat)^2)
  errores$MAE[i] <- mean(abs(y_test - y_hat))
}

# Mostrar tabla de errores por K
print(errores)

# Elegir mejor k (menor MSE)
mejor_k <- errores$k[which.min(errores$MSE)]
cat("Mejor k:", mejor_k, "\n")

# Ajuste final con mejor k
knn_final <- knn.reg(train = data.frame(x_train), test = data.frame(x_test), y = y_train, k = mejor_k)
y_pred_knn <- knn_final$pred

# -----------------------------
# Métricas de evaluación
# -----------------------------
mse_knn <- mean((y_test - y_pred_knn)^2)
mae_knn <- mean(abs(y_test - y_pred_knn))

cat("MSE KNN:", mse_knn, "\n")
cat("MAE KNN:", mae_knn, "\n")

# -----------------------------
# Crear curva de regresión KNN
# -----------------------------
# Grilla de valores para la curva
grid_x <- seq(min(x), max(x), length.out = 200)
grid_df <- data.frame(x = grid_x)

# Predicción sobre la grilla
knn_curve <- knn.reg(train = data.frame(x = x_train),
                     test = grid_df,
                     y = y_train,
                     k = mejor_k)

# Gráfico: datos y curva estimada
plot(x_train, y_train, pch = 16, col = "gray", cex = 0.8,
     xlab = "Capacidad de la batería (kwh)", ylab = "Autonomía (km)",
     main = paste("Curva de regresión KNN (k =", mejor_k, ")"))
lines(grid_x, knn_curve$pred, col = "red", lwd = 2)



#--------------------------------------------------------
# 6. REGRESIÓN LOESS (Locpoly)
#--------------------------------------------------------

# a. Estimar el mejor bandwidth con método plug-in
h_loess <- dpill(x_train, y_train) 
cat("Bandwidth LOESS estimado (dpill):", round(h_loess, 3), "\n")

# b. Ajustar el modelo sobre datos de entrenamiento
fit_loess <- locpoly(x = x_train, y = y_train, bandwidth = h_loess)

# c. Predicciones en conjunto de prueba (interpolación lineal)
y_hat_loess <- approx(fit_loess$x, fit_loess$y, xout = x_test, rule = 2)$y

# d. Cálculo de errores
mse_loess <- mean((y_test - y_hat_loess)^2)
mae_loess <- mean(abs(y_test - y_hat_loess))

cat("MSE LOESS:", mse_loess, "\n")
cat("MAE LOESS:", mae_loess, "\n")

# e. Visualización de la curva de regresión LOESS
plot(x_train, y_train, pch = 16, col = "gray", cex = 0.8,
     xlab = "Capacidad de la batería (kwh)", ylab = "Autonomía (km)",
     main = paste("Curva de regresión LOESS (h =", round(h_loess, 2), ")"))
lines(fit_loess, col = "darkgreen", lwd = 2)

#--------------------------------------------------------
# 7. Comparación final
#--------------------------------------------------------

cat("\n--- COMPARACIÓN DE MODELOS ---\n")
cat("Kernel -> MSE:", round(mse_kernel, 2), " MAE:", round(mae_kernel, 2), "\n")
cat("KNN    -> MSE:", round(mse_knn, 2), " MAE:", round(mae_knn, 2), "\n")
cat("LOESS  -> MSE:", round(mse_loess, 2), " MAE:", round(mae_loess, 2), "\n")



