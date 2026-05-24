# Análisis no paramétrico en vehículos eléctricos

Proyecto desarrollado para el curso de Teoría de Estadística No Paramétrica de la Universidad Nacional de Colombia.

## Objetivo

Analizar la relación entre la autonomía (`range_km`) y la capacidad de batería (`battery_capacity_kWh`) en vehículos eléctricos mediante métodos no paramétricos.

## Contenido del análisis

El proyecto incluye:

- Estadísticas descriptivas.
- Estimación de densidad Kernel.
- Intervalos de confianza para la densidad.
- Bootstrap para estimación de media y varianza.
- Pruebas no paramétricas de normalidad.
- Regresión no paramétrica:
  - Kernel Regression
  - K-Nearest Neighbors (KNN)
  - LOESS / Local Polynomial Regression
- Comparación de modelos mediante:
  - MSE
  - MAE

## Librerías utilizadas

- readxl
- dplyr
- sm
- bde
- extraDistr
- FNN
- KernSmooth
- RColorBrewer

## Base de datos

El análisis utiliza información de especificaciones de vehículos eléctricos contenida en:

```r
electric_vehicles_spec_2025.xlsx
