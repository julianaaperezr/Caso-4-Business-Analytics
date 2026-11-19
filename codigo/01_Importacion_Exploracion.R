# CASO 4 - QWE Inc.
# Business Analytics

# Cargar librerías
library(readxl)
library(dplyr)

# Identificar las hojas del archivo Excel
excel_sheets("datos/DATA.xlsx")

# Importar base de datos
QWE <- read_excel("datos/DATA.xlsx", sheet = "Case Data")

# Validación inicial de la base de datos
nrow(QWE)
ncol(QWE)
names(QWE)
head(QWE)