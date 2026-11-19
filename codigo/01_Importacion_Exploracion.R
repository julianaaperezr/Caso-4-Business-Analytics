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

# Análisis descriptivo de la variable Churn
# Cantidad de clientes según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(Clientes = n())

# Porcentaje de clientes según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Porcentaje = n() / nrow(QWE) * 100
  )

# Antigüedad promedio según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Antiguedad_promedio = mean(`Customer Age (in months)`, na.rm = TRUE)
  )

# CHI promedio según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    CHI_promedio = mean(`CHI Score 0-1`, na.rm = TRUE)
  )

# Solicitudes de soporte promedio según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Soporte_promedio = mean(`Support Cases 0-1`, na.rm = TRUE)
  )

# Revisar los valores de la variable soporte
summary(QWE$`Support Cases 0-1`)

# Solicitudes de soporte iniciales según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Soporte_inicial = mean(`Support Cases Month 0`, na.rm = TRUE)
  )

# Resumen general de la base de datos
summary(QWE)

# Actividad de los clientes según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Logins_promedio = mean(`Logins 0-1`, na.rm = TRUE)
  )

# Visualizaciones promedio según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Views_promedio = mean(`Views 0-1`, na.rm = TRUE)
  )

# Tiempo desde el último ingreso según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Dias_promedio = mean(`Days Since Last Login 0-1`, na.rm = TRUE)
  )

# Tiempo desde el último ingreso según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Dias_promedio = mean(`Days Since Last Login 0-1`, na.rm = TRUE)
  )

# Cambio promedio en artículos publicados según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Articulos_promedio = mean(`Blog Articles 0-1`, na.rm = TRUE)
  )

# Cambio en la prioridad del soporte según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Prioridad_promedio = mean(`SP 0-1`, na.rm = TRUE)
  )

# CHI inicial promedio según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    CHI_inicial = mean(`CHI Score Month 0`, na.rm = TRUE)
  )

# Prioridad inicial del soporte según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Prioridad_inicial = mean(`SP Month 0`, na.rm = TRUE)
  )

# Solicitudes iniciales de soporte según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Soporte_inicial = mean(`Support Cases Month 0`, na.rm = TRUE)
  )

# Resumen de variables según abandono
QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Antiguedad = mean(`Customer Age (in months)`, na.rm = TRUE),
    CHI_inicial = mean(`CHI Score Month 0`, na.rm = TRUE),
    CHI_cambio = mean(`CHI Score 0-1`, na.rm = TRUE),
    Soporte_inicial = mean(`Support Cases Month 0`, na.rm = TRUE),
    Prioridad_inicial = mean(`SP Month 0`, na.rm = TRUE),
    Logins_cambio = mean(`Logins 0-1`, na.rm = TRUE),
    Views_cambio = mean(`Views 0-1`, na.rm = TRUE),
    Dias_cambio = mean(`Days Since Last Login 0-1`, na.rm = TRUE),
    Articulos_cambio = mean(`Blog Articles 0-1`, na.rm = TRUE)
  )

# Visualizar la tabla completa
Resumen_QWE <- QWE %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Clientes = n(),
    Antiguedad = mean(`Customer Age (in months)`, na.rm = TRUE),
    CHI_inicial = mean(`CHI Score Month 0`, na.rm = TRUE),
    CHI_cambio = mean(`CHI Score 0-1`, na.rm = TRUE),
    Soporte_inicial = mean(`Support Cases Month 0`, na.rm = TRUE),
    Prioridad_inicial = mean(`SP Month 0`, na.rm = TRUE),
    Logins_cambio = mean(`Logins 0-1`, na.rm = TRUE),
    Views_cambio = mean(`Views 0-1`, na.rm = TRUE),
    Dias_cambio = mean(`Days Since Last Login 0-1`, na.rm = TRUE),
    Articulos_cambio = mean(`Blog Articles 0-1`, na.rm = TRUE)
  )

View(Resumen_QWE)

# Distribución de clientes según abandono
library(ggplot2)

ggplot(QWE, aes(x = factor(`Churn (1 = Yes, 0 = No)`))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribución de clientes según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Número de clientes"
  ) +
  theme_minimal()

# CHI inicial según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `CHI Score Month 0`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "CHI inicial según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "CHI inicial"
  ) +
  theme_minimal()

# Actividad de los clientes según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Logins 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Actividad de los clientes según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en logins"
  ) +
  theme_minimal()

# Cambio en visualizaciones según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Views 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cambio en visualizaciones según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en visualizaciones"
  ) +
  theme_minimal()

# Cambio en días desde el último ingreso según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Days Since Last Login 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cambio en días desde el último ingreso según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en días desde el último ingreso"
  ) +
  theme_minimal()

# Cambio en artículos publicados según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Blog Articles 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cambio en artículos publicados según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en artículos publicados"
  ) +
  theme_minimal()

# Antigüedad de los clientes según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Customer Age (in months)`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Antigüedad de los clientes según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Antigüedad en meses"
  ) +
  theme_minimal()

# Cambio en el indicador CHI según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `CHI Score 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cambio en el indicador CHI según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en CHI"
  ) +
  theme_minimal()

# Solicitudes iniciales de soporte según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `Support Cases Month 0`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Solicitudes iniciales de soporte según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Solicitudes iniciales de soporte"
  ) +
  theme_minimal()

# Prioridad inicial del soporte según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `SP Month 0`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Prioridad inicial del soporte según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Prioridad inicial del soporte"
  ) +
  theme_minimal()

# Cambio en la prioridad del soporte según abandono
ggplot(QWE, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`),
  y = `SP 0-1`
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cambio en la prioridad del soporte según abandono",
    x = "Estado del cliente (0 = Permanece, 1 = Abandona)",
    y = "Cambio en la prioridad del soporte"
  ) +
  theme_minimal()

# Revisar valores faltantes por variable
colSums(is.na(QWE))
