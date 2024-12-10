# Ggplot: crear visualizaciones gráficas con los datos 

# https://www.econjournals.com/index.php/ijeep/article/view/8037/4503

# Tengo que tener instalado y habilitado tidyverse
# install.packages("tidyverse")
library(tidyverse)

# cargar base de datos
data <- read_csv("https://raw.githubusercontent.com/Lufesc/ggplot/refs/heads/main/DatosSectorialesDptos.csv")
data

# Exploración inicial gráfico de dispersión
data %>%
  ggplot(aes(x=MANUF, y=PIB)) +
  geom_point()
  
# Gráfico de una variable
# Gráfico de barras
data %>%
  ggplot(aes(x=DEPTO)) +
  geom_bar()
  
data <- data %>% 
  mutate(ciclo = if_else(PIB <= 2.0, 
                                   "Recesion", 
                                   "Expansion")) 

data %>%
  ggplot(aes(x=ciclo)) + 
  geom_bar()
  
# Gráfico de líneas y puntos

data %>%
  filter(`DEPTO` == "SCZ") %>%
  ggplot(aes(x = PERIODO, y = PIB))+
  geom_line() 

data %>%
  filter(`DEPTO` == "SCZ") %>%
  ggplot(aes(x = PERIODO, y = PIB))+
  geom_line() +
  geom_point()

# Histograma
data %>%
  ggplot(aes(x = PIB)) +
  geom_histogram()

# Personalización del histograma
# Cambiar el número de bins o el ancho de las barras:

data %>% ggplot(aes(x = PIB)) + geom_histogram(bins = 10)
data %>% ggplot(aes(x = PIB)) + geom_histogram(binwidth = 5) # especificar el ancho de los bins

# Gráfico de columnas

data %>%
  filter(`DEPTO` == "SCZ") %>%
  ggplot(aes(x = PERIODO, y = PIB))+
  geom_col() 

data %>% 
  group_by(DEPTO) %>% 
  summarise(promedio = mean(PIB)) %>% 
  ggplot(aes(x = DEPTO, y = promedio)) + 
  geom_col()

# Gráfico de barras apiladas y proporciones

data %>% 
  group_by(DEPTO, ciclo) %>% 
  summarise(promedio = mean(PIB)) %>% 
  ggplot(aes(x = DEPTO, y = promedio, fill = ciclo)) + 
  geom_col(position = "dodge")

data %>% 
  group_by(DEPTO, ciclo) %>% 
  summarise(promedio = mean(PIB)) %>% 
  ggplot(aes(x = ciclo, y = promedio, fill = DEPTO)) + 
  geom_col(position = "dodge")

data %>% 
  group_by(DEPTO, ciclo) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(x = ciclo, y = tot, fill = DEPTO)) + 
  geom_col(position = "stack")

data %>% 
  group_by(DEPTO, ciclo) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(x = ciclo, y = tot, fill = DEPTO)) + 
  geom_col(position = "fill")

# Gráficos de línea combinando diferentes atributos

data %>% 
  filter(`DEPTO` %in% c("LA PAZ", "CBBA", "SCZ")) %>% 
  ggplot(aes(x = PERIODO, y = PIB, color = `DEPTO`)) + 
  geom_line()

data %>% 
  filter(`DEPTO` %in% c("LA PAZ", "CBBA", "SCZ")) %>% 
  ggplot(aes(x = PERIODO, y = PIB, linetype = `DEPTO`)) + 
  geom_line()

# Gráficos de dispersión

data %>% 
  ggplot(aes(x=MANUF, y=PIB)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>% 
  ggplot(aes(x=MANUF, y=PIB)) +
  geom_point() +
  geom_smooth()

data %>% 
  ggplot(aes(x=MANUF, y=PIB, color = DEPTO)) +
  geom_point() 

data %>% 
  ggplot(aes(x=MANUF, y=PIB, shape = DEPTO)) +
  geom_point()

data %>% 
  ggplot(aes(x=MANUF, y=PIB, size = DEPTO)) +
  geom_point()

data %>% 
  ggplot(aes(x=MANUF, y=PIB, color = CONSTR)) +
  geom_point()

data %>% 
  ggplot(aes(x=MANUF, y=PIB, shape = DEPTO, color = CONSTR)) +
  geom_point()

data %>% 
  ggplot(aes(x=MANUF, y=PIB, shape = DEPTO, color = CONSTR)) +
  geom_point() 

data %>% 
  ggplot(aes(x=MANUF, y=PIB, shape = DEPTO, color = CONSTR)) +
  geom_point() +
  geom_smooth(method = "lm")

## Manipulación de gráficos
# Rotulación: es preciso rotular claramente cada elemento de un gráfico.

data %>% 
  ggplot(aes(x=MANUF, y=PIB, color = DEPTO)) +
  geom_point() 

# Una versión rotulada del mismo gráfico

data %>% 
  ggplot(aes(x=MANUF, y=PIB, color = DEPTO)) +
  geom_point() +
  labs(title = "Actividad económica y producción industrial",
       subtitle = "Medidos en los departamentos del eje central, Bolivia",
       caption = "Fuente: Instituto Nacional de Estadística.",
       x = "Producción industrial (%)",
       y = "Producción económica (%)",
       color = "Departamentos")

data %>% 
  group_by(DEPTO) %>% 
  summarise(promedio = mean(PIB)) %>% 
  ggplot(aes(x = DEPTO, y = promedio)) + 
  geom_col() +
  labs(title = "Crecimiento económico promedio departamental",
       x = "Departamemtos",
       y = "Promedio, %") + 
  geom_text(aes(label=round(promedio, 2)), size=4, color="black", vjust = -0.2)

data %>% 
  group_by(DEPTO, ciclo) %>% 
  summarise(promedio = mean(PIB)) %>% 
  ggplot(aes(x = DEPTO, y = promedio, fill = ciclo)) + 
  geom_col(position = "dodge") +
  labs(title = "Crecimiento económico promedio departamental por ciclo",
       x = "Departamemtos",
       y = "Promedio, %",
       fill = "Ciclo económico") + 
  geom_text(aes(label=round(promedio, 2)), size=4, color="black", vjust = -0.2, position = position_dodge(0.9))

# Se puede realizar más ajustes: https://tidyverse.github.io/ggplot2-docs/reference/geom_text.html

# Cambiar el formato de números en ejes

datos_grandes <- tibble(Nombre = c("Bob", "Ana", "Jes"),
                        Valor = c(2023889, 5998300, 3700112))

datos_grandes

datos_grandes %>% 
  ggplot(aes(x = Nombre, y = Valor)) +
  geom_col()

datos_grandes %>% 
  ggplot(aes(x = Nombre, y = Valor)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_comma())

datos_grandes %>%
  mutate(Valor = Valor/1000000) %>% 
  ggplot(aes(x = Nombre, y = Valor)) +
  geom_col() +
  labs(y = "Valor (millones)")
