library(DBI)
data <- read.csv("C:/Users/rodja/Desktop/R/Gus/athlete_events.csv")


message("Limpieza de datos")
unique_data <- unique(data[c("Year", "Event", "Medal", "NOC")])
datos_sin_na <- na.omit(unique_data)
cantidad_medallas_pais <- table(datos_sin_na$Medal, datos_sin_na$NOC)
#message("Antes de la Revolucion Rusa y Primera Guerra Mundial")
data2 <- datos_sin_na
filtered_data_bf <- data2[data2$Year >= 1896 & data2$Year <= 1912, c("Year", "Event", "Medal", "NOC")]
filtered_data_bf$Medal <- ifelse(is.na(filtered_data_bf$Medal), 0, filtered_data_bf$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_bf$NOC), Year = unique(filtered_data_bf$Year))
filtered_data_bf$Medal[is.na(filtered_data_bf$Medal)] <- "0"
filtered_data_bf$Medal <- factor(filtered_data_bf$Medal)
cantidad_medallas_pais_ano_bf <- aggregate(Medal ~ NOC + Year, filtered_data_bf, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_bf, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_bf <- all_combinations
medal_values_bf <- unlist(cantidad_medallas_pais_ano_bf$Medal)
#message("Despues de la Revolucion Rusa y Primera Guerra Mundial")
data3 <- datos_sin_na
filtered_data_af <- data3[data3$Year >= 1924 & data3$Year <= 1936, c("Year", "Event", "Medal", "NOC")]
filtered_data_af$Medal <- ifelse(is.na(filtered_data_af$Medal), 0, filtered_data_af$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_af$NOC), Year = unique(filtered_data_af$Year))
filtered_data_af$Medal[is.na(filtered_data_af$Medal)] <- "0"
filtered_data_af$Medal <- factor(filtered_data_af$Medal)
cantidad_medallas_pais_ano_af <- aggregate(Medal ~ NOC + Year, filtered_data_af, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_af, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_af <- all_combinations
medal_values_af <- unlist(cantidad_medallas_pais_ano_af$Medal)
common_NOC <- intersect(cantidad_medallas_pais_ano_af$NOC, cantidad_medallas_pais_ano_bf$NOC)

new_cmpa_af <- cantidad_medallas_pais_ano_af[cantidad_medallas_pais_ano_af$NOC %in% common_NOC, ]
new_cmpa_bf <- cantidad_medallas_pais_ano_bf[cantidad_medallas_pais_ano_bf$NOC %in% common_NOC, ]
medal_values_af <- unlist(new_cmpa_af$Medal)
medal_values_bf <- unlist(new_cmpa_bf$Medal)
new_cmpa_af$Sum_Medals <- rowSums(new_cmpa_af$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)
new_cmpa_bf$Sum_Medals <- rowSums(new_cmpa_bf$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)


new_cmpa_af_totmedals <- new_cmpa_af[, !(names(new_cmpa_af) %in% c("Year"))]
total_medals_af <- aggregate(. ~ NOC, data = new_cmpa_af_totmedals, FUN = sum)
new_cmpa_bf_totmedals <- new_cmpa_bf[, !(names(new_cmpa_bf) %in% c("Year"))]
total_medals_bf <- aggregate(. ~ NOC, data = new_cmpa_bf_totmedals, FUN = sum)

order_indices <- match(total_medals_af$NOC, total_medals_bf$NOC)
total_medals_bf <- total_medals_bf[order_indices, ]

message("Antes de la Revolucion Rusa y Primera Guerra Mundial")
print(total_medals_bf)
message("Despues de la Revolucion Rusa y Primera Guerra Mundial")
print(total_medals_af)

despues <- total_medals_af$Sum_Medals
antes <- total_medals_bf$Sum_Medals

data_combinados <- cbind(despues, antes)

barplot(data_combinados, beside = TRUE, col = c("green", "blue"))

covarianza= cov(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)



AF= total_medals_af$Sum_Medals
BF= total_medals_bf$Sum_Medals


var1 = var(AF)
var2 = var(BF)
DE = sqrt(((length(AF)-1) * var1 + (length(BF)-1) * var2) / (length(BF) + length(AF) - 1))

y=lm(AF ~ BF)
print("La correlacion cuando empezo la Revolucion Rusa y la Primera Guerra Mundial es: " )
cor.test(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)
cat("La Desviacion estandar cuando empezo la Revolucion Rusa y la Primera Guerra Mundial es: ", DE )
cat("La covarianza cuando empezo la Revolucion Rusa y la Primera Guerra Mundial  es: ", covarianza )
cat("La regresion lineal cuando empezo la Revolucion Rusa y la Primera Guerra Mundial es: ")
y

library(DBI)
data <- read.csv("C:/Users/rodja/Desktop/R/Gus/athlete_events.csv")


message("Limpieza de datos")
unique_data <- unique(data[c("Year", "Event", "Medal", "NOC")])
datos_sin_na <- na.omit(unique_data)
cantidad_medallas_pais <- table(datos_sin_na$Medal, datos_sin_na$NOC)

#message("Antes de la Segunda Guerra Mundial")
data2 <- datos_sin_na
filtered_data_bf <- data2[data2$Year >= 1912 & data2$Year <= 1936, c("Year", "Event", "Medal", "NOC")]
filtered_data_bf$Medal <- ifelse(is.na(filtered_data_bf$Medal), 0, filtered_data_bf$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_bf$NOC), Year = unique(filtered_data_bf$Year))
filtered_data_bf$Medal[is.na(filtered_data_bf$Medal)] <- "0"
filtered_data_bf$Medal <- factor(filtered_data_bf$Medal)
cantidad_medallas_pais_ano_bf <- aggregate(Medal ~ NOC + Year, filtered_data_bf, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_bf, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_bf <- all_combinations
medal_values_bf <- unlist(cantidad_medallas_pais_ano_bf$Medal)

#message("Despues de la Segunda Guerra Mundial")
data3 <- datos_sin_na
filtered_data_af <- data3[data3$Year >= 1948 & data3$Year <= 1964, c("Year", "Event", "Medal", "NOC")]
filtered_data_af$Medal <- ifelse(is.na(filtered_data_af$Medal), 0, filtered_data_af$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_af$NOC), Year = unique(filtered_data_af$Year))
filtered_data_af$Medal[is.na(filtered_data_af$Medal)] <- "0"
filtered_data_af$Medal <- factor(filtered_data_af$Medal)
cantidad_medallas_pais_ano_af <- aggregate(Medal ~ NOC + Year, filtered_data_af, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_af, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_af <- all_combinations
medal_values_af <- unlist(cantidad_medallas_pais_ano_af$Medal)
#print(cantidad_medallas_pais_ano_af)
#print(medal_values_af)

common_NOC <- intersect(cantidad_medallas_pais_ano_af$NOC, cantidad_medallas_pais_ano_bf$NOC)
new_cmpa_af <- cantidad_medallas_pais_ano_af[cantidad_medallas_pais_ano_af$NOC %in% common_NOC, ]
new_cmpa_bf <- cantidad_medallas_pais_ano_bf[cantidad_medallas_pais_ano_bf$NOC %in% common_NOC, ]
medal_values_af <- unlist(new_cmpa_af$Medal)
medal_values_bf <- unlist(new_cmpa_bf$Medal)

new_cmpa_af$Sum_Medals <- rowSums(new_cmpa_af$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)
new_cmpa_bf$Sum_Medals <- rowSums(new_cmpa_bf$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)


new_cmpa_af_totmedals <- new_cmpa_af[, !(names(new_cmpa_af) %in% c("Year"))]
total_medals_af <- aggregate(. ~ NOC, data = new_cmpa_af_totmedals, FUN = sum)
new_cmpa_bf_totmedals <- new_cmpa_bf[, !(names(new_cmpa_bf) %in% c("Year"))]
total_medals_bf <- aggregate(. ~ NOC, data = new_cmpa_bf_totmedals, FUN = sum)

order_indices <- match(total_medals_af$NOC, total_medals_bf$NOC)
total_medals_bf <- total_medals_bf[order_indices, ]

message("Antes de la Segunda Guerra Mundial")
print(total_medals_bf)
message("Despues de la Segunda Guerra Mundial")
print(total_medals_af)

despues <- total_medals_af$Sum_Medals
antes <- total_medals_bf$Sum_Medals

data_combinados <- cbind(despues, antes)

barplot(data_combinados, beside = TRUE, col = c("green", "blue"))

covarianza= cov(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)



AF= total_medals_af$Sum_Medals
BF= total_medals_bf$Sum_Medals


var1 = var(AF)
var2 = var(BF)
DE = sqrt(((length(AF)-1) * var1 + (length(BF)-1) * var2) / (length(BF) + length(AF) - 1))

y=lm(AF ~ BF)
print("La correlacion cuando empezo la Segunda Guerra Mundial es: ")
cor.test(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)
cat("La Desviacion estandar cuando empezo la Segunda Guerra Mundial es: ", DE )
cat("La covarianza cuando empezo la Segunda Guerra Mundial  es: ", covarianza )
cat("La regresion lineal cuando empezo la Segunda Guerra Mundial es:  ")
y

library(DBI)
data <- read.csv("C:/Users/rodja/Desktop/R/Gus/athlete_events.csv")


message("Limpieza de datos")
unique_data <- unique(data[c("Year", "Event", "Medal", "NOC")])
datos_sin_na <- na.omit(unique_data)
cantidad_medallas_pais <- table(datos_sin_na$Medal, datos_sin_na$NOC)
#message("Antes de la caida del muro de Berlin")
data2 <- datos_sin_na
filtered_data_bf <- data2[data2$Year >= 1980 & data2$Year <= 1988, c("Year", "Event", "Medal", "NOC")]
filtered_data_bf$Medal <- ifelse(is.na(filtered_data_bf$Medal), 0, filtered_data_bf$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_bf$NOC), Year = unique(filtered_data_bf$Year))
filtered_data_bf$Medal[is.na(filtered_data_bf$Medal)] <- "0"
filtered_data_bf$Medal <- factor(filtered_data_bf$Medal)
cantidad_medallas_pais_ano_bf <- aggregate(Medal ~ NOC + Year, filtered_data_bf, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_bf, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_bf <- all_combinations
medal_values_bf <- unlist(cantidad_medallas_pais_ano_bf$Medal)
#message("Despues de la caida del muro de Berlin")
data3 <- datos_sin_na
filtered_data_af <- data3[data3$Year >= 1992 & data3$Year <= 1998, c("Year", "Event", "Medal", "NOC")]
filtered_data_af$Medal <- ifelse(is.na(filtered_data_af$Medal), 0, filtered_data_af$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_af$NOC), Year = unique(filtered_data_af$Year))
filtered_data_af$Medal[is.na(filtered_data_af$Medal)] <- "0"
filtered_data_af$Medal <- factor(filtered_data_af$Medal)
cantidad_medallas_pais_ano_af <- aggregate(Medal ~ NOC + Year, filtered_data_af, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_af, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_af <- all_combinations
medal_values_af <- unlist(cantidad_medallas_pais_ano_af$Medal)
common_NOC <- intersect(cantidad_medallas_pais_ano_af$NOC, cantidad_medallas_pais_ano_bf$NOC)

new_cmpa_af <- cantidad_medallas_pais_ano_af[cantidad_medallas_pais_ano_af$NOC %in% common_NOC, ]
new_cmpa_bf <- cantidad_medallas_pais_ano_bf[cantidad_medallas_pais_ano_bf$NOC %in% common_NOC, ]
medal_values_af <- unlist(new_cmpa_af$Medal)
medal_values_bf <- unlist(new_cmpa_bf$Medal)
new_cmpa_af$Sum_Medals <- rowSums(new_cmpa_af$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)
new_cmpa_bf$Sum_Medals <- rowSums(new_cmpa_bf$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)
#print(new_cmpa_af$Sum_Medals)
#print(new_cmpa_bf$Sum_Medals)

new_cmpa_af_totmedals <- new_cmpa_af[, !(names(new_cmpa_af) %in% c("Year"))]
total_medals_af <- aggregate(. ~ NOC, data = new_cmpa_af_totmedals, FUN = sum)
new_cmpa_bf_totmedals <- new_cmpa_bf[, !(names(new_cmpa_bf) %in% c("Year"))]
total_medals_bf <- aggregate(. ~ NOC, data = new_cmpa_bf_totmedals, FUN = sum)

order_indices <- match(total_medals_af$NOC, total_medals_bf$NOC)
total_medals_bf <- total_medals_bf[order_indices, ]

message("Antes de la caida del muro de Berlin")
print(total_medals_bf)
message("Despues de la caida del muro de Berlin")
print(total_medals_af)

despues <- total_medals_af$Sum_Medals
antes <- total_medals_bf$Sum_Medals

data_combinados <- cbind(despues, antes)

barplot(data_combinados, beside = TRUE, col = c("green", "blue"))


covarianza= cov(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)



AF= total_medals_af$Sum_Medals
BF= total_medals_bf$Sum_Medals


var1 = var(AF)
var2 = var(BF)
DE = sqrt(((length(AF)-1) * var1 + (length(BF)-1) * var2) / (length(BF) + length(AF) - 1))

y=lm(AF ~ BF)
print("La correlacion cuando el muro de Berlin cayo es: ")
cor.test(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)
cat("La Desviacion estandar cuando el muro de Berlin cayo es: ", DE )
cat("La covarianza cuando el muro de Berlin cayo es: ", covarianza )
cat("La regresion lineal cuando el muro de Berlin cayo es: ")
y

library(DBI)
data <- read.csv("C:/Users/rodja/Desktop/R/Gus/athlete_events.csv")


message("Limpieza de datos")
unique_data <- unique(data[c("Year", "Event", "Medal", "NOC")])
datos_sin_na <- na.omit(unique_data)
cantidad_medallas_pais <- table(datos_sin_na$Medal, datos_sin_na$NOC)
#message("Antes de que el primer hombre pisara la luna y antes de la muerte de Martin Luther King")
data2 <- datos_sin_na
filtered_data_bf <- data2[data2$Year >= 1960 & data2$Year <= 1968, c("Year", "Event", "Medal", "NOC")]
filtered_data_bf$Medal <- ifelse(is.na(filtered_data_bf$Medal), 0, filtered_data_bf$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_bf$NOC), Year = unique(filtered_data_bf$Year))
filtered_data_bf$Medal[is.na(filtered_data_bf$Medal)] <- "0"
filtered_data_bf$Medal <- factor(filtered_data_bf$Medal)
cantidad_medallas_pais_ano_bf <- aggregate(Medal ~ NOC + Year, filtered_data_bf, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_bf, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_bf <- all_combinations
medal_values_bf <- unlist(cantidad_medallas_pais_ano_bf$Medal)
#message("Despues de que el primer hombre pisara la luna y despues de la muerte de Martin Luther King")
data3 <- datos_sin_na
filtered_data_af <- data3[data3$Year >= 1972 & data3$Year <= 1980, c("Year", "Event", "Medal", "NOC")]
filtered_data_af$Medal <- ifelse(is.na(filtered_data_af$Medal), 0, filtered_data_af$Medal)
all_combinations <- expand.grid(NOC = unique(filtered_data_af$NOC), Year = unique(filtered_data_af$Year))
filtered_data_af$Medal[is.na(filtered_data_af$Medal)] <- "0"
filtered_data_af$Medal <- factor(filtered_data_af$Medal)
cantidad_medallas_pais_ano_af <- aggregate(Medal ~ NOC + Year, filtered_data_af, table)
all_combinations <- merge(all_combinations, cantidad_medallas_pais_ano_af, by = c("NOC", "Year"), all.x = TRUE)
all_combinations$Medal[is.na(all_combinations$Medal)] <- 0
cantidad_medallas_pais_ano_af <- all_combinations
medal_values_af <- unlist(cantidad_medallas_pais_ano_af$Medal)

common_NOC <- intersect(cantidad_medallas_pais_ano_af$NOC, cantidad_medallas_pais_ano_bf$NOC)

new_cmpa_af <- cantidad_medallas_pais_ano_af[cantidad_medallas_pais_ano_af$NOC %in% common_NOC, ]
new_cmpa_bf <- cantidad_medallas_pais_ano_bf[cantidad_medallas_pais_ano_bf$NOC %in% common_NOC, ]
medal_values_af <- unlist(new_cmpa_af$Medal)
medal_values_bf <- unlist(new_cmpa_bf$Medal)
new_cmpa_af$Sum_Medals <- rowSums(new_cmpa_af$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)
new_cmpa_bf$Sum_Medals <- rowSums(new_cmpa_bf$Medal[, c("Bronze", "Gold", "Silver")], na.rm = TRUE)


new_cmpa_af_totmedals <- new_cmpa_af[, !(names(new_cmpa_af) %in% c("Year"))]
total_medals_af <- aggregate(. ~ NOC, data = new_cmpa_af_totmedals, FUN = sum)
new_cmpa_bf_totmedals <- new_cmpa_bf[, !(names(new_cmpa_bf) %in% c("Year"))]
total_medals_bf <- aggregate(. ~ NOC, data = new_cmpa_bf_totmedals, FUN = sum)


order_indices <- match(total_medals_af$NOC, total_medals_bf$NOC)
total_medals_bf <- total_medals_bf[order_indices, ]

message("Antes de que el primer hombre pisara la luna y antes de la muerte de Martin Luther King")
print(total_medals_bf)
message("Despues de que el primer hombre pisara la luna y despues de la muerte de Martin Luther King")
print(total_medals_af)

despues <- total_medals_af$Sum_Medals
antes <- total_medals_bf$Sum_Medals

data_combinados <- cbind(despues, antes)

barplot(data_combinados, beside = TRUE, col = c("green", "blue"))


covarianza= cov(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)



AF= total_medals_af$Sum_Medals
BF= total_medals_bf$Sum_Medals


var1 = var(AF)
var2 = var(BF)
DE = sqrt(((length(AF)-1) * var1 + (length(BF)-1) * var2) / (length(BF) + length(AF) - 1))

y=lm(AF ~ BF)
print("La correlacion de las medallas cuando el hombre piso la luna es: ")
cor.test(total_medals_af$Sum_Medals,total_medals_bf$Sum_Medals)
cat("La Desviacion estandar de las medallas cuando el hombre piso la luna es: ", DE )
cat("La covarianza de las medallas cuando el hombre piso la luna es: ", covarianza )
cat("La regresion lineal de las medallas cuando el hombre piso la luna es: ")
y


library(DBI)
data <- read.csv("C:/Users/rodja/Desktop/R/Gus/athlete_events.csv")

message("Limpieza de datos")
unique_data <- unique(data[c("Year", "Event", "Medal", "NOC")])
datos_sin_na <- na.omit(unique_data)

filtered_data_bf <- datos_sin_na[datos_sin_na$Year >= 1960 & datos_sin_na$Year <= 1968, ]

filtered_data_af <- datos_sin_na[datos_sin_na$Year >= 1972 & datos_sin_na$Year <= 1980, ]

filtered_data_mex_bf <- filtered_data_bf[filtered_data_bf$NOC == "MEX", ]
filtered_data_mex_af <- filtered_data_af[filtered_data_af$NOC == "MEX", ]

cantidad_medallas_deporte_bf <- table(filtered_data_mex_bf$Medal, filtered_data_mex_bf$Event)

cantidad_medallas_deporte_af <- table(filtered_data_mex_af$Medal, filtered_data_mex_af$Event)

message("Medallas por evento de MEX antes de la Masacre de Tlatelolco")
print(cantidad_medallas_deporte_bf)
message("Medallas por evento de MEX despues de Masacre de Tlatelolco")
print(cantidad_medallas_deporte_af)

order_indices <- match(total_medals_af$NOC, total_medals_bf$NOC)
total_medals_af <- total_medals_af[order_indices, ]

message("Antes de la Masacre de Tlatelolco")
print(total_medals_bf)
message("Despues de la Masacre de Tlatelolco")
print(total_medals_af)

despues <- total_medals_af$Sum_Medals
antes <- total_medals_bf$Sum_Medals

data_combinados <- cbind(despues)

barplot(data_combinados, beside = TRUE, col = c("green"))

legend("topright", legend = c("Despues"), fill = c("green"))

AF= total_medals_af$Sum_Medals
BF= total_medals_bf$Sum_Medals


DE <- sd(AF)

cat("La Desviacion estandar de Mexico cuando fue la Masacre de Tlatelolco es ", DE )
