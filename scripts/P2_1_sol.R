#1
diabetes <- read.table("diabetes.data", header = T, na.strings = c("-9999.0"))
diabetes <- na.omit(diabetes)

# hay 10 variables numericas y un factor
str(diabetes)

# indice de las variables numericas
idx_num <- sapply(diabetes, is.numeric)

#3
summary(diabetes)

# la varianza solo tiene sentido para las variables numericas
var(diabetes[idx_num])

#4
boxplot(diabetes[idx_num])

#5 
sapply(diabetes[idx_num], tapply, diabetes$SEX, mean)

#6
# ignoramos SEX por factor e Y porque la cor es 1
cor_y <- cor(diabetes$Y, diabetes[, !names(diabetes) %in% c("SEX", "Y")])
# o simplemente, aunque nos complica hacer automaticamente el ej 7
cor_y_alt <- cor(diabetes$Y, diabetes[idx_num])

#7
# de los nombres de las variables escogemos las de mayor y menor valor absoluto
vars <- colnames(cor_y)[match(range(abs(cor_y)), cor_y)]
par(mfcol=c(1,2))
for (var in vars) plot(diabetes[, var], diabetes$Y, xlab=var, ylab="Y", pch=20)

#8
diabetes$SEX <- as.numeric(diabetes$SEX)
#o, si queremos los valores al reves
diabetes$SEX <- ifelse(diabetes$SEX == "M", 1, 2)

#9
# hay mas formas, una posible
is_outlier <- function(x, const=3) {
  x < median(x) - const*mad(x) | x > median(x) + const*mad(x)
}

mask <- sapply(diabetes[idx_num], is_outlier)
outliers <- apply(mask, 1, any)
diabetes <- diabetes[!outliers, ]


#10
idx <- sample(nrow(diabetes), 0.7*nrow(diabetes))
d_big <- diabetes[idx, ]
d_small <- diabetes[-idx, ]

#11
d_big_std <- scale(d_big)
d_small_std <- as.data.frame(scale(d_small, 
                                   center=attr(d_big_std, "scaled:center"), 
                                   scale=attr(d_big_std, "scaled:scale")))
# scale convierte el argumento en matrices, no devuelve un data.frame
d_big_std <- as.data.frame(d_big_std)

# tambien seria correcto calcular las medias y desviaciones una ve y 
# pasarselas a la funcion scale, sin usar attr

#o, sin usar la funcion scale
mean_d <- lapply(d_big, mean)
sd_d   <- lapply(d_big, sd)

d_big_std   <- (d_big - mean_d) / sd_d
d_small_std <- (d_small - mean_d) / sd_d