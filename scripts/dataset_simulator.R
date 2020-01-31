library(MASS)

generate_real_variable <- function(num_instances){
	runif(num_instances, 0, 1)
}

generate_integer_variable <- function(diff_values, num_instances){
	sample(seq(0, diff_values), num_instances, replace=TRUE) / diff_values
}

create_data_dummy <- function(num_instances){
        x1 <- generate_real_variable(num_instances)
        x2 <- normalize_vector(x1 + rnorm(num_instances, 0, 0.01))
        x3 <- normalize_vector(x1 + rnorm(num_instances, 0, 0.2))
        x4 <- generate_real_variable(num_instances)
        cbind(x1, x2, x3, x4)
}


create_data_vida <- function(num_instances){
	edad <- generate_integer_variable(81, num_instances)
	profesion <- generate_real_variable(num_instances)
	capital <- generate_real_variable(num_instances)
	reconocimiento <- generate_real_variable(num_instances)
	coberturas <- generate_integer_variable(10, num_instances)
	IMC_invertido <- generate_real_variable(num_instances)
	historial_medico <- generate_real_variable(num_instances)
	nomina <- generate_real_variable(num_instances)
	numero_trabajos_distinto <- generate_integer_variable(11, num_instances)
	cbind(edad, profesion, capital, reconocimiento, coberturas, IMC_invertido, historial_medico, nomina, numero_trabajos_distinto)
}

create_data_coche <- function(num_instances){
	edad <- generate_integer_variable(53, num_instances)
	anios_antiguedad <- generate_real_variable(num_instances)
	kilometros_anio <- generate_real_variable(num_instances)
	precio_automovil <- generate_real_variable(num_instances)
	numero_puntos <- generate_integer_variable(13, num_instances)
	coberturas <- generate_integer_variable(10, num_instances)
	historial_medico <- generate_real_variable(num_instances)
	historial_vehiculo <- generate_real_variable(num_instances)
	nomina <- generate_real_variable(num_instances)
	coches_asegurados <- generate_integer_variable(10, num_instances)
	cbind(edad, anios_antiguedad, kilometros_anio, precio_automovil, numero_puntos, coberturas, historial_medico, historial_vehiculo,
		nomina, coches_asegurados)
}

create_data_hogar <- function(num_instances){
	ninios <- generate_integer_variable(6, num_instances)
	precio_propiedad <- generate_real_variable(num_instances)
	salario_medio_barrio <- generate_real_variable(num_instances)
	numero_habitantes_poblacion <- generate_real_variable(num_instances)
	asegurados <- generate_integer_variable(8, num_instances)
	habitantes_edificio <- generate_real_variable(num_instances)
	desastres_previos <- generate_real_variable(num_instances)
	coberturas <- generate_integer_variable(10, num_instances)
	altura_planta <- generate_real_variable(num_instances)
	metros_cuadrados <- generate_real_variable(num_instances)
	anios_construccion <- generate_real_variable(num_instances)
	calidad_construccion <- generate_real_variable(num_instances)
	uso <- generate_real_variable(num_instances)
	alarmas_seguridad <- generate_real_variable(num_instances)
	domotica <- generate_real_variable(num_instances)
	capital_contenido <- generate_real_variable(num_instances)
	cbind(ninios, precio_propiedad, salario_medio_barrio, numero_habitantes_poblacion, asegurados, habitantes_edificio, desastres_previos, 
		coberturas, altura_planta, metros_cuadrados, anios_construccion, calidad_construccion, uso, alarmas_seguridad, domotica,
		capital_contenido)
}

normalize_vector <- function(Y){
	y_max <- max(Y)
	y_min <- min(Y)
	(Y - y_min) / (y_max - y_min)
}

generate_dataset_dummy <- function(num_instances){
        X <- create_data_dummy(num_instances)
        Y <- 2*X[,1]-3*X[,2]
        Y <- normalize_vector(Y)
        cbind(X, Y)
}

generate_dataset_vida <- function(num_instances){
	X <- create_data_vida(num_instances)
	Y <- 55*X[,1]-50*X[,2]+34*X[,3]+61*X[,4]-55*X[,5]+3*X[,6]-4*X[,7]+31*X[,8]+10*abs(X[,9]-0.5)
	Y <- normalize_vector(Y)
	cbind(X, Y)
}

generate_dataset_coche <- function(num_instances){
	X <- create_data_coche(num_instances)
	Y <- 50*abs(X[,1]-0.5)+51*X[,2]+34*X[,3]+61*X[,4]-55*X[,5]+3*X[,6]-4*X[,7]+31*X[,8]+10*abs(X[,9]-0.5)+X[,10]*0.0
	Y <- normalize_vector(Y)
        cbind(X, Y)
}

generate_dataset_hogar <- function(num_instances){
	X <- create_data_hogar(num_instances)
	Y <- 10*X[,1]+51*X[,2]-40*X[,3]+0.0*X[,4]+5*X[,5]+0.0*X[,6]+20*X[,7]+11*X[,8]+50*abs(X[,9]-0.5)+56*X[,10]+71*X[,11]-59*X[,12]+40*abs(X[,13]-0.5)-40*X[,14]+3*abs(X[,15]-0.5)+29*X[,16]
	Y <- normalize_vector(Y)
        cbind(X, Y)
}

num_instances <- 10000
dataset_dummy <- generate_dataset_dummy(num_instances)
write.matrix(dataset_dummy, "./dataset_dummy.txt", sep=",")
dataset_vida <- generate_dataset_vida(num_instances)
write.matrix(dataset_vida, "./dataset_vida.txt", sep=",")
dataset_hogar <- generate_dataset_hogar(num_instances)
write.matrix(dataset_hogar, "./dataset_hogar.txt", sep=",")
dataset_coche <- generate_dataset_coche(num_instances)
write.matrix(dataset_coche, "./dataset_coche.txt", sep=",")

