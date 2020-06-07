// Programacion - Instancia de evaluacion I
// German Ariel Silva

SubProceso EsValido <- NumeroValido (ValorIngresado,ConDecimal)
	definir NumerosValidos como cadena;
	definir I, J Como Entero;
	definir EsValido como logico;
	
	// Registro de numeros validos
	NumerosValidos <- "1234567890.";
	
	// Validación de datos numericos
	Para I<-0 Hasta Longitud(ValorIngresado)-1 Con Paso 1 Hacer
		Para J<-0 Hasta 10 Con Paso 1 Hacer
			Si SubCadena(ValorIngresado,I,I) = SubCadena(NumerosValidos,J,J) Entonces
				J <- 10;
				EsValido <- Verdadero;
			SiNo
				EsValido <- Falso;
			FinSi
		FinPara
		Si EsValido = Falso Entonces
			I <- Longitud(ValorIngresado);
		FinSi
	FinPara
	
	// Validación de numeros con decimal
	J <- 0;
	Para I<-0 Hasta Longitud(ValorIngresado)-1 Con Paso 1 Hacer
		Si SubCadena(ValorIngresado,I,I) = "." Entonces
			si ConDecimal = Falso Entonces
				EsValido <- Falso;
				I <- Longitud(ValorIngresado);
			SiNo
				J <- J+1;
				si J = 2 Entonces
					EsValido <- Falso;
					I <- Longitud(ValorIngresado);
				FinSi
			FinSi
			
		FinSi
	FinPara
FinSubProceso

SubProceso MayorAMenorAporte (N, A, P)
	Definir NomEnOrden como cadena;
	Definir ApEnOrden como real;
	Definir I, J como entero;
	Dimension NomEnOrden[50], ApEnOrden[50];
	NomEnOrden[0] <- N[0];
	ApEnOrden[0] <- A[0];
	
	// Inicializado en cero para comparación
	Para I<-0 Hasta 49 Con Paso 1 Hacer
		ApEnOrden[I] <- 0;
	FinPara
	
	// Comparacion y asignación en orden
	Para I<-0 Hasta P-1 Con Paso 1 Hacer
		Para J<-0 Hasta P-1 Con Paso 1 Hacer
			Si A[J] > ApEnOrden[I]Entonces
				Si I=0 entonces
					NomEnOrden[I] <- N[J];
					ApEnOrden[I] <- A[J];
				SiNo
					si A[J] < ApEnOrden[I-1] Entonces
						NomEnOrden[I] <- N[J];
						ApEnOrden[I] <- A[J];
					FinSi
				FinSi
			FinSi
		FinPara
		Escribir I+1, "º: ", NomEnOrden[I], " >> $", ApEnOrden[I];
	FinPara
FinSubProceso

SubProceso aQuienPagar ( N, A, P, Promedio)
	Definir EsDeudor como logico;
	Definir Cobrar, Pagar como real;
	Definir I, J, cont como entero;
	Definir Nombres como cadena;
	Dimension EsDeudor[50], Cobrar[100], Pagar[100], Nombres[100], cont[2];
	
	// Inicializado
	cont[0] <- 0;
	cont[1] <- 0;
	Para I<-0 Hasta 49 Con Paso 1 Hacer
		Cobrar[I] <- 0;
		Pagar[I] <- 0;
		Nombres[I] <- "";
		Nombres[I+50] <- "";
	FinPara
	
	// Distinción de deudores y no deudores - Registro de monto a favor/adeudado
	Para I<-0 Hasta P-1 Con Paso 1 Hacer
		si A[I] >= Promedio Entonces
			Cobrar[I+50] <- (trunc(A[I] - Promedio) + ((redon(((A[I] - Promedio) - trunc(A[I] - Promedio)) * 100)) / 100));
			EsDeudor[I] <- Falso;
			cont[0] <- cont[0] + 1;
		SiNo
			Pagar[I+50] <- (trunc(Promedio - A[I]) + ((redon(((Promedio - A[I]) - trunc(Promedio - A[I])) * 100)) / 100));
			EsDeudor[I] <- Verdadero;
			cont[1] <- cont[1] + 1;
		FinSi
	FinPara
	
	// Ordenamiento de montos de mayor a menor
	Para I<-0 Hasta P-1 Con Paso 1 Hacer
		Para J<-0 Hasta P-1 Con Paso 1 Hacer
			Si EsDeudor[J] = Falso Entonces
				Si Cobrar[J+50] > Cobrar[I]Entonces
					Si I=0 entonces
						Cobrar[I] <- Cobrar[J+50];
						Nombres[I] <- N[J];
					SiNo
						si Cobrar[J+50] < Cobrar[I-1] Entonces
							Cobrar[I] <- Cobrar[J+50];
							Nombres[I] <- N[J];
						FinSi
					FinSi
				FinSi
			SiNo
				Si Pagar[J+50] > Pagar[I]Entonces
					Si I=0 entonces
						Pagar[I] <- Pagar[J+50];
						Nombres[I+50] <- N[J];
					SiNo
						si Pagar[J+50] < Pagar[I-1] Entonces
							Pagar[I] <- Pagar[J+50];
							Nombres[I+50] <- N[J];
						FinSi
					FinSi
				FinSi
			FinSi
		FinPara
	FinPara
	
	// Muestra de información analizada
	Escribir "Si consideramos un gasto promedio de $", Promedio, ":";
	Escribir "—————— Con deuda ———————";
	Para I<-0 Hasta cont[1]-1 Con Paso 1 Hacer
		Escribir Nombres[I+50], " debe $", Pagar[I];
	FinPara
	Escribir "—— Con dinero a favor ——";
	Para I<-0 Hasta cont[0]-1 Con Paso 1 Hacer
		Escribir Nombres[I], " tiene $", Cobrar[I], " a favor.";
	FinPara
	
	Escribir "——————— Entonces ———————";
	I<-0;
	J<-0;
	Repetir // Analisis de saldo de deuda pagando a no deudores
		Si Cobrar[I] < Pagar[J] Entonces
			Escribir Nombres[J+50], " le debe $", Cobrar[I], " a ", Nombres[I];
			Pagar[J] <- Pagar[J]-Cobrar[I];
			I <- I+1;
		SiNo
			Escribir Nombres[J+50], " le debe $", Pagar[J], " a ", Nombres[I];
			Cobrar[I] <- Cobrar[I]-Pagar[J];
			J <- J+1;
			Si Cobrar[I]=0 Entonces
				I <- I+1;
			FinSi
		FinSi
	Hasta que I = cont[0]
FinSubProceso

Proceso AnalisisDeCostos
	Definir Carga, Nombres como cadena;
	Definir Personas, Aportes, AporteTotal, Promedio Como Real;
	Definir I Como Entero;
	Dimension Nombres[50], Aportes[50];
	
	// Inicializado
	AporteTotal <- 0;
	
	// Comienzo del programa - Carga de cantidad de personas
	Escribir "¡Hola! :)";
	Escribir "¿Tuviste una reunion y varios aportaron para la comida?";
	Escribir "Permitime ayudarte con las cuentas.";
	Escribir "";
	Escribir "¿Cuantas personas aportaron?";
	Repetir
		Leer Carga;
		Si (NumeroValido(Carga,Falso) = Falso) o (ConvertirANumero(Carga) > 50) Entonces
			Escribir "Lo que ingresaste no es válido. Probá de nuevo.";
		sino
			Personas <- ConvertirANumero(Carga);
			Si Personas < 2 Entonces
				Escribir "Con esa cantidad de personas no tendría sentido hacer algo, no? Vamos de nuevo.";
			FinSi
		FinSi
	Hasta Que NumeroValido(Carga,Falso) y Personas < 51 y Personas > 1
	
	// Carga de nombres y montos de aporte
	Para I<-0 Hasta Personas-1 Con Paso 1 Hacer
		Borrar Pantalla;
		Escribir "• Perfecto. ", Personas, " personas. Veamos quiénes fueron y cuánto aportaron.";
		Escribir "Ingresá el nombre de la ", I+1, "º persona: ";
		Leer Nombres[I];
		Escribir "Ingresá su aporte (cuánto gastó en pesos) separando los centavos con un punto: ";
		Repetir
			Leer Carga;
			Si NumeroValido(Carga,Verdadero) = Falso Entonces
				Escribir "Lo que ingresaste no es válido. Probá de nuevo.";
			sino
				Aportes[I] <- ConvertirANumero(Carga);
			FinSi
		Hasta Que NumeroValido(Carga,Verdadero)
	FinPara
	
	// Calculo de promedio del total aportado
	Para I<-0 Hasta Personas-1 Con Paso 1 Hacer
		AporteTotal <- AporteTotal + Aportes[I];
	FinPara
	Promedio <- AporteTotal / Personas;
	
	// Muestra de información útil
	Borrar Pantalla;
	Escribir "¡Excelente! Entonces de todo esto podemos decir:";
	Escribir "";
	Escribir "•• ¿Cuánto se aportó en total? ••";
	Escribir "——> $", AporteTotal;
	Escribir "";
	Escribir "•• ¿Quién aportó más? ••";
	MayorAMenorAporte(Nombres, Aportes, Personas);
	Escribir "";
	Escribir "•• ¿Quién debe a quien? ••";
	aQuienPagar(Nombres, Aportes, Personas, Promedio);
FinProceso
