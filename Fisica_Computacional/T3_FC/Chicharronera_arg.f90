PROGRAM Chicharronera
IMPLICIT NONE 
CHARACTER*80 :: arga
real(8) :: x(6)
COMPLEX :: a, b, c, det, x_1, x_2, res_1, res_2
CHARACTER(LEN=1) :: temp_1, temp_2

!Es lo mismo solo cambio la parte donde se ingresan los coeficientes 
!con lo visto en clase

! Este programa recibe como parametros 3 nùmeros complejos de precisiòn
!sencilla que representan los coeficientes de una ecuaciòn cuadràtica,
! tambien se usan otras variables del mismo tipo y precisiòn como auxiliares
! para presentar las raìces de dicho polinomio y su comprobaciòn

CALL getarg( 1, arga )
READ (arga, *) x(1)

CALL getarg( 2, arga )
READ (arga, *) x(2)

CALL getarg( 3, arga )
READ (arga, *) x(3)

CALL getarg( 4, arga )
READ (arga, *) x(4)

CALL getarg( 5, arga )
READ (arga, *) x(5)

CALL getarg(6, arga )
READ (arga, *) x(6)

a = cmplx(x(1), x(2))
b = cmplx(x(3), x(4))
c = cmplx(x(5), x(6))

det = b**2 - 4*a*c !variable auxiliar

IF ((a /= 0) ) THEN !sirve para comprobar que sea una ecuaciòn de segundo grado
	
	x_1 = (-b + sqrt(det)) / (2*a) !raìz 1

	x_2 = (-b - sqrt(det)) / (2*a) !raiz 2

	WRITE (*,*) "Las raìces del polinomio"
	WRITE (*,*) a, "x**2 +", b, "x +", c
	WRITE (*,*) "son:"
	WRITE (*,*) "------------------------------------------------"
	WRITE (*,*) x_1, x_2
	WRITE (*,*) "¿Desea comprobarlo?(Y/N)"
	READ (*,*) temp_2
		
	IF (temp_2 == "Y" .OR. temp_2 == "y") THEN !comprobaciòn de resultados

		WRITE (*,*) "x_1: ", x_1
		WRITE (*,*) "---------------------------------------"
		WRITE (*,*) a, "x_1**2", "+", b, "x_1", "+", c
		WRITE (*,*) "="
		res_1 = a*x_1**2 + b*x_1 + c !variable auxiliar
		WRITE (*,*) res_1
		WRITE (*,*) "__________________________________________________________"
		WRITE (*,*) "x_2: ", x_2
		WRITE (*,*) "---------------------------------------"
		WRITE (*,*) a, "x_2**2 +", b, "x_2 +", c
		WRITE (*,*) "="
		res_2 = a*x_2**2 + b*x_2 + c !variable auxiliar
		WRITE (*,*) res_2
			
	ELSE

	END IF

ELSE
	WRITE (*,*) "No es una ecuaciòn de segundo grado"

END IF


! Este programa recibe como parametros 3 nùmeros complejos de precisiòn
!sencilla que representan los coeficientes de una ecuaciòn cuadràtica,
! tambien se usan otras variables del mismo tipo y precisiòn como auxiliares
! para presentar las raìces de dicho polinomio y su comprobaciòn

!las variable temp1 se utiliza para mantener el programa en ejecuciòn por el 
!tiempo que se requiera y temp2 para la comprobaciòn

temp_1 = "Y"


DO WHILE (temp_1 == "Y" .OR. temp_1 == "y") !esto mantiene el programa hasta
!que temp1 sea diferente de Y o y

	WRITE (*,*) "Ingresa el coeficiente cuadràtico. (ejem. 1 + 4i como (1, 4))"
	READ (*,*) a
	
	
	WRITE (*,*) "Ingrese el coeficiente lineal"
	READ (*,*) b

	WRITE (*,*) "Ingrese el tèrmino independiente"
	READ (*,*) c

	det = b**2 - 4*a*c !variable auxiliar

	IF ((a /= 0) ) THEN !sirve para comprobar que sea una ecuaciòn de segundo grado
	
		x_1 = (-b + sqrt(det)) / (2*a) !raìz 1

		x_2 = (-b - sqrt(det)) / (2*a) !raiz 2

		WRITE (*,*) "Las raìces del polinomio"
		WRITE (*,*) a, "x**2 +", b, "x +", c
		WRITE (*,*) "son:"
		WRITE (*,*) "------------------------------------------------"
		WRITE (*,*) x_1, x_2
		WRITE (*,*) "¿Desea comprobarlo?(Y/N)"
		READ (*,*) temp_2
		
		IF (temp_2 == "Y" .OR. temp_2 == "y") THEN !comprobaciòn de resultados

			WRITE (*,*) "x_1: ", x_1
			WRITE (*,*) "---------------------------------------"
			WRITE (*,*) a, "x_1**2", "+", b, "x_1", "+", c
			WRITE (*,*) "="
			res_1 = a*x_1**2 + b*x_1 + c !variable auxiliar
			WRITE (*,*) res_1
			WRITE (*,*) "__________________________________________________________"
			WRITE (*,*) "x_2: ", x_2
			WRITE (*,*) "---------------------------------------"
			WRITE (*,*) a, "x_2**2 +", b, "x_2 +", c
			WRITE (*,*) "="
			res_2 = a*x_2**2 + b*x_2 + c !variable auxiliar
			WRITE (*,*) res_2
			
		ELSE

		END IF

	ELSE
		WRITE (*,*) "No es una ecuaciòn de segundo grado"

	END IF
	
	WRITE (*,*) "¿Quiere intentarlo de nuevo? (Y/N)"
	READ (*,*) temp_1

END DO

WRITE (*,*) "Bye" !fin del programa

END PROGRAM Chicharronera
