PROGRAM Chicharronera
IMPLICIT NONE 
REAL :: a, b, c, det, x_1, x_2
COMPLEX :: x_1_cmplx, x_2_cmplx

! Este programa recibe como parametros 3 nùmeros reales de precisiòn
!sencilla que representan los coeficientes de una ecuaciòn cuadràtica,
! tambien se usan otras variables del mismo tipo y precisiòn como auxiliares
! para presentar las raìces de dicho polinomio

WRITE (*,*) "Ingresa el coeficiente cuadràtico"
READ (*,*) a
		
WRITE (*,*) "Ingrese el coeficiente lineal"
READ (*,*) b

WRITE (*,*) "Ingrese el tèrmino independiente"
READ (*,*) c

det = b**2 - 4*a*c !variable auxiliar

IF ((a /= 0) ) THEN !sirve para comprobar que sea una ecuaciòn de segundo grado

IF (det > 0) THEN!raices reales
			
x_1 = (-b + sqrt(det)) / (2*a) !raìz 1
x_2 = (-b - sqrt(det)) / (2*a) !raiz 2	

WRITE (*,*) "Las raìces del polinomio"
WRITE (*,*) a, "x**2 +", b, "x +", c
WRITE (*,*) "son:"
WRITE (*,*) "------------------------------------------------"
WRITE (*,*) x_1, x_2	

ELSE IF (det < 0) THEN!raices complejas

x_1_cmplx = cmplx(-b/(2*a), sqrt(-det) / (2*a)) !raìz 1
x_2_cmplx = cmplx(-b/(2*a),-sqrt(-det)/(2*a)) !raiz 2

WRITE (*,*) "Las raìces del polinomio"
WRITE (*,*) a, "x**2 +", b, "x +", c
WRITE (*,*) "son:"
WRITE (*,*) "------------------------------------------------"
WRITE (*,*) x_1_cmplx, x_2_cmplx
	
ELSE IF (det == 0) THEN!raiz doble

x_1 = -b / (2*a)	
	
WRITE (*,*) "La raíz es doble"
WRITE (*,*) "La raìz del polinomio"
WRITE (*,*) a, "x**2 +", b, "x +", c
WRITE (*,*) "es:"
WRITE (*,*) "------------------------------------------------"
WRITE (*,*) x_1
		
END IF

ELSE IF (a == 0 .AND. b /= 0) THEN!lineal

x_1 = -c/b

WRITE (*,*) "La ecuación es lineal y la raíz es:",  x_1

ELSE IF (a == 0 .AND. b == 0) THEN!sin solución
WRITE (*,*) "No hay solución"

END IF

END PROGRAM Chicharronera
