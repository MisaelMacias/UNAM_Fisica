INTEGER, PARAMETER  :: N=1000
REAL, DIMENSION(N)  :: R,POTEN
REAL, DIMENSION(4)  :: EPSI,SIGMA,DR
!  como hacer formatos de salida
!
character(17)       :: cien='(6X,E9.3,6X,E9.3)' !x espacios, SX.XXXESX E9.3 
character(92)       :: cincuenta="('#',3X,'EPSILON = ',E9.3,4X,'SIGMA =' ,E9.3,4X,'DR = ',E9.3,5X,'NUMERO DE PUNTOS N = ',I7)"
! formato i7  xxxxxxx
! Definiciones de los datos epsilon y sigma
! 1 es Ne, 2 es Ar, 3 es Kr 4 es Xe
! como se inicializan arreglos
!
EPSI =(/0.0031,0.0104,0.014,0.02/) !EPSI(1)=0.0031, EPSI(2)=0.0104, EPSI=0.014,...
SIGMA =(/2.64,3.4,3.65,3.98/)      !SIGMA(1)=2.64 ...
!
! como se abren archivos para escritura/lectura
!
OPEN(1,FILE='0-11.mas-Ne.dat',STATUS='UNKNOWN') !old,new, unknown
OPEN(2,FILE='0-11.mas-Ar.dat',STATUS='UNKNOWN')
OPEN(3,FILE='0-11.mas-Kr.dat',STATUS='UNKNOWN')
OPEN(4,FILE='0-11.mas-Xe.dat',STATUS='UNKNOWN')
!
DO I=1, 4 !Contador para los gases
   DR(I) = 2.1*SIGMA(I)/REAL(N)
   WRITE(I,cincuenta)EPSI(I),SIGMA(I),DR(I),N
   DO J=1, N !Contador para los puntos
       R(J) = 0.9*SIGMA(I) + REAL(J)*DR(I)
       POTEN(J) = 4.0*EPSI(I)*((SIGMA(I)/R(J))**12 -(SIGMA(I)/R(J))**6)
       WRITE(I,cien)R(J),POTEN(J)
   END DO
   CLOSE(I)
END DO
! de una vez se escribe el script de gnuplot
!
open(5,FILE='script.gnp',status='unknown')
write(5,*) "set yrange[-0.025:0.02]"
write(5,*) "plot '0-11.mas-Ne.dat' u 1:2 w l, '0-11.mas-Ar.dat' u 1:2 w l, '0-11.mas-Kr.dat' u 1:2 w l,'0-11.mas-Xe.dat' u 1:2 w l"
! cierran los archivos
close (5)
!50 FORMAT('#',3X,'EPSILON = ',E9.3,4X,'SIGMA =' ,E9.3,4X,'DR = ',E9.3,5X,'NUMERO DE PUNTOS N = ',I7)
!100 FORMAT(6X,E9.3,6X,E9.3)
END PROGRAM
