PROGRAM ROTACION

IMPLICIT NONE
CHARACTER*30 :: NOM1,NOM2
INTEGER :: NTAM,NL,NA,NPIX,NEN,i
INTEGER*1, ALLOCATABLE, DIMENSION(:) :: x,y


WRITE(*,*) "Indica el nombre del archivo de imagen"
READ(*,*) NOM1
!NOM1 = "gato.bmp"

WRITE(*,*) "Indica el nombre de archivo rotado"
READ(*,*) NOM2

WRITE(*,*) "Escribe el tamaño en bytes"
READ(*,*) NTAM
!NTAM = 172854

WRITE(*,*) "Indica el largo de la imagen en pixeles"
READ(*,*) NL
!NL = 180

WRITE(*,*) "Indica el alto de imagen en pixeles"
READ(*,*) NA
!NA = 320

WRITE(*,*) "Indica el numero de bytes por pixel"
READ(*,*) NPIX
!NPIX = 3

ALLOCATE(x(NTAM),y(NTAM))

OPEN(1,FILE=NOM1,ACCESS="DIRECT",FORM="UNFORMATTED",STATUS="OLD",RECL=NTAM)!abre la imagen a rotar
READ(1,REC=1)(x(i),i=1,NTAM)
NEN = NTAM - NL*NA*NPIX

DO i = 1,NEN !copia el encabezado
 y(i)=x(i)
END DO

DO i = NEN+1,NTAM !intercambia los pixeles para rotar 180 grados
  y(i) = x(NTAM-i+NEN)
END DO

OPEN(2,FILE=NOM2,ACCESS="DIRECT",FORM="UNFORMATTED",STATUS="NEW",RECL=NTAM) !abre la imagen rotada
WRITE(2,REC=1)(y(i),i=1,NTAM)

CLOSE(1)
CLOSE(2)

END PROGRAM ROTACION
