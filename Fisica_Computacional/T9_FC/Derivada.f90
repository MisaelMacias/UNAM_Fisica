PROGRAM derivada
IMPLICIT NONE
INTEGER :: i
INTEGER, PARAMETER :: extra = SELECTED_REAL_KIND(p=24,r=1000)
REAL(extra) :: x,hh,xx,dif,ff
WRITE(*,*) "x=7.1"
WRITE(*,*) "f(x)=x**(1/2)"
WRITE(*,*) "f'(x)=1/2*sqrt(x)"

x=7.1_extra
xx= 1/(2*SQRT(x))

WRITE(*,*)"           ", "i/h", "      ", "f'(x)", "         ", "f'(x)(approx)", "      ", "|f'-sqrt(x)/sqrt(x)|"

DO i=1,24
 WRITE(*,*) i
 hh=1._extra/10._extra**i
 ff=(SQRT(x+hh)-SQRT(x))/hh !definición de derivada
 dif=ABS((ff-xx)/xx)
 PRINT 10,hh ,xx,ff, dif
10 FORMAT(2X,6F16.13)
END DO

END PROGRAM derivada
