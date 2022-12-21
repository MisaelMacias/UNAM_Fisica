program taylor
implicit none
real, parameter :: pi = 3.14159265359
integer, parameter :: extra = selected_real_kind(p=24,r=1000)
integer :: i, j, m, k, y, z
real(extra), dimension(2000) :: fact, coss
real(extra) :: x, xx, realcos, scos = 1
!serie de taylor alrededor del 0 (serie de Mclaurin) para aproximar el coseno
!en cualquier valor gracias a la traslación se que usa


write (*,*) "¿Cual es el valor que quieres evaluar?(en radianes)"
read (*,*) x

y = floor(x/pi)
z = modulo(y,2)

if (abs(x) .GT. 2*pi) then !traslación para evitar que algún dato de la serie explote
	xx = x - ((y-z)*pi)

else if (abs(x) .LE.2* pi) then
		xx = x
end if

fact(1) = 1

do i = 2, 2000, 1 !calculo de los factoriales necesarios
	fact(i) = fact(i-1) * (i)
end  do

do i = 1, 1000, 1 !elementos de la serie
	
	coss(i) = ((-1)**i) * ((xx**(2*i))/(fact(2*i)))
end do

do i = 1, 1000, 1 !suma de todos los elementos
	scos = scos + coss(i)
end do

realcos = cos(x) !coseno "real"
	
write (*,*)"El valor aproximado es : ", scos
write (*,*) "El valor 'real' es : ", realcos

end program taylor
