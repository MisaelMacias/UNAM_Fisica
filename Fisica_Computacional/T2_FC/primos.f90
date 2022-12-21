program primos
implicit none
integer :: n, i, k, cuenta
!programa que muestra los numeros primos anteriores al ingresado

write (*,*) "ingresa un entero mayor a 2"
read (*,*) n

if (n .GT. 2) then 
	
	do i = n, 2, -1
		cuenta = 0
		do k = i, 2, -1
			if (modulo(i,k) .EQ. 0) then
				cuenta = cuenta+1
			end if
		end do
		if (cuenta .EQ. 1) then
			write (*,*) i
		end if
	end do
	
else 

	write (*,*) "no es un numero mayor a 2"

end if


end program primos
