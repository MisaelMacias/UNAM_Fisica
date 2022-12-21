program dat
implicit none
integer :: i,j,k
real, dimension(10000000) :: x,z,y

call random_number(x) !genera numeros aleatorios entre 0 y 1

do i=1 , 10000000, 1 !mueve los numeros aleatorios entre -1 y 1
	y(i) = 2*x(i)-1
end do


open(1,file="datos.dat", status="unknown") !carga los datos en un archivo

do i=1, 10000000, 1
	write(1,*)y(i)
end do
close(1)

end program dat
