program rep
implicit none
integer :: i, j
real, dimension(10000000) :: x

do i=1, 10000000, 1 !bucle para leer datos aleatorios de -1 a 1
  read(*,*) x(i)
end do


do i=1, 10000000, 1!bucles para encontrar los elementos que se repiten
  do j=i, 10000000, 1
    if ((x(i) .lt. x(j)+0.000001) .and. (x(i) .gt. x(j)-0.000001)) then
      write(*,*) "El elemento", x(i),"con índice ",i ,"se repite en el índice", j
    end if
  end do
end do


end program
