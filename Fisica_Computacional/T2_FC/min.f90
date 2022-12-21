program min

character(10) :: a(205,26)
integer :: i,j
real :: x(205), y(205)

do i = 1, 205
	read (*,*) (a(i,j), j= 1, 26)
end do


do i = 1, 205
	read(a(i,13), *) x(i)
	read(a(i,26), *) y(i)
end do

print*, "b = ", (sum(y)*sum(x*x)-sum(x)*sum(x*y))/(205*sum(x*x)-sum(x)*sum(x))
print*, "m = ", (205*sum(x*y)-sum(x)*sum(y))/(205*sum(x*x)-sum(x)*sum(x))

end program min
