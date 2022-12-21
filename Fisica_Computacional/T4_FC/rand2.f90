program rand2
implicit none
integer :: i,n,seed
real :: x

call initrandomseed()

do i= 1, 100, 1
	call random_number(x)
	write(*,*) x
end do

contains

subroutine initrandomseed()
	integer :: i,n,clock
	integer, dimension(:), allocatable :: seed
	
	call random_seed(size=n)
	allocate(seed(n))
	call system_clock(count=clock)
	seed = 1
	call random_Seed(put=seed)
	deallocate(seed)	
end subroutine initrandomseed

end program rand2
