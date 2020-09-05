program random_field

implicit none

integer                         :: n,i,j
integer, allocatable            :: a(:,:)
integer*4                       :: timearray(3)
real                            :: rand
!-------------------------------

n = 10

allocate(a(n,n))


call itime(timearray)
i = rand ( timearray(1)+timearray(2)+timearray(3) ) 

do i = 1, n
  do j = 1, n
    a(j,i) = int(rand(0)*(20+1+10))-10
  end do
end do

do i = 1, n
  do j = 1, n
    write(*,'(I4)',advance='no') a(j,i)
  end do
  write(*,*)
end do




deallocate(a)

end program