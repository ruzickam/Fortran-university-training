program square

implicit none

integer         :: i
integer         :: j
!-------------------------------

do i = 1, 10 !line

  do j = 1, 10 !char
    write(*,'(A)',advance='no') 'A'
  end do

  write(*,*)
end do



end program