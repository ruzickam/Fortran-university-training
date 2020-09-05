program tenchars

implicit none

integer         :: i
!-------------------------------

do i = 1, 10
  write(*,'(A)',advance='no') 'A'
end do

write(*,*)


end program