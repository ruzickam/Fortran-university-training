program triangle

implicit none

integer         :: i
integer         :: j
integer         :: k
!-------------------------------
k = 1

do i = 1, 10 !line

  do j = 1, k
    write(*,'(A)',advance='no') 'A'
  end do
  
  k = k + 1
  write(*,*)
end do



end program