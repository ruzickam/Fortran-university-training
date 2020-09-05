program calc_speed

implicit none

integer                         :: n,i,j,k,summ
integer, allocatable            :: a(:,:)
integer, allocatable            :: b(:,:)
integer, allocatable            :: c(:,:)
double precision                :: operations_number
double precision                :: start,finish,speed,perf
integer*4                       :: timearray(3)
real                            :: rand
!-------------------------------

n = 10
summ = 0


allocate(a(n,n), b(n,n), c(n,n))


call itime(timearray)
i = rand ( timearray(1)+timearray(2)+timearray(3) ) 

do i = 1, n
  do j = 1, n
    a(i,j) = int(rand(0)*(20+1+10))-10
    b(i,j) = int(rand(0)*(20+1+10))-10
  end do
end do

!num of operations
operations_number = 2 * (n**3)


!matrix multiplication:______________________
call cpu_time(start)

do i = 1, n

  
  do j = 1, n

        
      do k = 1, n
        summ = summ + ( a(i,k) * b(k,j) )

      end do
     
    c(i,j) = summ      
    summ = 0
    

  end do
  
end do
call cpu_time(finish)
!nasobeni matic:______________________

speed = finish - start
perf = (operations_number / (10**6))/speed

!call print_matrix('a')
!call print_matrix('b')
!call print_matrix('c')

write(*,'(A)',advance='no') 'Processor performance during matrix calc (MFLOPS): '
write(*,'(F4.3)') perf
write(*,*)


deallocate(a,b,c)

contains

subroutine print_matrix(matrix_name)
implicit none
character(len=1)       :: matrix_name

write(*,*) 'Matice ', matrix_name, ': '
do i = 1, n
  do j = 1, n
    if ( matrix_name .eq. 'a' ) then
      write(*,'(I4)',advance='no') a(i,j)
    else if ( matrix_name .eq. 'b' ) then
      write(*,'(I4)',advance='no') b(i,j)
    else
      write(*,'(I6)',advance='no') c(i,j)
    end if
  end do
  write(*,*)
end do
write(*,*)

end subroutine print_matrix


end program