program perf_test

implicit none

integer                         :: i,j,k,l,m,num_iteration,n
double precision, allocatable   :: a(:,:)
double precision, allocatable   :: b(:,:)
double precision, allocatable   :: c(:,:)
double precision                :: start,finish,calc_speed,perf,operation_num,en,repetitions
integer*4                       :: timearray(3)
real                            :: rand
!-------------------------------

n = 0
num_iteration = 0


call itime(timearray)
i = rand ( timearray(1)+timearray(2)+timearray(3) ) 

do i = 1, n
  do j = 1, n
    a(i,j) = int(rand(0)*(20+1+10))-10
    b(i,j) = int(rand(0)*(20+1+10))-10
  end do
end do




do m = 1, 20

n = n + 50 ! matrix size (every 50)

allocate(a(n,n), b(n,n), c(n,n))

if ( n .eq. 50 ) then
  num_iteration = 50000
else if ( n .eq. 100 ) then
  num_iteration = 500
else if ( n .lt. 510 ) then
  num_iteration = 50
else
  num_iteration = 1
end if

!number of operations
en = n
repetitions = num_iteration
operation_num = 2 * (en**3) * repetitions

!matrix multiplication:______________________
call cpu_time(start)
do l = 1, num_iteration

do i = 1, n

  
  do j = 1, n
    c(i,j) = 0.0d0
        
      do k = 1, n
        c(i,j)  = c(i,j) + a(i,k) * b(k,j) 
      end do
         

  end do
  
end do

end do
call cpu_time(finish)
!matrix multiplication:______________________


calc_speed = finish - start
perf = (operation_num/calc_speed)/(10**6)

write(*,'(A)',advance='no') 'Matrix size:  '
write(*,'(I4)',advance='no') n
write(*,'(A)',advance='no') '  Number of repetitions: '
write(*,'(I5)',advance='no') num_iteration
write(*,'(A)',advance='no') '  Number of iterations: '
write(*,'(F14.1)',advance='no') operation_num
write(*,'(A)',advance='no') '  Processor performance (MFLOPS): '
write(*,'(F6.1)') perf

deallocate(a,b,c)

end do



end program