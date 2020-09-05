program integral

  !$ use omp_lib
  implicit none
  integer                 :: i
  integer                 :: n,ncpu
  double precision        :: d,v,y,x 
  !---------------------------------------------------
  n = 2000000000
  d = 1.0/n
  v = 0.0
  
  ncpu = 1  
  !$ ncpu = omp_get_max_threads()   
  write(*,*) 'Number of threads = ',ncpu    
  
  !$omp parallel
  
  !$omp do private(i,x,y),reduction(+:v)
  do i=1,n
    x = (i-0.5)*d
    y = 4.0/(1.0+x**2)
    v = v + y*d
  end do
  !$omp end do
  
  !$omp end parallel

  write(*,*) 'integral = ',v
 
end program integral
