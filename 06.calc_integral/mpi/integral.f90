program integral

  use mpi

  implicit none
  integer                 :: my_mpi_error
  integer                 :: cpu_id,ncpu  
  integer                 :: n,nd
  integer                 :: istart,istop,i  
  double precision        :: d,v,vi,y,x 
  !---------------------------------------------------

  n = 2000000000  
  d = 1.0/n
  
  ! initialize MPI
  call mpi_init(my_mpi_error)
  
  ! get number of CPUs and ID of current CPU
  call mpi_comm_size(MPI_COMM_WORLD,ncpu,my_mpi_error)
  call mpi_comm_rank(MPI_COMM_WORLD,cpu_id,my_mpi_error)
  
  if( cpu_id .eq. 0 ) then
    ! only master process will print
    write(*,*) 'Number of CPUs = ',ncpu
  end if
  
  ! each process will calculate n-th part of interval
  nd = n / ncpu 
  istart=nd*cpu_id + 1
  if( cpu_id .ne. ncpu -1 ) then
    istop=nd*(cpu_id+1) 
  else
    istop=n     
  end if
  
  ! debug
  write(*,*) 'CPU =',cpu_id,' Interval =', istart, '-', istop
  
  vi = 0.0
  do i=istart,istop
    x = (i-0.5)*d
    y = 4.0/(1.0+x**2)
    vi = vi + y*d
  end do
  
  ! collect intermediate results from all processes
  v = 0.0
  call mpi_reduce(vi,v,1,MPI_REAL8,MPI_SUM,0,MPI_COMM_WORLD,my_mpi_error)
  if( cpu_id .eq. 0 ) then
    ! only master process will print
    write(*,*) 'integral = ',v
  end if  
    
  ! finalize MPI
  call mpi_finalize(my_mpi_error)  
 
end program integral
