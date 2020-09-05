program calc_integral

implicit none

double precision :: a
double precision :: b
double precision :: n
double precision :: step
double precision :: results
integer :: nminusone
!-------------------------------

a = 0
b = 1
n = 1000
step = (b - a) / n !size of the integration step
nminusone = n - 1


!calc the integral
results = integral(a,b,n)


write(*,*) 'The result is: ', results



contains


!------functions-------
function funct(a) result(x)
implicit none

double precision :: a
double precision :: x

x = 4 / (1 + a**2)

end function funct
!------functions-------


!------functions-------
function integral(a,b,n) result(x)
implicit none

double precision :: a, b, n
double precision :: x

integer  :: i
double precision :: summ
double precision :: subtotal
double precision :: xvalue
double precision :: beginning
double precision :: ending
!-------------------------------
summ = 0
subtotal = 0
xvalue = a + step !function value at the current integration step
beginning = funct(a)
ending = funct(b)



do i = 1, nminusone !function values in integration steps
    subtotal = funct(xvalue) !function value in one particular integration step
    xvalue = xvalue + step !plus step - shift
    summ = summ + subtotal
end do

x = ((b - a) / n) * ((beginning/2)+(ending/2)+summ)


end function integral
!------functions-------



end program





