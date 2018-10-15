program main
    use ogpf
    call exmp01

contains

subroutine exmp01
    type(gpf) :: gp
    integer, parameter :: n = 20
    real(wp), parameter :: pi = 4.d0 * atan(1.d0)
    real(wp) :: x(n)
    real(wp) :: y(n)
    real(wp) :: z(n)
    x = linspace(-pi, pi, n)
    y = sin(x)
    z = exp(-x**2)

    call gp%title('example')
    call gp%xlabel('x')
    call gp%ylabel('y')
    call gp%options('set grid')
    call gp%plot(x1=x, y1=y, x2=x, y2=z)
end subroutine exmp01

end program
