program main
    use ogpf
    implicit none

    type(gpf) :: gp
    integer, parameter :: s = 10000
    real(8), dimension(2, s) :: d
    integer :: i
    d = 0

    call gp%animation_start(1)
    call gp%axis([-2.4d0, 2.8d0, -0.1d0, 10.1d0])
    call gp%options('set grid')

    do i=2, s
        d(:, i) = next_coord(d(1, i-1), d(2, i-1))
        call gp%plot(d(1, 1:i), d(2, 1:i), 'with points')
        if (mod(i, 100)==0) print *, i
    end do

    call gp%animation_show()

contains
    function next_coord(x, y) result(res)
        real(8), intent(in) :: x, y
        real(8), dimension(2) :: res
        real(8) :: r
        call random_number(r)
        select case(floor(100*r+1))
            case(1)
                res(1) = 0
                res(2) = 0.16*y
            case(2:8)
                res(1) = 0.2*x - 0.26*y
                res(2) = 0.23*x + 0.22*y + 1.6
            case(9:15)
                res(1) = -0.15*x + 0.28*y
                res(2) = 0.26*x +0.24*y + 0.44
            case(16:100)
                res(1) = 0.85*x + 0.04*y
                res(2) = -0.04*x + 0.85*y + 1.6
        end select
    end function

end program
