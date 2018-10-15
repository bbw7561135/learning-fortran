! TODO: read v and division value via cli or read
!       gnuplot window is not persistent... Why?
!       for now call `gnuplot -p ogpf_temp_script.gp`

program main
    use ogpf
    implicit none
    type(gpf) :: gp
    integer, parameter :: s = 100000
    integer, parameter :: v = 6
    real(8), parameter :: pi = 4 * atan(1.d0)
    real(8), dimension(2, s) :: d
    real(8), dimension(2) :: r, a
    real(8) :: phi, b
    integer :: i

    d = 0
    b = 5

    phi = 0
    do i=1,v
        d(:,i) = b * [cos(phi), sin(phi)]
        phi = phi + 2*pi / v
    end do

    call random_number(r)
    d(:,v+1) = floor(10*r+1)

    call gp%animation_start(1)
    call gp%axis([-5.d0, 5.d0, -5.d0, 5.d0])

    do i=v+2,s
        call random_number(r)
        a = d(:, floor(v*r(1))+1)
        d(:, i) = (a + d(:, i-1)) / 3
        ! if (mod(i, 10000) == 0) then
        !     call gp%plot(d(1, :i), d(2, :i), 'with points pt "."')
        !     print *, i
        ! end if
    end do

    ! call gp%animation_show()
    call gp%plot(d(1, :), d(2, :), 'with points pt "."')

end program
