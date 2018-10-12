program prog2
    implicit none
    integer :: i
    logical :: isPerfect
    print *, 'Perfekte Zahlen in [1, 10000]'
    do i = 1, 10000
        if (isPerfect(i)) then
            print *, i
        end if
    end do
end program

logical function isPerfect(x)
    implicit none
    integer :: i, x, m
    m = 0
    isPerfect = .false.
    do i = 1, x/2
        if (mod(x, i) == 0) m = m + i
    end do
    if (m == x) isPerfect = .true.
end function isPerfect
