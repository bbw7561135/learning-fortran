program prog1
    implicit none
    integer :: n
    logical :: isPerfect
    do
        print *, 'Gib eine ganze Zahl ein; "0" für Abbruch: '
        read *, n
        if (n == 0) exit
        if (isPerfect(n)) then
            print *, n, ' ist eine perfekte Zahl'
        else
            print *, n, ' ist keine perfekte Zahl'
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
