program toBinary
    implicit none
    integer, parameter :: m = 255
    integer :: n
    character(8) :: bin
    do
        print *, 'enter a number between 0 and ', m, ' ...'
        read *, n
        if (n == -1) exit
        if (n < 0 .or. n > m) then
            print *, 'input not in supported range ...'
        else
            print *, n, ' = 0b', bin(n)
        end if
    end do
end program

character(8) function bin(n)
    implicit none
    integer :: n, i
    i = 8
    bin = '00000000'
    do while (n /= 0 .and. i >= 0)
        bin(i:i) = achar(mod(n, 2)+48)
        n = n / 2
        i = i - 1
    end do
end function bin
