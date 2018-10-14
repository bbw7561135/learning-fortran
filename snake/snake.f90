module main_mod
contains
function main(fs, ms) result(res)
    implicit none
    integer(4), intent(in) :: fs, ms    ! field size, max size of snake
    integer(4) :: res       ! result of game: number of eaten food

    character(len=16) :: fmtstr
    character, dimension(fs, fs) :: field
    integer(4), dimension(ms, 2) :: snake
    integer(4), dimension(2) :: step
    integer(4) :: cs                ! current size
    real(8), dimension(2) :: r      ! random number variable
    integer(4) :: i
    character :: input

    write(fmtstr, '(A, I4, A)') '(', fs, '(1X, A))'

    res = 0
    field = ' '
    snake = 0
    cs = 2
    call random_number(r)
    snake(2, :) = floor(fs*r+1)
    call random_number(r)
    step = -1 + floor(3*r)
    snake(1, 1:2) = step + snake(2, :)
    do i=1, 2
        if (snake(1, i) < 1) then
            snake(1, i) = fs
        else if (snake(1, i) > fs) then
            snake(1, i) = 1
        end if
    end do
    field(snake(1:cs, 1), snake(1:cs, 2)) = 'x'
    write(*, fmtstr) field


    do
        100 read *, input
        select case(input)
            case('h')
                step = [-1, 0]
            case('j')
                step = [0, -1]
            case('k')
                step = [0, 1]
            case('l')
                step = [1, 0]
            case('q')
                return
            case default
                goto 100
        end select
    end do

end function main
end module main_mod

program prog
    use main_mod, only: main
    implicit none
    integer(4) :: steps
    character :: input
    ! real(4) :: r(5, 5)
    ! call random_number(r)
    ! print *, r
    ! steps = main(10, 5)
    do
        read *, input
        print *, input
    end do
end program
