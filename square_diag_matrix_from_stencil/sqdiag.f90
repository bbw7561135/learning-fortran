program test
    implicit none

    interface
        subroutine print_matrix(a, f, nm)
            implicit none
            real(8), dimension(:, :), intent(in) :: a
            real(4), intent(in) :: f
            character, intent(in) :: nm
        end subroutine
    end interface

    integer(4) :: i
    real(8), dimension(9) :: a
    real(8), dimension(3, 3) :: b
    character(len=16) :: fmtstr
    fmtstr = '(3(2X, F3.1))'
    a = 0
    do i=1, 9, 4
        a(i) = 1
    end do
    b = reshape(a, [3, 3])
    print fmtstr, a
    print fmtstr, b
    ! call print_matrix(a, 6.2, 'a')
end program


subroutine print_matrix(a, f, nm)
    implicit none
    real(8), dimension(:, :), intent(in) :: a
    real(4), intent(in) :: f
    character, intent(in) :: nm
    integer(4) :: cols, rows, i, j
    character(len=32) :: fmtstr
    cols = size(a, 1)
    rows = size(a, 2)
    print *, cols, rows
    write(fmtstr, '(A, I4, A, F4.1, A)') '(', cols, '(3X, F', f, '))'
    ! print fmtstr, a
    write(*, *) nm, ' = '
    do i=1, rows 
        write(*, fmtstr, advance='no') (a(i, j), j=1, cols)
        write(*, *) ''
    end do
end subroutine


function sqdiag(n, s, err)
    implicit none
    ! dimension of returned sqdiag
    integer(4), intent(in) :: n
    ! stencil vector
    real(8), dimension(:), intent(in) :: s
    ! errornumber
    integer(4), intent(inout) :: err

    ! result
    real(8), dimension(n, n) :: sqdiag
    real(8), dimension(n*n) :: tmp
    ! size of s, middle index
    integer(4) :: l, m
    ! loop indices
    integer(4) :: i, j, k

    sqdiag = 0
    l = size(s)
    ! checking conditions
    if (n < l/2 .or. mod(l, 2) == 0) then
        err = -1
        return
    end if
    ! if s contains one element, return diag(s(1))
    if (l == 1) then
        sqdiag = reshape([(s(1), i=1, n*n ,n+1)], [n, n])
        return
    end if

    tmp = 0
    m = (l-1) / 2
    k = 0
    do i = 1, n*m, n
    ! last i: n*(m-1)+1
        do j = 0, m+k
            tmp(i+j) = s(m+1-j+k)
            tmp(size(tmp)-(i-1)-j) = s(m+1+j-k)
            ! last iter: tmp(n*n - (n*(m-1)+1-1)) = tmp(n*(n-m+1))
        end do
        k = k + 1
    end do
    do i = n*m+1, n*(n-m), n+1
        tmp(i:i+l) = s
    end do

    sqdiag = reshape(tmp, [n, n])
end function sqdiag
