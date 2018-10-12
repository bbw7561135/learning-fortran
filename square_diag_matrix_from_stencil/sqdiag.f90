program test

end program


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
