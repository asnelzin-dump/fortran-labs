real(8) function f(x)
    implicit none
    real(8) :: x
    f = 2 * cos(x) - x**4 + 2
end function f

program integral_parabl
    use parabl
    implicit none 
    real(8) :: a, b, eps, s1, s2
    integer :: m
    integer, parameter :: k = 4

    write (*, *) 'Enter a, b, m, eps'
    read (*, *) a, b, m, eps

    call intpar(a, b, m, s1)

    do 
        m = m * 2
        call intpar(a, b, m, s2)
        write(*, *) 'm = ', m, ' s2 = ', s2
        if (abs(s2 - s1) / (2**k - 1) < eps) exit
        s1 = s2
    end do

    write(*, *) 
    write(*, *) '========================'
    write(*, *) 'Integral=',s2
end program
