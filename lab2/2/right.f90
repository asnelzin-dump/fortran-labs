module right
    contains
    subroutine IntRR(a, b, m, s)
        implicit none
        real(8) :: a, b, s, h, xi
        integer :: m, i
        real(8) :: f

        h = (b - a) / m
        s = 0
        do i = 1, m
            xi = a + i * h
            s = s + f(xi)
        end do
        s = s * h
    end subroutine IntRR
end module
