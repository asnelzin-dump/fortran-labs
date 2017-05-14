module trap
    contains
    subroutine Inttr(a, b, m, s)
        implicit none
        real(8) :: a, b, s, h, xi, xi1
        integer :: m, i
        real(8):: f
          
        h = (b - a) / m
        s = 0
        do i = 1, m
            xi = a + (i - 1) * h
            xi1 = a + i * h
            s = s + (f(xi) + f(xi1)) / 2
        end do
        s = s * h
    end subroutine Inttr
end module
