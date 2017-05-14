module parabl
    contains
    subroutine intpar(a, b, m, s)
        implicit none
        real(8) :: a, b, s, h, t1, t2, t3
        integer :: m, i
        real(8) :: f
        
        h = (b - a) / (2 * m)
        t1 = f(a) + f(b)
        
        t2 = 0
        do i = 1, m
            t2 = t2 + f(a + (2 * i - 1) * h)
        end do
        
        t2 = 4 * t2
        t3 = 0
        do i = 1, (m - 1)
            t3 = t3 + f(a + (2 * i) * h)
        end do

        t3 = 2 * t3
        s = (t1 + t2 + t3) * (h / 6)
    end subroutine
end module
