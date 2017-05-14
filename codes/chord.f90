real(8) function f(x)
    implicit none
    real(8) :: x
    f = 2 * cos(x) - x**4 + 2
end function f

module CHORD
contains
    subroutine DD(f, xa, xb, eps, xk, i)
        implicit none
        real(8) :: f
        real(8) :: xa, xb, eps, x0, x1, x2, xk
        integer, parameter :: imax = 50
        real(8), parameter :: m1 = 5.683, mm1 = 15.495
        integer :: i

        x0 = xb
        x1 = xa
        i = 1
        x2 = x1 - (f(x1) * (x1 - x0)) / (f(x1) - f(x0))
        print *, 'x2 = ', x2, ' f(x2) = ', f(x2)

        do while (((mm1 - m1) / m1 * abs(x2 - x1) > eps) .and. (i <= imax))
            x1 = x2
            i = i + 1
            x2 = x1 - (f(x1) * (x1 - x0)) / (f(x1) - f(x0))
            print *, 'x2 = ', x2, ' f(x2) = ', f(x2)
        end do
        xk = x2
    end subroutine DD
end module CHORD

program lab_1_MOD_v2
    use CHORD
    implicit none
    real(8) :: xa, xb, eps, xk
    integer :: i

    interface
        real(8) function f(x)
            real(8) :: x
        end function f
    end interface

    print *, 'enter <xa>: '
    read(*,*) xa
    print *, 'enter <xb>: '
    read(*,*) xb
    print *, 'enter <eps>: '
    read(*,*) eps

    call DD(f, xa, xb, eps, xk, i)

    print *
    print *,'results:'
    print *,'xk = ', xk
    print *,'f(xk) = ', f(xk)
    print *,'i = ', i

end program lab_1_MOD_v2