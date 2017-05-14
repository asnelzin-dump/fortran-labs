real(8) function f(x)
    implicit none
    real(8) :: x
    f = 2 * cos(x) - x**4 + 2
end function f

real(8) function f1(x)
    implicit none
    real(8) :: x
    f1 = -4 * x**3 - sin(x)
end function f1

module NEWTON
contains
    subroutine DD(f, f1, xa, xb, eps, xk, i)
        implicit none
        real(8) :: f, f1
        real(8) :: xa, xb, eps, x0, x1, xk
        integer, parameter :: imax = 50
        real(8), parameter :: m1 = 5.683, mm2 = 27.141
        integer :: i

        x0 = xb
        
        i = 1
        x1 = x0 - f(x0) / f1(x0)
        print *, 'x1 = ', x1, '  f(x1) = ', f(x1)

        do while (((mm2 / (2 * m1) * (x1 - x0)**2) > eps) .and. (i <= imax))
            x0 = x1
            i = i + 1
            x1 = x0 - f(x0) / f1(x0)
            print *, 'x1 = ', x1, ' f(x1)= ', f(x1)
        end do
        xk = x1
    end subroutine DD
end module NEWTON

program lab_1_MOD_v1
    use NEWTON
    implicit none
    real(8) :: xa, xb, eps, xk
    integer :: i

    interface
        real(8) function f(x)
            real(8) :: x
        end function f
    end interface

    interface
        real(8) function f1(x)
            real(8) :: x
        end function f1
    end interface

    print *, 'enter <xa>: '
    read(*,*) xa
    print *, 'enter <xb>: '
    read(*,*) xb
    print *, 'enter <eps>: '
    read(*,*) eps

    call DD(f, f1, xa, xb, eps, xk, i)

    print *
    print *,'results:'
    print *,'xk = ', xk
    print *,'f(xk) = ', f(xk)
    print *,'i = ', i

end program lab_1_MOD_v1