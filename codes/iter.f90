real(8) function f(x)
    implicit none
    real(8) :: x
    f = 2 * cos(x) - x**4 + 2
end function f

module SIMPLE_ITER
contains
    subroutine DD(f, xa, xb, eps, xk, i)
        implicit none
        real(8) :: f
        real(8) :: xa, xb, eps, x0, x1, xk
        real(8) :: alf, q
        integer, parameter :: imax = 50
        real(8), parameter :: m1 = 5.683, mm1 = 15.495
        integer :: i

        alf = 1 / mm1
        q = 1 - m1 / mm1

        x0 = (xa + xb) / 2

        i = 1
        x1 = x0 + alf * f(x0)
        print *, 'x1 = ', x1, ' f(x1) = ', f(x1)
        
        do while ((q / (1 - q) * abs(x1 - x0) > eps) .and. (i <= imax))
            x0 = x1
            i = i + 1
            x1 = x0 + alf * f(x0)
            print *, 'x1 = ', x1, ' f(x1) = ', f(x1)
        end do
        xk = x1
    end subroutine DD
end module SIMPLE_ITER

program lab_1_MOD_v4
    use SIMPLE_ITER
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

end program lab_1_MOD_v4