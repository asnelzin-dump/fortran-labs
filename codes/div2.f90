real(8) function f(x)
    implicit none
    real(8) :: x
    f = 2 * cos(x) - x**4 + 2
end function f

module DIV_2
contains
    subroutine DD(f, xa, xb, eps, xk, i)
        implicit none
        real(8) :: f
        real(8) :: xa, xb, eps, xc, xk
        integer, parameter :: imax=50
        integer :: i

        i = 0
        do while (((xb - xa) > eps) .and. (i <= imax))
            i = i + 1
            xc = (xa + xb) / 2
            print *, 'xc = ', xc, ' f(xc) = ', f(xc)
            if (f(xa) * f(xc) > 0) then
                xa = xc
            else
                xb = xc
            end if
        end do
        xk = xa
    end subroutine DD
end module DIV_2

program lab_1_MOD_v0
    use DIV_2
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

end program lab_1_MOD_v0