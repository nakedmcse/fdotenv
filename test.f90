! Fortran dotenv unit tests
program test
    use fdotenv
    implicit none

    ! Tests
    ! TODO: Call tests

    contains
        subroutine assert(condition, message)
            logical :: condition
            character(len=*) :: message
            if (.not.(condition)) then
                print *, "Assertion failed: ", message
                error stop
            end if
        end subroutine assert

        ! TODO: Implement tests
end program test