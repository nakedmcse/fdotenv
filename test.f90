! Fortran dotenv unit tests
program test
    use fdotenv
    implicit none

    ! Tests
    call test_load()
    call test_string()

    contains
        subroutine assert(condition, message)
            logical :: condition
            character(len=*) :: message
            if (.not.(condition)) then
                print *, "Assertion failed: ", message
                error stop
            end if
        end subroutine assert

        subroutine test_load()
            print *,"Test Load TO BE IMPLEMENTED"
        end subroutine test_load

        subroutine test_string()
            print *,"Test String TO BE IMPLEMENTED"
        end subroutine test_string

        ! TODO: Implement tests
end program test