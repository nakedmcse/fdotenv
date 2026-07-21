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
            ! Given
            character(len=*), parameter :: s_simple = 'var1=one' // char(10) // 'var2=two' // char(10) // "var3='three'"
            character(len=*), parameter :: s_replacement = 'name=myname' // char(10) // 'email=${name}@email.com'
            type(fdotenv_vars) :: vars_simple, vars_replacement
            type(fdotenv_status) :: status_simple, status_replacement

            ! When
            call fdotenv_parse_string(s_simple, vars_simple, status_simple)
            call fdotenv_parse_string(s_replacement, vars_replacement, status_replacement)

            ! Then
            call assert(.not. status_simple%error, "Simple string parse should not return error status")
            call assert(vars_simple%count == 3, "Simple string parse should return 3 variables")
            print *, vars_simple%items(1)%key, vars_simple%items(2)%key, vars_simple%items(3)%key
            print *, vars_simple%items(1)%value, vars_simple%items(2)%value, vars_simple%items(3)%value
            call assert(vars_simple%items(1)%key == "var1", "Simple string parse key 1 should be var1: " // vars_simple%items(1)%key)
            call assert(vars_simple%items(1)%value == "one", "Simple string parse value 1 should be one: " // vars_simple%items(1)%value)
            call assert(vars_simple%items(2)%key == "var2", "Simple string parse key 2 should be var1: " // vars_simple%items(2)%key)
            call assert(vars_simple%items(2)%value == "two", "Simple string parse value 2 should be two: " // vars_simple%items(2)%value)
            call assert(vars_simple%items(3)%key == "var3", "Simple string parse key 3 should be var3: " // vars_simple%items(3)%key)
            call assert(vars_simple%items(3)%value == "three", "Simple string parse value 3 should be three: " // vars_simple%items(3)%value)
            call assert(vars_simple%items(3)%singleQuoted, "Simple string parse value 3 should be single quoted")

            call assert(.not. status_replacement%error, "Replacement string parse should not return error status")
            call assert(vars_replacement%count == 2, "Replacement string parse should return 2 variables")
            call assert(vars_replacement%items(1)%key == "name", "Replacement string parse key 1 should be name: " // vars_replacement%items(1)%key)
            call assert(vars_replacement%items(1)%value == "myname", "Replacement string parse value 1 should be myname: " // vars_replacement%items(1)%value)
            call assert(vars_replacement%items(2)%key == "email", "Replacement string parse key 2 should be email: " // vars_replacement%items(2)%key)
            call assert(vars_replacement%items(2)%value == "myname@email.com", "Replacement string parse value 2 should be myname: " // vars_replacement%items(2)%value)

            print *,"Test String Completed"
        end subroutine test_string

        ! TODO: Implement tests
end program test