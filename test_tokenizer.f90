! Fortran dotenv tokenizer unit tests
program test_tokenizer
    use fdotenv_tokenizer
    implicit none

    ! Tests
    call test_tokenizer_normal()
    call test_tokenizer_error()

contains
    subroutine assert(condition, message)
        logical :: condition
        character(len=*) :: message
        if (.not.(condition)) then
            print *, "Assertion failed: ", message
            error stop
        end if
    end subroutine assert

    subroutine test_tokenizer_normal()
        ! Given
        character(len=*), parameter :: s = 'var1=one' // char(10) // 'var2="double quoted"' // char(10) &
            // "var3='single quoted'" // char(10) // 'var4="""' // char(10) // "triple" // char(10) // "quoted" &
            // char(10) // '"""'
        type(fdotenv_token_t) :: results(40)
        integer :: token_count = 1
        integer :: pos = 1
        logical :: done = .false.

        ! When
        do while (.not. done .and. token_count <= 40)
            call fdotenv_next_token(s,pos,results(token_count))
            if (results(token_count)%kind == fdotenv_token_type_eof) then
                done = .true.
            else
                token_count = token_count + 1
            end if
        end do

        ! Then
        call assert(token_count == 16, "token count wrong - should be 16")

        call assert(results(1)%kind == fdotenv_token_type_string, "var1 token type wrong - should be string")
        call assert(results(1)%text == "var1", "var1 token text wrong - " // results(1)%text)
        call assert(results(2)%kind == fdotenv_token_type_equals, "var1 equals token type wrong - should be equals")
        call assert(results(2)%text == "=", "var1 equals token text wrong - " // results(2)%text)
        call assert(results(3)%kind == fdotenv_token_type_string, "var1 value token type wrong - should be string")
        call assert(results(3)%text == "one", "var1 value token text wrong - " // results(3)%text)
        call assert(results(4)%kind == fdotenv_token_type_newline, "var1 newline token type wrong - should be newline")

        call assert(results(5)%kind == fdotenv_token_type_string, "var2 token type wrong - should be string")
        call assert(results(5)%text == "var2", "var2 token text wrong - " // results(5)%text)
        call assert(results(6)%kind == fdotenv_token_type_equals, "var2 equals token type wrong - should be equals")
        call assert(results(6)%text == "=", "var2 equals token text wrong - " // results(6)%text)
        call assert(results(7)%kind == fdotenv_token_type_double_quote, "var2 value token type wrong - should be double quote")
        call assert(results(7)%text == "double quoted", "var2 value token text wrong - " // results(7)%text)
        call assert(results(8)%kind == fdotenv_token_type_newline, "var2 newline token type wrong - should be newline")

        call assert(results(9)%kind == fdotenv_token_type_string, "var3 token type wrong - should be string")
        call assert(results(9)%text == "var3", "var3 token text wrong - " // results(9)%text)
        call assert(results(10)%kind == fdotenv_token_type_equals, "var3 equals token type wrong - should be equals")
        call assert(results(10)%text == "=", "var3 equals token text wrong - " // results(10)%text)
        call assert(results(11)%kind == fdotenv_token_type_single_quote, "var3 value token type wrong - should be single quote")
        call assert(results(11)%text == "single quoted", "var3 value token text wrong - " // results(11)%text)
        call assert(results(12)%kind == fdotenv_token_type_newline, "var3 newline token type wrong - should be newline")

        call assert(results(13)%kind == fdotenv_token_type_string, "var4 token type wrong - should be string")
        call assert(results(13)%text == "var4", "var4 token text wrong - " // results(13)%text)
        call assert(results(14)%kind == fdotenv_token_type_equals, "var4 equals token type wrong - should be equals")
        call assert(results(14)%text == "=", "var4 equals token text wrong - " // results(14)%text)
        call assert(results(15)%kind == fdotenv_token_type_double_quote_triple, "var4 value token type wrong - should be double quote triple")
        call assert(results(15)%text == char(10) // "triple" // char(10) // "quoted" // char(10), "var3 value token text wrong - " // results(15)%text)

        call assert(results(16)%kind == fdotenv_token_type_eof, "EOF token wrong")

        print *,"Tokenizer Test Completed"
    end subroutine test_tokenizer_normal

    subroutine test_tokenizer_error()
        print *,"Tokenizer Error Test TO BE IMPLEMENTED"
    end subroutine test_tokenizer_error
end program test_tokenizer