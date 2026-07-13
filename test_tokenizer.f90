! Fortran dotenv tokenizer unit tests
program test_tokenizer
    use fdotenv_tokenizer
    implicit none

    type :: token_stream
        type(fdotenv_token_t) :: results(40)
        integer :: token_count = 1
    end type token_stream

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

    subroutine get_token_stream(s, stream)
        character(len=*), intent(in) :: s
        type(token_stream), intent(inout) :: stream
        integer :: pos = 1
        do while (stream%token_count <= 40)
            call fdotenv_next_token(s,pos,stream%results(stream%token_count))
            if (stream%results(stream%token_count)%kind == fdotenv_token_type_eof) exit
            stream%token_count = stream%token_count + 1
        end do
    end subroutine get_token_stream

    subroutine test_tokenizer_normal()
        ! Given
        character(len=*), parameter :: s = 'var1=one' // char(10) // 'var2="double quoted"' // char(10) &
            // "var3='single quoted'" // char(10) // 'var4="""' // char(10) // "triple" // char(10) // "quoted" &
            // char(10) // '"""'
        type(token_stream) :: stream

        ! When
        call get_token_stream(s,stream)

        ! Then
        call assert(stream%token_count == 16, "token count wrong - should be 16")

        call assert(stream%results(1)%kind == fdotenv_token_type_string, "var1 token type wrong - should be string")
        call assert(stream%results(1)%text == "var1", "var1 token text wrong - " // stream%results(1)%text)
        call assert(stream%results(2)%kind == fdotenv_token_type_equals, "var1 equals token type wrong - should be equals")
        call assert(stream%results(2)%text == "=", "var1 equals token text wrong - " // stream%results(2)%text)
        call assert(stream%results(3)%kind == fdotenv_token_type_string, "var1 value token type wrong - should be string")
        call assert(stream%results(3)%text == "one", "var1 value token text wrong - " // stream%results(3)%text)
        call assert(stream%results(4)%kind == fdotenv_token_type_newline, "var1 newline token type wrong - should be newline")

        call assert(stream%results(5)%kind == fdotenv_token_type_string, "var2 token type wrong - should be string")
        call assert(stream%results(5)%text == "var2", "var2 token text wrong - " // stream%results(5)%text)
        call assert(stream%results(6)%kind == fdotenv_token_type_equals, "var2 equals token type wrong - should be equals")
        call assert(stream%results(6)%text == "=", "var2 equals token text wrong - " // stream%results(6)%text)
        call assert(stream%results(7)%kind == fdotenv_token_type_double_quote, "var2 value token type wrong - should be double quote")
        call assert(stream%results(7)%text == "double quoted", "var2 value token text wrong - " // stream%results(7)%text)
        call assert(stream%results(8)%kind == fdotenv_token_type_newline, "var2 newline token type wrong - should be newline")

        call assert(stream%results(9)%kind == fdotenv_token_type_string, "var3 token type wrong - should be string")
        call assert(stream%results(9)%text == "var3", "var3 token text wrong - " // stream%results(9)%text)
        call assert(stream%results(10)%kind == fdotenv_token_type_equals, "var3 equals token type wrong - should be equals")
        call assert(stream%results(10)%text == "=", "var3 equals token text wrong - " // stream%results(10)%text)
        call assert(stream%results(11)%kind == fdotenv_token_type_single_quote, "var3 value token type wrong - should be single quote")
        call assert(stream%results(11)%text == "single quoted", "var3 value token text wrong - " // stream%results(11)%text)
        call assert(stream%results(12)%kind == fdotenv_token_type_newline, "var3 newline token type wrong - should be newline")

        call assert(stream%results(13)%kind == fdotenv_token_type_string, "var4 token type wrong - should be string")
        call assert(stream%results(13)%text == "var4", "var4 token text wrong - " // stream%results(13)%text)
        call assert(stream%results(14)%kind == fdotenv_token_type_equals, "var4 equals token type wrong - should be equals")
        call assert(stream%results(14)%text == "=", "var4 equals token text wrong - " // stream%results(14)%text)
        call assert(stream%results(15)%kind == fdotenv_token_type_double_quote_triple, "var4 value token type wrong - should be double quote triple")
        call assert(stream%results(15)%text == char(10) // "triple" // char(10) // "quoted" // char(10), "var3 value token text wrong - " // stream%results(15)%text)

        call assert(stream%results(16)%kind == fdotenv_token_type_eof, "EOF token wrong")

        print *,"Tokenizer Test Completed"
    end subroutine test_tokenizer_normal

    subroutine test_tokenizer_error()
        ! Given
        character(len=*), parameter :: s1 = 'var1="one' // char(10)
        character(len=*), parameter :: s2 = 'var1="""' // char(10) // 'another line' // char(10)
        type(token_stream) :: e1,e2

        ! When
        call get_token_stream(s1,e1)
        call get_token_stream(s2,e2)

        ! Then
        print *,e1%token_count
        call assert(e1%token_count == 4, "E1 token count wrong - should be 4")
        call assert(e1%results(1)%kind == fdotenv_token_type_string, "E1 token 1 type wrong - should be string")
        call assert(e1%results(1)%text == "var1", "E1 token 1 text wrong - " // e1%results(1)%text)
        call assert(e1%results(2)%kind == fdotenv_token_type_equals, "E1 token 2 type wrong - should be equals")
        call assert(e1%results(3)%kind == fdotenv_token_type_error, "E1 token 3 type wrong - should be error")
        call assert(e1%results(4)%kind == fdotenv_token_type_eof, "E1 token 4 type wrong - should be EOF")

        call assert(e2%token_count == 4, "E2 token count wrong - should be 4")
        call assert(e2%results(1)%kind == fdotenv_token_type_string, "E2 token 1 type wrong - should be string")
        call assert(e2%results(1)%text == "var1", "E2 token 1 text wrong - " // e2%results(1)%text)
        call assert(e2%results(2)%kind == fdotenv_token_type_equals, "E2 token 2 type wrong - should be equals")
        call assert(e2%results(3)%kind == fdotenv_token_type_error, "E2 token 3 type wrong - should be error")
        call assert(e2%results(4)%kind == fdotenv_token_type_eof, "E2 token 4 type wrong - should be EOF")

        print *,"Tokenizer Error Test Completed"
    end subroutine test_tokenizer_error
end program test_tokenizer