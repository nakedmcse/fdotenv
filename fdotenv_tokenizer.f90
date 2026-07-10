! Dotenv Tokenizer
module fdotenv_tokenizer
    implicit none
    private

    integer, parameter, public :: fdotenv_token_type_error   = -1
    integer, parameter, public :: fdotenv_token_type_eof     = 0
    integer, parameter, public :: fdotenv_token_type_string  = 1
    integer, parameter, public :: fdotenv_token_type_equals  = 2
    integer, parameter, public :: fdotenv_token_type_hash    = 3
    integer, parameter, public :: fdotenv_token_type_newline = 4
    integer, parameter, public :: fdotenv_token_type_double_quote = 5
    integer, parameter, public :: fdotenv_token_type_double_quote_triple = 6
    integer, parameter, public :: fdotenv_token_type_single_quote = 7
    integer, parameter, public :: fdotenv_token_type_single_quote_triple = 8

    type, public :: fdotenv_token_t
        integer :: kind = fdotenv_token_type_error
        character(len=:), allocatable :: text
    end type fdotenv_token_t

    contains
        subroutine fdotenv_next_token(s, pos, tok)
            character(len=*), intent(in) :: s
            integer :: pos, start
            type(fdotenv_token_t), intent(out) :: tok
            character(len=1) :: ch

            if (pos > len(s)) then
                tok%kind = fdotenv_token_type_eof
                tok%text = ""
                return
            end if

            ch = s(pos:pos)

            select case(ch)
                case ('=')
                    tok%kind = fdotenv_token_type_equals
                    tok%text = "="
                    pos = pos + 1
                    return
                case ('#')
                    tok%kind = fdotenv_token_type_hash
                    ! find next newline or eof
                    ! extract text to there for return
                    ! move pos
                    return
                case ('"')
                    tok%kind = fdotenv_token_type_double_quote
                    tok%text = '"'
                    pos = pos + 1
                    return
                case ("'")
                    tok%kind = fdotenv_token_type_single_quote
                    tok%text = "'"
                    pos = pos + 1
                    return
                case default
                    ! String token
            end select

        end subroutine fdotenv_next_token

    ! TODO: Implement tokenizer
end module fdotenv_tokenizer