! Dotenv Tokenizer
module fdotenv_tokenizer
    implicit none
    private
    public :: fdotenv_next_token

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
        subroutine next_break(s, pos)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos

            do while (pos <= len(s) .and. .not. (s(pos:pos) == char(10) .or. s(pos:pos) == '='))
                pos = pos + 1
            end do
        end subroutine next_break

        subroutine next_given(s, pos, g, nl)
            character(len=*), intent(in) :: s
            character(len=1), intent(in) :: g
            logical, intent(in) :: nl
            integer, intent(inout) :: pos

            do while (pos <= len(s) .and. .not. s(pos:pos) == g)
                if (nl .and. s(pos:pos) == char(10)) exit
                pos = pos + 1
            end do
        end subroutine next_given

        subroutine next_triple(s, pos, t)
            character(len=*), intent(in) :: s
            character(len=3), intent(in) :: t
            integer, intent(inout) :: pos

            do while (pos + 2 <= len(s) .and. .not. s(pos:pos+2) == t)
                pos = pos + 1
            end do
        end subroutine next_triple

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
                    start = pos + 1
                    call next_given(s,pos,char(10),.false.)
                    tok%text = s(start:pos-1)
                    return
                case (char(10))
                    tok%kind = fdotenv_token_type_newline
                    tok%text = char(10)
                    pos = pos + 1
                    return
                case ('"')
                    if (pos + 2 <= len(s) .and. s(pos:pos+2) == '"""') then
                        tok%kind = fdotenv_token_type_double_quote_triple
                        start = pos + 3
                        pos = pos + 3
                        call next_triple(s, pos, '"""')
                        if (pos + 3 > len(s) .and. .not. s(pos:pos) == '"') tok%kind = fdotenv_token_type_error
                        tok%text = s(start:pos-1)
                        pos = pos + 3
                        return
                    end if
                    tok%kind = fdotenv_token_type_double_quote
                    start = pos + 1
                    pos = pos + 1
                    call next_given(s,pos,'"',.true.)
                    if (s(pos:pos) == char(10)) tok%kind = fdotenv_token_type_error
                    tok%text = s(start:pos-1)
                    pos = pos + 1
                    return
                case ("'")
                    if (pos + 2 <= len(s) .and. s(pos:pos+2) == "'''") then
                        tok%kind = fdotenv_token_type_single_quote_triple
                        start = pos
                        pos = pos + 3
                        call next_triple(s, pos, "'''")
                        if (pos + 3 > len(s) .and. .not. s(pos:pos) == "'") tok%kind = fdotenv_token_type_error
                        tok%text = s(start:pos-1)
                        pos = pos + 3
                        if (pos > len(s)) tok%kind = fdotenv_token_type_error
                        return
                    end if
                    tok%kind = fdotenv_token_type_single_quote
                    start = pos + 1
                    pos = pos + 1
                    call next_given(s,pos,"'",.true.)
                    if (s(pos:pos) == char(10)) tok%kind = fdotenv_token_type_error
                    tok%text = s(start:pos-1)
                    pos = pos + 1
                    return
                case default
                    tok%kind = fdotenv_token_type_string
                    start = pos
                    call next_break(s,pos)
                    tok%text = s(start:pos-1)
                    return
            end select
        end subroutine fdotenv_next_token

end module fdotenv_tokenizer