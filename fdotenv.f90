! Fortran dotenv parser
module fdotenv
    use fdotenv_tokenizer
    use iso_c_binding
    implicit none
    private
    public :: fdotenv_read_file, fdotenv_parse_string, fdotenv_load

    ! Interface to the C library function 'setenv'
    interface
        integer(c_int) function setenv(name, value, overwrite) bind(c, name="setenv")
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int), value :: overwrite
        end function setenv
    end interface

    type, public :: fdotenv_status
        logical :: error = .false.
        integer :: offset = 0
    end type fdotenv_status

    type, public :: fdotenv_kv
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        logical :: singleQuoted = .false.
    end type fdotenv_kv

    type, public :: fdotenv_vars
        type(fdotenv_kv), dimension(:), allocatable :: items
        integer :: count = 0
    contains
        procedure fdotenv_vars_append
    end type fdotenv_vars

    contains
        subroutine fdotenv_vars_append(this, value)
            class(fdotenv_vars) :: this
            type(fdotenv_kv) :: value
            type(fdotenv_kv), dimension(:), allocatable :: temp

            if (.not. allocated(this%items)) then
                allocate(this%items(256))
            elseif (size(this%items) == this%count) then
                allocate(temp(this%count * 2))
                temp(1:this%count) = this%items(1:this%count)
                call move_alloc(temp,this%items)
            end if

            this%count = this%count + 1
            this%items(this%count) = value
        end subroutine fdotenv_vars_append

        subroutine fdotenv_ensure_capacity(buffer,capacity,content_length,required_capacity,growth_size)
            character(len=:), allocatable, intent(inout) :: buffer
            integer, intent(inout) :: capacity
            integer, intent(in) :: content_length, required_capacity, growth_size

            character(len=:), allocatable :: resized
            integer :: new_capacity

            if (required_capacity <= capacity) return

            new_capacity = capacity + ((required_capacity - capacity + growth_size - 1) / growth_size) * growth_size
            allocate(character(len=new_capacity) :: resized)

            if (content_length > 0) then
                resized(1:content_length) = buffer(1:content_length)
            end if

            call move_alloc(resized, buffer)
            capacity = new_capacity
        end subroutine fdotenv_ensure_capacity

        subroutine fdotenv_expand(s, t)
            character(len=*), intent(in) :: s
            character(len=:), allocatable, intent(out) :: t

            character(len=:), allocatable :: env_value
            character(len=:), allocatable :: resized
            integer, parameter :: growth_size = 256

            integer :: input_pos, closing_pos, output_len, capacity, env_len, status, required_size
            input_pos = 1
            output_len = 0
            capacity = max(growth_size, len(s))

            allocate(character(len=capacity) :: t)

            do while (input_pos <= len(s))
                if (input_pos < len(s) .and. s(input_pos:input_pos + 1) == '${') then
                    closing_pos = index(s(input_pos + 2:), '}')

                    if (closing_pos > 0) then
                        closing_pos = input_pos + closing_pos + 1

                        call get_environment_variable(name=s(input_pos + 2:closing_pos - 1),length=env_len,status=status)

                        if (status == 0) then
                            if (env_len > 0) then
                                allocate(character(len=env_len) :: env_value)
                                call get_environment_variable(name=s(input_pos + 2:closing_pos - 1),value=env_value,status=status)
                                if (status == 0) then
                                    required_size = output_len + env_len
                                    call fdotenv_ensure_capacity(t,capacity,output_len,required_size,growth_size)
                                    t(output_len + 1:required_size) = env_value
                                    output_len = required_size
                                end if
                                deallocate(env_value)
                            end if

                            input_pos = closing_pos + 1
                            cycle
                        end if
                    end if
                end if

                call fdotenv_ensure_capacity(t,capacity,output_len,output_len + 1,growth_size)
                output_len = output_len + 1
                t(output_len:output_len) = s(input_pos:input_pos)

                input_pos = input_pos + 1
            end do

            allocate(character(len=output_len) :: resized)
            if (output_len > 0) then
                resized = t(1:output_len)
            end if
            call move_alloc(resized, t)
        end subroutine fdotenv_expand

        subroutine fdotenv_read_file(f, t)
            character(len=*), intent(in) :: f
            character(len=:), allocatable, intent(out) :: t
            integer :: iunit, file_size, io_stat

            inquire(file=f, size=file_size, iostat=io_stat)
            if (io_stat /= 0 .or. file_size <= 0) return

            allocate(character(len=file_size) :: t)
            open(newunit=iunit, file=f, status='old', action='read', access='stream', form='unformatted', iostat=io_stat)
            if (io_stat /= 0) then
                deallocate(t)
                return
            end if
            read(iunit, iostat=io_stat) t
            close(iunit)
        end subroutine fdotenv_read_file

        subroutine fdotenv_parse_string(s, vars, status)
            character(len=*), intent(in) :: s
            type(fdotenv_vars), intent(inout) :: vars
            type(fdotenv_status), intent(inout) :: status
            type(fdotenv_token_t) :: next_token
            type(fdotenv_kv) :: current_kv
            integer :: pos, i
            integer(c_int) :: ier
            logical :: done, seen_equals
            character(len=:), allocatable :: replaced
            done = .false.
            seen_equals = .false.
            pos = 1
            i = 1
            do while (.not. done)
                call fdotenv_next_token(s, pos, next_token)
                select case (next_token%kind)
                    case (fdotenv_token_type_error)
                        done = .true.
                        status%error = .true.
                        status%offset = pos

                    case (fdotenv_token_type_eof)
                        done = .true.

                    case (fdotenv_token_type_newline)
                        if (allocated(current_kv%key)) deallocate(current_kv%key)
                        if (allocated(current_kv%value)) deallocate(current_kv%value)
                        seen_equals = .false.

                    case (fdotenv_token_type_equals)
                        seen_equals = .true.

                    case (fdotenv_token_type_string)
                        if (seen_equals) then
                            current_kv%value = next_token%text
                            current_kv%singleQuoted = .false.
                            call vars%fdotenv_vars_append(current_kv)
                            deallocate(current_kv%key)
                            deallocate(current_kv%value)
                        else
                            current_kv%key = next_token%text
                        end if

                    case (fdotenv_token_type_hash)
                        ! Comment, do nothing

                    case (fdotenv_token_type_single_quote, fdotenv_token_type_single_quote_triple)
                        if (seen_equals .and. allocated(current_kv%key)) then
                            current_kv%singleQuoted = .true.
                            current_kv%value = next_token%text
                            call vars%fdotenv_vars_append(current_kv)
                            deallocate(current_kv%key)
                            deallocate(current_kv%value)
                            current_kv%singleQuoted = .false.
                            seen_equals = .false.
                        end if

                    case (fdotenv_token_type_double_quote, fdotenv_token_type_double_quote_triple)
                        if (seen_equals .and. allocated(current_kv%key)) then
                            current_kv%singleQuoted = .false.
                            current_kv%value = next_token%text
                            call vars%fdotenv_vars_append(current_kv)
                            deallocate(current_kv%key)
                            deallocate(current_kv%value)
                            seen_equals = .false.
                        end if

                    case default
                        ! Unknown token, do nothing
                end select
            end do

            if(vars%count > 0) then
                do i=1, vars%count
                    if (.not. vars%items(i)%singleQuoted) then
                        call fdotenv_expand(vars%items(i)%value, replaced)
                        call move_alloc(replaced, vars%items(i)%value)
                    end if
                    ier = setenv(vars%items(i)%key // c_null_char, vars%items(i)%value // c_null_char, 1)
                end do
            end if
        end subroutine fdotenv_parse_string

        subroutine fdotenv_load(f, status)
            character(len=*), intent(in) :: f
            type(fdotenv_status), intent(inout) :: status
            character(len=:), allocatable :: s
            type(fdotenv_vars) :: vars

            call fdotenv_read_file(f,s)
            call fdotenv_parse_string(s,vars,status)
        end subroutine fdotenv_load
end module fdotenv