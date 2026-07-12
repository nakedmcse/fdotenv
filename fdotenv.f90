! Fortran dotenv parser
module fdotenv
    use fdotenv_tokenizer
    implicit none
    private
    public :: fdotenv_read_file, fdotenv_parse_string, fdotenv_load

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

        subroutine fdotenv_read_file(f, t)
            character(len=*), intent(in) :: f
            character(len=:), allocatable, intent(out) :: t
            integer :: iunit, file_size, io_stat

            inquire(file=f, size=file_size, iostat=io_stat)
            if (io_stat /= 0 .or. file_size <= 0) return

            allocate(character(len=file_size) :: t)
            open(newunit=iunit, file=f, status='old', action='read', iostat=io_stat)
            if (io_stat /= 0) then
                deallocate(t)
                return
            end if
            read(iunit, fmt='(A)', advance='no', iostat=io_stat) t
            close(iunit)
        end subroutine fdotenv_read_file

        subroutine fdotenv_parse_string(s, vars, status)
            character(len=*), intent(in) :: s
            type(fdotenv_vars), intent(inout) :: vars
            type(fdotenv_status), intent(inout) :: status
            ! TODO: Loop tokens, parse
        end subroutine fdotenv_parse_string

        subroutine fdotenv_load(f, status)
            character(len=*), intent(in) :: f
            type(fdotenv_status), intent(inout) :: status
            character(len=:), allocatable :: s
            type(fdotenv_vars) :: vars

            call fdotenv_read_file(f,s)
            call fdotenv_parse_string(s,vars, status)
        end subroutine fdotenv_load
end module fdotenv