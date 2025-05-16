module procedures
    implicit none

    private
    public quicksort, print_matrix

contains
    ! Array quicksort subroutine
    recursive subroutine quicksort(a)
        real, intent(inout) :: a(:)
        real x, t
        integer :: first = 1, last
        integer i, j

        last = size(a, 1)
        x = a( (first+last) / 2 )
        i = first
        j = last

        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            i=i+1
            j=j-1
        end do

        if (first < i - 1) call quicksort(a(first : i - 1))
        if (j + 1 < last)  call quicksort(a(j + 1 : last))
    end subroutine quicksort

    ! Matrix Printing subroutine
    subroutine print_matrix(matrix, alphabet)
        integer, intent(in) :: matrix(:,:)
        character(len=1), intent(in) :: alphabet(:)

        integer :: i

        print '(X, *(A8))', alphabet
        do i = 1, size(matrix, 1)
            print '(A, *(I8))', alphabet(i), matrix(i,:)
        end do
    end subroutine print_matrix
end module procedures

program lab_1
    use procedures
    implicit none

    real, allocatable :: numbers(:), original_numbers(:)
    character(len=1), allocatable :: alphabet(:), linguistic_sequence(:)
    integer, allocatable :: precedence_matrix(:,:)

    integer :: n, i, index, alphabet_size, io_status, row, col
    real :: min_number, max_number, interval_length
    character(len=8) :: filename = "data.txt"

    ! Input alphabet size
    print *, "Enter alphabet size"
    do
        read *, alphabet_size
        if (alphabet_size <= 26) exit
        print *, "Alphabet size too large, try again"
    end do


    ! Allocate alphabet and precedence matrix
    allocate(alphabet(alphabet_size))
    allocate(precedence_matrix(alphabet_size, alphabet_size))

    ! Fill the alphabet with values
    do i = 1, alphabet_size
        alphabet(i) = achar(64 + i)
    end do

    ! Reading a number file and counting the amount of numbers
    open(unit=10, file=filename, status="old", action="read", iostat=io_status)
    if (io_status /= 0) then
        print *, "Error opening file."
        stop
    end if

    n = 0
    do
        read(10, *, iostat=io_status)
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(10)

    ! Allocating memory for arrays
    allocate(numbers(n))
    allocate(original_numbers(n))
    allocate(linguistic_sequence(n))

    ! Reading numbers from file to original_numbers
    open(unit=10, file=filename, status="old", action="read", iostat=io_status)
    if (io_status /= 0) then
        print *, "Error opening file."
        stop
    end if

    do i = 1, n
        read(10, *) original_numbers(i)
    end do
    close(10)

    ! Copying original_numbers to numbers for sorting
    numbers = original_numbers
    call quicksort(numbers)

    ! Calculating step intervals
    min_number = numbers(1)
    max_number = numbers(n)
    interval_length = (max_number - min_number) / alphabet_size

    ! Converting numbers to a lexical string
    do i = 1, n
        index = ceiling((original_numbers(i) - min_number) / interval_length)
        if (index < 1) index = 1
        if (index > alphabet_size) index = alphabet_size
        linguistic_sequence(i) = alphabet(index)
    end do

    ! Constructing a precedence matrix
    do i = 1, n - 1
        row = findloc(alphabet, linguistic_sequence(i), 1)
        col = findloc(alphabet, linguistic_sequence(i + 1), 1)
        precedence_matrix(row, col) = precedence_matrix(row, col) + 1
    end do

    ! Printing program results
    print *, "Alphabet Size:", alphabet_size
    print *, "Linguistic Sequence:", " ", linguistic_sequence
    print *, "Transition Matrix:"
    call print_matrix(precedence_matrix, alphabet)

    ! Freeing allocated memory
    deallocate(numbers, original_numbers, alphabet, linguistic_sequence, precedence_matrix)
end program lab_1
