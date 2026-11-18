
program matrix
    implicit none

    integer, dimension(:,:), allocatable :: a
    integer :: n, m, i, j

 do
     print *, "Enter number of rows (1 to 5):"
        read *, n
        if (n >= 1 .and. n <= 5) exit
        print *, "Invalid no. Try again."
 end do

do
        print *, "Enter number of columns (1 to 5):"
        read *, m
        if (m >= 1 .and. m <= 5) exit
        print *, "Invalid no. Try again."
end do

   
    allocate(a(n,m))

  
    do i = 1, ubound(a, 1)
        do j = 1, ubound(a, 2)
            a(i,j) = i * j
        end do
    end do

    call print_row_totals(a)

    deallocate(a)

contains

    subroutine print_row_totals(a)
        implicit none
        integer, dimension(:,:), intent(in) :: a
        integer :: i

        do i = 1, ubound(a, 1)
            print *, "Row", i, ":", a(i,:)
            print *, "Total:", sum(a(i,:))
        end do

    end subroutine print_row_totals

end program matrix
