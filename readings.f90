
program readings_above_mean
    implicit none

    real, dimension(:), allocatable :: readings
    real :: average
    integer :: n, i, count

   
    do
        print *, "How many readings (1 to 20)?"
        read *, n

        if (n >= 1 .and. n <= 20) exit

        print *, "Invalid no. Try again."
    end do


    allocate(readings(n))

    
    do i = 1, n
        print *, "Enter reading", i
        read *, readings(i)
    end do


    average = mean_of(readings)
    print *, "Mean =", average

    
    count = 0

    do i = 1, n
        if (readings(i) > average) then
            print *, "Index:", i, "Value:", readings(i)
            count = count + 1
        end if
    end do

    print *, "No of readings above mean:", count

contains


    function mean_of(x) result(avg)
        real, dimension(:), intent(in) :: x
        real :: avg

        avg = sum(x) / size(x)

    end function mean_of

end program readings_above_mean
