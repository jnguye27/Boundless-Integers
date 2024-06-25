! Program: Unbounded Integers
! Creator: Jessica Nguyen
! Date: 2024-03-26
! Purpose: Can calculate standard operations between 2 large numbers using linked lists.

program unbounded
    ! add in the linked list module
    use :: dynllist

    ! declare variables
    character(len=1) :: operation
    type(operand) :: first, second, result
    logical :: isValid, isZero
    character(len=1000) :: tempStr

    ! implement a user interface, like the one shown in A4's PDF, with a validation check for each
    write (*,'(/,A)') "Enter an operation: + - * / or !"
    read (*,'(A1)') operation
    do
        call charCheck(operation, isValid)

        if (isValid .EQV. .FALSE.) then
            write (*,'(/,A)') "Invalid operation input."
            write (*,'(A)') "Enter an operation: + - * / or !"
            read (*,'(A1)') operation
        else
            exit
        end if
    end do

    write (*,'(/,A)') "Enter first operand:"
    read (*,'(A1000)') first%numberStr
    do
        call intCheck(first, isValid)

        ! can't have a negative operand when doing factorials
        if ((operation .EQ. '!') .AND. (first%numberStr(1:1) .EQ. '-')) then
            isValid = .FALSE.
        end if

        if (isValid .EQV. .FALSE.) then
            write (*,'(/,A)') "Invalid operand input."
            write (*,'(A)') "Enter first operand:"
            read (*,'(A1000)') first%numberStr
        else
            exit
        end if
    end do

    if (operation .NE. '!') then
        write (*,'(/,A)') "Enter second operand:"
        read (*,'(A1000)') second%numberStr
        do
            call intCheck(second, isValid)

            ! can't be divided by zero
            if (operation .EQ. '/') then
                tempStr = second%numberStr
                call convertStrToList(second)
                call allZeros(second, isZero)
                if (isZero .EQV. .TRUE.) then
                    isValid = .FALSE.
                end if
                call emptyList(second)
                second%numberStr = tempStr
                if (tempStr(1:1) .EQ. '-') then
                    second%sign = '-'
                else
                    second%sign = '+'
                end if
            end if

            if (isValid .EQV. .FALSE.) then
                write (*,'(/,A)') "Invalid operand input."
                write (*,'(A)') "Enter second operand:"
                read (*,'(A1000)') second%numberStr
            else
                exit
            end if
        end do
    end if

    ! convert operand strings into operand linked lists
    call convertStrToList(first)
    call convertStrToList(second)

    ! do the operation on the operand(s)
    if ((operation .EQ. '+') .OR. (operation .EQ. '-')) then
        call addSubtract(first, second, result, operation)
    else if (operation .EQ. '*') then
        call multiply(first, second, result)
    else if (operation .EQ. '/') then
        call divide(first, second, result)
    else if (operation .EQ. '!') then
        call factorial(first, result)
    end if

    ! check if the result is made up of all zeros
    call allZeros(result, isZero)

    ! display the result
    write (*,'(/,A)') "The result is: "
    if (isZero .EQV. .TRUE.) then
        write (*,'(I0,/)') 0
    else
        call printList(result)
        write (*,'(A,/)') " "
    end if

    ! clean up the leftover lists
    call emptyList(result)
    call emptyList(second)
    call emptyList(first)

contains

! this subroutine will verify that the input is 1 valid operation only (+,-,*,/,!)
! returns true or false
subroutine charCheck(operation, isValid)
    ! declare parameter variables
    character(len=1), intent(in) :: operation
    logical, intent(inout) :: isValid

    ! check if the input is a valid char
    if ((operation .EQ. '+') .OR. (operation .EQ. '-') .OR. (operation .EQ. '*') &
        .OR. (operation .EQ. '/') .OR. (operation .EQ. '!')) then
        isValid = .TRUE.
    else
        isValid = .FALSE.
    end if
end subroutine charCheck

! this subroutine will verify that the input is valid (only integers and negative/positive signs)
! returns true or false
subroutine intCheck(list, isValid)
    ! declare parameter variables
    type(operand), intent(inout) :: list
    logical, intent(inout) :: isValid

    ! declare local variables
    integer :: i, isInt

    ! obtain the starting index and the operand's sign
    if (list%numberStr(1:1) .EQ. '-') then
        i = 2
        list%sign = '-'
    else if (list%numberStr(1:1) .EQ. '+') then
        i = 2
        list%sign = '+'
    else
        i = 1
        list%sign = '+'
    end if

    ! check if the input is a valid integer
    isValid = .TRUE.
    do 
        ! exit when we're through the entire string
        if (i .GT. len_trim(list%numberStr)) then
            exit
        end if

        ! check if the current character is an integer (in ascii)
        isInt = ichar(list%numberStr(i:i))
        if (isInt < ichar('0') .OR. isInt > ichar('9')) then
            ! if it's out of bounds, then it's false and we exit early
            isValid = .FALSE.
            exit
        end if

        ! go onto the next character 
        i = i + 1
    end do
end subroutine intCheck

! this subroutine will parse the digits (in the operand) into the linked list (turning chars into ints)
! returns the linked list
subroutine convertStrToList(list)
    ! declare parameter variable
    type(operand), intent(inout) :: list

    ! declare local variables
    integer :: i, parsedDigit

    ! parse the operand into the linked list
    if (len_trim(list%numberStr) .EQ. 1) then
        ! if the string is only 1 character long
        if ((list%numberStr(1:1) .EQ. '+') .OR. (list%numberStr(1:1) .EQ. '-')) then
            ! we make the linked list equal to zero if there is only a sign
            call insertNode(list, 0)
        else
            ! we make the linked list equal to that one digit
            parsedDigit = ichar(list%numberStr(1:1)) - ichar('0')
            call insertNode(list, parsedDigit)
        end if
    else if ((len_trim(list%numberStr) .EQ. 2) .AND. ((list%numberStr(1:1) .EQ. '+') .OR. (list%numberStr(1:1) .EQ. '-'))) then
        ! if the string is only 2 characters long (and one of them is the sign), insert only the digit
        parsedDigit = ichar(list%numberStr(2:2)) - ichar('0')
        call insertNode(list, parsedDigit)
    else
        ! if there is a sign infront of the number, skip it
        if ((list%numberStr(1:1) .EQ. '+') .OR. (list%numberStr(1:1) .EQ. '-')) then
            i = 2
        else
            i = 1
        end if
        
        ! if the left most digits equal zero, skip them
        do
            if (list%numberStr(i:i) .EQ. '0') then
                i = i + 1
            else
                exit
            end if
        end do

        do 
            ! exit the while loop once we have no more numbers to parse
            if (i .GT. len_trim(list%numberStr)) then
                exit
            else
                ! take the left most digit in the operand
                parsedDigit = ichar(list%numberStr(i:i)) - ichar('0')

                ! put it into the linked list
                call insertNode(list, parsedDigit)
            end if

            ! go onto the next digit
            i = i + 1
        end do
    end if
end subroutine convertStrToList

! this subroutine will add/subtract the operands together to make the result's linked list (by the tail)
! returns the linked list for the result
subroutine addSubtract(first, second, result, operation)
    ! declare parameter variables
    type(operand), intent(inout) :: first, second, result
    character(len=1), intent(in) :: operation

    ! declare local variables
    type(operand) :: firstCopy, secondCopy
    type(node), pointer :: firstPtr, secondPtr
    integer :: remainder, sum, isGreater
        
    ! initialize local variable
    call copyList(first, firstCopy)
    call flipList(firstCopy)
    call copyList(second, secondCopy)
    call flipList(secondCopy)
    remainder = 0

    ! depending on the operation, we add/subtract the signs differently
    if (((operation .EQ. '+') .AND. (first%sign .NE. second%sign)) .OR. &
        ((operation .EQ. '-') .AND. (first%sign .EQ. second%sign))) then
        ! find the bigger operand
        call greaterOperand(firstCopy, secondCopy, isGreater)

        ! sum = bigger operand - smaller operand
        if (isGreater .EQ. 1) then
            call oppositeSigns(firstCopy, secondCopy, result)
        else if (isGreater .EQ. 2) then
            ! switch the second's sign if the operation is subtraction
            if (operation .EQ. '-') then
                if (second%sign .EQ. '-') then
                    second%sign = '+'
                else 
                    second%sign = '-'
                end if
            end if
            call oppositeSigns(secondCopy, firstCopy, result)
        else 
            result%sign = '+'
            call insertNode(result, 0)
        end if
    else
        ! if the operation is subtraction, switch the second operand's sign: "+ plus +" is the same as "+ minus -"
        if (operation .EQ. '-') then
            if (second%sign .EQ. '-') then
                second%sign = '+'
            else 
                second%sign = '-'
            end if
        end if

        ! the result has the same sign as the 2 operands
        result%sign = first%sign    

        ! result = first operand + second operand
        firstPtr => firstCopy%tail%endpoint
        secondPtr => secondCopy%tail%endpoint

        do
            ! associated() checks if the linked list(s) ended to deal with it accordingly
            if ((associated(firstPtr) .EQV. .FALSE.) .AND. (associated(secondPtr) .EQV. .FALSE.)) then 
                exit
            else if (associated(firstPtr) .EQV. .FALSE.) then
                call sameSigns(secondPtr, result, remainder)
                exit
            else if (associated(secondPtr) .EQV. .FALSE.) then
                call sameSigns(firstPtr, result, remainder)
                exit
            else
                ! calculate the sum
                sum = firstPtr%digit + secondPtr%digit + remainder

                ! split the remainder from the calculated sum
                remainder = sum / 10
                sum = mod(sum, 10)

                ! put the sum into the result
                call insertNode(result, sum)

                ! go onto the next pair of numbers
                firstPtr => firstPtr%previous
                secondPtr => secondPtr%previous
            end if
        end do

        ! if there is a remainder left over, add it to the result
        if (remainder .NE. 0) then
            call insertNode(result, remainder)
        end if
    end if
end subroutine addSubtract

! this subroutine will multiply the operands together to make the result's linked list (by the tail)
! returns the linked list for the result
subroutine multiply(first, second, finalResult)
    ! declare parameter variables
    type(operand), intent(in) :: first, second
    type(operand), intent(inout) :: finalResult

    ! declare local variables
    type(operand) :: firstResult, secondResult
    type(node), pointer :: biggerPtr, smallerPtr
    integer :: isGreater, tensMultiplier, i, remainder, sum
        
    ! initialize local variables
    tensMultiplier = 0
    call insertNode(secondResult, 0)

    ! find the bigger operand
    call greaterOperand (first, second, isGreater)

    ! multiply the operands together depending on which is bigger
    if (isGreater .EQ. 1) then
        biggerPtr => first%tail%endpoint
        smallerPtr => second%tail%endpoint
    else
        biggerPtr => second%tail%endpoint
        smallerPtr => first%tail%endpoint
    end if

    do
        ! until we calculate with all of the digits in the smaller number, keep going
        if (associated(smallerPtr) .EQV. .FALSE.) then
            ! copy over the final result
            call moveList(secondResult, finalResult)
            exit
        else
            ! add the zeros first (identifies 1s, 10s, 100s, etc.)
            i = 0
            do
                if (i .EQ. tensMultiplier) then
                    exit
                else 
                    call insertNode(firstResult, 0)
                    i = i + 1
                end if
            end do
        end if

        ! reset the remainder
        remainder = 0

        ! calculate 1 result = all of the digits of the bigger operand * one digit of the smaller operand
        do
            ! if there are no more bigger operand digits, add the remainder then exit
            if (associated(biggerPtr) .EQV. .FALSE.) then
                if (remainder .NE. 0) then
                    call insertNode(firstResult, remainder)
                end if
                exit
            end if

            ! calculate the sum and split off the remainder
            sum = (smallerPtr%digit * biggerPtr%digit) + remainder
            remainder = sum / 10
            sum = mod(sum, 10)

            ! put the sum into a temporary list
            call insertNode(firstResult, sum)

            ! go onto the next digit of the bigger number
            biggerPtr => biggerPtr%previous
        end do

        ! if the result is all zeros (ex: 000), convert them into 1 zero
        call allZeros(firstResult,isZero)
        if (isZero .EQV. .TRUE.) then
            call emptyList(firstResult)
            call insertNode(firstResult,0)
        end if

        ! add the two results together
        firstResult%sign = '+'
        secondResult%sign = '+'
        call flipList(firstResult)
        call addSubtract(firstResult, secondResult, finalResult, '+')

        ! clear the first result to reuse the variable
        call emptyList(firstResult)

        ! put the current sum into the second result
        call moveList(finalResult, secondResult)
        
        ! go onto the next digit of the second operand
        smallerPtr => smallerPtr%previous
        tensMultiplier = tensMultiplier + 1
        
        ! reset the pointer's position of the greater number
        if (isGreater .EQ. 1) then
            biggerPtr => first%tail%endpoint
        else
            biggerPtr => second%tail%endpoint
        end if
    end do

    ! calculate the final sign
    if ((first%sign .EQ. second%sign) .OR. &
        ((finalResult%numDigits .EQ. 1) .AND. (finalResult%head%endpoint%digit .EQ. 0))) then
        finalResult%sign = '+'
    else
        finalResult%sign = '-'
    end if
end subroutine multiply

! this subroutine will divide the first operand by the second operand
! returns the linked list for the result
subroutine divide(first, second, finalResult)
    ! declare parameter variables
    type(operand), intent(inout) :: first, second, finalResult
    
    ! declare local variables
    type(operand) :: sum, currentResult
    character(len=1) :: firstSign, secondSign
    integer :: counter, isGreater, digit

    ! initialize location variables
    call insertNode(sum, 0)
    counter = 0
    firstSign = first%sign
    secondSign = second%sign

    call greaterOperand(first, second, isGreater)

    if (isGreater .EQ. 1) then
        do
            ! add the second operand to the sum
            call addSubtract(second, sum, currentResult, '+')
            call flipList(currentResult)

            ! if the current result is greater than the first number, exit
            ! else, +1 to the counter (answer)
            call greaterOperand (first, currentResult, isGreater)
            call flipList(currentResult)
            if (isGreater .EQ. 2) then
                exit
            else
                counter = counter + 1
                call moveList(currentResult, sum)
            end if
        end do
        
        ! put the counter into the final result 
        do
            if (counter .EQ. 0) then
                exit
            end if

            digit = mod(counter, 10)
            counter = counter / 10 

            call insertNode(finalResult, digit)
        end do

        ! give the final result a sign
        if (firstSign .NE. secondSign) then
            finalResult%sign = '-'
        else
            finalResult%sign = '+'
        end if
    else if (isGreater .EQ. 2) then
        call insertNode(finalResult, 0)
        finalResult%sign = '+'
    else
        call insertNode(finalResult, 1)
        if (firstSign .NE. secondSign) then
            finalResult%sign = '-'
        else
            finalResult%sign = '+'
        end if
    end if
end subroutine divide

! this subroutine will find the factorial of the first operand
! returns the linked list for the result
subroutine factorial(first, result)
    ! declare parameter variables
    type(operand), intent(inout) :: first, result

    ! declare local variables
    type(operand) :: minusOne, second
    logical :: isZero

    ! initialize local variables
    minusOne%sign = '-'
    minusOne%numberStr = "-1"
    call insertNode(minusOne, 1)
    call addSubtract(first, minusOne, second, '+')
    call flipList(second)

    ! if the input was 0!, deal with it this way
    call allZeros(first, isZero)
    if (isZero .EQV. .TRUE.) then
        call insertNode(result, 1)
    else
        do
            ! if x - 1 = 0, copy first into the final result
            call allZeros(second, isZero)
            if (isZero .EQV. .TRUE.) then
                call moveList(first, result)
                exit
            end if

            ! multiply the first and second operand together to get a result
            call multiply(first, second, result)

            ! copy the result into the first operand
            call moveList(result, first)

            ! x - 1 again into the second operand
            call addSubtract(second, minusOne, result, '+')
            call flipList(result)
            call moveList(result, second)
            call flipList(second)
        end do
    end if
end subroutine factorial

! this subroutine calculates which number is greater than the other (by the head)
! returns an indicator of which is the greater number
subroutine greaterOperand(first, second, isGreater)
    ! declare parameter variables
    type(operand), intent(in) :: first, second
    integer, intent(inout) :: isGreater

    ! declare local variables
    type(node), pointer :: firstPtr, secondPtr

    ! initialize local variables
    firstPtr => first%head%endpoint
    secondPtr => second%head%endpoint

    ! find the greater number by the number of digits each operand has
    if (first%numDigits .GT. second%numDigits) then
        ! the first operand is greater
        isGreater = 1
    else if (first%numDigits .LT. second%numDigits) then
        ! the second operand is greater
        isGreater = 2
    else
        ! if both operands have the same number of digits,
        ! compare all of the numbers within it until we find 2 different values and compare those
        do
            if ((associated(firstPtr) .EQV. .FALSE.) .AND. (associated(secondPtr) .EQV. .FALSE.)) then
                ! there are no more numbers, which means they're exactly equal
                isGreater = 3
                exit
            else if (firstPtr%digit .GT. secondPtr%digit) then
                ! the first digit is greater
                isGreater = 1
                exit
            else if (firstPtr%digit .LT. secondPtr%digit) then
                ! the second digit is greater
                isGreater = 2
                exit
            else
                ! go onto the next pair of numbers
                firstPtr => firstPtr%next
                secondPtr => secondPtr%next
            end if
        end do 
    end if
end subroutine greaterOperand

! this subroutine calculates the result for adding a (+)operand and (-)operand together (by the tail)
! returns both operands and the result 
subroutine oppositeSigns(biggerNum, smallerNum, result)
    ! declare parameter variables
    type(operand), intent(inout) :: biggerNum, smallerNum, result
    
    ! declare local variables
    type(node), pointer :: biggerPtr, smallerPtr, nextPtr
    integer :: sum

    ! initialize local variables
    biggerPtr => biggerNum%tail%endpoint
    smallerPtr => smallerNum%tail%endpoint

    ! the result's sign is whatever the biggest number's sign is
    result%sign = biggerNum%sign

    ! calculate the sum
    do
        ! add the rest of the bigger number's digits to the result when the smaller number has no more digits
        if (associated(smallerPtr) .EQV. .FALSE.) then
            do 
                ! exit once there are no more digits in the bigger number
                if (associated(biggerPtr) .EQV. .FALSE.) then
                    exit
                end if
                
                ! keep adding leftover digits from the bigger number
                call insertNode(result, biggerPtr%digit)
                biggerPtr => biggerPtr%previous
            end do
            exit
        else
            ! if the bigger number has a smaller digit, take 10 from its next number (that isn't a 0)
            if (biggerPtr%digit .LT. smallerPtr%digit) then
                nextPtr => biggerPtr%previous
                do
                    if (nextPtr%digit .EQ. 0) then
                        nextPtr%digit = 9
                        nextPtr => nextPtr%previous
                    else
                        nextPtr%digit = nextPtr%digit - 1
                        exit
                    end if
                end do
                biggerPtr%digit = biggerPtr%digit + 10
            end if

            ! calculate the final sum
            sum = biggerPtr%digit - smallerPtr%digit
            
            ! put the number into the result
            call insertNode(result, sum)

            ! go onto the next digit
            biggerPtr => biggerPtr%previous
            smallerPtr => smallerPtr%previous
        end if
    end do
end subroutine oppositeSigns

! this subroutine adds the left over numbers to the result (by the tail)
! returns the linked list's tail pointer, result, and remainder
subroutine sameSigns(listPtr, result, remainder)
    ! declare parameter variables
    type(node), pointer, intent(inout) :: listPtr
    type(operand), intent(inout) :: result
    integer, intent (inout) :: remainder
    
    ! declare local variable
    integer :: sum
    
    do 
        ! exit once there are no more numbers (returns NULL)
        if (associated(listPtr) .EQV. .FALSE.) then
            exit
        end if

        ! calculate the sum
        sum = listPtr%digit + remainder

        ! split the remainder from the sum
        remainder = sum / 10
        sum = mod(sum, 10)

        ! put the number into the result
        call insertNode(result, sum)

        ! go onto the next node's digit
        listPtr => listPtr%previous
    end do
end subroutine sameSigns

! this subroutine checks if the linked list is filled with only zeros (by the tail)
! returns true or false
subroutine allZeros(list, isZero)
    ! declare parameter variables
    type(operand), intent(in) :: list
    logical, intent(inout) :: isZero

    ! declare local variable
    type(node), pointer :: listPtr
    
    ! initialize variable
    listPtr => list%tail%endpoint
    
    do 
        ! exit once there are no more numbers (returns NULL)
        if (associated(list%tail%endpoint) .EQV. .FALSE.) then
            ! list is empty
            isZero = .FALSE.
            exit
        else if (associated(listPtr) .EQV. .FALSE.) then
            ! all zeros
            isZero = .TRUE.
            exit
        else if (listPtr%digit .NE. 0) then
            ! not all zeros
            isZero = .FALSE.
            exit
        else
            ! go onto the next node's digit
            listPtr => listPtr%previous
        end if
    end do
end subroutine allZeros

end program unbounded