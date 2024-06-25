! Program: Dynamic Linked List
! Creator: Jessica Nguyen
! Date: 2024-03-26
! Purpose: A module used for linked lists in unbounded.f03.

module dynllist
    ! declare an implicit type
    implicit none

    ! create a linked list structure
    type :: node
        integer :: digit
        type(node), pointer :: next => NULL()
        type(node), pointer :: previous => NULL()
    end type

    type :: linkedList
        type(node), pointer :: endpoint => NULL()
    end type
    
    ! create a structure for each operand
    type :: operand
        character(len=1) :: sign
        character(len=1000) :: numberStr
        integer :: numDigits = 0
        type(linkedList) :: head
        type(linkedList) :: tail
    end type

contains

    ! this subroutine inserts a new digit into the linked list (by the head)
    ! returns the linked list
    subroutine insertNode(list, digit)
        ! declare parameter variables
        type(operand), intent(inout) :: list
        integer, intent(in) :: digit

        ! declare local variables
        type(node), pointer :: newNode, listPtr
        
        ! initialize local variables
        allocate(newNode)
        listPtr => list%head%endpoint

        ! add the digit to the new node
        newNode%digit = digit

        ! update the tail to the new node
        list%tail%endpoint => newNode

        ! add the node to the end of the linked list
        do
            ! check if the pointer is null (aka. at the end of the list)
            if (associated(list%head%endpoint) .EQV. .FALSE.) then 
                ! if the head is null, add the node to the head
                list%head%endpoint => newNode
                exit
            else if (associated(listPtr%next) .EQV. .FALSE.) then
                ! if the linked list's node's next pointer is null, attach the new node to it
                listPtr%next => newNode
                newNode%previous => listPtr
                exit
            else
                ! if it's not at the end of the linked list, go onto the next node
                listPtr => listPtr%next
            end if
        end do

        ! add one to the number of digits in this operand
        list%numDigits = list%numDigits + 1
    end subroutine insertNode

    ! this subroutine empties the entire list (by the head)
    ! returns the linked list
    subroutine emptyList(list)
        ! declare parameter variable
        type(operand), intent(inout) :: list

        ! declare local variable
        type(node), pointer :: listPtr

        ! free all allocated nodes
        do
            ! exit when we're at the end of the linked list (returning NULL)
            if (associated(list%head%endpoint) .EQV. .FALSE.) then 
                exit
            else
                listPtr => list%head%endpoint
                list%head%endpoint => list%head%endpoint%next
                deallocate(listPtr)
            end if
        end do
        
        ! reset operand
        list%sign = ''
        list%numberStr = ''
        list%numDigits = 0
        list%head%endpoint => NULL()
        list%tail%endpoint => NULL()
    end subroutine emptyList

    ! this subroutine moves all of the data from one linked list to another (by the tail)
    ! returns the linked lists
    subroutine moveList(source, destination)
        ! declare parameter variables
        type(operand), intent(inout) :: source, destination

        ! declare local variable
        type(node), pointer :: listPtr

        ! initialize local variable
        listPtr => source%tail%endpoint

        ! make sure the destination is empty
        call emptyList(destination)

        ! move the list over
        do
            ! exit when we're at the end of the source's linked list (returning NULL)
            if (associated(listPtr) .EQV. .FALSE.) then 
                exit
            else
                call insertNode(destination, listPtr%digit)
                listPtr => listPtr%previous
            end if
        end do

        ! copy other parts of the struct
        destination%sign = source%sign 
        destination%numberStr = source%numberStr
        destination%numDigits = source%numDigits

        ! empty the source operand
        call emptyList(source)
    end subroutine moveList

    ! this subroutine copies all of the data from one linked list to another (by the tail)
    ! returns the linked lists
    subroutine copyList(source, destination)
        ! declare parameter variables
        type(operand), intent(inout) :: source, destination

        ! declare local variable
        type(node), pointer :: listPtr

        ! initialize local variable
        listPtr => source%tail%endpoint

        ! make sure the destination is empty
        call emptyList(destination)

        ! move the list over
        do
            ! exit when we're at the end of the source's linked list (returning NULL)
            if (associated(listPtr) .EQV. .FALSE.) then 
                exit
            else
                call insertNode(destination, listPtr%digit)
                listPtr => listPtr%previous
            end if
        end do

        ! copy other parts of the struct
        destination%sign = source%sign 
        destination%numberStr = source%numberStr
        destination%numDigits = source%numDigits
    end subroutine copyList

    ! this subroutine flips the entire linked list, where the head and tail switch sides (by the head)
    ! returns the linked list
    subroutine flipList(list)
        ! declare parameter variable
        type(operand), intent(inout) :: list

        ! declare local variables
        type(operand) :: tempList
        type(node), pointer :: listPtr

        ! initialize local variables
        tempList%sign = list%sign
        tempList%numberStr = list%numberStr
        listPtr => list%head%endpoint

        ! copy the linked list over to a temporary linked list
        do
            ! exit when we're at the end of the original linked list (returning NULL)
            if (associated(listPtr) .EQV. .FALSE.) then 
                ! move the flipped list from the temporary list to the original list
                call moveList(tempList, list)
                exit
            else
                call insertNode(tempList, listPtr%digit)
                listPtr => listPtr%next
            end if
        end do
    end subroutine flipList

    ! this subroutine displays the digits of an operand in a linked list (by the tail)
    subroutine printList(list)
        ! declare parameter variables
        type(operand), intent(in) :: list

        ! declare local variable
        type(node), pointer :: listPtr
        
        ! initialize local variable
        listPtr => list%tail%endpoint

        ! display the sign if it's negative
        if (list%sign .EQ. '-') then
            write (*,'(A)',advance='no') list%sign
        end if

        if (associated(list%tail%endpoint) .EQV. .TRUE.) then
            ! skip the left most zeros from being displayed
            if (associated(listPtr%previous) .EQV. .TRUE.) then
                do
                    ! skip zeros
                    if (listPtr%digit .EQ. 0) then
                        listPtr => listPtr%previous
                    else
                        exit
                    end if

                    ! if we're at the end of the linked list, exit
                    if (associated(listPtr) .EQV. .FALSE.) then 
                        exit
                    end if
                end do
            end if

            ! display the digits in the linked list
            do
                ! exit when we're at the end of the linked list (returning NULL)
                if (associated(listPtr) .EQV. .FALSE.) then 
                    exit
                else
                    write (*,'(I0)',advance='no') listPtr%digit
                    listPtr => listPtr%previous
                end if
            end do
        end if
    end subroutine printList

end module dynllist