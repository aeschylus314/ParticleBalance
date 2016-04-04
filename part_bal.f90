!C   Name    : part_bal 
!C   Function: Provides multi-species particle balance by grid zone. Includes total particles,
!		total neutral particles, and total recycling sources for each species.	
!C   Contains:
!C   Subroutines  : 	part_bal(NSpecies)
!			plasma_balance
!			recycling_balance
!			neutral_balance
!C   Functions    : None
!C   Use     : 
!C   Common blocks: None
!C   Modules      : None
!C


subroutine part_bal(NSpecies)
IMPLICIT NONE

integer, intent(in) :: NSpecies !Total number of species in the model

call plasma_balance(NSpecies)
call recycling_balance(NSpecies)

end subroutine part_bal

subroutine plasma_balance(NSpecies)
USE PLASMA_PARAM
USE PHYSICAL_CELL
IMPLICIT NONE
integer, intent(in) :: NSpecies
integer :: i, j
real :: PTot

open (314, file = 'Plasma_Balance.dat')

do j=1, NSpecies
PTot=0
do i = 1, NC_PL
PTot = PTot + DENS0(i,j)*VOLCEL(i)

end do
write (314, *) 'Total ions: Species ',j,': ', PTot
print*, 'Total ions: Species ',j,': ', PTot

end do

close(314)

end subroutine plasma_balance

! DOESN'T WORK AT ALL. FIX
subroutine recycling_balance(NSpecies)
USE PLASMA_PARAM
USE PHYSICAL_CELL
USE SOURCE_V_PL
IMPLICIT NONE
integer, intent(in) :: NSpecies
integer :: i, j
real :: RecTot

open (314, file = 'Recycling_Balance.dat')

do j=1, NSpecies
RecTot=0
do i = 1, NC_PL
RecTot = RecTot+VSOUP0(i,j)*VOLCEL(i)

end do
write (314, *) 'Total Recycling Source: Species ',j,': ', RecTot, 'Amps'

print*, 'Total Recycling Source: Species ',j,': ', RecTot, 'Amps'

end do

close(314)

end subroutine recycling_balance
