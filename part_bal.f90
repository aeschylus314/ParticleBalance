!   Name    : part_bal 
!   Function: Provides multi-species particle balance by grid zone. Includes total particles,
!		total neutral particles, and total recycling sources for each species.	
!   Contains:
!   Subroutines  : 	part_bal(NSpecies, NZone)
!			plasma_balance
!			neutral_balance
!  
!   Author: Ian Waters --iwaters@wisc.edu
!   GitHub Page: https://github.com/aeschylus314/ParticleBalance


subroutine part_bal(NSpecies, NZone)
IMPLICIT NONE

integer, intent(in) :: NSpecies ! Total number of species in the model
integer, intent(in) :: NZone    ! Total number of zones in the model 

call plasma_balance(NSpecies, NZone)

end subroutine part_bal

subroutine plasma_balance(NSpecies, NZone)
USE PLASMA_PARAM
USE PHYSICAL_CELL
USE SOURCE_V_PL
USE BOUNDARY_COND
IMPLICIT NONE
integer, intent(in) :: NSpecies
integer, intent(in) :: NZone
integer :: i, j, k, l
real :: PTot
real, dimension (NSpecies,0:NZone-1,4) :: Particle_Array ! Column 1 = Zone # Column 2 = # of plasma particles
						! Column 3 = Recycling Flux
						! Column 4 = local tau_p* effective

open (314, file = 'Plasma_Balance.dat')

! This nested do loop provides the plasma total particle count per zone and puts it
! in the second column of the Particle_Array matrix and the recycling flux per zone
! in the third column of the matrix. 
! *** Fill in information to do this for each species
do j=1, NSpecies	
!Initializes Paticle_Array with zeroes and Zone #s
! *** Add additional loop to initialize with multiple arrays (3D array?) for each species	
	do k=0, NZone-1
		Particle_Array(j,k,1:4) = (/ k, 0, 0, 0 /)
	end do
! This nested do loop provides the plasma total particle count per zone and puts it
! in the second column of the Particle_Array matrix and the recycling flux per zone
! in the third column of the matrix. 	
	do i = 1, NC_PL
		do l=0, NZone-1
			if (IZCELL(i) == l) then
				Particle_Array(j,l, 2) = Particle_Array(j,l, 2) + VOLCEL(i)*DENS0(i,j)
				Particle_Array(j,l, 3) = Particle_Array(j,l, 3) + VSOUP0(i,j)*VOLCEL(i)*PFLUX_TOTAL(j)
					
			endif
			  
		end do
	end do
! This loop calculates the effective confinement time by zone and puts it
! in column 4 of Particle_Array Matrix
	do l=0, NZone-1
		Particle_Array(j,l,4) = Particle_Array(j,l,2) / (Particle_Array(j,l,3)*6.241E+18)
	end do
end do

! Writes a semi-formatted version of the matrix to the file.
! The Zone #'s would look better if I could figure out how to make them integers in the print out.
write(314, *) 'Zone #					NTot			FluxTot			Tau_p_Eff'

do j=1, NSpecies
	write(314, *) 'Species ', j
		
	do l=0, NZone-1
		write(314, *) 'Zone', Particle_Array(j,l,1), Particle_Array(j,l,2), Particle_Array(j,l,3), Particle_Array(j,l,4)
	end do
end do

close(314)

end subroutine plasma_balance
