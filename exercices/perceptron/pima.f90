program logic_or
	implicit none

	integer, parameter :: nb_dat = 768
	real, dimension(nb_dat,9) :: input
	integer, dimension(nb_dat) :: targ
	integer :: output, precis
	real :: h, learn_rate = 0.1
	real, dimension(9) :: weights
	integer :: i, j, t


	open(10, file="pima-indians-diabetes.data")

	do i=1, nb_dat
		read(10,*) input(i,1:8), targ(i)
	end do
	input(:,9) = -1.0


	call random_number(weights)
	weights(:) = weights(:)*(0.02)-0.01

	!######################## ##########################
	!                Main training loop
	!######################## ##########################
	do t = 1, 50
		write(*,*) "Iteration :", t
		!######################## ##########################
		! Testing the result of the network with a forward
		!######################## ##########################
		precis = 0.0
		do i=1, nb_dat
			!Forward phase
			h = 0.0
			do j=1, 9
				h = h + weights(j)*input(i,j)
			end do
			
			if(h > 0) then
				output = 1
			else
				output = 0
			end if
			if(output == targ(i)) then
				precis = precis + 1
			end if

		end do
		write(*,*) "Precision rate :", real(precis)/real(nb_dat)


		!######################## ##########################
		!             Training on all data once
		!######################## ##########################
		do i=1, nb_dat
			!Forward phase
			h = 0.0
			do j=1, 9
				h = h + weights(j)*input(i,j)
			end do
			
			if(h > 0) then
				output = 1
			else
				output = 0
			end if
			
			!Back-propagation phase
			do j=1, 9
				weights(j) = weights(j) - learn_rate*(output-targ(i))*input(i,j)
			end do
		end do
	end do
	
end program logic_or

