program logic_or
	implicit none


	integer, parameter :: nb_dat = 768
	real, dimension(nb_dat,9) :: input
	integer, dimension(nb_dat) :: targ
	integer :: output, precis
	real :: h, learn_rate = 0.1, temp_targ, rdm
	real, dimension(9) :: weights, temp_input
	integer :: i, j, t, ind


	open(10, file="pima-indians-diabetes.data")

	do i=1, nb_dat
		read(10,*) input(i,1:8), targ(i)
	end do
	input(:,9) = -1.0

	
	do i = 0, nb_dat
		if(input(i,1) > 8) input(i,1) = 8
		input(i,8) = int(mod(input(i,8)-30,10.0))
		if(input(i,8) > 5) input(i,8) = 5
	end do	

	do i=1, 8
		input(:,i) = input(:,i)-(sum(input(:,i))/nb_dat)
		input(:,i) = input(:,i)/maxval(abs(input(:,i)))
	end do

	call random_number(weights)
	weights(:) = weights(:)*(0.02)-0.01

	!######################## ##########################
	!                Main training loop
	!######################## ##########################
	do t = 1, 100
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
		!               Fisher Yates Shuffle
		!######################## ##########################
		do i=1, nb_dat-1
			call random_number(rdm)
			ind = int(rdm * (nb_dat-i)) + i
			
			temp_input(:) = input(i,:)
			input(i,:) = input(ind,:)
			input(ind,:) = temp_input(:)

			temp_targ = targ(i)
			targ(i) = targ(ind)
			targ(ind) = temp_targ

		end do


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

