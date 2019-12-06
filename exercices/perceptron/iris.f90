program logic_or
	implicit none

	integer, parameter :: nb_dat = 150
	real, dimension(nb_dat,5) :: input
	real, dimension(5) :: temp_input
	integer, dimension(nb_dat,3) :: targ
	integer :: precis, temp_class
	integer, dimension(3) :: output, temp_targ
	real :: h, learn_rate = 0.005, rdm
	real, dimension(5,3) :: weights
	integer :: i, j, k, t, ind
	integer, dimension(1) :: max_a, max_b
	integer, dimension(3,3) :: confmat


	open(10, file="iris.data")
	
	targ(:,:) = 0
	do i=1, nb_dat
		read(10,*) input(i,1:4), temp_class
		targ(i, temp_class+1) = 1
	end do
	input(:,5) = -1.0

	do i=1, 4
		input(:,i) = input(:,i)-(sum(input(:,i))/nb_dat)
		input(:,i) = input(:,i)/maxval(abs(input(:,i)))
	end do


	call random_number(weights)
	weights(:,:) = weights(:,:)*(0.02)-0.01

	!######################## ##########################
	!                Main training loop
	!######################## ##########################
	do t = 1, 5000
		write(*,*) "Iteration :", t
		!######################## ##########################
		! Testing the result of the network with a forward
		!######################## ##########################
		precis = 0
		confmat = 0
		do i=1, nb_dat
			!Forward phase
			temp_targ = targ(i,:)
			do j=1,3
				h = 0.0
				do k=1, 5
					h = h + weights(k,j)*input(i,k)
				end do
				
				if(h > 0) then
					output(j) = 1
				else
					output(j) = 0
				end if
			end do
			max_a = maxloc(output)
			max_b = maxloc(temp_targ)
			confmat(max_b(1), max_a(1)) = confmat(max_b(1), max_a(1)) + 1
			if(max_a(1) == max_b(1)) then
				precis = precis + 1
			end if
		end do
		write(*,*) "Confmat : "
		do i=1,3
			write(*,*) confmat(i,:)
		end do		
		write(*,*) "Precision : ", real(precis)/real(nb_dat)


		!######################## ##########################
		!               Fisher Yates Shuffle
		!######################## ##########################
		do i=1, nb_dat-1
			call random_number(rdm)
			ind = int(rdm * (nb_dat-i)) + i
			
			temp_input(:) = input(i,:)
			input(i,:) = input(ind,:)
			input(ind,:) = temp_input(:)

			temp_targ(:) = targ(i,:)
			targ(i,:) = targ(ind,:)
			targ(ind,:) = temp_targ(:)

		end do


		!######################## ##########################
		!             Training on all data once
		!######################## ##########################
		do i=1, nb_dat
			!Forward phase
			do j=1,3
				h = 0.0
				do k=1, 5
					h = h + weights(k,j)*input(i,k)
				end do
				
				if(h > 0) then
					output(j) = 1
				else
					output(j) = 0
				end if
			end do
			
			!Back-propagation phase
			do k=1, 5
				do j=1,3
					weights(k,j) = weights(k,j) - learn_rate*(output(j)-targ(i,j))*input(i,k)
				end do			
			end do
		end do
	end do
	
end program logic_or

