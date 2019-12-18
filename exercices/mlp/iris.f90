program logic_or
	implicit none

	integer, parameter :: nb_dat = 150, nb_in = 4, nb_h = 4, nb_out = 3
	real, dimension(nb_dat,nb_in+1) :: input
	real, dimension(nb_in+1) :: temp_input
	real, dimension(nb_dat,nb_out) :: targ
	integer :: precis, temp_class
	real, dimension(nb_out) :: output, temp_targ, delta_o
	real, dimension(nb_h+1) :: hidden
	real, dimension(nb_h) :: delta_h
	real :: h, learn_rate = 0.1, rdm, beta = 1.0
	real, dimension(nb_in+1,nb_h) :: weights1
	real, dimension(nb_h+1,nb_out) :: weights2
	integer :: i, j, k, t, ind
	integer, dimension(1) :: max_a, max_b
	integer, dimension(3,3) :: confmat


	open(10, file="iris.data")
	
	targ(:,:) = 0
	do i=1, nb_dat
		read(10,*) input(i,1:nb_in), temp_class
		targ(i, temp_class+1) = 1
	end do
	input(:,nb_in+1) = -1.0

	do i=1, nb_in
		input(:,i) = input(:,i)-(sum(input(:,i))/nb_dat)
		input(:,i) = input(:,i)/maxval(abs(input(:,i)))
	end do


	call random_number(weights1)
	call random_number(weights2)
	weights1(:,:) = weights1(:,:)*(0.02)-0.01
	weights2(:,:) = weights2(:,:)*(0.02)-0.01

	!######################## ##########################
	!                Main training loop
	!######################## ##########################
	do t = 1, 100
		write(*,*) "Iteration :", t
		!######################## ##########################
		! Testing the result of the network with a forward
		!######################## ##########################
		precis = 0
		confmat = 0
		do i=1, nb_dat
			!Forward phase
			do j=1,nb_h
				h = 0.0
				do k=1, nb_in+1
					h = h + weights1(k,j)*input(i,k)
				end do
				hidden(j) = 1.0/(1.0 + exp(-beta*h))
			end do
			hidden(nb_h+1) = -1.0

			do j=1, nb_out
				h = 0.0
				do k=1, nb_h+1
					h = h + weights2(k,j)*hidden(k)
				end do
				output(j) = 1.0/(1.0 + exp(-beta*h))
			end do

			temp_targ = targ(i,:)
			max_a = maxloc(output)
			max_b = maxloc(temp_targ)
			confmat(max_b(1), max_a(1)) = confmat(max_b(1), max_a(1)) + 1
			if(max_a(1) == max_b(1)) then
				precis = precis + 1
			end if
		end do
		write(*,*) "Confmat : "
		do i=1,nb_out
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
			do j=1,nb_h
				h = 0.0
				do k=1, nb_in+1
					h = h + weights1(k,j)*input(i,k)
				end do
				hidden(j) = 1.0/(1.0 + exp(-beta*h))
			end do
			hidden(nb_h+1) = -1.0

			do j=1, nb_out
				h = 0.0
				do k=1, nb_h+1
					h = h + weights2(k,j)*hidden(k)
				end do
				output(j) = 1.0/(1.0 + exp(-beta*h))
			end do
			
			!Back-propagation phase
			do j=1, nb_out
				delta_o(j) = beta*(output(j)-targ(i,j))*output(j)*(1.0-output(j))
			end do

			do j=1, nb_h
				h = 0.0
				do k=1,nb_out
					h = h + weights2(j,k)*delta_o(k)
				end do
				delta_h(j) = beta*hidden(j)*(1.0-hidden(j))*h
			end do


			do j=1, nb_in+1
				do k=1, nb_h
					weights1(j,k) = weights1(j,k) - learn_rate*(delta_h(k)*input(i,j))
				end do
			end do
			
			do j=1, nb_h+1
				do k=1, nb_out
					weights2(j,k) = weights2(j,k) - learn_rate*(delta_o(k)*hidden(j))
				end do
			end do
			
		end do
	end do
	
end program logic_or

