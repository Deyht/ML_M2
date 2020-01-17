
!######################## ##########################
!                Utility module
!######################## ##########################
module functions
	implicit none
	!Contain all the shared data between the different 
	!functions used during the program main loop
	integer, parameter :: nb_train = 100, nb_test = 78, nb_in = 13, nb_h = VAL, nb_out = 3, nb_epoch = VAL
	real, dimension(nb_train,nb_in+1) :: input
	real, dimension(nb_train,nb_out) :: targ
	real, dimension(nb_test,nb_in+1) :: input_test
	real, dimension(nb_test,nb_out) :: targ_test
	real, dimension(nb_out) :: output, delta_o
	real, dimension(nb_h+1) :: hidden
	real, dimension(nb_h) :: delta_h
	real :: h, rdm, learn_rate = VAL, beta = VAL
	real, dimension(nb_in+1,nb_h) :: weights1
	real, dimension(nb_h+1,nb_out) :: weights2
	real, dimension(nb_in+1) :: temp_input
	real, dimension(nb_out) :: temp_targ

contains
	


	!######################## ##########################
	!         Network forward phase function
	!######################## ##########################
	subroutine forward(input_vect)
		!Forward ONE input vector (defined by i)
		!through the network and update the output vector values
		real, dimension(nb_in+1), intent(in) :: input_vect
		integer :: j, k 
		
		do j=1,nb_h
			h = 0.0
			do k=1, nb_in+1
				h = h + weights1(k,j)*input_vect(k)
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
	
	end subroutine forward
	
	
	!######################## ##########################
	!        Network backward phase function
	!######################## ##########################
	subroutine backward(i)
		!Compute the error gradient and use it to
		!propagate the weight update through the network
		integer, intent(in) :: i
		integer :: j,k
	
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
	
	
	end subroutine backward
	
	
	!######################## ##########################
	!        Confusion Matrix Compute function
	!######################## ##########################
	subroutine confusion_matrix
		integer :: accu
		integer, dimension(nb_out,nb_out) :: confmat
		integer, dimension(1) :: max_a, max_b
		integer :: i, j
		real :: quad_error
		real, dimension(nb_out) :: recall, precis
		
		quad_error = 0
		accu = 0
		confmat = 0
		do i=1, nb_test
			!Forward phase for one input vector
			call forward(input_test(i,:))

			!update the confusion matrix based on the
			!current difference between target and output
			do j=1, nb_out
				quad_error = quad_error + 0.5*(output(j) - temp_targ(j))**2
			end do
			
			temp_targ = targ_test(i,:)
			max_a = maxloc(output)
			max_b = maxloc(temp_targ)
			confmat(max_b(1), max_a(1)) = confmat(max_b(1), max_a(1)) + 1
			if(max_a(1) == max_b(1)) then
				accu = accu + 1
			end if
		end do
		
		do i=1, nb_out
			recall(i) = 0
			precis(i) = 0
			do j =1, nb_out
				recall(i) = recall(i) + confmat(i,j)
				precis(i) = precis(i) + confmat(j,i)
			end do
			recall(i) = confmat(i,i) / recall(i) * 100.0
			precis(i) = confmat(i,i) /precis(i) * 100.0
		end do
		
		write(*,*) "Confmat :                             Recall"
		do i=1,nb_out
			write(*,*) confmat(i,:), recall(i)
		end do
		write(*,*) "Precision : " , precis(:),"  Accuracy :", real(accu/real(nb_test))*100.0
		write(*,*)
		write(*,*) "Mean quadratic error of output layer : ", quad_error/real(nb_test)
		
	
	end subroutine confusion_matrix
	
	
end module


!######################## ##########################
!                  Main program
!######################## ##########################
program multi_layer_perceptron
	use functions
	implicit none
	integer :: i, j, k, t, temp_class
	real :: quad_error

	!load the train dataset
	open(10, file="wine_train.data")
	
	targ(:,:) = 0
	!read the data
	do i=1, nb_train
		read(10,*) temp_class, input(i,1:nb_in)
		targ(i, temp_class) = 1.
	end do
	!add the bias node in the input vector
	input(:,nb_in+1) = -1.0
	
	
	
	!load the test dataset
	open(11, file="wine_test.data")
	
	do i=1, nb_test
		read(11,*) temp_class, input_test(i,1:nb_in)
		targ_test(i, temp_class) = 1.
	end do
	!add the bias node in the input vector
	input_test(:,nb_in+1) = -1.0



	!initialize the network weights to small values
	call random_number(weights1)
	call random_number(weights2)
	weights1(:,:) = weights1(:,:)*VAL
	weights2(:,:) = weights2(:,:)*VAL

	!######################## ##########################
	!                Main training loop
	!######################## ##########################
	do t = 1, nb_epoch
		if(mod(t,10) == 0) then
			write(*,*)
			write(*,*) "**************************************************"
			write(*,*) "Iteration :", t
			! Testing the result of the network with a forward
			! and printing it in the form of a confusion matrix
			call confusion_matrix()
		end if
		

		!######################## ##########################
		!             Training on all data once
		!######################## ##########################
		quad_error = 0.0
		do i=1, nb_train
			!Forward phase
			call forward(input(i,:))
			
			!Backpropagation phase
			call backward(i)
			
			do j=1, nb_out
				quad_error = quad_error + 0.5*(output(j) - targ(i,j))**2
			end do
			
		end do
		if(mod(t,10) == 0) then
			write(*,*) "Train set mean quadratic error of output layer : ", quad_error/real(nb_train)
		end if
	end do
	
end program multi_layer_perceptron

