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
