!############################### ################################
! K-means exercise for the M2-P2 Machine Learning lessons
! contact : David Cornu - david.cornu@utinam.cnrs.fr
!############################### ################################


module utils
	implicit none

contains
	!This Function return a distance in ndim dimension
	!between two points (arguments are table with all the dimensions) 
	function dist(dat, cent, ndim)
		real :: dist
		real, intent(in) :: dat(:), cent(:)
		integer, intent(in) :: ndim
		integer :: i

		dist = 0.0
		do i = 1, ndim
			dist = dist + (dat(i)-cent(i))**2
		end do
	end function dist

end module utils



program kmeans

	use utils
	implicit none

	!Usefull data, feel free to add the ones you may need
	!for your own implementation of the algorithm
	real, allocatable :: input_data(:,:), centers(:,:), new_centers(:,:)
	integer, allocatable :: nb_points_per_center(:)
	integer :: nb_dim, nb_data
	real :: v_min, v_temp, all_dist, eps, rand
	integer :: nb_k, i_min
	integer :: i, j, l

	eps = 0.001
	nb_k = 4
	l = 0

	!This entry file must be edited to change to other
	!number of dimension. The code must be re-compiled !
	open(unit = 10, file="kmeans_input_file_2d.dat")

	!Read the dimensions of the data in the file
	read(10, *) nb_dim, nb_data
	allocate(input_data(nb_data, nb_dim))

	write(*,*) nb_dim, nb_data

	!Load all the data
	do i = 1, nb_dim
		read(10, *) input_data(:, i)
	end do

	close(10)

	!allocate the tables according to the dimension
	!gave in the input file
	allocate(centers(nb_k, nb_dim))
	allocate(new_centers(nb_k, nb_dim))
	allocate(nb_points_per_center(nb_k))

	!the origin of the centers are selected randomly
	!to the position of some points in the dataset
	do i=1, nb_k
		call random_number(rand)
		centers(i, :) = input_data(int(rand*nb_data),:)
	end do

	!############################### ################################
	!     Main loop, until the new centers do not move anymore
	!############################### ################################
	do
		l = l + 1
		
		!erase the data drom the previous iteration
		do i = 1, nb_k
			new_centers(i,:) = 0.0
		end do
		nb_points_per_center(:) = 0

		!############################### ################################
		!         Association phase, loop on the data points
		!############################### ################################
		do i = 1, nb_data

			!find the nearest point
			i_min = 1
			v_min = dist(input_data(i,:), centers(1,:), nb_dim)
			do j = 2, nb_k
				v_temp = dist(input_data(i,:), centers(j,:), nb_dim)
				if (v_temp <= v_min) then
					v_min = v_temp
					i_min = j
				end if
			end do
			
			!store the position of the point in the new centers in advance
			new_centers(i_min,:) = new_centers(i_min,:) + input_data(i,:)
			!store the number of points associated to this center
			nb_points_per_center(i_min) = nb_points_per_center(i_min) + 1

		end do !data point loop

		!############################### ################################
		!           Update phase, calculate the new centers
		!############################### ################################
		do i = 1, nb_k
			if(nb_points_per_center(i) /= 0) then
				new_centers(i,:) = new_centers(i,:) / nb_points_per_center(i)
			end if
		end do


		!Calculate the sum of distances between the centers and the new ones
		all_dist = 0.0
		do i = 1, nb_k
			all_dist = all_dist + dist(centers(i,:), new_centers(i,:), nb_dim)
		end do
		write(*,*) "Step :", l, " error :", all_dist


		!Effectivly move the centers by puting them at the centroids position
		do i = 1, nb_k
			if(nb_points_per_center(i) /= 0) then
				centers(i,:) = new_centers(i,:)
			end if
		end do


		!End the loop if the overall distance is less than a defined epsilon
		if(all_dist <= eps .OR. l >= 100) then
			exit
		end if

	end do !main loop

	!############################### ################################
	!      Save the ending centroid position for visualisation
	!############################### ################################	
	open(unit = 10, file="kmeans_output_2d.dat")
	
	write(10,*) nb_dim, nb_k
	
	do i = 1, nb_k
		write (10,*) centers(i, :)
	end do
	
	

end program kmeans








