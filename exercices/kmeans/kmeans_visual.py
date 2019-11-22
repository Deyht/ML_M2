import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

f = open("kmeans_output_2d.dat")

line = f.readline()
words = line.split()

nb_dim = int(words[0])
nb_k = int(words[1])

centers = np.zeros((nb_dim, nb_k))

for i in range(0,nb_k):
	line = f.readline()
	words = line.split()
	centers[:,i] = words[:]

f.close()


f = open("kmeans_input_file_2d.dat")

line = f.readline()
words = line.split()
nb_data = int(words[1])
data = np.zeros((nb_dim, nb_data))

for i in range(0,nb_dim):
	line = f.readline()
	words = line.split()
	data[i,:] = words[:]

f.close()

distances = np.zeros((nb_k, nb_data))

for i in range(0,nb_data):
	for j in range(0,nb_k):
		for k in range(0, nb_dim):
			distances[j,i] += (centers[k, j] - data[k,i])**2

print distances[:, 0]

arg_min = np.zeros(nb_data)
for i in range(0,nb_data):
	arg_min[i] = np.argmin(distances[:,i])

if (nb_dim == 2 ):

	for i in range(0, nb_k):
		ind = np.where(arg_min[:] == i)
		plt.plot(data[0,ind], data[1,ind], '.', color = plt.cm.rainbow(float(nb_k-i)/nb_k))
	plt.plot(centers[0,:], centers[1,:], '.', markersize=14)
else :
	fig = plt.figure()
	ax = fig.add_subplot(111, projection="3d")
	for i in range(0, nb_k):
		ind = np.where(arg_min[:] == i)
		ax.scatter(data[0,ind], data[1,ind], data[2,ind], color = plt.cm.rainbow(float(nb_k-i)/nb_k))
	ax.scatter(centers[0,:], centers[1,:], centers[2,:], marker = "x", c = "k", s = 64, depthshade="False", linewidth=3)
	ax.set_xlabel("x")
	ax.set_ylabel("y")
	ax.set_zlabel("z")

plt.show()
#plt.savefig("kmeans_3d_res.png", dpi=200)
	


