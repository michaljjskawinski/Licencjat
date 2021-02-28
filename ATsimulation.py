##########################################################
#	| SYMULACJA TKANKI TŁUSZCZOWEJ |	
#	Stworzono: 31.12.2020
#   
#	Funkcja dyfuzji pochodzi z:
#   https://scipython.com/book/chapter-7-matplotlib/examples/the-two-dimensional-diffusion-equation/
#
#	TO DO:
#	- Tkanka tłuszczowa ma pochłaniać LPS
#	- Śmierć komórki tłuszczowej - maksymalny rozmiar, liza do otoczenia
#	- Input do symulacji ???
#	- Output ??
#	- Napływ LPS z zewnątrz: funkcja [narazie hardcoded]
##########################################################

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import time
import random 

# `Romiar tabel
w = h = 10.
# Interwały  x-, y- kierunkach, 
dx = dy = 0.1
# Stopień dyfuzji
D = 4.

nx, ny = int(w/dx), int(h/dy)
dx2, dy2 = dx*dx, dy*dy
dt = dx2 * dy2 / (2 * D * (dx2 + dy2))


# Inicjaja początkowych gridów
u = np.zeros((ny, nx))  #2D grid umiejscowienia+zawartości LPS (wiersz/kolumna)
AT_u = np.zeros((ny, nx)) #2D grid umiejsowienia komórek AT (wiersz/kolumna)

# Umiejscowienie i liczba startowych LPS (narazie harcoded, potem jakaś funkcja)
u[50,50] = 50
u[70,70] = 50

# AT cell
class AT_cell:

	def __init__(self,x,y,LPS_container):
		self.x = x
		self.y = y
		self.LPS_container = LPS_container
		self.alive = True

	def move(self, AT_u0):
		AT_u_new = AT_u0.copy()
		x = self.x
		y = self.y
		while(1):
			val = random.randint(1, 4) 
			if val == 1: 		#UP
				if y == ny-1:
					continue
				elif AT_u_new[y+1,x] != 0:
					continue
				else:
					AT_u_new[y+1,x] = 1
					AT_u_new[y,x] = 0
					self.y = y+1
					break
			elif val == 2: 		#DOWN
				if y == 0:
					continue
				elif AT_u_new[y-1,x] != 0:
					continue
				else:
					AT_u_new[y-1,x] = 1
					AT_u_new[y,x] = 0
					self.y = y-1
					break
			elif val == 3: 		#RIGHT
				if x == nx-1:
					continue
				elif AT_u_new[y,x+1] != 0:
					continue
				else:
					AT_u_new[y,x+1] = 1
					AT_u_new[y,x] = 0
					self.x = x+1
					break
			elif val == 4: 		#LEFT
				if x == 0:
					continue
				elif AT_u_new[y,x-1] != 0:
					continue
				else:
					AT_u_new[y,x-1] = 1
					AT_u_new[y,x] = 0
					self.x = x-1
					break
		return AT_u_new

	def eat_LPS(self, u0):
		u_new = u0.copy()
		
		#... tutaj wchlania  stałą liczbę LPS
		if self.LPS_container > 100:#narazie stała maksymalna pojemność LPS, po której liza
			self.alive = False
		return u_new

	def lysis(self, u0):
		u_new = u0.copy()
		return u_new


AT_cells_num = 1	#Liczba komórek 
AT_cells_list = []	#Lista komórek 

for cell in range(AT_cells_num):
	while (1):
		AT_x0 = random.randint(0, nx-1)
		AT_y0 = random.randint(0, ny-1)
		if  AT_u[AT_y0, AT_x0] == 0:
			AT_u[AT_y0, AT_x0] = 1
			AT_cells_list.append( AT_cell(AT_x0, AT_y0, 0) )
			break
		else:
			continue




# Funkcja dyfuzji per timestep
def diffuse_in_timestep(u):
	# Propagate with forward-difference in time, central-difference in space
	u0 = u.copy()
	u[1:-1, 1:-1] = u0[1:-1, 1:-1] + D * dt * (
	  (u0[2:, 1:-1] - 2*u0[1:-1, 1:-1] + u0[:-2, 1:-1])/dx2
	  + (u0[1:-1, 2:] - 2*u0[1:-1, 1:-1] + u0[1:-1, :-2])/dy2 )
	return u





#SYMULACJA
nsteps = 101	#Liczba kroków

def simulate(u, AT_u, nsteps):
	for step in range(nsteps):
		u = diffuse_in_timestep(u)
		for cell in AT_cells_list:
			AT_u = cell.move(AT_u)
			print("x={}\ty={}".format(cell.x,cell.y))
		 	u = cell.eat_LPS(u)
		 	if cell.alive = False:
		 		u = cell.lysis(u)
		 		AT_cells_list.remove(cell)
		fig = plt.imshow(AT_u)
		plt.pause(0.05)
		fig.remove()

		#fig2 = plt.imshow(u)
		#plt.pause(0.05)
		#fig2.remove()

	return u, AT_u



#MAIN
u, At_u = simulate(u, AT_u, nsteps)





#ncoś zakodowane jakiś plot aniowany
#def init():
#    fig = plt.imshow(AT_u)
#    return fig,
#def animate(i):
#    u = diffuse_in_timestep(u)
#    AT_u,  = move_AT_cell(AT_x0, AT_y0)
#    fig = plt.imshow(u)
#    return fig,
#
#anim = FuncAnimation(fig, animate, init_func=init,
#                               frames=200, interval=nsteps, blit=True)





