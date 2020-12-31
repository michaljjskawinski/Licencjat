##########################################################
#           | SYMULACJA TKANKI TŁUSZCZOWEJ |        
#                   Stworzono: 31.12.2020
#   Funkcja dyfuzji pochodzi z:
#   https://scipython.com/book/chapter-7-matplotlib/examples/the-two-dimensional-diffusion-equation/
#
##########################################################

import numpy as np
import matplotlib.pyplot as plt
import time

# Table size
w = h = 10.
# Intervals in x-, y- directions, mm
dx = dy = 0.1
# Diffusivity parameter 
D = 4.

nx, ny = int(w/dx), int(h/dy)
dx2, dy2 = dx*dx, dy*dy
dt = dx2 * dy2 / (2 * D * (dx2 + dy2))


# Starting numpy table with nothin in it
u = 0 * np.ones((nx, ny)) 


# Some sort of function to determinate starting LPS
u[50,50] = 50
u[70,70] = 50


# Diffusion function per timestep
def diffuse_in_timestep(u):
    # Propagate with forward-difference in time, central-difference in space
    u0 = u.copy()
    u[1:-1, 1:-1] = u0[1:-1, 1:-1] + D * dt * (
          (u0[2:, 1:-1] - 2*u0[1:-1, 1:-1] + u0[:-2, 1:-1])/dx2
          + (u0[1:-1, 2:] - 2*u0[1:-1, 1:-1] + u0[1:-1, :-2])/dy2 )
    return u

# Number of timesteps
nsteps = 101

for m in range(nsteps):
    u = diffuse_in_timestep(u)
    time.sleep(0.1)
    
plt.imshow(u)
plt.show()






