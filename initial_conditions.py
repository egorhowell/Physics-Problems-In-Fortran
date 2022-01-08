#!/usr/bin/env python
# coding: utf-8

# In[43]:


import numpy as np

# Ouput what the code does
print('This code generates initial conditions for the relative motion of the Sun-Earth system and for the')
print('Sun-Mercury system, treating each two-body problem as an equivalent one-body problem.')
print('')

# Paramters
G = 1
R = 1
m_sun = 1

print('The parameters are: G =',G,', M =',m_sun,'* M_sun and R =',R,'AU')
print('')


# -------- This section is for the earth's initial conditons -----------------


# Earth variables
a_earth = 1
e_earth = 0.0167
m_earth = 3.0024584e-6
M_earth_tot = m_sun + m_earth

# Computation of the apocentre and pericentre for Earth
apoc_earth = a_earth*(1+e_earth)
peri_earth = a_earth*(1-e_earth)

# Computation of velocity at pericentre and apocentre for Earth
Vp_earth = ( (G*M_earth_tot/a_earth) * ((1+e_earth)/(1-e_earth)) )**0.5
Va_earth = ( (G*M_earth_tot/a_earth) * ((1-e_earth)/(1+e_earth)) )**0.5

# Computation of the inital conditions in cartesian coordinates for Earth
earth_pos = np.array([apoc_earth, 0, 0])
earth_vel = np.array([0, Va_earth, 0])

# writing the inital values to a file
f = open("earth_initial.txt", "w")
np.savetxt("earth_initial.txt", np.array([[apoc_earth,0,0], [0,Va_earth,0]]), fmt="%s")
f.close()

print('initial positions of the earth-sun system in x,y,z: ', earth_pos, 'AU')
print('initial velocities of the earth-sun system in Vx,Vy,Vz: ', earth_vel, 'AU / unit time ')
print('these values were written to the file: earth_initial.txt')
print('')


# -------- This section is for the mercury's initial conditons -----------------


# Mercury variables
a_merc = 0.387098
e_merc = 0.205635
m_merc = 1.65956463e-7
M_merc_tot = m_sun + m_merc

# Computation of the apocentre and pericentre for Mercury
apoc_merc = a_merc*(1+e_merc)
peri_merc = a_merc*(1-e_merc)

# Computation of velocity at pericentre and apocentre for Mercury
Vp_merc = ( (G*M_merc_tot/a_merc) * ((1+e_merc)/(1-e_merc)) )**0.5
Va_merc = ( (G*M_merc_tot/a_merc) * ((1-e_merc)/(1+e_merc)) )**0.5

# Computation of the inital conditions in cartesian coordinates for Mercury
merc_pos = np.array([apoc_merc, 0, 0])
merc_vel = np.array([0, Va_merc, 0])

# Writing the initial values to a file
f = open("merc_initial.txt", "w")
np.savetxt("merc_initial.txt", np.array([[apoc_merc,0,0], [0,Va_merc,0]]), fmt="%s")
f.close()

print('initial positions of the mercury-sun system in x,y,z: ', merc_pos, 'AU')
print('initial velocities of the mercury-sun system in Vx,Vy,Vz: ', merc_vel, 'AU / unit time')
print('These values were written to the file: merc_initial.txt')

