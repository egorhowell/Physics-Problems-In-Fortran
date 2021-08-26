#!/usr/bin/env python
# coding: utf-8

# In[2]:


import numpy as np
import matplotlib.pyplot as plt

a1 = 10
b1 = 2.001
a2 = -10
b2 = 1.999

f = lambda x : np.sin(x)/(x**3 - 2*x**2 + 4*x - 8)

x1 = np.linspace(b1,a1,10000)
x2 = np.linspace(a2,b2,10000)

y1 = f(x1)
y2 = f(x2)

dx1 = (x1[0] - x1[-1])/len(x1)
dx2 = (x2[0] - x2[-1])/len(x2)

plt.plot(x1,y1)
plt.plot(x2,y2)   

def trapezoid(f, a, b, n):
    deltax = float(b - a)/(n)
    h = float(b - a) / n
    s = 0.0
    s += f(a)/2.0
    for i in range(1, n):
        s += f(a + i*h)
    s += f(b)/2.0
    return s * h


# In[2]:


# read in packages
import numpy as np

# defining the Trapezoidal method function
def Trapezoid(f, a, b, N):
    
    # spacing between trapzoids
    h = (b-a)/float(N)
    
    # compute start and end point
    u = 0.5*(f(a) + f(b))
    
    # loop over the rest of the points
    for i in range(1,N):
        u = u + f(a + i*h)
    
    # return the value of the integral
    return h*u

# the integral
f = lambda x : np.sin(x)/(x**3 - 2*x**2 + 4*x - 8)

# outputting the result of the integral using the function twice to take into account the pole
print('The Integral =', Trapezoid(f,-15,1.99999,1000000000) + Trapezoid(f, 2.00001, 15, 1000000000))


# In[21]:


import numpy as np
import matplotlib.pyplot as plt

def Trapezoidal(f, a, b, n):
    
    h = (b-a)/float(n)
    s = 0.5*(f(a) + f(b))
    for i in range(1,n,1):
        s = s + f(a + i*h)
    return h*s

# the integral
f = lambda x : np.sin(x)/(x**3 - 2*x**2 + 4*x - 8)

array = []
N = np.logspace(0,7,8)
number = []

for i in range(len(N)):
    
    num = N[i]
    
    # outputting the result of the integral using the function twice to take into account the pole
    Intgeral = Trapezoidal(f,-15,1.99999,int(num)) + Trapezoidal(f, 2.00001, 15,int(num))
    g = abs(Intgeral + 0.21657)
    array.append(g)
    number.append(num)
    
    
print(number,array)


# In[29]:


x = np.array([1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, 10000000.0])
y = np.array([22733.84900196802, 2273.2148388877436, 227.29352285307806, 22.701614861480866, 2.242723852128967, 0.19954634472387825, 0.00849103514794361, 0.0002501528175569978])
plt.figure(0)
plt.plot(x,y,color='black')
plt.xscale('log')
plt.yscale('log')
plt.ylabel('Numerical Error',fontsize=14)
plt.xlabel('Number of Trapezoids',fontsize=14)
plt.xticks(fontsize=14)
plt.yticks(fontsize=14)
plt.savefig('myfile.png', bbox_inches = "tight")


# In[30]:


print(0.21620670859666702 - 0.21657)


# In[4]:


# read in packages
import numpy as np

# defining the Trapezoidal method function
def Trapezoid(f, a, b, N):
    
    # spacing between trapzoids
    h = (b-a)/float(N)
    
    # compute start and end point
    u = 0.5*(f(a) + f(b))
    
    # loop over the rest of the points
    for i in range(1,N):
        u = u + f(a + i*h)
    
    # return the value of the integral
    return h*u

# the integral
f = lambda x : np.sin(x)/(x**3 - 2*x**2 + 4*x - 8)

# outputting the result of the integral
# using the function twice to take into account the pole
I1 = Trapezoid(f,-15,1.99999,1000000000)
I2 = Trapezoid(f, 2.00001, 15, 1000000000)
print('The Integral =', (I1 + I2))


# In[ ]:




