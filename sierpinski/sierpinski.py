# -*- coding: utf-8 -*-

#  TODO: it somehow works, now make it pretty

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# starting vertices
v = 3
# steps
s = 1000
# triangle
t = np.zeros((s, 2))

lo = 1
hi = 10

for i in range(v+1):
    t[i] = np.random.uniform(lo, hi, 2)



fig = plt.figure()
ax = fig.add_subplot(111, aspect='equal', autoscale_on=False,
                     xlim=(1, 10), ylim=(1, 10))

PLOT, = ax.plot([], [], 'r.')

def step(i):
    if not i%100:
        print(i)
    i += (v+1)
    r = t[np.random.randint(0, v)]
    t[i] = (r + t[i-1]) / 2
    PLOT.set_data(t[:,0], t[:,1])
    return PLOT,

def init():
    PLOT.set_data([], [])
    return PLOT,

a = animation.FuncAnimation(fig, step, frames=1000, interval=10,
                            blit=True,
                            init_func=init)

plt.show()
