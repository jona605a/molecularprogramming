# Jonathan Højlev, 26/6 # Rani Ey. í Bø 26/06

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

def read_states_from_file(filename):
    steps = [[]]
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            line=line.strip()
            line = line.replace('\x00', "")
            if line == "NEXTSTEP":
                steps.append([])
            else:
                if len(line.split()) < 2: continue
                spec, conc = line.split()
                conc = float(conc)
                steps[-1].append((spec,conc))
    steps=steps[:-1]
    return steps

def plot_to_file(data, filename, names_to_include=None):
    data_names = [a for a,_ in data[0]]
    data = [[a for _,a in bla] for bla in data]
    data = np.array(data)
    data = data.T


    # Plot each species with a different color
    T = len(data[0])

    times = [0] + sum([[i-0.001,i] for i in range(1,T)], start=[])
    data = [sum([[i, i] for i in dat[:-1]], start=[]) + [dat[-1]] for dat in data]
    plt.figure(figsize=(10, 6))
    for i in range(len(data)):
        if names_to_include == None or data_names[i] in names_to_include:
            plt.plot(times, data[i], label=data_names[i], alpha = 0.5, linewidth=3)

    # Adding title and labels
    plt.title('Species Data Over Steps')
    plt.xlabel('Step')
    plt.ylabel('Value')
    plt.grid()
    plt.legend()

    # Show the plot
    plt.savefig(filename)



if len(sys.argv) == 3:
    steps = read_states_from_file(sys.argv[1])
    plot_to_file(steps, sys.argv[2])


if len(sys.argv) > 3:
    steps = read_states_from_file(sys.argv[1])
    plot_to_file(steps, sys.argv[2], sys.argv[3:])
