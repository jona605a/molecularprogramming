import graphviz

import sys
import os
import shutil


def read_tree_from_file(file_path,unit_scale=1.0):
    tree = []
    with open(file_path,"r") as file:
        lines = file.readlines()
        for line in lines:
            if line.find(",") < 0:
                break
            line = line[1:]
            label = line[:line.find('"')]
            line = line[len(label) + 4:]
            x = float(line[:line.find(",")]) * unit_scale
            y = float(line[line.find(",") + 1:line.find(")")])
            line = line[line.find(")") + 4:]
            self_id = int(line[:line.find(",")])
            parent_id = int(line[line.find(",") + 1:line.find(")")])
            tree.append((label,(x,y),self_id,parent_id))

    return tree
            


def create_tree_from_string(tree,name):
    scale = 1
    for vertex in tree:
        scale = max(scale,len(vertex[0]))

    scale = 1 + (scale - 1) * 0.2

    G = graphviz.Graph("u",format="png",node_attr={'shape': 'plaintext'})
    G.graph_attr["layout"] = "neato" 
    x,y = tree[0][1]
    G.node(str(tree[0][2]),label=tree[0][0],pos=str(x * scale) + "," + str(y) + "!")

    for i,vertex in enumerate(tree[1:]):
        x,y = vertex[1]
        G.node(str(vertex[2]),label=vertex[0],pos=str(x * scale) + "," + str(-y)+ "!")
        G.edge(str(vertex[3]),str(vertex[2]))
    G.render(name)



try:
    shutil.rmtree("output")
    os.mkdir("output")
except:
    os.mkdir("output")
    pass


if len(sys.argv) == 3:
    create_tree_from_string(read_tree_from_file(sys.argv[1]),sys.argv[2])

if len(sys.argv) == 4:
    create_tree_from_string(read_tree_from_file(sys.argv[1],float(sys.argv[3])),sys.argv[2])