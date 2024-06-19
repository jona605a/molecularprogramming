file ?= out

fsout:
	dotnet run > drawing/fsout/$(file).out

draw: fsout
	python drawing/python_tree_render.py drawing/fsout/$(file).out drawing/graphs/$(file)

