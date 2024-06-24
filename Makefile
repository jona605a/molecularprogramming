file ?= counter

fsout:
	dotnet run examples/crn/$(file).crn > outfiles/$(file).out

draw: fsout
	python drawing/states_plotter.py outfiles/$(file).out outfiles/$(file)plot
