# Amazing Race DIMACS generator for N cars
# Written by Cole Bardin

# Turns lap, place, car, and N into the numeric variable 
def getVar(lap, place, car, N):
	return N*N*(lap-1) + N*(place - 1) + car + 1

def main():
	N = int(input("Enter number of cars:\n"))
	lines = 0

	with open(f"{N}carDIMACS", "w") as fp:
		# Question D: Boiler Plate
		fp.write(f"p cnf {N*N*2}\n")
		# Cars don't disappear
		# For both laps, for each car, write a clasue with all the positions
		for lap in range(1,3):
			for car in range(N):
				for pos in range(1,N+1):
					fp.write(f"{getVar(lap, pos, car, N)} ")
				fp.write("0\n")
				lines += 1
		# Single car can't be in two places at once
		# For both laps, for each car, write a clause for each combination of 2 positions
		for lap in range(1,3):
			for car in range(N):
				for pos in range(1,N):
					for pos2 in range(pos+1, N+1):
						fp.write(f"-{getVar(lap, pos, car, N)} -{getVar(lap, pos2, car, N)} 0\n")
						lines += 1
		# Two cars can't be in the same position at the same time
		# For both laps, for each position, write a clause for each combination of 2 cars
		for lap in range(1,3):
			for pos in range(1,N+1):
				for car in range(N):
					for car2 in range(car+1, N):
						fp.write(f"-{getVar(lap, pos, car, N)} -{getVar(lap, pos, car2, N)} 0\n")
						lines += 1
		# End of boiler plate 	
		
		# Question E: 
		# Tricky part of the puzzle: Amazing Race checks
		# For each car, for each position, print out valid ending positions for an Amazing Race
		for car in range(N):
			for pos in range(1,N+1):
				# Negated lap 1 position
				fp.write(f"-{getVar(1, pos, car, N)} ")		
				# Position if moved towards 1st place, include if valid
				if((pos - car) > 0):
					fp.write(f"{getVar(2, pos - car, car, N)} ")
				# Position if moved towards last place, include if valid
				if((pos + car) <= N and car != 0):
					fp.write(f"{getVar(2, pos + car, car, N)} ")
				fp.write("0\n")
				lines += 1
		# Rewrite the header with number of clauses
		fp.seek(0)
		fp.write(f"p cnf {N*N*2} {lines}\n")
	print(f"Wrote to file named {N}carDIMACS")
	
if __name__ ==  "__main__":
	main()
