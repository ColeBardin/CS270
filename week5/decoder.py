def main():
	with open("decodeIn", "r") as fp:
		line = fp.readline()
		N = int(line)
		lines = fp.readline()
		numbers = lines.split(" ")
	
		
		for term in numbers:
			lap = 1
			pos = 0
			car = 0
			num = int(term)
			if(num > 0):
				if( (num - N*N) > 0 ):
					lap = 2
					num -= N*N
				for p in range(N-1, 0, -1):
					if( (num - p*N) > 0):
						pos = p
						num -= p*N
						break
				car = num - 1
				print(f"lap:{lap}, pos:{pos}, car:{car}")
					

if  __name__ == "__main__":
	main()
