import csv

with open('p083_matrix.txt', newline='\n') as csvfile:
    data = list(csv.reader(csvfile))

vertices = [(i,j) for i in range(80) for j in range(80)]

values = {}
for i in range(80):
     for j in range(80):
        values[(i,j)] = int(data[i][j])

conjugates = {}

conjugates[(0,0)] = {(0,1), (1,0)}
conjugates[(79,0)] = {(78,0), (79,1)}
conjugates[(0,79)] = {(0,78), (1,79)}
conjugates[(79,79)] = {(79,78), (78,79)}

for i in range(1,79):
    conjugates[(0,i)] = {(1,i), (0, i - 1), (0, i + 1)}
    conjugates[(79,i)] = {(78,i), (79, i - 1), (79, i + 1)}
    conjugates[(i,0)] = {(i,1), (i - 1, 0), (i + 1, 0)}
    conjugates[(i,79)] = {(i,78), (i - 1, 79), (i + 1, 79)}

for i in range(1,79):
     for j in range(1,79):
        conjugates[(i,j)] = {(i + 1,j), (i, j + 1), (i - 1, j), (i, j - 1)}

sums = {}
for v in vertices:
    sums[v] = 10**10

sums[(0,0)] = int(data[0][0])

isUpdated = True
while isUpdated:
    isUpdated = False
    for v in vertices:
        newSum = values[v] + min(map(lambda x : sums[x], conjugates[v]))
        if newSum < sums[v]:
            isUpdated = True
            sums[v] = newSum
    
print(sums[(79,79)])

