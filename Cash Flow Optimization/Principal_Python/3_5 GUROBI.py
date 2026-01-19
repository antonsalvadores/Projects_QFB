
from gurobipy import *


model = Model("Ejercicio_3_5")

# Flujo
flujo = [150, 100, -200, 200, -50, -300]
tiempo = len(flujo) - 1

#Variables
x = [model.addVar(lb=0.0, ub=100, vtype=GRB.CONTINUOUS, name=f"x{q+1}") for q in range(0, tiempo+1)]  
y = [model.addVar(lb=0, vtype=GRB.CONTINUOUS, name= f"y{q+1}") for q in range(0, tiempo+1)]  
z = [model.addVar(lb=0, vtype=GRB.CONTINUOUS, name= f"z{q+1}", ) for q in range(0, tiempo+1)]   

# Intereses
rx = 0.01  
ry = 0.02  
rz = 0.003   

#Funcion objetivo
model.setObjective(z[tiempo], GRB.MAXIMIZE)

# Restricciones
for q in range(0, tiempo+1): 
    if q == 0:
        model.addConstr(flujo[q] == x[q] + y[q] - z[q], f"Q{q+1}")
    elif q == 1:
        model.addConstr(flujo[q] == x[q] - 1.01*x[q-1] + y[q]
        - z[q] + (1+rz)*z[q-1], f"Q{q+1}")
    elif q == 3:
        model.addConstr(flujo[q] == x[q] - 1.01*x[q-1] + y[q] - (1+ry)*y[q-2]
        - z[q] + (1+rz)*z[q-1], f"Q{q+1}") 
    elif q == tiempo:
        model.addConstr(flujo[q] == - 1.01*x[q-1] - z[q] + (1+rz)*z[q-1], f"Q{q+1}")    
    else:
        model.addConstr(flujo[q] == x[q] - 1.01*x[q] - (1+ry)*y[q-2]
        - z[q] + (1+rz)*z[q-1], f"Q{q+1}") 
        
# Resolver el modelo        
model.optimize()

print(f"{model.status}")

