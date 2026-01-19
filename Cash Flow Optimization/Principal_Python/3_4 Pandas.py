
import pandas as pd

from gurobipy import *

# Crear el modelo
model = Model("Ejercicio_3_4")

# Flujo
flujo = [100, 500, 100, -600, -500, 200, 600, -900]
tiempo = len(flujo) - 1

# Variables de decisión para préstamos e inversiones
x = model.addVar(name="x1", lb=0, vtype=GRB.CONTINUOUS)  
y = [model.addVar(name=f"y{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(0, tiempo)] 
z = [model.addVar(name=f"z{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(0, tiempo+1)]  
k = [model.addVar(name=f"k{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(0, tiempo+1)]  

# Intereses
rx = 0.01  
ry = 0.018  
rz = 0.025  
rk = 0.005  

# Función objetivo (maximizar) 
model.setObjective(k[tiempo], GRB.MAXIMIZE)

# Restricciones para cada periodo
for q in range(0, tiempo+1):
    if q == 0:
        model.addConstr(flujo[q] == x + y[q] + z[q] - k[q], f"Q{q+1}")
    elif q == 1:
        model.addConstr(flujo[q] == -rx*x + y[q] - ry*y[q-1] + z[q] - (1+rz)*z[q-1] - k[q] + (1+rk)*k[q-1], f"Q{q+1}")
    elif q == tiempo:
        model.addConstr(flujo[q] == -(1+rx)*x - (1+ry)*y[q-2] - (1+rz)*z[q-1] - k[q] + (1+rk)*k[q-1], f"Q{q+1}")
    elif q == tiempo - 1:
        model.addConstr(flujo[q] == -rx*x - (ry*y[q-1] + (1+ry)*y[q-2]) + z[q] - (1+rz)*z[q-1] - k[q] + (1+rk)*k[q-1], f"Q{q+1}")
    else:
        model.addConstr(flujo[q] == -rx*x + y[q] - (ry*y[q-1] + (1+ry)*y[q-2]) + z[q] - (1+rz)*z[q-1] - k[q] + (1+rk)*k[q-1], f"Q{q+1}")

# Resolver el modelo
model.optimize()

# Informe de sensibilidad

var_names = []
var_final = []
coste_reducido = []
coef_optimo = []
obj_low = []
obj_up = []
constr_names = []
precio_sombra = []
rhs_low = []
rhs_up = []

for v in model.getVars():
    var_names.append(v.VarName)
    var_final.append(v.x)
    coste_reducido.append(v.RC)
    coef_optimo.append(v.Obj)
    obj_low.append(v.SAObjLow)
    obj_up.append(v.SAObjUp)

for c in model.getConstrs():
    constr_names.append(c.ConstrName)
    precio_sombra.append(c.Pi)
    rhs_low.append(c.SARHSLow)
    rhs_up.append(c.SARHSUp)

df_vars = pd.DataFrame({
    'Variable': var_names,
    'Valor final': var_final,
    'Coste reducido': coste_reducido,
    'Coeficiente óptimo': coef_optimo,
    'Disminución permitido': obj_low,
    'Crecimiento permitida': obj_up
})

df_constrs = pd.DataFrame({
    'Restricción': constr_names,
    'Precio Sombra': precio_sombra,
    'Valor mínimo permitido': rhs_low,
    'Valor máximo permitido': rhs_up
})

# Exportar a Excel
with pd.ExcelWriter('ejercicio34.xlsx') as writer:
    df_vars.to_excel(writer, sheet_name='Sensibilidad_Variables', index=False)
    df_constrs.to_excel(writer, sheet_name='Sensibilidad_Restricciones', index=False)


