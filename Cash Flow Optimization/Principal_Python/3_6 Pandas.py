
import pandas as pd

from gurobipy import *


model = Model("Ejercicio_3_6")

x = [model.addVar(name=f"x{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(0, 2)] 

model.setObjective(4*x[0] + 3*x[1], GRB.MAXIMIZE)


#Restricciones

model.addConstr(x[0] + x[1] <= 100)
model.addConstr((2*x[0] + x[1])/100  <= 1.5)
model.addConstr((3*x[0] + 4*x[1])/100  <= 3.6)

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
with pd.ExcelWriter('ejercicio36.xlsx') as writer:
    df_vars.to_excel(writer, sheet_name='Sensibilidad_Variables', index=False)
    df_constrs.to_excel(writer, sheet_name='Sensibilidad_Restricciones', index=False)


