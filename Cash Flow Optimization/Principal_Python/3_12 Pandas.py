
import pandas as pd
from gurobipy import *
import matplotlib.pyplot as plt
import numpy as np


model = Model("Ejercicio_3_12")

#Datos

precio = [102.44, 99.95, 100.02, 102.66, 87.90, 85.43, 83.42, 103.82,
           110.29, 108.85, 109.95, 107.36, 104.62, 99.07, 103.78, 64.66]

cupon = [5.625, 4.75, 4.25, 5.25, 0, 0, 0, 5.75, 6.875, 6.5, 6.625, 
           6.125, 5.625, 4.75, 5.5, 0]

vencimiento = [1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9]

pasivos = [24* 10**6, 26* 10**6, 28 * 10**6, 28* 10**6, 
           26* 10**6, 29* 10**6, 32* 10**6, 33* 10**6, 34* 10**6] 

rk = 0.02

nbonos = len(precio)
tiempo = max(vencimiento)

# Variables 

x = [model.addVar(name=f"x{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(nbonos)]
k = [model.addVar(name=f"k{q+1}", lb=0, vtype=GRB.CONTINUOUS) for q in range(tiempo)]
s = model.addVar(name=f"s", lb=0, vtype=GRB.CONTINUOUS)


#Objetivo

model.setObjective(quicksum([x[q] * precio[q] + s/nbonos for q in range(nbonos)]), GRB.MINIMIZE)

#Restricciones

for t in range(tiempo):
   cupon_t = quicksum([cupon[q] * x[q] for q in range(nbonos) if vencimiento[q] >= t+1])

   vencimiento_t = quicksum([100 * x[q] for q in range(nbonos) if vencimiento[q] == t+1])
      
   if t == tiempo-1:
        reinversion_t = (1+rk) * k[t-1]
   elif t == 0:
        reinversion_t = -k[t] + (1+rk)*s
   else:
        reinversion_t = (1+rk) * k[t-1] - k[t]
    
   model.addConstr(cupon_t + vencimiento_t + reinversion_t == pasivos[t], f"Y{t+1}")

model.optimize()

# Informe de sensibilidad

var_name = []
var_final = []
coste_reducido = []
coef_optimo = []
obj_low = []
obj_up = []
constr_name = []
precio_sombra = []
rhs_low = []
rhs_up = []

for v in model.getVars():
    var_name.append(v.VarName)
    var_final.append(v.x)
    coste_reducido.append(v.RC)
    coef_optimo.append(v.Obj)
    obj_low.append(v.SAObjLow)
    obj_up.append(v.SAObjUp)

for c in model.getConstrs():
    constr_name.append(c.ConstrName)
    precio_sombra.append(c.Pi)
    rhs_low.append(c.SARHSLow)
    rhs_up.append(c.SARHSUp)

df_vars = pd.DataFrame({
    'Variable': var_name,
    'Valor final': var_final,
    'Coste reducido': coste_reducido,
    'Coeficiente óptimo': coef_optimo,
    'Disminución permitido': obj_low,
    'Crecimiento permitida': obj_up
})

df_constrs = pd.DataFrame({
    'Restricción': constr_name,
    'Precio Sombra': precio_sombra,
    'Valor mínimo permitido': rhs_low,
    'Valor máximo permitido': rhs_up
})

# Exportar a Excel
with pd.ExcelWriter('ejercicio3_12.xlsx') as writer:
    df_vars.to_excel(writer, sheet_name='Sensibilidad_Variables', index=False)
    df_constrs.to_excel(writer, sheet_name='Sensibilidad_Restricciones', index=False)

#Grafico

x =  np.linspace(1, tiempo, tiempo)
y = [(1/(precio_sombra[t])**(1/(t+1))-1)*100 for t in range(tiempo)]

plt.plot(x, y)

plt.title('Estructura temporal de la tasa de interés')
plt.xlabel('Vencimiento (años)')
plt.ylabel('Tasa de interés (%)')
plt.xlim(1, tiempo)     # Limitar el eje x de 0 a 10
plt.ylim(3, 5.25) # Limitar el eje y de -1.5 a 1.5

plt.grid()

plt.savefig('tasainteres.jpg', dpi=300, bbox_inches='tight')

plt.show()

