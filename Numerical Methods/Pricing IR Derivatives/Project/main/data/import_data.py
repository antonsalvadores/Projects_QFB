import pandas as pd
import datetime

# Seleccionamos la ruta donde tenemos los datos

route = r"C:\MÁSTER QFB\2º CURSO\project_antón_carlos\main\data\Datos_Ejercicio_1.xlsx"

# Seleccionamos las columnas (y nombres) de cada curva de tipos

curves = {
    "6M": ("B:H", [0]),
    "3M": ("J:P", [0]),
    "1M": ("R:X", [0]),
    "OIS": ("Z:AG", [0, 1])
}

# Cargamos los datos en un diccionario, donde cada clave contiene una curva específica

data = {}
for name, (cols, date_indices) in curves.items():
    df = pd.read_excel(route, sheet_name="Euribor Curves", header=2, usecols=cols, parse_dates=date_indices).dropna()
    df.columns = [c.split('.')[0] for c in df.columns]
    # Convertimos las fechas a objetos datetime
    cols_date = ['Payment Date', 'Maturity Date']
    for col in cols_date:
        if col in df.columns:
            df[col] = df[col].dt.to_pydatetime()
    data[name] = df

data['Volatility'] = pd.read_excel(route,
                       sheet_name="Normal Volatility",
                       header=3,
                       usecols="D:S",
                       index_col=0)
# Dividimos la volatilidad entre 10000 para convertirla en puntos
# básicos, pues en el excel viene escalada
data['Volatility'] = data['Volatility'] / 10000.0
