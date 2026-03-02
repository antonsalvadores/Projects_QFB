from datetime import datetime
from data.import_data import data
from basics.target_calendar import TargetCalendar
from basics.schedule_generator import ScheduleGenerator
from underlyings.interest_rate_curve import InterestRateCurve
from underlyings.normal_volatility_surface import NormalVolatilitySurface
from products.volatility.normal_cap import NormalCap
from products.volatility.shifted_lognormal_cap import ShiftedLognormalCap

# Datos del ejercicio b y c

valuation_date = datetime(year=2018, month=12, day=31)
notional: float = 10_000_000.0
spot_lag: int = 2
tenor_years: int = 20

# Parámetros del Interest Rate Cap
cap_strike: float = 0.015133
cap_frequency_months: int = 6
cap_day_count: str = 'act360'

# Construcción de curva descuento y forward
df_ois = data['OIS']
curve_ois = InterestRateCurve(
    start_date=valuation_date,
    end_dates=df_ois['Maturity Date'].tolist(),
    discount_factors=df_ois['Discount'].tolist(),
    interpolation='linear',
    day_count='act365'
)

df_6m = data['6M']
curve_euribor = InterestRateCurve(
    start_date=valuation_date,
    end_dates=df_6m['Maturity Date'].tolist(),
    discount_factors=df_6m['Discount'].tolist(),
    interpolation='linear',
    day_count='act365'
)

# Construcción superficie de volatilidad
df_vol = data['Volatility'].copy()

# Eliminamos la columna ATM del Excel, al ser una columna con
# strike dinámico, pues no nos sirve para interpolar
if 'ATM' in df_vol.columns:
    df_vol = df_vol.drop(columns=['ATM'])

# Extracción de ejes X (Strikes) e Y (Tenors)
strikes = [float(col) for col in df_vol.columns]
tenor_mapping = {
    '1Yr': 1.0, '18Mo': 1.5, '2Yr': 2.0, '3Yr': 3.0, '4Yr': 4.0,
    '5Yr': 5.0, '6Yr': 6.0, '7Yr': 7.0, '8Yr': 8.0, '9Yr': 9.0,
    '10Yr': 10.0, '12Yr': 12.0, '15Yr': 15.0, '20Yr': 20.0, '25Yr': 25.0, '30Yr': 30.0
}
tenors = [tenor_mapping[t] for t in df_vol.index]
volatilities_matrix = df_vol.values

# Instanciamos con nuestros datos
vol_surface = NormalVolatilitySurface(
    tenors=tenors,
    strikes=strikes,
    volatilities=volatilities_matrix,
    interpolation='linear'
)

# Generamos el calendario de flujos de caja de cada bono según el calendario
# TARGET, las convenciones Modified Following y Rolling Backward (EOM)
calendar = TargetCalendar()
schedule_gen = ScheduleGenerator(calendar)

cap_schedule, effective_date, maturity_date = schedule_gen.generate_schedule(
    valuation_date=valuation_date,
    spot_lag=spot_lag,
    tenor_years=tenor_years,
    frequency_months=cap_frequency_months
)

# Cálculo del NPV del cap
if __name__ == "__main__":
    # Construcción del cap con el modelo normal
    normal_cap = NormalCap(
        notional=notional,
        strike=cap_strike,
        day_count=cap_day_count,
        schedule=cap_schedule
    )

    # Cálculo del valor presente (NPV)
    npv_cap = normal_cap.npv(
        discount_curve=curve_ois,
        forward_curve=curve_euribor,
        vol_surface=vol_surface
    )

    # Visualización de resultados

    print("=" * 65)
    print("RESUMEN DE VALORACIÓN - CAP (MODELO NORMAL)")
    print("=" * 65)
    print(f"Valuation Date : {valuation_date.strftime('%Y-%m-%d')}")
    print(f"Effective Date : {effective_date.strftime('%Y-%m-%d')}")
    print(f"Maturity Date  : {maturity_date.strftime('%Y-%m-%d')}")
    print(f"Notional       : {notional:,.2f} EUR")
    print(f"Strike         : {cap_strike * 100:.4f} %")
    print("-" * 65)
    print(f"VALOR DEL CAP (Prima MtM)        : {npv_cap:,.2f} EUR")
    print("=" * 65)

    # Apartado c

    # Construcción del Cap bajo el modelo Shifted Lognormal
    shift_parameter: float = 0.03

    sln_cap = ShiftedLognormalCap(
        notional=notional,
        strike=cap_strike,
        day_count=cap_day_count,
        schedule=cap_schedule
    )

    # Llamamos al método que calcula la volatilidad implícita
    implied_vol = sln_cap.implied_volatility(
        target_npv=npv_cap,
        discount_curve=curve_ois,
        forward_curve=curve_euribor,
        shift=shift_parameter
    )

    # Comprobamos que el NPV al recalcularlo con esa volatilidad da el del apartado b
    check_npv = sln_cap.npv(
        discount_curve=curve_ois,
        forward_curve=curve_euribor,
        implied_volatility=implied_vol,
        shift=shift_parameter
    )

    print("\n" + "=" * 65)
    print("RESUMEN DE VALORACIÓN - CAP (SHIFTED LOGNORMAL)")
    print("=" * 65)
    print(f"Target NPV (Apartado B) : {npv_cap:,.2f} EUR")
    print(f"Shift Parameter         : {shift_parameter * 100:.2f} %")
    print("-" * 65)
    print(f"VOLATILIDAD IMPLÍCITA   : {implied_vol * 100:.4f} %")
    print(f"NPV de Comprobación     : {check_npv:,.2f} EUR")
    print("=" * 65)

