from datetime import datetime
from data.import_data import data
from basics.target_calendar import TargetCalendar
from basics.schedule_generator import ScheduleGenerator
from underlyings.interest_rate_curve import InterestRateCurve
from products.interest_rate.fixed_coupon_bond import FixedCouponBond
from products.interest_rate.float_coupon_bond import FloatCouponBond

# Datos del ejercicio a

valuation_date = datetime(year=2018, month=12, day=31)
notional: float = 10_000_000.0
spot_lag: int = 2
tenor_years: int = 20

# Parámetros Pata Fija
fixed_coupon_rate: float = 0.024215
fixed_frequency_months: int = 12
fixed_day_count: str = '30360'

# Parámetros Pata Flotante
float_frequency_months: int = 6
float_day_count: str = 'act360'

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

# Generamos el calendario de flujos de caja de cada bono según el calendario
# TARGET, las convenciones Modified Following y Rolling Backward (EOM)
calendar = TargetCalendar()
schedule_gen = ScheduleGenerator(calendar)

fixed_schedule, effective_date, maturity_date = schedule_gen.generate_schedule(
    valuation_date=valuation_date,
    spot_lag=spot_lag,
    tenor_years=tenor_years,
    frequency_months=fixed_frequency_months
)

float_schedule, _, _ = schedule_gen.generate_schedule(
    valuation_date=valuation_date,
    spot_lag=spot_lag,
    tenor_years=tenor_years,
    frequency_months=float_frequency_months
)

# Calculamos el valor del bono fijo y flotante, que redundan en el valor del swap

if __name__ == "__main__":
    # Bono fijo
    fixed_bond = FixedCouponBond(
        notional=notional,
        coupon_rate=fixed_coupon_rate,
        day_count=fixed_day_count,
        schedule=fixed_schedule
    )

    # Bono Flotante
    float_bond = FloatCouponBond(
        notional=notional,
        day_count=float_day_count,
        schedule=float_schedule
    )

    # NPV de los bonos
    npv_fixed = fixed_bond.npv(discount_curve=curve_ois)
    npv_float = float_bond.npv(discount_curve=curve_ois, forward_curve=curve_euribor)

    # NPV swap (pagador fijo, recibe flotante)
    npv_swap = npv_float - npv_fixed

    # Visualización de resultados

    print("=" * 60)
    print("RESUMEN DE VALORACIÓN - INTEREST RATE SWAP")
    print("=" * 60)
    print(f"Valuation Date : {valuation_date.strftime('%Y-%m-%d')}")
    print(f"Effective Date : {effective_date.strftime('%Y-%m-%d')}")
    print(f"Maturity Date  : {maturity_date.strftime('%Y-%m-%d')}")
    print(f"Notional       : {notional:,.2f} EUR")
    print("-" * 60)
    print(f"NPV Bono Fijo (Pata Pagadora)    : {npv_fixed:,.2f} EUR")
    print(f"NPV Bono Variable (Pata Receptora): {npv_float:,.2f} EUR")
    print("-" * 60)
    print(f"VALOR DEL SWAP (MtM)             : {npv_swap:,.2f} EUR")
    print("=" * 60)