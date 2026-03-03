import numpy as np
from datetime import datetime
from scipy.stats import norm
from underlyings.interest_rate_curve import InterestRateCurve
from basics.day_counter import DayCounter


class ShiftedLognormalCaplet:
    def __init__(self,
                 notional: float,
                 strike: float,
                 start_date: datetime,
                 end_date: datetime,
                 fixing_date: datetime,
                 day_count: str):
        self.notional = notional
        self.strike = strike
        self.start_date = start_date
        self.end_date = end_date
        self.fixing_date = fixing_date
        self.day_count = day_count
        self.delta_time = DayCounter.year_fraction(self.day_count,
                                                   self.start_date,
                                                   self.end_date)

    def npv(self,
            discount_curve: InterestRateCurve,
            forward_curve: InterestRateCurve,
            implied_volatility: float,
            shift: float) -> float:

        if self.end_date < discount_curve.start_date:
            return 0.0

        # Tiempo hasta comienzo del caplet (posible movimiento volatilidad)
        time_to_expiry = DayCounter.year_fraction('act365', discount_curve.start_date, self.fixing_date)

        # Descuento del NPV del caplet
        df = discount_curve.interpolate(self.end_date)

        # Tipo forward desde el inicio del caplet hasta el vencimiento de este
        forward_rate = forward_curve.forward_rate(self.start_date, self.end_date)

        # Cálculo NPV fórmula shifted lognormal

        # Si la opción ha expirado
        if time_to_expiry <= 0.0:
            intrinsic_value = max(forward_rate - self.strike, 0.0)
            return self.notional * self.delta_time * df * intrinsic_value

        # Shift del Forward y del Strike
        f_shifted = forward_rate + shift
        k_shifted = self.strike + shift

        # Prevención matemática del modelo lognormal con variables estrictamente positivas
        if f_shifted <= 0.0 or k_shifted <= 0.0:
            intrinsic_value = max(forward_rate - self.strike, 0.0)
            return self.notional * self.delta_time * df * intrinsic_value


        vol_sqrt_t = implied_volatility * (time_to_expiry ** 0.5)

        if vol_sqrt_t == 0.0:
            intrinsic_value = max(forward_rate - self.strike, 0.0)
            return self.notional * self.delta_time * df * intrinsic_value

        d1 = (np.log(f_shifted / k_shifted) + 0.5 * (implied_volatility ** 2) * time_to_expiry) / vol_sqrt_t
        d2 = d1 - vol_sqrt_t

        option_value = f_shifted * norm.cdf(d1) - k_shifted * norm.cdf(d2)

        return self.notional * self.delta_time * df * option_value