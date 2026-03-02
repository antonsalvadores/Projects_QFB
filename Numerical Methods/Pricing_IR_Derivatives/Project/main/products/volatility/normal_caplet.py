from datetime import datetime
from scipy.stats import norm
from underlyings.interest_rate_curve import InterestRateCurve
from underlyings.normal_volatility_surface import NormalVolatilitySurface
from basics.day_counter import DayCounter


class NormalCaplet:
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
            vol_surface: NormalVolatilitySurface) -> float:

        if self.end_date < discount_curve.start_date:
            return 0.0

        # Tiempo hasta comienzo del caplet (posible movimiento volatilidad)
        time_to_expiry = DayCounter.year_fraction('act365', discount_curve.start_date, self.fixing_date)

        # Descuento del NPV del caplet
        df = discount_curve.interpolate(self.end_date)

        # Tipo forward desde el inicio del caplet hasta el vencimiento de este
        forward_rate = forward_curve.forward_rate(self.start_date, self.end_date)

        # Cálculo NPV fórmula Bachelier

        # Si la opción ha expirado
        if time_to_expiry <= 0.0:
            intrinsic_value = max(forward_rate - self.strike, 0.0)
            return self.notional * self.delta_time * df * intrinsic_value

        sigma_n = vol_surface.get_volatility(time_to_expiry, self.strike)
        vol_sqrt_t = sigma_n * (time_to_expiry ** 0.5)

        # Prevención matemática de división por cero si la volatilidad es exactamente 0
        # (si el forward fuese determinista)
        if vol_sqrt_t == 0.0:
            intrinsic_value = max(forward_rate - self.strike, 0.0)
            return self.notional * self.delta_time * df * intrinsic_value

        d = (forward_rate - self.strike) / vol_sqrt_t

        option_value = (forward_rate - self.strike) * norm.cdf(d) + vol_sqrt_t * norm.pdf(d)


        return self.notional * self.delta_time * df * option_value