from datetime import datetime
from underlyings.interest_rate_curve import InterestRateCurve
from basics.day_counter import DayCounter

# Cálculo de NPV cupón a tipo variable

class FloatInterestRateCoupon:
    def __init__(self,
                 notional: float,
                 start_date: datetime,
                 end_date: datetime,
                 fixing_date: datetime,
                 day_count: str):
        self.notional = notional
        self.start_date = start_date
        self.end_date = end_date
        self.fixing_date = fixing_date
        self.day_count = day_count
        self.delta_time = DayCounter.year_fraction(self.day_count,
                                                   self.start_date,
                                                   self.end_date)

    def npv(self, discount_curve: InterestRateCurve, forward_curve: InterestRateCurve) -> float:
        if self.end_date < discount_curve.start_date:
            return 0.0

        forward_rate = forward_curve.forward_rate(self.start_date, self.end_date)
        discount_factor = discount_curve.interpolate(self.end_date)

        return self.notional * discount_factor * forward_rate * self.delta_time