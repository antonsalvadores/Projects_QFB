from datetime import datetime
from underlyings.interest_rate_curve import InterestRateCurve
from basics.day_counter import DayCounter

# Cálculo de NPV cupón a tipo fijo

class FixedInterestRateCoupon:
    def __init__(self,
                 notional: float,
                 coupon: float,
                 start_date: datetime,
                 end_date: datetime,
                 day_count: str):
        self.notional = notional
        self.coupon = coupon
        self.start_date = start_date
        self.end_date = end_date
        self.day_count = day_count
        self.delta_time = DayCounter.year_fraction(self.day_count,
                                                   self.start_date,
                                                   self.end_date)

    def npv(self, discount_curve: InterestRateCurve) -> float:
        if self.end_date < discount_curve.start_date:
            return 0.0

        discount_factor = discount_curve.interpolate(self.end_date)
        return self.notional * discount_factor * self.coupon * self.delta_time