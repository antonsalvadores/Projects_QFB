from typing import List, Dict
from products.interest_rate.float_interest_rate_coupon import FloatInterestRateCoupon
from underlyings.interest_rate_curve import InterestRateCurve

# Cálculo NPV bono cupón a tipo variable

class FloatCouponBond:
    def __init__(self,
                 notional: float,
                 day_count: str,
                 schedule: List[Dict]):
        self.notional = notional
        self.day_count = day_count
        self.schedule = schedule
        self.coupons = []

        for period in self.schedule:
            float_coupon = FloatInterestRateCoupon(
                notional=self.notional,
                start_date=period['start_date'],
                end_date=period['end_date'],
                fixing_date=period['fixing_date'],
                day_count=self.day_count
            )
            self.coupons.append(float_coupon)

    # Suma de NPV de cupones variables + NPV nominal

    def npv(self, discount_curve: InterestRateCurve, forward_curve: InterestRateCurve) -> float:
        npv = 0.0
        for coupon in self.coupons:
            npv += coupon.npv(discount_curve, forward_curve)

        last_payment_date = self.schedule[-1]['end_date']
        if last_payment_date >= discount_curve.start_date:
            npv += self.notional * discount_curve.interpolate(last_payment_date)

        return npv