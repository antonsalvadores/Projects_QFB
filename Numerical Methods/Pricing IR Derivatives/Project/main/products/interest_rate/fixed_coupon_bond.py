from typing import List, Dict
from products.interest_rate.fixed_interest_rate_coupon import FixedInterestRateCoupon
from underlyings.interest_rate_curve import InterestRateCurve

# Cálculo de NPV bono cupón a tipo fijo

class FixedCouponBond:
    def __init__(self,
                 notional: float,
                 coupon_rate: float,
                 day_count: str,
                 schedule: List[Dict]):
        self.notional = notional
        self.coupon_rate = coupon_rate
        self.day_count = day_count
        self.schedule = schedule
        self.coupons = []

        for period in self.schedule:
            fixed_coupon = FixedInterestRateCoupon(
                notional=self.notional,
                coupon=self.coupon_rate,
                start_date=period['start_date'],
                end_date=period['end_date'],
                day_count=self.day_count
            )
            self.coupons.append(fixed_coupon)

    # Suma de NPV de cupones fijos + NPV nominal

    def npv(self, discount_curve: InterestRateCurve) -> float:
        npv = 0.0
        for coupon in self.coupons:
            npv += coupon.npv(discount_curve)

        last_payment_date = self.schedule[-1]['payment_date']
        if last_payment_date >= discount_curve.start_date:
            npv += self.notional * discount_curve.interpolate(last_payment_date)

        return npv