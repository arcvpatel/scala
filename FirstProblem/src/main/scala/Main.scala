object Main extends App  {
  println(getBestGroupPrices(Seq(Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")),
    Seq(CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00))))

  def min(c1: CabinPrice, c2: CabinPrice): CabinPrice = if (c1.price > c2.price) c2 else c1

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    if(rates.length == 0 || prices.length == 0) return Seq.empty

    val groupedRates = rates.groupBy(r => (r.rateGroup))
    val groupedPrices = prices.groupBy(p => (p.cabinCode))
    val results = groupedPrices.map { case(k, v) =>
      //first group the rate
      val foundMin = groupedRates.collect { case(rk, rv) =>
        val matchingCabins = v.filter{ x=> (rv.collect{case(a) => a.rateCode}).contains(x.rateCode)}
        if(!matchingCabins.isEmpty) {
          val thisMin = matchingCabins.reduceLeft(min)
          BestGroupPrice(thisMin.cabinCode, thisMin.rateCode, thisMin.price, rk)
        }else BestGroupPrice("junk", "junk", 0.0, "junk")
      }
      foundMin
    }

    results.flatten.filter{_.cabinCode != "junk"}.toSeq
  }
}

case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String,
                      rateCode: String,
                      price: BigDecimal)
case class BestGroupPrice(cabinCode: String,
                          rateCode: String,
                          price: BigDecimal,
                          rateGroup: String)