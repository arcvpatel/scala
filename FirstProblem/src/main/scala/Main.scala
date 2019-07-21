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

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    if(rates.length == 0 || prices.length == 0) return Seq.empty

    val groupedRates = rates.groupBy(r => (r.rateGroup))
    val groupedPrices = prices.groupBy(p => (p.cabinCode))
    val results = groupedPrices.map { case(k, v) =>
      //first group the rate

      val foundMin = groupedRates.map { case(rk, rv) =>
        // println(rk)
        var min = ("cabinCode", "rateCode", BigDecimal(99999.00), "rateGroup")
        rv.foreach {it =>
          //we can now find min
          val possibleMin = v.filter(_.rateCode == it.rateCode)
          min = if(possibleMin.length > 0 && min._3 > possibleMin(0).price) (possibleMin(0).cabinCode, it.rateCode, possibleMin(0).price, it.rateGroup ) else (min._1, min._2, min._3, min._4)
        }
        //if(min._1 != "cabinCode")
          (min)
      }
      foundMin
    }

    results.flatten.filter {x => x._1 != ("cabinCode")}.map{x => BestGroupPrice(x._1, x._2, x._3, x._4)}.toSeq
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