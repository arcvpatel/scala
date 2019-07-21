import org.scalatest.FlatSpec


class Test extends FlatSpec {
  "GetBestGroupPrice function" should "return the best prices based on the given input" in {
    val result = Main.getBestGroupPrices(Seq(Rate("M1", "Military"),
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
        CabinPrice("CB", "S2", 270.00)))

    assert(result === Seq(BestGroupPrice("CB", "M1", BigDecimal(230.0), "Military"),
      BestGroupPrice("CB", "S1", BigDecimal(245.0), "Senior"),
      BestGroupPrice("CA", "M1", BigDecimal(200.0), "Military"),
      BestGroupPrice("CA", "S1", BigDecimal(225.0), "Senior")
    ))
  }

  it should "handle different types of rate codes" in {
    val result = Main.getBestGroupPrices(Seq(Rate("ST1", "Standard"),
      Rate("ST2", "Standard"),
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")),
      Seq(CabinPrice("CA", "ST1", 550.00),
        CabinPrice("CA", "M1", 450.00),
        CabinPrice("CA", "ST2", 225.00),
        CabinPrice("CA", "S1", 360.00),
        CabinPrice("CA", "S2", 160.00),
        CabinPrice("CB", "M1", 230.00),
        CabinPrice("CB", "M2", 260.00),
        CabinPrice("CB", "S1", 245.00),
        CabinPrice("CB", "S2", 270.00)))
    assert (result === Seq(BestGroupPrice("CB", "M1", BigDecimal(230.0), "Military"),
      BestGroupPrice("CB", "S1", BigDecimal(245.0), "Senior"),
      BestGroupPrice("CA", "ST2", BigDecimal(225.0), "Standard"),
      BestGroupPrice("CA", "M1", BigDecimal(450.0), "Military"),
      BestGroupPrice("CA", "S2", BigDecimal(160.0), "Senior")
    ))
  }

  it should "take empty input arguments in to consideration" in {
    val result = Main.getBestGroupPrices(Seq.empty, Seq.empty)
    assert (result === Seq.empty)
  }

  it should "take empty input rate codes in to consideration" in {
    val result = Main.getBestGroupPrices(Seq(Rate("ST1", "Standard"),
      Rate("ST2", "Standard"),
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")), Seq.empty)
    assert (result === Seq.empty)
  }
}
