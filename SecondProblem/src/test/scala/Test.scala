import Main._
import org.scalatest.FlatSpec

class Test extends FlatSpec{

  "allCombinablePromotions" should "successfully return all possible combinations of promotions" in {
    val result = allCombinablePromotions(Seq(Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2")))
    )

    assert(result === Seq(PromotionCombo(Seq("P1", "P2")),
                          PromotionCombo(Seq("P1", "P4", "P5")),
                          PromotionCombo(Seq("P2", "P3")),
                          PromotionCombo(Seq("P3", "P4", "P5"))
                          ))
  }

  it should "return correct results with different dataset" in {
    val result = allCombinablePromotions(Seq(Promotion("P1", Seq("P4", "P5")),
      Promotion("P2", Seq("P3")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2")))
    )

    assert(result === Seq(PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    ))
  }

  it should "again return correct results with one more different dataset" in {
    val result = allCombinablePromotions(Seq(Promotion("P1", Seq("P2", "P3")),
      Promotion("P2", Seq("P1")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P3")),
      Promotion("P5", Seq("P2")))
    )

    assert(result === Seq(PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P2", "P4")),
      PromotionCombo(Seq("P3", "P5"))
    ))
  }

  "combinablePromotions " should " return all promotion combinations associated with a given promo" in {
      val result = combinablePromotions("P1", Seq(Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4", "P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2")))
      )

      assert(result === Seq(PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5"))
      ))
  }

 it should " return all promotion combinations associated when give a different promo code" in {
    val result = combinablePromotions("P4", Seq(Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2")))
    )

   assert(result === Seq(PromotionCombo(Seq("P1", "P4", "P5")),
     PromotionCombo(Seq("P3", "P4", "P5"))
   ))
  }

  it should " work with different dataset as well" in {
    val result = combinablePromotions("P2", Seq(Promotion("P1", Seq("P4", "P5")),
      Promotion("P2", Seq("P3")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2")))
    )

    assert(result === Seq(PromotionCombo(Seq("P1", "P2"))))
  }

}
