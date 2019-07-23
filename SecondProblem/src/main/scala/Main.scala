object Main extends App {

  println(allCombinablePromotions(Seq(Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2")))
  ))

  println(combinablePromotions("P3", Seq(Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))))
  )

  def getPromotion(groupedPromotions: Map[String, Seq[Promotion]], n: String): Promotion = {
    groupedPromotions.get(n) match {
      case None => Promotion("", Seq.empty)
      case Some(s: Seq[Promotion]) => s(0)
    }
  }

  def getAllPossibleCombo(promotions: Seq[Promotion], thisPromotion:Promotion, groupedPromotions: Map[String, Seq[Promotion]], resultList: Seq[Seq[String]]): Seq[Seq[String]] = {
    if(promotions.isEmpty) {
      resultList
    }else {
      val partialCombo = promotions.foldLeft(List[String](thisPromotion.code)) {(acc, a) => {
        // println("Recursive promotion code: " + thisPromotion.code)
        // println("a code: " + a.code)
        if(a.code == thisPromotion.code) {
          acc
        }else {
          val intersection = getPromotion(groupedPromotions, a.code).notCombinableWith.intersect(thisPromotion.notCombinableWith)
          val firstCriteria = !intersection.contains(a.code) && !intersection.contains(thisPromotion.code)
          val secondCriteria = !thisPromotion.notCombinableWith.contains(a.code)
          val thirdCriteria = !getPromotion(groupedPromotions, a.code).notCombinableWith.contains(thisPromotion.code)
          val fourthCriteria = acc.intersect(a.notCombinableWith).isEmpty
          if(firstCriteria && thirdCriteria && secondCriteria && fourthCriteria) {
            val appended = acc :+ a.code
            // println("Append")
            // println(appended)
            appended
          }else {
            // println("acc only")
            //println(acc)
            acc
          }
        }
      }
      }
      val thisIsASubset = resultList.exists (item => {
        item.intersect(partialCombo).size == partialCombo.size
      })
      if(partialCombo.isEmpty || thisIsASubset)
        getAllPossibleCombo(promotions.drop(1), thisPromotion, groupedPromotions, resultList)
      else
        getAllPossibleCombo(promotions.drop(1), thisPromotion, groupedPromotions, resultList :+ partialCombo.sorted)
    }
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val groupedPromotions = allPromotions.groupBy(x => x.code)
    // println(groupedPromotions)
    val finalCombo = allPromotions.map {getAllPossibleCombo(allPromotions,_, groupedPromotions, List())}

    finalCombo.flatten.distinct.map{ PromotionCombo(_)}
  }

  def combinablePromotions( promotionCode: String,
                            allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val allValidPromotions = allCombinablePromotions(allPromotions)
    allValidPromotions.filter(_.promotionCodes.contains(promotionCode))
  }
}

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])
