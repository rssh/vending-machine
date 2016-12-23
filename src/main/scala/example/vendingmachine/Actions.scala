package example.vendingmachine



sealed trait InputAction
sealed trait OutputAction

// UserInteractions
sealed trait UserInputAction extends InputAction

case class PutCoin(value:Int) extends UserInputAction
//TODO: implement
//case class InsertCard(cn:String) extends UserInputAction
case class ChooseItem(itemId:Int) extends UserInputAction
case object ChooseCancel extends UserInputAction

sealed trait UserOutputAction extends OutputAction
case class ListItems(itemId:Int, description:String, price:String) extends UserOutputAction
case class OutItem(itemId:Int) extends UserOutputAction
case class OutCoin(value:Int) extends UserOutputAction
case class DisplayReject(message:String) extends UserOutputAction


// Administrative Interactions

sealed trait AdministrativeInputAction extends InputAction

case class CleanCoinContainer(value:Int) extends AdministrativeInputAction
case class UpdateCoinContainer(value:Int, nCoins:Int) extends AdministrativeInputAction
case class UpdateItemContainer(itemId: String, nCoins:Int) extends AdministrativeInputAction
case class UpdateItemDescription(itemId: Int, description:String, price:Int) extends AdministrativeInputAction

sealed trait AdministrativeOutputAction extends OutputAction
case class CoinsContainerIsEmpty(value:Int)
case class ItemContainerIsEmpty(itemId:Int)

