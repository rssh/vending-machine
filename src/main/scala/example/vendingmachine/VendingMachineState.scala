package example.vendingmachine


trait VendingMachineState
{

  this: VendingMachine =>

  case class ItemContainer(nItems:Int, description:String, price:Int)

  type CoinsContainers = Map[Int,Int]
  type ItemsContainers = Map[Int,ItemContainer]

  case class State(
     currentSum: Int = 0,
     //currentCard: Option[Int] = None,
     currentItem: Option[Int] = None,
     coinsContainers: CoinsContainers = Map(),
     itemsContainers: ItemsContainers = Map()
  ) {
  }

  def putCoin(coinValue:Int)(state:State):(State,Seq[OutputAction])=
    state.coinsContainers.get(coinValue) match {
       case None => discard(state,s"Bad coin value $coinValue, coinsContainers=${state.coinsContainers}")
       case Some(nCoins) =>
              // TODO: this must be function over state
              val nextState = state.copy(
                 currentSum = state.currentSum + coinValue,
                 coinsContainers = state.coinsContainers.updated(coinValue,nCoins+1)
              )
              checkEndOfPurchase(nextState)
    }
  
  def chooseItem(id:Int)(state:State):(State,Seq[OutputAction])=
    state.itemsContainers.get(id) match {
       case None => (state,Seq(DisplayReject(s"Invalid item ${id}")))
       case Some(nItems) =>
                    checkEndOfPurchase(state.copy(currentItem = Some(id)))
    }

  def chooseCancel(state:State):(State,Seq[OutputAction])=
    outCoins(state.currentSum)(state.copy(currentItem = None))
 
  def checkEndOfPurchase(state:State):(State,Seq[OutputAction])=
   state.currentItem match {
       case None => (state,Seq())
       case Some(id) =>
                    state.itemsContainers.get(id) match {
                      case None => discard(state,s"item $id is not found")
                      case Some(ItemContainer(nItems,description,price)) =>
                            if (nItems == 0) discard(state,s"item $description is ended")
                            else if (state.currentSum >= price) {
                               val nextSum = state.currentSum - price
                               val signalEvent = if (nItems == 1)
                                                   Some(ItemContainerIsEmpty(id))
                                                 else None
                               val statePurchase = state.copy(currentSum = nextSum,
                                        currentItem = None,
                                        itemsContainers = state.itemsContainers.updated(id,          
                                                        ItemContainer(nItems-1,description, price))
                               )
                               compose(statePurchase,OutItem(id),outCoins(nextSum) _ )
                            } else 
                               (state,Seq())
                    }
   }

   def outCoins(sum:Int)(state:State):(State,Seq[OutputAction]) =
   {
      val s0 = (state,Seq[OutputAction]())
      val orderedCoins = state.coinsContainers.toSeq.sortBy(- _._1)
      val (rest,coins,nextCoinsContainer) = findExchangeSeq(sum,orderedCoins,Seq())
      val nextState = state.copy(currentSum = rest,
                                 coinsContainers = nextCoinsContainer)
      val nextOut = coins.map(OutCoin(_))
      (nextState,nextOut)
   }

   def findExchangeSeq(rest:Int, allCoinsOrdered:Seq[(Int,Int)],coins:Seq[Int]):(Int,Seq[Int],Map[Int,Int]) =
      if (rest == 0) 
        (0,coins,allCoinsOrdered.toMap)
      else {
        allCoinsOrdered match {
          case Seq((x,n), tail @ _ *) if (n==0 || x > rest) => 
                           val (nrest,ncoins,container) = findExchangeSeq(rest,tail,coins)
                           (nrest,ncoins,container.updated(x,n)) 
          case Seq((x,n), tail @ _ *)=> 
                  val (nrest, ncoins, container) = findExchangeSeq(rest-x,
                                                      (x,n-1) +: tail,
                                                       x +: coins)
                  if (nrest == 0) {
                    (nrest,ncoins,container) 
                  } else {
                    val (nrest1, ncoins1,container1) = findExchangeSeq(rest,tail,coins)
                    if (nrest1 < nrest) 
                       (nrest1, ncoins1, container1.updated(x,n))
                    else
                       (nrest, ncoins,container)
                  }
           case Seq() => (rest,coins,allCoinsOrdered.toMap)
        }
        
      }
        
         
      def discard(state:State,message:String) = (state,Seq(DisplayReject(message)))
  
      def compose(state:State,actions:Seq[OutputAction],f:(State)=>(State,Seq[OutputAction])):(State,Seq[OutputAction]) =
      {
         val (nstate,nactions) = f(state)
         (nstate, actions ++ nactions)
      }

      def compose(state:State,action:OutputAction,f:(State)=>(State,Seq[OutputAction])):(State,Seq[OutputAction]) =
            compose(state,Seq(action),f)


}

