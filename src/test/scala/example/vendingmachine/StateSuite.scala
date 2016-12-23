package example.vendingmachine

import org.scalatest._

class StateSuite extends FunSuite
{

 test("buy espresso") {
   val vs = prepareMachine()
   val (state1,out1) = vs.putCoin(25)(vs.state)
   assert(out1.isEmpty)
   val (state2,out2) = vs.putCoin(25)(state1)
   val (state3,out3) = vs.chooseItem(ESPRESSO)(state2)
   assert(state3.itemsContainers(ESPRESSO).nItems===199)
   assert(out3(0)===OutItem(1))
 }

 test("buy black tea") {
   val vs = prepareMachine()
   val (state1,out1) = vs.putCoin(25)(vs.state)
   assert(out1.isEmpty)
   val (state2,out2) = vs.chooseItem(BLACK_TEA)(state1)
   System.err.println(s"out2=${out2}")
   assert(out2(0).isInstanceOf[DisplayReject])
   val (state3,out3) = vs.chooseCancel(state2)
   System.err.println(s"out3=${out3}")
 }


 def prepareMachine():VendingMachineImpl =
 {
   val vs = new VendingMachineImpl()
   vs.state = new vs.State(
                    coinsContainers=Map(100->1,50->5,25->7,10->1,5->20,2->10,1->0),
                    itemsContainers=Map(
                     ESPRESSO -> vs.ItemContainer(200,"espresso",50),
                     DOPIO -> vs.ItemContainer(200,"dopio",75),
                     LATTE -> vs.ItemContainer(200,"latte",75),
                     CAPPUCCINO -> vs.ItemContainer(2,"cappuccino",100),
                     BLACK_TEA -> vs.ItemContainer(0,"black tea",25)
                    )
                  )
   vs
 }

 val ESPRESSO=1
 val DOPIO=2
 val LATTE=3
 val CAPPUCCINO=4
 val BLACK_TEA=5

}
