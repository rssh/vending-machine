package example.vendingmachine

import org.scalatest._

class ExchangeSuite extends FunSuite
{

 test("exhange sequence for 12 - 1") {
   val vs = new VendingMachineImpl() {}
   val (rest,coins,coinsLeft) = vs.findExchangeSeq(12,Seq(10->2,5->2,2->2,1->1),Seq())
   System.err.println(s"rest=$rest, coins=$coins, coinsLeft=$coinsLeft")
   assert(rest===0)
   assert(coinsLeft(10)===1)
 }

 test("exhange sequence for 12 - 2") {
   val vs = new VendingMachineImpl() {}
   val (rest,coins,coinsLeft) = vs.findExchangeSeq(12,Seq(10->1,5->2,2->1,1->1),Seq())
   System.err.println(s"rest=$rest, coins=$coins, coinsLeft=$coinsLeft")
   assert(rest===0)
   assert(coinsLeft(10)===0)
 }

 test("exhange sequence for 12 - 3") {
   val vs = new VendingMachineImpl() {}
   val (rest,coins,coinsLeft) = vs.findExchangeSeq(12,Seq(10->0,5->2,2->0,1->1),Seq())
   System.err.println(s"rest=$rest, coins=$coins, coinsLeft=$coinsLeft")
   assert(rest===1)
   assert(coinsLeft(10)===0)
   assert(coinsLeft(5)===0)
 }

}
