package example.vendingmachine


class VendingMachineImpl extends VendingMachine with VendingMachineApi with VendingMachineState
{

  var state = new State


  def userInteraction(action:UserInputAction):Seq[UserOutputAction] =
   action match {
     case PutCoin(value) => userAction(putCoin(value)_)
     case ChooseItem(id) => userAction(chooseItem(id)_)
     case ChooseCancel   => userAction(chooseCancel _)
   }

  def userAction(f:State => (State,Seq[OutputAction])):Seq[UserOutputAction] =
  {
    val (nextState, outAction) = f(state)
    state = nextState
    outAction.flatMap{ 
      case ua: UserOutputAction => Some(ua)
      case otherAction => administrative(otherAction)
                          None
    }
  }

  def administrative(x:OutputAction):Unit =
   Console.println("administrative: $x")

  def adminInteraction(key:String, action: AdministrativeInputAction):Seq[AdministrativeOutputAction] = ???

}

