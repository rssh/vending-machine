package example.vendingmachine


trait VendingMachine


trait VendingMachineApi
{

 def userInteraction(action:UserInputAction):Seq[UserOutputAction]

 def adminInteraction(key:String,action: AdministrativeInputAction):Seq[AdministrativeOutputAction]

}

