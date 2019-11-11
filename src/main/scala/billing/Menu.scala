package billing


object MenuBilling extends App {

case class FoodItemInfo(name: String, temperature: String, price: BigDecimal)

sealed trait DrinkName { val name: String}
    case object Coffee extends DrinkName { val name = "Coffee" }
    case object Tea extends DrinkName { val name = "Tea" }

sealed trait FoodName { val name: String}
    case object Bread extends FoodName { val name = "Bread" }
    case object Burger extends FoodName { val name = "Burger" }
    case object Sausage extends FoodName { val name = "Sausage" }

sealed trait FoodTemperature { val name : String }
    case object Cold extends FoodTemperature { val name = "Cold" }
    case object Hot extends FoodTemperature { val name = "Hot" }

    val purchases = List("Coffee", "Bread")
    println(main(purchases))

def main(purchases: List[String]) : BigDecimal = getPurchaseTotal(purchases)

def getFood(food: FoodName): String = 
    food match {
        case Bread => food.name
        case Burger => food.name
        case Sausage => food.name
    }

def getTemperature(temp: FoodTemperature): String =
    temp match {
        case Hot => temp.name
        case Cold => temp.name
    }

def getDrink(drink: DrinkName): String = 
    drink match {
        case Tea => drink.name
        case Coffee => drink.name
    }

def getDatabase(): List[FoodItemInfo]=
    List(
        FoodItemInfo(getDrink(Coffee), getTemperature(Hot), BigDecimal(0.50)),
        FoodItemInfo(getDrink(Tea), getTemperature(Hot), BigDecimal(1.0)),
        FoodItemInfo(getFood(Bread), getTemperature(Hot), BigDecimal(1.5)),
        FoodItemInfo(getFood(Burger), getTemperature(Cold), BigDecimal(2.0)),
        FoodItemInfo(getFood(Sausage), getTemperature(Cold), BigDecimal(100))
    )

def getDrinks(): List[String]= List(getDrink(Coffee), getDrink(Tea))

def getMaxCharge(): BigDecimal = 20.00

def getPurchased(items: List[String]): List[FoodItemInfo] = 
    getDatabase()
        .filter(item => items.contains(item.name))
        .map((item) => FoodItemInfo(item.name, item.temperature, item.price))

def getTotal(items: List[FoodItemInfo]) : BigDecimal = 
    items.map(item => item.price).reduce(_+_)

def isHotService(items: List[FoodItemInfo]) : Boolean =
    items
        .filter(item => !getDrinks().contains(item.name))
        .groupBy(item => item.temperature.toLowerCase)
        .contains(getTemperature(Hot).toLowerCase)

def isMax(serviceCharge: BigDecimal): Boolean = serviceCharge >= getMaxCharge()

def isDrinkOnly(purchases: List[String]) :Boolean = 
    (purchases.distinct diff getDrinks().distinct).size == 0

def getFoodServiceCharge(isHot: Boolean): BigDecimal = if (isHot) 0.2 else 0.1

def getServiceCharge(purchases: List[String], isHot: Boolean): BigDecimal = 
    if(isDrinkOnly(purchases)) 0.00 else getFoodServiceCharge(isHot)

def getTotalBill(subTotal: BigDecimal, serviceChargePercent: BigDecimal): BigDecimal ={
    val charges = subTotal * serviceChargePercent
    if (isMax(charges)) (getMaxCharge() + subTotal) else (charges + subTotal)
}

def getPurchaseTotal(items: List[String]): BigDecimal = {
    val foodItems = getPurchased(items)
    getTotalBill(getTotal(foodItems), getServiceCharge(items, isHotService(foodItems)))
                                        .setScale(2, BigDecimal.RoundingMode.HALF_UP)
}
}