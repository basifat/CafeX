import billing.MenuBilling._

import org.scalatest._

class MenuTests extends FlatSpec with Matchers {

    "A Menu" should "have a database of food items" in {

        val expected = List(
            FoodItemInfo(getDrink(Coffee), getTemperature(Hot), BigDecimal(0.50)),
            FoodItemInfo(getDrink(Tea), getTemperature(Hot), BigDecimal(1.0)),
            FoodItemInfo(getFood(Bread), getTemperature(Hot), BigDecimal(1.5)),
            FoodItemInfo(getFood(Burger), getTemperature(Cold), BigDecimal(2.0)),
            FoodItemInfo(getFood(Sausage), getTemperature(Cold), BigDecimal(100))
        )

        getDatabase() should equal (expected)
    }

    it should "get a list of purchased items information from the database" in {

        val mockPurchases = List(getDrink(Coffee), getDrink(Tea))
        val expected = List(
                FoodItemInfo(getDrink(Coffee), getTemperature(Hot), 0.5),
                FoodItemInfo(getDrink(Tea), getTemperature(Hot), 1.0)
        )

        getPurchased(mockPurchases) should equal (expected)
    }

    it should "return the total for all purchased items with no service charge included" in {

        val mockPurchases = List(
                FoodItemInfo(getDrink(Coffee), getTemperature(Cold), 0.5),
                FoodItemInfo(getDrink(Tea), getTemperature(Hot), 0.5)
        )

        assert(getTotal(mockPurchases) == 1.0)
    }

    it should "return true if customer purchases any hot food" in {
        val mockPurchases = List(
                FoodItemInfo(getDrink(Coffee), getTemperature(Cold), 0.5),
                FoodItemInfo(getDrink(Tea), getTemperature(Hot), 0.5),
                FoodItemInfo(getFood(Bread), getTemperature(Hot), 1.5)
        )

        assert(isHotService(mockPurchases) == true)
    }


    it should "return false if customer purchases no hot food" in {
        
        val mockPurchases = List(
                FoodItemInfo(getDrink(Coffee), getTemperature(Cold), 0.5),
                FoodItemInfo(getDrink(Tea), getTemperature(Hot), 0.5),
                FoodItemInfo(getFood(Burger), getTemperature(Cold), 1.5)
        )

        assert(isHotService(mockPurchases) == false)
    }

    it should "should decide correctly the service charge to apply if purchases include any hot food" in {
        
        assert(getFoodServiceCharge(true) == 0.20)
    }

    it should "should decide correctly the service charge to apply if purchases include no hot food" in {

        assert(getFoodServiceCharge(false) == 0.10)
    }

    it should "should return true if purchases include only drinks" in {

        val mockPurchases = List(getDrink(Coffee), getDrink(Tea))
        assert(isDrinkOnly(mockPurchases) == true)
    }

    it should "get the correct service charge (0%) if customer purchases only drinks" in {
        
        val mockPurchases = List(getDrink(Coffee), getDrink(Tea))
        assert(getServiceCharge(mockPurchases, false) == 0.00)
    }

    it should "get the correct service charge (20%) if customer purchases any hot food" in {
        
        val mockPurchases = List(getDrink(Coffee), getDrink(Tea), getFood(Bread))
        assert(getServiceCharge(mockPurchases, true) == 0.20)
    }

    it should "get the correct service charge (10%) if customer purchases NO hot food" in {

        val mockPurchases = List(getDrink(Coffee), getDrink(Tea), getFood(Burger))
        assert(getServiceCharge(mockPurchases, false) == 0.10)
    }

    it should "get the correct total bill for purchases, including the the service charge" in {
        
        val mockTotal = 10.00
        val mockServiceChargePercent = 0.20
        assert(getTotalBill(mockTotal, mockServiceChargePercent) == 12.00)
    }

    it should "accept a list of purchases (drinks only) and return the total that excludes a service charge" in {
        
        val mockPurchases = List(getDrink(Coffee), getDrink(Tea))
        assert(getPurchaseTotal(mockPurchases) == 1.50)
    }

    it should "accept a list of purchases (cold foods and drinks) and return the total that includes a cold food service charge" in {
        
        val mockPurchases = List(getDrink(Coffee), getDrink(Tea), getFood(Burger))
        assert(getPurchaseTotal(mockPurchases) == 3.85)
    }

    it should "accept a list of purchases (cold and hot foods) and return the total that includes a hot food service charge" in {
        
        val mockPurchases = List(getFood(Bread), getFood(Burger))
        assert(getPurchaseTotal(mockPurchases) == 4.20)
    }

    it should "accept a list of purchases (cold foods, hot foods and drinks) and return the total that includes a hot food service charge" in {

        val mockPurchases = List(getDrink(Coffee), getDrink(Tea), getFood(Bread), getFood(Burger))
        assert(getPurchaseTotal(mockPurchases) == 6.00)
    }

    it should "accept a list of purchases (cold foods, hot foods and drinks) and return the total, that includes a maximum service charge of £20.00" in {

        val mockPurchases = List(getDrink(Coffee), getDrink(Tea), getFood(Bread), getFood(Burger), getFood(Sausage))
        assert(getPurchaseTotal(mockPurchases) ==  125.00)
    }

    }
