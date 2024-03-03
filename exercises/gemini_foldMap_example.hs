--
-- NEED A LOT OF WORK HERE TO UNDERSTAND FOLDMAP
-- NEED TO UNDERSTAND MONOIDS FIRST!!!
--

-- Import foldMap
import Data.Foldable (foldMap)


-- Define a data type for a fruit with name and price
data Fruit = Fruit { name :: String, price :: Int } deriving Show

-- Sample fruit data
a=Fruit "Apple" 2
b=Fruit "Banana" 1
o=Fruit "Orange" 3
fruits :: [Fruit] = [a, b, o]

getPrice :: Fruit -> Int
getPrice (Fruit _ price) = price

-- map fruits to prices
mapFruitsToPrices :: [Fruit] -> [Int]
mapFruitsToPrices = map getPrice

-- -- ABOVE IS MY BIT --
-- -- CUT AND PASTE CODE FROM Gemini BELOW --


-- Calculate the total price of fruits using foldMap with a list
totalPriceList :: Int
totalPriceList = foldMap (\fruit -> fruit.price) fruits

-- -- Use the existing Foldable instance for the underlying list within Cart
-- data Cart = Cart [Fruit]

-- -- Calculate the total price of fruits in a Cart using foldMap on the list
-- totalPriceCart :: Cart -> Int
-- totalPriceCart (Cart xs) = foldMap (\fruit -> fruit.price) xs -- Directly fold over the list of fruits

-- -- Main function to execute and print results
-- main :: IO ()
-- main = do
--   putStrLn "Total price from list (should be 6):"
--   print totalPriceList
--   putStrLn "Total price from Cart (should be 6):"
--   let myCart = Cart fruits
--   print (totalPriceCart myCart)
