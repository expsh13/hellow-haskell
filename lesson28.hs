import qualified Data.Map as Map
import Language.Haskell.TH (location)

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList
  [ ("Arkham Asylum", (40.7128, 74.0059))
  , ("Gotham City", (41.8781, 87.6298))
  , ("Metropolis", (34.0522, 118.2437))
  , ("New York", (40.7128, 74.0060))
  ]

-- ラジアン変換
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

-- 緯度と軽度をラジアンに
latLongToRadians :: LatLong -> (Double, Double)
latLongToRadians (lat, long) = (toRadians lat, toRadians long)

-- 2点間の距離を計算
haversine :: LatLong -> LatLong -> Double
haversine cords1 cords2 = earthRadius * c
  where (lat1, long1) = latLongToRadians cords1
        (lat2, long2) = latLongToRadians cords2
        dlat = lat2 - lat1
        dlong = long2 - long1
        a = sin(dlat / 2) ^ 2 + cos lat1 * cos lat2 * sin(dlong / 2) ^ 2
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
        earthRadius = 3956 -- 地球の半径（マイル単位）

-- 欠損可能性のある距離を出力
printDistance::Maybe Double->IO ()
printDistance Nothing = putStrLn "Error, invalid location."
printDistance (Just distance) = putStrLn ("Distance: " ++ show distance ++ " miles.")

main::IO ()
main = do
  putStrLn "Enter first location:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter destination:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

-- addMaybe::Maybe Int->Maybe Int->Maybe Int
-- addMaybe Nothing _ = Nothing
-- addMaybe _ Nothing = Nothing
-- addMaybe (Just x) (Just y) = Just (x + y)

-- distanceFromNY::LatLong->Double
-- distanceFromNY cords = haversine cords nyCords
--   where nyCords = (40.7128, 74.0060)