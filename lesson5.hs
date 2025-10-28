
-- genIfXEven f = (\x -> if even x
--                  then f x
--                  else x) 

ifEven myFunction x = 
  if even x
    then myFunction x
    else x


inc n = n + 1
double n = n * 2
square n = n^2

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

getRequestUrl host apikey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?apikey=" ++ apikey

-- genHostRequestBuilder host = (\apikey resource id ->
--   getRequestUrl host apikey resource id)

-- exampleUrlBuilder = genHostRequestBuilder "http://example.com"

-- genApiRequestBuilder hostBuilder resource apikey = (\id ->
--   hostBuilder apikey resource id)

-- myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "MY_RESOURCE" "MY_API_KEY"

url id = getRequestUrl "http://example.com" "MY_API_KEY" "MY_RESOURCE" id

subtract2 = flip (-) 2


binaryPartialApplication f x = (\y -> f x y)