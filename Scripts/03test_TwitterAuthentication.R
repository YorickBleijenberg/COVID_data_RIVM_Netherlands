#Twitter Authentication
library(rtweet)

#Save Twitter account Keys and Tokens.
api_key <- "estGftXZ9ma5EoqncaXCiT97K"
api_secret_key <- "SdATIcfTKgJOPjhh6LpzH2WqTHxCDlajPqZNLIBGJ2OFIWiZCC"
access_token <- "1305140449762594817-JTzMlNQwljj4Y3xqpet832Mh8nHDmz"
access_token_secret <- "VAFeaxNE6iDW6SxNNLxFZlXgW1VGf86Lu7SUGMOEoBxxy"


## authenticate via web browser
token <- create_token(
  app = "YoricksCoronaApp",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)