#Twitter Authentication
library(rtweet)

#Save Twitter account Keys and Tokens.
api_key <- "nBAYmSBzMw7yirC7hNCAqFI2Y"
api_secret_key <- "WsOxPYoIkoRen1SxYymtuHvZvPUxZjs8TyTeLM6fw18LjavIcJ"
access_token <- "25058013-RBtLeZE3hJWuGlvd45tabBfbbIApPgaA2sGjn7gkl"
access_token_secret <- "4Ighs4eyHaKSPzEPcIWxvV1vJm4ost5ILBj4eWuAkkfw6"

## authenticate via web browser
token <- create_token(
  app = "YoricksCoronaApp",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)