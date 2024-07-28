module Choreography.WorkerManager.Message where

import Foreign (Foreign)

-- |　Worker からのメッセージは一旦 Main にたまる
type MessageToMain =
  { messageType :: String -- "send" | "bcast" | "terminate"
  , messageData :: Foreign -- 受信データ, "terminate" の場合は空
  , targetLocation :: String -- 送信先、main 以外の場合は転送, "bcast", "terminate" の場合は空
  }

type MessageToWorker =
  { messageData :: Foreign -- 送信データ
  , originLocation :: String -- 送信元
  }
