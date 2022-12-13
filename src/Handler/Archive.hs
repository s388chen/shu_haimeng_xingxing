module Handler.Archive where

import Import hiding (elem, map, (.))
import Prelude (elem, map, (.))

-- | Add a word to user's favorites
postArchiveR :: Handler Value
postArchiveR = do
  archive <- (requireCheckJsonBody :: Handler Archived)
  print archive
  (userId, _) <- requireAuthPair
  archived <- runDB $ selectList [ArchivedUserId ==. Just userId] []
  let archivedWordList = map (archivedWord . entityVal) archived
  if archivedWord archive `elem` archivedWordList
    then returnJson ""
    else do
      let archived' = archive {archivedUserId = Just userId}
      insertedArchived <- runDB $ insertEntity archived'
      returnJson insertedArchived