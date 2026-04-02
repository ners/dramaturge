module Itinerary where

import Control.Lens.Combinators (filtered)
import Control.Lens.Operators ((%~))
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Effectful
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import GHC.Generics (Generic)
import Text.URI (URI)
import Prelude

data Itinerary = Itinerary
    { visited :: HashSet URI
    , queue :: Seq URI
    }
    deriving stock (Generic)

empty :: Itinerary
empty =
    Itinerary
        { visited = mempty
        , queue = mempty
        }

push :: (State Itinerary :> es) => URI -> Eff es ()
push uri =
    State.modify $
        filtered (not . HashSet.member uri . visited)
            %~ (#visited %~ HashSet.insert uri) . (#queue %~ (|> uri))

pop :: (State Itinerary :> es) => Eff es (Maybe URI)
pop =
    State.gets (Seq.viewl . queue) >>= \case
        Seq.EmptyL -> pure Nothing
        url Seq.:< queue -> do
            State.modify \s -> s{queue}
            pure $ Just url
