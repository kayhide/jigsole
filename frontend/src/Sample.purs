module Sample where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Puzzle (Puzzle)
import Data.Puzzle as Puzzle
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax


loadPuzzle :: forall eff. String -> Aff (ajax :: AJAX | eff) Puzzle
loadPuzzle path = do
    res <- Affjax.get $ "/samples/" <> path <> ".json"
    either (const $ throwError $ error "Failed to load sample") pure
      $ runExcept $ Puzzle.readPuzzle res.response
