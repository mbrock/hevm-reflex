{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Control.Monad.State.Strict hiding (state)
import Data.Monoid
import Data.Maybe
import Data.SCargot
import Data.SCargot.Language.HaskLike
import Data.SCargot.Repr
import EVM
import EVM.Solidity
import EVM.Emacs hiding (main, display)
import EVM.TTY (currentSrcMap)
import EVM.Dapp
import Reflex.Dom
import Reflex.Dom.Class
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle
import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)

cmd t xs =
  get >>= \x -> handleCmd x (t, map (WFSAtom . HSString) xs)

setup :: Console ()
setup =
  cmd "load-dapp"
    [ "../ds-token"
    , "../ds-token/out/token.t.sol.json"
    ]

basedOn :: (PostBuild t f, DomBuilder t f) => Dynamic t a -> (a -> f b) -> f ()
basedOn x f = void (dyn (fmap f x))

command ui t xs =
  liftIO (execStateT (cmd t xs) ui)

main :: IO ()
main =
  execStateT setup UiStarted >>=
    \case
      ui0@(UiDappLoaded dapp) ->
        mainWidget $ do
          testChooser <- el "div" $ dropdown Nothing
            (pure
              (Map.fromList $
                (Nothing, "Choose a unit test...") :
                [ (Just (t, x), t <> ": " <> x)
                | (t, xs) <- view dappUnitTests dapp, x <- xs
                ]))
            def
            
          basedOn (view dropdown_value testChooser) $
            \case
              Nothing -> blank
              Just (contractName, testName) -> do
                command ui0 "run-test" [contractName, testName] >>=
                  \case
                    ui1@(UiVm s0) -> do
                      rec
                        foo <- foldDynM
                          (\_ ui@(UiVm s) ->
                             liftIO $
                               execStateT
                                 (takeStep s StepNormally (StepUntil (isNextSourcePosition s)))
                                 ui)
                          ui1
                          step
                        step <- button "Step"
                        basedOn foo $
                          \case
                            UiVm s -> do
                              text "PC: "
                              display . pure $ view (uiVm . state . pc) s
                              text " "
                              -- maybe blank text (view uiVmMessage s)
                              fromMaybe (text "<no source map>") $ do
                                dapp <- view uiVmDapp s
                                sm <- currentSrcMap dapp (view uiVm s)
                                (_, bytes) <- view (dappSources . sourceFiles . at (srcMapFile sm)) dapp
                                let
                                  before = BS.take (srcMapOffset sm) bytes
                                  snippet = BS.take (srcMapLength sm) (BS.drop (srcMapOffset sm) bytes)
                                  after = BS.drop (srcMapLength sm + srcMapOffset sm) bytes
                                pure $ do
                                  pre <- elAttr "pre" ("style" =: "max-height: 600px; overflow: scroll") $ do
                                    text . Text.dropEnd 1 . Text.unlines . reverse . take 8 . reverse . Text.lines . decodeUtf8 $ before
                                    el "b" (text (decodeUtf8 snippet))
                                    text (decodeUtf8 after)
                                  -- void . liftJS . liftJSM . eval $
                                  --   ("setTimeout(function() {var pre = document.querySelector('pre'); if (!pre) return; var b = pre.querySelector('b'); pre.scrollTop = b.offsetTop - pre.offsetHeight / 4;}, 0)" :: String)
                                  blank
                      blank
                      
          blank
