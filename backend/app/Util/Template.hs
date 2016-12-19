{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Template
  ( echoTemplateErrors
  ) where


import           Control.Monad (forM_)
import           Servant.EDE   (TemplateError)


echoTemplateErrors :: [TemplateError] -> IO ()
echoTemplateErrors errors = do
  let echoError (filePath, err) = do
                putStrLn $ "EDE Template Compile Error in " ++ filePath ++ ": "
                putStrLn $ err
  forM_ errors echoError

