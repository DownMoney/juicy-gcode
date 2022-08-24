{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Svg as SVG

import Options.Applicative
import Paths_juicy_gcode (version)
import Data.Version (showVersion)
import Development.GitRev (gitHash)

import Data.Text (Text, pack, snoc)
import qualified Data.Configurator as C

import Render ( renderDoc )
import Data.GCode.Types ( GCode )
import Types ( MachineSettings(MachineSettings) )
import Data.GCode (ppGCodeCompact)
import Data.GCode.Parse ( parseOnlyGCode )
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Conversions (toText, FromText (fromText))

toGCode' :: String -> GCode
toGCode' = toGCode . toText 

defaultFlavor :: MachineSettings
defaultFlavor =  MachineSettings (toGCode' "G17\nG90\nG0 Z1\nG0 X0 Y0") (toGCode' "G0 Z1") (toGCode' "G01 Z0 F10.00") (toGCode' "G00 Z1") 

createFinalGcode :: MachineSettings -> GCode -> GCode
createFinalGcode (MachineSettings begin end _ _) gops = begin ++ gops ++ end

data Options = Options { _svgfile        :: String
                       , _cfgfile        :: Maybe String
                       , _outfile        :: Maybe String
                       , _dpi            :: Int
                       , _resolution     :: Double
                       , _generateBezier :: Bool
                       }

options :: Parser Options
options = Options
  <$> argument str
      ( metavar "SVGFILE"
     <> help "The SVG file to be converted" )
  <*> optional (strOption
      ( long "flavor"
     <> short 'f'
     <> metavar "CONFIGFILE"
     <> help "Configuration of G-Code flavor" ))
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUTFILE"
     <> help "The output G-Code file (default is standard output)" ))
  <*> option auto
      ( long "dpi"
     <> value 96
     <> short 'd'
     <> metavar "DPI"
     <> help "Used to determine the size of the SVG when it does not contain any units; dot per inch (default is 96)" )
 <*> option auto
      ( long "resolution"
     <> value 0.1
     <> short 'r'
     <> metavar "RESOLUTION"
     <> help "Shorter paths are replaced by line segments; mm (default is 0.1)" )
  <*> switch
      ( long "generate-bezier"
      <> short 'b'
      <> help "Generate bezier curves (G5) instead of arcs (G2,G3)" )

runWithOptions :: Options -> IO ()
runWithOptions (Options svgFile mbCfg mbOut dpi resolution generateBezier) =
    do
        mbDoc <- SVG.loadSvgFile svgFile
        flavor <- maybe (return defaultFlavor) readFlavor mbCfg
        case mbDoc of
            (Just doc) -> writer (ppGCodeCompact $ createFinalGcode flavor $ renderDoc generateBezier dpi resolution doc flavor)
            Nothing    -> putStrLn "juicy-gcode: error during opening the SVG file"
    where
        writer = maybe putStr writeFile mbOut

toGCode :: Text -> GCode
toGCode t = case parseOnlyGCode $ encodeUtf8 (snoc t '\n') of
  Left err -> error $ "failed to parse gcode:" ++ fromText t ++ " with error " ++ err
  Right gcode -> gcode


readFlavor :: FilePath -> IO MachineSettings
readFlavor cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  begin        <- C.require cfg (pack "gcode.begin")
  end          <- C.require cfg (pack "gcode.end")
  toolon       <- C.require cfg (pack "gcode.toolon")
  tooloff      <- C.require cfg (pack "gcode.tooloff")
  return $ MachineSettings (toGCode begin) (toGCode end) (toGCode toolon) (toGCode tooloff)

versionOption :: Parser (a -> a)
versionOption = infoOption
                    (concat ["juicy-gcode ", showVersion version, ", git revision ", $(gitHash)])
                    (long "version" <> short 'v' <> help "Show version")

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> versionOption <*> options)
      ( fullDesc
     <> progDesc "Convert SVGFILE to G-Code"
     <> header "juicy-gcode - The SVG to G-Code converter" )
