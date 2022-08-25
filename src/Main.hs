{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Configurator as C
import Data.GCode (ppGCodeCompact, (&))
import Data.GCode.Generate (feed, g, m, xyz, z, (<#>))
import Data.GCode.Parse (parseOnlyGCode)
import Data.GCode.RS274 (rapid)
import Data.GCode.Types (GCode)
import Data.Text (Text, pack, snoc)
import Data.Text.Conversions (FromText (fromText), toText)
import Data.Text.Encoding (encodeUtf8)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import GCodeTransformations (setOutputSize, toOrigin)
import qualified Graphics.Svg as SVG
import Options.Applicative
import Paths_juicy_gcode (version)
import Render (renderDoc)
import Text.Read (readMaybe)
import Transformation (scaleTransform)
import Types (MachineSettings (MachineSettings))

toGCode :: Text -> GCode
toGCode t = case parseOnlyGCode $ encodeUtf8 (snoc t '\n') of
  Left err -> error $ "failed to parse gcode:" ++ fromText t ++ " with error " ++ err
  Right gcode -> gcode

toGCode' :: String -> GCode
toGCode' = toGCode . toText

defaultFlavor :: MachineSettings
defaultFlavor = MachineSettings [m <#> 3, g <#> 21, g <#> 90] [rapid & xyz 0.0 0.0 5.0, m <#> 2] [rapid & z (-0.125) & feed 2200.0] [rapid & z 5.000] 13000.0

createFinalGcode :: MachineSettings -> GCode -> GCode
createFinalGcode (MachineSettings begin end _ _ _) gops = begin ++ gops ++ end

data Options = Options
  { _svgfile :: String,
    _cfgfile :: Maybe String,
    _outfile :: Maybe String,
    _dpi :: Int,
    _resolution :: Double,
    _generateBezier :: Bool,
    _scale :: (Double, Double),
    _outputSizeX :: Maybe Double,
    _outputSizeY :: Maybe Double
  }

doubleOption :: Mod OptionFields Double -> Parser Double
doubleOption = option $ maybeReader readMaybe

options :: Parser Options
options =
  Options
    <$> argument
      str
      ( metavar "SVGFILE"
          <> help "The SVG file to be converted"
      )
    <*> optional
      ( strOption
          ( long "flavor"
              <> short 'f'
              <> metavar "CONFIGFILE"
              <> help "Configuration of G-Code flavor"
          )
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUTFILE"
              <> help "The output G-Code file (default is standard output)"
          )
      )
    <*> option
      auto
      ( long "dpi"
          <> value 96
          <> short 'd'
          <> metavar "DPI"
          <> help "Used to determine the size of the SVG when it does not contain any units; dot per inch (default is 96)"
      )
    <*> option
      auto
      ( long "resolution"
          <> value 0.1
          <> short 'r'
          <> metavar "RESOLUTION"
          <> help "Shorter paths are replaced by line segments; mm (default is 0.1)"
      )
    <*> switch
      ( long "generate-bezier"
          <> short 'b'
          <> help "Generate bezier curves (G5) instead of arcs (G2,G3)"
      )
    <*> option
      auto
      ( long "scale"
          <> value (1.0, 1.0)
          <> short 's'
          <> metavar "SCALE"
          <> help "Scale the input file by (x,y)"
      )
    <*> optional
      ( doubleOption
          ( long "outputWidth"
              <> short 'w'
              <> metavar "OUTPUTWIDTH"
              <> help "Set the output width in mm"
          )
      )
    <*> optional
      ( doubleOption
          ( long "outputHeight"
              <> short 'h'
              <> metavar "OUTPUTHEIGHT"
              <> help "Set the output height in mm"
          )
      )

runWithOptions :: Options -> IO ()
runWithOptions (Options svgFile mbCfg mbOut dpi resolution generateBezier scale outputWidth outputHeight) =
  do
    mbDoc <- SVG.loadSvgFile svgFile
    flavor <- maybe (return defaultFlavor) readFlavor mbCfg
    case mbDoc of
      (Just doc) -> do
        let additionalTransforms = uncurry scaleTransform scale

        let gcode = renderDoc generateBezier dpi resolution doc flavor additionalTransforms
        let gcodeWithTransforms = setOutputSize (outputWidth, outputHeight) $ toOrigin gcode

        writer (ppGCodeCompact $ createFinalGcode flavor gcodeWithTransforms)
      Nothing -> putStrLn "juicy-gcode: error during opening the SVG file"
  where
    writer = maybe putStr writeFile mbOut

readFlavor :: FilePath -> IO MachineSettings
readFlavor cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  begin <- C.require cfg (pack "gcode.begin")
  end <- C.require cfg (pack "gcode.end")
  toolon <- C.require cfg (pack "gcode.toolon")
  tooloff <- C.require cfg (pack "gcode.tooloff")
  travelFeed <- C.require cfg (pack "gcode.travelFeed")
  return $ MachineSettings (toGCode begin) (toGCode end) (toGCode toolon) (toGCode tooloff) travelFeed

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (concat ["juicy-gcode ", showVersion version, ", git revision ", $(gitHash)])
    (long "version" <> short 'v' <> help "Show version")

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts =
      info
        (helper <*> versionOption <*> options)
        ( fullDesc
            <> progDesc "Convert SVGFILE to G-Code"
            <> header "juicy-gcode - The SVG to G-Code converter"
        )
