{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import            System.Environment
import qualified  Data.Text as T
import            GHC.Generics
import qualified  GHC.Base as G
import            Data.Aeson
import            Data.Maybe



data Platform
   = ANDROID
   | IOS
   | WEB
  deriving (Generic, Show, Eq, Ord, Enum, Read)

instance FromJSON Platform
  where
    parseJSON (String a) = genericParseJSON defaultOptions $ String (T.toUpper a)
    parseJSON _ = fail "Invalid Platform"

instance ToJSON Platform where
    toJSON = String . T.toLower . T.pack . show

createPathRegex :: T.Text -> T.Text -> Platform -> T.Text
createPathRegex "in.juspay.godel.placeholder" asset platform = createPathRegex "in.juspay.godel" asset platform
createPathRegex app asset platform                           = createPathRegex' $
  case asset of
    "index"       -> case platform of
                      WEB -> [stdIndexPathCmn, newIndexPathCmn]
                      _     -> [stdIndexPathApp, stdIndexPathCmn, newIndexPathCmn]

    "manifest"    -> (: [stdManifestPath]) $ case platform of
                      WEB     -> ["hyper", "bundles", "web", "release"]
                      IOS     -> ["juspay", "payments", "release", "IOS"]
                      ANDROID -> ["juspay", "payments", "release"]

    "fonts"       -> [[".*"]]

    "sdk_config"  -> [[".*"]]

    "tracker"     -> (: [["hyper", "bundles", "app", "tracker", semVerRegex]]) $ case platform of
                       WEB -> ["hyper", "bundles", "web", "release", createMappRegex "in.juspay.hyperos", sdkV2Regex', clientIdRegex, "(stable|" <> assetVerRegex <> "|staggered)"]
                       _     -> ["juspay", "payments", "2\\.0", "release(\\/" <> assetVerRegex <> ")?"]

    "config"      -> case (app, platform) of
                       ("in.juspay.godel", _)       -> [[".*"]]
                       ("in.juspay.hyperos", WEB) -> [[".*"]]
                       ("in.juspay.hyperos", _)     -> [["juspay", "payments", "2\\.0", "release(\\/"<> assetVerRegex <> ")?"]]
                       (_, WEB)                   -> [stdAssetPathCmn, newConfigPathCmn]
                       _                            -> [stdAssetPathCmn, stdAssetPathApp, newConfigPathCmn]

    _             -> case (app, platform) of
                       ("in.juspay.godel", _)  -> [[".*"]]
                       (_, WEB)              -> [stdAssetPathCmn, galactusAssetPath, galactusAssetPathV1, galactusAssetPathV1', newConfigPathCmn]
                       ("in.yatri.consumer",_) -> [preReleaseAssetPath, galactusAssetPathV1']
                       ("in.yatri.provider",_) -> [preReleaseAssetPath, galactusAssetPathV1']
                       _                       -> [stdAssetPathCmn, stdAssetPathApp, galactusAssetPath, galactusAssetPathV1, galactusAssetPathV1', newConfigPathCmn]

  where
    createPathRegex' :: [[T.Text]] -> T.Text
    createPathRegex' patterns =
      if asset /= "index" && app == "in.juspay.escrow"
        then (createPathRegex'' patterns) <> "|" <> (createPathRegex "in.juspay.hyperpay" asset platform)
        else createPathRegex'' patterns

    createPathRegex'' :: [[T.Text]] -> T.Text
    createPathRegex'' = T.intercalate "|" . G.map ((bucketPath <>) . T.intercalate "\\/" . (++ [fileName <> "$"]))

    bucketPath =
      case app of
        "in.juspay.godel" -> "^https:\\/\\/.*\\/"
        _                 -> "^https:\\/\\/" <> (T.replace "." "\\." getReadAssetsDomain) <> "\\/"

    stdManifestPath = ["hyper", "bundles", "in\\.juspay\\.merchants", clientIdRegex, platform', "(cug|release)"]

    platform' = T.toLower . T.pack $ show platform

    ext = "\\." <> (getExtensionForAsset platform app asset)

    lookupAppFName "com.juspay.gemi" = \case
      "strings" -> Just $ "strings(_[a-z,A-Z,0-9]+)?\\." <> ext
      fname     -> Just $ fname <> "\\." <> ext
    lookupAppFName _                 = const Nothing

    platformPrefix = case platform of
      WEB -> ""
      _     -> "v1-"

    getDefaultFName fname = platformPrefix <> fname <> ext

    fileName =
      case asset of
        "manifest"              -> "manifest" <> ext
        "sdk_config"            -> "sdk_config" <> ext
        "fonts"                 -> "(.*)" <> ext
        "certificates"          -> "certificates_v1" <> ext
        "acs_js_source"         -> getDefaultFName "acs"
        "boot_loader_js_source" -> getDefaultFName "boot_loader"
        "index"                 -> case platform of
                                    WEB -> getDefaultFName "(prod-split_)?(br[0-9]+-)?index[0-9]*"
                                    _     -> getDefaultFName "index_bundle"
        _                       -> let mbAppFName = lookupAppFName app asset
                                   in  fromMaybe (getDefaultFName asset) mbAppFName

    clientIdRegex = "[a-z,0-9]+((\\.|,|-|_)[a-z,0-9]+)*"
    branchRegex   = "[^\\/]+"

    scope =
      case platform of
        IOS -> "release\\/IOS"
        _     -> "release"

    sdkV1Regex    = "1\\.0rc1"
    sdkV2Regex    = "2\\.0rc1"
    sdkV2Regex'   = "2\\.0\\.0"
    assetVerRegex = "([0-9].){2}[0-9]+"
    semVerRegex   = "([0-9]+\\.){2}[0-9]+"
    calVerRegex   = semVerRegex <> "-(release|hotfix)(-[a-z0-9]+)?-[0-9]+(-[0-9]+)?\\.[0-9]+"
    preReleaseVerRegex = semVerRegex <> "-(main)(-"<>clientIdRegex <>")?\\.[0-9]+"

    mappRegex       = createMappRegex app

    createMappRegex = (<> ("(\\." <> clientIdRegex <> ")?")) . T.replace "." "\\."

    indexV1Regex    = sdkV1Regex <> "_[0-9]+"

    indexV2Regex    = sdkV2Regex <> "_[0-9]+"

    newIndexPathPrefix = ["hyper", "bundles", "app", mappRegex]
    newIndexPathCmn    = newIndexPathPrefix <> case app of
      "com.juspay.gemi" -> [branchRegex, clientIdRegex, platform']
      _                 -> [versionRegex, platform']
      where
        versionList  = [indexV1Regex, indexV2Regex, semVerRegex, calVerRegex]
        versionRegex = "(" <> (T.intercalate "|" versionList) <> ")"

    stdIndexPathCmn =
      case app of
        "in.juspay.hypercredit" -> stdIndexPathCmn' indexV1Regex
        _                       -> stdIndexPathCmn' indexV2Regex
        where
          stdIndexPathCmn' indexVRegex = ["hyper", "bundles", platform', "release", mappRegex, sdkV2Regex', clientIdRegex, "(stable|" <> indexVRegex <> "|staggered)"]
    stdIndexPathApp =
      case app of
        "in.juspay.arya"             -> ["juspay", mappRegex, scope, sdkV1Regex, indexV1Regex]
        "in.juspay.hypercredit"      -> stdIndexPathApp' indexV1Regex
        "neopenkochi.yatri"        -> becknPath
        "neopenkochi.yatripartner" -> becknPath
        _                            -> stdIndexPathApp' indexV2Regex
      where
        becknPath                     = ["juspay", "beckn", mappRegex, scope, sdkV1Regex, indexV1Regex]
        stdIndexPathApp' indexVRegex  = ["juspay", "payments", mappRegex, scope, sdkV2Regex <> "(\\/(" <> indexVRegex <> "|" <> "staggered-release-assets))?"]

    stdAssetPathCmn = ["hyper", "bundles", platform', "release", mappRegex, sdkV2Regex', clientIdRegex, "(stable|" <> assetVerRegex <> "|staggered)"]

    newConfigPathCmn = ["hyper", "bundles", "config", mappRegex, clientIdRegex, versionRegex]
      where
        versionRegex = "(" <> (T.intercalate "|" [calVerRegex, indexV2Regex, indexV1Regex, semVerRegex]) <> ")"

    stdAssetPathApp = ["juspay", "payments", mappRegex, "release" <> (if platform == IOS then "(\\/IOS)?" else  "") <> "(\\/(" <> indexV2Regex <> "|" <> assetVerRegex <> "|" <> "staggered-release-assets))?"]

    galactusAssetPath = ["hyper", "bundles", createMappRegex "in.juspay.merchants", clientIdRegex, "configuration", "[0-9]+\\.[0-9]+"]
    galactusAssetPathV1' = ["hyper", "configs", clientIdRegex, mappRegex, semVerRegex]
    preReleaseAssetPath = ["hyper", "configs", clientIdRegex, mappRegex, preReleaseVerRegex]

    galactusAssetPathV1 = ["hyper", "configs", clientIdRegex, mappRegex, semVerRegex, "configuration"]

    getReadAssetsDomain :: T.Text
    getReadAssetsDomain = T.pack "assets.juspay.in"

    getExtensionForAsset :: Platform -> T.Text -> T.Text -> T.Text
    getExtensionForAsset platform app asset =
      case (app, asset) of
        ("com.juspay.gemi", asset) | asset /= "index" -> "json"
        (_                , "sdk_config")             -> "json"
        (_                , "manifest")               -> "json"
        (_                , "fonts")                  -> "ttf"
        _                                             -> getDefaultFNameExt platform

    getDefaultFNameExt :: Platform -> T.Text
    getDefaultFNameExt =
      \case
        WEB     -> "js"
        IOS     -> "jsa"
        ANDROID -> "zip"
      

main :: IO ()
main = do
        args <- getArgs
        case args of
          [arg1, arg2] -> do
            let android = createPathRegex (T.pack arg1) (T.pack arg2) ANDROID
            let ios = createPathRegex (T.pack arg1) (T.pack arg2) IOS
            let web = createPathRegex (T.pack arg1) (T.pack arg2) WEB
            putStrLn (show (android <> (T.pack "|") <> ios <> (T.pack "|") <> web))
          _     -> error "Provide proper Haskell <argument>"