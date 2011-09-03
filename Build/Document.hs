module Build.Document where


import Data.String.Utils hiding (join)
import System.Cmd
import System.Exit
import System.FilePath
import System.Directory
import System.IO
import Data.List hiding (find)
import Control.Monad
import Control.Applicative
import System.FilePath.FilePather

type Name =
  String

type Title =
  String

type Package =
  String

type DependencyURI =
  String

type Version =
  String

type Extension =
  String

data Versions = Versions {
  avalon_framework :: Version
, batik_all :: Version
, commons_io :: Version
, commons_logging :: Version
, docbook_xml :: Version
, docbook_xsl :: Version
, fop_hyph :: Version
, fop :: Version
, serializer :: Version
, xalan :: Version
, xerces :: Version
, xml_apis :: Version
, xml_apis_ext :: Version
, xmlgraphics_commons :: Version
} deriving (Eq, Show)

data Config = Config {
  name :: Name
, title :: Title
, commitMessage :: String
, params :: [(String, String)]
, versions :: Versions
, dependencyURI :: DependencyURI
, printCommand :: Bool
} deriving (Eq, Show)

defaultVersions :: Versions
defaultVersions = Versions {
  avalon_framework = "4.2.0"
, batik_all = "1.7"
, commons_io = "1.3.1"
, commons_logging = "1.0.4"
, docbook_xml = "4.5"
, docbook_xsl = "1.76.1"
, fop = "1.0"
, fop_hyph = "1.2"
, serializer = "2.7.1"
, xalan = "2.7.1"
, xerces = "2.7.1"
, xml_apis = "1.3.04"
, xml_apis_ext = "1.3.04"
, xmlgraphics_commons = "1.4"
}

defaultConfig ::
  Name -- ^ The name.    
  -> Title -- ^ The title.           
  -> Config
defaultConfig n t =
  Config n t
         "Automated Versioning"
         [
           ("html.stylesheet", takeFileName style)
         , ("chunk.section.depth", "3")
         , ("paper.type", "A4")
         , ("draft.watermark.image", [])
         , ("body.font.family", "Delicious")
         , ("monospace.font.family", "Courier")
         , ("title.font.family", "Gentium Basic")
         , ("navig.showtitles", "0")
         ]
         defaultVersions
         "https://raw.github.com/tonymorris/docbook-dependencies/2.0.0"
         True

spellingErrors ::
  FilePath
spellingErrors =
  "spellingerrors"

lib ::
  FilePath
lib =
  "lib"

libGot ::
  FilePath
libGot =
  lib </> ".got"

src ::
  FilePath
src =
  "src"

docbooksrc ::
  FilePath
docbooksrc =
  src </> "docbook"

indexFile ::
  FilePath
indexFile =
  docbooksrc </> "index.xml"

webDir ::
  FilePath
webDir =
  "etc" </> "web"

htmlIndex ::
  FilePath
htmlIndex =
  "etc" </> "$NAME.html"

resources ::
  FilePath
resources =
  "resources"

style ::
  FilePath
style =
  "etc" </> "style.css"

build ::
  FilePath
build =
  "dist"

dist ::
  Config
  -> FilePath
dist c =
  build </> name c

release ::
  FilePath
release =
  build </> "release"

reviewRelease ::
  FilePath
reviewRelease =
  build </> "reviewRelease"

archive ::
  Config
  -> FilePath
archive c =
  name c ++ ".tar.gz"

dependencies ::
  Config
  -> [(Package, Version, Extension)]
dependencies c =
  (\(p, v, e) -> (p, v . versions $ c, e)) <$>  
  [
    ("avalon-framework", avalon_framework, "jar")
  , ("batik-all", batik_all, "jar")
  , ("commons-io", commons_io, "jar")
  , ("commons-logging", commons_logging, "jar")
  , ("fop", fop, "jar")
  , ("fop-hyph", fop_hyph, "jar")
  , ("serializer", serializer, "jar")
  , ("xalan", xalan, "jar")
  , ("xercesImpl", xerces, "jar")
  , ("xml-apis", xml_apis, "jar")
  , ("xml-apis-ext", xml_apis_ext, "jar")
  , ("xmlgraphics-commons", xmlgraphics_commons, "jar")
  , ("docbook-xsl", docbook_xsl, "zip")
  , ("docbook-xml", docbook_xml, "zip")
  ]

jarDependencies ::
  Config
  -> [FilePath]
jarDependencies c =
  (\(p, v, e) -> lib </> p ++ '-' : v ++ '.' : e) <$> dependencies c

classpath ::
  Config
  -> String
classpath =
  intercalate [searchPathSeparator] . jarDependencies

uriDependencies ::
  Config
  -> [FilePath]
uriDependencies c =
  (\(p, v, e) -> dependencyURI c </> p ++ '-' : v ++ '.' : e) <$> dependencies c

getDependencies ::
  Config
  -> IO ExitCode
getDependencies c =
  do mkdir lib
     r <- fmap (\d -> unwords ["wget", "-P", lib, d]) (uriDependencies c) ++
          [
            unwords [ "unzip -d "
                    , lib
                    , lib </> join ["docbook-xsl-", docbook_xsl . versions $ c, ".zip"]
                    ]
          , unwords [ "rsync"
                    , "-aH"
                    , "xslt/"
                    , lib </> join ["docbook-xsl-", docbook_xsl . versions $ c]
                    ]
          ] >--> c
     touch libGot
     return r

getDependenciesIfNotGot ::
  Config
  -> IO ExitCode
getDependenciesIfNotGot c =
  do e <- doesFileExist libGot
     if e then return ExitSuccess else getDependencies c

withDependencies ::
  Config
  -> IO a
  -> IO a
withDependencies c =
  (getDependenciesIfNotGot c >>)

xsltproc' ::
  Config -- ^ The build configuration.
  -> String -- ^ The output directory or file.
  -> FilePath -- ^ The XSL style.
  -> String -- ^ The input XML.
  -> String
xsltproc' c out xsl xml =
  unwords [ "xsltproc"
          , "--catalogs"
          , "--xinclude"
          , "--nonet "
          , p (params c)
          , "--output"
          , out
          , lib </> "docbook-xsl-" ++ (docbook_xsl . versions $ c) </> xsl
          , xml
          ]
    where
    p = unwords . map (\(k, v) -> "--stringparam \"" ++ k ++ "\" \"" ++ v ++ "\"")

mkdirThen ::
  Config
  -> FilePath
  -> String
  -> IO ExitCode
mkdirThen c d k =
  mkdir d >> system' c k

xsltproc ::
  Config
  -> FilePath
  -> FilePath
  -> IO ExitCode
xsltproc c o x =
  withDependencies c (mkdirThen c (dist c) (xsltproc' c (dist c </> o) x indexFile))

markup ::
  FilePath -- ^ The output directory or file.
  -> FilePath -- ^ The XSL style.
  -> FilePath -- ^ The output directory.
  -> Config -- ^ The build configuration
  -> IO ExitCode
markup out xsl d c =
  xsltproc c out xsl <* copyStyle c d *> copyResources c (dist c </> d)

html ::
  Config
  -> IO ExitCode
html =
  markup "html/index.html" "html/docbook-utf8.xsl" "html"

chunkHtml ::
  Config
  -> IO ExitCode
chunkHtml =
  markup "chunk-html/index.html" "html/docbook-chunk-utf8.xsl" "chunk-html"

xhtml ::
  Config
  -> IO ExitCode
xhtml =
  markup "xhtml/index.html" "html/docbook-utf8.xsl" "xhtml"

chunkXhtml :: 
  Config
  -> IO ExitCode
chunkXhtml =
  markup "chunk-xhtml/index.html" "html/docbook-chunk-utf8.xsl" "chunk-xhtml"

fo ::
  Config
  -> IO ExitCode
fo c =
  xsltproc c ("fo" </> "index.fo") ("fo" </> "docbook.xsl")

foIfNotAlreadyFo ::
  Config
  -> IO a
  -> IO a
foIfNotAlreadyFo c a =
  do e <- doesFileExist (dist c </> "fo" </> "index.fo")
     (if e 
        then id
        else (fo c >>)) a

fop' ::
  Config -- ^ The build configuration.
  -> FilePath -- ^ The distribution directory.
  -> FilePath -- ^ The output file.
  -> IO ExitCode
fop' c t f =
  withDependencies c .
  foIfNotAlreadyFo c $
  mkdirThen c (dist c </> t) (unwords [ "java"
                                      , "-classpath"
                                      , classpath c
                                      , "org.apache.fop.cli.Main"
                                      , "-c"
                                      , "etc/fop.cfg"
                                      , "-fo"
                                      , dist c </> "fo/index.fo -" ++ t
                                      , dist c </> f
                                      ])

pdf ::  
  Config
  -> IO ExitCode
pdf c =
  fop' c "pdf" ("pdf" </> "index.pdf")

ps ::
  Config
  -> IO ExitCode
ps c =
  fop' c "ps" ("ps" </> "index.ps")

pcl ::
  Config
  -> IO ExitCode
pcl c =
  fop' c "pcl" ("pcl" </> "index.pcl")

png ::
  Config
  -> IO ExitCode
png c =
  fop' c "png" ("png" </> "index.png")

rtf ::  
  Config
  -> IO ExitCode
rtf c =
  fop' c "rtf" ("rtf" </> "index.rtf")

clean ::
  IO ()
clean =
  rmdir build

cleanAll ::
  IO ()
cleanAll =
  clean >> rmdir lib

buildAll ::
  [Config -> IO ExitCode]
buildAll =
  [fo, html, chunkHtml, xhtml, chunkXhtml, fo, pdf, ps, pcl, png, rtf]

cleanBuildTargets ::
  [Config -> IO ExitCode]
  -> Config
  -> IO ()
cleanBuildTargets t c =
  clean >> mapM_ ($c) t

alll ::
	Config
  -> IO ExitCode
alll c =
  (squish . sequence buildAll) c

web ::
  Config
  -> IO ExitCode
web c =
  alll c >->
  do h <- find always (extensionEq "html") webDir
     mapM_ (\p -> let p' = joinPath . drop (length . splitPath $ webDir) . splitPath $ p
                      d = takeDirectory p'
                  in do mkdir (build </> d)
                        f <- readFile p
                        let z = replace' f [("$TITLE", title c), ("$NAME", name c)]
                        writeFile (build </> p') z) h
     r <- find always (extensionSatisfies $ \p -> takeExtension p /= ".html") webDir
     p' <- filterM doesFileExist r
     mapM_ (\p -> let z = joinPath . drop (length . splitPath $ webDir) . splitPath $ p 
                  in do mkdir (build </> takeDirectory z)
                        copyFile p (build </> z)) p'
     t <- readFile htmlIndex
     d <- getDirectoryContents (dist c </> "png")
     let pngh = (\(i, k) -> "<li class=\"" ++ k : "\"><a href=\"" ++ name c ++ "png/index" ++ i ++ ".png\">Page</a></li>") =<< ([] : map show [2 .. length . filter ("index" `isPrefixOf`) $ d]) `zip` join (repeat "ox")
     writeFile (build </> name c ++ ".html") (replace' t [("$NAME", name c), ("$PNGPAGES", pngh), ("$TITLE", title c)])
     system' c (unwords ["tar -C ", build, " -zcf ", build </> archive c, " ", name c])

releaseBuild ::
  FilePath
  -> [Config -> IO ExitCode]
  -> Config
  -> IO ExitCode
releaseBuild d t c =
  do cleanBuildTargets t c
     mkdir d
     system' c (unwords [ "tar"
                        , "-C"
                        , build
                        , "-zcf"
                        , d </> archive c
                        , name c
                        ])

rel ::
  Config
  -> IO ExitCode
rel =
  releaseBuild release buildAll

allSgml ::
  IO [FilePath]
allSgml =
  System.FilePath.FilePather.find always (extensionEq "xml") docbooksrc

aspell ::
  String
  -> [String]
  -> String
aspell skip x =
  unwords ([ 
             "aspell"
           , "--dont-backup"
           , "--master=en_GB"
           , "--encoding=utf-8"
           , "--mode=sgml"
           , "-p"
           , "." </> "etc" </> "aspell-words.txt"          
           ] ++ (("--add-sgml-skip=" ++) <$> words skip) ++ x)

spellcheck ::
  Config
  -> IO ()
spellcheck c =
  do p <- allSgml
     s <- readFile ("etc" </> "add-sgml-skip")    
     mapM_ (\z -> 
       system' c (aspell s ["-c", z])) p

spellcheckNonInteractive ::
  Config
  -> IO ExitCode
spellcheckNonInteractive c =
  do p <- allSgml
     s <- readFile ("etc" </> "add-sgml-skip")  
     let cat = unwords ["cat", intercalate " " p]
     let asp = aspell s ["list"]
     exitWith' $ system' c (cat ++ " | " ++ asp ++ " > " ++ spellingErrors) >-> 
       do e <- readFile spellingErrors
          let n = length . lines $ e
          putStrLn "****************"
          putStrLn e
          putStrLn ("(" ++ show n ++ ") spelling error" ++ if n == 1 then [] else "s")
          putStrLn "****************"
          return $ if n == 0
                     then ExitSuccess
                     else ExitFailure 12
   
lint ::
  Config
  -> IO ExitCode
lint c = 
  system' c (unwords [ "xmllint"
                     , "--noout"
                     , "--catalogs"
                     , "--nonet"
                     , "--xinclude"
                     , "--postvalid"
                     , indexFile
                     ])

-- no export

system' ::
  Config
  -> String
  -> IO ExitCode
system' c s =
  do printCommand c `when` (putStr ">> " >> putStrLn s)
     system s

touch ::
  FilePath
  -> IO ()
touch f =
  openFile f AppendMode >>= hClose

mkdir ::
  FilePath
  -> IO ()
mkdir =
  createDirectoryIfMissing True

rmdir ::
  FilePath
  -> IO ()
rmdir d =
  doesDirectoryExist d >>= flip when (removeDirectoryRecursive d)

copyStyle ::
  Config -- ^ The build configuration.
  -> FilePath -- ^ The output directory.
  -> IO ()
copyStyle c d =
  copyFile style (dist c </> d </> "style.css")

copyResources ::
  Config
  -> String
  -> IO ExitCode
copyResources c d =
  system' c (unwords [ "rsync"
                     , "-a"
                     , resources
                     , d
                     ])

exitWith' ::
  IO ExitCode
  -> IO a
exitWith' z =
  do e <- z
     exitWith e

squish ::
  [IO ExitCode]
  -> IO ExitCode
squish =
  foldr (>->) (return ExitSuccess)

(>-->) ::
  [String]
  -> Config
  -> IO ExitCode
m >--> c =
  squish (map (system' c) m)

infixl 4 >-->

(>->) ::
  IO ExitCode
  -> IO ExitCode
  -> IO ExitCode
x >-> y =
  do x' <- x
     if x' == ExitSuccess
       then y
       else return x'

infixl 4 >->

replace' ::
  String
  -> [(String, String)]  
  -> String
replace' =
  foldl' (\s (x, y) -> replace x y s)
  
