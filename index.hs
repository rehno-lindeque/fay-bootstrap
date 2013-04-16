
module Index (main) where

import Prelude
import FFI

data HTMLElement a = 
  -- Root element
    HTML [a]
  -- Metadata and scripting
  | HEAD [a]
  | TITLE [a]
  | META [a]
  | BASE [a]
  | LINK [a]
  | STYLE [a]
  | NOSCRIPT [a]
  | SCRIPT [a]
  -- Text-level semantics
  | SPAN [a] 
  | A [a]
  | RT [a]
  | RP [a]
  | DFN [a]
  | ABBR [a]
  | Q [a]
  | CITE [a]
  | EM [a]
  | TIME [a]
  | VAR [a]
  | SAMP [a]
  | I [a]
  | B [a]
  | SUB [a]
  | SUP [a]
  | SMALL [a]
  | STRONG [a]
  | MARK [a]
  | RUBY [a]
  | INS [a]
  | DEL [a]
  | BDI [a]
  | BDO [a]
  | S [a]
  | KBD [a]
  | WBR [a]
  | CODE [a]
  -- Grouping content
  | BR [a]
  | HR [a]
  | FIGCAPTION [a]
  | FIGURE [a]
  | P [a]
  | OL [a]
  | UL [a]
  | LI [a]
  | DIV [a]
  | PRE [a]
  | BLOCKQUOTE [a]
  | DL [a]
  | DT [a]
  | DD [a]
  -- Forms
  | FIELDSET [a]
  | METER [a]
  | LEGEND [a]
  | LABEL [a]
  | INPUT [a]
  | TEXTAREA [a]
  | FORM [a]
  | SELECT [a]
  | OPTGROUP [a]
  | OPTION [a]
  | OUTPUT [a]
  | BUTTON [a]
  | DATALIST [a]
  | KEYGEN [a]
  | PROGRESS [a]
  -- Document sections
  | BODY [a]
  | ASIDE [a]
  | ADDRESS [a]
  | H1 [a]
  | H2 [a]
  | H3 [a]
  | H4 [a]
  | H5 [a]
  | H6 [a]
  | SECTION [a]
  | HEADER [a]
  | NAV [a]
  | ARTICLE [a]
  | FOOTER [a]
  | HGROUP [a]
  -- Tabular data
  | COL [a]
  | COLGROUP [a]
  | CAPTION [a]
  | TABLE [a]
  | TR [a]
  | TD [a]
  | TH [a]
  | TBODY [a]
  | THEAD [a]
  | TFOOT [a]
  -- Interactive elements
  | MENU [a]
  | COMMAND [a]
  | SUMMARY [a]
  | DETAILS [a]
  -- Embedding content
  | IMG [a]
  | AREA [a]
  | MAP [a]
  | EMBED [a]
  | OBJECT [a]
  | PARAM [a]
  | SOURCE [a]
  | IFRAME [a]
  | CANVAS [a]
  | TRACK [a]
  | AUDIO [a]
  | VIDEO [a]
  deriving (Read, Show)

{-


-}

showString :: String -> String
showString = ffi "JSON.stringify(%1)"

showEl :: (HTMLElement [String]) -> String
showEl = ffi "JSON.stringify(%1)"

-- instance Show HTMLElement

main = putStrLn (showEl $ DIV [(showEl $ SPAN [showString "text"]), (showEl $ SPAN [])])
--main = putStrLn $ showString "text"
