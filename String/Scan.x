{
module String.Scan (
    scanStringInterp
  , scanStringExc
  , scanStringQuote
  )
where


}

$squote = \'
$dquote = \"
@stringchar = \\\" | [^ \" # ] -- " <- this is here to keep the text formatting intact
@stringchar2 = \\\'|[^\'] -- '

tokens :-
  <0> $dquote  {setScanDQuote}
  <0> $squote  {setScanSQuote}

  <dquote> 
{



scanStringInterp :: String -> Exp

scanStringEsc :: String -> Exp

scanStringQuote :: String -> Exp

}
