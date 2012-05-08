-module(eg_font_16).
-export([width/1, kern/2, fontName/0, firstChar/0,lastChar/0]).
-export([index/0,ascender/0,capHeight/0,descender/0,italicAngle/0]).
-export([xHeight/0, flags/0, type/0, stemV/0,fontBBox/0,widths/0]).
-export([encoding/0]).
fontName() -> "OCR-A-Digits".
index() -> 16.
type() -> external.
encoding() -> "AdobeStandardEncoding".
firstChar() ->32.
lastChar() ->160.
ascender() ->814.
capHeight() ->610.
descender() ->-186.
italicAngle() ->0.
xHeight() ->407.
flags() ->32.
stemV() ->70.
fontBBox() ->{0,0,523,666}.
widths() ->[200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,620,620,620,620,620,620,620,620,620,620,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,500].
width(32)->200;
width(48)->620;
width(49)->620;
width(50)->620;
width(51)->620;
width(52)->620;
width(53)->620;
width(54)->620;
width(55)->620;
width(56)->620;
width(57)->620;
width(160)->500;
width(_)->unknown.
kern(_,_) -> 0.
