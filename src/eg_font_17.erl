-module(eg_font_17).
-export([width/1, kern/2, fontName/0, firstChar/0,lastChar/0]).
-export([index/0,ascender/0,capHeight/0,descender/0,italicAngle/0]).
-export([xHeight/0, flags/0, type/0, stemV/0,fontBBox/0,widths/0]).
-export([encoding/0]).
fontName() -> "OCR-B-Digits".
index() -> 17.
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
fontBBox() ->{0,-13,621,742}.
widths() ->[200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,763,763,763,763,763,763,763,763,763,763,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,500].
width(32)->200;
width(48)->763;
width(49)->763;
width(50)->763;
width(51)->763;
width(52)->763;
width(53)->763;
width(54)->763;
width(55)->763;
width(56)->763;
width(57)->763;
width(160)->500;
width(_)->unknown.
kern(_,_) -> 0.
