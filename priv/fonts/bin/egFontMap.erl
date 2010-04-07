-module(egFontMap).
% This module is created by the mkegFontMap function in module eg_afm
-export([handler/1,allFonts/0]).
handler("Times-Roman")-> egFont1;
handler("Times-Bold")-> egFont2;
handler("Helvetica")-> egFont3;
handler("Helvetica-Bold")-> egFont4;
handler("Helvetica-Oblique")-> egFont5;
handler("Helvetica-BoldOblique")-> egFont6;
handler("Times-Italic")-> egFont7;
handler("Times-BoldItalic")-> egFont8;
handler("ZapfDingbats")-> egFont9;
handler("Courier")-> egFont10;
handler("Courier-Bold")-> egFont11;
handler("Courier-Oblique")-> egFont12;
handler("Courier-BoldOblique")-> egFont13;
handler("Symbol")-> egFont14;
handler(_) -> undefined.
allFonts() -> ["Times-Roman","Times-Bold","Helvetica","Helvetica-Bold","Helvetica-Oblique",
 "Helvetica-BoldOblique","Times-Italic","Times-BoldItalic","ZapfDingbats",
 "Courier","Courier-Bold","Courier-Oblique","Courier-BoldOblique","Symbol"].
