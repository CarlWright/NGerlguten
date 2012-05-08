-module(eg_font_map).
-export([handler/1,all_fonts/0]).
handler("Courier-BoldOblique")-> eg_font_1;
handler("Courier-Bold")-> eg_font_2;
handler("Courier")-> eg_font_3;
handler("Courier-Oblique")-> eg_font_4;
handler("Helvetica-Oblique")-> eg_font_5;
handler("Helvetica-Bold")-> eg_font_6;
handler("Helvetica")-> eg_font_7;
handler("Helvetica-BoldOblique")-> eg_font_8;
handler("Symbol")-> eg_font_9;
handler("Times-Italic")-> eg_font_10;
handler("Times-Bold")-> eg_font_11;
handler("Times-BoldItalic")-> eg_font_12;
handler("Times-Roman")-> eg_font_13;
handler("ZapfDingbats")-> eg_font_14;
handler("Victorias-Secret")-> eg_font_15;
handler("OCR-A-Digits")-> eg_font_16;
handler("OCR-B-Digits")-> eg_font_17;
handler(_) -> undefined.
all_fonts() -> ["Courier-BoldOblique","Courier-Bold","Courier","Courier-Oblique",
 "Helvetica-Oblique","Helvetica-Bold","Helvetica","Helvetica-BoldOblique",
 "Symbol","Times-Italic","Times-Bold","Times-BoldItalic","Times-Roman",
 "ZapfDingbats","Victorias-Secret","OCR-A-Digits","OCR-B-Digits"].
