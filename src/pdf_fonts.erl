-module(pdf_fonts).

-export([allFonts/0, fontNumber/1, fontInfo/1, fontHandler/1, 
	 fontHandler1/1,
	 fontInfo/1]).

%% allFonts()           -> [SymName]
%% fontIndex(IntName)   -> Int    (call the font F1)
%% fontHandler(SymName) -> Atom | eNoFont
%% fontInfo(Font)       ->
%%   {I,pdf_embedded} | {I, pdf_builtin} | external.


allFonts() ->
    ["Helvetica","Helvetica-Bold","Helvetica-Oblique","Helvetica-BoldOblique",
     "Times-Roman","Times-Bold","Times-Italic","Times-BoldItalic",
     "Courier","Courier-Bold","Courier-Oblique","Courier-BoldOblique",
     "Symbol",
     "ZapfDingbats",
     "AvantGarde-Book","AvantGarde-BookOblique",
     "AvantGarde-Demi","AvantGarde-DemiOblique",
     "Bookman-Demi","Bookman-DemiItalic",
     "Bookman-Light","Bookman-LightItalic",
     "Helvetica-Narrow","Helvetica-Narrow-Oblique",
     "Helvetica-Narrow-Bold","Helvetica-Narrow-BoldOblique",
     "NewCenturySchlbk-Roman","NewCenturySchlbk-Italic",
     "NewCenturySchlbk-Bold","NewCenturySchlbk-BoldItalic",
     "Palatino-Roman","Palatino-Italic",
     "Palatino-Bold","Palatino-BoldItalic",
     "ZapfChancery-MediumItalic",
     "Helvetica-Condensed","Helvetica-Condensed-Bold",
     "Helvetica-Condensed-Oblique","Helvetica-Condensed-BoldObl"].

fontInfo(Atom) ->
    case fontNumber(Atom) of
	I when I >= 1,I =< 14 ->
	    {I, pdf_embedded};
	I when I >= 14,I =< 35 ->
	    {I, pdf_builtin};
	_ ->
	    external
    end.

fontHandler(Name) ->
    case fontHandler1(Name) of
	eNoFont ->
	    io:format("There is no font named:~s~n"
		      "Substituting Times-Roman",[Name]),
	    fontHandler1("Times-Roman");
	N ->
	    N
    end.

%% The first 14 fonts are completely built-in
fontHandler1("Helvetica")         -> eHelvetica;
fontHandler1("Helvetica-Bold")    -> eHelvetica_Bold;
fontHandler1("Helvetica-Oblique") -> eHelvetica_Oblique;
fontHandler1("Helvetica-BoldOblique") ->  eHelvetica_BoldOblique;
fontHandler1("Times-Roman") -> eTimes_Roman;
fontHandler1("Times-Bold") ->  eTimes_Bold;
fontHandler1("Times-Italic") -> eTimes_Italic;
fontHandler1("Times-BoldItalic") -> eTimes_BoldItalic;
fontHandler1("Courier") -> eCourier;
fontHandler1("Courier-Bold") ->  eCourier_Bold;
fontHandler1("Courier-Oblique") -> eCourier_Oblique;
fontHandler1("Courier-BoldOblique") -> eCourier_BoldOblique;
fontHandler1("Symbol") ->  eSymbol;
fontHandler1("ZapfDingbats") ->  eZapfDingbats;
%% The next 25 fonts need width tables
fontHandler1("AvantGarde-Book") -> eAvantGarde_Book;
fontHandler1("AvantGarde-BookOblique") -> eAvantGarde_BookOblique;
fontHandler1("AvantGarde-Demi") -> eAvantGarde_Demi;
fontHandler1("AvantGarde-DemiOblique") -> eAvantGarde_DemiOblique;
fontHandler1("Bookman-Demi") -> eBookman_Demi;
fontHandler1("Bookman-DemiItalic") -> eBookman_DemiItalic;
fontHandler1("Bookman-Light") -> eBookman_Light;
fontHandler1("Bookman-LightItalic") -> eBookman_LightItalic;
fontHandler1("Helvetica-Narrow") -> eHelvetica_Narrow;
fontHandler1("Helvetica-Narrow-Oblique") -> eHelvetica_Narrow_Oblique;
fontHandler1("Helvetica-Narrow-Bold") -> eHelvetica_Narrow_Bold;
fontHandler1("Helvetica-Narrow-BoldOblique") -> eHelvetica_Narrow_BoldOblique;
fontHandler1("NewCenturySchlbk-Roman") -> eNewCenturySchlbk_Roman;
fontHandler1("NewCenturySchlbk-Italic") -> eNewCenturySchlbk_Italic;
fontHandler1("NewCenturySchlbk-Bold") -> eNewCenturySchlbk_Bold;
fontHandler1("NewCenturySchlbk-BoldItalic") -> eNewCenturySchlbk_BoldItalic;
fontHandler1("Palatino-Roman") -> ePalatino_Roman;
fontHandler1("Palatino-Italic") -> ePalatino_Italic;
fontHandler1("Palatino-Bold") ->  ePalatino_Bold;
fontHandler1("Palatino-BoldItalic") ->  ePalatino_BoldItalic;
fontHandler1("ZapfChancery-MediumItalic") -> eZapfChancery_MediumItalic;
fontHandler1(X) -> eNoFont.

fontNumber(eTimes_Roman) -> 1;
fontNumber(eTimes_Bold) -> 2;
fontNumber(eTimes_Italic) -> 3;
fontNumber(eTimes_BoldItalic) -> 4;
fontNumber(eHelvetica) -> 5;
fontNumber(eHelvetica_Bold) -> 6;
fontNumber(eHelvetica_Oblique) -> 7;
fontNumber(eHelvetica_BoldOblique) -> 8;
fontNumber(eCourier) -> 9;
fontNumber(eCourier_Bold) -> 10;
fontNumber(eCourier_Oblique) -> 11;
fontNumber(eCourier_BoldOblique) -> 12;
fontNumber(eSymbol) -> 13;
fontNumber(eZapfDingbats) -> 14;
fontNumber(eAvantGarde_Book) -> 15;
fontNumber(eAvantGarde_BookOblique) -> 16;
fontNumber(eAvantGarde_Demi) -> 17;
fontNumber(eAvantGarde_DemiOblique) -> 18;
fontNumber(eBookman_Demi) -> 19;
fontNumber(eBookman_DemiItalic) -> 20;
fontNumber(eBookman_Light) -> 21;
fontNumber(eBookman_LightItalic) -> 22;
fontNumber(eHelvetica_Narrow) -> 23;
fontNumber(eHelvetica_Narrow_Oblique) -> 24;
fontNumber(eHelvetica_Narrow_Bold) -> 25;
fontNumber(eHelvetica_Narrow_BoldOblique) -> 26;
fontNumber(eNewCenturySchlbk_Roman) -> 27;
fontNumber(eNewCenturySchlbk_Italic) -> 28;
fontNumber(eNewCenturySchlbk_Bold) -> 29;
fontNumber(eNewCenturySchlbk_BoldItalic) -> 30;
fontNumber(ePalatino_Roman) -> 31;
fontNumber(ePalatino_Italic) -> 32;
fontNumber(ePalatino_Bold) -> 33;
fontNumber(ePalatino_BoldItalic) -> 34;
fontNumber(eZapfChancery_MediumItalic) -> 35;
fontNumber(X) -> not_pdf.







