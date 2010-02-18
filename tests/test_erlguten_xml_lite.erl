-module (test_erlguten_xml_lite).

-include_lib ("eunit/include/eunit.hrl").

erlguten_xml_lite_test_()->
[?_assert(erlguten_xml_lite:parse_all_forms("<a>aa<b>aaa</b></a>") =:= [{xml,{a,[],[{raw,"aa"},{b,[],[{raw,"aaa"}]}]}}]),

?_assert(erlguten_xml_lite:parse_all_forms("<a>bc</i>") =:= {error,{errorInLine,0,{badendtagfound,"i",starttagis,"a"}}}),

?_assert(erlguten_xml_lite:parse_all_forms("<p>aaa<br/>aaa</p>") =:= [{xml,{p,[],[{raw,"aaa"},{br,[],[]},{raw,"aaa"}]}}]),

?_assert(erlguten_xml_lite:parse_all_forms("<?xml version=\"1.0\"?>
<!DOCTYPE report SYSTEM \"report-xml.dtd\">
<report>
  <header>a</header></report>") =:= [{pi,"xml version=\"1.0\""},
 {doctype," report SYSTEM \"report-xml.dtd\""},
 {xml,{report,[],[{header,[],[{raw,"a"}]}]}}]),
 
?_assert(erlguten_xml_lite:parse_all_forms("<p>aaaa<![CDATA[
zip a doodly]]> aa </p>") =:= [{xml,{p,[],
         [{raw,"aaaa"},{cdata,"\nzip a doodly"},{raw," aa "}]}}]),
  
?_assert(erlguten_xml_lite:parse_all_forms("<?xml version=\"1.0\"?>
<!DOCTYPE fooy doody>
<!-- this is just a ball of fun -->
<p>aaa</p>") =:= [{pi,"xml version=\"1.0\""},
 {doctype," fooy doody"},
 {comment," this is just a ball of fun "},
 {xml,{p,[],[{raw,"aaa"}]}}]),
         
 ?_assert(erlguten_xml_lite:parse_all_forms("<p>aaa<br/>aaa</p>") =:= [{xml,{p,[],[{raw,"aaa"},{br,[],[]},{raw,"aaa"}]}}])
].

