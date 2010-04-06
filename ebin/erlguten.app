{application, erlguten,
 [{description,  "PDF output production system"},
  {vsn,          "2.2"},
  {modules,      [erlguten, erlguten_normalise_xml, erlguten_para_break, 
                  %% xml parsing
                    eg_xml_lite, eg_xml_tokenise, 
                  %% pdf formatting
                    pdf, erlguten_pdf_export, eg_pdf_assemble, 
                  %% font server
                    eg_font_server, eg_afm, 
                  %% line calculations
                    erlguten_lines2pdf, erlguten_line_break, erlguten_geometry ]},
  {registered,   [erlguten_sup, erlguten_font_server]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlguten_app, []}}]}.