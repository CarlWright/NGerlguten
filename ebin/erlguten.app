{application, erlguten,
 [{description,  "PDF output production system"},
  {vsn,          "2.2"},
  {modules,      [erlguten, erlguten_normalise_xml, erlguten_para_break, 
                  %% xml parsing
                    erlguten_xml_lite, erlguten_xml_tokenise, 
                  %% pdf formatting
                    pdf, erlguten_pdf_export, erlguten_pdf_assemble, 
                  %% font server
                    erlguten_font_server, erlguten_afm, 
                  %% line calculations
                    erlguten_lines2pdf, erlguten_line_break, erlguten_geometry ]},
  {registered,   [erlguten_sup, erlguten_font_server]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlguten_app, []}}]}.