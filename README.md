# LambdaRoyal Logistik Warenfluss Analyse=

Diese Serveranwendung reagiert auf die Veränderung
* des Clojure Quellcodes sankey.clj
* des per Programmparameters -i angegebenen Excel (.xls) Files

und stellt die Warenfluss Analyse entsprechend des Excelfiles per SVG unter
http://localhost:8080 bereit.

## Strukture des Servers

server.clj startet einen HTTP Server, liefert statisches Reload Script aus setzt einen watch auf die zentrale datenstruktur core/current-page
core.clj enthält die von adapter veränderte und server verarbeitete datenstruktur 

##Start des Servers##

lein repl --port 8080 --path samples

--port optional
--path relativer oder absoluter pfad. entweder eine existierende datei oder ein existierendes Verzeichnis
