# Datenkonzept Meteo

## Datenerhebung

Die meterologischen Messwerte basieren auf fernübertragenenen Sensordaten die im [FUK ThinksBoard](https://thingsboard.forstliche-umweltkontrolle.de/home) (TB) auflaufen. Das TB dient als Sensormonitor in dem der Datenfluss überwacht wird. Hier werden keine Datenmanipulationen vorgenommen und außschließlich Rohdaten gesichert.

Der Datenzugriff auf das TB erfolgt über eine REST API und dazugehörigen [Zugriffsfunktionen](https://thingsboard.gruenecho.de/swagger-ui/index.html).

***!!! Swagger noch auf gruenecho.de***

Für einen Skriptbasierten Zugriff wurde das R-Projekt [tb_access](https://github.com/Landeskompetenzzentrum-Forst-Eberswalde/FUK-TB-api-functions/tree/main/R/tb_access) erstellt. Einzelne Skripte erlaube hier den Zugriff auf gefiilterte Datensätze und halten diese für einen späteren Zugriff als R-workspace in einem lokalen Output-Ordner vor.

***??? Ist das sinnvoll? Da zur Analyse meist vollständigt Datenreihen benötigt dauert der API-Zugriff etwas lange und mit Hilfe der .rda files kann schnell an alter Stelle weitergearbeitet werden.***

## Datenverarbeitung

Das Ziel ist es geschlossene Zeitreihen an Tageswerten herzustellen um Jahressumme berechnen zu können. Außerdem sollen Messfehler identifiziert und entfernt werden.

In einem erstenm Schritt werden stündliche Werte im Falle kleinerer Zeitschritte gebildet. Schwellenwerte für Minimum und Maximum entsprechend physikalisch möglicher Ergebnisse werden diese bereinigt und zu Tageswerten aggregiert.

***!!! Schwellenwerte müssen erneut kontrolliert werden***

Zur Füllung der Zeitreihen sollen dann [regionalisierte Tagesgrids des DWD](https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/) an die Messreihen der Freilandstationen angepasst werden.

Um eine Rückkopplung zur Sensorüberwachung herzustellen sollen die Datenergänzungsroutinen wöchentlich durchgeführt und im Falle von Messauffälligkeiten einen Sensorbericht auf GitHub abgelegt werden.

***??? Ist das sinnvoll? Crone Job auf den Source-Befehl der R-Skripte. Wenn Auffälligkeiten aufteauchen wird eine Tabelle erstellt und im Rahmen von Git Pull sieht Paul ob einzelne Sensoren komische Werte leifern.***

Um das Informationsdashboard zu Aktualisieren sollten neben den Aktualiserungen im Rahmen der Datenlieferungen die Tageswerte für die Datenlieferung vorbereitet werden und als ungeprüft dargestellt werde. Eine Routine ins upload Schema könnte tagesaktuell die Daten überführen. Diese Rohdaten könnten dann unter Level II dargestellt werden, wobei eine wöchentliche Korrektur im Rahmen der Datenergänzungsroutinen erfolgen würde.

***??? Ist das sinnvoll und so umsetzbar?***

Sind die Daten der Freilandstationen bereinig und lückengefüllt dienen diese zur Lückenfüllung der entsprechenden Bestandesflächen.

***!!! Muss noch erfolgen***

Anschließend werden diese als Eingangsdaten für die Wasserhaushaltsmodellierung genutuzt. Diese Ergebnisse dienen wiederum der Evaluierung der Bodenfeuchte- und Bodenspannungsmesswerte in Vorbereitung der Datenlieferung. Neben den Messwerten der Bodensonden sollen auch die moellierten Werte an ICP Forests geliefert werden.

***!!! Muss noch erfolgen***

## Datenhaltung

Grundsätzlich bleiben den Rohdaten im TB unangetastet. Sämtliche folgenden Arbeitsschritte sind dann auf Grundlage der versionierten R-Skripte reproduzierbar.

***??? Wie gehen wir mit Änderungen bei den regionalisierten Daten beim DWD um?***

Die bereinigten, und soweit automatisiert möglich, lückengefüllten Tagesdaten werden in einer PostgreSQL Datenbank auf einem für die FUK angemieteten Serven abgelegt.

In dieser Datenbank befinden sich ebenfalls die für die Datenkontrolle notwendigen bereits an ICP Forests gelieferten Daten im Schema [icp_download](http://116.203.31.116:8000/project/default/editor?schema=icp_download). Je nach dem ob die verarbeiteten Daten für die jährliche Datenlieferung oder für eine Neuübermittlung der Erhebungen vorgesehen sind, werden diese in der Tabelle *mm_mem* im Schema [icp_upload](http://116.203.31.116:8000/project/default/editor?schema=icp_upload) oder [icp_resubmission](http://116.203.31.116:8000/project/default/editor?schema=icp_resubmission).

***??? Ist das sinnvoll?***

Zwischenergebnisse werden grundsätzlich nur als RData lokal abgelegt. Sollten Ergebnisse jedoch für Folgeuntersuchungen von Bedeutung sein, können diese jeoch im Schema [fuk](http://116.203.31.116:8000/project/default/editor?schema=fuk) gesichert werden.

***??? Ist das sinnvoll?***

## Datenveröffentlichung

Alle an ICP Forests gelieferten Daten sind über die REST API und auf Anfrage erhältlichen JSON Web Token (JWT) abrufbar.

***Wie sieht das praktisch aus? Wer verschickt den Token. Werden Nutzer registriert? Sollen einige Daten gänzlich frei verfügbar sein?Geben wir die Koordinaten raus?***

Die in Grafiken veröffentlichten Ergebnisse können zudem als CSV exportiert werden.

***Wollen wir das für alle Daten anbieten?***

Neben den geprüften und ggf. lückengefüllten Daten sollen außerdem Ergebnisse der Datenverarbeitung veröffentlicht weren. Hierzu werden die Ergebnischarts (z.B. die Anzahl an Fehltagen) im Rahmen der Routinen auf einen Ordner des Repos gespielt und hier abgerufen.

***??? Wollen wir das? Ist das so praktikabel?***