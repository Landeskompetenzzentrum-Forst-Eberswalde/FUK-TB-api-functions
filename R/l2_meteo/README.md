# Datenkonzept Meteo

Die meterologischen Messwerte basieren auf fernübertragenenen Sensordaten die im [FUK ThinksBoard](https://thingsboard.forstliche-umweltkontrolle.de/home) (TB) auflaufen. Das TB dient als Sensormonitor in dem der Datenfluss überwacht wird. Hier werden keine Datenmanipulationen vorgenommen und außschließlich Rohdaten gesichert.

Der Datenzugriff auf das TB erfolgt über eine REST API und dazugehörigen [Zugriffsfunktionen](https://thingsboard.gruenecho.de/swagger-ui/index.html).

***!!! Swagger noch auf gruenecho.de***

Für einen Skriptbasierten Zugriff wurde das R-Projekt [tb_access](https://github.com/Landeskompetenzzentrum-Forst-Eberswalde/FUK-TB-api-functions/tree/main/R/tb_access) erstellt. Einzelne Skripte erlaube hier den Zugriff auf gefiilterte Datensätze und halten diese für einen späteren Zugriff als R-workspace in einem lokalen Output-Ordner vor.

***??? Ist das sinnvoll? Da zur Analyse meist vollständigt Datenreihen benötigt dauert der API-Zugriff etwas lange und mit Hilfe der .rda files kann schnell an alter Stelle weitergearbeitet werden.***

Das Ziel ist es geschlossene Zeitreihen herzustellen um Jahressumme berechnen zu können. Außerdem sollen Messfehler identifiziert und entfernt werden. Zur Füllung der Zeitreihen sollen dann [regionalisierte Tagesgrids des DWD](https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/) an die Messreihen angepasst werden.

Um eine Rückkopplung zur Sensorüberwachung herzustellen sollen die Datenergänzungsroutinen wöchentlich durchgeführt und im Falle von Messauffälligkeiten einen Sensorbericht auf GitHub abgelegt werden.

***??? Ist das sinnvoll? Crone Job auf den Source-Befehl der R-Skripte. Wenn Auffälligkeiten aufteauchen wird eine Tabelle erstellt und im Rahmen von Git Pull sieht Paul ob einzelne Sensoren komische Werte leifern.***

Um das Informationsdashboard zu Aktualisieren sollten neben den Aktualiserungen im Rahmen der Datenlieferungen die Tageswerte für die Datenlieferung vorbereitet werden und als ungeprüft dargestellt werde. Eine Routine ins upload Schema könnte tagesaktuell die Daten überführen. Diese Rohdaten könnten dann unter Level II dargestellt werden, wobei eine wöchentliche Korrektur im Rahmen der Datenergänzungsroutinen erfolgen würde.

***??? Ist das sinnvoll und so umsetzbar?***

