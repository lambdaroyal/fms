#Installation und Update von FMS

##Synopsis

Dieses Dokument beschreibt die Schritte, um mit Hilfe eines .bat Files die aktuelle Version von FMS von Github zu laden und zu einer startbaren Software zu bauen.

##Vorbedingungen (einmalig)

###Git Client installieren

Git ist ein Versionsverwaltungsprogramm. Wir nutzen Git zur Verwaltung der lokalen Quellcodes und zum Abgleich mit GitHub.

Das Installationsprogramm für Windows kann unter http://git-scm.com/download/win bezogen werden und muss anschliessend ausgeführt werden.

Während der Installation wird nach der Option für "Adjusting your Path environment" gefragt. Hier muss die Option "Run Git from the Windows Command Prompt" ausgewählt werden.

Alle anderen Voreinstellungen können übernommen werden.

###Installation Leiningen

Leiningen ist für das Übersetzen der Quellcodes in eine startbare Software zuständig.

Das Installationsprogramm für Windows kann unter http://leiningen-win-installer.djpowell.net/ bezogen werden und muss anschliessend ausgeführt werden.

Alle Voreinstellungen können übernommen werden. 

Die sich zum Schluss öffende Console kann geschlossen werden.

###Download the build scripts

Es steht unter https://github.com/lambdaroyal/fms/blob/master/make-fms.bat das build script bereit. Der Inhalt des Files muss in ein .bat File in einem beliebigen Verzeichnis kopiert werden (z.B. c:/temp).

##Verwendung des build scripts
Nach dem Download kann das .bat File doppelgeklickt werden. 

Die Ausgabe sieht wie folgt aus (Ausschnitt)

<pre>
E:\development\clojure>make-fms.bat
Lambdaroyal FMS Installer - this script downloads and builds the software into f
ms. Using
Creating directory fms
Creating directory fms-temp
Downloading the sources
Cloning into 'fms-temp'...
Username for 'https://github.com': gixxi
Password for 'https://gixxi@github.com':
remote: Counting objects: 140, done.
remote: Compressing objects: 100% (72/72), done.
remote: Total 140 (delta 49), reused 106 (delta 32)
Receiving objects: 100% (140/140), 94.54 KiB | 0 bytes/s, done.
Resolving deltas: 100% (49/49), done.
Checking connectivity... done.
Building software
Compiling lambdaroyal.logistics.c2server.core
</pre>


