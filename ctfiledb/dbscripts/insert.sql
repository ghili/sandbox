
SET search_path TO ct;

BEGIN;

INSERT INTO dossier (nom) VALUES ('racine');
INSERT INTO dossier (nom, iddossierparent) VALUES ('tvshow', 0);

INSERT INTO support (nom,iddossierracine) VALUES ('dvd00', 1);

INSERT INTO fichier (nom, iddossier) VALUES ('xfiles.avi', 1);

COMMIT;
