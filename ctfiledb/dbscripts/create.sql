
CREATE SCHEMA ct;

SET search_path TO ct, public;

CREATE TABLE support (
  id_support        serial PRIMARY KEY,
  nom               varchar (256) NOT NULL UNIQUE,
  chck              bigint UNIQUE,
  date_creation     timestamp UNIQUE
);

CREATE TABLE dossier (
  id_dossier        serial PRIMARY KEY,
  nom               varchar(256) NOT NULL,
  chemin            text,
  id_dossier_parent bigint REFERENCES dossier(id_dossier) ON DELETE CASCADE,
  id_support        bigint REFERENCES support(id_support) ON DELETE RESTRICT
);

-- restrict pour empêcher la suppression si un fichier existe dans le dossier
CREATE TABLE fichier (
  id_fichier        serial PRIMARY KEY,
  nom               varchar(256) NOT NULL,
  extension         varchar(128),
  taille            bigint,
  date_fichier		timestamp,
  id_dossier        bigint REFERENCES dossier(id_dossier) ON DELETE RESTRICT
);

