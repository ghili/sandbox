
CREATE SCHEMA ct;

SET search_path TO ct, public;

CREATE TABLE support (
  id                serial PRIMARY KEY,
  nom               varchar (256) NOT NULL UNIQUE,
  chck              bigint UNIQUE,
  dtcreation        timestamp UNIQUE
);

CREATE TABLE dossier (
  id                serial PRIMARY KEY,
  nom               varchar(256) NOT NULL,
  chemin            text,
  iddossierparent   integer REFERENCES dossier(id) ON DELETE CASCADE,
  idsupport         integer REFERENCES support(id) ON DELETE RESTRICT
);

-- restrict pour empêcher la suppression si un fichier existe dans le dossier
CREATE TABLE fichier (
  id                serial PRIMARY KEY,
  nom               varchar(256) NOT NULL,
  extension         varchar(128),
  taille            integer,
  dtfichier         timestamp,
  iddossier         integer REFERENCES dossier(id) ON DELETE RESTRICT
);

