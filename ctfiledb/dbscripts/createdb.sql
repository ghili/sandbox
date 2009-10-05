--CREATE TABLESPACE pfdbspace LOCATION 'C:/Minh/perso/work/db/pfdbts';
CREATE TABLESPACE pfdbspace LOCATION '/home/pg/data/pfdbts';

CREATE DATABASE pfdb TABLESPACE = pfdbspace;

