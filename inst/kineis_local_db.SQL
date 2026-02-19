-- KINEIS realtime db (run once)



PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA busy_timeout = 5000;

BEGIN;


CREATE TABLE  checkpoint(
  id INTEGER PRIMARY KEY,
  checkpoint TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE INDEX  idx_checkpoint_updated_at ON checkpoint(updated_at);


CREATE TABLE sensors(
  deviceUid INTEGER NOT NULL,
  msgDatetime TEXT NOT NULL,     -- ISO-8601 string
  sensor INTEGER NOT NULL,
  value TEXT,
  PRIMARY KEY(deviceUid, msgDatetime, sensor) ON CONFLICT IGNORE
) ;

CREATE INDEX sensors_by_time ON sensors(msgDatetime);
CREATE INDEX sensors_by_dev_time ON sensors(deviceUid, msgDatetime);


-- Doppler fixes  
CREATE TABLE doppler(
  deviceUid INTEGER NOT NULL,
  deviceRef CHAR , 
  msgDatetime TEXT NOT NULL,        -- ISO-8601 string
  acqDatetime TEXT,
  dopplerDatetime TEXT,
  dopplerLocLon REAL,
  dopplerLocLat REAL,
  dopplerLocAlt REAL,
  dopplerLocErrorRadius REAL,
  dopplerLocClass TEXT,
  PRIMARY KEY(deviceUid, msgDatetime) ON CONFLICT IGNORE
) ;

CREATE INDEX doppler_by_time ON doppler(msgDatetime);
CREATE INDEX doppler_by_dev_time ON doppler(deviceUid, msgDatetime);


COMMIT;
