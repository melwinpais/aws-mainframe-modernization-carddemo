-- CardDemo Database Initialization Script
-- PostgreSQL 16.9

-- Create database (run as postgres superuser)
-- CREATE DATABASE carddemo;

-- Create user and grant privileges
CREATE USER carddemo_user WITH PASSWORD 'carddemo_pass';
GRANT ALL PRIVILEGES ON DATABASE carddemo TO carddemo_user;

-- Connect to carddemo database before running the following
\c carddemo

-- Grant schema privileges
GRANT ALL ON SCHEMA public TO carddemo_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO carddemo_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO carddemo_user;

-- Set default privileges for future objects
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO carddemo_user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO carddemo_user;
