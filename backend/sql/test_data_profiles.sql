-- This will create some developer profiles with test data.

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Steven Universe', 'steven_gh', 'Guardian Apprentice', NULL, false, NULL, 'Beach City', NULL, 'steven.universe@gems.org', 'http://stevenuniverse.example.org', 'steven_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'steven_gh', NULL, '623cf297a88aa998e58a684eb9afe1d9');

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Rose Quartz', 'rose_gh', 'Guardian Leader (ex)', NULL, false, NULL, 'Beach City', NULL, 'rose.quartz@gems.org', 'http://rosequartz.example.org', 'rose_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'rose_gh', 'https://avatars.githubusercontent.com/u/9272766?v=3', NULL);

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Amethyst', 'amethyst_gh', 'Guardian', NULL, false, NULL, 'Beach City', NULL, 'amethyst@gems.org', 'http://amethyst.example.org', 'amethyst_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'amethyst_gh', 'https://avatars.githubusercontent.com/u/17814736?v=3', NULL);

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Pearl', 'pearl_gh', 'Guardian', NULL, false, NULL, 'Beach City', NULL, 'pearl@gems.org', 'http://pearl.example.org', 'pearl_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'pearl_gh', 'https://avatars.githubusercontent.com/u/18231570?v=3', NULL);

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Garnet', 'garnet_gh', 'Guardian Leader', NULL, false, NULL, 'Beach City', NULL, 'garnet@gems.org', 'http://garnet.example.org', 'garnet_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'garnet_gh', 'https://avatars.githubusercontent.com/u/59016?v=3', NULL);

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Connie Maheswaran', 'connie_gh', NULL, NULL, false, NULL, 'Beach City', NULL, 'connie@gems.org', 'http://connie.example.org', 'connie_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'connie_gh', NULL, NULL);

INSERT INTO profiles (id, name, url_fragment, job, bio, available, zip_code, city, country, email, homepage, github_username, twitter_handle, created_at, signup_method, github_oauth_login, github_avatar_url, gravatar_id) VALUES (uuid_generate_v4(), 'Fred Feuerstein', 'fred_gh', 'Steinbruch', NULL, false, NULL, 'Steintal', NULL, 'fred.feuerstein@steintal.de', 'http://feuerstein.example.org', 'fred_twitter', NULL, '2017-01-08 23:40:29.900634+01', 'GitHub', 'fred_gh', 'https://avatars.githubusercontent.com/u/17061750?v=3', '623cf297a88aa998e58a684eb9afe1d9');

