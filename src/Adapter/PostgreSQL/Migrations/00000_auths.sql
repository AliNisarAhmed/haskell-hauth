CREATE extension citext;
CREATE extension pgcrypto;

CREATE TABLE auths (
	id bigserial PRIMARY KEY NOT NULL,
	pass text NOT NULL,
	email citext NOT NULL UNIQUE,
	email_verification_code TEXT NOT NULL,
	is_email_verified boolean NOT NULL
)