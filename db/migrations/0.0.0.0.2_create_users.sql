CREATE TABLE users (
    id VARCHAR PRIMARY KEY,
    name VARCHAR NOT NULL,
    age INTEGER NOT NULL,
    email VARCHAR NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
);

INSERT INTO users (id, name, age, email, created_at)
    VALUES ('d91ae396-42e7-4483-a3ef-e729c486980f', 'First User', 20, 'first@mail.com', now());