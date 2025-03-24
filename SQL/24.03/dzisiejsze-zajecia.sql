SHOW DATABASES;
USE sakila;
SHOW tables;

SELECT * FROM film;

SELECT title, release_year, length FROM film;

SELECT CONCAT(title, ' (', release_year,  ')') AS Film FROM film;

SELECT * FROM film WHERE length >= 120;				-- które filmy trwają co najmniej 120 min

SELECT * FROM film WHERE release_year <> 2006;		-- które filmy nie zostały wypuszczone w 2006 roku

SELECT * FROM rental WHERE return_date IS NULL;		-- które filmy pożyczone nie zostały zwrócone

SELECT * FROM film WHERE rating = 'G' AND rental_rate <= 2.99;

SELECT * FROM actor WHERE last_name = 'Harris' OR first_name = 'Julia';

SELECT * FROM film WHERE rating IN ('G', 'PG');

SELECT * FROM film WHERE rental_rate BETWEEN 2.99 AND 4.99;

SELECT * FROM actor WHERE last_name LIKE 'J%';

SELECT * FROM film WHERE description LIKE '%student%';

SELECT * FROM film ORDER BY length DESC, title;

SELECT CONCAT(first_name, ' ', last_name) FROM actor ORDER BY last_name;

SELECT * FROM payment ORDER BY amount LIMIT 10;

SELECT length FROM film WHERE length > 150 ORDER BY length DESC LIMIT 10;

-- zadania
-- zadanie 1 -- odp: 200
SELECT * FROM actor;
SELECT COUNT(actor_id) FROM actor;

-- zadanie 2
SELECT * FROM language;

-- zadanie 3 -- odp: 109
SELECT COUNT(country_id) FROM country;

-- zadanie 4
-- List the titles and descriptions of all movies that cost no more than $1 to rent and are suitable for children.
SELECT * FROM film;
SELECT title, description FROM film WHERE rating IN('PG-13', 'G', 'PG') AND rental_rate <= 1.00;

-- zadanie 5
-- Display in a single column labeled ’Actor’ the first and last names of actors whose first or last name starts with the letter ’J’.
SELECT CONCAT(first_name, ' ', last_name) AS 'Actor' FROM actor WHERE first_name LIKE 'J%' OR last_name LIKE 'J%';

-- zadanie 6 -- odp: 2311
-- How many rentals were recorded in June?
SELECT * FROM rental;
SELECT COUNT(rental_id) FROM rental WHERE rental_date LIKE '____-06%';

-- zadanie 7
-- List all payment amounts handled by the employee with ID 1.
SELECT * FROM payment WHERE staff_id = 1;

-- zadanie 8
-- List all actor first names in alphabetical order.
SELECT first_name FROM actor ORDER BY first_name ASC;

-- zadanie 9
-- List all movie titles in the PG category sorted from shortest to longest, with a secondary alphabetical sorting by title.
SELECT * FROM film;
SELECT title FROM film WHERE rating LIKE 'PG' ORDER BY LENGTH(title) ASC, title ASC;