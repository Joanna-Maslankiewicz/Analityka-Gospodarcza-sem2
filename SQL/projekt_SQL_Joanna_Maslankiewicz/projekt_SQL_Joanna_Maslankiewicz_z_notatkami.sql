-- ZAJECIA 1

SHOW DATABASES;
-- DROP DATABASE 'university' - delete database
CREATE DATABASE university;
USE university;

-- create the "courses" table
CREATE TABLE courses (
course_id INT PRIMARY KEY AUTO_INCREMENT,
title VARCHAR(100) NOT NULL
);

-- create the "students" table with a foreign key to "courses"
CREATE TABLE students (
id INT PRIMARY KEY AUTO_INCREMENT,
name VARCHAR(50) NOT NULL,
course_id INT,
FOREIGN KEY (course_id) REFERENCES courses(course_id)
);

-- insert sample data into courses
INSERT INTO courses (title) VALUES
	('Introduction to Databases'),
    ('Web Development'),
    ('Mathematics 101');

-- insert sample data into students
INSERT INTO students (name, course_id) VALUES
	('Alice Smith', 1),		-- Enrolled in "Introduction to Databases"
    ('Bob Johnson', 2),		-- Enrolled in "Web Development"
    ('Charlie Brown', 1),	-- Enrolled in "Introduction to Databases"
    ('Diana Lee', NULL);	-- not enrolled in any course yet

-- QUERIES:

-- Query to see all records from both tables
SELECT * FROM courses;
SELECT * FROM students;

-- Query to demonstrate the relationship (join students and courses)
SELECT
	s.name AS student_name,		-- create alias; s instead of name (when used we write "s", but in the table we see this column as "student_name")
    c.title AS course_title		-- create alias; c instead of title (when used we write "c", but in the table we see this column as "course_title")
FROM
	students s
LEFT JOIN courses c	-- relacja jeden do wielu; nie wypisuje kursów na które nikt się nie zapisał; FULL OUTER JOIN - jak chcemy zobaczyć wszystkie kursy i studentów, nawet jeśli któreś nie są przypisane
ON s.course_id = c.course_id;





-- ZAJECIA 2

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

-- zadanie 10
-- List information about actors.
SELECT * FROM actor;

-- zadanie 11
-- List titles and descriptions of all movies that cost no more than 1$ to rent and are rated 'G'.
SELECT * FROM film;
SELECT title, description FROM film WHERE rating = 'G' AND rental_rate <= 1.00;

-- zadanie 12
-- List information about countries from which rental store customers may come.
SELECT * FROM country;

-- zadanie 13
-- List different last names of actors.
SELECT DISTINCT last_name FROM actor;

-- zadanie 14
-- Display first name, last name, and email of all customers, merging the first and last name into a single column labeled "person".
SELECT CONCAT(first_name, ' ', last_name) AS person, email FROM customer;

-- zadanie 15
-- List rentals recorded in June for customers with IDs 111, 222, 333, 444 and 555.
SELECT * FROM rental WHERE MONTH(rental_date) = 6 AND customer_id IN(111, 222, 333, 444, 555);

-- zadanie 16
-- List distinct actor names that contain the letter 'A' but do not start or end with 'A'.
SELECT DISTINCT first_name FROM actor WHERE first_name LIKE '%A%' AND first_name NOT LIKE 'A%' AND first_name NOT LIKE '%A';

-- zadanie 17
-- List movie titles that contain the word 'love' and either have a replacement cost between 20 and 30 dollars or are at least 90 minutes long.
SELECT * FROM film;
SELECT title FROM film WHERE title LIKE '%love%' AND (replacement_cost BETWEEN 20 AND 30 OR length >= 90);

-- zadanie 18
-- List the first 100 payment records for customers with IDs between 100 and 199 who rented movies in August, sorted by payment amount and then by payment date.
SELECT * FROM payment;
SELECT * FROM payment WHERE customer_id BETWEEN 100 AND 199 AND rental_id IN (SELECT rental_id FROM rental WHERE MONTH(rental_date) = 8)
ORDER BY amount, payment_date;

-- zadanie 19
-- List all active customers of store 1 along with their email addresses in a single column labeled "customer" with the format "First Last (email)", sorted alphabetically by last name and first name.
SELECT * FROM customer;
SELECT CONCAT(first_name, ' ', last_name, ' (', email, ')') FROM customer ORDER BY last_name, first_name;





-- ZAJECIA 3

SELECT CONCAT_WS(';', 'This ', ' is ', ' fun !') AS FUNKCJA;

SELECT SUBSTRING('s333333@student.uek.krakow.pl', 2, 6) AS Substring;

SELECT * FROM film;
SELECT COUNT(*) FROM film ORDER BY rental_duration, title;

SELECT COUNT(*) AS p2 FROM rental WHERE staff_id = 2;

SELECT COUNT(DISTINCT inventory_id) AS rentals FROM rental;

SELECT COUNT(inventory_id) / COUNT(DISTINCT customer_id) AS average FROM rental;

SELECT SUM(amount) AS total FROM payment;
SELECT AVG(amount) AS average FROM payment;
SELECT MIN(amount) AS min FROM payment;
SELECT MAX(amount) AS max FROM payment;

SELECT MIN(amount) AS min, AVG(amount) AS avg, MAX(amount) AS max FROM payment;

SELECT COUNT(*) AS n, MIN(amount) AS min, AVG(amount) AS avg , MAX(amount) AS max FROM payment WHERE customer_id = 148;

SELECT staff_id, COUNT(*) AS n FROM payment
GROUP BY staff_id;

SELECT customer_id, COUNT(*) AS n FROM payment
GROUP BY customer_id;

SELECT customer_id, SUM(amount) AS total FROM
payment GROUP BY customer_id;

SELECT customer_id, SUM(amount) AS total FROM payment
GROUP BY customer_id ORDER BY total DESC;

SELECT replacement_cost, AVG(rental_rate) AS avg FROM film 
GROUP BY replacement_cost HAVING MIN(rental_rate) > 0;

-- ZADANIE 1
-- 1. Calculate the average length of movie titles based on all films in the database. Display the result in a column labeled average.
-- 2. Determine the number of films available for rental in each store (count each title only once). Display results in two columns: store (store ID) and titles (number of films).
-- 3. Identify the top 10 most popular actors (those appearing in the most films), sorted descending by film count. Use columns: actor (actor ID) and n (number of films).
-- 4. List the age rating category, along with the minimum, average, and maximum replacement cost for each, sorted by average. Use columns: category, min, average, and max.
-- 5. List the IDs of 10 countries represented by the fewest cities (but more than 10) in the database. Display two columns: country (country ID) and n (number of cities). Is Poland among them?

SELECT * FROM country;

-- zadanie 1.1
SELECT AVG(CHAR_LENGTH(title)) AS average FROM film;

-- zadanie 1.2
SELECT store_id AS store, COUNT(DISTINCT(film_id)) AS titles FROM inventory GROUP BY store;

-- zadanie 1.3
SELECT actor_id AS actor, COUNT(film_id) as n FROM film_actor
GROUP BY actor ORDER BY n DESC LIMIT 10;

-- zadanie 1.4
SELECT rating AS category, MIN(replacement_cost) AS min, AVG(replacement_cost) AS average, MAX(replacement_cost) AS max
FROM film GROUP BY category ORDER BY average;

-- zadanie 1.5
SELECT country_id AS country, COUNT(city) as n FROM city
GROUP BY country HAVING n > 10 ORDER BY n LIMIT 10;

SELECT country_id FROM country WHERE country LIKE 'Poland'; -- Poland isn't among them

-- Generate a pseudorandom number from {0, 1, . . . , 10}:
SELECT FLOOR(RAND() * 11) AS random;

-- variance and standard deviation
SELECT VAR_POP(amount) AS sigma FROM payment;
SELECT VAR_SAMP(amount) AS sigma FROM payment;
SELECT STDDEV_POP(amount) AS sigma FROM payment;
SELECT STDDEV_SAMP(amount) AS sigma FROM payment;
-- additionally: 
-- VARIANCE: Equivalent to VAR_POP. 
-- STD, STDDEV: Equivalent to STDDEV_POP.

-- In MySQL, compute mode/median manually
SELECT amount AS mode FROM payment GROUP BY amount ORDER BY COUNT(amount) DESC LIMIT 1;
SELECT amount AS median FROM payment ORDER BY amount LIMIT 8024, 1;

-- ZADANIE 2
-- 1. Calculate the average and standard deviation of actors’ last name lengths, rounded to 4 decimal places, labeled average and sigma.
-- 2. What is the range of the number of film copies available in store ID 1? Compute the minimum and maximum separately in columns n min and n max (two queries).
-- 3. Calculate the average and standard deviation of film lengths for each age rating category, rounded to 2 decimal places, in columns category, average, and sigma.
-- 4. Determine the mode of rental days for films with a rental rate of 4.99.
-- 5. Calculate the average and standard deviation of replacement costs per age rating category, for films lasting 90–150 minutes, in columns category, average, and sigma.

SELECT * FROM film;

-- zadanie 2.1
SELECT ROUND(AVG(CHAR_LENGTH(last_name)), 4) AS average, ROUND(STDDEV_POP(CHAR_LENGTH(last_name)), 4) AS sigma FROM actor;

-- zadanie 2.2
SELECT store_id, COUNT(film_id) AS n_min FROM inventory WHERE store_id = 1 GROUP BY film_id ORDER BY n_min LIMIT 1;
SELECT store_id, COUNT(film_id) AS n_max FROM inventory WHERE store_id = 1 GROUP BY film_id ORDER BY n_max DESC LIMIT 1;

-- zadanie 2.3
SELECT rating AS category, ROUND(AVG(length), 2) AS average, ROUND(STDDEV_SAMP(length), 2) AS sigma FROM film GROUP BY category;

-- zadanie 2.4
SELECT rental_duration AS mode FROM film WHERE rental_rate = 4.99 GROUP BY mode ORDER BY COUNT(mode) DESC LIMIT 1;

-- zadanie 2.5
SELECT rating AS category, AVG(replacement_cost) AS average, STDDEV_SAMP(replacement_cost) AS sigma FROM film WHERE length BETWEEN 90 AND 150 GROUP BY category;

/* DATY
Valid Ranges:
- DATE: ’1000-01-01’ to ’9999-12-31’.
- DATETIME: ’1000-01-01 00:00:00.000000’ to ’9999-12-31 23:59:59.999999’.
- TIMESTAMP (UTC): ’1970-01-01 00:00:01.000000’ to ’2038-01-19 03:14:07.999999’.
- TIME: ’-838:59:59.000000’ to ’838:59:59.000000’.
- YEAR: ’1901’ to ’2155’.
- Use full formats, e.g., ’13:13’ becomes ’13:13:00’; ’1313’ becomes ’00:13:13’.

Selected Functions:
- CURDATE(): Current date.
- CURTIME(): Current time.
- NOW(): Current date and time.
- DATE(expr): Extracts the date.
- TIME(expr): Extracts the time.
- DAY(date): Day of the month.
- DAYNAME(date): Name of the day.
- DAYOFWEEK(date): Day of the week (1 = Sunday).
- DAYOFYEAR(date): Day of the year.
- WEEKOFYEAR(date): Week of the year.
- MONTH(date): Month number.
- MONTHNAME(date): Month name.
- QUARTER(date): Quarter.
- YEAR(date): Year.
- HOUR(time): Hour.
- MINUTE(time): Minute.
- SECOND(time): Second.

More Functions:
- MAKEDATE(year, day): Creates a date from day of the year.
- DATEDIFF(d1, d2): Days between d1 and d2.
- TIMEDIFF(d1, d2): Time difference in time format.
- TIMESTAMPDIFF(unit, d1, d2): Time difference in specified unit.
- ADDDATE(date, days): Shifts date forward.
- ADDTIME(time, interval): Shifts time forward.
- SUBDATE(date, days): Shifts date backward.
- SUBTIME(time, interval): Shifts time backward. */

SELECT DAYOFYEAR(NOW()) AS day, WEEKOFYEAR(NOW()) AS week, QUARTER(NOW()) AS quarter;
SELECT ADDDATE(NOW(), 14) AS date;

-- Polish day/month names:
SET lc_time_names = 'pl_PL';
SELECT DAYNAME(NOW()) AS day;

-- DATE_FORMAT to customize:
SELECT DATE_FORMAT(NOW(), '%W, %e %M %Y') AS today;
SELECT DATE_FORMAT(NOW(), '%d.%m.%y, %H:%i:%s:%f') AS now;

/* Format specifiers:
- %a: Abbreviated weekday.
- %b: Abbreviated month.
- %c: Month (1-12).
- %d: Day (00-31).
- %e: Day (0-31).
- %f: Microseconds.
- %H: Hour (00-23).
- %i: Minutes.
- %M: Month name.
- %m: Month (01-12).
- %s: Seconds.
- %W: Weekday name.
- %Y: Year (4 digits).
- %y: Year (2 digits). */

-- ZADANIE 3
-- 1. How many days did the shortest, average, and longest film rentals last? Display in columns min, average, and max.
-- 2. Which day of the week had the most unreturned film rentals? Display the day name in day and count in n.
-- 3. List all payments made on Wednesdays handled by staff ID 2, sorted ascending by payment date.
-- 4. List total revenue by day of the week, in columns day (Sunday to Saturday) and revenue.
-- 5. List revenue by staff ID and month, in columns id (staff ID), month (month name), and revenue, sorted by ID and month order.

SELECT * FROM rental;

-- zadanie 3.1
SELECT MIN(DATE(return_date)-DATE(rental_date)) AS min, AVG(DATE(return_date)-DATE(rental_date)) AS average, MAX(DATE(return_date)-DATE(rental_date)) AS max FROM rental WHERE return_date IS NOT NULL;

-- zadanie 3.2
SELECT DAYNAME(DATE(rental_date)) AS day_name, COUNT(DAYNAME(DATE(rental_date))) AS n FROM rental WHERE return_date IS NULL GROUP BY day_name ORDER BY day_name DESC LIMIT 1;

-- zadanie 3.3
SELECT payment_id AS payments FROM payment WHERE staff_id = 2 AND DAYNAME(payment_date) LIKE 'środa' ORDER BY payment_date ASC;

-- zadanie 3.4
SELECT SUM(amount) as revenue, DAYNAME(payment_date) as day FROM payment GROUP BY DAYNAME(payment_date);

-- zadanie 3.5
SELECT staff_id AS id, MONTHNAME(DATE(payment_date)) AS month, SUM(amount) AS revenue FROM payment GROUP BY id, month ORDER BY id, month;





-- ZAJECIA 4

-- single-row subqueries
SELECT * FROM address WHERE address_id = (SELECT address_id FROM customer WHERE first_name = 'Theresa' AND last_name = 'Watson');

-- multi-row subqueries
SELECT name FROM language WHERE language_id IN (SELECT DISTINCT language_id FROM film);

SELECT country FROM country WHERE country_id IN (SELECT country_id FROM city WHERE city LIKE 'K%');

-- nested subqueries
SELECT CONCAT(first_name, ' ', last_name) AS customer FROM customer WHERE customer_id IN 
(SELECT customer_id FROM rental WHERE inventory_id = 
(SELECT inventory_id FROM film WHERE title = 'Iron Moon'));

-- complex subqueries
SELECT CONCAT(first_name, ' ', last_name) AS customer FROM customer WHERE customer_id IN
(SELECT customer_id FROM payment GROUP BY customer_id HAVING SUM(amount) > 0.75 * (SELECT SUM(amount) AS total FROM payment GROUP BY customer_id ORDER BY total DESC LIMIT 1));

-- subqueries with multiple columns
SELECT title, rental_duration, rental_rate FROM film WHERE (rental_duration, rental_rate) = (SELECT MAX(rental_duration), MIN(rental_rate) FROM film);

-- can reference tables from the outer query
SELECT rental_id, staff_id FROM rental R WHERE inventory_id IN (SELECT inventory_id FROM inventory WHERE R.staff_id = store_id);

-- zadanie 1.1
-- List the country where the city Bellevue is located.
SELECT country FROM country WHERE country_id = (SELECT country_id FROM city WHERE city = 'Bellevue');

-- zadanie 1.2
-- List all phone numbers of customers from Poland.
SELECT phone FROM address WHERE city_id IN (SELECT city_id FROM city WHERE country_id = (SELECT country_id FROM country WHERE country = 'Poland'));

-- zadanie 1.3
-- List actors (first and last name in a column labeled actor) who starred in films in the Children category.
SELECT DISTINCT CONCAT(first_name, ' ', last_name) AS actor FROM actor WHERE actor_id IN 
(SELECT actor_id FROM film_actor WHERE film_id IN
(SELECT film_id FROM film_category WHERE category_id =
(SELECT category_id FROM category WHERE name = 'Children')));

-- zadanie 1.4
-- List titles of films with the most copies in store 1. 
SELECT title FROM film WHERE film_id IN 
(SELECT film_id FROM inventory WHERE store_id = 1 GROUP BY film_id HAVING COUNT(*) =
(SELECT MAX(copy_count) FROM (SELECT COUNT(*) AS copy_count FROM inventory WHERE store_id = 1 GROUP BY film_id) as counts));

-- zadanie 1.5 
-- List customers (first and last name in a column labeled customer) whose total payments are at most half the average payment total.
#SELECT * FROM payment
SELECT CONCAT(first_name, ' ', last_name) AS customer FROM customer WHERE customer_id IN 
(SELECT customer_id FROM payment GROUP BY customer_id HAVING SUM(amount) <= 0.5 * (SELECT AVG(total) FROM (SELECT SUM(amount) AS total FROM payment GROUP BY customer_id) AS sub));

-- TYPES OF JOINS:
-- Cross join - Iloczyn kartezjański - returns all possible combinations of rows; no key relationship required
-- Inner join - returns records that have matching values in both tables
-- Outer join:
	-- Left join - returns all records from the left table, and the matched records from the right table
    -- Right join - returns all records from the right table, and the matched records from the left table
    -- Full join (not supported in MySQL) - returns all records when there is a match in either left or right table
-- joins are more efficient than subqueries

-- CROSS JOIN
SELECT * FROM category CROSS JOIN language;

-- INNER JOIN
SELECT * FROM rental INNER JOIN inventory ON rental.inventory_id = inventory.inventory_id;
# or without join:
SELECT * FROM rental, inventory WHERE rental.inventory_id = inventory.inventory_id;

-- multiple tables:
SELECT CONCAT(C.first_name, ' ', C.last_name) AS customer, F.title, R.rental_date, R.return_date FROM customer C INNER JOIN rental R USING(customer_id)
INNER JOIN inventory I USING(inventory_id) INNER JOIN film F USING(film_id);
# or without join:
SELECT CONCAT(C.first_name, ' ', C.last_name) AS customer, F.title, R.rental_date, R.return_date FROM customer C, rental R, inventory I, film F 
WHERE C.customer_id = R.customer_id AND I.film_id = F.film_id 
ORDER BY R.rental_date, R.return_date;

-- subquery:
SELECT CONCAT(first_name, ' ', last_name) AS customer FROM customer WHERE customer_id IN 
(SELECT customer_id FROM rental WHERE inventory_id =
(SELECT inventory_id FROM film WHERE title = 'Iron Moon'));
# or using join:
SELECT CONCAT(first_name, ' ', last_name) AS customer FROM customer C, rental R, inventory I, film F 
WHERE C.customer_id = R.customer_id AND R.inventory_id = I.inventory_id AND I.film_id AND F.title = 'Iron Moon';

-- OUTER JOIN
# left join:
SELECT * FROM film F LEFT OUTER JOIN film_actor FA ON F.film_id = FA.film_id;

# right join:
SELECT * FROM film RIGHT OUTER JOIN language USING(language_id);

# full join (using UNION):
(SELECT * FROM film LEFT JOIN language USING(language_id)) UNION (SELECT * FROM film RIGHT JOIN language USING(language_id));

-- UNION
	-- combines result sets of two or more SELECT statements, removing duplicates (UNION) or keeping all rows (UNION ALL)
	-- UNION - removes duplicates
    -- UNION ALL - includes all rows
    -- use case: aggregate silimiar data from multiple queries with compatible structures
    -- result: unique rows (UNION) or all rows including duplicates (UNION ALL)
    -- syntax:	SELECT column1, column2 FROM tableA UNION SELECT column1, column2 FROM tableB

SELECT actor_id, first_name, last_name FROM actor UNION SELECT customer_id, first_name, last_name FROM customer;

# with sorting:
(SELECT actor_id AS id, first_name AS first, last_name AS last FROM actor) UNION (SELECT customer_id, first_name, last_name FROM customer) ORDER BY last, first;

-- FULL OUTER JOIN
	-- returns all records from both tables, with NULLs for non-matching rows
    -- use case: combine rows from two tables, including all rows regardless of matches
    -- result: includes matching rows, non-matching rows from the first table (NULLs for second), and non-matching rows from the second table (NULLs for first)
    -- not natively supported in MySQL; use LEFT JOIN + RIGHT JOIN with UNION
	-- syntax: 	SELECT a.column1, b.column2 FROM tableA a FULL OUTER JOIN tableB ON a.common_column = b.common_column

-- joining multiple tables:
SELECT F.film_id, F.title, I.store_id, CONCAT(C.first_name, ' ', C.last_name) AS customer, R.rental_date, R.return_date FROM film F
LEFT JOIN inventory I USING(film_id) JOIN rental R USING(inventory_id) JOIN customer C USING(customer_id)
ORDER BY 2, 3, 5, 6;

-- zadanie 2.1
-- List all possible combinations of actor names (actor) and category names (category), sorted by actor's last name, first name, and category name.
SELECT CONCAT(A.first_name, ' ', A.last_name) AS actor, C.name AS category FROM actor A CROSS JOIN category C 
ORDER BY A.last_name, A.first_name, C.name;

-- zadanie 2.2
-- List all possible combinations of category names (type) and film ratings (category), sorted by category name and rating.
SELECT C.name AS type, F.rating AS category FROM category C CROSS JOIN film F
GROUP BY type, category ORDER BY type, category;

-- zadanie 2.3
-- List film titles (film), category names (type), and ratings (category), sorted by category and title.
SELECT F.title AS film, C.name AS type, F.rating AS category 
FROM film F INNER JOIN film_category CF USING(film_id) INNER JOIN category C USING(category_id) 
ORDER BY type, film;

-- zadanie 2.4
-- List all customers of store 1 served by staff member 2.
SELECT DISTINCT C.* FROM customer C INNER JOIN rental R USING(customer_id) WHERE C.store_id = 1 AND R.staff_id = 2;

-- zadanie 3.1
-- List film ID, title, and total copies across both stores (id, film, n). Films with no copies should have n = 0.
SELECT F.film_id AS id, F.title AS film, COUNT(I.film_id) AS n
FROM film F LEFT JOIN inventory I USING(film_id)
GROUP BY film_id;

-- zadanie 3.2
-- List all payment dates and amounts, along with corresponding rental ID, rental date, and return date (payment_date, amount, id, rental_date, return_date).
SELECT P.payment_date AS payment_date, P.amount AS amount, R.rental_id AS id, R.rental_date AS rental_date, R.return_date AS return_date
FROM payment P LEFT JOIN rental R USING(rental_id)
ORDER BY payment_date;

-- zadanie 3.3
-- List all pairs of IDs from film_actor and film_category (including duplicates) in columns film and actor_category, sorted by film ID and actor/category ID.
(SELECT FA.film_id AS film, FA.actor_id AS actor_category FROM film_actor FA)
UNION ALL
(SELECT FC.film_id, FC.category_id FROM film_category FC)
ORDER BY film, actor_category;

-- zadanie 3.4
-- Assign each country the total payments made by its customers, sorted in descending order.
SELECT C.country AS country, SUM(P.amount) AS total FROM payment P 
INNER JOIN customer USING(customer_id) INNER JOIN address USING(address_id) INNER JOIN city USING(city_id) INNER JOIN country C USING(country_id)
GROUP BY country ORDER BY total DESC;

-- zadanie 3.5
-- For each customer, list the number of rented films per category (customer, category, n), sorted by last name, first name, and number of rentals (descending).
SELECT CONCAT(C.first_name, ' ', C.last_name) AS customer, CA.name AS category, COUNT(R.rental_id) AS n
FROM customer C INNER JOIN rental R USING(customer_id) INNER JOIN inventory I USING(inventory_id) INNER JOIN film F USING(film_id)
INNER JOIN film_category FC USING(film_id) INNER JOIN category CA USING(category_id)
GROUP BY C.first_name, C.last_name, CA.name 
ORDER BY C.last_name, C.first_name, n DESC;