SHOW DATABASES;
USE sakila;
SHOW tables;

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