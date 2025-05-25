SHOW DATABASES;
USE sakila;
SHOW tables;

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