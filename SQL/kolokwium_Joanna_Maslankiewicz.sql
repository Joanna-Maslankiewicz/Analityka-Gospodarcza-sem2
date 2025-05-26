SHOW DATABASES;
USE sakila;
SHOW tables;

-- Zadanie 1
SELECT SUBSTRING(title, 1, 5) AS film, length FROM film WHERE LENGTH(title) > 10 ORDER BY length ASC;

-- Zadanie 2
SELECT C.first_name AS customer, IF(SUM(P.amount)>100, 'Wysoka', 'Niska') AS payment_amount FROM customer C 
LEFT JOIN payment P USING(customer_id) GROUP BY C.customer_id;

-- Zadanie 3
SELECT rental_id, TIMESTAMPDIFF(DAY, rental_date, return_date) AS rental_days 
FROM rental WHERE YEAR(return_date) = 2005 AND MONTH(return_date) = 7;

-- Zadanie 4
SELECT L.name AS language_name, COUNT(F.film_id) AS number_of_films FROM film F INNER JOIN language L USING(language_id)
GROUP BY F.language_id HAVING COUNT(F.film_id) > 900 ORDER BY number_of_films DESC;