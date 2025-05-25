USE Sakila;
Show tables;
#SQL Exercises - Part 1
	#1. List information about actors.;
	#2. List information about languages in which movies could be filmed.
	#3. List information about countries from which rental store customers may come.
	#4. List different last names of actors.
	#5. Display the first name, last name, and email of all customers, merging the first and last name into a single column labeled ”person”.
#Ad.1
Select * from actor;
#Ad.2
Select * from language;
#Ad.3
Select * from country;
#Ad.4
Select last_name from actor;
#Ad.5
Select * from customer;
Select concat(first_name, ' ', last_name) as person, email from customer;

#SQL Exercises - Part 2
#1. List titles and descriptions of all movies that cost no more than $1 to rent and are rated ’G’.
#2. Display actor names (first and last) in a single column labeled ”actor” where either the first or last name starts with ’J’.
#3. List rentals recorded in June for customers with IDs 111, 222,333, 444, and 555.
#4. List distinct actor names that contain the letter ’A’ but do not start or end with ’A’.
#5. List movie titles that contain the word ’love’ and either have a replacement cost between 20 and 30 dollars or are at least 90 minutes long.

#Ad.1
Show tables;
Select * from film;
Select title, description from film where rental_rate <= 1 AND rating = "G"; 
#Ad.2
Select concat(first_name, ' ', last_name) from actor where first_name like "J%" or last_name like "J%";
#Ad.3
Select * from rental;
Select rental_id from rental where customer_id in ('111','222','333','444','555');
#Ad.4 
Select distinct first_name from actor where first_name like '%a%'
	and first_name not like '%a' 
    and first_name not like 'a%';
#Ad.5
Select title from film where title like '%love%' and (replacement_cost between 20 and 30 or length >=90);

#SQL Exercises - Part 3
#1. List all unique payment amounts processed by employee with ID 1.
#2. List all actor first names in alphabetical order.
#3. List all movie titles in the PG category sorted from shortest to longest, with a secondary alphabetical sorting by title.
#4. List the first 100 payment records for customers with IDs between 100 and 199 who rented movies in August, sorted by payment amount and then by payment date.
#5. List all active customers of store 1 along with their email addresses in a single column labeled ”customer” with the format ”First Last (email)”, sorted alphabetically by last name and first name.

#Ad.1
select distinct amount from payment where staff_id = 1;
#Ad.2
select first_name from actor order by first_name;
#Ad.3
select title from film where rating like "PG" order by length asc, title asc;
#Ad.4
SELECT p.payment_id, p.customer_id, p.amount, p.payment_date
FROM payment p
JOIN rental r ON p.rental_id = r.rental_id
WHERE p.customer_id BETWEEN 100 AND 199
AND r.rental_date BETWEEN '2005-08-01' AND '2005-08-31'
ORDER BY p.amount ASC, p.payment_date ASC
LIMIT 100;
#Ad.5
Select * from customer;
SELECT CONCAT(first_name, ' ', last_name, ' (', email, ')') AS customer FROM customer order by last_name, first_name;

#Task 1 - Questions
#1. Calculate the average length of movie titles based on all films in the database. Display the result in a column labeled average.
#2. Determine the number of films available for rental in each store (count each title only once). Display results in two columns: store (store ID) and titles (number of films).
#3. Identify the top 10 most popular actors (those appearing in the most films), sorted descending by film count. Use columns: actor (actor ID) and n (number of films).
#4. List the age rating category, along with the minimum, average, and maximum replacement cost for each, sorted by average. Use columns: category, min, average, and max.
#5. List the IDs of 10 countries represented by the fewest cities (but more than 10) in the database. Display two columns: country (country ID) and n (number of cities). Is Poland among them?

#Ad.1
Select AVG(char_length(title)) as average from film;
#Ad.2
Select store_id as store, count(distinct(film_id)) as title from inventory group by store;
#Ad.3
Select actor_id as actor, count(distinct(film_id)) as film from film_actor group by actor order by film desc limit 10;
#Ad.4
Select rating AS category,
MIN(replacement_cost) AS min ,
AVG(replacement_cost) AS average ,
MAX(replacement_cost) AS max FROM film
GROUP BY category ORDER BY average;
#Ad.5
Select country_id as country, count(city) as city from city group by country having city > 10 order by city limit 10;

#Task 2 - Questions
#1. Calculate the average and standard deviation of actors’ last name lengths, rounded to 4 decimal places, labeled average and sigma.
#2. What is the range of the number of film copies available in store ID 1? Compute the minimum and maximum separately in columns n min and n max (two queries).
#3. Calculate the average and standard deviation of film lengths for each age rating category, rounded to 2 decimal places, in columns category, average, and sigma.
#4. Determine the mode of rental days for films with a rental rate of 4.99.
#5. Calculate the average and standard deviation of replacement costs per age rating category, for films lasting 90–150 minutes, in columns category, average, and sigma.

#Ad.1
Select round(AVG(char_length(last_name)),4) as average, 
round(stddev_pop(char_length(last_name)),4) as sigma
from actor;
#Ad.2
Select store_id, count(film_id) as film_min from inventory
	where store_id = 1 group by film_id order by film_min limit 1;
Select store_id, count(film_id) as film_max from inventory
	where store_id = 1 group by film_id order by film_max DESC LIMIT 1;
#Ad.3
select rating as category, 
	Round(AVG(length), 2) as average,
    Round(stddev_samp(length), 2) as sigma
    from film group by category;
#Ad.4
select rental_duration as mode from film
	where rental_rate = 4.99 group by mode
    order by count(mode) desc limit 1;
#Ad.5
select rating as category,
	AVG(replacement_cost) AS average,
	STDDEV_SAMP(replacement_cost) AS sigma
from film where length between 90 and 150 
group by category;

#Task 3 - Questions
#1. How many days did the shortest, average, and longest film rentals last? Display in columns min, average, and max.
#2. Which day of the week had the most unreturned film rentals? Display the day name in day and count in n.
#3. List all payments made on Wednesdays handled by staff ID 2, sorted ascending by payment date.
#4. List total revenue by day of the week, in columns day (Sunday to Saturday) and revenue.
#5. List revenue by staff ID and month, in columns id (staff ID), month (month name), and revenue, sorted by ID and month order

#Ad.1
Select MIN(datediff(return_date,rental_date)) as min,	
	AVG(datediff(return_date,rental_date)) as avg,
    MAX(datediff(return_date,rental_date)) as max
from rental where return_date is not null;
#Ad.2
Select DAYNAME(rental_date) as day, count(*) as n from rental 
	where return_date is null
	group by day order by n desc limit 1;
#Ad.3
Select * from payment where DAYNAME(payment_date) = 'Wednesday' and staff_id = 2
order by payment_date asc;
#Ad.4
Select DAYNAME(payment_date) as day, 
	sum(amount) as revenue from payment
group by day
order by field(day, 'Sunday', 'Monday','Tuesday','Wednesday', 'Thursday',
'Friday','Saturday'); 
#Ad.5
Select staff_id as id,
	monthname(payment_date) as month, 
	sum(amount) as revenue 
from payment
group by id, month
order by id, month;

#Exercise Set 1: Subqueries
#1 List the country where the city Bellevue is located.
#2 List all phone numbers of customers from Poland.
#3 List actors (first and last name in a column labeled actor) who starred in films in the Children category.
#4 List titles of films with the most copies in store 1.
#5 List customers (first and last name in a column labeled customer) whose total payments are at most half the average payment total.

#Ad.1
Select country from country where country_id in 
( SELECT country_id FROM city WHERE city LIKE 'Bellevue');
#Ad.2
Select phone from address where city_id in
(Select city_id from city where country_id =
(select country_id from country where country = 'Poland'));
#Ad.3
Select distinct concat(first_name, ' ',last_name) as actor from actor where actor_id in
(select actor_id from film_actor where film_id in
(select film_id from film_category where category_id =
(select category_id from category where name ='Children')));
#Ad.4
Select title from film where film_id in 
(select film_id from inventory where store_id =1
	group by film_id 
		having count(*) = 
			(select max(copy_count) from (
				select count(*) as copy_count from inventory where store_id = 1 group by film_id)
                as count));
#Ad.5
Select concat(first_name, ' ',last_name) as customer from customer where customer_id in
	(select customer_id from payment group by customer_id having sum(amount) <= 0.5 * 
		(select avg(total) from (select sum(amount) as total from payment group by customer_id) as sub)); 
        
#Exercise Set 2: Joins
#1 List all possible combinations of actor names (actor) and category names (category), sorted by actor’s last name, first name, and category name.
#2 List all possible combinations of category names (type) and film ratings (category), sorted by category name and rating.
#3 List film titles (film), category names (type), and ratings (category), sorted by category and title.
#4 List all customers of store 1 served by staff member 2.

#Ad.1
select concat(A.first_name, ' ', A.last_name) as actor,
C.name as category from actor A cross join category C
order by A.last_name, A.first_name, C.name;
#Ad.2
select C.name as type, F.rating as category
from category C cross join film F 
group by type, category order by type, category;
#Ad.3
Select F.title as film, C.name as type, F.rating as category
from film F inner join film_category CF using(film_id)
inner join category C using(category_id)
order by type, film;
#Ad.4
SELECT DISTINCT C.* FROM customer C
INNER JOIN rental R USING (customer_id)
WHERE C.store_id = 1 AND R.staff_id = 2;

#Exercise Set 3: Joins and UNION
#1 List film ID, title, and total copies across both stores (id, film, n). Films with no copies should have n=0.
#2 List all payment dates and amounts, along with corresponding rental ID, rental date, and return date (payment date, amount, id, rental date, return date).
#3 List all pairs of IDs from film actor and film category (including duplicates) in columns film and actor category, sorted by film ID and actor/category ID.
#4 Assign each country the total payments made by its customers, sorted in descending order.
#5 For each customer, list the number of rented films per category (customer, category, n), sorted by last name, first name, and number of rentals (descending).

#Ad.1
Select F.film_id as id, F.title as film, count(I.film_id) as n 
from film F left join inventory I using(film_id)
group by film_id;
#Ad.2
Select P.payment_date as payment_date, P.amount as amount,
R.rental_id as id, R.rental_date as rental_date,
R.return_date as return_date
from payment P left join rental R using(rental_id)
order by payment_date;
#Ad.3
(Select FA.film_id as film, FA.actor_id as actor_category from film_actor FA)
union all
(Select FC.film_id, FC.category_id from film_category FC)
order by film, actor_category;
#Ad.4
Select C.country as country, sum(P.amount) as total from payment P inner join customer using (customer_id)
inner join address using(address_id) inner join city using(city_id)
Inner join country C using(country_id)
group by country order by total desc;
#Ad.5
Select concat(C.first_name, ' ',C.last_name) as customer, CA.name as cateogry, 
count(R.rental_id) as n from customer C inner join rental R using(customer_id) 
inner join inventory I using(inventory_id) inner join film F using(film_id)
inner join film_category FC using(film_id) inner join
 category CA using(category_id)
 group by C.first_name, C.last_name, CA.name
 order by C.last_name, C.first_name, n desc;
