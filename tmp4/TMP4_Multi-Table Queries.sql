# Multi-Table Queries

# Query 1: Get a list of category names and a count of movies that fall into that category. Name the category column “category” the count column “num_films”. Order the results alphabetically (ascending). Use the WHERE clause to join the tables.
select name as category, Count(*) as num_films
from category, film, film_category 
where film_category.film_id = film.film_id and film_category.category_id = category.category_id
group by name
order by name;

# Query 2: Repeat the query above using a JOIN clause instead of the WHERE clause. 
select name as category, Count(*) as num_films
from film_category join category on film_category.category_id = category.category_id  
join film on film_category.film_id = film.film_id
group by name
order by name;

# Query 3: Get a list of country names and a count of the cities that are in that country. Name the count column “num_cities”. Order the results alphabetically (ascending). Use the WHERE clause to join the tables.
select country as country, Count(*) as num_cities
from country, city
where city.country_id = country.country_id
group by country
order by country asc;

# Query 4:  Repeat the query above using a JOIN clause instead of the WHERE clause.
select country as country, Count(*) as num_cities
from country join city on city.country_id = country.country_id
group by country
order by country;

# Query 5: Get a list of each customer’s last name and first name and the number of rentals they have. Name the count column “num_rentals”. Order the result by the number of rentals in descending
# order. The highest number of rentals should be at the top. Sort any es (same number of rentals) by last name (ascending). Use the WHERE clause to join the tables. 
select last_name, first_name, Count(*) as num_rentals
from customer, rental
where rental.customer_id = customer.customer_id
group by last_name, first_name
order by num_rentals desc, last_name asc;

# Query 6: Repeat the query above using a JOIN clause instead of the WHERE clause. 
select last_name, first_name, Count(*) as num_rentals
from customer join rental on rental.customer_id = customer.customer_id
group by last_name, first_name
order by num_rentals desc, last_name asc;

# Query 7: Get a list of each customer’s last name and first name and the amount of money they have spent on rentals. Name the sum column “total_spent”. Order the result by the amount in descending
# order. The highest amount of money spent should be at the top. Sort any es (amount of money spent) by last name (ascending). Use the JOIN clause for this query. 
select last_name, first_name, sum(payment.amount) as total_spent
from customer join payment on payment.customer_id = customer.customer_id
group by last_name, first_name
order by total_spent desc, last_name asc;

# Query 8: Get the number of actors in each film. Order the results (ascending) by the film title and name column with the actor count “num_actors”.
select title as film, count(actor_id) as num_actors
from film join film_actor on film_actor.film_id = film.film_id
group by title
order by title;

# Query 9: Get the number of films each manager holds. Use only the manager staff id to identify the manager. Name the column with the number of films “num_films”. 
select manager_staff_id as manager, count(film_id) as num_films
from store join inventory on inventory.store_id = store.store_id
group by manager_staff_id
order by manager_staff_id;

# Query 10: Get the number of customers per manager. Use only the manager staff id to identify the manager. Name the column with the number of films “num_customers”. Order by store id (ascending). 
select manager_staff_id as manager, count(customer_id) as num_customers
from store join customer on customer.store_id = store.store_id
group by manager_staff_id
order by store.store_id asc;


