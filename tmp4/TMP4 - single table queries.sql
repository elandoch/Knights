# 1. Get a list of all film titles alphabetized by title.  
SELECT title
FROM film
ORDER BY title;

# 2. Find the description, release year, length, and rating for the movie “KENTUCKIAN GIANT”.  
SELECT title, description, release_year, length, rating
FROM film
WHERE title = "KENTUCKIAN GIANT";

# 3. Find the first name and last name of each employee (staff table). Your query should include the last name first, and then the first name.  
SELECT last_name, first_name
FROM staff;

# 4. Repeat the query above, but this me, the results should include only one column with the format last name, first name. The output column should be named “name”  
SELECT CONCAT(last_name, ', ', first_name) AS name
FROM staff;

# 5. Get the number of customers. The output should be a single number. Name the column “num_customers”  
SELECT COUNT(*) AS num_customers
FROM customer;

# 6. Get the number of customers who are active vs inactive in the system.  
SELECT active, COUNT(*) AS count
FROM customer
GROUP BY active;

# 7. Get the average amount a customer spends on a rental.  
SELECT AVG(amount) AS avg_spent
FROM payment;

# 8. Get maximum amount any customer has spent on a rental.  
SELECT MAX(amount) AS max_spent
FROM payment;

# 9. Get a list of the actors. The results should include only one column with the format last name, first name.  The column should be named “actor_name”  The results should be sorted be sorted alphabetically by the last name (ascending).  
SELECT CONCAT(last_name, ', ', first_name) AS actor_name
FROM actor
ORDER BY last_name;

# 10. Repeat this query above, but the results should be in reverse order. 
SELECT CONCAT(last_name, ', ', first_name) AS actor_name
FROM actor
ORDER BY last_name DESC;