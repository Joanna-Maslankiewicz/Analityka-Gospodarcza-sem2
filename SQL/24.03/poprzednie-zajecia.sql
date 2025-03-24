SHOW DATABASES;
-- DROP DATABASE university; -- delete database
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
LEFT JOIN 	-- relacja jeden do wielu; nie wypisuje kursów na które nikt się nie zapisał; FULL OUTER JOIN - jak chcemy zobaczyć wszystkie kursy i studentów, nawet jeśli któreś nie są przypisane
	courses c ON s.course_id = c.course_id;