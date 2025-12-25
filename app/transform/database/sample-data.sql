-- Sample Data for CardDemo Application
-- This file contains realistic test data for development and testing

-- Clear existing data (in reverse order of dependencies)
DELETE FROM transactions;
DELETE FROM card_xref;
DELETE FROM cards;
DELETE FROM accounts;
DELETE FROM customers;
DELETE FROM users;

-- Insert Users (passwords are BCrypt hashed 'password')
INSERT INTO users (user_id, first_name, last_name, password, user_type, created_at, updated_at) VALUES
('U0001', 'Admin', 'User', '$2a$10$dXJ3SW6G7P50lGmMkkmwe.20cQQubK3.HZWzG3YB1tlRy.fqvM/BG', 'A', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
('U0002', 'John', 'Smith', '$2a$10$dXJ3SW6G7P50lGmMkkmwe.20cQQubK3.HZWzG3YB1tlRy.fqvM/BG', 'U', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
('U0003', 'Mary', 'Jones', '$2a$10$dXJ3SW6G7P50lGmMkkmwe.20cQQubK3.HZWzG3YB1tlRy.fqvM/BG', 'U', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
('U0004', 'Bob', 'Wilson', '$2a$10$dXJ3SW6G7P50lGmMkkmwe.20cQQubK3.HZWzG3YB1tlRy.fqvM/BG', 'U', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- Insert Customers
INSERT INTO customers (customer_id, first_name, middle_name, last_name, address_line_1, address_line_2, address_line_3, state_code, country_code, zip_code, phone_number_1, phone_number_2, ssn, government_issued_id, date_of_birth, fico_credit_score, created_at, updated_at) VALUES
(1, 'John', 'A', 'Smith', '123 Main Street', 'Apt 4B', '', 'NY', 'USA', '10001', '212-555-0101', '212-555-0102', 123456789, 'DL-NY-12345678', '1985-03-15', 720, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(2, 'Mary', 'B', 'Jones', '456 Oak Avenue', 'Suite 200', '', 'CA', 'USA', '90210', '310-555-0201', '', 234567890, 'DL-CA-23456789', '1990-07-22', 780, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(3, 'Bob', '', 'Wilson', '789 Elm Street', '', '', 'IL', 'USA', '60601', '312-555-0301', '312-555-0302', 345678901, 'DL-IL-34567890', '1978-11-30', 650, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4, 'Alice', 'C', 'Johnson', '321 Pine Road', 'Unit 5', '', 'MA', 'USA', '02101', '617-555-0401', '', 456789012, 'DL-MA-45678901', '1995-05-18', 800, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- Insert Accounts
INSERT INTO accounts (account_id, customer_id, active_status, current_balance, credit_limit, cash_credit_limit, open_date, expiration_date, reissue_date, current_cycle_credit, current_cycle_debit, address_zip, group_id, created_at, updated_at) VALUES
(1, 1, 'Y', 1250.50, 5000.00, 1000.00, '2023-01-15', '2026-01-15', '2025-01-15', 500.00, 1750.50, '10001', 'GRP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(2, 2, 'Y', 3420.75, 10000.00, 2000.00, '2022-06-20', '2025-06-20', '2024-06-20', 1200.00, 4620.75, '90210', 'GRP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(3, 3, 'Y', 567.25, 3000.00, 500.00, '2023-09-10', '2026-09-10', '2025-09-10', 200.00, 767.25, '60601', 'GRP002', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4, 4, 'Y', 0.00, 7500.00, 1500.00, '2024-01-05', '2027-01-05', '2026-01-05', 0.00, 0.00, '02101', 'GRP002', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- Insert Cards
INSERT INTO cards (card_number, account_id, customer_id, card_status, expiration_date, issue_date, created_at, updated_at) VALUES
(4000123456789001, 1, 1, 'ACTIVE', '2026-01-31', '2023-01-15', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4000123456789002, 2, 2, 'ACTIVE', '2025-06-30', '2022-06-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4000123456789003, 3, 3, 'ACTIVE', '2026-09-30', '2023-09-10', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4000123456789004, 4, 4, 'ACTIVE', '2027-01-31', '2024-01-05', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
(4000123456789005, 2, 2, 'INACTIVE', '2025-06-30', '2022-06-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- Insert Card Cross-Reference
INSERT INTO card_xref (card_number, customer_id, account_id, created_at) VALUES
(4000123456789001, 1, 1, CURRENT_TIMESTAMP),
(4000123456789002, 2, 2, CURRENT_TIMESTAMP),
(4000123456789003, 3, 3, CURRENT_TIMESTAMP),
(4000123456789004, 4, 4, CURRENT_TIMESTAMP),
(4000123456789005, 2, 2, CURRENT_TIMESTAMP);

-- Insert Transactions
INSERT INTO transactions (account_id, card_number, transaction_type, transaction_category, amount, transaction_date, transaction_time, description, merchant_name, merchant_city, merchant_zip, created_at) VALUES
(1, 4000123456789001, 'PURCHASE', 'GROCERY', 125.50, '2024-12-20', '10:30:00', 'GROCERY STORE PURCHASE', 'WHOLE FOODS MARKET', 'NEW YORK', '10001', CURRENT_TIMESTAMP),
(1, 4000123456789001, 'PURCHASE', 'DINING', 85.75, '2024-12-21', '19:15:00', 'RESTAURANT PURCHASE', 'OLIVE GARDEN', 'NEW YORK', '10002', CURRENT_TIMESTAMP),
(2, 4000123456789002, 'PURCHASE', 'GAS', 65.00, '2024-12-19', '08:45:00', 'GAS STATION PURCHASE', 'SHELL GAS STATION', 'LOS ANGELES', '90210', CURRENT_TIMESTAMP),
(2, 4000123456789002, 'PURCHASE', 'RETAIL', 250.00, '2024-12-22', '14:20:00', 'DEPARTMENT STORE PURCHASE', 'MACYS', 'LOS ANGELES', '90211', CURRENT_TIMESTAMP),
(3, 4000123456789003, 'PURCHASE', 'GROCERY', 95.25, '2024-12-23', '11:00:00', 'GROCERY STORE PURCHASE', 'JEWEL-OSCO', 'CHICAGO', '60601', CURRENT_TIMESTAMP),
(1, 4000123456789001, 'WITHDRAWAL', 'ATM', 100.00, '2024-12-23', '16:30:00', 'CASH WITHDRAWAL', 'CHASE ATM', 'NEW YORK', '10001', CURRENT_TIMESTAMP),
(2, 4000123456789002, 'PAYMENT', 'PAYMENT', -500.00, '2024-12-24', '09:00:00', 'PAYMENT RECEIVED', 'PAYMENT', 'ONLINE', '00000', CURRENT_TIMESTAMP),
(3, 4000123456789003, 'PURCHASE', 'RETAIL', 45.50, '2024-12-24', '13:45:00', 'RETAIL PURCHASE', 'TARGET', 'CHICAGO', '60602', CURRENT_TIMESTAMP);

-- Verify data load
SELECT 'Users loaded: ' || COUNT(*) FROM users;
SELECT 'Accounts loaded: ' || COUNT(*) FROM accounts;
SELECT 'Customers loaded: ' || COUNT(*) FROM customers;
SELECT 'Cards loaded: ' || COUNT(*) FROM cards;
SELECT 'Card XRef loaded: ' || COUNT(*) FROM card_xref;
SELECT 'Transactions loaded: ' || COUNT(*) FROM transactions;
