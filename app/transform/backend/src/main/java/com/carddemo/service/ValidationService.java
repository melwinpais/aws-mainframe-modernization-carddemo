package com.carddemo.service;

import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.regex.Pattern;

/**
 * Service for validating business data according to CardDemo requirements
 * Implements all validation rules from Requirements 17.1-17.12
 */
@Service
public class ValidationService {

    // Validation patterns
    private static final Pattern NUMERIC_PATTERN = Pattern.compile("^\\d+$");
    private static final Pattern DATE_PATTERN = Pattern.compile("^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$");
    private static final Pattern CURRENCY_PATTERN = Pattern.compile("^-?\\d{1,10}(\\.\\d{2})?$");
    private static final Pattern USER_ID_PATTERN = Pattern.compile("^[A-Za-z0-9]{1,8}$");
    private static final Pattern PASSWORD_PATTERN = Pattern.compile("^.{8}$");
    private static final Pattern NAME_PATTERN = Pattern.compile("^[A-Za-z\\s]+$");
    private static final Pattern PHONE_PATTERN = Pattern.compile("^\\(\\d{3}\\)\\d{3}-\\d{4}$");
    private static final Pattern SSN_PATTERN = Pattern.compile("^\\d{9}$");
    private static final Pattern FICO_PATTERN = Pattern.compile("^\\d{3}$");

    /**
     * Validate account ID (Requirement 17.1)
     * Must be numeric, 11 digits, and not all zeroes
     * 
     * @param accountId the account ID to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateAccountId(String accountId) {
        if (accountId == null || accountId.trim().isEmpty()) {
            return ValidationResult.failure("Account number not provided", "accountId");
        }

        if (!NUMERIC_PATTERN.matcher(accountId).matches()) {
            return ValidationResult.failure("Account number must be a non zero 11 digit number", "accountId");
        }

        if (accountId.length() != 11) {
            return ValidationResult.failure("Account number must be a non zero 11 digit number", "accountId");
        }

        if (accountId.equals("00000000000")) {
            return ValidationResult.failure("Account number must be a non zero 11 digit number", "accountId");
        }

        return ValidationResult.success();
    }

    /**
     * Validate card number (Requirement 17.2)
     * Must be numeric and 16 digits
     * 
     * @param cardNumber the card number to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateCardNumber(String cardNumber) {
        if (cardNumber == null || cardNumber.trim().isEmpty()) {
            return ValidationResult.failure("Card number is required", "cardNumber");
        }

        if (!NUMERIC_PATTERN.matcher(cardNumber).matches()) {
            return ValidationResult.failure("Card number must be 16 digits", "cardNumber");
        }

        if (cardNumber.length() != 16) {
            return ValidationResult.failure("Card number must be 16 digits", "cardNumber");
        }

        return ValidationResult.success();
    }

    /**
     * Validate date format (Requirement 17.3)
     * Must be YYYY-MM-DD format with valid year, month (1-12), and day ranges
     * 
     * @param date the date to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateDate(String date) {
        if (date == null || date.trim().isEmpty()) {
            return ValidationResult.failure("Date is required", "date");
        }

        if (!DATE_PATTERN.matcher(date).matches()) {
            return ValidationResult.failure("Date must be in YYYY-MM-DD format", "date");
        }

        // Additional validation for valid date ranges
        String[] parts = date.split("-");
        int year = Integer.parseInt(parts[0]);
        int month = Integer.parseInt(parts[1]);
        int day = Integer.parseInt(parts[2]);

        // Validate year range (reasonable range)
        if (year < 1900 || year > 2100) {
            return ValidationResult.failure("Year must be between 1900 and 2100", "date");
        }

        // Validate day based on month
        int[] daysInMonth = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        
        // Check for leap year
        if (month == 2 && isLeapYear(year)) {
            daysInMonth[1] = 29;
        }

        if (day > daysInMonth[month - 1]) {
            return ValidationResult.failure("Invalid day for the specified month", "date");
        }

        return ValidationResult.success();
    }

    /**
     * Validate currency amount (Requirement 17.4)
     * Must be signed numeric format with two decimal places (S9(10)V99)
     * 
     * @param amount the amount to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateCurrencyAmount(String amount) {
        if (amount == null || amount.trim().isEmpty()) {
            return ValidationResult.failure("Amount is required", "amount");
        }

        if (!CURRENCY_PATTERN.matcher(amount).matches()) {
            return ValidationResult.failure("Amount must be in format S9(10)V99 with two decimal places", "amount");
        }

        try {
            BigDecimal value = new BigDecimal(amount);
            
            // Check if value is within range -9999999999.99 to 9999999999.99
            BigDecimal maxValue = new BigDecimal("9999999999.99");
            BigDecimal minValue = new BigDecimal("-9999999999.99");
            
            if (value.compareTo(maxValue) > 0 || value.compareTo(minValue) < 0) {
                return ValidationResult.failure("Amount must be between -9999999999.99 and 9999999999.99", "amount");
            }
        } catch (NumberFormatException e) {
            return ValidationResult.failure("Invalid amount format", "amount");
        }

        return ValidationResult.success();
    }

    /**
     * Validate user ID (Requirement 17.5)
     * Must be 1-8 characters, alphanumeric
     * 
     * @param userId the user ID to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateUserId(String userId) {
        if (userId == null || userId.trim().isEmpty()) {
            return ValidationResult.failure("Please enter User ID ...", "userId");
        }

        if (!USER_ID_PATTERN.matcher(userId).matches()) {
            return ValidationResult.failure("User ID must be 1-8 alphanumeric characters", "userId");
        }

        return ValidationResult.success();
    }

    /**
     * Validate password (Requirement 17.6)
     * Must be exactly 8 characters
     * 
     * @param password the password to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validatePassword(String password) {
        if (password == null || password.trim().isEmpty()) {
            return ValidationResult.failure("Please enter Password ...", "password");
        }

        if (!PASSWORD_PATTERN.matcher(password).matches()) {
            return ValidationResult.failure("Password must be exactly 8 characters", "password");
        }

        return ValidationResult.success();
    }

    /**
     * Validate account active status (Requirement 17.7)
     * Must be 'Y' or 'N'
     * 
     * @param status the status to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateAccountStatus(String status) {
        if (status == null || status.trim().isEmpty()) {
            return ValidationResult.failure("Account status is required", "activeStatus");
        }

        if (!status.equals("Y") && !status.equals("N")) {
            return ValidationResult.failure("Account Active Status must be Y or N", "activeStatus");
        }

        return ValidationResult.success();
    }

    /**
     * Validate user type (Requirement 17.8)
     * Must be 'A' (Admin) or 'U' (User)
     * 
     * @param userType the user type to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateUserType(String userType) {
        if (userType == null || userType.trim().isEmpty()) {
            return ValidationResult.failure("User type is required", "userType");
        }

        if (!userType.equals("A") && !userType.equals("U")) {
            return ValidationResult.failure("User type must be A (Admin) or U (User)", "userType");
        }

        return ValidationResult.success();
    }

    /**
     * Validate name (Requirement 17.9)
     * Must contain only alphabetic characters and spaces
     * 
     * @param name the name to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateName(String name) {
        if (name == null || name.trim().isEmpty()) {
            return ValidationResult.failure("Name is required", "name");
        }

        if (!NAME_PATTERN.matcher(name).matches()) {
            return ValidationResult.failure("Name must contain only alphabetic characters and spaces", "name");
        }

        return ValidationResult.success();
    }

    /**
     * Validate US phone number (Requirement 17.10)
     * Must be in format (XXX)XXX-XXXX with numeric digits
     * 
     * @param phoneNumber the phone number to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validatePhoneNumber(String phoneNumber) {
        if (phoneNumber == null || phoneNumber.trim().isEmpty()) {
            return ValidationResult.failure("Phone number is required", "phoneNumber");
        }

        if (!PHONE_PATTERN.matcher(phoneNumber).matches()) {
            return ValidationResult.failure("Phone number must be in format (XXX)XXX-XXXX", "phoneNumber");
        }

        return ValidationResult.success();
    }

    /**
     * Validate US SSN (Requirement 17.11)
     * Must be 9-digit numeric format and reject invalid SSN patterns
     * (000, 666, 900-999 in first part)
     * 
     * @param ssn the SSN to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateSSN(String ssn) {
        if (ssn == null || ssn.trim().isEmpty()) {
            return ValidationResult.failure("SSN is required", "ssn");
        }

        if (!SSN_PATTERN.matcher(ssn).matches()) {
            return ValidationResult.failure("SSN must be 9 digits", "ssn");
        }

        // Extract first 3 digits
        int firstPart = Integer.parseInt(ssn.substring(0, 3));

        // Reject invalid SSN patterns
        if (firstPart == 0 || firstPart == 666 || (firstPart >= 900 && firstPart <= 999)) {
            return ValidationResult.failure("Invalid SSN pattern", "ssn");
        }

        return ValidationResult.success();
    }

    /**
     * Validate FICO credit score (Requirement 17.12)
     * Must be 3-digit numeric format (300-850)
     * 
     * @param score the FICO score to validate
     * @return ValidationResult indicating success or failure with message
     */
    public ValidationResult validateFicoScore(String score) {
        if (score == null || score.trim().isEmpty()) {
            return ValidationResult.failure("FICO score is required", "ficoCreditScore");
        }

        if (!FICO_PATTERN.matcher(score).matches()) {
            return ValidationResult.failure("FICO score must be 3 digits", "ficoCreditScore");
        }

        int scoreValue = Integer.parseInt(score);
        if (scoreValue < 300 || scoreValue > 850) {
            return ValidationResult.failure("FICO score must be between 300 and 850", "ficoCreditScore");
        }

        return ValidationResult.success();
    }

    /**
     * Normalize user ID to uppercase (Requirement 17.5)
     * 
     * @param userId the user ID to normalize
     * @return normalized user ID in uppercase
     */
    public String normalizeUserId(String userId) {
        if (userId == null) {
            return null;
        }
        return userId.toUpperCase();
    }

    /**
     * Helper method to check if a year is a leap year
     * 
     * @param year the year to check
     * @return true if leap year, false otherwise
     */
    private boolean isLeapYear(int year) {
        return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
    }
}
