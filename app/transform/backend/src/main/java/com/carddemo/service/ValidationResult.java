package com.carddemo.service;

/**
 * Represents the result of a validation operation
 * Contains validation status and error message if validation failed
 */
public class ValidationResult {
    private final boolean valid;
    private final String message;
    private final String field;

    private ValidationResult(boolean valid, String message, String field) {
        this.valid = valid;
        this.message = message;
        this.field = field;
    }

    /**
     * Create a successful validation result
     * @return ValidationResult indicating success
     */
    public static ValidationResult success() {
        return new ValidationResult(true, null, null);
    }

    /**
     * Create a failed validation result with error message
     * @param message the error message
     * @return ValidationResult indicating failure
     */
    public static ValidationResult failure(String message) {
        return new ValidationResult(false, message, null);
    }

    /**
     * Create a failed validation result with error message and field name
     * @param message the error message
     * @param field the field name that failed validation
     * @return ValidationResult indicating failure
     */
    public static ValidationResult failure(String message, String field) {
        return new ValidationResult(false, message, field);
    }

    public boolean isValid() {
        return valid;
    }

    public String getMessage() {
        return message;
    }

    public String getField() {
        return field;
    }

    @Override
    public String toString() {
        return "ValidationResult{" +
                "valid=" + valid +
                ", message='" + message + '\'' +
                ", field='" + field + '\'' +
                '}';
    }
}
