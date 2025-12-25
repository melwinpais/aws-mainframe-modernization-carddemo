package com.carddemo.exception;

import java.util.ArrayList;
import java.util.List;

/**
 * Exception thrown when validation fails
 * HTTP Status: 400 Bad Request
 */
public class ValidationException extends RuntimeException {
    
    private final List<FieldError> fieldErrors;

    public ValidationException(String message) {
        super(message);
        this.fieldErrors = new ArrayList<>();
    }

    public ValidationException(String message, String field, Object rejectedValue) {
        super(message);
        this.fieldErrors = new ArrayList<>();
        this.fieldErrors.add(new FieldError(field, message, rejectedValue));
    }

    public ValidationException(String message, List<FieldError> fieldErrors) {
        super(message);
        this.fieldErrors = fieldErrors != null ? fieldErrors : new ArrayList<>();
    }

    public List<FieldError> getFieldErrors() {
        return fieldErrors;
    }

    public void addFieldError(String field, String message, Object rejectedValue) {
        this.fieldErrors.add(new FieldError(field, message, rejectedValue));
    }

    /**
     * Represents a field-specific validation error
     */
    public static class FieldError {
        private final String field;
        private final String message;
        private final Object rejectedValue;

        public FieldError(String field, String message, Object rejectedValue) {
            this.field = field;
            this.message = message;
            this.rejectedValue = rejectedValue;
        }

        public String getField() {
            return field;
        }

        public String getMessage() {
            return message;
        }

        public Object getRejectedValue() {
            return rejectedValue;
        }
    }
}
