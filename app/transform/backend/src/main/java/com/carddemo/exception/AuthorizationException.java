package com.carddemo.exception;

/**
 * Exception thrown when authorization fails (insufficient permissions)
 * HTTP Status: 403 Forbidden
 */
public class AuthorizationException extends RuntimeException {
    
    public AuthorizationException(String message) {
        super(message);
    }

    public AuthorizationException(String message, Throwable cause) {
        super(message, cause);
    }
}
