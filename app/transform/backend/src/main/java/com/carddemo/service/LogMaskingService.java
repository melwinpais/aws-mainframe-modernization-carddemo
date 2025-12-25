package com.carddemo.service;

import org.springframework.stereotype.Service;

/**
 * Service for masking sensitive data in logs
 * Implements Requirements 14.5, 14.6
 * Ensures passwords, card numbers, and SSN are not logged in plain text
 */
@Service
public class LogMaskingService {

    private static final String MASK_CHAR = "*";

    /**
     * Mask a password completely
     * Requirement 14.6: Never log passwords
     * 
     * @param password the password to mask
     * @return masked password (all asterisks)
     */
    public String maskPassword(String password) {
        if (password == null || password.isEmpty()) {
            return "";
        }
        return MASK_CHAR.repeat(8); // Always show 8 asterisks for passwords
    }

    /**
     * Mask a card number, showing only last 4 digits
     * Requirement 14.6: Mask all but last 4 digits of card numbers
     * 
     * @param cardNumber the card number to mask
     * @return masked card number (e.g., "************1234")
     */
    public String maskCardNumber(String cardNumber) {
        if (cardNumber == null || cardNumber.isEmpty()) {
            return "";
        }
        
        if (cardNumber.length() < 4) {
            return MASK_CHAR.repeat(cardNumber.length());
        }
        
        String lastFour = cardNumber.substring(cardNumber.length() - 4);
        return MASK_CHAR.repeat(12) + lastFour;
    }

    /**
     * Mask a card number (Long), showing only last 4 digits
     * 
     * @param cardNumber the card number to mask
     * @return masked card number (e.g., "************1234")
     */
    public String maskCardNumber(Long cardNumber) {
        if (cardNumber == null) {
            return "";
        }
        return maskCardNumber(String.valueOf(cardNumber));
    }

    /**
     * Mask an SSN, showing only last 4 digits
     * Requirement 14.6: Mask all but last 4 digits of SSN
     * 
     * @param ssn the SSN to mask
     * @return masked SSN (e.g., "***-**-1234")
     */
    public String maskSSN(String ssn) {
        if (ssn == null || ssn.isEmpty()) {
            return "";
        }
        
        if (ssn.length() < 4) {
            return MASK_CHAR.repeat(ssn.length());
        }
        
        String lastFour = ssn.substring(ssn.length() - 4);
        return "***-**-" + lastFour;
    }

    /**
     * Mask an SSN (Long), showing only last 4 digits
     * 
     * @param ssn the SSN to mask
     * @return masked SSN (e.g., "***-**-1234")
     */
    public String maskSSN(Long ssn) {
        if (ssn == null) {
            return "";
        }
        return maskSSN(String.valueOf(ssn));
    }

    /**
     * Mask a JWT token, showing only first and last 4 characters
     * 
     * @param token the JWT token to mask
     * @return masked token
     */
    public String maskToken(String token) {
        if (token == null || token.isEmpty()) {
            return "";
        }
        
        if (token.length() <= 8) {
            return MASK_CHAR.repeat(token.length());
        }
        
        String firstFour = token.substring(0, 4);
        String lastFour = token.substring(token.length() - 4);
        return firstFour + MASK_CHAR.repeat(token.length() - 8) + lastFour;
    }

    /**
     * Create a safe log message with masked sensitive data
     * 
     * @param message the message template
     * @param args the arguments (will be masked if sensitive)
     * @return formatted message with masked sensitive data
     */
    public String createSafeLogMessage(String message, Object... args) {
        if (args == null || args.length == 0) {
            return message;
        }
        
        Object[] maskedArgs = new Object[args.length];
        for (int i = 0; i < args.length; i++) {
            maskedArgs[i] = maskIfSensitive(args[i]);
        }
        
        return String.format(message, maskedArgs);
    }

    /**
     * Mask an object if it appears to be sensitive data
     * 
     * @param obj the object to check and potentially mask
     * @return masked value if sensitive, original value otherwise
     */
    private Object maskIfSensitive(Object obj) {
        if (obj == null) {
            return null;
        }
        
        String str = obj.toString();
        
        // Check if it looks like a card number (16 digits)
        if (str.matches("\\d{16}")) {
            return maskCardNumber(str);
        }
        
        // Check if it looks like an SSN (9 digits)
        if (str.matches("\\d{9}")) {
            return maskSSN(str);
        }
        
        // Check if it looks like a JWT token (contains dots and is long)
        if (str.contains(".") && str.length() > 50) {
            return maskToken(str);
        }
        
        return obj;
    }
}
