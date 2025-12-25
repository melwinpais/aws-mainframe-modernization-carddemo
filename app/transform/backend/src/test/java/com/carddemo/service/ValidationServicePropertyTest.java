package com.carddemo.service;

import net.jqwik.api.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Property-based tests for ValidationService
 * Tests universal properties across all inputs
 * Validates Requirements 17.1-17.12
 */
class ValidationServicePropertyTest {

    private final ValidationService validationService = new ValidationService();

    /**
     * Property 16: Numeric ID Validation
     * For any numeric identifier input, the system should validate the format
     * matches the expected length and pattern
     * Validates: Requirements 17.1, 17.2, 17.5
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 16: Numeric ID Validation")
    void testNumericIdValidation(@ForAll("accountIds") String accountId) {
        ValidationResult result = validationService.validateAccountId(accountId);
        
        boolean isValid = accountId != null && 
                         accountId.matches("\\d{11}") && 
                         !accountId.equals("00000000000");
        
        assertThat(result.isValid()).isEqualTo(isValid);
        
        if (!result.isValid()) {
            assertThat(result.getMessage()).isNotNull();
            assertThat(result.getMessage()).isNotEmpty();
        }
    }

    /**
     * Property 17: Date Format Validation
     * For any date input, the system should validate YYYY-MM-DD format
     * Validates: Requirements 17.3
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 17: Date Format Validation")
    void testDateFormatValidation(@ForAll("dates") String date) {
        ValidationResult result = validationService.validateDate(date);
        
        if (result.isValid()) {
            // Valid dates must match the pattern
            assertThat(date).matches("\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])");
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 18: Currency Amount Validation
     * For any currency amount input, the system should validate signed numeric format
     * Validates: Requirements 17.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 18: Currency Amount Validation")
    void testCurrencyAmountValidation(@ForAll("currencyAmounts") String amount) {
        ValidationResult result = validationService.validateCurrencyAmount(amount);
        
        if (result.isValid()) {
            // Valid amounts must match the pattern
            assertThat(amount).matches("-?\\d{1,10}(\\.\\d{2})?");
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 19: Enumerated Value Validation
     * For any enumerated field input, the system should validate the value
     * Validates: Requirements 17.7, 17.8
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 19: Enumerated Value Validation")
    void testAccountStatusValidation(@ForAll("accountStatuses") String status) {
        ValidationResult result = validationService.validateAccountStatus(status);
        
        boolean isValid = status != null && (status.equals("Y") || status.equals("N"));
        assertThat(result.isValid()).isEqualTo(isValid);
    }

    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 19: Enumerated Value Validation")
    void testUserTypeValidation(@ForAll("userTypes") String userType) {
        ValidationResult result = validationService.validateUserType(userType);
        
        boolean isValid = userType != null && (userType.equals("A") || userType.equals("U"));
        assertThat(result.isValid()).isEqualTo(isValid);
    }

    /**
     * Property 20: Name Validation
     * For any customer name input, the system should validate alphabetic characters and spaces
     * Validates: Requirements 17.9
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 20: Name Validation")
    void testNameValidation(@ForAll("names") String name) {
        ValidationResult result = validationService.validateName(name);
        
        if (result.isValid()) {
            assertThat(name).matches("[A-Za-z\\s]+");
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 21: US Phone Number Validation
     * For any US phone number input, the system should validate format (XXX)XXX-XXXX
     * Validates: Requirements 17.10
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 21: US Phone Number Validation")
    void testPhoneNumberValidation(@ForAll("phoneNumbers") String phoneNumber) {
        ValidationResult result = validationService.validatePhoneNumber(phoneNumber);
        
        if (result.isValid()) {
            assertThat(phoneNumber).matches("\\(\\d{3}\\)\\d{3}-\\d{4}");
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 22: US SSN Validation
     * For any US SSN input, the system should validate 9-digit format and reject invalid patterns
     * Validates: Requirements 17.11
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 22: US SSN Validation")
    void testSSNValidation(@ForAll("ssns") String ssn) {
        ValidationResult result = validationService.validateSSN(ssn);
        
        if (result.isValid()) {
            assertThat(ssn).matches("\\d{9}");
            int firstPart = Integer.parseInt(ssn.substring(0, 3));
            assertThat(firstPart).isNotEqualTo(0);
            assertThat(firstPart).isNotEqualTo(666);
            assertThat(firstPart < 900 || firstPart > 999).isTrue();
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 23: FICO Score Validation
     * For any FICO credit score input, the system should validate 3-digit format
     * Validates: Requirements 17.12
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 23: FICO Score Validation")
    void testFicoScoreValidation(@ForAll("ficoScores") String score) {
        ValidationResult result = validationService.validateFicoScore(score);
        
        if (result.isValid()) {
            assertThat(score).matches("\\d{3}");
            int scoreValue = Integer.parseInt(score);
            assertThat(scoreValue).isBetween(300, 850);
        } else {
            assertThat(result.getMessage()).isNotNull();
        }
    }

    /**
     * Property 24: User ID Case Normalization
     * For any user ID input, the system should convert it to uppercase
     * Validates: Requirements 17.5
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 24: User ID Case Normalization")
    void testUserIdCaseNormalization(@ForAll("userIds") String userId) {
        String normalized = validationService.normalizeUserId(userId);
        
        if (userId != null) {
            assertThat(normalized).isEqualTo(userId.toUpperCase());
        } else {
            assertThat(normalized).isNull();
        }
    }

    // Providers for generating test data

    @Provide
    Arbitrary<String> accountIds() {
        return Arbitraries.oneOf(
            Arbitraries.strings().numeric().ofLength(11),  // Valid format
            Arbitraries.strings().ofLength(11),            // Invalid characters
            Arbitraries.strings().numeric().ofMinLength(1).ofMaxLength(10), // Too short
            Arbitraries.strings().numeric().ofMinLength(12).ofMaxLength(20), // Too long
            Arbitraries.just("00000000000"),               // All zeroes
            Arbitraries.just(null),                        // Null
            Arbitraries.just("")                           // Empty
        );
    }

    @Provide
    Arbitrary<String> dates() {
        return Arbitraries.oneOf(
            Arbitraries.strings().withCharRange('0', '9').ofLength(10), // Random digits
            Arbitraries.just("2024-01-15"),                // Valid date
            Arbitraries.just("2024-12-31"),                // Valid date
            Arbitraries.just("2024-13-01"),                // Invalid month
            Arbitraries.just("2024-01-32"),                // Invalid day
            Arbitraries.just("invalid"),                   // Invalid format
            Arbitraries.just(null),                        // Null
            Arbitraries.just("")                           // Empty
        );
    }

    @Provide
    Arbitrary<String> currencyAmounts() {
        return Arbitraries.oneOf(
            Arbitraries.bigDecimals().between(java.math.BigDecimal.valueOf(-9999999999.99), 
                                              java.math.BigDecimal.valueOf(9999999999.99))
                       .map(bd -> String.format("%.2f", bd)),
            Arbitraries.just("invalid"),
            Arbitraries.just(null),
            Arbitraries.just("")
        );
    }

    @Provide
    Arbitrary<String> accountStatuses() {
        return Arbitraries.oneOf(
            Arbitraries.just("Y"),
            Arbitraries.just("N"),
            Arbitraries.just("X"),
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> userTypes() {
        return Arbitraries.oneOf(
            Arbitraries.just("A"),
            Arbitraries.just("U"),
            Arbitraries.just("X"),
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> names() {
        return Arbitraries.oneOf(
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(25),
            Arbitraries.just("John Doe"),
            Arbitraries.just("Mary Jane"),
            Arbitraries.just("John123"),  // Invalid - contains numbers
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> phoneNumbers() {
        return Arbitraries.oneOf(
            Arbitraries.just("(555)123-4567"),  // Valid
            Arbitraries.just("(800)555-1212"),  // Valid
            Arbitraries.just("555-123-4567"),   // Invalid format
            Arbitraries.just("5551234567"),     // Invalid format
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> ssns() {
        return Arbitraries.oneOf(
            Arbitraries.strings().numeric().ofLength(9),  // Random 9 digits
            Arbitraries.just("123456789"),                // Valid
            Arbitraries.just("000123456"),                // Invalid - starts with 000
            Arbitraries.just("666123456"),                // Invalid - starts with 666
            Arbitraries.just("900123456"),                // Invalid - starts with 900
            Arbitraries.just("12345678"),                 // Too short
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> ficoScores() {
        return Arbitraries.oneOf(
            Arbitraries.integers().between(300, 850).map(String::valueOf),
            Arbitraries.just("299"),  // Too low
            Arbitraries.just("851"),  // Too high
            Arbitraries.just("12"),   // Too short
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> userIds() {
        return Arbitraries.oneOf(
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8),
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8)
                       .map(String::toLowerCase),  // Lowercase to test normalization
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8)
                       .map(s -> s.substring(0, Math.min(s.length(), 4)).toUpperCase() + 
                                s.substring(Math.min(s.length(), 4)).toLowerCase()), // Mixed case
            Arbitraries.just(""),
            Arbitraries.just(null)
        );
    }
}
